{ pkgs, lib, ... }:
let
  inherit (builtins)
    concatStringsSep all length elemAt genList elem foldl'
    isAttrs isList isPath isString isFloat isInt isNull isBool isFunction;

  catLines = concatStringsSep "\n";
  catComma = concatStringsSep ",";
  catComma' = concatStringsSep ", ";

  # add a single ident level to code
  identLines = lines: catLines (map (x: "  ${x}") lines);
  ident = code: identLines (lib.splitString "\n" code);
  identCat = lines: ident (catLines lines);

  # code for dealing with stdlib type mappings
  # maps a type definition tree, changing the base name to new_name
  updateNames = new_name: attrs:
    if attrs?__pathStdlib__ then
      lib.mapAttrsRecursive
        (path: value:
          if lib.last path == "__pathStdlib__" then
            new_name + (lib.removePrefix attrs.__pathStdlib__ value)
          else value)
        attrs
    else attrs;

  # maps a type definition tree, making the values the properties of of_what
  updateProps = of_what: attrs:
    if attrs?__pathStdlib__ then
      let
        attrs' = lib.filterAttrs (k: v: k != "__pathStdlib__") attrs;
        prop = if attrs.__pathStdlib__ == "" then of_what else notlua.keywords.UNSAFE_PROP of_what attrs.__pathStdlib__;
      in
      (builtins.mapAttrs (k: v: if isAttrs v then updateProps prop v else v) attrs')
      // prop
    else attrs;

  reduceArity = n: type:
    if type?__maxArity__ && (type.__minArity__ or (-1)) >= n then type // {
      __minArity__ = type.__minArity__ - n;
      __maxArity__ = type.__maxArity__ - n;
    } else if (type.__minArity__ or (-1)) >= n then type // {
      __minArity__ = type.__minArity__ - n;
    } else type;

  # change the name of a value, copying its type
  changeName = val: name:
    let
      type = luaType val;
      raw = notlua.keywords.ERAW name;
    in
    if type == null || !type?__type__ then raw
    else if type.__type__ == "function" then type // {
      __functor = notlua.keywords.CALL;
    } // raw
    else if type.__type__ == "table" then (updateNames name val) // raw
    else type // raw;

  luaType = val:
    if (val.__type__ or 0) == null then null
    else if val?__type__ then
      lib.filterAttrs (k: v: elem k [ "__retType__" "__type__" "__minArity__" "__maxArity__" "__entry__" ]) val
    else if val?__kind__ then null
    else if isList val || isAttrs val then { __type__ = "table"; }
    else if isPath val || isString val then { __type__ = "string"; }
    else if isInt val || isFloat val then { __type__ = "number"; }
    else if isNull val then { __type__ = "nil"; }
    else if isFunction val then { __type__ = "function"; } // (funcType val)
    else if isBool val then { __type__ = "boolean"; }
    else null;

  # check whether something is a valid "var" as per lua spec
  validVar = x:
    (x.__validVar__ or false) || ((x.__kind__ or "") == "rawStdlib");

  funcType = val:
    (if isFunction val then
      let argc = countArgs val;
      in { __minArity__ = argc; __maxArity__ = argc; }
    else if val?__meta__.__call then funcType (reduceArity 1 (luaType val.__meta__.__call))
    else if !val?__minArity__ then { }
    else if val?__maxArity__ then { inherit (val) __minArity__ __maxArity__; }
    else { inherit (val) __minArity__; }) // { __retType__ = retType val; };

  retType = val:
    if isFunction val then let applied = applyVars null "" 1 val; in luaType applied.result
    else if val?__retType__ then val.__retType__
    else if (val.__type__ or "function") != "function" then luaType val
    else null;

  humanType = val:
    let
      type = luaType val;
    in
    if (type.__type__ or null) == null then
      "unknown"
    else type.__type__;

  checkType = type1: type2:
    if type1 == null || type2 == null then true
    else
      all
        (x: length x < 2 || builtins.any (x: x == null) x || elemAt x 0 == elemAt x 1)
        (lib.attrValues (lib.zipAttrs [ type1 type2 ]));

  checkTypeAndMetaMatch = meta: args:
    let
      args' = builtins.filter ({ type, ... }: type != "unknown") (map (expr: { inherit expr; type = humanType expr; }) args);
      head = builtins.head args';
    in
    if args' == [ ] then true
    else if head?__meta__.${meta} then
      all ({ expr, type }: type == head.type && (expr.__meta__ or null) == head.__meta__) args'
    else
      all ({ type, ... }: type == head.type) args';

  isTypeOrHasMeta = types: meta: expr:
    elem (humanType expr) (types ++ [ "unknown" ]) || expr?__meta__.${meta};

  # check that no args contain the given meta key
  noMeta = meta: expr: !expr?__meta__.${meta};

  noMeta' = meta: args:
    all (noMeta meta) args;

  typeIfNoMeta = type: meta: expr:
    if noMeta meta expr then { __type__ = type; } else { };

  typeIfNoMeta' = type: meta: args:
    if noMeta' meta args then { __type__ = type; } else { };

  canBeFalsy = expr: expr != true && elem (humanType expr) [ "unknown" "boolean" "nil" ];
  canBeTruthy = expr: expr != false && (humanType expr == "unknown" || !(canBeFalsy expr));

  intersectTypes = type1: type2:
    if type1 == null || type2 == null then { }
    else
      (
        let ret = lib.filterAttrs (k: v: (type2.${k} or null) == v) type1;
        in if ret != { } then ret else { }
      );

  mkMacroKw = suffix: callback: { __kind__ = "custom" + suffix; __callback__ = callback; };
  mkMacroKw' = suffix: callback: (mkMacroKw suffix callback) // {
    __args__ = [ ];
    __functor = self: arg: self // { __args__ = self.__args__ ++ [ arg ]; };
  };

  # The following functions may take state: moduleName and scope
  # scope is how many variables are currently in scope
  # the count is used for generating new variable names
  # moduleName will additionally get changed based on the branch

  pushScope = n: state@{ scope, ... }: state // { scope = scope + n; };
  pushScope1 = pushScope 1;
  pushName = suffix: state@{ moduleName, ... }: state // { moduleName = moduleName + (toString suffix); };
  genPrefix = suffix: { moduleName, ... }: "${moduleName}${suffix}";

  isKeyword = s:
    builtins.elem s [
      "and"
      "break"
      "do"
      "else"
      "elseif"
      "end"
      "false"
      "for"
      "function"
      "if"
      "in"
      "local"
      "nil"
      "not"
      "or"
      "repeat"
      "return"
      "then"
      "true"
      "until"
      "while"
    ];

  # wrap a key for table constructor
  keySafe = s:
    (builtins.match "^[a-zA-Z_][a-zA-Z_0-9]*$" s) != null
    && (!(isKeyword s));
  wrapKey = scope: s: if keySafe s then s else "[${notlua.utils.compileExpr scope s}]";

  # Create a variable name with a prefix and scope
  varName = prefix: scope: "${prefix}${toString scope}";

  # Apply variable to a function
  applyVar = func': var:
    let
      func = if isFunction func' then func' else func'.__functor func';
      args = builtins.functionArgs func;
      prop = notlua.keywords.PROP var;
      getProp =
        if isAttrs var
        then (k: var.${k} or (prop k))
        else prop;
    in
    if args == { } then func var
    else func (builtins.mapAttrs (k: _: getProp k) args);

  # This function is mostly useful when you don't know how many variables a user-provided function takes.
  # It takes variable count (can be null), prefix and scope (prefix should be derived from module name), as
  # well as the function itself, creates new variables with your prefix and applies them to the function until
  # it stops being a function (e.g. if it becomes a functor, like vararg functions). It returns an attrset with
  # the `result` value, which contains the end result of function applcation. If you haven't provided `count`,
  # it also returns `argc` attr, containing the argument count of the function.
  applyVars = count: prefix: scope: func: applyVars' scope count prefix scope func 0;
  applyVars' = origScope: count: prefix:
    let
      self = scope: func: argc:
        if count != null && scope == (origScope + count) then { result = func; }
        else if count == null && !(isFunction func) then { result = func; inherit argc; }
        else self (scope + 1) (applyVar func (notlua.keywords.ERAW (varName prefix scope))) (argc + 1);
    in
    self;

  countArgs = func: (applyVars null "" 1 func).argc;

  notlua = rec {
    utils = rec {
      inherit reduceArity applyVar applyVars wrapKey varName pushName pushScope checkType luaType humanType ident;

      # compile a function. First argument is state, second argument is function name, whether it's vararg,
      # and if it has a name, whether it should be local; third argument is function itself
      compileFunc = state@{ scope, ... }: { name ? "", var ? false, local ? true, ... }: func:
        let
          prefix = genPrefix "_arg" state;
          res' = applyVars null prefix scope func;
          argc' = res'.argc;
          argc = if var then argc' - 1 else argc';
          res = if var then (applyVars argc prefix scope func).result (keywords.ERAW "...") else res'.result;
          header =
            if name == "" then
              "function"
            else
              (if local then "local " else "") + "function ${name}";
        in
        ''
          ${header}(${catComma' (
            genList (n: "${varName prefix (scope + n)}") argc
            ++ (lib.optional var "...")
          )})
          ${ident (compileStmt (pushScope argc state) res)}
          end'';

      # only used for operators
      compileWrapExpr = state: expr:
        if !(isAttrs expr) || ((expr.__wrapSafe__ or false) == true)
        then compileExpr state expr
        else compilePrefixExpr state expr;

      # prefixexp ::= var | functioncall | `(´ exp `)´
      compilePrefixExpr = state: expr:
        let
          compiled = compileExpr state expr;
        in
        if
          ((expr.__prefixExp__ or false) == true)
          || validVar expr
        then compiled
        else "(${compiled})";

      compileExpr = state: expr:
        if isString expr then builtins.toJSON expr
        else if isInt expr || isFloat expr then toString expr
        else if isBool expr then lib.boolToString expr
        else if isNull expr then "nil"
        else if isPath expr then compileExpr state (toString expr)
        else if isFunction expr then compileFunc state { } expr
        else if isList expr then
          (if expr == [ ] then "{}" else ''
            {
            ${identCat (map (x: compileExpr state x + ";" ) expr)}
            }'')
        else if !expr?__kind__ then
          let
            lists = notlua.keywords.LIST_PART expr;
            attrs = notlua.keywords.ATTR_PART expr;
          in
          if lists == [ ] && attrs == { } then "{}"
          else
            ("{"
              + (if lists == [ ] then "" else "\n" + (identCat (map (x: compileExpr state x + ";") lists)))
              + (if attrs == { } then "" else "\n" + (identCat (lib.mapAttrsToList (k: v: "${wrapKey state k} = ${compileExpr state v};") attrs)))
              + "\n}")
        else if expr.__kind__ == "rawStdlib" then
          expr.__pathStdlib__
        else if expr.__kind__ == "customExpr" || expr.__kind__ == "custom" then
          expr.__callback__ (expr // { __self__ = expr; __state__ = state; })
        else throw "Invalid expression kind ${expr.__kind__}";

      compileStmt = state@{ scope, ... }: stmt:
        if isList stmt then
          catLines (lib.imap0 (i: compileStmt (pushName i state)) stmt)
        else if elem (stmt.__kind__ or null) [ "customStmt" "custom" ] then
          stmt.__callback__ (stmt // { __self__ = stmt; __state__ = state; })
        else throw "Trying to use an expression of type ${humanType stmt} as a statement";

      # compile a module
      compile = moduleName: input: compileStmt { inherit moduleName; scope = 1; } input + "\n";
    };

    # "type definitions"
    # output: an attrset with stdlib and keywords (keywords contains REQ, REQ')
    neovim = attrs@{ neovim-unwrapped ? null, plugins ? [ ], extraLuaPackages ? (_: [ ]), ... }:
      pkgs.callPackage ./stdlib/nvim.nix (attrs // {
        inherit plugins extraLuaPackages;
        inherit keywords utils;
      });

    lua = attrs@{ lua ? null, ... }:
      pkgs.callPackage ./stdlib/lua.nix (attrs // {
        inherit keywords utils;
      });

    keywords = screamingKeywords;
    screamingKeywords = let inherit (utils) compileExpr compileWrapExpr compilePrefixExpr compileFunc compileStmt; in rec {
      # pass some raw code to lua directly
      # string -> expr&stmt
      RAW = code: MACRO (_: code);
      # string -> expr
      ERAW = code: EMACRO (_: code) // { __validVar__ = true; };
      # string -> stmt
      SRAW = code: SMACRO (_: code);

      LIST_PART = list:
        if isList list then list
        else if list?__list__ then list.__list__
        else if isAttrs list then [ ]
        else throw "this isn't a table";

      ATTR_PART = attrs:
        if isList attrs then { }
        else lib.filterAttrs (k: v: k != "__list__") attrs;

      MERGE = a: b:
        let
          listA = LIST_PART a;
          listB = LIST_PART b;
          attrsA = ATTR_PART a;
          attrsB = ATTR_PART b;
          lists = listA ++ listB;
          attrs = attrsA // attrsB;
        in
        if lists == [ ] then attrs
        else if attrs == { } then lists
        else attrs // { __list__ = lists; };

      # Access a property
      # Corresponding lua code: table.property
      # expr -> string -> expr
      PROP = expr: name:
        let
          self = EMACRO
            ({ __state__, ... }:
              assert lib.assertMsg
                ((isTypeOrHasMeta [ "table" ] "__index" expr)
                  || (isTypeOrHasMeta [ "table" ] "__newindex" expr))
                "Unable to get property ${name} of a ${humanType expr}!";
              compileExpr __state__ (UNSAFE_PROP expr name))
          // (if validVar expr then { __validVar__ = true; } else { });
        in
        self // (
          if expr?__pathStdlib__ && expr?${name} then expr.${name}
          else if expr?__entry__ then updateProps self expr.__entry__
          else { }
        ) // { __wrapSafe__ = true; };

      UNSAFE_PROP = expr: name:
        let
          self = EMACRO
            ({ __state__, ... }:
              if isKeyword name then
                "${compilePrefixExpr __state__ expr}[${compileExpr __state__ name}]"
              else
                "${compilePrefixExpr __state__ expr}.${name}")
          // (if validVar expr then { __validVar__ = true; } else { });
        in
        self // (
          if expr?__pathStdlib__ && expr?${name} then expr.${name}
          else if expr?__entry__ then updateProps self expr.__entry__
          else { }
        ) // { __wrapSafe__ = true; };

      # Apply a list of arguments to a function/operator
      APPLY = foldl' applyVar;

      # Call something
      # Useful if you need to call a zero argument function, or if you need to handle some weird metatable stuff
      # corresponding lua code: someFunc()
      # expr -> arg1 -> ... -> argN -> expr&stmt
      CALL = func:
        funcType func // (MACRO' ({ __minArity__ ? null, __maxArity__ ? null, __args__, __state__, ... }:
          assert lib.assertMsg
            (isTypeOrHasMeta [ "function" "table" ] "__call" func)
            (
              ''
                Calling a ${humanType __args__} (${compileExpr __state__ func}) might be a bad idea!
                If you still want to do it, use UNSAFE_CALL instead of CALL
              ''
            );
          assert lib.assertMsg
            ((__minArity__ == null || (length __args__) >= __minArity__)
            &&
            (__maxArity__ == null || (length __args__) <= __maxArity__))
            ("error: wrong function arity for ${compileExpr __state__ func}! "
            + "expected at least ${toString __minArity__}; "
            + (if __maxArity__ != null then "at most ${toString __maxArity__}; " else "")
            + "found ${toString (length __args__)}");
          compileExpr __state__ (APPLY (UNSAFE_CALL func) __args__)
        )) // { __prefixExp__ = true; };
      UNSAFE_CALL = func: MACRO'
        ({ __args__, __state__, ... }:
          "${compilePrefixExpr __state__ func}(${catComma' (map (compileExpr __state__) __args__)})"
        ) // { __prefixExp__ = true; };

      # Call a method
      # corresponding lua code: someTable:someFunc()
      # expr -> identifier -> arg1 -> ... -> argN -> expr&stmt
      MCALL = val: name:
        let
          func = PROP val name;
        in
        CALL func // (MACRO' ({ __args__, __state__, ... }:
          builtins.seq (compileExpr __state__ (APPLY (CALL func val) __args__)) (compileExpr __state__ (APPLY (UNSAFE_MCALL val name) __args__))
        ));
      UNSAFE_MCALL = val: name:
        let
          func = PROP val name;
        in
        UNSAFE_CALL func // (MACRO' ({ __args__, __state__, ... }:
          "${compilePrefixExpr __state__ val}:${name}(${catComma' (map (compileExpr __state__) __args__)})"
        ));

      # corresponding lua code: a = b
      # expr -> expr -> stmt
      SET = expr: val: SMACRO'
        ({ __args__, __state__, ... }:
          let
            checkLhs = x:
              assert lib.assertMsg
                (validVar x)
                "error: SET target must be a valid var, but it's a ${humanType x}";
              x;
          in
          assert lib.assertMsg
            (if isList expr then all ({ fst, snd }: checkType (luaType fst) (luaType snd)) (lib.zipLists expr __args__)
            else checkType (luaType expr) (if length __args__ > 1 then luaType __args__ else luaType val))
            (if isList expr then
              "error: setting ${catComma' (map (compileExpr __state__) expr)} to wrong types. They should be ${catComma' (map humanType expr)} but are ${catComma' (map humanType __args__)}" + (if length expr != length __args__ then " (length mismatch is allowed)" else "")
            else
              "error: setting ${compileExpr __state__ expr} to wrong type. It should be ${humanType expr} but is ${humanType val}");
          compileStmt __state__ (APPLY (UNSAFE_SET (if isList expr then map checkLhs expr else checkLhs expr)) __args__))
        val;
      UNSAFE_SET = expr: val: SMACRO'
        ({ __args__, __state__, ... }:
          let
            target =
              if isList expr then catComma' (map (compileExpr __state__) expr)
              else compileExpr __state__ expr;
          in
          "${target} = ${catComma' (map (compileExpr __state__) __args__)}")
        val;

      OP1' = type: typeCheck: op: expr:
        (OP1 op expr)
        // (if typeCheck != null then { __typeCheck__ = typeCheck; } else { })
        // (
          if isFunction type then type expr
          else if type != null then { __type__ = type; }
          else { }
        );

      # opName -> expr -> expr
      OP1 = op: expr: EMACRO
        ({ __state__, __typeCheck__ ? null, ... }:
          assert lib.assertMsg
            (__typeCheck__ == null || __typeCheck__ expr)
            ("Trying to apply `${op}` to an expression ${compileExpr __state__ expr} of type ${humanType expr}! "
              + "If that's what you intended, try OP1 \"${op}\" <expr> instead.");
          "${op}${compileWrapExpr __state__ expr}")
      // { __wrapSafe__ = true; };

      # The following operators have the signature
      # expr -> expr
      LEN = OP1' (typeIfNoMeta "number" "__len") (isTypeOrHasMeta [ "string" "table" ] "__len") "#";
      NOT = OP1' "boolean" null "not ";
      UNM = OP1' (typeIfNoMeta "number" "__unm") (isTypeOrHasMeta [ "number" ] "__unm") "-";
      BITNOT = OP1' (typeIfNoMeta "number" "__bnot") (isTypeOrHasMeta [ "number" ] "__bnot") "~";

      OP2' = type: typeCheck: op: arg1: arg2:
        (OP2 op arg1 arg2)
        // (if typeCheck != null then { __typeCheck__ = typeCheck; } else { })
        // (
          if type != null && isFunction type then
            ({ __typeFn__ = type; } // (type [ arg1 arg2 ]))
          else if type != null then { __type__ = type; }
          else { }
        );

      # opName -> expr1 -> ... -> exprN -> expr
      OP2 = op: arg1: arg2: EMACRO
        ({ __args__, __state__, __typeCheck__ ? null, ... }:
          assert lib.assertMsg
            (__typeCheck__ == null || __typeCheck__ __args__)
            ("Trying to apply `${op}` to expressions ${catComma' (map (compileExpr __state__) __args__)} of types "
              + "${catComma' (map humanType __args__)}! If that's what you intended, try OP2 \"${op}\" <exprs> instead");
          concatStringsSep " ${op} " (map (compileWrapExpr __state__) __args__))
      // {
        __args__ = [ arg1 arg2 ];
        __functor = self: arg: self // {
          __args__ = self.__args__ ++ [ arg ];
        } // (if self?__typeFn__ then self.__typeFn__ (self.__args__ ++ [ arg ]) else { });
      };

      # The following all have the signature
      # expr1 -> ... -> exprN -> expr
      EQ = OP2' "boolean" (checkTypeAndMetaMatch "__eq") "==";
      NE = OP2' "boolean" (checkTypeAndMetaMatch "__eq") "~=";
      LT = OP2' "boolean" (checkTypeAndMetaMatch "__lt") "<";
      GT = OP2' "boolean" (checkTypeAndMetaMatch "__lt") ">";
      LE = OP2' "boolean" (checkTypeAndMetaMatch "__le") "<=";
      GE = OP2' "boolean" (checkTypeAndMetaMatch "__le") ">=";
      AND = OP2'
        (args:
          let
            maybeTruthyCount = (lib.findFirst ({ i, x }: !(canBeTruthy x)) { i = length args; } (lib.imap0 (i: x: { inherit i x; }) args)).i;
            maybeTruthy = if maybeTruthyCount == length args then lib.init args else lib.take maybeTruthyCount args;
            maybeTruthy' = builtins.filter canBeFalsy maybeTruthy;
            baseType = luaType (if maybeTruthyCount == length args then lib.last args else elemAt args maybeTruthyCount);
            types = map luaType maybeTruthy';
          in
          builtins.foldl' intersectTypes baseType types
        )
        null "and";
      OR = OP2'
        (args:
          let
            maybeFalsyCount = (lib.findFirst ({ i, x }: !(canBeFalsy x)) { i = length args; } (lib.imap0 (i: x: { inherit i x; }) args)).i;
            maybeFalsy = if maybeFalsyCount == length args then lib.init args else lib.take maybeFalsyCount args;
            maybeFalsy' = builtins.filter canBeTruthy maybeFalsy;
            baseType = luaType (if maybeFalsyCount == length args then lib.last args else elemAt args maybeFalsyCount);
            types = map luaType maybeFalsy';
          in
          builtins.foldl' intersectTypes baseType types
        )
        null "or";

      CAT = OP2' "string" null "..";
      ADD = OP2' (typeIfNoMeta' "number" "__add") (all (isTypeOrHasMeta [ "number" ] "__add")) "+";
      SUB = OP2' (typeIfNoMeta' "number" "__sub") (all (isTypeOrHasMeta [ "number" ] "__sub")) "-";
      MUL = OP2' (typeIfNoMeta' "number" "__mul") (all (isTypeOrHasMeta [ "number" ] "__mul")) "*";
      DIV = OP2' (typeIfNoMeta' "number" "__div") (all (isTypeOrHasMeta [ "number" ] "__div")) "/";
      IDIV = OP2' (typeIfNoMeta' "number" "__idiv") (all (isTypeOrHasMeta [ "number" ] "__idiv")) "//";
      MOD = OP2' (typeIfNoMeta' "number" "__mod") (all (isTypeOrHasMeta [ "number" ] "__mod")) "%";
      POW = OP2' (typeIfNoMeta' "number" "__pow") (all (isTypeOrHasMeta [ "number" ] "__pow")) "^";
      BITAND = OP2' (typeIfNoMeta' "number" "__band") (all (isTypeOrHasMeta [ "number" ] "__band")) "&";
      BITOR = OP2' (typeIfNoMeta' "number" "__bor") (all (isTypeOrHasMeta [ "number" ] "__bor")) "|";
      BXOR = OP2' (typeIfNoMeta' "number" "__bxor") (all (isTypeOrHasMeta [ "number" ] "__bxor")) "~";
      SHL = OP2' (typeIfNoMeta' "number" "__shl") (all (isTypeOrHasMeta [ "number" ] "__shl")) "<<";
      SHR = OP2' (typeIfNoMeta' "number" "__shr") (all (isTypeOrHasMeta [ "number" ] "__shr")) ">>";

      # Corresponding lua code: for ... in ...
      # argc -> expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN' = argc: expr: body: SMACRO ({ __state__, ... }:
        let
          prefix = genPrefix "_for" __state__;
          res = applyVars argc prefix __state__.scope body;
          argc' = if argc != null then argc else res.argc;
          varNames = genList (n: "${varName prefix (__state__.scope + n)}") argc';
        in
        ''
          for ${catComma varNames} in ${compileExpr __state__ expr} do
          ${ident (compileStmt (pushScope argc' __state__) res.result)}
          end'');
      FOR_IN' = FORIN';

      # expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN = FORIN' null;
      FOR_IN = FORIN;

      FORRANGE = arg1: arg2: arg3: SMACRO'
        ({ __args__, __state__, ... }:
          let
            vars =
              assert lib.assertMsg
                (all (x: elem (humanType x) [ "unknown" "number" ]) __args__)
                ("Error: trying to use FORRANGE on values of types ${catComma' (map humanType __args__)}! "
                  + "Please don't do that.");
              if length __args__ == 3 || length __args__ == 4 then
                map (compileExpr __state__) (lib.init __args__)
              else throw "for range can only receive 3 or 4 arguments";
            body = lib.last __args__;
            prefix = genPrefix "_nfor" __state__;
            name = varName prefix __state__.scope;
          in
          ''
            for ${name} = ${catComma vars} do
            ${ident (compileStmt (pushScope1 __state__) (body (ERAW name)))}
            end''
        )
        arg1
        arg2
        arg3;
      FOR_RANGE = FORRANGE;

      WHILE = cond: body: SMACRO ({ __state__, ... }: ''
        while ${compileExpr __state__ cond} do
        ${ident (compileStmt __state__ body)}
        end
      '');

      REPEAT = body: cond: SMACRO ({ __state__, ... }: ''
        repeat
        ${ident (compileStmt __state__ body)}
        until ${compileExpr __state__ cond}
      '');

      # Issues a return statement
      # Corresponding lua code: return
      # expr -> stmt
      RETURN = SMACRO' ({ __args__, __state__, ... }:
        if __args__ == [ ] then "return"
        else "return ${catComma' (map (compileExpr __state__) __args__)}");

      BREAK = SRAW "break";

      # Creates a zero argument function with user-provided statements
      # stmts -> expr
      DEFUN = func:
        EMACRO ({ __state__, ... }: (compileFunc __state__ { } func)) // {
          __type__ = null;
          __retType__ = luaType func;
          __minArity__ = 0;
          __maxArity__ = 0;
        };
      # Creates a vararg functions (last argument will be the hidden `arg` lua variable)
      # stmts -> expr
      DEFUN_VAR = func:
        EMACRO ({ __state__, ... }: (compileFunc __state__ { var = true; } func)) // {
          __retType__ = retType func;
          __minArity__ = countArgs func - 1;
        };

      # Corresponding lua code: if then (else?)
      # (expr -> stmts ->)* (fallback expr ->)? stmts
      IF = expr: stmt: SMACRO'
        ({ __args__, __state__, ... }:
          let
            data =
              if length __args__ / 2 * 2 != length __args__ then {
                branches = lib.init __args__;
                fallback = lib.last __args__;
              } else if elemAt __args__ (length __args__ - 2) == ELSE then {
                branches = lib.take (length __args__ - 2) __args__;
                fallback = lib.last __args__;
              } else {
                branches = __args__;
                fallback = null;
              };
          in
          (lib.removeSuffix "else" (concatStringsSep ""
            (lib.imap0
              (i: x:
                if i / 2 * 2 == i then ''
                  if ${compileExpr __state__ x} then
                ''
                else ''
                  ${ident (compileStmt __state__ x)}
                  else'')
              data.branches)
          + (if data.fallback != null then
            "\n${ident (compileStmt __state__ data.fallback)}\n"
          else "")))
          + "end"
        )
        expr
        stmt;

      # Signifies the fallback branch in IF. May only be the last branch.
      # Note that you may also omit it and just include the last branch without a preceding condition.
      ELSE = true;

      # Corresponding lua code: table[key]
      # table -> key -> expr
      IDX = table: key:
        let
          self = EMACRO
            ({ __state__, ... }:
              assert lib.assertMsg
                ((isTypeOrHasMeta [ "table" ] "__index" table)
                  || (isTypeOrHasMeta [ "table" ] "__newindex" table))
                "Unable to get key ${compileExpr __state__ key} of a ${humanType table} ${compileExpr __state__ table}!";
              compileExpr __state__ (UNSAFE_IDX table key))
          // (if validVar table then { __validVar__ = true; } else { });
        in
        self
        // (if table?__entry__ then updateProps self table.__entry__ else { })
        // { __wrapSafe__ = true; };

      UNSAFE_IDX = table: key:
        let
          self = EMACRO
            ({ __state__, ... }:
              "${compilePrefixExpr __state__ table}[${compileExpr __state__ key}]")
          // (if validVar table then { __validVar__ = true; } else { });
        in
        self
        // (if table?__entry__ then updateProps self table.__entry__ else { })
        // { __wrapSafe__ = true; };

      # Creates variables and passes them to the function
      # Corresponding lua code: local ... = ...
      # expr1 -> ... -> exprN -> (expr1 -> ... -> exprN -> stmt) -> stmt
      LET = LMACRO ({ __state__, __vars__, ... }:
        map
          ({ name, value, ... }: {
            code = compileExpr __state__ value;
            expr = changeName value name;
          })
          __vars__);

      # Creates variables and passes them to the function as well as variable binding code
      # Corresponding lua code: local ... = ...
      # ((expr1 -> ... -> exprN) ->)* (expr1 -> ... -> exprN -> stmt) -> stmt
      LETREC = LMACRO ({ __state__, __vars__, ... }:
        let
          # this is just the raw names
          vars''' = map ({ value, name, ... }: ERAW name) __vars__;
          # this has more well defined types
          vars'' = map ({ value, name, ... }: changeName (APPLY value vars''') name) __vars__;
          # and do one more pass just to be sure, in case the additional type info above
          # helps reason about the types even better
          vars' = map ({ value, name, ... }: changeName (APPLY value vars'') name) __vars__;
        in
        map
          ({ name, value }:
            let val = APPLY value vars'; in {
              code = compileExpr (pushScope (length __vars__) __state__) val;
              expr = changeName val name;
              predef = true;
            })
          __vars__);
      LET_REC = LETREC;

      # Process arbitrary code during compilation
      # (macroArgs -> string) -> stmt&expr
      # macroArgs are the macro itself, unified with { __state__ } (compiler state)
      # to pass additional parameters to macro, do (MACRO ... // params)
      MACRO = mkMacroKw "";
      SMACRO = mkMacroKw "Stmt";
      EMACRO = mkMacroKw "Expr";
      # the following are vararg macros (same as above, but they also receive { __args__ })
      MACRO' = mkMacroKw' "";
      SMACRO' = mkMacroKw' "Stmt";
      EMACRO' = mkMacroKw' "Expr";

      # Create custom "let" generators
      # binding processor is a function that receives
      # {
      #   __state__ = initial __state__ before the let binding;
      #   __vars__ = a list of{
      #     name (var name); value (whatever user passed to let);
      #   }
      # }
      # unified with the macro itself (like in MACRO)
      # and must return for each binding:
      # {
      #   code = raw code which the variables will be set to;
      #   expr = whatever data will be passed to the user (may be enriched with type info/etc);
      # }
      LMACRO = processor: arg1: arg2: SMACRO'
        (attrs@{ __args__, __state__, ... }:
          let
            func = lib.last __args__;
            vals = lib.init __args__;
            prefix = genPrefix "_var" __state__;
            names = genList (i: varName prefix (__state__.scope + i)) (length vals);
            values = processor (attrs // {
              __vars__ = (lib.zipListsWith (name: value: { inherit name value; }) names vals);
            });
            kvs = lib.zipListsWith (key: val: { inherit key val; }) names values;
            predefVars = builtins.filter ({ key, val }: val.predef or false) kvs;
            predefs = catLines (map
              ({ key, val }:
                if val.local or true then "local ${key}" else "${key} = nil")
              predefVars);
          in
          (if predefs == "" then "" else predefs + "\n") +
          ''
            ${catLines (map ({ key, val }:
              "${if val.local or true && !(val.predef or false) then "local " else ""}${key} = ${val.code}"
            ) kvs)}
            ${
              compileStmt
                (pushScope (length vals) __state__)
                (APPLY func (map (x: x.expr) values))
            }''
        )
        arg1
        arg2;
    };
    pascalKeywords = lib.mapAttrs'
      (name: value: {
        name = lib.concatMapStrings (s: builtins.substring 0 1 s + lib.toLower (builtins.substring 1 (-1) s)) (lib.splitString "_" name);
        inherit value;
      })
      screamingKeywords;
    camelKeywords = lib.mapAttrs'
      (name: value: {
        name = lib.concatImapStrings
          (i: s:
            if i == 1 then lib.toLower s
            else builtins.substring 0 1 s + lib.toLower (builtins.substring 1 (-1) s)
          )
          (lib.splitString "_" name);
        inherit value;
      })
      screamingKeywords;
  };

in
{
  options = {
    notlua = lib.mkOption {
      type = lib.mkOptionType {
        name = "notlua";
        description = "NotLua module";
        check = builtins.isAttrs;
      };
      description = "NotLua module, not to be set by the user. See https://github.com/chayleaf/notlua for more info.";
      default = notlua;
    };
  };
  config = {
    _module.args = {
      inherit notlua;
    };
    inherit notlua;
  };
}
