{ pkgs, lib, ... }:
let
  inherit (builtins)
    concatStringsSep all length elemAt genList elem foldl' hasAttr getAttr
    isAttrs isList isPath isString isFloat isInt isNull isBool isFunction;

  catLines = concatStringsSep "\n";
  catComma = concatStringsSep ",";
  catComma' = concatStringsSep ", ";

  # add a single ident level to code
  identLines = lines: catLines (map (x: "  ${x}") lines);
  ident = code: identLines (lib.splitString "\n" code);
  identCat = lines: ident (catLines lines);

  # maps a type definition tree, changing the base name to new_name
  updateNames = new_name: attrs:
    if attrs?__name__ then
      lib.mapAttrsRecursive
        (path: value:
          if lib.last path == "__name__" then
            new_name + (lib.removePrefix attrs.__name__ value)
          else value)
        attrs
    else attrs;

  # maps a type definition tree, making the values the properties of of_what
  updateProps = of_what: attrs:
    if attrs?__name__ then
      let
        attrs' = lib.filterAttrs (k: v: k != "__name__") attrs;
        prop = if attrs.__name__ == "" then of_what else notlua.keywords.UNSAFE_PROP of_what attrs.__name__;
      in
      (builtins.mapAttrs (k: v: if isAttrs v then updateProps prop v else v) attrs')
      // prop
    else attrs;

  reduceArity = n: type:
    if isAttrs type && type?__minArity__ && type?__maxArity__ && type.__minArity__ >= n then type // {
      __minArity__ = type.__minArity__ - n;
      __maxArity__ = type.__maxArity__ - n;
    } else if isAttrs type && type?__minArity__ && type.__minArity >= n then type // {
      __minArity__ = type.__minArity__ - n;
    } else type;

  # change the name of a value, copying its type
  changeName = val: name:
    let
      type = luaType val;
      raw = notlua.keywords.RAW name;
    in
    if isAttrs val && val?__meta__ && val.__meta__?__call then reduceArity 1 (luaType val.__meta__.__call) // {
      __functor = notlua.keywords.CALL;
    } // raw
    else if type == null || !type?__type__ then raw
    else if type.__type__ == "function" then type // {
      __functor = notlua.keywords.CALL;
    } // raw
    else if type.__type__ == "table" then (updateNames name val) // raw
    else type // raw;

  luaType = val:
    if isAttrs val && val?__retType__ then val.__retType__
    else if isAttrs val && val?__type__ && val.__type__ == null then null
    else if isAttrs val && val?__type__ then
      lib.filterAttrs (k: v: elem k [ "__retType__" "__type__" "__minArity__" "__maxArity__" ]) val
    else if isAttrs val && val?__kind__ then null
    else if isList val || isAttrs val then { __type__ = "table"; }
    else if isPath val || isString val then { __type__ = "string"; }
    else if isInt val || isFloat val then { __type__ = "number"; }
    else if isNull val then { __type__ = "nil"; }
    else if isFunction val then { __type__ = "function"; } // (funcType val)
    else if isBool val then { __type__ = "boolean"; }
    else null;

  funcType = val:
    (if isFunction val then
      let argc = countArgs val;
      in { __minArity__ = argc; __maxArity__ = argc; }
    else if !(isAttrs val) || !val?__minArity__ then { }
    else if val?__maxArity__ then { inherit (val) __minArity__ __maxArity__; }
    else { inherit (val) __minArity__; }) // { __retType__ = retType val; };

  retType = val:
    if isFunction val then let applied = applyVars null "" 1 val; in luaType applied.result
    else if isAttrs val && val?__type__ && val.__type__ != "function" then luaType val
    else null;

  humanType = val:
    let
      type = luaType val;
    in
    if type == null || !(isAttrs type) || !type?__type__ || type.__type__ == null then
      "unknown"
    else type.__type__;

  checkType = type1: type2:
    if type1 == null || type2 == null then true
    else
      all
        (x: length x < 2 || builtins.any (x: x == null) x || elemAt x 0 == elemAt x 1)
        (lib.attrValues (lib.zipAttrs [ type1 type2 ]));

  checkTypeAndMetaMatch = meta: args:
    let args' = builtins.filter ({ type, ... }: type != "unknown") (map (expr: { inherit expr; type = humanType expr; }) args);
    in if args' == [ ] then true
    else if isAttrs (builtins.head args') && (builtins.head args')?__meta__ && hasAttr meta (builtins.head args').__meta__ then
      all ({ expr, ... }: isAttrs expr && expr?__meta__ && expr.__meta__ == (builtins.head args').__meta__) args'
    else
      all ({ type, ... }: type == (builtins.head args').type) args';

  isTypeOrHasMeta = types: meta: expr:
    (elem (humanType expr) (types ++ [ "unknown" ])) || (isAttrs expr && expr?__meta__ && hasAttr meta expr.__meta__);

  # The following functions may take state: moduleName and scope
  # scope is how many variables are currently in scope
  # the count is used for generating new variable names
  # moduleName will additionally get changed based on the branch

  pushScope = n: state@{ scope, ... }: state // { scope = scope + n; };
  pushScope1 = pushScope 1;
  pushName = suffix: state@{ moduleName, ... }: state // { moduleName = moduleName + (toString suffix); };
  genPrefix = suffix: { moduleName, ... }: "${moduleName}${suffix}";

  # wrap an expression in parentheses if necessary
  # probably not the best heuristics, but good enough to make the output readable
  wrapSafe = s:
    (builtins.match
      "^[-\"a-zA-Z0-9_.()]*$"
      (builtins.replaceStrings [ "[" "]" ] [ "" "" ] s))
    != null;
  wrapExpr = s: if wrapSafe s then s else "(${s})";

  # Same, but for table keys
  keySafe = s: (builtins.match "^[a-zA-Z_][_a-zA-Z0-9]*$" s) != null;
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
        then (k: if hasAttr k var then getAttr k var else prop k)
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
        else self (scope + 1) (applyVar func (notlua.keywords.RAW (varName prefix scope))) (argc + 1);
    in
    self;

  countArgs = func: (applyVars null "" 1 func).argc;

  notlua = rec {
    utils = rec {
      inherit reduceArity applyVar applyVars wrapKey wrapExpr varName pushName pushScope checkType luaType humanType ident;

      # compile a function. First argument is state, second argument is function name, whether it's vararg,
      # and if it has a name, whether it should be local; third argument is function itself
      compileFunc = state@{ scope, ... }: { name ? "", var ? false, local ? true, ... }: func:
        let
          prefix = genPrefix "_arg" state;
          res' = applyVars null prefix scope func;
          argc' = res'.argc;
          argc = if var then argc' - 1 else argc';
          res = if var then (applyVars argc prefix scope func).result (keywords.RAW "arg") else res'.result;
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

      compileWrapExpr = state: expr:
        let compiled = compileExpr state expr;
        in if isAttrs expr && expr?__wrapSafe__ && expr.__wrapSafe__ == true then compiled
        else if !(isAttrs expr) then compiled
        else wrapExpr compiled;

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
          if lists == [ ] && attrs == { } then "{}" else
          ("{"
            + (if lists == [ ] then "" else "\n" + (identCat (map (x: compileExpr state x + ";") lists)))
            + (if attrs == { } then "" else "\n" + (identCat (lib.mapAttrsToList (k: v: "${wrapKey state k} = ${compileExpr state v};") attrs)))
            + "\n}")
        else if expr.__kind__ == "raw" then
          expr.__name__
        else if expr.__kind__ == "custom" then
          expr.__callback__ (expr // { self = expr; inherit state; })
        else throw "Invalid expression kind ${expr.__kind__}";

      compileStmt = state@{ scope, ... }: stmt:
        if isList stmt then
          catLines (lib.imap0 (i: compileStmt (pushName i state)) stmt)
        else if isAttrs stmt && stmt?__kind__ && stmt.__kind__ == "customStmt" then
          stmt.__callback__ (stmt // { self = stmt; inherit state; })
        else if isAttrs stmt && stmt?__kind__ && stmt.__kind__ == "rawStmt" then
          stmt.__name__
        else compileExpr state stmt;

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

    keywords = let inherit (utils) compileExpr compileWrapExpr compileFunc compileStmt; in rec {
      # pass some raw code to lua directly
      # string -> expr
      RAW = name: { __kind__ = "raw"; __name__ = name; };
      # string -> stmt
      RAW' = name: { __kind__ = "rawStmt"; __name__ = name; };

      LIST_PART = list:
        if isList list then list
        else if isAttrs list && list?__list__ then list.__list__
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
      PROP = expr: name: EMACRO
        ({ state, ... }:
          assert lib.assertMsg
            ((isTypeOrHasMeta [ "table" ] "__index" expr)
              || (isTypeOrHasMeta [ "table" ] "__newindex" expr))
            "Unable to get property ${name} of a ${humanType expr}!";
          compileExpr state (UNSAFE_PROP expr name))
      // (if isAttrs expr && expr?__name__ && hasAttr name expr then getAttr name expr else { })
      // { __wrapSafe__ = true; };

      UNSAFE_PROP = expr: name: EMACRO
        ({ state, ... }:
          "${compileWrapExpr state expr}.${name}")
      // (if isAttrs expr && expr?__name__ && hasAttr name expr then getAttr name expr else { })
      // { __wrapSafe__ = true; };

      # Apply a list of arguments to a function/operator
      APPLY = foldl' applyVar;

      # Call something
      # Useful if you need to call a zero argument function, or if you need to handle some weird metatable stuff
      # corresponding lua code: someFunc()
      # expr -> arg1 -> ... -> argN -> expr
      CALL = func:
        funcType func // (EMACRO' ({ __minArity__ ? null, __maxArity__ ? null, args, state, ... }:
          assert lib.assertMsg
            (!(elem (humanType func) [ "number" "boolean" "nil" "string" ]))
            ("Calling a ${humanType args} (${compileExpr state func}) might be a bad idea! "
            + "If you still want to do it, use UNSAFE_CALL instead of CALL");
          assert lib.assertMsg
            ((__minArity__ == null || (length args) >= __minArity__)
            &&
            (__maxArity__ == null || (length args) <= __maxArity__))
            ("error: wrong function arity for ${compileExpr state func}! "
            + "expected at least ${toString __minArity__}; "
            + (if __maxArity__ != null then "at most ${toString __maxArity__}; " else "")
            + "found ${toString (length args)}");
          compileExpr state (APPLY (UNSAFE_CALL func) args)
        )) // { __wrapSafe__ = true; };
      UNSAFE_CALL = func: EMACRO'
        ({ args, state, ... }:
          "${compileWrapExpr state func}(${catComma' (map (compileExpr state) args)})"
        ) // { __wrapSafe__ = true; };

      # Call a method
      # corresponding lua code: someTable:someFunc()
      # expr -> identifier -> arg1 -> ... -> argN -> expr
      MCALL = val: name: EMACRO'
        ({ args, state, ... }:
          assert lib.assertMsg
            (elem (humanType val) [ null "unknown" "table" ])
            ("Calling a method of a ${humanType val} (${compileExpr state val}) might be a bad idea! "
              + "If you still want to do it, use UNSAFE_MCALL instead of MCALL");
          compileExpr state (APPLY (UNSAFE_MCALL val name) args)
        ) // { __wrapSafe__ = true; };
      UNSAFE_MCALL = val: name: EMACRO'
        ({ args, state, ... }:
          "${compileWrapExpr state val}:${name}(${catComma' (map (compileExpr state) args)})"
        ) // { __type__ = null; __wrapSafe__ = true; };

      # corresponding lua code: a = b
      # expr -> expr -> stmt
      SET = expr: val: SMACRO'
        ({ args, state, ... }:
          assert lib.assertMsg
            (checkType (luaType expr) (luaType val))
            "error: setting ${compileExpr state expr} to wrong type. It should be ${humanType expr} but is ${humanType val}";
          compileStmt state (APPLY UNSAFE_SET args))
        expr
        val;
      UNSAFE_SET = expr: val: SMACRO'
        ({ args, state, ... }:
          let
            target = builtins.head args;
            vals = builtins.tail args;
          in
          "${compileExpr state target} = ${catComma' (map (compileExpr state) vals)}")
        expr
        val;

      OP1' = type: typeCheck: op: expr:
        (OP1 op expr)
        // (if typeCheck != null then { inherit typeCheck; } else { })
        // (if type != null then { __type__ = type; } else { });

      # opName -> expr -> expr
      OP1 = op: expr: EMACRO ({ state, typeCheck ? null, ... }:
        assert lib.assertMsg
          (typeCheck == null || typeCheck expr)
          ("Trying to apply `${op}` to an expression ${compileExpr state expr} of type ${humanType expr}! "
            + "If that's what you intended, try OP1 \"${op}\" <expr> instead.");
        "${op}${compileWrapExpr state expr}");

      # The following operators have the signature
      # expr -> expr
      LEN = OP1' "number" (isTypeOrHasMeta [ "string" "table" ] "__len") "#";
      NOT = OP1' "boolean" null "not ";
      UNM = OP1' "number" (isTypeOrHasMeta [ "number" ] "__unm") "-";
      BITNOT = OP1' "number" (isTypeOrHasMeta [ "number" ] "__bnot") "~";

      OP2' = type: typeCheck: op: arg1: arg2:
        (OP2 op arg1 arg2)
        // (if typeCheck != null then { inherit typeCheck; } else { })
        // (if type != null then { __type__ = type; } else { });

      # opName -> expr1 -> ... -> exprN -> expr
      OP2 = op: arg1: arg2: EMACRO'
        ({ args, state, typeCheck ? null, ... }:
          assert lib.assertMsg
            (typeCheck == null || typeCheck args)
            ("Trying to apply `${op}` to expressions ${catComma' (map (compileExpr state) args)} of types "
              + "${catComma' (map humanType args)}! If that's what you intended, try OP2 \"${op}\" <exprs> instead");
          concatStringsSep " ${op} " (map (compileWrapExpr state) args))
        arg1
        arg2;

      # The following all have the signature
      # expr1 -> ... -> exprN -> expr
      EQ = OP2' "boolean" (checkTypeAndMetaMatch "__eq") "==";
      NE = OP2' "boolean" (checkTypeAndMetaMatch "__eq") "~=";
      LT = OP2' "boolean" (checkTypeAndMetaMatch "__lt") "<";
      GT = OP2' "boolean" (checkTypeAndMetaMatch "__lt") ">";
      LE = OP2' "boolean" (checkTypeAndMetaMatch "__le") "<=";
      GE = OP2' "boolean" (checkTypeAndMetaMatch "__le") ">=";
      AND = OP2 "and";
      OR = OP2 "or";

      CAT = OP2' "string" null "..";
      ADD = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__add")) "+";
      SUB = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__sub")) "-";
      MUL = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__mul")) "*";
      DIV = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__div")) "/";
      IDIV = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__idiv")) "//";
      MOD = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__mod")) "%";
      POW = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__pow")) "^";
      BITAND = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__band")) "&";
      BITOR = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__bor")) "|";
      BXOR = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__bxor")) "~";
      SHL = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__shl")) "<<";
      SHR = OP2' "number" (all (isTypeOrHasMeta [ "number" ] "__shr")) ">>";

      # Corresponding lua code: for ... in ...
      # argc -> expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN' = argc: expr: body: SMACRO ({ state, ... }:
        let
          prefix = genPrefix "_for" state;
          res = applyVars argc prefix state.scope body;
          argc' = if argc != null then argc else res.argc;
          varNames = genList (n: "${varName prefix (state.scope + n)}") argc';
        in
        ''
          for ${catComma varNames} in ${compileExpr state expr} do
          ${ident (compileStmt (pushScope argc' state) res.result)}
          end'');

      # expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN = FORIN' null;

      FORRANGE = arg1: arg2: arg3: SMACRO'
        ({ args, state, ... }:
          let
            vars =
              assert lib.assertMsg
                (all (x: elem (humanType x) [ "unknown" "number" ]) args)
                ("Error: trying to use FORRANGE on values of types ${catComma' (map humanType args)}! "
                  + "Please don't do that.");
              if length args == 3 || length args == 4 then
                map (compileExpr state) (lib.init args)
              else throw "for range can only receive 3 or 4 arguments";
            body = lib.last args;
            prefix = genPrefix "_nfor" state;
            name = varName prefix state.scope;
          in
          ''
            for ${name} = ${catComma vars} do
            ${ident (compileStmt (pushScope1 state) (body (RAW name)))}
            end''
        )
        arg1
        arg2
        arg3;

      WHILE = cond: body: SMACRO ({ state, ... }: ''
        while ${compileExpr state cond} do
        ${ident (compileStmt state body)}
        end
      '');

      REPEAT = body: cond: SMACRO ({ state, ... }: ''
        repeat
        ${ident (compileStmt state body)}
        until ${compileExpr state cond}
      '');

      # Issues a return statement
      # Corresponding lua code: return
      # expr -> stmt
      RETURN = SMACRO' ({ args, state, ... }:
        if args == [ ] then "return"
        else "return ${catComma' (map (compileExpr state) args)}");

      BREAK = RAW' "break";

      # Creates a zero argument function with user-provided statements
      # stmts -> expr
      DEFUN = func:
        EMACRO ({ state, ... }: (compileFunc state { } func)) // {
          __type__ = null;
          __retType__ = luaType func;
          __minArity__ = 0;
          __maxArity__ = 0;
        };
      # Creates a vararg functions (last argument will be the hidden `arg` lua variable)
      # stmts -> expr
      DEFUN_VAR = func:
        EMACRO ({ state, ... }: (compileFunc state { var = true; } func)) // {
          __retType__ = retType func;
          __minArity__ = countArgs func - 1;
        };

      # Corresponding lua code: if then (else?)
      # (expr -> stmts ->)* (fallback expr ->)? stmts
      IF = expr: stmt: SMACRO'
        ({ args, state, ... }:
          let
            data =
              if length args / 2 * 2 != length args then {
                branches = lib.init args;
                fallback = lib.last args;
              } else if elemAt args (length args - 2) == ELSE then {
                branches = lib.take (length args - 2) args;
                fallback = lib.last args;
              } else {
                branches = args;
                fallback = null;
              };
          in
          (lib.removeSuffix "else" (concatStringsSep ""
            (lib.imap0
              (i: x:
                if i / 2 * 2 == i then ''
                  if ${compileExpr state x} then
                ''
                else ''
                  ${ident (compileStmt state x)}
                  else'')
              data.branches)
          + (if data.fallback != null then
            "\n${ident (compileStmt state data.fallback)}\n"
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
          self = EMACRO ({ state, ... }:
            assert lib.assertMsg
              (checkType (luaType table) (luaType { }))
              "Unable to get key ${compileExpr state key} of a ${humanType table} ${compileExpr state table}!";
            compileExpr state (UNSAFE_IDX table key));
        in
        self
        // (if builtins.isAttrs table && table?__entry__ then updateProps self table.__entry__ else { })
        // { __wrapSafe__ = true; };

      UNSAFE_IDX = table: key:
        let
          self = EMACRO ({ state, ... }:
            "${compileWrapExpr state table}[${compileExpr state key}]");
        in
        self
        // (if builtins.isAttrs table && table?__entry__ then updateProps self table.__entry__ else { })
        // { __wrapSafe__ = true; };

      # Creates variables and passes them to the function
      # Corresponding lua code: local ... = ...
      # expr1 -> ... -> exprN -> (expr1 -> ... -> exprN -> stmt) -> stmt
      LET = LMACRO ({ state, vars, ... }:
        map
          ({ name, value, ... }: {
            code = compileExpr state value;
            expr = changeName value name;
          })
          vars);

      # Creates variables and passes them to the function as well as variable binding code
      # Corresponding lua code: local ... = ...
      # ((expr1 -> ... -> exprN) ->)* (expr1 -> ... -> exprN -> stmt) -> stmt
      LETREC = LMACRO ({ state, vars, ... }:
        let
          # this is just the raw names
          vars''' = map ({ value, name, ... }: RAW name) vars;
          # this has more well defined types
          vars'' = map ({ value, name, ... }: changeName (APPLY value vars''') name) vars;
          # and just to be sure, do one more pass in case the additional type info above helps
          vars' = map ({ value, name, ... }: changeName (APPLY value vars'') name) vars;
        in
        map
          ({ name, value }:
            let val = APPLY value vars'; in {
              code = compileExpr (pushScope (length vars) state) val;
              expr = changeName val name;
              predef = true;
            })
          vars);

      # Process arbitrary code during compilation to be able to access state
      # (state -> { result = (stmt|expr), state = new state }) -> (stmt|expr)
      MACRO = isStmt: callback: {
        __kind__ = if isStmt then "customStmt" else "custom";
        __callback__ = callback;
      };
      SMACRO = MACRO true;
      EMACRO = MACRO false;
      # MACRO variant that passes a {args} argument
      MACRO' = stmt: callback: (MACRO stmt callback) // {
        args = [ ];
        __functor = self: arg: self // {
          args = self.args ++ [ arg ];
        };
      };
      SMACRO' = MACRO' true;
      EMACRO' = MACRO' false;

      # Create custom "let" generators
      # binding processor is a function that receives
      # {
      #   state = initial state before the let binding;
      #   vars = a list of{
      #     name (var name); value (whatever user passed to let);
      #   }
      # }
      # and must return for each binding:
      # {
      #   code = raw code which the variables will be set to;
      #   expr = whatever data will be passed to the user;
      # }
      LMACRO = processor: arg1: arg2: SMACRO'
        ({ args, state, ... }:
          let
            func = lib.last args;
            vals = lib.init args;
            prefix = genPrefix "_var" state;
            names = genList (i: varName prefix (state.scope + i)) (length vals);
            values = processor {
              inherit state;
              vars = (lib.zipListsWith (name: value: { inherit name value; }) names vals);
            };
            kvs = lib.zipListsWith (key: val: { inherit key val; }) names values;
            predefVars = builtins.filter ({ key, val }: val?predef && val.predef) kvs;
            predefs = catLines (map
              ({ key, val }:
                if !val?local || val.local then "local ${key}" else "${key} = nil")
              predefVars);
          in
          (if predefs == "" then "" else predefs + "\n") +
          ''
            ${catLines (map ({ key, val }:
              "${if (!val?local || val.local) && (!val?predef || !val.predef) then "local " else ""}${key} = ${val.code}"
            ) kvs)}
            ${
              compileStmt
                (pushScope (length vals) state)
                (APPLY func (map (x: x.expr) values))
            }''
        )
        arg1
        arg2;
    };
  };
in
{
  options = {
    notlua = lib.mkOption {
      # type = lib.types.unspecified;
      description = "NotLua functions. TODO: docs";
    };
  };
  config = {
    inherit notlua;
  };
}
