{ pkgs, lib, ... }:
let
  catLines = builtins.concatStringsSep "\n";
  catComma = builtins.concatStringsSep ",";
  catComma' = builtins.concatStringsSep ", ";

  # add a single ident level to code
  identLines = lines: catLines (map (x: "  ${x}") lines);
  ident = code: identLines (lib.splitString "\n" code);

  updateNames = new_name: attrs:
    if attrs?_name then
      lib.mapAttrsRecursive (path: value: if lib.last path == "_name" then new_name + (lib.removePrefix attrs._name value) else value) attrs
    else
      attrs;

  changeName = val: name:
    let type = luaType val; in if type == null || !(type?_type) then notlua.keywords.RAW name
    else if type._type == "function" then type // { __functor = self: self // (notlua.keywords.CALL self); } // (notlua.keywords.RAW name)
    else if type._type == "table" then (updateNames name val) // (notlua.keywords.RAW name)
    else type // (notlua.keywords.RAW name);

  luaType = val:
    if builtins.isAttrs val && val?_type && val._type != null then lib.filterAttrs (k: v: k == "_type" || k == "_minArity" || k == "_maxArity") val
    else if builtins.isAttrs val && val?__kind then null
    else if builtins.isList val || builtins.isAttrs val then { _type = "table"; }
    else if builtins.isPath val || builtins.isString val then { _type = "string"; }
    else if builtins.isInt val || builtins.isFloat val then { _type = "number"; }
    else if builtins.isNull val then { _type = "nil"; }
    else if builtins.isFunction val then { _type = "function"; }
    else if builtins.isBool val then { _type = "boolean"; }
    else null;

  luaTypeNoFunctors = val:
    if builtins.isAttrs val && val?__functor then null
    else luaType val;

  printType = val:
    let type = luaType val; in if type == null || !(builtins.isAttrs type) || !(type?_type) || type._type == null then "unknown" else type._type;
  printTypeNoFunctors = val:
    let type = luaTypeNoFunctors val; in if type == null || !(builtins.isAttrs type) || !(type?_type) || type._type == null then "unknown" else type._type;

  checkType = type1: type2: if type1 == null || type2 == null then true else
  (builtins.all
    (x: builtins.length x < 2 || builtins.any (x: x == null) x || builtins.elemAt x 0 == builtins.elemAt x 1)
    (lib.attrValues (lib.zipAttrs [ type1 type2 ])));

  # The following functions may take state: moduleName and scope
  # scope is how many variables are currently in scope
  # the count is used for generating new variable names
  # moduleName will additionally get changed based on the branch

  pushScope = n: attrs@{ scope, ... }: attrs // { scope = scope + n; };
  pushScope1 = pushScope 1;
  pushName = suffix: attrs@{ moduleName, ... }: attrs // { moduleName = moduleName + (builtins.toString suffix); };

  # wrap an expression in parentheses if necessary
  # probably not the best heuristics, but good enough to make the output readable
  wrapSafe = s: (builtins.match "^[-\"a-zA-Z0-9_.()]*$" s) != null;
  wrapExpr = s: if wrapSafe s then s else "(${s})";

  # Same, but for table keys
  keySafe = s: (builtins.match "^[a-zA-Z_][_a-zA-Z0-9]*$" s) != null;
  wrapKey = scope: s: if keySafe s then s else "[${notlua.utils.compileExpr scope s}]";

  # Create a variable name with a prefix and scope
  varName = prefix: scope: "${prefix}${builtins.toString scope}";

  # Apply variable to a function
  applyRawVar = func: var:
    let
      args = builtins.functionArgs func;
      raw = if builtins.isAttrs var && var?_name then var._name else builtins.toString var;
      getKeyRaw = k: notlua.keywords.RAW "${raw}.${k}";
      getKey = if builtins.isAttrs var then (k: if builtins.hasAttr k var then builtins.getAttr k var else getKeyRaw k) else getKeyRaw;
      nonraw = if builtins.isString var then (notlua.keywords.RAW var) else var;
    in
    if args == { } then func nonraw
    else func (builtins.mapAttrs (k: v: getKey k) args);

  # This function is mostly useful when you don't know how many variables a user-provided function takes.
  # It takes variable count (can be null), prefix and scope (prefix should be derived from module name), as
  # well as the function itself, creates new variables with your prefix and applies them to the function until
  # it stops being a function (e.g. if it becomes a functor, like vararg functions). It returns an attrset with
  # the `result` value, which contains the end result of function applcation. If you haven't provided `count`,
  # it also returns `argc` attr, containing the argument count of the function.
  applyVars = count: prefix: scope: func: applyVars' scope count prefix scope func 0;
  applyVars' = origScope: count: prefix:
    let
      self = (scope: func: argc:
        if count != null && scope == (origScope + count) then { result = func; }
        else if count == null && !(builtins.isFunction func) then { result = func; inherit argc; }
        else self (scope + 1) (applyRawVar func (varName prefix scope)) (argc + 1));
    in
    self;

  notlua = rec {
    utils = rec {
      inherit applyRawVar applyVars wrapKey wrapExpr varName pushName pushScope checkType luaType ident;

      # compile a function. First argument is state, second argument is function name, whether it's vararg,
      # and if it has a name, whether it should be local; third argument is function itself
      compileFunc = state@{ moduleName, scope, ... }: { name ? "", var ? false, local ? true, ... }: func:
        let
          prefix = "${moduleName}_arg";
          res' = applyVars null prefix scope func;
          argc' = res'.argc;
          argc = if var then argc' - 1 else argc';
          res = if var then ((applyVars argc prefix scope func).result (keywords.RAW "arg")) else res'.result;
          header = if name == "" then "function" else (if local then "local " else "") + "function ${name}";
        in
        ''
          ${header}(${catComma' (
            builtins.genList (n: "${varName prefix (scope + n)}") argc
            ++ (lib.optional var "...")
          )})
          ${ident (compileStmt (pushScope argc state) res)}
          end'';

      compileExpr = state: expr:
        if builtins.isString expr then builtins.toJSON expr
        else if builtins.isInt expr then builtins.toString expr
        else if builtins.isFloat expr then builtins.toString expr
        else if builtins.isBool expr then lib.boolToString expr
        else if builtins.isNull expr then "nil"
        else if builtins.isPath expr then compileExpr state (builtins.toString expr)
        else if builtins.isFunction expr then (compileFunc state { } expr)
        else if builtins.isList expr then
          (if expr == [ ] then "{}" else ''
            {
            ${ident (catLines (map (x: (compileExpr state x) + ";" ) expr))}
            }'')
        else if !(expr?__kind) then
          let
            lists = notlua.keywords.LIST_PART expr;
            attrs = notlua.keywords.ATTR_PART expr;
          in
          (if lists == [ ] && attrs == { } then "{}" else "{"
            + (if lists == [ ] then "" else "\n" + (ident (catLines (map (x: (compileExpr state x) + ";") lists))))
            + (if attrs == { } then "" else "\n" + (ident (catLines (lib.mapAttrsToList (k: v: "${wrapKey state k} = ${compileExpr state v};") attrs))))
            + "\n}")
        else if expr.__kind == "raw" then
          "${expr._name}"
        else if expr.__kind == "custom" then
          expr.__callback (expr // { self = expr; inherit state; })
        else builtins.throw "Invalid expression kind ${expr.__kind}";

      compileStmt = state@{ moduleName, scope, ... }: stmt:
        if builtins.isList stmt then
          catLines (lib.imap0 (i: compileStmt (pushName i state)) stmt)
        else if builtins.isAttrs stmt && (stmt?__kind) && stmt.__kind == "customStmt" then
          stmt.__callback (stmt // { self = stmt; inherit state; })
        else if builtins.isAttrs stmt && (stmt?__kind) && stmt.__kind == "rawStmt" then
          "${stmt._name}"
        else compileExpr state stmt;

      # compile a module
      compile = moduleName: input: (compileStmt { inherit moduleName; scope = 1; } input) + "\n";
    };

    # "type definitions"
    # output: an attrset with stdlib and keywords (keywords contains REQ, REQ')
    neovim = attrs@{ neovim-unwrapped ? null, plugins ? [ ], extraLuaPackages ? (_: [ ]) }:
      pkgs.callPackage ./stdlib/nvim.nix (attrs // {
        inherit plugins extraLuaPackages;
        inherit (keywords) CALL LMACRO;
        inherit (utils) compileExpr wrapExpr;
      });

    lua = attrs@{ lua ? null }:
      pkgs.callPackage ./stdlib/lua.nix (attrs // {
        inherit (keywords) CALL LMACRO;
        inherit (utils) compileExpr wrapExpr;
      });

    keywords = let inherit (utils) compileExpr compileFunc compileStmt; in rec {
      # pass some raw code to lua directly
      # string -> expr&stmt
      RAW = name: { __kind = "raw"; _name = name; };
      # raw statement
      RAW' = name: { __kind = "rawStmt"; _name = name; };

      LIST_PART = list:
        if builtins.isList list then list
        else if builtins.isAttrs list && list?__list then list.__list
        else if builtins.isAttrs list then [ ]
        else throw "this isn't a table";

      ATTR_PART = attrs:
        if builtins.isList attrs then { }
        else lib.filterAttrs (k: v: k != "__list") attrs;

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
        else attrs // { __list = lists; };

      # Access a property
      # Corresponding lua code: table.property
      # expr -> string -> expr
      PROP = expr: name: (EMACRO ({ state, ... }:
        assert lib.assertMsg (checkType (luaTypeNoFunctors expr) (luaType { })) "Unable to get property ${name} of a ${printTypeNoFunctors expr}!";
        compileExpr state (UNSAFE_PROP expr name)))
      // (if builtins.isAttrs expr && expr?_name && builtins.hasAttr name expr then builtins.getAttr name expr else { });
      UNSAFE_PROP = expr: name: EMACRO ({ state, ... }:
        "${wrapExpr (compileExpr state expr)}.${name}");

      # Apply a list of arguments to a function/operator
      APPLY = builtins.foldl' lib.id;

      # Call something
      # Useful if you need to call a zero argument function, or if you need to handle some weird metatable stuff
      # corresponding lua code: someFunc()
      # expr -> arg1 -> ... -> argN -> expr
      CALL = func: EMACRO' ({ args, state, ... }:
        assert lib.assertMsg
          (!(builtins.elem (printType func) [ "number" "boolean" "nil" "string" ]))
          ("Calling a ${printType args} (${compileExpr state func}) might be a bad idea! "
            + "If you still want to do it, use UNSAFE_CALL instead of CALL");
        assert lib.assertMsg
          ((!(func?_minArity) || (builtins.length args) >= func._minArity)
            &&
            (!(func?_maxArity) || (builtins.length args) <= func._maxArity))
          ("error: wrong function arity for ${compileExpr state func}! "
            + "expected at least ${builtins.toString func._minArity}; "
            + (if func?_maxArity then "at most ${builtins.toString func._maxArity}; " else "")
            + "found ${builtins.toString (builtins.length args)}");
        compileExpr state (APPLY (UNSAFE_CALL func) args)
      );
      UNSAFE_CALL = func: EMACRO' ({ args, state, ... }:
        "${wrapExpr (compileExpr state func)}(${catComma' (map (compileExpr state) args)})"
      );

      # Call a method
      # corresponding lua code: someTable:someFunc()
      # expr -> identifier -> arg1 -> ... -> argN -> expr
      MCALL = val: name: EMACRO' ({ args, state, ... }:
        assert lib.assertMsg
          (builtins.elem (printType val) [ null "unknown" "table" ])
          ("Calling a method of a ${printType val} (${compileExpr state val}) might be a bad idea! "
            + "If you still want to do it, use UNSAFE_MCALL instead of MCALL");
        compileExpr state (APPLY (UNSAFE_MCALL val name) args));
      UNSAFE_MCALL = val: name: EMACRO' ({ args, state, ... }:
        "${wrapExpr (compileExpr state val)}:${name}(${catComma' (map (compileExpr state) args)})");

      # Call a property
      # corresponding lua code: someTable.someFunc()
      PCALL = val: name: EMACRO' ({ args, state, ... }:
        assert lib.assertMsg
          (builtins.elem (printType val) [ null "unknown" "table" ])
          ("Calling a property of a ${printType val} (${compileExpr state val}) might be a bad idea! "
            + "If you still want to do it, use UNSAFE_PCALL instead of PCALL");
        compileExpr state (APPLY (UNSAFE_PCALL val name) args));
      UNSAFE_PCALL = val: name: EMACRO' ({ args, state, ... }:
        "${wrapExpr (compileExpr state val)}.${name}(${catComma' (map (compileExpr state) args)})");

      # corresponding lua code: a = b
      # expr -> expr -> stmt
      SET = expr: val: SMACRO'
        ({ args, state, ... }:
          assert lib.assertMsg
            (checkType (luaType expr) (luaTypeNoFunctors val))
            "error: setting ${compileExpr state expr} to wrong type. It should be ${printType expr} but is ${printTypeNoFunctors val}";
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

      # opName -> expr -> expr
      OP1' = type: types: op: expr:
        (OP1 op expr)
        // { types = types ++ [ null "unknown" ]; }
        // (if type != null then { _type = type; } else { });
      OP1 = op: expr: EMACRO ({ state, types ? [ ], ... }:
        assert lib.assertMsg
          (types == [ ] || builtins.elem (printTypeNoFunctors expr) types)
          ("Trying to apply `${op}` to an expression ${compileExpr state expr} of type ${printTypeNoFunctors expr}! "
            + "If that's what you intended, try OP1 \"${op}\" <expr> instead.");
        "${op}${wrapExpr (compileExpr state expr)}");

      # The following operators have the signature
      # expr -> expr
      LEN = OP1' "number" [ "string" "table" ] "#";
      NOT = OP1 "boolean" [ "boolean" ] "not ";
      UNM = OP1' "number" [ "number" ] "-";
      BITNOT = OP1 "number" [ "number" ] "~";

      # opName -> expr1 -> ... -> exprN -> expr
      OP2' = type: types: op: arg1: arg2:
        (OP2 op arg1 arg2)
        // (if types != null then { types = types ++ [ null "unknown" ]; } else { })
        // (if type != null then { _type = type; } else { });
      OP2 = op: arg1: arg2: EMACRO'
        ({ args, state, types ? [ ], ... }:
          assert lib.assertMsg
            (types == [ ] || (builtins.all (lib.flip builtins.elem types) (map printTypeNoFunctors args)))
            ("Trying to apply `${op}` to expressions ${catComma' (map (compileExpr state) args)} of types "
              + "${catComma' (map printTypeNoFunctors args)}! If that's what you intended, try OP2 \"${op}\" <exprs> instead");
          builtins.concatStringsSep " ${op} " (map (x: wrapExpr (compileExpr state x)) args))
        arg1
        arg2;

      # The following all have the signature
      # expr1 -> ... -> exprN -> expr
      EQ = OP2' "boolean" null "==";
      NE = OP2' "boolean" null "~=";
      GT = OP2' "boolean" [ "number" "string" "table" ] ">";
      LT = OP2' "boolean" [ "number" "string" "table" ] "<";
      GE = OP2' "boolean" [ "number" "string" "table" ] ">=";
      LE = OP2' "boolean" [ "number" "string" "table" ] "<=";
      AND = OP2 "and";
      OR = OP2 "or";

      CAT = OP2' "string" null "..";
      ADD = OP2' "number" [ "number" ] "+";
      SUB = OP2' "number" [ "number" ] "-";
      MUL = OP2' "number" [ "number" ] "*";
      DIV = OP2' "number" [ "number" ] "/";
      MOD = OP2' "number" [ "number" ] "%";
      POW = OP2' "number" [ "number" ] "^";

      # Corresponding lua code: for ... in ...
      # argc -> expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN' = argc: expr: body: SMACRO ({ state, ... }:
        let
          prefix = "${state.moduleName}_var";
          res = applyVars argc prefix state.scope body;
          argc' = if argc != null then argc else res.argc;
          varNames = builtins.genList (n: "${varName prefix (state.scope + n)}") argc';
        in
        ''
          for ${catComma varNames} in ${compileExpr state expr} do
          ${ident (compileStmt (pushScope argc state) res.result)}
          end'');

      # expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN = FORIN' null;

      FORRANGE = arg1: arg2: arg3: SMACRO'
        ({ args, state, ... }:
          let
            vars =
              assert lib.assertMsg
                (builtins.all (x: builtins.elem (printTypeNoFunctors x) [ "unknown" "number" ]) args)
                ("Error: trying to use FORRANGE on values of types ${catComma' (map printTypeNoFunctors args)}! "
                  + "Please don't do that.");
              if builtins.length args == 3 || builtins.length args == 4 then
                map (compileExpr state) (lib.init args)
              else throw "for range can only receive 3 or 4 arguments";
            body = lib.last args;
            prefix = "${state.moduleName}_var";
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
      DEFUN = func: EMACRO ({ state, ... }: (compileFunc state { } func));
      # Creates a vararg functions (last argument will be the hidden `arg` lua variable)
      # stmts -> expr
      DEFUN_VAR = func: EMACRO ({ state, ... }: (compileFunc state { var = true; } func));

      # Corresponding lua code: if then (else?)
      # (expr -> stmts ->)* (fallback expr ->)? stmts
      IF = expr: stmt: SMACRO'
        ({ args, state, ... }:
          let
            data =
              if (builtins.length args) / 2 * 2 != (builtins.length args) then {
                branches = lib.init args;
                fallback = lib.last args;
              } else if builtins.elemAt args ((builtins.length args) - 2) == ELSE then {
                branches = lib.take ((builtins.length args) - 2) args;
                fallback = lib.last args;
              } else {
                branches = args;
                fallback = null;
              };
          in
          (lib.removeSuffix "else" ((builtins.concatStringsSep "" (lib.imap0
            (i: x:
              if (i / 2 * 2) == i then ''
                if ${compileExpr state x} then
              ''
              else ''
                ${ident (compileStmt state x)}
                else'')
            data.branches))
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
      IDX = table: key: EMACRO ({ state, ... }:
        assert lib.assertMsg
          (checkType (luaTypeNoFunctors table) (luaType { }))
          "Unable to get key ${compileExpr state key} of a ${printType table} ${compileExpr state table}!";
        compileExpr state (UNSAFE_IDX table key));

      UNSAFE_IDX = table: key: EMACRO ({ state, ... }:
        "${wrapExpr (compileExpr state table)}[${compileExpr state key}]");

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
        map
          ({ name, value }: {
            code = compileExpr
              (pushScope (builtins.length vars) state)
              (APPLY (applyRawVar value) (map (var: (changeName var.value var.name)) vars));
            expr = changeName value name;
          })
          vars);

      # Process arbitrary code during compilation to be able to access state
      # (state -> { result = (stmt|expr), state = new state }) -> (stmt|expr)
      MACRO = isStmt: callback: {
        __kind = if isStmt then "customStmt" else "custom";
        __callback = callback;
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
            prefix = "${state.moduleName}_var";
            names = builtins.genList (i: varName prefix (state.scope + i)) (builtins.length vals);
            values = processor {
              inherit state;
              vars = (lib.zipListsWith (name: value: { inherit name value; }) names vals);
            };
          in
          ''
            ${catLines (lib.zipListsWith (key: val:
              "${if !(val?local) || val.local then "local " else ""}${key} = ${val.code}"
            ) names values)}
            ${compileStmt (pushScope (builtins.length vals) state) (APPLY (applyRawVar func) (map (x: x.expr) values))}''
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
