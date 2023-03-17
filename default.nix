{ pkgs, lib, ... }:
let
  # add a single ident level to code
  identLines = lines: builtins.concatStringsSep "\n" (map (x: "  ${x}") lines);
  ident = code: identLines (lib.splitString "\n" code);

  # convert list into pairs
  pairsv = ret: list: key: if list == [] then {
      list = ret;
      leftover = key;
    } else pairsk (ret ++ [[key (builtins.head list)]]) (builtins.tail list);
  pairsk = ret: list: if list == [] then {
      list = ret;
      leftover = null;
    } else pairsv ret (builtins.tail list) (builtins.head list);

  # list end
  end = list: builtins.elemAt list (builtins.length list - 1);
  # pop list end
  pop = list: lib.take (builtins.length list - 1) list;

  luaType = val:
    if builtins.isAttrs val && val?__kind then (
      if val?_type then val._type
      # can't know the type of arbitrary expressions!
      else null
    ) else if builtins.isList val || builtins.isAttrs val then "table"
    else if builtins.isPath val || builtins.isString val then "string"
    else if builtins.isInt val || builtins.isFloat val then "number"
    else if builtins.isNull val then "nil"
    else if builtins.isFunction val then let info = getInfo val; in (
      if info != null && info?_expr then luaType info._expr
      else if info != null && info?_stmt then luaType info._stmt
      else "function"
    ) else if builtins.isBool val then "boolean"
    else null;

  # vararg system
  getInfo = func: if builtins.isFunction func && lib.trace "ARGS" lib.traceVal (builtins.functionArgs func) == {} then (
    let ret = func {__GET_INFO = true;}; in if builtins.isAttrs ret then ret else null
  ) else null;
  isGetInfo = arg: arg == { __GET_INFO = true; };
  argsSink = key: args: finally: arg: (
    if isGetInfo arg then
      {${key} = finally args;}
    else if builtins.isAttrs arg && arg?__kind && arg.__kind == "unroll" then
      {${key} = finally (args ++ arg.list);}
    else
      argsSink key (args ++ [arg]) finally);

  # The following functions may take state: moduleName and scope
  # scope is how many variables are currently in scope
  # the count is used for generating new variable names

  pushScope = n: { moduleName, scope }: { inherit moduleName; scope = scope + n; };
  pushScope1 = pushScope 1;

  # wrap an expression in parentheses if necessary
  # probably not the best heuristics, but good enough to make the output readable
  wrapSafe = s: (builtins.match "^[-\"a-zA-Z0-9_.()]*$" s) != null;
  wrapExpr = s: if wrapSafe s then s else "(${s})";

  # Same, but for table keys
  keySafe = s: (builtins.match "^[a-zA-Z_][_a-zA-Z0-9]*$" s) != null;
  wrapKey = scope: s: if keySafe s then s else "[${result.utils.compileExpr scope s}]";

  applyVars' = origScope: count: prefix: let self = (scope: func: argc:
  let info = getInfo func; in (
    if count != null && scope == (origScope + count) then { result = func; }
    else if count == null && !builtins.isFunction func then { result = func; inherit argc; }
    # else if info != null && info?_expr then { result = info._expr; inherit argc; }
    else if info != null && info?_stmt then { result = info._stmt; inherit argc; }
    else self (scope + 1) (let
      args = builtins.functionArgs func;
      name = "${prefix}${builtins.toString scope}"; in
        if args == {} then func (result.keywords.RAW name)
        else func (builtins.mapAttrs (k: v: result.keywords.RAW "${name}.${k}") args)) (argc + 1))
  ); in self;
  applyVars = count: prefix: scope: func: applyVars' scope count prefix scope func 0;

  result = rec {
    utils = rec {
      compileFunc = state@{moduleName, scope}: id: expr:
      (let info = getInfo expr; in
      if info != null && info?_expr then compileFunc state id info._expr
      else if info != null && info?_stmt then compileFunc state id info._stmt
      else let
        res = applyVars null "${moduleName}_arg" scope expr;
        argc = res.argc;
        func = res.result;
        header = if id == "" then "function" else "local function ${id}";
      in ''
        ${header}(${builtins.concatStringsSep ", " (builtins.genList (n: "${moduleName}_arg${builtins.toString (scope + n)}") argc)})
        ${ident (compileStmt (pushScope argc state) func)}
        end'');

      compileExpr = state: func: (
        if builtins.isString func then builtins.toJSON func
        else if builtins.isInt func then builtins.toString func
        else if builtins.isFloat func then builtins.toString func
        else if builtins.isBool func then (if func then "true" else "false")
        else if builtins.isNull func then "nil"
        else if builtins.isPath func then compileExpr state (builtins.toString func)
        else if builtins.isFunction func then let
          info = getInfo func; in
          if info != null && info?_name then
            info._name
          else if info != null && info?_expr then
            compileExpr state info._expr
          else if info != null && info?_stmt then
            assert false; null
          else (compileFunc state "" func)
        else if builtins.isList func then ''
          {
          ${ident (builtins.concatStringsSep "\n" (map (x: (compileExpr state x) + ";" ) func))}
          }''
        else if builtins.isAttrs func && func?_expr then compileExpr state func._expr
        else if builtins.isAttrs func && !(func?__kind) then ''
          {
          ${ident (builtins.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${wrapKey state k} = ${compileExpr state v};") func))}
          }''
        else if func.__kind == "raw" then
          "${func._name}"
        else if func.__kind == "op2" then
          builtins.concatStringsSep " ${func.op} " (map (x: wrapExpr (compileExpr state x)) func.args)
        else if func.__kind == "op1" then
          "${func.op}${wrapExpr (compileExpr state func.expr)}"
        else if func.__kind == "defun" then
          (compileFunc state (if func?id then func.id else "") func.func)
        else if func.__kind == "prop" then
          assert lib.assertMsg (luaType func.expr == null || luaType func.expr == "table") "Unable to get property ${func.name} of a ${luaType func.expr}!";
          "${wrapExpr (compileExpr state func.expr)}.${func.name}"
        else if func.__kind == "call" then
          let args = func._args; in
          assert lib.assertMsg
            ((!(func._func?_minArity) || (builtins.length args) >= func._func._minArity) && (!(func._func?_maxArity) || (builtins.length args) <= func._func._maxArity))
            "error: wrong function arity for ${compileExpr state func._func}! expected at least ${builtins.toString func._func._minArity}; found ${builtins.toString (builtins.length args)}";
          "${wrapExpr (compileExpr state func._func)}(${builtins.concatStringsSep ", " (map (compileExpr state) args)})"
        else if func.__kind == "mcall" then
          "${wrapExpr (compileExpr state func.val)}:${func.name}(${builtins.concatStringsSep ", " (map (compileExpr state) func.args)})"
        else if func.__kind == "tableAttr" then
          assert lib.assertMsg (luaType func.table == null || luaType func.table == "table") "Unable to get table value ${compileExpr state func.key} of a ${luaType func.table} ${compileExpr state func.table}!";
          "${wrapExpr (compileExpr state func.table)}[${compileExpr state func.key}]"
          else if func.__kind == "custom" then
            let res = func.callback state; in compileExpr res.state res.result
        else assert lib.assertMsg false "Invalid expression kind ${func.__kind}"; null
      );

      compileStmt = state@{moduleName,scope}: func: (
        if builtins.isList func then builtins.concatStringsSep "\n" (map (compileStmt state) func)
        else if builtins.isAttrs func && func?_stmt then compileStmt state func._stmt
        else if builtins.isAttrs func && (func?__kind) then (
          if func.__kind == "assign" then
            assert lib.assertMsg
              (luaType func.expr == null || luaType func.val == null || luaType func.val == func.expr._type)
              "error: setting ${compileExpr state func.expr} to wrong type. It should be ${luaType func.expr} but is ${luaType func.val}";
            "${compileExpr state func.expr} = ${compileExpr state func.val}"
          else if func.__kind == "let" then ''
            ${builtins.concatStringsSep "\n" (lib.imap0 (n: val:
            "local ${moduleName}_var${builtins.toString (scope + n)} = ${
              compileExpr state val
            }") func.vals)}
            ${
              let res = applyVars (builtins.length func.vals) "${moduleName}_var" scope func.func; in
              compileStmt (pushScope (builtins.length func.vals) state) res.result
            }''
          else if func.__kind == "letrec" then let argc = builtins.length func.vals; in ''
            ${builtins.concatStringsSep "\n" (lib.imap0 (n: val:
            "local ${moduleName}_var${builtins.toString (scope + n)} = ${
              let res = applyVars argc "${moduleName}_var" scope val; in
              compileExpr (pushScope argc state) res.result
            }") func.vals)}
            ${
              let res = applyVars argc "${moduleName}_var" scope func.func; in
              compileStmt (pushScope (builtins.length func.vals) state) res.result
            }''
          else if func.__kind == "for" then let
            res = applyVars func.argc "${moduleName}_var" scope func.body;
            varNames = builtins.genList (n: "${moduleName}_var${builtins.toString (scope + n)}") func.argc;
            in ''
              for ${builtins.concatStringsSep "," varNames} in ${compileExpr scope func.expr} do
              ${
                ident (compileStmt (pushScope1 state) res.result)
              }
              end''
          else if func.__kind == "return" then
            "return ${compileExpr state func.expr}"
          else if func.__kind == "if" then
            (lib.removeSuffix "else" ((builtins.concatStringsSep "" (map
              (cond: ''
                if ${compileExpr state (builtins.elemAt cond 0)} then
                ${ident (compileStmt state (builtins.elemAt cond 1))}
                else'')
              func.conds))
            + (if func.fallback != null then "\n${ident (compileStmt state func.fallback)}\n" else ""))) + "end"
          else if func.__kind == "custom" then
            let res = func.callback state; in compileStmt res.state res.result
          else compileExpr state func
        ) else if builtins.isFunction func then (let
          info = getInfo func; in
          if info != null && info?_stmt then compileStmt state info._stmt
          else compileExpr state func
        ) else compileExpr state func
      );

      # compile a module
      compile = moduleName: input: (compileStmt { inherit moduleName; scope = 1; } input) + "\n";
    };

    # "type definitions" for neovim
    # you can override neovim-unwrapped too 
    neovim = let kw = keywords; in attrs@{ plugins ? [], extraLuaPackages ? (_: []) }: rec {
      stdlib = pkgs.callPackage ./nvim (attrs // {
        inherit plugins extraLuaPackages;
        inherit isGetInfo;
        inherit (kw) CALL RAW;
        inherit (utils) compileExpr;
      });
      keywords = let
        reqletGen = names: func:
          if names == [] then func
          else result: reqletGen (builtins.tail names) (func (stdlib._reqlet (builtins.head names) result._name)); in {
        inherit (stdlib) REQ REQ';
        REQLET =
          argsSink "_stmt" [] (args: {
            __kind = "let";
            vals = map stdlib.require (pop args);
            func = reqletGen (map (x: "require(\"${x}\")") (pop args)) (end args);
          });
        REQLET' =
          argsSink "_stmt" [] (args: keywords.MACRO (state: {
            __kind = "let";
            vals = pop args;
            func = reqletGen (map (utils.compileExpr state) (pop args)) (end args);
          }));
      };
    };

    keywords = rec {
      # pass some raw code to lua directly
      RAW = name: { __kind = "raw"; _name = name; };

      # Access a property
      # Corresponding lua code: table.property
      # expr -> identifier -> expr
      PROP = expr: name: { __kind = "prop"; inherit expr name; };

      # Escape a list so it can be passed to vararg functions
      UNROLL = list: { __kind = "unroll"; inherit list; };

      # Apply a list of arguments to a function/operator (probably more useful than the above)
      APPLY = func: list: func (UNROLL list);

      # Call a function
      # Useful if you need to call a zero argument function, or if you need to handle some weird metatable stuff
      # corresponding lua code: someFunc()
      # expr -> arg1 -> ... -> argN -> expr
      CALL = func: argsSink "_expr" [] (args: { __kind = "call"; _func = func; _args = args; });

      # Call a method
      # corresponding lua code: someTable:someFunc()
      # expr -> identifier -> arg1 -> ... -> argN -> expr
      MCALL = val: name: argsSink "_expr" [] (args: { __kind = "mcall"; inherit val name args; });

      # corresponding lua code: =
      # expr -> expr -> stmt
      SET = expr: val: { __kind = "assign"; inherit expr val; };

      OP1 = op: expr: { __kind = "op1"; inherit expr; };
      NOT = OP1 "~";

      # opName -> expr1 -> ... -> exprN -> expr
      OP2 = op: argsSink "_expr" [] (args: { __kind = "op2"; inherit op args; });

      # The following all have the signature
      # expr1 -> ... -> exprN -> expr
      EQ = OP2 "==";
      NE = OP2 "~=";
      GT = OP2 ">";
      LT = OP2 "<";
      GE = OP2 ">=";
      LE = OP2 "<=";
      AND = OP2 "and";
      OR = OP2 "or";

      ADD = OP2 "+";
      SUB = OP2 "+";
      MUL = OP2 "*";
      DIV = OP2 "/";

      # Corresponding lua code: for ... in ...
      # argc -> expr -> (expr1 -> ... -> exprN -> stmts) -> stmts
      FORIN = argc: expr: body: { __kind = "for"; inherit argc expr body; };
      FORIN1 = FORIN 1;
      FORIN2 = FORIN 2;
      FORIN3 = FORIN 3;

      # Issues a return statement
      # Corresponding lua code: return
      # expr -> stmt
      RETURN = expr: { __kind = "return"; inherit expr; };

      # Creates a zero argument function with user-provided statements
      # stmts -> expr
      DEFUN = func: { __kind = "defun"; inherit func; };

      # Corresponding lua code: if then (else?)
      # [[cond expr]] -> fallbackExpr? -> stmts
      IFELSE' = conds: fallback: { __kind = "if"; inherit fallback; conds = if builtins.isList (builtins.elemAt conds 0) then conds else [conds]; };

      # Corresponding lua code: if then (else?)
      # (expr -> stmts ->)* (fallback expr ->)? stmts
      IF = argsSink "_stmt" [] (args:
        let pairs = pairsk [] args; in
        if pairs.leftover == null && builtins.length pairs.list > 1 && builtins.elemAt (end pairs.list) 0 == ELSE
        then IFELSE' (pop pairs.list) (builtins.elemAt (end pairs.list) 1)
        else IFELSE' pairs.list pairs.leftover
      );

      # Signifies the fallback branch in IF. May only be the last branch.
      # Note that you may also omit it and just include the last branch without a preceding condition.
      ELSE = true;

      # Corresponding lua code: table[key]
      # table -> key -> expr
      ATTR = table: key: { __kind = "tableAttr"; inherit table key; };

      # Creates variables and passes them to the function
      # Corresponding lua code: local ... = ...
      # expr1 -> ... -> exprN -> (expr1 -> ... -> exprN -> stmt) -> stmt
      LET = argsSink "_stmt" [] (args: { __kind = "let"; vals = pop args; func = end args; });

      # Creates variables and passes them to the function as well as variable binding code
      # Corresponding lua code: local ... = ...
      # ((expr1 -> ... -> exprN) ->)* (expr1 -> ... -> exprN -> stmt) -> stmt
      LETREC = argsSink "_stmt" [] (args: { __kind = "letrec"; vals = pop args; func = end args; });

      # Process arbitrary code during compilation to be able to access state
      # (state -> { result = (stmt|expr), state = new state }) -> (stmt|expr)
      MACRO = callback: { __kind = "custom"; inherit callback; };
    };
  }; in {
    options = {
      notlua = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        description = "NotLua functions. TODO: docs";
      };
    };
    config = {
      notlua = result;
    };
  }
