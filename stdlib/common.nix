{ lib
, CALL
, MACRO
, APPLY
, LET
, compileExpr
, getReqAttrs
, stdlib
}:

let
  req = name: let res = update res name (getReqAttrs name); in res;
  update = self: prefix: lib.mapAttrs (k: v: let
    v' = update self prefix v;
    in (if builtins.isAttrs v && v?__kind then (
      if v.__kind == "rec" then
        lib.attrByPath (lib.splitString "." v.path) null self
      else if v.__kind == "raw" && v._type == "function" then
        v' // {
          __functor = self: CALL self;
        }
      else v'
    ) else if builtins.isAttrs v then v'
    else if prefix != "" && k == "_name" then
      (if v == "" then prefix else "${prefix}.${v}")
    else v));
  pop = list: lib.take (builtins.length list - 1) list;
  end = list: builtins.elemAt list (builtins.length list - 1);
  _reqlet = code: varname: let res = update res varname (getReqAttrs code); in res;
  reqletGen = names: func:
    if names == [] then func
    else result: reqletGen (builtins.tail names) (func (_reqlet (builtins.head names) result._name));
  reqlet = args: {
    __functor = self: arg: reqlet (args ++ [arg]);
    __kind = "let";
    vals = map stdlib.require (pop args);
    func = reqletGen (map (x: "require(\"${x}\")") (pop args)) (end args);
  };
  reqlet' = args: {
    __functor = self: arg: reqlet' (args ++ [arg]);
  } // (MACRO (state: {
      result = APPLY
        LET
        (pop args)
        (reqletGen (map (compileExpr state) (pop args)) (end args));
      inherit state;
    }));
in {
  inherit update;
  REQ = name: req "require(\"${name}\")";
  REQ' = code: req (compileExpr { moduleName = "req"; scope = 1; } code);
  REQLET = arg: reqlet [arg];
  REQLET' = arg: reqlet' [arg];
}
