{ lib
, CALL
, LMACRO
, APPLY
, LET
, compileExpr
, compileStmt
, getReqAttrs
, stdlib
}:

let
  update = self: prefix: lib.mapAttrs (k: v: let
    v' = update self prefix v;
    in (if builtins.isAttrs v && v?__kind then (
      if v.__kind == "rec" then
        lib.attrByPath (lib.splitString "." v.path) null self
      else if v.__kind == "raw" && v._type == "function" then
        v' // { __functor = CALL; }
      else v'
    ) else if builtins.isAttrs v then v'
    else if prefix != "" && k == "_name" then
      (if v == "" then prefix else "${prefix}.${v}")
    else v));

  codeDefsForVar = code: varname: let res = update res varname (getReqAttrs code); in res;
  req = name: codeDefsForVar name name;

  REQLET' = LMACRO ({ vars, state, ...}:
    map ({ name, value, ... }: rec {
      code = compileExpr state value;
      expr = codeDefsForVar code name;
    }) vars
  );
  REQLET = LMACRO ({ vars, ... }:
    map ({ name, value, ... }: rec {
      code = "require(\"${value}\")";
      expr = codeDefsForVar code name;
    }) vars
  );
in {
  inherit update;
  REQ = name: req "require(\"${name}\")";
  REQ' = code: req (compileExpr { moduleName = "req"; scope = 1; } code);
  inherit REQLET REQLET';
}
