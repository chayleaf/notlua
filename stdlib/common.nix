{ lib
, keywords
, utils
, getExprTypeDefs
, typeDefs
}:

let
  update = self: prefix: lib.mapAttrs (k: v:
    let
      v' = update self prefix v;
    in
    (if builtins.isAttrs v && v?__kind then
      (if v.__kind == "rec" then
        lib.attrByPath (lib.splitString "." v.path) null self
      else if v.__kind == "raw" && v._type == "function" then
        v' // { __functor = self: self // (keywords.CALL self); }
      else v'
      ) else if builtins.isAttrs v then v'
    else if prefix != "" && k == "_name" then
      (if v == "" then prefix else "${prefix}.${v}")
    else v));

  codeDefsForVar = code: varname: let res = update res varname (getExprTypeDefs code); in res;
  req = name: codeDefsForVar name name;

  stdlib = update stdlib "" typeDefs;

  inherit (keywords) LET LETREC RAW CALL IF EQ NE IDX NOT AND SET ELSE PROP CAT FORIN OR;

  initTable = res: k: IF (NOT (IDX res k)) (SET (IDX res k) { });
  forceInitTable = res: k: SET (IDX res k) { };
  setTable = res: k: v: lib.mapAttrsToList (k1: v1: SET (PROP (IDX res k) k1) v1) v;

  dump1 =
    let
      type = CALL (RAW "type");
      debug-getinfo = CALL (RAW "debug.getinfo");
    in
    (seen: dump1: dump2: k: v: path: res: IF
      (EQ (type v) "table")
      (IF (IDX seen v)
        (IF (AND (NOT (IDX res k)) (NE (IDX seen v) true)) [
          (forceInitTable res k)
          (setTable res k {
            __kind = "rec";
            path = IDX seen v;
          })
        ])
        ELSE [
        (initTable res k)
        (dump2 v (CAT path k) (IDX res k))
        (setTable res k {
          __kind = "raw";
          _type = "table";
          _name = CAT path k;
        })
      ])
      (EQ (type v) "function")
      (LET (debug-getinfo v) (info: [
        (forceInitTable res k)
        (setTable res k {
          __kind = "raw";
          _type = "function";
          _name = CAT path k;
          _minArity = PROP info "nparams";
        })
        (IF (NOT (PROP info "isvararg")) (SET (PROP (IDX res k) "_maxArity") (PROP info "nparams")))
      ]))
      ELSE [
      (forceInitTable res k)
      (setTable res k {
        __kind = "raw";
        _type = type v;
        _name = CAT path k;
      })
    ]);

  dump2 =
    let
      pairs = CALL (RAW "pairs");
      tostring = CALL (RAW "tostring");
    in
    (seen: dump1: dump2: t: path: res: [
      (SET (IDX seen t) path)
      (IF (NE path "") (SET path (CAT path ".")))
      (FORIN (pairs t) (k: v:
        let k' = tostring k; in
        (IF (OR (NE path "") (NE k' "package")) (dump1 k' v path res))))
    ]);

  dumpLuaExpr =
    let
      require = CALL (RAW "require");
      print = CALL (RAW "print");
    in
    expr: LET { } { } (require "cjson") (seen: result: cjson:
      (LETREC
        (dump1 seen)
        (dump2 seen)
        (dump1: dump2: [
          (dump1 "" expr "" result)
          (print (CALL (PROP cjson "encode") (IDX result "")))
        ])
      ));
in
{
  REQ = name: req "require(\"${name}\")";
  REQ' = code: req (utils.compileExpr { moduleName = "__req"; scope = 1; } code);
  dumpLuaExpr = expr: utils.compile "main" (dumpLuaExpr expr);
  inherit stdlib dump1 dump2 initTable forceInitTable setTable;
}
