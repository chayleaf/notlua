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

  initTable = table: IF (NOT table) (SET table { });
  setTable = table: lib.mapAttrsToList (k1: v1: SET (PROP table k1) v1);
  initSetTable = table: v: lib.toList (SET table { }) ++ (setTable table v);

  dump1 =
    let
      type = CALL (RAW "type");
      debug-getinfo = CALL (RAW "debug.getinfo");
    in
    (seen: dump1: dump2: k: v: path: res:
      let
        seen' = IDX seen v;
        __kind = "raw";
        _name = CAT path k;
        res' = IDX res k;
      in
      IF
        (EQ (type v) "table")
        (IF seen'
          (IF
            (AND (NOT res') (NE seen' true))
            (initSetTable res' {
              __kind = "rec";
              path = seen';
            }))
          ELSE [
          (initTable res')
          (dump2 v (CAT path k) res')
          (setTable res' {
            inherit __kind _name;
            _type = "table";
          })
        ])
        (EQ (type v) "function")
        (LET (debug-getinfo v) ({ nparams, isvararg, ... }: [
          (initSetTable res' {
            inherit __kind _name;
            _type = "function";
            _minArity = nparams;
          })
          (IF (NOT isvararg) (setTable res' {
            _maxArity = nparams;
          }))
        ]))
        ELSE
        (initSetTable res' {
          inherit __kind _name;
          _type = type v;
        }));

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

  dump12 = seen: LETREC (dump1 seen) (dump2 seen);

  dumpLuaExpr =
    let
      require = CALL (RAW "require");
      print = CALL (RAW "print");
    in
    expr: LET { } { } (require "cjson") (seen: result: { encode, ... }:
      (dump12 seen (dump1: dump2: [
        (dump1 "" expr "" result)
        (print (CALL encode (IDX result "")))
      ])));
in
{
  REQ = name: req "require(\"${name}\")";
  REQ' = code: req (utils.compileExpr { moduleName = "__req"; scope = 1; } code);
  dumpLuaExpr = expr: utils.compile "main" (dumpLuaExpr expr);
  inherit stdlib dump12 initSetTable;
}
