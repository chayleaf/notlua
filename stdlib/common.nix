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
    (if builtins.isAttrs v && v?__kind__ then
      (if v.__kind__ == "rec" then
        lib.attrByPath (lib.splitString "." v.__name__) null self
      else if v.__kind__ == "raw" && ((v.__type__ == "function") || (v?__meta__ && v.__meta__?__call)) then
        v' // {
          __functor = keywords.CALL;
        }
      else v'
      ) else if builtins.isAttrs v then v'
    else if prefix != "" && k == "__name__" then
      (if v == "" then prefix else "${prefix}.${v}")
    else v));

  codeDefsForVar = code: varname: let res = update res varname (getExprTypeDefs code); in res;
  req = name: codeDefsForVar name name;

  stdlib = update stdlib "" typeDefs;

  inherit (keywords) LET LETREC RAW CALL IF EQ NE IDX NOT AND SET ELSE PROP CAT FORIN OR;

  initTable = table: IF (NOT table) (SET table { });
  setTable = table: lib.mapAttrsToList (k1: v1: SET (PROP table k1) v1);
  initSetTable = table: v: lib.toList (initTable table) ++ (setTable table v);
  forceInitSetTable = table: v: lib.toList (SET table { }) ++ (setTable table v);

  dump1 =
    let
      type = CALL (RAW "type");
      debug-getinfo = CALL (RAW "debug.getinfo");
      getmetatable = CALL (RAW "getmetatable");
    in
    (seen: dump1: dump2: k: v: path: res:
      let
        seen' = IDX seen v;
        __kind__ = "raw";
        __name__ = CAT path k;
        res' = IDX res k;
      in
      [
        # dump metatable
        (IF (AND (NE (type v) "string") (NE (getmetatable v) null))
          (LET [ ] (getmetatable v) (keys: metatable: [
            (initTable res')
            (IF (EQ (type (PROP metatable "__index")) "table")
              (dump2 (PROP metatable "__index") __name__ res'))
            (setTable res' {
              __meta__ = { };
            })
            (dump2 metatable (CAT path k "!!meta") (PROP res' "__meta__"))
          ])))
        (IF
          (EQ (type v) "table")
          (IF seen'
            (IF
              (AND (OR (NOT res') (NOT (PROP res' "__kind__"))) (NE seen' true))
              (forceInitSetTable res' {
                __kind__ = "rec";
                __name__ = seen';
              }))
            ELSE [
            (initTable res')
            (dump2 v __name__ res')
            (setTable res' {
              inherit __kind__ __name__;
              __type__ = "table";
            })
          ])
          (EQ (type v) "function")
          (LET (debug-getinfo v) ({ nparams, isvararg, ... }: [
            (initSetTable res' {
              inherit __kind__ __name__;
              __type__ = "function";
              __minArity__ = nparams;
            })
            (IF (NOT isvararg) (setTable res' {
              __maxArity__ = nparams;
            }))
          ]))
          ELSE
          (initSetTable res' {
            inherit __kind__ __name__;
            __type__ = type v;
          }))
      ]);

  dump2 =
    let
      pairs = CALL (RAW "pairs");
      tostring = CALL (RAW "tostring");
      type = CALL (RAW "type");
    in
    (seen: dump1: dump2: t: path: res: [
      (SET (IDX seen t) path)
      (IF (NE path "") (SET path (CAT path ".")))
      (FORIN (pairs t) (k: v:
        let k' = tostring k; in
        (IF (AND (OR (NE k' "__index") (NE (type v) "table")) (OR (NE path "") (NE k' "package"))) (dump1 k' v path res))))
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
