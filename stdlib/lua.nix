{ lib
, stdenvNoCC
, callPackage
, substituteAll

, CALL
, LMACRO
, APPLY
, LET
, compileExpr
, compileStmt

, lua
, ... }: 

# TODO: bfs instead of dfs in var dumps

let
common = callPackage ./common.nix { inherit CALL LMACRO APPLY LET compileExpr compileStmt getReqAttrs stdlib; };
inherit (common) update;
lua' = lua.withPackages (p: [ p.cjson ]);

stdlib = update stdlib "" data;
data = builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
  phases = [ "installPhase" ];
  name = "lua-types.json";
  dumpProgram = ./dump_lua_globals.lua;
  nativeBuildInputs = [ lua' ];
  installPhase = ''
    export HOME="$TMPDIR"
    lua $dumpProgram > $out
  '';
}));

getReqAttrs = expr: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
  phases = [ "installPhase" ];
  name = "lua-types-${expr}.json";
  dumpProgram = substituteAll {
    src = ./dump_expr.lua;
    inherit expr;
  };
  nativeBuildInputs = [ lua' ];
  installPhase = ''
    export HOME="$TMPDIR"
    lua $dumpProgram > $out
  '';
}));

in {
  inherit stdlib;
  keywords = {
    inherit (common) REQ REQ' REQLET REQLET';
  };
}
