{ lib
, stdenvNoCC
, substituteAll

, CALL
, LMACRO
, compileExpr
, wrapExpr

, lua
, ...
}:

# TODO: bfs instead of dfs in var dumps

let
  common = import ./common.nix { inherit lib CALL LMACRO compileExpr wrapExpr getExprTypeDefs typeDefs; };
  lua' = lua.withPackages (p: [ p.cjson ]);

  typeDefs = builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "lua-types.json";
    dumpProgram = ./dump_lua_globals.lua;
    nativeBuildInputs = [ lua' ];
    installPhase = ''
      export HOME="$TMPDIR"
      lua $dumpProgram > $out
    '';
  }));

  getExprTypeDefs = expr: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
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

in
{
  inherit (common) stdlib;
  keywords = {
    inherit (common) REQ REQ';
  };
}
