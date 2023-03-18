{ lib
, stdenvNoCC
, substituteAll
, writeTextFile

, keywords
, utils

, lua
, ...
}:

# TODO: bfs instead of dfs in var dumps

let
  common = import ./common.nix { inherit lib keywords utils getExprTypeDefs typeDefs; };
  lua' = lua.withPackages (p: [ p.cjson ]);

  typeDefs = builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "lua-types.json";
    dumpProgram = writeTextFile {
      name = "dump-lua-expr.lua";
      text = common.dumpLuaExpr (keywords.RAW "_G");
    };
    nativeBuildInputs = [ lua' ];
    installPhase = ''
      export HOME="$TMPDIR"
      lua $dumpProgram > $out
    '';
  }));

  getExprTypeDefs = expr: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "lua-types-${expr}.json";
    dumpProgram = writeTextFile {
      name = "dump-lua-expr-${expr}.lua";
      text = common.dumpLuaExpr (keywords.RAW expr);
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
