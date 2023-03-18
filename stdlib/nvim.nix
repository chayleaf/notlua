{ lib
, stdenvNoCC
, callPackage
, substituteAll
, neovim-unwrapped
, neovimUtils
, wrapNeovimUnstable

, CALL
, LMACRO
, APPLY
, LET
, compileExpr
, compileStmt

, plugins
, extraLuaPackages ? (_: [])
, ... }: 

# TODO: bfs instead of dfs in var dumps

let
common = callPackage ./common.nix { inherit CALL LMACRO APPLY LET compileExpr compileStmt getReqAttrs stdlib; };
inherit (common) update;

stdlib = update stdlib "" data;
evalAndOptions = lua: lua.buildLuaPackage {
  pname = "neovim-options-lua-and-eval-lua";
  version = neovim-unwrapped.version;
  src = neovim-unwrapped.src;
  nativeBuildInputs = [];
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/lua/${lua.lua.luaversion}
    cp $src/src/nvim/{options,eval}.lua $out/share/lua/${lua.lua.luaversion}
    runHook postInstall
  '';
};
basicConfig = neovimUtils.makeNeovimConfig {
  extraLuaPackages = p: [
    p.cjson
    (evalAndOptions p)
  ];
};
basicNeovim = wrapNeovimUnstable neovim-unwrapped basicConfig;

pluginConfig = neovimUtils.makeNeovimConfig {
  extraLuaPackages = p: [ p.cjson ] ++ (extraLuaPackages p);
  plugins = map (plugin: if plugin?plugin then {plugin=plugin.plugin;} else {inherit plugin;}) plugins;
};
pluginNeovim = wrapNeovimUnstable neovim-unwrapped pluginConfig;

data = builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
  phases = [ "installPhase" ];
  name = "neovim-types.json";
  dumpProgram = ./dump_nvim_globals.lua;
  nativeBuildInputs = [ basicNeovim ];
  installPhase = ''
    export HOME="$TMPDIR"
    nvim --headless -S $dumpProgram -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
  '';
}));

getReqAttrs = expr: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
  phases = [ "installPhase" ];
  name = "neovim-types-${expr}.json";
  dumpProgram = substituteAll {
    src = ./dump_expr.lua;
    inherit expr;
  };
  nativeBuildInputs = [ pluginNeovim ];
  installPhase = ''
    export HOME="$TMPDIR"
    nvim --headless -S $dumpProgram -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
  '';
}));

in {
  inherit stdlib;
  keywords = {
    inherit (common) REQ REQ' REQLET REQLET';
  };
}
