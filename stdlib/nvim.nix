{ lib
, stdenvNoCC
, substituteAll
, neovimUtils
, wrapNeovimUnstable

, CALL
, LMACRO
, compileExpr

, neovim-unwrapped
, plugins
, extraLuaPackages ? (_: [ ])
, ...
}:

# TODO: bfs instead of dfs in var dumps

let
  common = import ./common.nix { inherit lib CALL LMACRO compileExpr getExprTypeDefs typeDefs; };

  evalAndOptions = lua: lua.buildLuaPackage {
    pname = "neovim-options-lua-and-eval-lua";
    version = neovim-unwrapped.version;
    src = neovim-unwrapped.src;
    nativeBuildInputs = [ ];
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
    plugins = map (plugin: if plugin?plugin then { plugin = plugin.plugin; } else { inherit plugin; }) plugins;
  };
  pluginNeovim = wrapNeovimUnstable neovim-unwrapped pluginConfig;

  typeDefs = builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "neovim-types.json";
    dumpProgram = ./dump_nvim_globals.lua;
    nativeBuildInputs = [ basicNeovim ];
    installPhase = ''
      export HOME="$TMPDIR"
      nvim --headless -S $dumpProgram -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
    '';
  }));

  getExprTypeDefs = expr: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
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

in
{
  inherit (common) stdlib;
  keywords = {
    inherit (common) REQ REQ' REQLET REQLET';
  };
}
