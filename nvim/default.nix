{ stdenvNoCC
, lib
, substituteAll
, neovim-unwrapped
, neovimUtils
, wrapNeovimUnstable

, CALL
, isGetInfo
, compileExpr

, plugins
, extraLuaPackages ? []
, ... }: 

# TODO: bfs instead of dfs in var dumps

let
update = self: prefix: lib.mapAttrs (k: v: let
  v' = update self prefix v;
  in (if builtins.isAttrs v && v?__kind then (
    if v.__kind == "rec" then
      lib.attrByPath (lib.splitString "." v.path) null self
    else if v.__kind == "raw" && v._type == "function" then
        (args:
          if isGetInfo args then v'
          else CALL v' args)
    else v'
  ) else if builtins.isAttrs v then v'
  else if prefix != "" && k == "_name" then
    (if v == "" then prefix else "${prefix}.${v}")
  else v));
result = update result "" data;
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
  dumpPlugin = ./dump_nvim_globals.lua;
  nativeBuildInputs = [ basicNeovim ];
  installPhase = ''
    export HOME="$TMPDIR"
    nvim --headless -S $dumpPlugin -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
  '';
}));
getReqAttrs = name: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
  phases = [ "installPhase" ];
  name = "neovim-types-${name}.json";
  dumpPlugin = substituteAll {
    src = ./dump_plugin.lua;
    package = name;
  };
  nativeBuildInputs = [ pluginNeovim ];
  installPhase = ''
    export HOME="$TMPDIR"
    nvim --headless -S $dumpPlugin -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
  '';
}));
req = name: let res = update res name (getReqAttrs name); in res;
REQ = name: req "require(\"${name}\")";
# the code must not use external state! this can't be checked
# this could (?) be fixed with REQBIND', but I don't need it
REQ' = code: req (compileExpr { moduleName = "req"; scope = 1; } code);
_reqlet = code: varname: let res = update res varname (getReqAttrs code); in res;
in result // {
  inherit REQ REQ' _reqlet;
}
