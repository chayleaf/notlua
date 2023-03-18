{ lib
, stdenvNoCC
, substituteAll
, neovimUtils
, wrapNeovimUnstable
, writeTextFile

, keywords
, utils

, neovim-unwrapped
, plugins
, extraLuaPackages ? (_: [ ])
, ...
}:

# TODO: bfs instead of dfs in var dumps

let
  common = import ./common.nix { inherit lib keywords utils getExprTypeDefs typeDefs; };

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

  dumpNvimGlobals =
    let
      inherit (keywords) LET RAW CALL IF EQ IDX NOT AND ELSE PROP CAT FORIN OR LEN;
      inherit (common) dump12 initSetTable;
      require = CALL (RAW "require");
      print = CALL (RAW "print");
      pairs = CALL (RAW "pairs");
      TYPE = CALL (RAW "type");
    in
    LET { } { } (require "cjson")
      (seen: result: { encode, ... }:
        (dump12 seen (dump1: dump2:
          LET
            # dump functons
            ({ funcs, ... }: path: res: FORIN (pairs funcs) (k: { args, ... }:
              let
                init = args: initSetTable (IDX res k) (args // {
                  __kind = "raw";
                  _name = CAT path k;
                  _type = "function";
                });
              in
              IF
                (EQ (TYPE args) "table")
                (IF
                  (EQ (LEN args) 1)
                  (init {
                    _minArity = IDX args 1;
                  })
                  (EQ (LEN args) 2)
                  (init {
                    _minArity = IDX args 1;
                    _maxArity = IDX args 2;
                  })
                  ELSE
                  (print "ERROR"))
                (EQ (TYPE args) "number")
                (init {
                  _minArity = args;
                  _maxArity = args;
                })
                ELSE
                (init {
                  _minArity = 0;
                  _maxArity = 0;
                })
            ))
            # dump options
            ({ options, ... }: path: opt: res:
              LET
                # types
                { bool = "boolean"; string = "string"; number = "number"; }
                # keyword tables (rather than lists)
                # yep, it's hardcoded
                { fillchars = true; listchars = true; winhighlight = true; }

                (types: kwoptions: FORIN (pairs options) (k: { full_name, abbreviation, list, type, ... }:
                  let
                    k = full_name;
                    abbr = abbreviation;
                    init = _type: initSetTable (IDX res k) {
                      __kind = "raw";
                      _name = CAT path k;
                      inherit _type;
                    };
                    checkAbbr =
                      IF (EQ (TYPE abbr) "string")
                        (initSetTable (IDX res abbr) {
                          __kind = "rec";
                          path = CAT path k;
                        });
                  in
                  (IF
                    (AND opt (OR (IDX kwoptions k) list)) [
                    (init "table")
                    checkAbbr
                  ]
                    (NOT opt) [
                    (init (IDX types type))
                    checkAbbr
                  ]))))
            (dumpf: dumpo:
              let vim = PROP result "vim"; in [
                (initSetTable vim {
                  __kind = "raw";
                  _type = "table";
                  _name = "vim";
                })
                (FORIN (pairs (RAW "vim._submodules")) (k: [
                  (initSetTable (IDX vim k) {
                    __kind = "raw";
                    _type = "table";
                    _name = CAT "vim." k;
                  })
                  (dump2 (IDX (RAW "vim") k) (CAT "vim." k) (IDX vim k))
                ]))
                (dump2 (IDX (RAW "package.loaded") "vim.shared") "vim" vim)
                (dump2 (IDX (RAW "package.loaded") "vim._editor") "vim" vim)
                (dump2 (RAW "_G") "" result)
                (dumpf (require "eval") "vim.fn." (PROP vim "fn"))
                (dumpo (require "options") "vim.o." false (PROP vim "o"))
                (dumpo (require "options") "vim.opt." true (PROP vim "opt"))
                (print (CALL encode result))
              ]))
        ));

  typeDefs = builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "neovim-types.json";
    dumpProgram = writeTextFile {
      name = "dump-nvim-globals.lua";
      text = utils.compile "main" dumpNvimGlobals;
    };
    nativeBuildInputs = [ basicNeovim ];
    installPhase = ''
      export HOME="$TMPDIR"
      nvim --headless -S $dumpProgram -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
    '';
  }));

  getExprTypeDefs = expr: builtins.fromJSON (builtins.readFile (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "neovim-types-${expr}.json";
    dumpProgram = writeTextFile {
      name = "dump-neovim-expr-${expr}.lua";
      text = common.dumpLuaExpr (keywords.RAW expr);
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
    inherit (common) REQ REQ';
  };
}
