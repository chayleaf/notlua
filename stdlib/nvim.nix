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
      inherit (keywords) LET RAW CALL IF EQ IDX ELSE PROP CAT FORIN OR LEN;
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
                  __kind__ = "raw";
                  __name__ = CAT path k;
                  __type__ = "function";
                });
              in
              IF
                (EQ (TYPE args) "table")
                (IF
                  (EQ (LEN args) 1)
                  (init {
                    __minArity__ = IDX args 1;
                  })
                  (EQ (LEN args) 2)
                  (init {
                    __minArity__ = IDX args 1;
                    __maxArity__ = IDX args 2;
                  })
                  ELSE
                  (print "ERROR"))
                (EQ (TYPE args) "number")
                (init {
                  __minArity__ = args;
                  __maxArity__ = args;
                })
                ELSE
                (init {
                  __minArity__ = 0;
                  __maxArity__ = 0;
                })
            ))
            # dump options
            ({ options, ... }: path: res: [
              (initSetTable (PROP res "bo.__entry__") {
                __kind__ = "raw";
                __type__ = "table";
                __name__ = "";
              })
              (initSetTable (PROP res "wo.__entry__") {
                __kind__ = "raw";
                __type__ = "table";
                __name__ = "";
              })
              (LET
                # types
                { bool = "boolean"; string = "string"; number = "number"; }
                # keyword tables (rather than lists)
                # yep, it's hardcoded
                { fillchars = true; listchars = true; winhighlight = true; }

                (types: kwoptions: FORIN (pairs options) (k: { full_name, abbreviation, list, type, scope, ... }:
                  let
                    k = full_name;
                    abbr = abbreviation;
                    init = key: __type__: [
                      (initSetTable (IDX (PROP res key) k) {
                        __kind__ = "raw";
                        __name__ = CAT path key "." k;
                        inherit __type__;
                      })
                      (IF (EQ (TYPE abbr) "string")
                        (initSetTable (IDX (PROP res key) abbr) {
                          __kind__ = "rec";
                          __name__ = CAT path key "." k;
                        }))
                    ];
                    init2 = key: __type__: [
                      (initSetTable (IDX (PROP (PROP res key) "__entry__") k) {
                        inherit __type__;
                        __name__ = k;
                      })
                      (IF (EQ (TYPE abbr) "string")
                        (initSetTable (IDX (PROP (PROP res key) "__entry__") abbr) {
                          __kind__ = "rec";
                          __name__ = CAT path key ".__entry__." k;
                        }))
                    ];
                    type' = IDX types type;
                  in
                  [
                    (IF (OR (IDX kwoptions k) list) [
                      (init "opt" "table")
                      (init "opt_local" "table")
                      (init "opt_global" "table")
                    ]
                      ELSE [
                      (init "opt" type')
                      (init "opt_local" type')
                      (init "opt_global" type')
                    ])
                    (init "o" type')
                    (FORIN (pairs scope) (k: v: [
                      (IF (EQ v "global")
                        (init "go" type'))
                      (IF (EQ v "buffer")
                        (init2 "bo" type'))
                      (IF (EQ v "window")
                        (init2 "wo" type'))
                    ]))
                  ])))
            ])
            (dumpf: dumpo:
              let vim = PROP result "vim"; in [
                (initSetTable vim {
                  __kind__ = "raw";
                  __type__ = "table";
                  __name__ = "vim";
                })
                (FORIN (pairs (RAW "vim._submodules")) (k: [
                  (initSetTable (IDX vim k) {
                    __kind__ = "raw";
                    __type__ = "table";
                    __name__ = CAT "vim." k;
                  })
                  (dump2 (IDX (RAW "vim") k) (CAT "vim." k) (IDX vim k))
                ]))
                (dump2 (IDX (RAW "package.loaded") "vim.shared") "vim" vim)
                (dump2 (IDX (RAW "package.loaded") "vim._editor") "vim" vim)
                (dump2 (RAW "_G") "" result)
                (dumpf (require "eval") "vim.fn." (PROP vim "fn"))
                (dumpo (require "options") "vim." vim)
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
