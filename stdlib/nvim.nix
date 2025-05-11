{ lib
, stdenvNoCC
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
  readJson = file:
    let contents = builtins.readFile file;
    in assert lib.assertMsg (lib.hasPrefix "{" contents) "Invalid JSON: ${contents}"; builtins.fromJSON contents;

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
      p.rapidjson
      (evalAndOptions p)
    ];
  };
  basicNeovim = wrapNeovimUnstable neovim-unwrapped basicConfig;

  pluginConfig = neovimUtils.makeNeovimConfig {
    extraLuaPackages = p: [ p.rapidjson ] ++ (extraLuaPackages p);
    plugins = map (plugin: if plugin?plugin then { plugin = plugin.plugin; } else { inherit plugin; }) plugins;
  };
  pluginNeovim = wrapNeovimUnstable neovim-unwrapped pluginConfig;

  dumpNvimGlobals =
    let
      inherit (keywords) LET ERAW CALL IF EQ IDX ELSE PROP CAT FORIN OR LEN SET RETURN NOT;
      inherit (common) dump12 initSetTable;
      require = CALL (ERAW "require");
      print = CALL (ERAW "print");
      pairs = CALL (ERAW "pairs");
      table.insert = CALL (ERAW "table.insert");
      string.gmatch = CALL (ERAW "string.gmatch");
      TYPE = CALL (ERAW "type");
    in
    LET { } { } { } (ERAW "vim._defer_require") (require "rapidjson") (a: b: [
      (FORIN (string.gmatch b "([^.]+)") (str: SET a (IDX a str)))
      (RETURN a)
    ])
      (seen: result: extra_keys: old_defer: { encode, ... }: access: [
        (SET (ERAW "vim._defer_require") (a: b: [
          (FORIN (pairs b) (k: v: table.insert extra_keys (CAT (CAT a ".") k)))
          (RETURN (CALL old_defer a b))
        ]))
        (dump12 seen (dump1: dump2:
          LET
            # dump functions
            ({ funcs, ... }: path: res: FORIN (pairs funcs) (k: { args, ... }:
              let
                init = args: initSetTable (IDX res k) (args // {
                  __kind__ = "rawStdlib";
                  __pathStdlib__ = CAT path k;
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
                __kind__ = "rawStdlib";
                __type__ = "table";
                __pathStdlib__ = "";
              })
              (initSetTable (PROP res "wo.__entry__") {
                __kind__ = "rawStdlib";
                __type__ = "table";
                __pathStdlib__ = "";
              })
              (LET
                # types
                { bool = "boolean"; boolean = "boolean"; string = "string"; number = "number"; }
                # keyword tables (rather than lists)
                # yep, it's hardcoded
                { fillchars = true; listchars = true; winhighlight = true; }

                (types: kwoptions: FORIN (pairs options) (k: { full_name, abbreviation, list, type, scope, ... }:
                  let
                    k = full_name;
                    abbr = abbreviation;
                    init = key: __type__: [
                      (initSetTable (IDX (PROP res key) k) {
                        __kind__ = "rawStdlib";
                        __pathStdlib__ = CAT path key "." k;
                        inherit __type__;
                      })
                      (IF (EQ (TYPE abbr) "string")
                        (initSetTable (IDX (PROP res key) abbr) {
                          __kind__ = "rec";
                          __pathStdlib__ = CAT path key "." k;
                        }))
                    ];
                    init2 = key: __type__: [
                      (initSetTable (IDX (PROP (PROP res key) "__entry__") k) {
                        inherit __type__;
                        __pathStdlib__ = k;
                      })
                      (IF (EQ (TYPE abbr) "string")
                        (initSetTable (IDX (PROP (PROP res key) "__entry__") abbr) {
                          __kind__ = "rec";
                          __pathStdlib__ = CAT path key ".__entry__." k;
                        }))
                    ];
                    type' = IDX types type;
                  in
                  [
                    (IF (OR (IDX kwoptions k) list) [
                      (init "opt" "table")
                      (init "opt_local" "table")
                      (init "opt_global" "table")
                    ] ELSE [
                      (init "opt" type')
                      (init "opt_local" type')
                      (init "opt_global" type')
                    ])
                    (init "o" type')
                    (FORIN (pairs scope) (k: v: [
                      (IF
                        (EQ v "global") (init "go" type')
                        (EQ v "buf") (init2 "bo" type')
                        (EQ v "win") (init2 "wo" type')
                        # old names
                        (EQ v "buffer") (init2 "bo" type')
                        (EQ v "windor") (init2 "wo" type')
                        ELSE [(print "unknown var type" v) (CALL (ERAW "assert") false)])
                    ]))
                  ])))
            ])
            (dumpf: dumpo:
              let vim = PROP result "vim"; in [
                (initSetTable vim {
                  __kind__ = "rawStdlib";
                  __type__ = "table";
                  __pathStdlib__ = "vim";
                })
                (FORIN (pairs (ERAW "vim._submodules")) (k: [
                  (initSetTable (IDX vim k) {
                    __kind__ = "rawStdlib";
                    __type__ = "table";
                    __pathStdlib__ = CAT "vim." k;
                  })
                  (dump2 (IDX (ERAW "vim") k) (CAT "vim." k) (IDX vim k))
                ]))
                (require "vim.shared")
                (require "vim._editor")
                (FORIN (pairs extra_keys) (k: v: access (ERAW "_G") v))
                (FORIN (pairs (ERAW "vim._submodules")) (k: v: [
                  (IF (NOT (IDX vim k)) (SET (IDX vim k) {}))
                  (dump2 (IDX (ERAW "vim") k) (CAT "vim." k) (IDX vim k))
                ]))
                (dump2 (ERAW "_G") "" result)
                (dumpf (require "eval") "vim.fn." (PROP vim "fn"))
                (dumpo (require "options") "vim." vim)
                (print (CALL encode result))
              ])))
      ]);

  typeDefs = readJson (stdenvNoCC.mkDerivation {
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
  });

  getExprTypeDefs = expr: readJson (stdenvNoCC.mkDerivation {
    phases = [ "installPhase" ];
    name = "neovim-types-${expr}.json";
    dumpProgram = writeTextFile {
      name = "dump-neovim-expr-${expr}.lua";
      text = common.dumpLuaExpr (keywords.ERAW expr);
    };
    nativeBuildInputs = [ pluginNeovim ];
    installPhase = ''
      export HOME="$TMPDIR"
      nvim --headless -S $dumpProgram -i NONE -u NONE -n -c 'echo""|qall!' 2>$out
    '';
  });

in
{
  inherit (common) stdlib;
  keywords = {
    inherit (common) REQ REQ';
  };
  screamingKeywords = {
    inherit (common) REQ REQ';
  };
  pascalKeywords = {
    Req = common.REQ;
    Req' = common.REQ';
  };
  camelKeywords = {
    req = common.REQ;
    req' = common.REQ';
  };
}
