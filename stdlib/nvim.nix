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
      inherit (keywords) LET LETREC RAW CALL IF EQ IDX NOT AND ELSE PROP CAT FORIN OR LEN;
      inherit (common) dump1 dump2 initTable setTable;
      require = CALL (RAW "require");
      print = CALL (RAW "print");
      pairs = CALL (RAW "pairs");
      type = CALL (RAW "type");
    in
    LET { } { } (require "cjson")
      (seen: result: cjson:
        (LETREC
          (dump1 seen)
          (dump2 seen)
          (dump1: dump2: LET
            # dumpf
            (t: path: res: FORIN (pairs (PROP t "funcs")) (k: v: IF
              (EQ (type (PROP v "args")) "table")
              (IF (EQ (LEN (PROP v "args")) 1) [
                (initTable res k)
                (setTable res k {
                  __kind = "raw";
                  _name = CAT path k;
                  _type = "function";
                  _minArity = IDX (PROP v "args") 1;
                })
              ]
                (EQ (LEN (PROP v "args")) 2) [
                (initTable res k)
                (setTable res k {
                  __kind = "raw";
                  _name = CAT path k;
                  _type = "function";
                  _minArity = IDX (PROP v "args") 1;
                  _maxArity = IDX (PROP v "args") 2;
                })
              ]
                ELSE
                (print "ERROR"))
              (EQ (type (PROP v "args")) "number") [
              (initTable res k)
              (setTable res k {
                __kind = "raw";
                _name = CAT path k;
                _type = "function";
                _minArity = PROP v "args";
                _maxArity = PROP v "args";
              })
            ]
              ELSE [
              (initTable res k)
              (setTable res k {
                __kind = "raw";
                _name = CAT path k;
                _type = "function";
                _minArity = 0;
                _maxArity = 0;
              })
            ]
            ))
            # dumpo
            (t: path: opt: res:
              LET
                # types
                { bool = "boolean"; string = "string"; number = "number"; }
                # keywords
                { fillchars = true; listchars = true; winhighlight = true; }

                (types: kvoptions: FORIN (pairs (PROP t "options")) (k: v:
                  let k = PROP v "full_name"; abbr = PROP v "abbreviation"; in
                  (IF (AND opt (OR (IDX kvoptions k) (PROP v "list"))) [
                    (initTable res k)
                    (setTable res k {
                      __kind = "raw";
                      _name = CAT path k;
                      _type = "table";
                    })
                    (IF (EQ (type abbr) "string") [
                      (initTable res abbr)
                      (setTable res abbr {
                        __kind = "rec";
                        path = CAT path k;
                      })
                    ])
                  ]
                    (NOT opt) [
                    (initTable res k)
                    (setTable res k {
                      __kind = "raw";
                      _name = CAT path k;
                      _type = IDX types (PROP v "type");
                    })
                    (IF (EQ (type abbr) "string") [
                      (initTable res abbr)
                      (setTable res abbr {
                        __kind = "rec";
                        path = CAT path k;
                      })
                    ])
                  ]))))
            (dumpf: dumpo: [
              (initTable result "vim")
              (setTable result "vim" {
                __kind = "raw";
                _type = "table";
                _name = "vim";
              })
              (FORIN (pairs (RAW "vim._submodules")) (k: [
                (initTable (PROP result "vim") k)
                (setTable (PROP result "vim") k {
                  __kind = "raw";
                  _type = "table";
                  _name = CAT "vim." k;
                })
                (dump2 (IDX (RAW "vim") k) (CAT "vim." k) (IDX (PROP result "vim") k))
              ]))
              (dump2 (IDX (RAW "package.loaded") "vim.shared") "vim" (PROP result "vim"))
              (dump2 (IDX (RAW "package.loaded") "vim._editor") "vim" (PROP result "vim"))
              (dump2 (RAW "_G") "" result)
              (dumpf (require "eval") "vim.fn." (PROP result "vim.fn"))
              (dumpo (require "options") "vim.o." false (PROP result "vim.o"))
              (dumpo (require "options") "vim.opt." true (PROP result "vim.opt"))
              (print (CALL (PROP cjson "encode") result))
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
