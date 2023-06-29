{ flake, lib, pkgs }:
let
  inherit (flake.utils) compileExpr compileStmt;
  inherit (flake.screamingKeywords) ERAW SRAW PROP APPLY CALL MCALL SET OP2 AND ADD OR UNM FORIN RETURN DEFUN DEFUN_VAR IF ELSE IDX LET LETREC MACRO LT SUB CAT;
  nvim = flake.neovim { plugins = [ pkgs.vimPlugins.nvim-cmp ]; };
  lua = flake.lua { };
  defaultState = { moduleName = "m"; scope = 1; };
  chk = { stmt ? null, expr ? null, raw }:
    let result = if stmt != null then compileStmt defaultState stmt else compileExpr defaultState expr;
    in lib.assertMsg (result == raw) "Expected ${raw}, found ${result}";
  eq = a: b: lib.assertMsg (a == b) "Expected ${builtins.toJSON b}, found ${builtins.toJSON a}";
  inherit (nvim.screamingKeywords) REQ;
in
assert !(builtins.tryEval (compileStmt defaultState (nvim.stdlib.table.remove 1 2 3))).success;
assert chk
{
  expr = a: (nvim.stdlib.vim.api.nvim_buf_set_option a "omnifunc" "v:lua.vim.lsp.omnifunc");
  raw = ''
    function(m_arg1)
      vim.api.nvim_buf_set_option(m_arg1, "omnifunc", "v:lua.vim.lsp.omnifunc")
    end'';
};
assert !(builtins.tryEval (compileStmt defaultState (nvim.stdlib.table.remove 1))).success;
assert chk
{
  expr = nvim.stdlib.vim.api.nvim_create_autocmd "test" {
    group = 0;
    callback = { buf, ... }:
      (LET
        (nvim.stdlib.vim.filetype.match { inherit buf; })
        (filetype: nvim.stdlib.print filetype));
  };
  raw = ''
    vim.api.nvim_create_autocmd("test", {
      callback = function(m_arg1)
        local m_var2 = vim.filetype.match({
          buf = m_arg1.buf;
        })
        print(m_var2)
      end;
      group = 0;
    })'';
};
assert chk
{
  stmt = FORIN (ERAW "test") (k: (v: CALL (ERAW "print") k v));
  raw = ''
    for m_for1,m_for2 in test do
      print(m_for1, m_for2)
    end'';
};
assert chk
{
  expr = test: { test2, test3 }: (RETURN (ADD test test2 (UNM test3)) 5);
  raw = ''
    function(m_arg1, m_arg2)
      return m_arg1 + m_arg2.test2 + -m_arg2.test3, 5
    end'';
};
assert chk
{
  expr = ERAW "test";
  raw = "test";
};
assert chk
{
  expr = PROP (ERAW "test") "prop";
  raw = "test.prop";
};
assert chk
{
  expr = IDX (ERAW "test") "prop";
  raw = "test[\"prop\"]";
};
assert chk
{
  expr = APPLY ADD [ 0 1 2 3 ];
  raw = "0 + 1 + 2 + 3";
};
assert chk
{
  expr = AND true false (OP2 "==" false true);
  raw = "true and false and (false == true)";
};
assert chk
{
  expr = CALL (ERAW "test");
  raw = "test()";
};
assert chk
{
  expr = CALL (ERAW "test") [ 5 ] { test = 5; } 5 "5";
  raw = ''
    test({
      5;
    }, {
      test = 5;
    }, 5, "5")'';
};
assert chk
{
  expr = MCALL (ERAW "test") "method" 5 6;
  raw = "test:method(5, 6)";
};
assert chk
{
  expr = DEFUN (SRAW "test");
  raw = ''
    function()
      test
    end'';
};
assert chk
{
  stmt = SET (ERAW "test") 5;
  raw = "test = 5";
};
assert chk
{
  stmt = let a = ERAW "a"; b = ERAW "b"; in SET [ a b ] b a;
  raw = "a, b = b, a";
};
assert chk
{
  stmt = let a = ERAW "a"; b = ERAW "b"; in SET [ a b ] 1 2;
  raw = "a, b = 1, 2";
};
assert chk
{
  stmt = LET 5 (five: SET five 6);
  raw = ''
    local m_var1 = 5
    m_var1 = 6'';
};
assert chk
{
  stmt = LET 5 6 (five: six: SET [ five six ] 7 8);
  raw = ''
    local m_var1 = 5
    local m_var2 = 6
    m_var1, m_var2 = 7, 8'';
};
assert !(builtins.tryEval (compileStmt defaultState (LET 5 (five: SET five "a")))).success;
assert !(builtins.tryEval (compileStmt defaultState (LET 5 6 (five: six: SET [ five six ] 7 "a")))).success;
assert !(builtins.tryEval (compileStmt defaultState (SET 5 5))).success;
assert chk
{
  stmt = IF (ERAW "test") (RETURN 5) (ERAW "test2") (RETURN 6) (RETURN 7);
  raw = ''
    if test then
      return 5
    elseif test2 then
      return 6
    else
      return 7
    end'';
};
assert chk
{
  stmt = IF (ERAW "test") (RETURN 5) (ERAW "test2") (RETURN 6) ELSE (RETURN 7);
  raw = ''
    if test then
      return 5
    elseif test2 then
      return 6
    else
      return 7
    end'';
};
assert chk
{
  stmt = IF (AND true false) (CALL (ERAW "print") "this shouldn't happen");
  raw = ''
    if true and false then
      print("this shouldn't happen")
    end'';
};
assert chk
{
  stmt = LET (ERAW "test") (ERAW "test2") ({ test }: test2: [ (RETURN (ADD test test2)) ]);
  raw = ''
    local m_var1 = test
    local m_var2 = test2
    return m_var1.test + m_var2'';
};
assert chk
{
  stmt = LETREC (test: test2: test2) (test: test2: test) (test: test2: [ ]);
  raw = ''
    local m_var1
    local m_var2
    m_var1 = m_var2
    m_var2 = m_var1
  '';
};
assert chk
{
  stmt = MACRO ({ ... }: "this will be lua in 1976\n");
  raw = ''
    this will be lua in 1976
  '';
};
assert !(builtins.tryEval (compileExpr defaultState (ADD "" 5))).success;
assert !(builtins.tryEval (compileExpr defaultState (ADD "" (ADD 4 5)))).success;
assert !(builtins.tryEval (compileExpr defaultState (UNM ""))).success;
assert !(builtins.tryEval (compileStmt defaultState (SET nvim.stdlib.vim.o.colorcolumn 17))).success;
assert chk
{
  stmt = SET nvim.stdlib.vim.o.colorcolumn "17";
  raw = ''vim.o.colorcolumn = "17"'';
};
assert chk
{
  stmt = SET (IDX nvim.stdlib.vim.bo 1).textwidth 72;
  raw = ''vim.bo[1].textwidth = 72'';
};
assert chk
{
  stmt = LET (REQ "vim.shared") (REQ "vim._editor") (vim-shared: vim-editor: nvim.stdlib.print vim-shared);
  raw = ''
    local m_var1 = require("vim.shared")
    local m_var2 = require("vim._editor")
    print(m_var1)'';
};
assert chk
{
  expr = { key = { test }: RETURN test; };
  raw = ''
    {
      key = function(m_arg1)
        return m_arg1.test
      end;
    }'';
};
assert chk
{
  expr = lua.stdlib.print 5;
  raw = "print(5)";
};
assert chk
{
  expr = (lua.screamingKeywords.REQ "cjson").encode;
  raw = "require(\"cjson\").encode";
};
assert chk
{
  stmt = LET (lua.screamingKeywords.REQ "cjson") (cjson: RETURN cjson.encode);
  raw = ''
    local m_var1 = require("cjson")
    return m_var1.encode'';
};
assert chk
{
  stmt = LET (lua.screamingKeywords.REQ' (PROP (lua.stdlib.require "cjson") "encode")) (lua.screamingKeywords.REQ' (lua.stdlib.require "cjson")) (encode: _: RETURN encode);
  raw = ''
    local m_var1 = require("cjson").encode
    local m_var2 = require("cjson")
    return m_var1'';
};
assert chk
{
  stmt = (LET (lua.screamingKeywords.REQ "cjson") ({ encode, ... }: encode 5));
  raw = ''
    local m_var1 = require("cjson")
    m_var1.encode(5)'';
};
assert chk
{
  stmt = LETREC
    (fib:
      (n:
        IF (LT n 2)
          (RETURN n)
          ELSE
          (RETURN (ADD (fib (SUB n 1)) (fib (SUB n 2))))))
    (fib: lua.stdlib.print (fib 5));
  raw = ''
    local m_var1
    m_var1 = function(m_arg2)
      if m_arg2 < 2 then
        return m_arg2
      else
        return m_var1(m_arg2 - 1) + m_var1(m_arg2 - 2)
      end
    end
    print(m_var1(5))'';
};
assert !(builtins.tryEval (compileStmt defaultState (LETREC
  (fib:
    (n:
      IF (LT n 2)
        (RETURN n)
        ELSE
        (RETURN (ADD (fib (SUB n 1)) (fib (SUB n 2))))))
  (fib: lua.stdlib.print (fib 5 6))))).success;
assert !(builtins.tryEval (compileStmt defaultState (LETREC
  (fib:
    (n:
      IF (LT n 2)
        (RETURN n)
        ELSE
        (RETURN (ADD (fib (SUB n 1) 5) (fib (SUB n 2))))))
  (fib: lua.stdlib.print (fib 5))))).success;
assert chk
{
  expr = CALL (ERAW "test" // { __type__ = "function"; __minArity__ = 1; }) 5;
  raw = "test(5)";
};
assert chk
{
  expr = CALL (ERAW "test" // { __type__ = "function"; __minArity__ = 0; });
  raw = "test()";
};
assert chk
{
  expr = CALL
    (ERAW "test" // {
      __meta__.__call = { __type__ = "function"; __minArity__ = 2; };
    }) 5 6;
  raw = "test(5, 6)";
};
assert chk
{
  stmt = LET
    (ERAW "test" // {
      __meta__.__call = { __type__ = "function"; __minArity__ = 2; };
    })
    (x: CALL x 5);
  raw = ''
    local m_var1 = test
    m_var1(5)'';
};
assert !(builtins.tryEval (compileExpr defaultState (CALL
  (ERAW "test" // {
    __meta__.__call = { __type__ = "function"; __minArity__ = 1; __maxArity__ = 1; };
  }) 5 5 5))).success;
assert !(builtins.tryEval (compileExpr defaultState (CALL (ERAW "test" // { __type__ = "function"; __minArity__ = 2; }) 5))).success;
assert !(builtins.tryEval (compileExpr defaultState (CALL (ERAW "test" // { __type__ = "function"; __minArity__ = 0; __maxArity__ = 0; }) 5))).success;
assert eq (flake.screamingKeywords.MERGE { a = 1; } [ ]) { a = 1; };
assert eq (flake.screamingKeywords.MERGE { a = 1; } [ 1 ]) { a = 1; __list__ = [ 1 ]; };
assert eq (flake.screamingKeywords.MERGE { a = 1; __list__ = [ 1 ]; } [ 2 ]) { a = 1; __list__ = [ 1 2 ]; };
assert chk
{
  expr = flake.screamingKeywords.MERGE { a = 1; } [ 1 ];
  raw = ''
    {
      1;
      a = 1;
    }'';
};
assert chk
{
  expr = { };
  raw = "{}";
};
assert chk
{
  expr = nvim.stdlib.vim.inspect 5 { };
  raw = "vim.inspect(5, {})";
};
assert chk
{
  expr = (nvim.screamingKeywords.REQ "cmp").mapping;
  raw = "require(\"cmp\").mapping";
};
assert chk
{
  expr = (nvim.screamingKeywords.REQ "cmp").mapping.close;
  raw = "require(\"cmp\").mapping.close";
};
assert chk
{
  expr = (IDX nvim.stdlib.vim.bo "test").binary;
  raw = "vim.bo[\"test\"].binary";
};
assert chk
{
  expr = (IDX nvim.stdlib.vim.bo "test").bin;
  raw = "vim.bo[\"test\"].binary";
};
assert chk
{
  expr = nvim.stdlib.vim.cmd "test";
  raw = "vim.cmd(\"test\")";
};
assert chk
{
  expr = [ ];
  raw = "{}";
};
assert chk
{
  expr = DEFUN_VAR (a: b: RETURN b);
  raw = ''
    function(m_arg1, ...)
      return ...
    end'';
};
assert chk
{
  expr = PROP (ERAW "test") "nil";
  raw = ''test["nil"]'';
};
assert chk
{
  expr = { nil = 1; };
  raw = ''
    {
      ["nil"] = 1;
    }'';
};
assert eq (flake.utils.humanType (OR true null)) "boolean";
assert eq (flake.utils.humanType (ADD 1 5)) "number";
assert eq (flake.utils.humanType (CAT "" 5)) "string";
assert eq (flake.utils.humanType (AND true null)) "nil";
assert eq (flake.utils.humanType (AND null true)) "nil";
assert eq (flake.utils.humanType (AND true false)) "boolean";
assert eq (flake.utils.humanType (OR null true)) "boolean";
assert eq (flake.utils.humanType (AND (AND true null) true)) "nil";
assert eq (flake.utils.humanType (OR 5 6)) "number";
{
  name = "flake-checks";
  type = "derivation";
}
