{ flake, lib, pkgs }:
let
  inherit (flake.utils) compileExpr compileStmt;
  inherit (flake.keywords) RAW PROP APPLY CALL MCALL SET OP2 AND ADD UNM FORIN RETURN DEFUN DEFUN_VAR IF ELSE IDX LET LETREC MACRO LT SUB;
  nvim = flake.neovim { plugins = [ pkgs.vimPlugins.nvim-cmp ]; };
  lua = flake.lua { };
  defaultState = { moduleName = "m"; scope = 1; };
  chk = { stmt ? null, expr ? null, raw }:
    let result = if stmt != null then compileStmt defaultState stmt else compileExpr defaultState expr;
    in lib.assertMsg (result == raw) "Expected ${raw}, found ${result}";
  eq = a: b: lib.assertMsg (a == b) "Expected ${builtins.toJSON b}, found ${builtins.toJSON a}";
  inherit (nvim.keywords) REQ;
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
  stmt = FORIN (RAW "test") (k: (v: CALL (RAW "print") k v));
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
  expr = RAW "test";
  raw = "test";
};
assert chk
{
  expr = PROP (RAW "test") "prop";
  raw = "test.prop";
};
assert chk
{
  expr = IDX (RAW "test") "prop";
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
  expr = CALL (RAW "test");
  raw = "test()";
};
assert chk
{
  expr = CALL (RAW "test") [ 5 ] { test = 5; } 5 "5";
  raw = ''
    test({
      5;
    }, {
      test = 5;
    }, 5, "5")'';
};
assert chk
{
  expr = MCALL (RAW "test") "method";
  raw = "test:method()";
};
assert chk
{
  expr = DEFUN (RAW "test");
  raw = ''
    function()
      test
    end'';
};
assert chk
{
  stmt = SET (RAW "test") 5;
  raw = "test = 5";
};
assert chk
{
  stmt = IF (RAW "test") 5 (RAW "test2") 6 7;
  raw = ''
    if test then
      5
    elseif test2 then
      6
    else
      7
    end'';
};
assert chk
{
  stmt = IF (RAW "test") 5 (RAW "test2") 6 ELSE 7;
  raw = ''
    if test then
      5
    elseif test2 then
      6
    else
      7
    end'';
};
assert chk
{
  stmt = IF (AND true false) (CALL (RAW "print") "this shouldn't happen");
  raw = ''
    if true and false then
      print("this shouldn't happen")
    end'';
};
assert chk
{
  stmt = LET (RAW "test") (RAW "test2") ({ test }: test2: [ test test2 ]);
  raw = ''
    local m_var1 = test
    local m_var2 = test2
    m_var1.test
    m_var2'';
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
  stmt = MACRO true ({ ... }: "this will be lua in 1976\n");
  raw = ''
    this will be lua in 1976
  '';
};
assert !(builtins.tryEval (compileExpr defaultState (ADD "" 5))).success;
assert !(builtins.tryEval (compileExpr defaultState (UNM ""))).success;
assert !(builtins.tryEval (compileStmt defaultState (SET nvim.stdlib.vim.o.colorcolumn 17))).success;
assert chk
{
  stmt = SET nvim.stdlib.vim.o.colorcolumn "17";
  raw = ''vim.o.colorcolumn = "17"'';
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
  expr = { key = { test }: test; };
  raw = ''
    {
      key = function(m_arg1)
        m_arg1.test
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
  expr = (lua.keywords.REQ "cjson").encode;
  raw = "require(\"cjson\").encode";
};
assert chk
{
  stmt = LET (lua.keywords.REQ "cjson") (cjson: cjson.encode);
  raw = ''
    local m_var1 = require("cjson")
    m_var1.encode'';
};
assert chk
{
  stmt = LET (lua.keywords.REQ' (PROP (lua.stdlib.require "cjson") "encode")) (lua.keywords.REQ' (lua.stdlib.require "cjson")) (encode: _: encode);
  raw = ''
    local m_var1 = require("cjson").encode
    local m_var2 = require("cjson")
    m_var1'';
};
assert chk
{
  stmt = (LET (lua.keywords.REQ "cjson") ({ encode, ... }: encode 5));
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
assert eq (flake.keywords.MERGE { a = 1; } [ ]) { a = 1; };
assert eq (flake.keywords.MERGE { a = 1; } [ 1 ]) { a = 1; __list__ = [ 1 ]; };
assert eq (flake.keywords.MERGE { a = 1; __list__ = [ 1 ]; } [ 2 ]) { a = 1; __list__ = [ 1 2 ]; };
assert chk
{
  expr = flake.keywords.MERGE { a = 1; } [ 1 ];
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
  expr = nvim.stdlib.vim.inspect 5;
  raw = "vim.inspect(5)";
};
assert chk
{
  expr = (nvim.keywords.REQ "cmp").mapping;
  raw = "require(\"cmp\").config.mapping";
};
assert chk
{
  expr = (nvim.keywords.REQ "cmp").mapping.close;
  raw = "require(\"cmp\").config.mapping.close";
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
  expr = DEFUN_VAR (a: b: b);
  raw = ''
    function(m_arg1, ...)
      arg
    end'';
};
{
  name = "flake-checks";
  type = "derivation";
}
