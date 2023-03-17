{ flake, lib }:
let
inherit (flake.utils) compileExpr compileStmt;
inherit (flake.keywords) RAW PROP APPLY CALL MCALL SET OP2 AND ADD FORIN2 RETURN DEFUN IF ELSE ATTR LET LETREC MACRO;
nvim = flake.neovim {};
defaultState = { moduleName = "m"; scope = 1; };
chk = { stmt ? null, expr ? null, raw }:
  let result = if stmt != null then compileStmt defaultState stmt else compileExpr defaultState expr;
  in lib.assertMsg (result == raw) "Expected ${raw}, found ${result}";
inherit (nvim.keywords) REQLET;
in
assert chk {
  stmt = [ (nvim.stdlib.vim.api.nvim_buf_set_option 5 "omnifunc" "v:lua.vim.lsp.omnifunc") ];
  raw = ''vim.api.nvim_buf_set_option(5, "omnifunc", "v:lua.vim.lsp.omnifunc")'';
};
assert chk {
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
assert chk {
  stmt = FORIN2 (RAW "test") (k: (v: CALL (RAW "print") k v));
  raw = ''
    for m_var1,m_var2 in test do
      print(m_var1, m_var2)
    end'';
};
assert chk {
  expr = test: { test2, test3 }: RETURN (ADD test test2 test3);
  raw = ''
    function(m_arg1, m_arg2)
      return m_arg1 + m_arg2.test2 + m_arg2.test3
    end'';
};
assert chk {
  expr = RAW "test";
  raw = "test";
};
assert chk {
  expr = PROP (RAW "test") "prop";
  raw = "test.prop";
};
assert chk {
  expr = ATTR (RAW "test") "prop";
  raw = "test[\"prop\"]";
};
assert chk {
  expr = APPLY ADD [ 0 1 2 3 ];
  raw = "0 + 1 + 2 + 3";
};
assert chk {
  expr = AND true false (OP2 "==" false true);
  raw = "true and false and (false == true)";
};
assert chk {
  expr = CALL (RAW "test");
  raw = "test()";
};
assert chk {
  expr = CALL (RAW "test") [ 5 ] { test = 5; } 5 "5";
  raw = ''
    test({
      5;
    }, {
      test = 5;
    }, 5, "5")'';
};
assert chk {
  expr = MCALL (RAW "test") "method";
  raw = "test:method()";
};
assert chk {
  expr = DEFUN (RAW "test");
  raw = ''
    function()
      test
    end'';
};
assert chk {
  stmt = SET (RAW "test") 5;
  raw = "test = 5";
};
assert chk {
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
assert chk {
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
assert chk {
  stmt = IF (AND true false) (CALL (RAW "print") "this shouldn't happen");
  raw = ''
    if true and false then
      print("this shouldn't happen")
    end'';
};
assert chk {
  stmt = LET (RAW "test") (RAW "test2") ({ test }: test2: [ test test2 ]);
  raw = ''
    local m_var1 = test
    local m_var2 = test2
    m_var1.test
    m_var2'';
};
assert chk {
  stmt = LETREC (test: test2: test2) (test: test2: test) (test: test2: []);
  raw = ''
    local m_var1 = m_var2
    local m_var2 = m_var1
  '';
};
assert chk {
  stmt = MACRO (state: { inherit state; result = RAW "this will be lua in 1976\n"; });
  raw = ''
    this will be lua in 1976
  '';
};
assert !(builtins.tryEval (compileStmt defaultState (SET nvim.stdlib.vim.o.colorcolumn 17))).success;
assert chk {
  stmt = SET nvim.stdlib.vim.o.colorcolumn "17";
  raw = ''vim.o.colorcolumn = "17"'';
};
assert chk {
  stmt = REQLET "vim.shared" "vim._editor" (vim-shared: vim-editor: nvim.stdlib.print vim-shared);
  raw = ''
    local m_var1 = require("vim.shared")
    local m_var2 = require("vim._editor")
    print(m_var1)'';
};
assert chk {
  expr = { key = { test }: test; };
  raw = ''
    {
      key = function(m_arg1)
        m_arg1.test
      end;
    }'';
};
{
  name = "flake-checks";
  type = "derivation";
}
