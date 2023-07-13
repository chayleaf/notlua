# Not Lua

Ever wanted to write Lua programs in... not Lua? Well, you're in the
right place! Now you can also write them in Nix!

Features:
- TYPE SYSTEM (well, only those types that can be checked are checked)
- it accidentally looks like lisp
- first-class Neovim support
- that's about it?

Limitations:
- poor error messages?
- you lose currying compared to Nix (please don't try it, you're bound
  to fail)

Simple examples: see [checks](./checks.nix).

Complex examples: see [my neovim
config](https://github.com/chayleaf/dotfiles/blob/master/home/common/nvim.nix)

## Try it out

Check out [sample.nix](./sample.nix) (you can run it with
`./sample.nix` after cloning this repository). To immediately feed the
compiled code to Lua, do `./sample.nix | lua` (assuming you have Lua
in your `PATH` of course).

To use this in your config, add this flake's output
`nixosModules.default` to your NixOS module list and then access it via
`config.notlua` or the module argument `notlua`. Alternatively, use this
flake's output `lib.${system}`.

## Overview

`notlua.utils` contains various functions, out of which you should only
really care about `compile`. It takes a module name (which will be used
to prefix variables, so you can put multiple modules in a single .lua
file), and the statements to compile to lua. The other functions are
internal functions mostly only useful for macro developers.

There are many keywords provided. You can choose your preferred keyword
case by using `notlua.screamingKeywords` (`KEYWORD_NAME`),
`notlua.pascalKeywords` (`KeywordName`) or `notlua.camelKeywords`
(`keywordName`). Below, the list of keywords in `screamingKeywords` is
provided, which is the default and also available via `notlua.keywords`:

- `RAW <code>`: escape raw Lua code (Use `ERAW` or `SRAW` to hint that
  it's a statement/expression)
- `PROP <expression> <name>`: get a property of `<expression>` with a
  name `<name>` (must be a string)
- `UNSAFE_PROP`: same, but don't throw an error if expression isn't a
  table
- `CALL <expr> [<exprs>...]`: call a function. Arguments are variadic,
  i.e. if you want to call `print(1, 2)` you do `CALL print 1 2`.
- `UNSAFE_CALL`: same, but don't check function arity (argument count)
- `APPLY`: apply a list of arguments to a function (so you can do `APPLY
  (CALL print) [1 2]`)
- `MCALL`: call a method (`table:method()`). First argument is the
  table, second is method name, other arguments are passed to the
  function.
- `SET <var/vars> <exprs...>`: sets something to something (the `=`
  statement). You can pass multiple values:
  - to assign to multiple lhs (`a, b = c`), use a list as lhs
    (`SET [a b] c`)
  - to assign multiple rhs (`a = b, c`), pass the rhs as varargs
    (`SET a b c`)
  - you can combine them (`a, b = b, a` -> `SET [a b] b a`)
- `UNSAFE_SET`: same, but don't throw an error if type doesn't match or
  if lhs isn't a valid lhs
- `LEN`: length operator
- `NOT`: not operator
- `UNM`: unary minus
- `EQ` `GE` `LE` `GT` `LT` `NE` `AND` `OR` - logical operators
- `ADD` `SUB` `MUL` `DIV` - arithmetic operators
- `CAT` - concat operator
- All binary operators take a variable amount of arguments, just like
  functions!
- There's probably many more operators you can add via `OP2` (`OP2 "=="`
  is the same as `EQ`, etc)
- `FORIN` - iterate over an iterator with a callback (first argument:
  iterator, second argument: callback).
  Example: `FORIN (CALL pairs table) (k: v: CALL print k v)`
- `FORRANGE` - Lua's numeric for
- `WHILE` - while loop
- `REPEAT` - repeat loop
- `RETURN` - return an expression (or just return, or return multiple
  expressions)
- `BREAK` - break statement
- `DEFUN` - create a zero argument function with provided body.
- `DEFUN_VAR` - create a function with a variable argument count. The
  varargs will be passed as the last parameter.
- `IF` - if condition. Syntax: `IF <cond1> <branch1> <cond2> <branch2>
  <fallback branch>`, or `IF <cond1> <branch1> <cond2> <branch2> ELSE
  <fallback branch>` (those are equivalent)
- `IDX` - dynamically get a table's value (the `[]` operator)
- `UNSAFE_IDX` - same, but don't throw an error if it isn't a table.
- `LET` - create a local variable and call a callback with it.
  Example: `LET 1 2 3 (one: two: three: CALL print (ADD one two))`
- `LETREC`: same, but instead of expressions you provide functions that
  take the variables and generate the expressions. Example: the fifth
  fibonacci number:
  ```lua
  LETREC
    (fib:
      (n:
        IF (LT n 2)
          (RETURN n)
        ELSE
          (RETURN (ADD (fib (SUB n 1)) (fib (SUB n 2))))))
    (fib: CALL print (fib 5))
  ```
- `MERGE`: merge two table-like values in Nix (can be lists or
  attrsets). If you do that, use `ATTR_PART` to get only the attrs of a
  table and `LIST_PART` to get only the list part.
- `MACRO`, `MACRO'`: the entire compiler is built on macros! In fact,
  the only keyword that isn't a macro is `MACRO` (for obvious reasons).
  Macros are functions that take transpiler state and return raw
  compiled code. There are also `LMACRO`s (short for "let macro") for
  creating custom variants of `LET`. For more info on macros, see
  [INTERNALS](./INTERNALS.md).

Additionally, statement lists compile to one statement per line,
expression lists and attrsets compile to tables. Functions compile to
function expressions. You can even pass functions with attrset
parameters to the compiler: `({ a, b, c }: d: CALL print a b c d)` will
result in:

```lua
function(arg1, arg2)
  print(arg1.a, arg1.b, arg1.c, arg2)
end
```

There are autogenerated bindings for convenient interaction with Lua or
Neovim. They are available at `notlua.neovim { neovim-unwrapped,
plugins, extraLuaPackages }` and `notlua.lua { lua }` (all attributes
are optional). It exposes two modules - `stdlib` and `keywords`.
`stdlib` contains the default functions (such as `print` and `require`),
and `keywords` provides `REQ` (a version of `stdlib.require` that also
autogenerates bindings) and `REQ'` (same, but it executes not just
requires, but arbitrary code, which can generate wrong bindings if you
use it the wrong way, be careful).

Note that this means you don't have to do `CALL print a b` like I wrote
above, just `print a b` is enough! However, if a function has zero
arguments, or if you want to call a table, you will still have to use
`CALL` (or even `UNSAFE_CALL`).

The bindings are type-aware and will not let you call a function with a
wrong argument count or set a Vim option to a wrong type (or any
other module's value, for that matter). That is, unless you use the
unsafe versions of the methods.

## License

GPL 3 or later - you're free to use this in FOSS projects, or in
projects which you use privately.

