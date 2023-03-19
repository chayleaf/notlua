#!/usr/bin/env -S nix eval --raw --impure -f
let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) callPackage;
  config = (callPackage ./. { }).config;
  inherit (config) notlua;

  notlua-lua = notlua.lua {
    # lua = pkgs.lua.withPackages (p: with p; [
    #  # add lua packages here if you want to use them
    # ]);
  };
  inherit (notlua.utils) compile;
in
with notlua-lua.stdlib; with notlua.keywords; with notlua-lua.keywords;

compile "main" [
  (print "Hello, World!")
  (IF (EQ (ADD 2 2) 3)
    (print "Goodbye, World!"))
]
