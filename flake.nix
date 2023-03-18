{
  description = "A Nix DSL that compiles to Lua";

  outputs = { self, nixpkgs }: {
    nixosModules.default = ./default.nix;
    formatter = builtins.foldl' (a: b: a // { ${b.system} = b.result; }) { } (map
      (system: {
        inherit system;
        result = let pkgs = nixpkgs.legacyPackages.${system}; in pkgs.nixpkgs-fmt;
      }) [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ]);
    checks.x86_64-linux.default = let pkgs = nixpkgs.legacyPackages.x86_64-linux; in pkgs.callPackage ./checks.nix {
      flake = (pkgs.callPackage ./default.nix { }).config.notlua;
    };
  };
}
