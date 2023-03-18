{
  description = "A Nix DSL that compiles to Lua";

  outputs = { self, nixpkgs }: {
    nixosModules.default = ./default.nix;
    checks.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.callPackage ./checks.nix {
      flake = (nixpkgs.legacyPackages.x86_64-linux.callPackage ./default.nix { }).config.notlua;
    };
  };
}
