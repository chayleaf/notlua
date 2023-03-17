{
  description = "A Nix DSL that compiles to Lua";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: 
    let output = (flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in rec {
        nixosModules.default = pkgs.callPackage ./default.nix {};
      }
    )); in output // {
      checks.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.callPackage ./checks.nix {
        flake = output.nixosModules."x86_64-linux".default;
      };
    };
}
