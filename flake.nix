{
  description = "A Nix DSL that compiles to Lua";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: 
    let output = (flake-utils.lib.eachDefaultSystem (system: {
      nixosModules.default = ./default.nix;
    })); in output // {
      checks.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.callPackage ./checks.nix {
        flake = (nixpkgs.legacyPackages.x86_64-linux.callPackage output.nixosModules."x86_64-linux".default {}).config.notlua;
      };
    };
}
