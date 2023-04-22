{
  description = "A Nix DSL that compiles to Lua";

  outputs = { self, nixpkgs }:
    let
      forEachSystem = func: builtins.foldl' (a: b: a // { ${b.system} = b.result; }) { } (map
        (system: {
          inherit system;
          result = func { inherit system; pkgs = import nixpkgs { inherit system; }; };
        }) [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ]);
    in
    {
      nixosModules.default = ./default.nix;
      # this isn't an officially supported third-party flake output
      # homeManagerModules.default = ./default.nix;
      lib = forEachSystem ({ pkgs, ... }: (import ./default.nix { inherit pkgs; inherit (pkgs) lib; }).config.notlua);
      formatter = forEachSystem ({ pkgs, ... }: pkgs.nixpkgs-fmt);
      checks.x86_64-linux.default = let pkgs = nixpkgs.legacyPackages.x86_64-linux; in pkgs.callPackage ./checks.nix {
        flake = (pkgs.callPackage ./default.nix { }).config.notlua;
      };
    };
}
