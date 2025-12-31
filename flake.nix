{
  description = "cutelb";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      imports = [
        haskell-flake.flakeModule
      ];

      perSystem = { pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;

          devShell = {
            tools = hp: {
              cabal = hp.cabal-install;
              ghc   = hp.ghc;
            };
          };
        };
      };
    };
}
