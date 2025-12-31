{
  description = "GHC environment for Juspay Haskell intro course";
  inputs = {
    # Use an older nixpkgs that uses GHC 9.2 (what Juspay uses) as default, and
    # thus provides cached packages.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # Use an older version of haskell-flake, before PR (#418) which broke
    # compatibility with older nixpkgs
    haskell-flake.url = "github:srid/haskell-flake/c9d08b";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { config, self', pkgs, ... }: {
        haskellProjects.default = { };
      };
    };
}
