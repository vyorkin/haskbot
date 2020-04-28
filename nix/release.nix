let
  nixpkgs = import (import ./pinned-pkgs.nix) { inherit config; };
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];
  packages = import ./packages.nix;
  deps = import ./deps.nix;

  config = {
    allowBroken = true;
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc883.override {
        overrides = self: super: ((deps super) // builtins.mapAttrs (name: path: super.callCabal2nix name (gitignore path) {}) packages);
      };
    };
  };
in nixpkgs
