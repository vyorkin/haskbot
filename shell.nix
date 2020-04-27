let
  pkgs = import <nixpkgs> { config.allowBroken = true; };
  hsPkgs = import ./default.nix { };
in hsPkgs.shellFor {
  # Include only the *local* packages of your project.
  # packages = ps: with ps; [
  #   pkga
  #   pkgb
  # ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # You might want some extra tools in the shell (optional).
  buildInputs = with pkgs.haskellPackages; [
    ghcid
    hlint
    hasktags
    haskdogs
    weeder
    pretty-simple
    pretty-show
  ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
