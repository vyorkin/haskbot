let
  nixpkgs = import ./nix/release.nix;
  packages = import ./nix/project.nix;
in nixpkgs.haskellPackages.shellFor {
  packages = p: builtins.attrValues packages;

  buildInputs = with nixpkgs.haskellPackages; [
    cabal-install

    ghcid

    hlint
    weeder

    hoogle

    hasktags
    haskdogs

    pretty-simple
    pretty-show
  ];

  shellHook = ''
    export CABAL_DIR=${builtins.toString ./.cabal}
  # '';
}
