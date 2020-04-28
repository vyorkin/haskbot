with import <nixpkgs> { };

let
  npkgs = with pkgs; [
    zlib
    libzip
  ];
  hpkgs = with pkgs.haskell.packages.ghc883; [
    ghcid
    hlint
    hasktags
    haskdogs
    weeder
    pretty-simple
    pretty-show
  ];
in pkgs.mkShell { buildInputs = npkgs ++ hpkgs; }
