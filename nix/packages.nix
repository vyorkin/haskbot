
# For monorepos (projects with multiple packages)

# let
#   findHaskellPackages = (import ./lib/utils.nix).findHaskellPackages;
# in
#   findHaskellPackages ../code

# For single-project repos

{ haskbot = ../.; }
