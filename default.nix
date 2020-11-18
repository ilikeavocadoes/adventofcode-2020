with import ./pinned-nixpkgs.nix;

haskellPackages.extend (haskell.lib.packageSourceOverrides {
  adventofcode2020 = ./.;
})
