let pkgs = import ./pinned-nixpkgs.nix;

in

(import ./.).shellFor {
  packages = p: [ p.adventofcode2020 ];
  #withHoogle = true;
  buildInputs = [ pkgs.ghcid ];
}

