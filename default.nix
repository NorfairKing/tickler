let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  intray-overlay = import (
    (pkgs.fetchFromGitHub (import ./intray-version.nix) 
    + "/overlay.nix")
  );
in pkgsv {
  overlays = [ intray-overlay (import ./overlay.nix) ];
  config.allowUnfree = true;
}
