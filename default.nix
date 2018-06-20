let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  intray-version = import ./intray-version.nix;
  intray-repo = pkgs.fetchFromGitHub intray-version;
  intray-overlay = import (intray-repo + "/overlay.nix");
  validity-version = import (intray-repo + "/validity-version.nix");
  validity-overlay = import (pkgs.fetchFromGitHub validity-version + "/overlay.nix");
in pkgsv {
  overlays = [ validity-overlay intray-overlay (import ./overlay.nix) ];
  config.allowUnfree = true;
}
