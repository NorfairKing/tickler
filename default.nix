let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs = pkgsv {};
  intray-version = import ./nix/intray-version.nix;
  intray-repo = pkgs.fetchFromGitHub intray-version;
  intray-overlay = import (intray-repo + "/nix/overlay.nix");
  validity-version = import (intray-repo + "/nix/validity-version.nix");
  validity-overlay = import (pkgs.fetchFromGitHub validity-version + "/nix/overlay.nix");
in pkgsv {
  overlays = [ validity-overlay intray-overlay (import ./nix/overlay.nix) ];
  config.allowUnfree = true;
}
