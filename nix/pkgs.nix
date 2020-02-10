let
  pkgsv = import ( import ./nixpkgs.nix );
  pkgs = pkgsv {};
  intray-version = import ./intray-version.nix;
  intray-repo = pkgs.fetchFromGitHub intray-version;
  intray-overlay = import ( intray-repo + "/nix/overlay.nix" );
  validity-version = import ( intray-repo + "/nix/validity-version.nix" );
  validity-overlay =
    import ( pkgs.fetchFromGitHub validity-version + "/nix/overlay.nix" );
  pretty-relative-time-overlay =
    import (
      pkgs.fetchFromGitHub (import ./pretty-relative-time-version.nix) + "/nix/overlay.nix"
    );
  mergeless-overlay =
    import (
      pkgs.fetchFromGitHub (import ./mergeless-version.nix) + "/nix/overlay.nix"
    );

in
  pkgsv {
    overlays =
      [
        validity-overlay
        pretty-relative-time-overlay
        mergeless-overlay
        intray-overlay
        ( import ./gitignore-src.nix )
        ( import ./overlay.nix )
      ];
    config.allowUnfree = true;
  }
