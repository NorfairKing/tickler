let
  pkgsv = import ( import ./nixpkgs.nix );
  pkgs = pkgsv {};
  intray-version = import ./intray-version.nix;
  intray-repo = pkgs.fetchFromGitHub intray-version;
  intray-overlay = import ( intray-repo + "/nix/overlay.nix" );
  validity-version = import ./validity-version.nix;
  validity-overlay =
    import ( pkgs.fetchFromGitHub validity-version + "/nix/overlay.nix" );
  mergeless-version = import ./mergeless-version.nix;
  mergeless-overlay =
    import ( pkgs.fetchFromGitHub mergeless-version + "/nix/overlay.nix" );
  mergeful-version = import ./mergeful-version.nix;
  mergeful-overlay =
    import ( pkgs.fetchFromGitHub mergeful-version + "/nix/overlay.nix" );
  pretty-relative-time-version = import ./pretty-relative-time-version.nix;
  pretty-relative-time-overlay =
    import ( pkgs.fetchFromGitHub pretty-relative-time-version + "/nix/overlay.nix" );
in
  pkgsv {
    overlays =
      [
        validity-overlay
        intray-overlay
        pretty-relative-time-overlay
        mergeless-overlay
        mergeful-overlay
        ( import ./gitignore-src.nix )
        ( import ./overlay.nix )
      ];
    config.allowUnfree = true;
  }
