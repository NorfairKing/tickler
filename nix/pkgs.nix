let
  pkgsv = import ( import ./nixpkgs.nix );
  pkgs = pkgsv {};
  intray-version = import ./intray-version.nix;
  intray-repo = pkgs.fetchFromGitHub intray-version;
  intray-overlay = import ( intray-repo + "/nix/overlay.nix" );
  validity-version = import ( intray-repo + "/nix/validity-version.nix" );
  validity-overlay =
    import ( pkgs.fetchFromGitHub validity-version + "/nix/overlay.nix" );
in
  pkgsv {
    overlays =
      [
        validity-overlay
        intray-overlay
        ( import ./gitignore-src.nix )
        ( import ./overlay.nix )
      ];
    config.allowUnfree = true;
  }
