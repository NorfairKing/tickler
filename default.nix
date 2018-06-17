let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  intray-overlay =
            import (pkgs.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "intray";
              rev = "43f857d6fc3348b8251e169289dd6f264fe433af";
              sha256 = "0vdfhzzkf0rq1d0a2pkn4mr2fs784p3vw6v6l34m0sm3pijyrsyx";
              fetchSubmodules = true;
            } + "/overlay.nix");

in pkgsv {
  overlays = [ intray-overlay (import ./overlay.nix) ];
  config.allowUnfree = true;
}
