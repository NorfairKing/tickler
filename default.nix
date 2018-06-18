let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  intray-overlay =
            import (pkgs.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "intray";
              rev = "6b9d8af5602db6decab3e2c04b07ac0183e43636";
              sha256 = "11hcw4mv5m046zmf2ksdf12s846w7s67vra9rwvw49l9binvk38s";
              fetchSubmodules = true;
            } + "/overlay.nix");

in pkgsv {
  overlays = [ intray-overlay (import ./overlay.nix) ];
  config.allowUnfree = true;
}
