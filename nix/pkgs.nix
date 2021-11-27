{ sources ? import ./sources.nix
}:
import sources.nixpkgs {
  overlays =
    [
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (import (sources.intray + "/nix/overlay.nix"))
      (import (sources.looper + "/nix/overlay.nix"))
      (import (sources.mergeful + "/nix/overlay.nix"))
      (import (sources.mergeless + "/nix/overlay.nix"))
      (import (sources.pretty-relative-time + "/nix/overlay.nix"))
      (import (sources.safe-coloured-text + "/nix/overlay.nix"))
      (import (sources.sydtest + "/nix/overlay.nix"))
      (import (sources.typed-uuid + "/nix/overlay.nix"))
      (import (sources.validity + "/nix/overlay.nix"))
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
