{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, ticklerPackages ? pkgs.ticklerPackages
}:
let
  tickler-production = import (./nixos-module.nix) {
    inherit sources;
    inherit pkgs;
    inherit ticklerPackages;
    envname = "production";
  };

  api-port = 8001;
  web-port = 8002;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "tickler-module-test";
    machine = {
      imports = [
        tickler-production
      ];
      services.tickler.production = {
        enable = true;
        api-server = {
          enable = true;
          port = api-port;
          log-level = "LevelDebug";
        };
        web-server = {
          enable = true;
          port = web-port;
          api-url = "http://machine:${builtins.toString api-port}";
          log-level = "LevelDebug";
        };
      };
    };
    testScript = ''
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_unit("tickler-production.service")
      machine.wait_for_open_port(${builtins.toString api-port})
      machine.succeed("curl localhost:${builtins.toString api-port}")
      machine.wait_for_open_port(${builtins.toString web-port})
      machine.succeed("curl localhost:${builtins.toString web-port}")
    '';
  }
)
