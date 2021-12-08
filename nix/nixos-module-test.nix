{ pkgs ? import ./pkgs.nix }:
let
  tickler-production = import (./nixos-module.nix) { envname = "production"; };

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
        inherit api-port;
        inherit web-port;
        log-level = "LevelDebug";
      };
      users.users.testuser.isNormalUser = true;
    };
    testScript = ''
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_unit("tickler-production.service")
      machine.wait_for_open_port(${builtins.toString api-port})
      machine.succeed("curl localhost:${builtins.toString api-port}")
      machine.wait_for_open_port(${builtins.toString web-port})
      machine.succeed("curl localhost:${builtins.toString web-port}")

      machine.wait_for_unit("home-manager-testuser.service")
    '';
  }
)
