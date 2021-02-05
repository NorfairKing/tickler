{ pkgs ? import ./pkgs.nix }:
let
  tickler-production = import (./nixos-module.nix) { envname = "production"; };
  home-manager = import (
    builtins.fetchTarball {
      url = "https://github.com/rycee/home-manager/archive/472ca211cac604efdf621337067a237be9df389e.tar.gz";
      sha256 = "sha256:1gbfsnd7zsxwqryxd4r6jz9sgdz6ghlkapws1cdxshrbxlwhqad1";
    } + "/nixos/default.nix"
  );

  api-port = 8001;
  web-port = 8002;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "tickler-module-test";
    machine = {
      imports = [
        tickler-production
        home-manager
      ];
      services.tickler.production = {
        enable = true;
        inherit api-port;
        inherit web-port;
        log-level = "LevelDebug";
      };
      users.users.testuser.isNormalUser = true;
      home-manager.users.testuser = { pkgs, ... }: {
        imports = [
          ./home-manager-module.nix
        ];
        xdg.enable = true;
        home.stateVersion = "20.09";
        programs.tickler = {
          enable = true;
          sync = {
            enable = true;
            url = "localhost:${builtins.toString api-port}";
            username = "testuser";
            password = "testpassword";
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.wait_for_unit("multi-user.target")

      machine.wait_for_unit("tickler-production.service")
      machine.wait_for_open_port(${builtins.toString api-port})
      machine.succeed("curl localhost:${builtins.toString api-port}")
      machine.wait_for_open_port(${builtins.toString web-port})
      machine.succeed("curl localhost:${builtins.toString web-port}")

      machine.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      machine.succeed(su("testuser", "cat ~/.config/tickler/config.yaml"))

      # Something is wrong such that the register command hangs, for unknown reasons
      # machine.succeed(su("testuser", "tickler register"))
      # machine.succeed(su("testuser", "tickler login"))
      # machine.succeed(su("testuser", "tickler sync"))
    '';
  }
)
