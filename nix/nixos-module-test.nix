{ pkgs
, tickler-nixos-module-factory
}:
let
  tickler-production = tickler-nixos-module-factory {
    envname = "production";
  };

  api-port = 8000;
  web-port = 8080;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "tickler-module-test";
    nodes = {
      apiserver = {
        imports = [
          tickler-production
        ];
        services.tickler.production = {
          enable = true;
          api-server = {
            enable = true;
            port = api-port;
            log-level = "Debug";
          };
        };
      };
      webserver = {
        imports = [
          tickler-production
        ];
        services.tickler.production = {
          enable = true;
          web-server = {
            enable = true;
            port = web-port;
            api-url = "http://apiserver:${builtins.toString api-port}";
            log-level = "Debug";
          };
        };
      };
    };
    testScript = ''
      apiserver.wait_for_unit("multi-user.target")
      webserver.wait_for_unit("multi-user.target")

      apiserver.wait_for_open_port(${builtins.toString api-port})
      apiserver.succeed("curl localhost:${builtins.toString api-port}")

      webserver.wait_for_open_port(${builtins.toString web-port})
      webserver.succeed("curl localhost:${builtins.toString web-port}")

      apiserver.wait_for_unit("tickler-api-server-production.service")
      webserver.wait_for_unit("tickler-web-server-production.service")
    '';
  }
)
