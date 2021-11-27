{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.tickler;


in
{
  options =
    {
      programs.tickler =
        {
          enable = mkEnableOption "Tickler cli";
          cache-dir =
            mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "The cache dir";
            };
          data-dir =
            mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "The data dir";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Tickler synchronisation";
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description = "The username to use for syncing";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            description = "The password to use for syncing";
                          };
                        url =
                          mkOption {
                            type = types.str;
                            default = "https://api.tickler.cs-syd.eu";
                            description =
                              "The sync server to use for syncing";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      ticklerPkgs = (import ../nix/pkgs.nix).ticklerPackages;

      nullOrOption =
        name: opt: optionalString (opt != null) "${name}: ${opt}";
      syncConfig =
        optionalString (cfg.sync != null) ''
          url: '${cfg.sync.url}'
          username: '${cfg.sync.username}'
          sync: NeverSync
        '';
      configFileContents =
        ''
          ${nullOrOption "cache-dir" cfg.cache-dir}
          ${nullOrOption "data-dir" cfg.data-dir}
          ${syncConfig}
        '';

      cli = ticklerPkgs.tickler-cli;

      syncTicklerName = "sync-tickler";
      syncTicklerService =
        {
          Unit =
            {
              Description = "Sync tickler items";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "tickler-sync" ''
                  ${cli}/bin/tickler login --password "${cfg.sync.password}"
                  ${cli}/bin/tickler sync
                ''}";
              Type = "oneshot";
            };
        };

      syncTicklerTimer =
        {
          Unit =
            {
              Description = "Sync tickler items every five minutes";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*:0/5";
              Persistent = true;
              Unit = "${syncTicklerName}.service";
            };
        };
      packages = optionals cfg.enable [ cli ];
      services =
        optionalAttrs (cfg.enable && cfg.sync.enable) {
          "${syncTicklerName}" = syncTicklerService;
        };
      timers =
        optionalAttrs (cfg.enable && cfg.sync.enable) {
          "${syncTicklerName}" = syncTicklerTimer;
        };
    in
    mkIf cfg.enable {
      xdg.configFile."tickler/config.yaml".text = configFileContents;
      systemd.user =
        {
          startServices = true;
          services = services;
          timers = timers;
        };
      home.packages = packages;
    };
}
