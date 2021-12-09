{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, ticklerPackages ? pkgs.ticklerPackages
, envname
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.tickler."${envname}";
  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
  toYamlFile = pkgs.callPackage ./to-yaml.nix { };
in
{
  options.services.tickler."${envname}" =
    {
      enable = mkEnableOption "Tickler Service";
      api-server = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Tickler API Server";
            config = mkOption {
              default = { };
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
            };
            hosts =
              mkOption {
                type = types.listOf types.str;
                default = [ ];
                example = [ "api.tickler.cs-syd.eu" ];
                description = "The host to serve API requests on";
              };
            port =
              mkOption {
                type = types.int;
                default = 8101;
                example = 8101;
                description = "The port to serve API requests on";
              };
            log-level =
              mkOption {
                type = types.nullOr types.str;
                default = null;
                example = "LevelInfo";
                description = "The minimal severity of log messages";
              };
            default-looper-enabled =
              mkOption {
                type = types.nullOr types.bool;
                default = null;
                example = true;
                description = "Whether loopers are on by default";
              };
            default-looper-period =
              mkOption {
                type = types.nullOr types.int;
                default = null;
                example = 600;
                description =
                  "The number of seconds to use as the default period for loopers";
              };
            default-looper-retry-delay =
              mkOption {
                type = types.nullOr types.int;
                default = null;
                example = 1000000;
                description =
                  "The number of microseconds to use as the default retry delay";
              };
            default-looper-retry-amount =
              mkOption {
                type = types.nullOr types.int;
                default = null;
                example = 5;
                description = "The default number of times to retry a looper";
              };
            admins =
              mkOption {
                type = types.nullOr (types.listOf types.str);
                default = null;
                example = [ "syd" ];
                description =
                  "A list of the usernames that will have admin privileges";
              };
            freeloaders =
              mkOption {
                type = types.nullOr (types.listOf types.str);
                default = null;
                example = [ "freeloaders" ];
                description =
                  "A list of the usernames that will have have access without payment";
              };
            email-verification-address =
              mkOption {
                type = types.nullOr types.str;
                example = "verification@tickler.cs-syd.eu";
                default = null;
                description = "The email address to use for email verification";
              };
            email-triggered-address =
              mkOption {
                type = types.nullOr types.str;
                example = "triggered@tickler.cs-syd.eu";
                default = null;
                description = "The email address to use for email triggering";
              };
            email-admin-notification-from-address =
              mkOption {
                type = types.nullOr types.str;
                example = "admin-notification@tickler.cs-syd.eu";
                default = null;
                description =
                  "The email address to use to send admin notifications from";
              };
            email-admin-notification-to-address =
              mkOption {
                type = types.nullOr types.str;
                example = "syd@example.com";
                default = null;
                description =
                  "The email address to use to send admin notifications from";
              };
            monetisation =
              mkOption {
                default = null;
                type =
                  types.nullOr (
                    types.submodule {
                      options =
                        {
                          stripe-plan =
                            mkOption {
                              type = types.str;
                              example = "plan_XXXXXXXXXXXXXX";
                              description = "Stripe plan for subscriptions.";
                            };
                          stripe-secret-key =
                            mkOption {
                              type = types.str;
                              example = "sk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                              description = "Stripe secret key.";
                            };
                          stripe-publishable-key =
                            mkOption {
                              type = types.str;
                              example = "pk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                              description = "Stripe publishable key.";
                            };
                        };
                    }
                  );
              };
            loopers =
              mkOption {
                type =
                  types.nullOr (
                    types.submodule {
                      options =
                        let
                          looperOption =
                            mkOption {
                              type =
                                types.nullOr (
                                  types.submodule {
                                    options =
                                      {
                                        enabled = mkEnableOption "Looper";
                                        period =
                                          mkOption {
                                            type = types.nullOr types.int;
                                            default = null;
                                            example = 60;
                                            description =
                                              "The number of seconds between running the looper";
                                          };
                                        retry-delay =
                                          mkOption {
                                            type = types.nullOr types.int;
                                            default = null;
                                            example = 1000000;
                                            description =
                                              "The number of microseconds between retrying the looper when it fails";
                                          };
                                        retry-amount =
                                          mkOption {
                                            type = types.nullOr types.int;
                                            default = null;
                                            example = 5;
                                            description =
                                              "The number of times to retry the looper when it fails";
                                          };
                                      };
                                  }
                                );
                              default = null;
                            };
                        in
                        {
                          triggerer = looperOption;
                          emailer = looperOption;
                          triggered-intray-item-scheduler = looperOption;
                          triggered-intray-item-sender = looperOption;
                          verification-email-converter = looperOption;
                          triggered-email-scheduler = looperOption;
                          triggered-email-converter = looperOption;
                          admin-notification-email-converter = looperOption;
                        };
                    }
                  );
                default = null;
              };
          };
        });
      };
      web-server = mkOption {
        default = null;
        type = types.nullOr
          (types.submodule {
            options = {
              enable = mkEnableOption "Tickler Web Server";
              config = mkOption {
                default = { };
                description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              };
              hosts =
                mkOption {
                  type = types.listOf types.str;
                  default = [ ];
                  example = [ "tickler.cs-syd.eu" ];
                  description = "The host to serve web requests on";
                };
              port =
                mkOption {
                  type = types.int;
                  default = 8100;
                  example = 8100;
                  description = "The port to serve web requests on";
                };
              log-level =
                mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  example = "LevelInfo";
                  description = "The minimal severity of log messages";
                };
              api-url =
                mkOption {
                  type = types.str;
                  example = "https://api.tickler.cs-syd.eu";
                  description = "The url of the api server to call";
                };
              default-intray-url =
                mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  example = "https://api.intray.cs-syd.eu";
                  description = "The default intray url to use for triggers";
                };
              tracking-id =
                mkOption {
                  type = types.nullOr types.str;
                  example = "UA-53296133-1";
                  default = null;
                  description = "The tracking id for google analytics";
                };
              verification-tag =
                mkOption {
                  type = types.nullOr types.str;
                  example = "ADkAx2F-JQO9KJBBdLfAGuJ_OMqPOsX5MdGDsfd0Ggw";
                  default = null;
                  description = "The verification tag for google search console";
                };
            };
          });
      };
    };
  config =
    let
      workingDir = "/www/tickler/${envname}/data/";
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      attrOrNullHead = name: value: optionalAttrs (!builtins.isNull value && value != [ ]) { "${name}" = builtins.head value; };
      api-server-config = with cfg.api-server; mergeListRecursively [
        (attrOrNullHead "host" hosts)
        (attrOrNull "port" port)
        (attrOrNull "admins" admins)
        (attrOrNull "freeloaders" freeloaders)
        (attrOrNull "log-level" log-level)
        (attrOrNull "monetisation" monetisation)
        cfg.api-server.config
      ];
      api-server-config-file = toYamlFile "tickler-api-server-config" api-server-config;
      api-server-service = optionalAttrs (cfg.api-server.enable or false) {
        "tickler-api-server-${envname}" = {
          description = "Tickler ${envname} api server service";
          wantedBy = [ "multi-user.target" ];
          environment = {
            "TICKLER_SERVER_CONFIG_FILE" = "${api-server-config-file}";
          };
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${ticklerPackages.tickler-server}/bin/tickler-server
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              # ensure Restart=always is always honoured
              StartLimitIntervalSec = 0;
            };
        };
      };
      api-host = optionalAttrs (cfg.api-server.enable or false && cfg.api-server.hosts != [ ]) {
        "${builtins.head (cfg.api-server.hosts)}" =
          {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://localhost:${builtins.toString (cfg.api-server.port)}";
            serverAliases = tail cfg.api-server.hosts;
          };
      };
      web-server-config = with cfg.web-server; mergeListRecursively [
        (attrOrNullHead "host" hosts)
        (attrOrNull "port" port)
        (attrOrNull "log-level" log-level)
        (attrOrNull "api-url" api-url)
        (attrOrNull "tracking" tracking-id)
        (attrOrNull "verification" verification-tag)
        cfg.web-server.config
      ];
      web-server-config-file = toYamlFile "tickler-web-server-config" web-server-config;
      web-server-service = optionalAttrs (cfg.web-server.enable or false) {
        "tickler-web-server-${envname}" = {
          description = "Tickler ${envname} web server service";
          wantedBy = [ "multi-user.target" ];
          environment = {
            "TICKLER_WEB_SERVER_CONFIG_FILE" = "${web-server-config-file}";
          };
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${ticklerPackages.tickler-web-server}/bin/tickler-web-server
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              # ensure Restart=always is always honoured
              StartLimitIntervalSec = 0;
            };
        };
      };
      web-host =
        let redirectHost = host: {
          "www.${host}" = {
            enableACME = true;
            forceSSL = true;
            globalRedirect = host;
          };
        };
        in
        optionalAttrs (cfg.web-server.enable or false && cfg.web-server.hosts != [ ])
          {
            "${builtins.head (cfg.web-server.hosts)}" =
              {
                enableACME = true;
                forceSSL = true;
                locations."/".proxyPass = "http://localhost:${builtins.toString (cfg.web-server.port)}";
                serverAliases = tail cfg.web-server.hosts;
              };
          } // mergeListRecursively (builtins.map redirectHost cfg.web-server.hosts);

    in
    mkIf cfg.enable {
      systemd.services = mergeListRecursively [
        api-server-service
        web-server-service
      ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional (cfg.api-server.enable or false) cfg.api-server.port)
        (optional (cfg.web-server.enable or false) cfg.web-server.port)
      ];
      services.nginx.virtualHosts = mergeListRecursively [
        api-host
        web-host
      ];
    };

}
