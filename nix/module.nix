{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.tickler."${envname}";
  concatAttrs = attrList: fold ( x: y: x // y ) {} attrList;
in {
  options.services.tickler."${envname}" =
    {
      enable = mkEnableOption "Tickler Service";
      envname =
        mkOption {
          type = types.string;
          default = "production";
          example = "production";
          description = "The name of the environment";
        };
      web-host =
        mkOption {
          type = types.string;
          example = "tickler.cs-syd.eu";
          description = "The host to serve web requests on";
        };
      api-host =
        mkOption {
          type = types.string;
          example = "api.tickler.cs-syd.eu";
          description = "The host to serve API requests on";
        };
      web-port =
        mkOption {
          type = types.int;
          default = 8100;
          example = 8100;
          description = "The port to serve web requests on";
        };
      api-port =
        mkOption {
          type = types.int;
          default = 8101;
          example = 8101;
          description = "The port to serve API requests on";
        };
      default-intray-url =
        mkOption {
          type = types.string;
          default = "https://api.intray.cs-syd.eu";
          example = "https://api.intray.cs-syd.eu";
          description = "The default intray url to use for triggers";
        };
      default-looper-enabled =
        mkOption {
          type = types.nullOr ( types.bool );
          default = null;
          example = true;
          description = "Whether loopers are on by default";
        };
      default-looper-period =
        mkOption {
          type = types.nullOr ( types.int );
          default = null;
          example = 600;
          description =
            "The number of seconds to use as the default period for loopers";
        };
      default-looper-retry-delay =
        mkOption {
          type = types.nullOr ( types.int );
          default = null;
          example = 1000000;
          description =
            "The number of microseconds to use as the default retry delay";
        };
      default-looper-retry-amount =
        mkOption {
          type = types.nullOr ( types.int );
          default = null;
          example = 5;
          description = "The default number of times to retry a looper";
        };
      tracking-id =
        mkOption {
          type = types.nullOr types.string;
          example = "UA-53296133-1";
          default = null;
          description = "The tracking id for google analytics";
        };
      verification-tag =
        mkOption {
          type = types.nullOr types.string;
          example = "ADkAx2F-JQO9KJBBdLfAGuJ_OMqPOsX5MdGDsfd0Ggw";
          default = null;
          description = "The verification tag for google search console";
        };
      admins =
        mkOption {
          type = types.nullOr ( types.listOf types.string );
          default = null;
          example = [ "syd" ];
          description =
            "A list of the usernames that will have admin privileges";
        };
      email-verification-address =
        mkOption {
          type = types.nullOr types.string;
          example = "verification@tickler.cs-syd.eu";
          default = null;
          description = "The email address to use for email verification";
        };
      email-triggered-address =
        mkOption {
          type = types.nullOr types.string;
          example = "triggered@tickler.cs-syd.eu";
          default = null;
          description = "The email address to use for email triggering";
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
                        type = types.string;
                        example = "plan_XXXXXXXXXXXXXX";
                        description = "Stripe plan for subscriptions.";
                      };
                    stripe-secret-key =
                      mkOption {
                        type = types.string;
                        example = "sk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe secret key.";
                      };
                    stripe-publishable-key =
                      mkOption {
                        type = types.string;
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
                                      type = types.nullOr ( types.int );
                                      default = null;
                                      example = 60;
                                      description =
                                        "The number of seconds between running the looper";
                                    };
                                  retry-delay =
                                    mkOption {
                                      type = types.nullOr ( types.int );
                                      default = null;
                                      example = 1000000;
                                      description =
                                        "The number of microseconds between retrying the looper when it fails";
                                    };
                                  retry-amount =
                                    mkOption {
                                      type = types.nullOr ( types.int );
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
                  in {
                    triggerer = looperOption;
                    emailer = looperOption;
                    triggered-intray-item-scheduler = looperOption;
                    triggered-intray-item-sender = looperOption;
                    verification-email-converter = looperOption;
                    triggered-email-scheduler = looperOption;
                    triggered-email-converter = looperOption;
                  };
              }
            );
          default = null;
        };



      t = null;

    };
  config =
    let
      tickler-service =
        let
          workingDir = "/www/tickler/${envname}/data/";
          tickler-pkgs = (import ../nix/pkgs.nix).ticklerPackages;
          configFile =
            let
              config =
                {
                  api-port = cfg.api-port;
                  web-port = cfg.web-port;
                  web-host = cfg.web-host;
                  default-intray-url = cfg.default-intray-url;
                  tracking = cfg.tracking-id;
                  verification = cfg.verification-tag;
                  admins = cfg.admins;
                  monetisation =
                    optionalAttrs ( !builtins.isNull cfg.monetisation ) {
                      stripe-plan = cfg.monetisation.stripe-plan;
                      stripe-secret-key = cfg.monetisation.stripe-secret-key;
                      stripe-publishable-key =
                        cfg.monetisation.stripe-publishable-key;
                    };
                  loopers =
                    let
                      looperConf =
                        subcfg: conf:
                          optionalAttrs ( subcfg != null ) {
                            enable = subcfg.enabled;
                            period = subcfg.period;
                            retry-policy =
                              {
                                delay = subcfg.retry-delay;
                                amount = subcfg.retry-amount;
                              };
                            inherit conf;
                          };
                      looperConf' = subcfg: looperConf subcfg null;
                    in {
                      "default-enabled" = cfg.default-looper-enabled;
                      "default-period" = cfg.default-looper-period;
                      "default-retry-delay" = cfg.default-looper-retry-delay;
                      "default-retry-amount" =
                        cfg.default-looper-retry-amount;
                      "triggerer" = looperConf' cfg.loopers.triggerer;
                      "emailer" = looperConf' cfg.loopers.emailer;
                      "triggered-intray-item-scheduler" =
                        looperConf' cfg.loopers.triggered-intray-item-scheduler;
                      "triggered-intray-item-sender" =
                        looperConf' cfg.loopers.triggered-intray-item-sender;
                      "verification-email-converter" =
                        looperConf cfg.loopers.verification-email-converter cfg.email-verification-address;
                      "triggered-email-scheduler" =
                        looperConf' cfg.loopers.triggered-email-scheduler;
                      "triggered-email-converter" =
                        looperConf cfg.loopers.triggered-email-converter cfg.email-triggered-address;
                    };
                };
            in
              pkgs.writeText "tickler-config" ( builtins.toJSON config );
          unlessNull = o: optionalAttrs ( !builtins.isNull o );
        in {
          description = "Tickler ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${tickler-pkgs.tickler-web-server}/bin/tickler-web-server serve --config-file ${configFile}
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
        };
    in
      mkIf cfg.enable {
        systemd.services =
          {
            "tickler-${envname}" = tickler-service;
          };
        networking.firewall.allowedTCPPorts = [ cfg.web-port cfg.api-port ];
        services.nginx.virtualHosts =
          {
            "${cfg.web-host}" =
              {
                enableACME = true;
                forceSSL = true;
                locations."/".proxyPass =
                  "http://localhost:${builtins.toString (cfg.web-port)}";
              };
            "${cfg.api-host}" =
              {
                enableACME = true;
                forceSSL = true;
                locations."/".proxyPass =
                  "http://localhost:${builtins.toString (cfg.api-port)}";
              };
          };
      };
}
