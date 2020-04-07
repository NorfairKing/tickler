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
            types.submodule {
              options =
                let
                  looperOption =
                    mkOption {
                      type =
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
                        };
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
            };
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
          unlessNull = o: optionalAttrs ( !builtins.isNull o );
          mkLooperVars =
            name: looper:
              unlessNull looper (
                with looper;

                concatAttrs [
                  (
                    unlessNull enabled {
                      "TICKLER_SERVER_LOOPER_${name}_ENABLED" =
                        "${if enabled then "True" else "False"}";
                    }
                  )
                  (
                    unlessNull period {
                      "TICKLER_SERVER_LOOPER_${name}_PERIOD" =
                        "${builtins.toString period}";
                    }
                  )
                  (
                    unlessNull retry-delay {
                      "TICKLER_SERVER_LOOPER_${name}_RETRY_DELAY" =
                        "${builtins.toString retry-delay}";
                    }
                  )
                  (
                    unlessNull retry-amount {
                      "TICKLER_SERVER_LOOPER_${name}_RETRY_AMOUNT" =
                        "${builtins.toString retry-amount}";
                    }
                  )
                ]
              );
        in {
          description = "Tickler ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          environment =
            concatAttrs [
              {
                "TICKLER_SERVER_WEB_HOST" =
                  "${builtins.toString (cfg.web-host)}";
                "TICKLER_WEB_SERVER_PORT" =
                  "${builtins.toString (cfg.web-port)}";
                "TICKLER_SERVER_PORT" = "${builtins.toString (cfg.api-port)}";
                "TICKLER_WEB_SERVER_DEFAULT_INTRAY_URL" =
                  cfg.default-intray-url;
              }
              (
                unlessNull cfg.tracking-id {
                  "TICKLER_WEB_SERVER_TRACKING" = cfg.tracking-id;
                }
              )
              (
                unlessNull cfg.verification-tag {
                  "TICKLER_WEB_SERVER_SEARCH_CONSOLE_VERIFICATION" =
                    cfg.verification-tag;
                }
              )
              (
                unlessNull cfg.email-verification-address {
                  "TICKLER_SERVER_VERIFICATION_EMAIL_ADDRESS" =
                    cfg.email-verification-address;
                }
              )
              (
                unlessNull cfg.email-verification-address {
                  "TICKLER_SERVER_TRIGGERED_EMAIL_ADDRESS" =
                    cfg.email-triggered-address;
                }
              )
              (
                optionalAttrs ( cfg.monetisation != null ) (
                  with cfg.monetisation;

                  {
                    "TICKLER_SERVER_STRIPE_PLAN" = "${stripe-plan}";
                    "TICKLER_SERVER_STRIPE_SECRET_KEY" =
                      "${stripe-secret-key}";
                    "TICKLER_SERVER_STRIPE_PUBLISHABLE_KEY" =
                      "${stripe-publishable-key}";
                  }
                )
              )
              (
                with cfg;

                concatAttrs [
                  (
                    unlessNull default-looper-enabled {
                      "TICKLER_SERVER_LOOPERS_DEFAULT_ENABLED" =
                        "${if default-looper-enabled then "True" else "False"}";
                    }
                  )
                  (
                    unlessNull default-looper-period {
                      "TICKLER_SERVER_LOOPERS_DEFAULT_PERIOD" =
                        "${builtins.toString default-looper-period}";
                    }
                  )
                  (
                    unlessNull default-looper-retry-delay {
                      "TICKLER_SERVER_LOOPERS_DEFAULT_RETRY_DELAY" =
                        "${builtins.toString default-looper-retry-delay}";
                    }
                  )
                  (
                    unlessNull default-looper-retry-amount {
                      "TICKLER_SERVER_LOOPERS_DEFAULT_RETRY_AMOUNT" =
                        "${builtins.toString default-looper-retry-amount}";
                    }
                  )
                ]
              )
              (
                with cfg.loopers;

                concatAttrs [
                  (
                    unlessNull triggerer ( mkLooperVars "TRIGGERER" triggerer )
                  )
                  ( unlessNull emailer ( mkLooperVars "EMAILER" emailer ) )
                  (
                    unlessNull triggered-intray-item-scheduler (
                      mkLooperVars "TRIGGERED_INTRAY_ITEM_SCHEDULER" triggered-intray-item-scheduler
                    )
                  )
                  (
                    unlessNull triggered-intray-item-sender (
                      mkLooperVars "TRIGGERED_INTRAY_ITEM_SENDER" triggered-intray-item-sender
                    )
                  )
                  (
                    unlessNull verification-email-converter (
                      mkLooperVars "VERIFICATION_EMAIL_CONVERTER" verification-email-converter
                    )
                  )
                  (
                    unlessNull triggered-email-scheduler (
                      mkLooperVars "TRIGGERED_EMAIL_SCHEDULER" triggered-email-scheduler
                    )
                  )
                  (
                    unlessNull triggered-email-converter (
                      mkLooperVars "TRIGGERED_EMAIL_CONVERTER" triggered-email-converter
                    )
                  )
                ]
              )
            ];
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${tickler-pkgs.tickler-web-server}/bin/tickler-web-server \
                serve \
                --admin \
                syd
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
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
