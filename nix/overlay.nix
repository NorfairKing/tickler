final: prev:
with final.lib;
with final.haskell.lib;
let
  stripe-spec = builtins.fetchGit {
    url = "https://github.com/stripe/openapi";
    rev = "c48cf54aab65f4966ba285bdfaf86ed52f5fb70c";
  };
  generatedStripe = final.generateOpenAPIClient {
    name = "tickler-stripe-client";
    configFile = ../stripe-client-gen.yaml;
    src = stripe-spec + "/openapi/spec3.yaml";
  };

in
{
  ticklerReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.haskellPackages.ticklerPackages;

  ticklerRelease =
    final.symlinkJoin {
      name = "tickler-release";
      paths = attrValues final.ticklerReleasePackages;
    };

  generatedTicklerStripeCode = generatedStripe;

  haskellPackages =
    prev.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              let
                generatedStripePackage = self.callPackage (generatedStripe + "/default.nix") { };

                ticklerPkg = name:
                  overrideCabal
                    (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (../${name}) "--no-hpack" { }))
                    (old: {
                      doBenchmark = true;
                      doHaddock = false;
                      doCoverage = false;
                      doHoogle = false;
                      doCheck = false; # Only check the release version.
                      hyperlinkSource = false;
                      enableLibraryProfiling = false;
                      enableExecutableProfiling = false;

                      configureFlags = (old.configureFlags or [ ]) ++ [
                        # Optimisations
                        "--ghc-options=-O2"
                        # Extra warnings
                        "--ghc-options=-Wall"
                        "--ghc-options=-Wincomplete-uni-patterns"
                        "--ghc-options=-Wincomplete-record-updates"
                        "--ghc-options=-Wpartial-fields"
                        "--ghc-options=-Widentities"
                        "--ghc-options=-Wredundant-constraints"
                        "--ghc-options=-Wcpp-undef"
                        "--ghc-options=-Werror"
                      ];
                      # Ugly hack because we can't just add flags to the 'test' invocation.
                      # Show test output as we go, instead of all at once afterwards.
                      testTarget = (old.testTarget or "") + " --show-details=direct";

                    });
                ticklerPkgWithComp = exeName: name: generateOptparseApplicativeCompletion exeName (ticklerPkg name);
                ticklerPkgWithOwnComp = name: ticklerPkgWithComp name name;
                ticklerPackages =
                  {
                    "tickler-data" = ticklerPkg "tickler-data";
                    "tickler-data-gen" = ticklerPkg "tickler-data-gen";
                    "tickler-api" = ticklerPkg "tickler-api";
                    "tickler-api-gen" = ticklerPkg "tickler-api-gen";
                    "tickler-client" = ticklerPkg "tickler-client";
                    "tickler-stripe-client" = generatedStripePackage;
                    "tickler-server" = ticklerPkgWithOwnComp "tickler-server";
                    "tickler-server-gen" = ticklerPkg "tickler-server-gen";
                    "tickler-web-server" =
                      let
                        bulma-css =
                          builtins.fetchurl {
                            url = "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css";
                            sha256 = "sha256:0nbwcsa1gi36f2aq9y96bap7glkp40k3g2bjb9s1vmg0011sri1v";
                          };
                        bulma-tooltip-css =
                          builtins.fetchurl {
                            url = "https://cdn.jsdelivr.net/npm/bulma-tooltip@3.0.2/dist/css/bulma-tooltip.min.css";
                            sha256 = "sha256:0xih9z80znhb3svn2xs6jbhh1mfkbywa1yjrq6p2llxk80md2yaw";
                          };
                        jquery-js =
                          builtins.fetchurl {
                            url = "https://code.jquery.com/jquery-3.1.1.min.js";
                            sha256 = "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
                          };
                      in
                      overrideCabal (ticklerPkgWithOwnComp "tickler-web-server") (
                        old:
                        {
                          preConfigure =
                            ''
                              ${old.preConfigure or ""}

                              mkdir -p static/jquery/
                              ln -s ${jquery-js} static/jquery/jquery.min.js
                              mkdir -p static/bulma/
                              ln -s ${bulma-css} static/bulma/bulma.min.css
                              ln -s ${bulma-tooltip-css} static/bulma/bulma-tooltip.min.css
                            '';
                        }
                      );
                    "tickler-web-server-gen" = ticklerPkg "tickler-web-server-gen";
                    "tickler-web-server-webdriver" = final.haskellPackages.sydtest-webdriver.enableWebdriver (ticklerPkg "tickler-web-server-webdriver");
                  };
              in
              {
                inherit ticklerPackages;
              } // ticklerPackages
          );
      }
    );
}
