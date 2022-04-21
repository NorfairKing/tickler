final: previous:
with final.lib;
with final.haskell.lib;
{
  ticklerPackages =
    let
      pathFor = name: final.gitignoreSource (../. + "/${name}");
      ticklerPkg = name:
        overrideCabal
          (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (pathFor name) "--no-hpack" { }))
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
    in
    {
      "tickler-data" = ticklerPkg "tickler-data";
      "tickler-data-gen" = ticklerPkg "tickler-data-gen";
      "tickler-api" = ticklerPkg "tickler-api";
      "tickler-api-gen" = ticklerPkg "tickler-api-gen";
      "tickler-client" = ticklerPkg "tickler-client";
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

  ticklerReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.ticklerPackages;

  ticklerRelease =
    final.symlinkJoin {
      name = "tickler-release";
      paths = attrValues final.ticklerReleasePackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              let
                servantAuthRepo =
                  final.fetchFromGitHub {
                    owner = "haskell-servant";
                    repo = "servant-auth";
                    rev = "23971e889f8cbe8790305bda8915f00aa8be5ad9";
                    sha256 =
                      "sha256:0q1n0s126ywqw3g9xiiaw59s9jn2543v7p4zgxw99p68pihdlysv";
                  };
                persistentRepo =
                  final.fetchFromGitHub {
                    owner = "yesodweb";
                    repo = "persistent";
                    rev = "333be4996eb6eea2dc37d3a14858b668f0b9e381";
                    sha256 =
                      "sha256:1j76s7666vadm4q1ma73crkrks6q6nskzb3jqaf6rp2qmw1phfpr";
                  };

                stripeHaskellRepo =
                  final.fetchFromGitHub {
                    owner = "NorfairKing";
                    repo = "stripe";
                    rev = "008e992cae9c9bdb025bcf575c1bdf1037632a8a";
                    sha256 =
                      "sha256:1sxp8phdw1ahndy6h9q4ad0hdfraxyy5qnjd7w80v6m83py419gk";
                  };
                stripeHaskellPkg =
                  name:
                  dontCheck (
                    self.callCabal2nix name (stripeHaskellRepo + "/${name}") { }
                  );
                servantAuthPkg =
                  name:
                  doJailbreak (
                    self.callCabal2nix name (servantAuthRepo + "/${name}") { }
                  );
                persistentPkg =
                  name:
                  overrideCabal
                    (
                      # Because there is some nastiness that makes nix think we need the haskell sqlite library.
                      self.callCabal2nix name (persistentRepo + "/${name}") { }
                    )
                    (
                      old:
                      {
                        librarySystemDepends = [ final.sqlite ];
                      }
                    );
                yesodAutoReloadRepo = builtins.fetchGit {
                  url = "https://github.com/NorfairKing/yesod-autoreload";
                  rev = "f4f03bae0b9c1916838bb1c52a7182ac5afb28e0";
                };

              in
              {
                envparse = self.callHackage "envparse" "0.4.1" { };
                yesod-autoreload = self.callCabal2nix "yesod-autoreload" yesodAutoReloadRepo { };
                # Temporary hack until we can upgrade.
                sydtest-persistent-sqlite = dontCheck super.sydtest-persistent-sqlite;
                sydtest-yesod = dontCheck super.sydtest-yesod;
              } // genAttrs [
                "stripe-core"
                "stripe-haskell"
                "stripe-http-client"
                "stripe-http-streams"
              ]
                stripeHaskellPkg // genAttrs [
                "servant-auth"
                "servant-auth-client"
                "servant-auth-docs"
                "servant-auth-swagger"
                "servant-auth-server"
              ]
                servantAuthPkg // genAttrs [
                "persistent"
                "persistent-sqlite"
                "persistent-template"
              ]
                persistentPkg // final.ticklerPackages
          );
      }
    );
}
