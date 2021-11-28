final: previous:
with final.haskell.lib;

{
  ticklerPackages =
    let
      pathFor = name: final.gitignoreSource (../. + "/${name}");
      ticklerPkg =
        name:
        failOnAllWarnings (
          disableLibraryProfiling (final.haskellPackages.callCabal2nix name (pathFor name) { })
        );
      ticklerPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (ticklerPkg name);
      ticklerPkgWithOwnComp = name: ticklerPkgWithComp name name;
    in
    {
      "tickler-data" = ticklerPkg "tickler-data";
      "tickler-data-gen" = ticklerPkg "tickler-data-gen";
      "tickler-api" = ticklerPkg "tickler-api";
      "tickler-api-gen" = ticklerPkg "tickler-api-gen";
      "tickler-cli" = ticklerPkgWithComp "tickler" "tickler-cli";
      "tickler-client" = ticklerPkg "tickler-client";
      "tickler-client-gen" = ticklerPkg "tickler-client-gen";
      "tickler-server" = ticklerPkgWithOwnComp "tickler-server";
      "tickler-server-gen" = ticklerPkg "tickler-server-gen";
      "tickler-web-server" =
        let
          semantic-js =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js;
              sha256 =
                "sha256:0ll00jawcwd4nj568sj7lfp2ixrni9wqf37sz5nhz6wggjk9xhdp";
            };
          semantic-css =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css;
              sha256 =
                "sha256:0m13jdkv3vdqr0pbr1zfc2ndsafr2p5mnfzkbm7pd8v1ylwy8rpn";
            };
          jquery-js =
            builtins.fetchurl {
              url = https://code.jquery.com/jquery-3.1.1.min.js;
              sha256 =
                "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
            };
          icons-ttf =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf;
              sha256 =
                "sha256:1nm34hrh3inyrq7cbkh47g8m2hbqpsgkzbdrpfiiii7m8bsq2zyb";
            };
          icons-woff =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff;
              sha256 =
                "sha256:1qgzlmd80c4ckh9zpfl2qzjvg389hvmkdhkv8amyq4c71y2a9dlm";
            };
          icons-woff2 =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2;
              sha256 =
                "sha256:1lqd60f1pml8zc93hgwcm6amkcy6rnbq3cyxqv5a3a25jnsnci23";
            };
        in
        overrideCabal (ticklerPkgWithOwnComp "tickler-web-server") (
          old:
          {
            preConfigure =
              ''
                ${old.preConfigure or ""}

                mkdir -p static/
                cp ${jquery-js} static/jquery.min.js
                mkdir -p static/semantic/
                cp ${semantic-css} static/semantic/semantic.min.css
                cp ${semantic-js} static/semantic/semantic.min.js
                mkdir -p static/semantic/themes/default/assets/fonts
                cp ${icons-ttf} static/semantic/themes/default/assets/fonts/icons.ttf
                cp ${icons-woff} static/semantic/themes/default/assets/fonts/icons.woff
                cp ${icons-woff2} static/semantic/themes/default/assets/fonts/icons.woff2
              '';
          }
        );
    };

  ticklerRelease =
    final.symlinkJoin {
      name = "tickler-release";
      paths = final.lib.attrValues final.ticklerPackages;
    };

  haskellPackages =
    previous.haskellPackages.extend (
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
        with final.haskellPackages;

        {
          yesod-autoreload = self.callCabal2nix "yesod-autoreload" yesodAutoReloadRepo { };
        } // final.lib.genAttrs [
          "stripe-core"
          "stripe-haskell"
          "stripe-http-client"
          "stripe-http-streams"
        ]
          stripeHaskellPkg // final.lib.genAttrs [
          "servant-auth"
          "servant-auth-client"
          "servant-auth-docs"
          "servant-auth-swagger"
          "servant-auth-server"
        ]
          servantAuthPkg // final.lib.genAttrs [
          "persistent"
          "persistent-sqlite"
          "persistent-template"
        ]
          persistentPkg // final.ticklerPackages
    );
}
