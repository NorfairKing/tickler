final: previous:
with final.haskell.lib;

{
  ticklerPackages =
    let
      pathFor =
        name:
          builtins.path {
            inherit name;
            path = ../. + "/${name}";
            filter =
              path: type:
                !final.lib.hasPrefix "." (baseNameOf path);
          };
      ticklerPkg =
        name:
          failOnAllWarnings (
            disableLibraryProfiling ( final.haskellPackages.callCabal2nix name ( pathFor name ) {} )
          );
    in
      final.lib.genAttrs [
        "tickler-data"
        "tickler-data-gen"
        "tickler-api"
        "tickler-api-gen"
        "tickler-cli"
        "tickler-client"
        "tickler-client-gen"
        "tickler-data"
        "tickler-data-gen"
        "tickler-server"
        "tickler-server-gen"
      ] ticklerPkg // {
        "tickler-web-server" =
        let semantic-js = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js;
              sha256 = "sha256:0ll00jawcwd4nj568sj7lfp2ixrni9wqf37sz5nhz6wggjk9xhdp";
            };
            semantic-css = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css;
              sha256 = "sha256:0m13jdkv3vdqr0pbr1zfc2ndsafr2p5mnfzkbm7pd8v1ylwy8rpn";
            };
            jquery-js = builtins.fetchurl {
              url = https://code.jquery.com/jquery-3.1.1.min.js;
              sha256 = "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
            };
            icons-ttf = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf;
              sha256 = "sha256:1nm34hrh3inyrq7cbkh47g8m2hbqpsgkzbdrpfiiii7m8bsq2zyb";
            };
            icons-woff = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff;
              sha256 = "sha256:1qgzlmd80c4ckh9zpfl2qzjvg389hvmkdhkv8amyq4c71y2a9dlm";
            };
            icons-woff2 = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2;
              sha256 = "sha256:1lqd60f1pml8zc93hgwcm6amkcy6rnbq3cyxqv5a3a25jnsnci23";
            };
        in overrideCabal (ticklerPkg "tickler-web-server") (old: {
          preConfigure = ''
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
        });
      };
  haskellPackages =
    previous.haskellPackages.extend (
      self: super:
        let
          ticklerPkg =
            name:
              disableLibraryProfiling ( super.callCabal2nix name ( ./. + "/${name}" ) {} );
          stripeHaskellRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "stripe";
              rev = "7ced8cef1e932d3fb222dfb3c79c25595cdc82ab";
              sha256 =
                "sha256:04dsfx568hmmrr7zg5gbqwipdiy7lvpckfk2ayln6gh6zf9jxl13";
            };
          looperRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "looper";
              rev = "929a8ad6a99a84624767bd9d619cc5318c6bda56";
              sha256 =
                "07wc2as7p2pz08a9qfx2jm3kz1cvfg73d872il3zhyplbd6yhzbx";
            };
          stripeHaskellPkg =
            name:
              dontCheck (
                self.callCabal2nix name ( stripeHaskellRepo + "/${name}" ) {}
              );
        in
          with final.haskellPackages;

          {
              amazonka = callHackage "amazonka" "1.6.1" {};
              amazonka-test = callHackage "amazonka-test" "1.6.1" {};
              amazonka-core = callHackage "amazonka-core" "1.6.1" {};
              amazonka-ses = callHackage "amazonka-ses" "1.6.1" {};
              looper = self.callCabal2nix "looper" looperRepo {};
            } // final.lib.genAttrs [
              "stripe-core"
              "stripe-haskell"
              "stripe-http-client"
              "stripe-http-streams"
            ] stripeHaskellPkg // final.ticklerPackages
    );
}
