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
        "tickler-server-test-utils"
        "tickler-web-server"
      ] ticklerPkg;
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
          mergelessRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "mergeless";
              rev = "3d5f4b54cc2c4c8c6f33a716bc6b67f376b8d1d5";
              sha256 =
                "sha256:0far86wdprvyk8i50y4i5wzc0vcqj5ksdf90jnyyalrbklgxxgkv";
            };
          stripeHaskellPkg =
            name:
              dontCheck (
                self.callCabal2nix name ( stripeHaskellRepo + "/${name}" ) {}
              );

          mergelessPkg =
            name:
              self.callCabal2nix name ( mergelessRepo + "/${name}" ) {};

        in
          with final.haskellPackages;

          {
              amazonka = callHackage "amazonka" "1.6.1" {};
              amazonka-test = callHackage "amazonka-test" "1.6.1" {};
              amazonka-core = callHackage "amazonka-core" "1.6.1" {};
              amazonka-ses = callHackage "amazonka-ses" "1.6.1" {};
              # http-client = callHackage "http-client" "0.5.14" {};
              # servant-auth-server = doJailbreak (super.servant-auth-server);
              looper = self.callCabal2nix "looper" looperRepo {};

            } // final.lib.genAttrs [
              "stripe-core"
              "stripe-haskell"
              "stripe-http-client"
              "stripe-http-streams"
            ] stripeHaskellPkg // final.lib.genAttrs [
              "mergeless"
              "genvalidity-mergeless"
            ] mergelessPkg // final.ticklerPackages
    );
}
