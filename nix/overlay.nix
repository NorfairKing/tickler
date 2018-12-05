final:
  previous:
    with final.haskell.lib;
    {
      ticklerPackages = 
        let
          pathFor = name:
            builtins.path {
              inherit name;
              path = ../. + "/${name}";
              filter = path:
                type:
                  !final.lib.hasPrefix "." (baseNameOf path);
            };
          ticklerPkg = name:
          failOnAllWarnings (disableLibraryProfiling (final.haskellPackages.callCabal2nix name (pathFor name) {}));
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
      haskellPackages = previous.haskellPackages.extend (
        self:
          super:
            let
              ticklerPkg = name:
                disableLibraryProfiling (super.callCabal2nix name (./. + "/${name}") {});
            in with final.haskellPackages; {
              amazonka = callHackage "amazonka" "1.6.0" {};
              amazonka-test = callHackage "amazonka-test" "1.6.0" {};
              amazonka-core = callHackage "amazonka-core" "1.6.0" {};
              amazonka-ses = callHackage "amazonka-ses" "1.6.0" {};
            } // final.ticklerPackages
      );
    }
