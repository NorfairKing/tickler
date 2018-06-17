final:
  previous:
    with final.haskell.lib;
    {
      haskellPackages = previous.haskellPackages.override {
        overrides = self:
          super:
            let
              ticklerPkg = name:
                disableLibraryProfiling (super.callCabal2nix name (./. + "/${name}") {});
            in final.lib.genAttrs [
              "tickler-data"
              "tickler-data-gen"
              "tickler-api"
              "tickler-api-gen"
              "tickler-cli"
              "tickler-client"
              "tickler-data"
              "tickler-data-gen"
              "tickler-server"
              "tickler-server-test-utils"
              "tickler-web-server"
            ] ticklerPkg;
      };
    }
