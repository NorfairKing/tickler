{
  description = "tickler";
  nixConfig = {
    extra-substituters = "https://tickler.cachix.org";
    extra-trusted-public-keys = "tickler.cachix.org-1:hOYeQ5gFg2xfpA3fxBBS2ixfsvJL2t6wUjdFAVL1Cqc=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    mergeless.url = "github:NorfairKing/mergeless?ref=flake";
    mergeless.flake = false;
    yesod-autoreload.url = "github:NorfairKing/yesod-autoreload?ref=flake";
    yesod-autoreload.flake = false;
    yesod-static-remote.url = "github:NorfairKing/yesod-static-remote?ref=flake";
    yesod-static-remote.flake = false;
    looper.url = "github:NorfairKing/looper?ref=flake";
    looper.flake = false;
    pretty-relative-time.url = "github:NorfairKing/pretty-relative-time?ref=flake";
    pretty-relative-time.flake = false;
    mergeful.url = "github:NorfairKing/mergeful?ref=flake";
    mergeful.flake = false;
    typed-uuid.url = "github:NorfairKing/typed-uuid?ref=flake";
    typed-uuid.flake = false;
    intray.url = "github:NorfairKing/intray";
    intray.flake = false;
    openapi-code-generator.url = "github:Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator?ref=flake";
    linkcheck.url = "github:NorfairKing/linkcheck?ref=flake";
    linkcheck.flake = false;
    seocheck.url = "github:NorfairKing/seocheck?ref=flake";
    seocheck.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , mergeless
    , yesod-autoreload
    , yesod-static-remote
    , looper
    , pretty-relative-time
    , mergeful
    , typed-uuid
    , intray
    , openapi-code-generator
    , linkcheck
    , seocheck
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (mergeless + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (yesod-autoreload + "/nix/overlay.nix"))
          (import (yesod-static-remote + "/nix/overlay.nix"))
          (import (looper + "/nix/overlay.nix"))
          (import (pretty-relative-time + "/nix/overlay.nix"))
          (import (mergeful + "/nix/overlay.nix"))
          (import (typed-uuid + "/nix/overlay.nix"))
          (import (intray + "/nix/overlay.nix"))
          (_:_: { generateOpenAPIClient = openapi-code-generator.packages.${system}.default.passthru.generateOpenAPIClient; })
          (import (linkcheck + "/nix/overlay.nix"))
          (import (seocheck + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix {
        inherit (pkgs.ticklerReleasePackages) tickler-server tickler-web-server;
        inherit (pkgs.haskellPackages.looper.passthru) mkLooperOption;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.ticklerRelease;
        generatedIntrayStripeCode = pkgs.generatedIntrayStripeCode;
        generatedTicklerStripeCode = pkgs.generatedTicklerStripeCode;
      };
      checks.${system} = {
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit pkgs;
          tickler-nixos-module-factory = self.nixosModuleFactories.${system}.default;
        };
        dependency-graph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph {
          packages = builtins.attrNames pkgs.haskellPackages.ticklerPackages;
          inherit (pkgs) haskellPackages;
        };
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "tickler-api"
            "tickler-client"
            "tickler-data"
            "tickler-server"
            # "tickler-stripe-client" # No need to cover generated code
            "tickler-web-server"
          ];
          coverage = [
            "tickler-api-gen"
            "tickler-data-gen"
            "tickler-server-gen"
            "tickler-web-server-gen"
            "tickler-web-server-webdriver"
          ];
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "tickler-shell";
        packages = p: builtins.attrValues p.ticklerPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
          chromedriver
          chromium
          selenium-server-standalone

        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = ''
          ${self.checks.${system}.pre-commit.shellHook}
          ${pkgs.haskellPackages.sydtest-webdriver.setupFontsConfigScript}
        '';
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
    };
}
