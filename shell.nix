{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "tickler-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    chromedriver
    chromium
    haskellPackages.autoexporter
    killall
    selenium-server-standalone
    stripe-cli
    unzip
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + pkgs.haskellPackages.sydtest-webdriver.setupFontsConfigScript;
}
