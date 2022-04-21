let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "tickler-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    chromedriver
    chromium
    haskellPackages.autoexporter
    killall
    selenium-server-standalone
    unzip
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + pkgs.haskellPackages.sydtest-webdriver.setupFontsConfigScript;
}
