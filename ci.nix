let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
{
  release = pkgs.ticklerRelease;
  pre-commit-check = pre-commit.run;
  hoogle = pkgs.ticklerHoogle;
  shell = pkgs.symlinkJoin {
    name = "shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
  nixos-module-test = import ./nix/nixos-module-test.nix { inherit pkgs; };
}
