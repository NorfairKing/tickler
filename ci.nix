let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;

in
pkgs.ticklerPackages // {
  pre-commit-check = pre-commit-hooks.run;
}
