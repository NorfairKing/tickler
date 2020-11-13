let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;

in
{
  release = pkgs.ticklerRelease;
  nixos-module-test = import ./nix/nixos-module-test.nix { inherit pkgs; };
  pre-commit-check = pre-commit-hooks.run;
}
