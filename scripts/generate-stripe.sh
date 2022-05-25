sudo rm -rf stripe-client
cp -rHL $(nix-build nix/pkgs.nix -A generatedTicklerStripeCode) tickler-stripe-client
sudo chmod -R +rwx tickler-stripe-client
