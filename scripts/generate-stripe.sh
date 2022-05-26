sudo rm -rf intray-stripe-client
sudo rm -rf tickler-stripe-client
cp -rHL $(nix-build nix/pkgs.nix -A generatedIntrayStripeCode) intray-stripe-client
cp -rHL $(nix-build nix/pkgs.nix -A generatedTicklerStripeCode) tickler-stripe-client
sudo chmod -R +rwx intray-stripe-client
sudo chmod -R +rwx tickler-stripe-client
