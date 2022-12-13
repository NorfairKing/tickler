sudo rm -rf intray-stripe-client
nix build .#generatedIntrayStripeCode
cp -rHL result intray-stripe-client
sudo chmod -R +rwx intray-stripe-client
sudo rm -rf tickler-stripe-client
nix build .#generatedTicklerStripeCode
cp -rHL result tickler-stripe-client
sudo chmod -R +rwx tickler-stripe-client
