nix build .#generatedTicklerStripeCode

rm -rf intray-stripe-client
mkdir -p intray-stripe-client
cp -R result/* intray-stripe-client
chmod -R 764 intray-stripe-client

nix build .#generatedIntrayStripeCode

rm -rf tickler-stripe-client
mkdir -p tickler-stripe-client
cp -R result/* tickler-stripe-client
chmod -R 764 tickler-stripe-client
