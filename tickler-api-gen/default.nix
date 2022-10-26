{ mkDerivation, base, bytestring, genvalidity, genvalidity-aeson
, genvalidity-bytestring, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, genvalidity-typed-uuid, genvalidity-uuid, intray-data-gen, lib
, QuickCheck, sydtest, sydtest-discover, text, tickler-api
, tickler-data, tickler-data-gen
}:
mkDerivation {
  pname = "tickler-api-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity genvalidity-aeson
    genvalidity-bytestring genvalidity-text genvalidity-time
    genvalidity-typed-uuid genvalidity-uuid intray-data-gen QuickCheck
    text tickler-api tickler-data tickler-data-gen
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text QuickCheck sydtest text tickler-api tickler-data
    tickler-data-gen
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
