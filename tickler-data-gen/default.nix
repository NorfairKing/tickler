{ mkDerivation, base, bytestring, genvalidity
, genvalidity-bytestring, genvalidity-persistent
, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-sydtest-persistent, genvalidity-text
, genvalidity-time, genvalidity-typed-uuid, genvalidity-uuid
, intray-api-gen, intray-data, lib, QuickCheck, servant-client-core
, sydtest, sydtest-discover, sydtest-persistent-sqlite, text
, tickler-data, time, validity, validity-bytestring, validity-text
, validity-time
}:
mkDerivation {
  pname = "tickler-data-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity genvalidity-bytestring
    genvalidity-persistent genvalidity-text genvalidity-time
    genvalidity-typed-uuid genvalidity-uuid intray-api-gen QuickCheck
    servant-client-core text tickler-data validity validity-bytestring
    validity-text validity-time
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-sydtest-persistent genvalidity-text intray-data
    QuickCheck sydtest sydtest-persistent-sqlite text tickler-data time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
