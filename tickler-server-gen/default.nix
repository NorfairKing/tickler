{ mkDerivation, aeson, base, bytestring, cache, containers, cookie
, criterion, genvalidity-bytestring, genvalidity-sydtest
, genvalidity-text, genvalidity-time, http-client, http-types
, intray-client, intray-server, intray-server-gen, lib
, monad-logger, path, path-io, persistent, persistent-sqlite
, QuickCheck, servant, servant-auth-client, servant-auth-server
, servant-client, servant-server, sydtest, sydtest-aeson
, sydtest-discover, sydtest-persistent, sydtest-persistent-sqlite
, sydtest-wai, text, tickler-api, tickler-api-gen, tickler-client
, tickler-data, tickler-data-gen, tickler-server
, tickler-stripe-client, time, typed-uuid, unordered-containers
, uuid
}:
mkDerivation {
  pname = "tickler-server-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cache cookie genvalidity-bytestring
    genvalidity-sydtest genvalidity-text genvalidity-time http-client
    http-types intray-server intray-server-gen monad-logger path
    path-io persistent-sqlite QuickCheck servant servant-auth-client
    servant-auth-server servant-client servant-server sydtest
    sydtest-persistent-sqlite sydtest-wai text tickler-api
    tickler-api-gen tickler-client tickler-data tickler-data-gen
    tickler-server time typed-uuid
  ];
  testHaskellDepends = [
    aeson base bytestring containers genvalidity-bytestring
    genvalidity-sydtest genvalidity-text genvalidity-time http-types
    intray-client intray-server-gen monad-logger path path-io
    persistent QuickCheck servant servant-client sydtest sydtest-aeson
    sydtest-persistent text tickler-api tickler-api-gen tickler-client
    tickler-data tickler-data-gen tickler-server tickler-stripe-client
    time typed-uuid unordered-containers uuid
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base bytestring cookie criterion http-client QuickCheck servant
    servant-auth-client servant-client sydtest tickler-client
  ];
  license = "unknown";
}
