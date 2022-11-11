{ mkDerivation, aeson, base, bytestring, containers, lib, servant
, servant-auth-client, servant-client, servant-flatten, text
, tickler-api, tickler-data, time, typed-uuid, validity
, validity-bytestring, validity-containers, validity-text
, validity-time, validity-uuid
}:
mkDerivation {
  pname = "tickler-client";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers servant servant-auth-client
    servant-client servant-flatten text tickler-api tickler-data time
    typed-uuid validity validity-bytestring validity-containers
    validity-text validity-time validity-uuid
  ];
  license = "unknown";
}
