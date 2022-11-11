{ mkDerivation, aeson, autodocodec, base, bytestring, intray-api
, intray-data, lib, servant, servant-auth, servant-auth-server
, text, tickler-data, time, typed-uuid, validity, validity-aeson
, validity-bytestring, validity-text, validity-time, validity-uuid
}:
mkDerivation {
  pname = "tickler-api";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring intray-api intray-data servant
    servant-auth servant-auth-server text tickler-data time typed-uuid
    validity validity-aeson validity-bytestring validity-text
    validity-time validity-uuid
  ];
  license = "unknown";
}
