{ mkDerivation, aeson, autodocodec, base, base16-bytestring, bcrypt
, bytestring, hashable, http-api-data, intray-data, lib
, path-pieces, persistent, persistent-template, random
, servant-client-core, text, time, typed-uuid, validity
, validity-bytestring, validity-text, validity-time, validity-uuid
}:
mkDerivation {
  pname = "tickler-data";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base16-bytestring bcrypt bytestring hashable
    http-api-data intray-data path-pieces persistent
    persistent-template random servant-client-core text time typed-uuid
    validity validity-bytestring validity-text validity-time
    validity-uuid
  ];
  license = "unknown";
}
