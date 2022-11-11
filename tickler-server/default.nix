{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-ses, async
, autodocodec, autodocodec-yaml, base, blaze-html, bytestring
, cache, conduit, cookie, envparse, esqueleto, exceptions
, http-client, http-client-tls, intray-client, intray-server, jose
, lens, lib, looper, monad-logger, mtl, optparse-applicative, path
, path-io, persistent, persistent-sqlite, pretty-show
, resource-pool, resourcet, retry, servant, servant-auth-client
, servant-auth-server, servant-client, servant-server, shakespeare
, text, tickler-api, tickler-client, tickler-data
, tickler-stripe-client, time, typed-uuid, unliftio
, unordered-containers, wai, wai-cors, warp
}:
mkDerivation {
  pname = "tickler-server";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-ses async autodocodec
    autodocodec-yaml base blaze-html bytestring cache conduit cookie
    envparse esqueleto exceptions http-client http-client-tls
    intray-client intray-server jose lens looper monad-logger mtl
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-show resource-pool resourcet retry servant
    servant-auth-client servant-auth-server servant-client
    servant-server shakespeare text tickler-api tickler-client
    tickler-data tickler-stripe-client time typed-uuid unliftio
    unordered-containers wai wai-cors warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "tickler-server";
}
