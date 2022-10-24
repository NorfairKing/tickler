{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, base, base16-bytestring, bytestring, cookie, data-default
, envparse, http-client, http-client-tls, http-types, intray-data
, lib, monad-logger, mtl, optparse-applicative, path, path-io
, pretty-relative-time, pretty-show, servant, servant-auth-client
, servant-client, servant-client-core, shakespeare
, template-haskell, text, tickler-api, tickler-client, tickler-data
, tickler-server, time, transformers, typed-uuid
, unordered-containers, yesod, yesod-auth, yesod-autoreload
, yesod-static, yesod-static-remote
}:
mkDerivation {
  pname = "tickler-web-server";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base
    base16-bytestring bytestring cookie data-default envparse
    http-client http-client-tls http-types intray-data monad-logger mtl
    optparse-applicative path path-io pretty-relative-time pretty-show
    servant servant-auth-client servant-client servant-client-core
    shakespeare template-haskell text tickler-api tickler-client
    tickler-data tickler-server time transformers typed-uuid
    unordered-containers yesod yesod-auth yesod-autoreload yesod-static
    yesod-static-remote
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "tickler-web-server";
}
