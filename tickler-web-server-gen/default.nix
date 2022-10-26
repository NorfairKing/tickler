{ mkDerivation, base, containers, genvalidity-sydtest, http-client
, http-types, intray-client, intray-server-gen, lib, monad-logger
, mtl, path, path-io, persistent, pretty-show, QuickCheck, sydtest
, sydtest-discover, sydtest-persistent, sydtest-yesod, text
, tickler-client, tickler-data, tickler-data-gen
, tickler-server-gen, tickler-web-server, time, typed-uuid
, unordered-containers, yesod-auth
}:
mkDerivation {
  pname = "tickler-web-server-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity-sydtest http-client http-types monad-logger mtl
    path path-io persistent pretty-show QuickCheck sydtest
    sydtest-yesod text tickler-client tickler-data tickler-data-gen
    tickler-server-gen tickler-web-server time typed-uuid
    unordered-containers yesod-auth
  ];
  testHaskellDepends = [
    base containers genvalidity-sydtest http-client http-types
    intray-client intray-server-gen mtl path path-io persistent
    pretty-show QuickCheck sydtest sydtest-persistent sydtest-yesod
    text tickler-client tickler-data tickler-data-gen
    tickler-server-gen tickler-web-server time typed-uuid yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
