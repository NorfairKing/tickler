{ mkDerivation, base, bytestring, containers, genvalidity-sydtest
, http-client, http-types, intray-client, intray-server-gen, lib
, mtl, persistent, servant-auth-client, sydtest, sydtest-discover
, sydtest-webdriver, sydtest-webdriver-yesod, sydtest-yesod, text
, tickler-api, tickler-client, tickler-server-gen
, tickler-web-server, tickler-web-server-gen, time
, unordered-containers, webdriver, yesod-auth
}:
mkDerivation {
  pname = "tickler-web-server-webdriver";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers http-client intray-client mtl persistent
    servant-auth-client sydtest sydtest-webdriver
    sydtest-webdriver-yesod sydtest-yesod text tickler-api
    tickler-client tickler-server-gen tickler-web-server
    tickler-web-server-gen time unordered-containers webdriver
    yesod-auth
  ];
  testHaskellDepends = [
    base bytestring containers genvalidity-sydtest http-client
    http-types intray-client intray-server-gen mtl persistent sydtest
    sydtest-webdriver sydtest-webdriver-yesod sydtest-yesod text
    tickler-api tickler-client tickler-server-gen tickler-web-server
    tickler-web-server-gen time webdriver
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
