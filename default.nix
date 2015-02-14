{ mkDerivation, aeson, attoparsec, base, bytestring, data-default
, either, errors, exceptions, hlint, hspec, http-client, http-types
, lens, lens-aeson, mtl, random, stdenv, tasty, tasty-hunit, text
, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "couch-simple";
  version = "0.0.0.0";
  src = ./.;
  buildDepends = [
    aeson attoparsec base bytestring data-default either exceptions
    http-client http-types lens lens-aeson mtl text transformers
    unordered-containers uuid
  ];
  testDepends = [
    aeson base data-default either errors hlint hspec http-client
    http-types lens lens-aeson random tasty tasty-hunit text
    transformers unordered-containers uuid
  ];
  homepage = "https://github.com/mdorman/couch-simple";
  description = "A lightweight modern client for CouchDB";
  license = stdenv.lib.licenses.mit;
}
