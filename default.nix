{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, couchdb, data-default, directory, exceptions, filepath
, hjsonschema, hlint, http-client, http-types, integer-gmp, mtl
, random, stdenv, tasty, tasty-hunit, text, transformers
, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "couch-simple";
  version = "0.0.1.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring data-default exceptions
    http-client http-types integer-gmp mtl text transformers
    unordered-containers uuid vector
  ];
  testHaskellDepends = [
    aeson base bytestring data-default directory exceptions filepath
    hjsonschema hlint http-client http-types random tasty tasty-hunit
    text transformers unordered-containers uuid
  ];
  testToolDepends = [ couchdb ];
  homepage = "https://github.com/mdorman/couch-simple";
  description = "A modern, lightweight, complete client for CouchDB";
  license = stdenv.lib.licenses.mit;
}
