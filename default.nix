{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, data-default, directory, errors, exceptions, filepath
, hjsonschema, hlint, hspec, http-client, http-types, integer-gmp
, lens, lens-aeson, mtl, random, stdenv, tasty, tasty-hunit, text
, transformers, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "couch-simple";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring data-default exceptions
    http-client http-types integer-gmp mtl text transformers
    unordered-containers uuid vector
  ];
  testHaskellDepends = [
    aeson base bytestring data-default directory errors filepath
    hjsonschema hlint hspec http-client http-types lens lens-aeson
    random tasty tasty-hunit text transformers unordered-containers
    uuid
  ];
  homepage = "https://github.com/mdorman/couch-simple";
  description = "A modern, lightweight, complete client for CouchDB";
  license = stdenv.lib.licenses.mit;
}
