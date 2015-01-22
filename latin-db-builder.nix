# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, filepath, HDBC, HDBCSqlite3, hexpat, HUnit, parsec
, regexTdfa, regexTdfaText, text, textIcu
}:

cabal.mkDerivation (self: {
  pname = "latin-db";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    HDBC HDBCSqlite3 hexpat parsec regexTdfa regexTdfaText text textIcu
  ];
  testDepends = [
    filepath hexpat HUnit parsec regexTdfa regexTdfaText text
  ];
  meta = {
    description = "Process Wiktionary dumps to extract structured Latin data";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
