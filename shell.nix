{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

# TODO: I know there's an easier way to express this...
import ./latin-db-builder.nix {
  cabal = haskellPackages.cabal;
  filepath = haskellPackages.filepath;
  HDBC = haskellPackages.HDBC;
  HDBCSqlite3 = haskellPackages.HDBCSqlite3;
  hexpat = haskellPackages.hexpat;
  HUnit = haskellPackages.HUnit;
  parsec = haskellPackages.parsec;
  regexTdfa = haskellPackages.regexTdfa;
  regexTdfaText = haskellPackages.regexTdfaText;
  text = haskellPackages.text;
  textIcu = haskellPackages.textIcu;
}
