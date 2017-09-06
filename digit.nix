{ mkDerivation, ansi-wl-pprint, base, directory, filepath, hedgehog
, lens, papa, parsers, pretty, process, semigroupoids, semigroups
, stdenv, tasty, tasty-hedgehog, tasty-hspec, template-haskell
, text, transformers
}:
mkDerivation {
  pname = "digit";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens papa parsers semigroupoids semigroups template-haskell
  ];
  testHaskellDepends = [
    ansi-wl-pprint base directory filepath hedgehog papa pretty process
    tasty tasty-hedgehog tasty-hspec text transformers
  ];
  homepage = "https://github.com/qfpl/digit";
  description = "A data-type representing digits 0-9 and other combinations";
  license = stdenv.lib.licenses.bsd3;
}
