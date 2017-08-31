{ mkDerivation, base, directory, doctest, filepath, lens, papa
, parsec, parsers, QuickCheck, semigroupoids, semigroups, stdenv
, template-haskell
}:
mkDerivation {
  pname = "digit";
  version = "0.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens papa parsec parsers semigroupoids semigroups
    template-haskell
  ];
  testHaskellDepends = [
    base directory doctest filepath lens parsec QuickCheck
    template-haskell
  ];
  homepage = "https://github.com/qfpl/digit";
  description = "A data-type representing digits 0-9 and other combinations";
  license = stdenv.lib.licenses.bsd3;
}
