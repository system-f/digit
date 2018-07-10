0.6

* Removed the `Digit` datatype
* Added new datatypes with approach `D*` instances
  * `BinDigit`
  * `OctDigit`
  * `DecDigit`
  * `HexDigit`
  * `HeXDigit`
  * `HeXDigit`
* Culled most of the doctests, and leaving one or two per function to illustrate
  usage
* Juggled re-exports to make the library easier to use and to improve documentation
* Added `Data.Digit.Enum`, which contains ascending-order enumerations of the new datatypes

0.5.3

* Bump version of [tasty-hedgehog, hedgehog, papa]
* Updated default.nix

0.5.2

* Add `Data.Digit.Natural (_NaturalDigits, naturalToDigits, digitsToNatural)`

0.5.1

* Replace doctest with hunit and hedgehog via tasty

0.5.0

* Data types for other bases (binary, octal, hexadecimal).
* Remove the `Digit…` data types.
* Introduce the `Digit` data type.

0.4.0

* Refactor to classy prisms for each digit.

0.3.0

* Refactor decimal type.
* `Digits` and `Digits1` data types for list of digits.

0.2.9

* Remove redundant type-class constraints.
* Add tests.
* Add `HasDigit` lens.
* Add `papa` dependency.

0.2.8

* Add `(/+/)` function.

0.2.7

* Add `mantissa` function for digits.

0.2.6

* Use `Integral` not `Int`.
* `Digits` data type (zero or many digits) and associated functions.

0.2.5

* Change parser to handle non-empty list.

0.2.4

* More Digit parsers.

0.2.3

* Digit parsers.

0.2.2

* Modulus operations.
* `digits` function.
* Fix tests.

0.2.1

* Include digit parsers.

0.2.0

* Update to use digit prisms.
