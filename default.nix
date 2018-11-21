{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./digit.nix {};

in
  drv
  # if pkgs.lib.inNixShell then drv.env else drv
