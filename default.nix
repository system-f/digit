{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      hedgehog       = self.callHackage "hedgehog" "0.6" {};
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
      concurrent-output = pkgs.haskell.lib.doJailbreak super.concurrent-output;
      polyparse = pkgs.haskell.lib.doJailbreak super.polyparse;
    };
  };

  drv = modifiedHaskellPackages.callPackage ./digit.nix {};

in
  drv
  # if pkgs.lib.inNixShell then drv.env else drv
