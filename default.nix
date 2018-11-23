{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  sources = {
    tasty-hspec = pkgs.fetchFromGitHub {
      owner = "mitchellwrosen";
      repo = "tasty-hspec";
      rev = "563d7c491d0fb5ad0c341ab0135d8787f6b34e50";
      sha256 = "0c8mzxsyymnx00bs5vr1hgya5v87as2k211vcmj17g2wigmjfqrb";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty-hspec = self.callCabal2nix "tasty-hspec" sources.tasty-hspec { };
      hedgehog       = self.callHackage "hedgehog" "0.6" {};
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
      polyparse = self.callHackage "polyparse" "1.12.1" {};
      concurrent-output = pkgs.haskell.lib.doJailbreak super.concurrent-output;
    };
  };

  drv = modifiedHaskellPackages.callPackage ./digit.nix {};

in
  drv
  # if pkgs.lib.inNixShell then drv.env else drv
