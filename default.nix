{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "05bf21fad6176eed1c9bbfbe08f1f058447a510b";
      sha256 = "0ggv9c789l473k8hxsqcp6mnncw38qkv4v5hjpfln2l479ljafxx";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;        
    };
  };

  digit = modifiedHaskellPackages.callPackage ./digit.nix {};

in

  digit

