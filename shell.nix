{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  drvWithTools = pkgs.haskell.lib.addBuildDepends
    (import ./. { inherit nixpkgs compiler; })
    [ pkgs.cabal-install ];
in
  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
