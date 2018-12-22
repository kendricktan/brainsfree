{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:

let
  # Can't use overlays as there is an infinite
  # recursion in the list of dependencies that needs
  # to be fixed first
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};

  brainsfree = haskellPackages.callPackage ./brainsfree.nix {};

in
  brainsfree
