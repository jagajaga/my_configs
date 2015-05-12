{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellngPackages.callPackage ./project.nix {}
