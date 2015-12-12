{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./git-prompt.nix {}
