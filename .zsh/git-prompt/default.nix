{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellngPackages.callPackage ./git-prompt.nix {}
