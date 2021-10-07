{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "wasted-time" ./. {}
