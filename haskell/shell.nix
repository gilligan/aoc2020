{ pkgs ? import ../nix }:

let
  inherit (pkgs) haskell;
  hsPkgs = haskell.packages.ghc884.extend (haskell.lib.packageSourceOverrides {
    day01 = ./day01;
  });
in
  hsPkgs.shellFor {
    packages = ps: with ps; [ day01 ];
    nativeBuildInputs = with pkgs; [ cabal-install entr ghcid ];
  }
