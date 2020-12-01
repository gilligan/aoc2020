{ pkgs ? import ../../nix }:

let
  inherit (pkgs) haskell;

  hsPkgs = pkgs.haskellPackages.extend (haskell.lib.packageSourceOverrides {
    aoc = ./.;
  });

  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${hsPkgs.ghcid.bin}/bin/ghcid --clear --command "cabal repl aoc:test:tests" --test "hspec spec" --setup "import Test.Hspec"
  '';

in
  hsPkgs.shellFor {
    packages = ps: with ps; [ aoc ];
    nativeBuildInputs = with pkgs; [ cabal-install entr ghcid haskell-language-server watch-tests ];
    shellHook = ''
      alias ghcide=haskell-language-server
    '';
  }
