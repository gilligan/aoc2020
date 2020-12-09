{ pkgs ? import ../../nix }:
let
  inherit (pkgs) haskell;

  hsPkgs = pkgs.haskellPackages.extend (haskell.lib.packageSourceOverrides {
    aoc = ./.;
  });

  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${hsPkgs.ghcid.bin}/bin/ghcid --clear \
    --command "cabal repl aoc:test:tests" \
    --test "hspec spec" \
    --setup "import Test.Hspec" \
    --restart=./src --restart=./test
  '';

in
hsPkgs.shellFor {
  withHoogle = true;
  packages = ps: with ps; [ aoc ];
  nativeBuildInputs = with pkgs; [ cabal-install entr ghcid haskell-language-server watch-tests ormolu nixpkgs-fmt hlint ];
}
