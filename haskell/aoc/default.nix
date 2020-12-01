{ pkgs ? import ../../nix
, compiler ? "ghc8101"
}:

let
  hsPkgs = pkgs.haskell.packages."${compiler}";
  drv = hsPkgs.callCabal2nix "path-hs" (pkgs.lib.cleanSource ./.) {};
  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${hsPkgs.ghcid.bin}/bin/ghcid --clear --command "cabal repl path-hs:test:tests" --test "hspec spec" --setup "import Test.Hspec"
  '';
in
  {
    path-hs = drv;
    shell = hsPkgs.shellFor {
      withHoogle = false;
      packages = ps: [ps.cabal-install ps.ghcide ps.hspec-discover];
      buildInputs = with pkgs; [
        watch-tests
      ];
    };
  }

