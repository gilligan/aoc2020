{ pkgs ? import ../nix }:

pkgs.mkShell {
  buildInputs = with pkgs; [ rustc cargo rls ];
}
