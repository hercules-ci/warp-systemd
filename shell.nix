{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.stack

    # releaser
    pkgs.cabal-install
    pkgs.ghc
    pkgs.haskellPackages.releaser
  ];
  buildInputs = [
    pkgs.zlib
  ];
}
