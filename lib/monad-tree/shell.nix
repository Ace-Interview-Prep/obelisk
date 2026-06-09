{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (hp: [
      hp.mtl
      hp.transformers
      hp.ghc-typelits-natnormalise
      hp.ghc-typelits-knownnat
    ]))
    pkgs.cabal-install
  ];
}
