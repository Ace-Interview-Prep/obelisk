{ system ? builtins.currentSystem }:
let
  reflex-platform = import ../reflex-platform { inherit system; };
  pkgs = reflex-platform.nixpkgs;
  ghc = reflex-platform.ghc;
in pkgs.mkShell {
  buildInputs = [
    (ghc.ghcWithPackages (ps: [
      ps.reflex
      ps.reflex-dom-core
      ps.jsaddle
      ps.jsaddle-warp
      ps.data-default
      ps.dependent-map
      ps.dependent-sum
      ps.ghcjs-dom
      ps.lens
      ps.patch
      ps.warp
      ps.websockets
    ]))
    pkgs.cabal-install
    pkgs.pkg-config
    pkgs.zlib
  ];
}
