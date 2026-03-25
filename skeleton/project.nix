{ pkgs, obeliskLib, ... }:

{

  name = "obelisk-skeleton";
  src = ./.;

  inherit (obeliskLib) source-repository-packages;

  obelisk.static.path = import ./static { inherit pkgs; };
  # For projects without a build step, use:
  # obelisk.static.path = ./static/src;

  # Multiple static directories (each with its own build pipeline):
  # obelisk.static.paths = {
  #   css     = import ./css { inherit pkgs; };       # Tailwind/PostCSS build
  #   landing = import ./landing { inherit pkgs; };    # Landing page assets
  #   media   = ./media/src;                           # Raw path, no build step
  # };
  # Access in Haskell: static @"css/styles.css", static @"landing/index.html"

  shell = {
    crossPlatforms = ps: with ps; [
      wasi32

      # To enable JS builds in `nix-shell`, uncomment ghcjs below and use:
      #   cabal build/repl/run backend -f -wasm
      #   ob-run/ob-repl -f -wasm
      # ghcjs
    ];
    withHoogle = true;
  };

  # if you're not in a hurry
  #optimizations.all = true;

}
