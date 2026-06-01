{ pkgs, jengaLib, ... }:

{

  name = "jenga-skeleton";
  src = ./.;

  inherit (jengaLib) source-repository-packages;

  jenga.static.path = import ./static { inherit pkgs; };

  shell = {
    crossPlatforms = ps: with ps; [
      # wasi32  # disabled: servant/generics-sop TH fails on GHC WASM backend
      # ghcjs   # uncomment for JS builds
    ];
    withHoogle = true;
  };

  # Mobile & Web targets — uncomment when module options are declared:
  # android = { ... };
  # ios = { ... };

  #optimizations.all = true;

}
