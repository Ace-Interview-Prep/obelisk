{ pkgs, jengaLib, lib, ... }:

{

  name = "jenga-skeleton";
  src = ./.;

  inherit (jengaLib) source-repository-packages;

  jenga.static.path = import ./static { inherit pkgs; };

  shell = {
    crossPlatforms = ps: with ps; [
      wasi32
      # ghcjs
    ];
    withHoogle = true;
  };

  # Backend-only source-repository-packages.
  # These are NOT included in source-repository-packages (which gets
  # resolved for ALL platforms including WASM). Instead, we provide
  # them as a native-only override.
  overrides = [
    ({ config, pkgs, lib, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isWasm) {
      packages.jenga-backend-servant.src = jengaLib.src + "/lib/jenga-backend-servant";
    })
  ];

  #optimizations.all = true;

}
