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

  overrides = [
  ];

  #optimizations.all = true;

}
