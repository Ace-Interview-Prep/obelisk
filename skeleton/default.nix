{ system ? builtins.currentSystem }:
let jenga = import ./deps/jenga { inherit system; };
in jenga.project (import ./project.nix)
