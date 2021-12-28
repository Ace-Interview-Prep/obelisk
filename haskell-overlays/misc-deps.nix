{ hackGet }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in

{
  regex-base = self.callHackage "regex-base" "0.94.0.0" {};
  regex-posix = self.callHackage "regex-posix" "0.96.0.0" {};
  regex-tdfa = self.callHackage "regex-tdfa" "1.3.1.0" {};
  test-framework = haskellLib.dontCheck (self.callHackage "test-framework" "0.8.2.0" {});
  hnix = haskellLib.dontCheck super.hnix;
  hnix-store-core = haskellLib.dontCheck super.hnix-store-core;
  hnix-store = haskellLib.dontCheck super.hnix-store;

  aeson-gadt-th = self.callHackage "aeson-gadt-th" "0.2.4" {};

  ghcid = self.callCabal2nix "ghcid" (hackGet ../dep/ghcid) {};
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) {});

  logging-effect = self.callCabal2nix "logging-effect" (hackGet ../dep/logging-effect) {};
  resourcet = self.callHackage "resourcet" "1.2.4.2" {};
  unliftio-core = self.callHackage "unliftio-core" "0.2.0.1" {};
  shelly = self.callHackage "shelly" "1.9.0" {};
  monad-logger = self.callHackage "monad-logger" "0.3.36" {};
  git = self.callHackage "git" "0.3.0" {};
  hpack  self.callHackage "hpack" "0.34.1" {};
}
