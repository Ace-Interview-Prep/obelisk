{ hackGet }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in

{
  # Actually broken in current nixpkgs master due to MonadFail changes
  git = haskellLib.markUnbroken super.git;

  # hpack requires cabal >= 3.0 but the ghc865 package set builds it with 2.4 by default
  hpack = super.hpack.overrideScope (self: super: { Cabal = self.Cabal_3_2_0_0; });

  # These versions work with both the ghc865 and ghc8107 package sets
  universe = self.callHackage "universe" "1.2" {};
  universe-instances-extended = self.callHackage "universe-instances-extended" "1.1.1" {};

  regex-base = self.callHackage "regex-base" "0.94.0.0" {};
  regex-posix = self.callHackage "regex-posix" "0.96.0.0" {};
  regex-tdfa = self.callHackage "regex-tdfa" "1.3.1.0" {};
  test-framework = haskellLib.dontCheck (self.callHackage "test-framework" "0.8.2.0" {});
  hnix = haskellLib.dontCheck super.hnix;
  hnix-store-core = haskellLib.dontCheck super.hnix-store-core;
  hnix-store = haskellLib.dontCheck super.hnix-store;

  # https://github.com/haskell/hackage-security/issues/247
  hackage-security = haskellLib.dontCheck super.hackage-security; # only tests use aeson and are not compat with 1.5;
  heist = haskellLib.dontCheck (haskellLib.doJailbreak super.heist); # aeson 1.5 bump
  aeson-gadt-th = haskellLib.doJailbreak super.aeson-gadt-th; # requires aeson 1.5 for ghc8.10 support?
  deriving-compat = self.callHackage "deriving-compat" "0.6" {};
  http-api-data = haskellLib.doJailbreak super.http-api-data;
  nix-derivation = haskellLib.doJailbreak super.nix-derivation;
  algebraic-graphs = haskellLib.doJailbreak super.algebraic-graphs;
  hnix = haskellLib.overrideCabal super.hnix (drv: {
    jailbreak = true;
    preBuild = ''
      substituteInPlace src/Nix/Expr/Types.hs --replace "instance Hashable1 NonEmpty" ""
    '';
  });

  snap = haskellLib.doJailbreak super.snap;
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) {});

  logging-effect = haskellLib.doJailbreak (self.callHackage "logging-effect" "1.3.10" {});
  # unliftio-core = self.callHackage "unliftio-core" "0.2.0.1" {};
  # shelly = self.callHackage "shelly" "1.9.0" {};
  # monad-logger = self.callHackage "monad-logger" "0.3.36" {};
  hpack = self.callHackage "hpack" "0.34.1" {};
}
