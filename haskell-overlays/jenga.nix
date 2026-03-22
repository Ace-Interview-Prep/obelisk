{ hackGet }:
self: super:
let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in
{
  # ── Jenga core libraries (lib/) ────────────────────────────
  lamarckian = self.callCabal2nix "lamarckian" (hackGet ../lib/lamarckian) {};
  lamarckian-core = self.callCabal2nix "lamarckian-core" (hackGet ../lib/lamarckian-core) {};

  # ── Scrappy ecosystem (dep/) ───────────────────────────────
  scrappy-core = super.callPackage (hackGet ../dep/scrappy-core) {};
  scrappy-template = super.callPackage (hackGet ../dep/scrappy-template) {};
  scrappy-requests = self.callCabal2nix "scrappy-requests" (hackGet ../dep/scrappy-requests) {};

  # ── UI framework (dep/) ────────────────────────────────────
  ClasshSS = super.callPackage (hackGet ../dep/ClasshSS) {};
  reflex-classhss = super.callPackage (hackGet ../dep/reflex-classh) {};
  reflex-dom-echarts = self.callCabal2nix "reflex-dom-echarts" (hackGet ../dep/reflex-dom-echarts) {};
  echarts-jsdom = self.callCabal2nix "echarts-jsdom" (hackGet ../dep/echarts-jsdom) {};

  # ── Utility libraries (dep/) ───────────────────────────────
  IStr = super.callPackage (hackGet ../dep/IStr) {};
  mmark-ext = self.callCabal2nix "mmark-ext" (hackGet ../dep/mmark-ext) {};
  templates = self.callCabal2nix "templates" (hackGet ../dep/templates) {};
  network-uri = super.callCabal2nix "network-uri" (hackGet ../dep/network-uri) {};
  vessel = self.callCabal2nix "vessel" (hackGet ../dep/vessel) {};

  # ── Hackage version pins ───────────────────────────────────
  henforcer = self.callHackage "henforcer" "1.0.0.1" {};
  text-metrics = self.callHackage "text-metrics" "0.3.0" {};
  skylighting = haskellLib.dontHaddock (self.callHackage "skylighting" "0.10.2" {});
  skylighting-core = haskellLib.dontHaddock (self.callHackage "skylighting-core" "0.10.2" {});
  ghc-syntax-highlighter = haskellLib.dontHaddock (self.callHackage "ghc-syntax-highlighter" "0.0.6.0" {});
  ghc-lib-parser = haskellLib.dontHaddock super.ghc-lib-parser;

  # ── Compatibility fixes ────────────────────────────────────
  stripe-core = haskellLib.doJailbreak super.stripe-core;
  stripe-tests = haskellLib.doJailbreak super.stripe-tests;
  stripe-http-client = haskellLib.doJailbreak super.stripe-http-client;
  bytestring-aeson-orphans = haskellLib.doJailbreak super.bytestring-aeson-orphans;
  snap-extras = haskellLib.doJailbreak super.snap-extras;
  parseargs = haskellLib.dontCheck super.parseargs;
  beam-automigrate = haskellLib.doHaddock super.beam-automigrate;
}
