{ hackGet }:
self: super:
let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;

  rhyoliteSrc = hackGet ../dep/rhyolite;
  # thunkSet + hackGet breaks when called on store-path strings because
  # hackGet's `import (p + /thunk.nix)` tries to realize the path literal
  # /thunk.nix. Resolve rhyolite's dep thunks directly via fetchTarball.
  fetchRhyoliteDep = name:
    let json = builtins.fromJSON (builtins.readFile (rhyoliteSrc + "/dep/${name}/github.json"));
    in builtins.fetchTarball {
      url = "https://github.com/${json.owner}/${json.repo}/archive/${json.rev}.tar.gz";
      inherit (json) sha256;
    };
  rhyoliteRepos = builtins.mapAttrs (name: _: fetchRhyoliteDep name)
    (pkgs.lib.filterAttrs (_: type: type == "directory") (builtins.readDir (rhyoliteSrc + "/dep")));
  jengaAuthSrc = hackGet ../lib/jenga-auth;
  obeliskOauthSrc = hackGet ../dep/obelisk-oauth;
in
{
  # ── Rhyolite packages ──────────────────────────────────────
  rhyolite-backend = self.callCabal2nix "rhyolite-backend" (rhyoliteSrc + "/backend") {};
  rhyolite-beam-db = self.callCabal2nix "rhyolite-beam-db" (rhyoliteSrc + "/beam/db") {};
  rhyolite-beam-orphans = self.callCabal2nix "rhyolite-beam-orphans" (rhyoliteSrc + "/beam/orphans") {};
  rhyolite-beam-task-worker-types = self.callCabal2nix "rhyolite-beam-task-worker-types" (rhyoliteSrc + "/beam/task/types") {};
  rhyolite-beam-task-worker-backend =
    haskellLib.dontCheck (self.callCabal2nix "rhyolite-beam-task-worker-backend" (rhyoliteSrc + "/beam/task/backend") {});
  rhyolite-notify-listen = self.callCabal2nix "rhyolite-notify-listen" (rhyoliteSrc + "/notify-listen/notify-listen") {};
  rhyolite-notify-listen-beam = self.callCabal2nix "rhyolite-notify-listen-beam" (rhyoliteSrc + "/notify-listen/notify-listen-beam") {};
  psql-simple-class = self.callCabal2nix "psql-simple-class" (rhyoliteSrc + "/psql-extras/psql-simple-class") {};
  psql-simple-beam = self.callCabal2nix "psql-simple-beam" (rhyoliteSrc + "/psql-extras/psql-simple-beam") {};
  psql-serializable = self.callCabal2nix "psql-serializable" (rhyoliteSrc + "/psql-extras/psql-serializable") {};
  rhyolite-common = self.callCabal2nix "rhyolite-common" (rhyoliteSrc + "/common") {};
  rhyolite-email = self.callCabal2nix "rhyolite-email" (rhyoliteSrc + "/email") {};
  mime-mail-orphans = self.callCabal2nix "mime-mail-orphans" (rhyoliteSrc + "/email/mime-mail-orphans") {};
  semimap = self.callCabal2nix "semimap" (rhyoliteSrc + "/semimap") {};
  rhyolite-frontend = self.callCabal2nix "rhyolite-frontend" (rhyoliteSrc + "/frontend") {};
  signed-data = self.callCabal2nix "signed-data" (rhyoliteSrc + "/signed-data/signed-data") {};
  signed-data-clientsession = self.callCabal2nix "signed-data-clientsession" (rhyoliteSrc + "/signed-data/signed-data-clientsession") {};
  rhyolite-widgets = self.callCabal2nix "rhyolite-widgets" (rhyoliteSrc + "/widgets") {};
  rhyolite-account-backend = self.callCabal2nix "rhyolite-account-backend" (rhyoliteSrc + "/account/backend") {};
  rhyolite-account-types = self.callCabal2nix "rhyolite-account-types" (rhyoliteSrc + "/account/types") {};

  # ── Rhyolite external deps ─────────────────────────────────
  bytestring-aeson-orphans = self.callCabal2nix "bytestring-aeson-orphans" rhyoliteRepos.bytestring-aeson-orphans {};
  monoid-map = haskellLib.doJailbreak (self.callCabal2nix "monoid-map" rhyoliteRepos.monoid-map {});
  postgresql-simple-interpolate = self.callCabal2nix "postgresql-simple-interpolate" rhyoliteRepos.postgresql-simple-interpolate {};
  gargoyle = self.callCabal2nix "gargoyle" (rhyoliteRepos.gargoyle + "/gargoyle") {};
  gargoyle-postgresql = self.callCabal2nix "gargoyle-postgresql" (rhyoliteRepos.gargoyle + "/gargoyle-postgresql") {};
  gargoyle-postgresql-connect = self.callCabal2nix "gargoyle-postgresql-connect" (rhyoliteRepos.gargoyle + "/gargoyle-postgresql-connect") {};
  gargoyle-postgresql-nix = haskellLib.overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix" (rhyoliteRepos.gargoyle + "/gargoyle-postgresql-nix") {})
    { librarySystemDepends = [ pkgs.postgresql ]; };
  push-notifications = self.callCabal2nix "push-notifications" rhyoliteRepos.push-notifications {};
  postgresql-lo-stream = haskellLib.markUnbroken (self.callCabal2nix "postgresql-lo-stream" rhyoliteRepos.postgresql-lo-stream {});
  beam-automigrate = self.callCabal2nix "beam-automigrate" rhyoliteRepos.beam-automigrate {};
  beam-migrate = haskellLib.doJailbreak (self.callCabal2nix "beam-migrate" (rhyoliteRepos.beam + "/beam-migrate") {});
  beam-postgres = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "beam-postgres" (rhyoliteRepos.beam + "/beam-postgres") {}));
  beam-core = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "beam-core" (rhyoliteRepos.beam + "/beam-core") {}));
  vessel = haskellLib.doJailbreak (self.callCabal2nix "vessel" (hackGet ../dep/vessel) {});

  # ── Rhyolite hackage pins ──────────────────────────────────
  HaskellNet-SSL = self.callHackage "HaskellNet-SSL" "0.3.4.4" {};
  base-orphans = self.callHackageDirect {
    pkg = "base-orphans";
    ver = "0.8.6";
    sha256 = "sha256:17hplm1mgw65jbszg5z4vqk4i24ilxv8mbszr3s8lhpll5naik26";
  } {};
  aeson-qq = self.callHackage "aeson-qq" "0.8.4" {};
  validation = haskellLib.dontCheck super.validation;
  postgresql-syntax = haskellLib.dontCheck super.postgresql-syntax;

  # ── Jenga auth (lib/) ──────────────────────────────────────
  jenga-auth-frontend = self.callCabal2nix "jenga-auth-frontend" (jengaAuthSrc + "/jenga-auth-frontend") {};
  jenga-auth-common = self.callCabal2nix "jenga-auth-common" (jengaAuthSrc + "/jenga-auth-common") {};
  jenga-auth-backend = self.callCabal2nix "jenga-auth-backend" (jengaAuthSrc + "/jenga-auth-backend") {};

  # ── Obelisk OAuth (dep/) ───────────────────────────────────
  obelisk-oauth-backend = self.callCabal2nix "obelisk-oauth-backend" (obeliskOauthSrc + "/backend") {};
  obelisk-oauth-common = self.callCabal2nix "obelisk-oauth-common" (obeliskOauthSrc + "/common") {};

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
  snap-extras = haskellLib.doJailbreak super.snap-extras;
  parseargs = haskellLib.dontCheck super.parseargs;
}
