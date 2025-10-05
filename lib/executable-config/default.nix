# This module exposes a tool for several platforms that can be used to inject
# configuration information into a canonical location. It also provides a haskell
# package that can be used to retrieve the injected configuration on each supported
# platform.
{ lib
, runCommand
, obeliskCleanSource
}:

let
  setup = ''
    set -x
    mkdir -p $out
    mkdir -p $out/static
  '';
  createConfig = config: lib.optionalString (!(builtins.isNull config)) ''
    if ! mkdir $out/config; then
      2>&1 echo config directory already exists or could not be created
      exit 1
    fi
    cp -a "${config}"/* "$out/config"
    # Needed for android deployments
    find "$out/config" -type f -printf '%P\0' > "$out/config.files"
  '';
  # Walk recursively, look for hashed files, remove the plain originals
  removeNonHashed = ''
    assets_root="$out/static/"
    find "$assets_root" -type f -regextype posix-extended -regex '.*/[A-Za-z0-9]{16,}-[^/]+$' | while read -r hashed; do
         dir=$(dirname "$hashed")
         base=$(basename "$hashed")
         orig="''${base#*-}"
         orig_path="$dir/$orig"
         if [ -f "$orig_path" ]; then
           echo "Removing duplicate original: $orig_path"
             rm -f -- "$orig_path"
         fi
    done
  '';
  mkStaticDir = name: assets: ''
    cp --no-preserve=mode -Lr "${assets}" $out/static/${name} 
  '';
  #mkStaticDirs =
  injectMany = config: processedStatic:
    let staticDirs = lib.mapAttrsToList (name: assetDrv: mkStaticDir name assetDrv.symlinked) processedStatic;
    in
      runCommand "inject-config" {} (
        ''''
        + setup
        + (lib.concatStrings staticDirs)
        + removeNonHashed
        + createConfig config
        + ''''
      );


    #lib.mapAttrs (name: assetDrv: mkStaticDir ) processedStatic

      #map (injectConfig config) assetsMany
  injectConfig = config: assets: runCommand "inject-config" {} (''
    set -x
    mkdir -p $out
    mkdir -p $out/static
    cp --no-preserve=mode -Lr "${assets}" $out/static/staticAssets
    chmod +w "$out"

    assets_root="$out/static/staticAssets"
    # Walk recursively, look for hashed files, remove the plain originals
    find "$assets_root" -type f -regextype posix-extended -regex '.*/[A-Za-z0-9]{16,}-[^/]+$' | while read -r hashed; do
         dir=$(dirname "$hashed")
         base=$(basename "$hashed")
         orig="''${base#*-}"
         orig_path="$dir/$orig"
         if [ -f "$orig_path" ]; then
           echo "Removing duplicate original: $orig_path"
             rm -f -- "$orig_path"
         fi
    done

  '' + lib.optionalString (!(builtins.isNull config)) ''
    if ! mkdir $out/config; then
      2>&1 echo config directory already exists or could not be created
      exit 1
    fi
    cp -a "${config}"/* "$out/config"
    # Needed for android deployments
    find "$out/config" -type f -printf '%P\0' > "$out/config.files"
  '');
in

{
  haskellOverlay = self: super:
    let
      pkgs = self.callPackage ({pkgs}: pkgs) {};
    in {
      obelisk-executable-config-lookup = pkgs.haskell.lib.overrideCabal
        (self.callCabal2nix "obelisk-executable-config-lookup" (obeliskCleanSource ./lookup) {})
        (drv: {
          # Hack until https://github.com/NixOS/cabal2nix/pull/432 lands
          libraryHaskellDepends = (drv.libraryHaskellDepends or [])
            ++ pkgs.lib.optionals (with pkgs.stdenv.hostPlatform; isAndroid && is32bit) [
              self.android-activity
            ];
        });
  };

  platforms = {
    android = {
      # Inject the given config directory into an android assets folder
      injectMany = injectMany;
      inject = injectConfig;
    };
    ios = {
      # Inject the given config directory into an iOS app
      injectMany = injectMany;
      inject = injectConfig;
    };
    web = {
      inject = self: self.callCabal2nix "obelisk-executable-config-inject" (obeliskCleanSource ./inject) {};
    };
  };
}
