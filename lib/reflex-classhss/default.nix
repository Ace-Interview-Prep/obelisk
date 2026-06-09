{ mkDerivation, base, data-default, lens, lib, template-haskell
, text
, reflex-dom-core
, pkgs
, ClasshSS ? pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.callCabal2nix "ClasshSS" ../ClasshSS-dev {})
, monad-tree ? pkgs.haskellPackages.callCabal2nix "monad-tree" ../monad-tree {}
}:
mkDerivation {
  pname = "reflex-classhss";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default lens monad-tree template-haskell text ClasshSS reflex-dom-core
  ];
  homepage = "https://github.com/augyg/ClasshSS";
  description = "Typified Tailwind for Rapid Development";
  license = lib.licenses.mit;
}
