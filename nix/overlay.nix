final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = import ./haskell-overlay.nix final;
  };

  dockerImages = {
    refl-club = final.callPackage ./pkgs/refl-club-image.nix { };
  };
}
