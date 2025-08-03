final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = import ./haskell-overlay.nix final;
  };

  dockerImages = {
    refl-club =
      assert (final.haskellPackages.refl-club.self.rev != null);
      final.callPackage ./pkgs/refl-club-image.nix { };
  };
}
