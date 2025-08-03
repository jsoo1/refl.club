final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = import ./haskell-overlay.nix final;
  };
}
