{
  description = "Personal website.";
  outputs = { nixpkgs, self }:
    let
      inherit (nixpkgs) lib;
      fold = l: f: lib.foldl' (x: y: lib.recursiveUpdate x (f y)) { } l;
      overlays.default = import ./nix/overlay.nix;
    in
      { inherit overlays; } // fold lib.platforms.all (localSystem: {
        packages.${localSystem} = import "${nixpkgs}/pkgs/top-level" {
          inherit localSystem;
          overlays = [
            overlays.default
            (_: _: {
              inherit self;
              default = self.packages.${localSystem}.haskellPackages.refl-club;
            })
          ];
        };

        apps.${localSystem}.deploy = {
          type = "app";
          program = lib.getExe self.packages.${localSystem}.dockerImages.refl-club.deploy;
        };

        devShells.${localSystem}.default =
          self.packages.${localSystem}.haskellPackages.refl-club.shell;
      });
}
