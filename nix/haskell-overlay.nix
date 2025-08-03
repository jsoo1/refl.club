{
  haskell,
  gcrane,
  google-cloud-sdk,
  fetchFromGitHub,
  nixfmt,
  self ? null,
  mkShell,
  ...
}:
final: prev: {
  refl-club = (final.callPackage ./pkgs/refl-club.nix { }).overrideAttrs (o: {
    passthru = (o.passthru or { }) // {
      shell = mkShell {
        name = "refl.club-shell";
        inputsFrom = [ final.refl-club ];
        packages = [
          final.cabal2nix
          final.cabal-install
          final.ormolu
          gcrane
          google-cloud-sdk
          nixfmt
        ];
      };
    };
  });

  inherit self;

  feed-xmlbf = final.callPackage ./pkgs/feed-xmlbf.nix { };

  servant-feed = final.callPackage ./pkgs/servant-feed.nix { };

  xml-types-xmlbf = final.callPackage ./pkgs/xml-types-xmlbf.nix { };

  org-mode = haskell.lib.doJailbreak (final.callPackage ./pkgs/org-mode.nix { });

  org-mode-lucid = haskell.lib.doJailbreak (final.callPackage ./pkgs/org-mode-lucid.nix { });
}
