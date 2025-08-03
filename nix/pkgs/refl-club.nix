{ nix-gitignore, mkDerivation, base, bifunctors, bytestring, containers
, exceptions, feed, feed-xmlbf, file-embed, http-media, lib, lucid
, modern-uri, mtl, org-mode, org-mode-lucid, servant-feed
, servant-lucid, servant-seo, servant-server, template-haskell
, text, time, warp
, self
}:
mkDerivation {
  pname = "refl-club";
  version = "0.0.1.0";
  src = "${self}";
  preBuild = ''
    mkdir .git
    echo "${self.rev or "dirty"}" > .git/HEAD
  '';
  isLibrary = true;
  isExecutable = true;
  enableSeparateBinOutput = true;
  libraryHaskellDepends = [
    base bifunctors bytestring containers exceptions feed feed-xmlbf
    file-embed http-media lucid modern-uri mtl org-mode org-mode-lucid
    servant-feed servant-lucid servant-seo servant-server
    template-haskell text time
  ];
  executableHaskellDepends = [
    base feed-xmlbf servant-seo servant-server warp
  ];
  homepage = "https://github.com/jsoo1/refl.club#readme";
  license = lib.licenses.gpl3Only;
  mainProgram = "refl-club";
  passthru = { inherit self; };
}
