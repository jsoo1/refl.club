{ fetchFromGitHub, mkDerivation, base, containers, hashable, lib, lucid, org-mode
, text
}:
mkDerivation {
  pname = "org-mode-lucid";
  version = "1.7.0";
  src = fetchFromGitHub {
    owner = "jsoo1";
    repo = "org-mode";
    rev = "3c74b72b2f575278aebfb9453cee5fcd2212873b";
    hash = "sha256-ige4Z4XjmonrOpa8LsCSLj/yrnwtHUDgKtV0Ay/vjkU=";
    postFetch = ''
      mv $out ./build 
      mv ./build/org-mode-lucid $out
    '';
  };
  libraryHaskellDepends = [
    base containers hashable lucid org-mode text
  ];
  homepage = "https://github.com/fosskers/org-mode";
  description = "Lucid integration for org-mode";
  license = lib.licenses.bsd3;
}
