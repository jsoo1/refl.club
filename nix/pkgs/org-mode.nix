{ fetchFromGitHub, mkDerivation, base, containers, filepath, hashable, lib
, megaparsec, parser-combinators, tasty, tasty-hunit
, template-haskell, text, time
}:
mkDerivation {
  pname = "org-mode";
  version = "2.1.0";
  src = fetchFromGitHub {
    owner = "jsoo1";
    repo = "org-mode";
    rev = "3c74b72b2f575278aebfb9453cee5fcd2212873b";
    hash = "sha256-tRyId5VAz3lAIvPfq9N2JhsDm/BjMp/a4Y490NqZDHU=";
    postFetch = ''
      mv $out ./build 
      mv ./build/org-mode $out
    '';
  };
  libraryHaskellDepends = [
    base containers filepath hashable megaparsec parser-combinators
    template-haskell text time
  ];
  testHaskellDepends = [
    base megaparsec tasty tasty-hunit text time
  ];
  homepage = "https://github.com/fosskers/org-mode";
  description = "Parser for Emacs org-mode files";
  license = lib.licenses.bsd3;
}
