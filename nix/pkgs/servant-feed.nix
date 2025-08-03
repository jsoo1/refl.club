{ fetchFromGitHub, mkDerivation, base, bytestring, http-media, lib, servant, xmlbf
, xmlbf-xeno
}:
mkDerivation {
  pname = "servant-feed";
  version = "0.0.1.0";
  src = fetchFromGitHub {
    owner = "jsoo1";
    repo = "servant-feed";
    rev = "8b03cfea346087664cecf125ae0de2dfc8210f7b";
    hash = "sha256-azjSqwxrfFtwS1juiYrDz5HGdysnhAbS/PDcVLUz4FI=";
  };
  libraryHaskellDepends = [
    base bytestring http-media servant xmlbf xmlbf-xeno
  ];
  description = "Atom and RSS feeds for Servant APIs";
  license = lib.licenses.bsd3;
}
