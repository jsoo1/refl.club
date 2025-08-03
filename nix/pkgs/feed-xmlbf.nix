{
  fetchFromGitHub,
  mkDerivation,
  base,
  feed,
  lib,
  text,
  unordered-containers,
  xml-types,
  xml-types-xmlbf,
  xmlbf,
}:
mkDerivation {
  pname = "feed-xmlbf";
  version = "0.0.1.1";
  src = fetchFromGitHub {
    owner = "jsoo1";
    repo = "feed-xmlbf";
    rev = "485ba53dbf8d203248b2eafbfc85fb5bca922399";
    hash = "sha256-dPFQUKN3oqo8ThGdZ803KyHj4KO88As9ouFYfk1rcyM=";
  };
  libraryHaskellDepends = [
    base
    feed
    text
    unordered-containers
    xml-types
    xml-types-xmlbf
    xmlbf
  ];
  description = "xmlbf To and From Xml instances for feed";
  license = lib.licenses.bsd3;
}
