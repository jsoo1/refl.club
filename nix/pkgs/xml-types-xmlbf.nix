{
  fetchFromGitHub,
  mkDerivation,
  base,
  html-entities,
  lib,
  text,
  unordered-containers,
  xml-types,
  xmlbf,
}:
mkDerivation {
  pname = "xml-types-xmlbf";
  version = "0.0.1.0";
  src = fetchFromGitHub {
    owner = "jsoo1";
    repo = "xml-types-xmlbf";
    rev = "6b23e7ea880d542549802aea8dfb841ebd12f684";
    hash = "sha256-iMiz7eF2UZyJnNl70kC1yeK6rsbemKKCiXG8gpwEYMw=";
  };
  libraryHaskellDepends = [
    base
    html-entities
    text
    unordered-containers
    xml-types
    xmlbf
  ];
  description = "Orphan xmlbf To and From Xml instances for the xml-types library types";
  license = lib.licenses.bsd3;
}
