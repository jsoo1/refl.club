{ dockerTools, haskellPackages }:

dockerTools.streamLayeredImage {
  name = "refl-club";
  contents = [ haskellPackages.refl-club.out ];
}
