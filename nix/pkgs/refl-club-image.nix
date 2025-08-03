{ dockerTools, haskellPackages }:

dockerTools.buildLayeredImage {
  name = "refl-club";
  contents = [ haskellPackages.refl-club.bin ];
  config.Entrypoint = [ "/bin/refl-club" ];
}
