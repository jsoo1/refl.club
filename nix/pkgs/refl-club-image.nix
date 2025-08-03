{
  lib,
  dockerTools,
  haskellPackages,
  writeShellApplication,
  google-cloud-sdk,
  gcrane,
  gzip,
  tini,
}:

assert (haskellPackages.refl-club.self.rev != null);

let
  self = dockerTools.buildLayeredImage {
    name = "refl-club";
    config = {
      Entrypoint = [
        "${lib.getExe tini}"
        "--"
      ];
      Cmd = [ "${lib.getExe haskellPackages.refl-club}" ];
    };
  };
in
self.overrideAttrs (o: {
  passthru = o.passthru // {
    deploy = writeShellApplication {
      name = "deploy-refl.club";
      runtimeInputs = [
        google-cloud-sdk
        gcrane
      ];
      text = ''
        image="''${REGION}-docker.pkg.dev/''${PROJECT}/''${REPOSITORY}/${o.imageName}:${o.passthru.imageTag}"

        tardir="$(mktemp --directory --tmpdir deploy-refl.club.XXXX)"

        trap 'rm -rf $tardir' EXIT

        tar="''${tardir}/refl-club.tar"

        gunzip --stdout ${self} > "''${tar}"

        gcrane push "''${tar}" "''${image}"

        gcloud run deploy "''${APP}" --image "''${image}" --platform managed --region "''${REGION}"
      '';
    };
  };
})
