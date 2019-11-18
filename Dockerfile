from debian:stable

run apt-get update && apt-get install -y \
        cabal-install \
        ghc
