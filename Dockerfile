from debian:stable

run apt-get update && apt-get install -y \
        cabal-install \
        ghc \
        zlib1g-dev
run cabal update
volume /src
workdir /src
add . /src
run cabal new-update
run cabal new-configure
run cabal new-build
