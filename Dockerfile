from alpine:latest as build

run apk update
run apk add --no-cache curl musl-dev gmp-dev zlib-dev ncurses-dev perl make gcc
workdir /src
run curl -LO https://github.com/redneb/ghc-alt-libc/releases/download/ghc-8.6.5-musl/ghc-8.6.5-x86_64-unknown-linux-musl.tar.xz
workdir /src
run tar xvf ghc-8.6.5-x86_64-unknown-linux-musl.tar.xz
workdir /src/ghc-8.6.5
run ./configure
run make install
workdir /src
run rm -rf ghc*
volume /src/refl.club
add . /src/refl.club 
workdir /src/refl.club
run apk update
run apk add --no-cache cabal
run cabal new-update
run cabal new-configure
run cabal new-build
