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
run apk update
run apk add --no-cache cabal
run mkdir -p /home/john/projects/refl.club
volume /home/john/projects/refl.club
add . /home/john/projects/refl.club
workdir /home/john/projects/refl.club
run mkdir -p "/home/john/.cabal/bin"
env PATH "$PATH:/home/john/.cabal/bin"
run cabal update
run cabal new-update
run cabal new-configure
run cabal new-build
