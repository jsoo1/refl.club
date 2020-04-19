from alpine:latest as build

run apk update && apk upgrade
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
run adduser builder -h /home/builder -G users -D
user builder
run mkdir -p /home/builder/refl.club
run cabal new-update
add . /home/builder/refl.club
workdir /home/builder/refl.club
run cabal new-install exe:refl-club
run mkdir out && cp $(realpath ~/.cabal/bin/refl-club) out

from alpine:latest as run
run apk update && apk upgrade
copy --from=build /home/builder/refl.club/out/refl-club /usr/local/bin
run addgroup app && adduser refl -G app -G users -D
run chown refl:app /usr/local/bin/refl-club
user refl
cmd [ "refl-club" ]
