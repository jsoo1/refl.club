from alpine:3.10 as build

run apk update && apk upgrade
run apk add --no-cache cabal curl gcc gmp-dev make musl-dev ncurses-dev perl zlib-dev
workdir /src
run curl -LO https://github.com/redneb/ghc-alt-libc/releases/download/ghc-8.6.5-musl/ghc-8.6.5-x86_64-unknown-linux-musl.tar.xz
workdir /src
run tar xvf ghc-8.6.5-x86_64-unknown-linux-musl.tar.xz
workdir /src/ghc-8.6.5
run ./configure
run make install
workdir /src
run rm -rf ghc*
run mkdir -p /refl.club
add . /refl.club
workdir /refl.club
run cabal new-update
run cabal new-install exe:refl-club
run mv $(realpath ~/.cabal/bin/refl-club) /usr/local/bin

from alpine:3.10 as run

copy --from=build /usr/local/bin/refl-club /usr/local/bin/refl-club
copy --from=build /refl.club/static /static
run apk update && apk upgrade
run apk add --no-cache zlib
run addgroup app && adduser refl -G app -G users -D
run chown refl:app /usr/local/bin/refl-club && chown -R refl:app /static
user refl
cmd [ "refl-club", "/static" ]
