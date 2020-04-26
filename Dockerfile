from docker.pkg.github.com/jsoo1/refl.club/refl.club.build:cdab0e41596dae59f5dd73af785276b988a3fe48 as build

add . /refl.club
workdir /refl.club
run cabal new-update
run cabal v1-install
run mv $(realpath ~/.cabal/bin/refl-club) /usr/local/bin
run make static

from alpine:3.10 as run

copy --from=build /usr/local/bin/refl-club /usr/local/bin/refl-club
copy --from=build /refl.club/.static /static
run apk update && apk upgrade
run apk add --no-cache zlib
run addgroup app && adduser refl -G app -G users -D
run chown refl:app /usr/local/bin/refl-club && chown -R refl:app /static
user refl
cmd [ "refl-club", "/static" ]
