from jsoo1/refl.club:7afa8a80a3ccbfb294d271684a8e5497d09f4f0a as build

run rm -rf /refl.club/*
add . /refl.club
workdir /refl.club
run cabal v1-install
run mv $(realpath ~/.cabal/bin/refl-club) /usr/local/bin
run make static

from alpine:3.10 as run

copy --from=build /usr/local/bin/refl-club /usr/local/bin/refl-club
copy --from=build /refl.club/.static /static
run apk update && apk upgrade
run addgroup app && adduser refl -G app -G users -D
run chown refl:app /usr/local/bin/refl-club && chown -R refl:app /static
user refl
cmd [ "refl-club", "/static" ]
