#    This file is part of refl.club - a personal website and blog
#    Copyright (C) 2020 John Soo <jsoo1@asu.edu>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

from jsoo1/refl.club:618ff18ad3e3118e38e46737cfecadd9d732bb5f as build

add . /refl.club
workdir /refl.club
run cabal v2-install exe:refl-club --overwrite-policy=always
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
