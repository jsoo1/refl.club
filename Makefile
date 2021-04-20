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

aws = docker run --rm -it --workdir / --volume $(PWD)/out:/out --volume ~/.aws:/root/.aws mikesir87/aws-cli aws

default: out/bootstrap clean-zip out/refl.club.zip

all: default modify

out/refl.club.zip: #out/bootstrap required but omitted for speed
	zip -j out/refl.club.zip out/bootstrap

.PHONY: clean-zip
clean-zip:
	rm out/refl.club.zip

out/bootstrap: clean-container out
	docker run \
		--name refl.club \
		-it \
		--volume $(PWD)/out:/root/.cabal/bin \
		--volume $(PWD):$(PWD) \
		--workdir $(PWD) \
		refl-club-build \
		cabal v1-install
	sudo chmod 755 out/bootstrap

out:
	mkdir -p out

.PHONY: clean-container
clean-container:
	docker rm refl.club || true

.PHONY: clean-docker-artifacts
clean-docker-artifacts:
	rm -rf dist-newstyle/build

.PHONY: build-image
build-image:
	docker build -t refl-club-build:latest .

.PHONY: create
create: #out/refl.club.zip required but omitted for speed
	$(aws) lambda create-function \
		--function-name refl-club \
		--zip-file fileb://out/refl.club.zip \
		--handler Index.handler \
		--runtime provided \
		--role $(shell cat ~/.aws/arn)

.PHONY: modify
modify:
	$(aws) lambda update-function-code \
		--function-name refl-club \
		--zip-file fileb://out/refl.club.zip

.PHONY: delete
delete:
	$(aws) lambda delete-function --function-name refl-club

.PHONY: test
test:
	$(aws) lambda invoke --function-name refl-club out/response.txt
