aws = docker run --rm -it --workdir / --volume $(PWD)/out:/out --volume ~/.aws:/root/.aws mikesir87/aws-cli aws

default: out/bootstrap out/refl.club.zip

all: out/bootsrap out/refl.club.zip modify

out/refl.club.zip: #out/bootstrap required but omitted for speed
	zip -j out/refl.club.zip out/bootstrap

out/bootstrap: clean-container out
	sudo docker run \
		--name refl.club \
		-it \
		--volume $(PWD)/out:/root/.cabal/bin \
		--volume $(PWD):$(PWD) \
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



