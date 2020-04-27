GIT_HASH = $(shell git rev-parse HEAD)
STATIC = grammars john-soo-resume.pdf refl.css
STATIC_SRC = $(foreach file,$(STATIC),static/$(file))
STATIC_TARGET = $(foreach file,$(STATIC),.static/$(GIT_HASH)-$(file))
aws = docker run --rm -it --workdir / --volume $(PWD)/out:/out --volume ~/.aws:/root/.aws mikesir87/aws-cli aws

default: out/bootstrap clean-zip out/refl.club.zip

all: default modify

out/refl.club.zip: #out/bootstrap required but omitted for speed
	zip -j out/refl.club.zip out/bootstrap

.PHONY: static
static: $(STATIC_TARGET)

$(STATIC_TARGET): $(STATIC_SRC) | .static
	for f in $?; do cp -r "$$f" ".static/$(GIT_HASH)-$$(basename $$f)"; done

.static:
	mkdir -p $@

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
