default: out/bootstrap

out/bootstrap: out
	sudo docker run --name refl.club -it --volume $(PWD):$(PWD) refl-club-build cabal new-build
	sudo cp 
out:
	mkdir -p out

.PHONY: build-image
build-image:
	docker build -t refl-club-build:latest .
