
help:
	@echo usage
	@cat Makefile

migrate:
	cd db && ./seed.bash

reset-dev-database:
	cd db && ./reset.bash

run:
	stack run

copy-docker-binary:
	docker cp CONTAINER_ID:/freewill/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/freewill/freewill ~/freewill.docker.bin
