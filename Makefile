
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
	./scripts/docker_build_and_upload.sh
