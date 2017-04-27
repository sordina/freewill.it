
help:
	@echo usage
	@cat Makefile

migrate:
	cd db && ./seed.bash

reset-dev-database:
	cd db && ./reset.bash

run:
	stack run
