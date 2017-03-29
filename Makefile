
help:
	@echo usage
	@cat Makefile

migrate:
	cd db && ./seed.bash

run:
	stack run
