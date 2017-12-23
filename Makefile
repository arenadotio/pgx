all: build

build:
	@jbuilder build @install

clean:
	@jbuilder clean

test:
	@jbuilder runtest

.PHONY: all build clean test
