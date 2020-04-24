all: build

build:
	@dune build @install @examples

clean:
	@dune clean

coverage: clean
	@BISECT_ENABLE=yes dune runtest
	@bisect-ppx-report send-to Coveralls

test:
	@dune runtest --force

.PHONY: all build clean coverage test
