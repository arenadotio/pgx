PACKAGES := $(shell ls -1 *.opam | xargs echo | sed 's/.opam//g' | sed 's/ /,/g')

all: build

build:
	@dune build @install @examples -p $(PACKAGES)

clean:
	@dune clean

coverage: clean
	@BISECT_ENABLE=yes dune runtest -p $(PACKAGES)
	@bisect-ppx-report send-to Coveralls

test:
	@dune runtest --force -p $(PACKAGES)

.PHONY: all build clean coverage test
