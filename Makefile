all: build

build:
	@dune build @install @examples

clean:
	@dune clean

coverage: clean
	@BISECT_ENABLE=YES dune runtest
	@bisect-ppx-report coveralls coverage.json
	@curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs

test:
	@dune runtest --force

.PHONY: all build clean coverage test
