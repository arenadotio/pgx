all: build

build:
	@jbuilder build @install @examples

clean:
	@rm -rf `find . -name 'bisect*.out'` _coverage
	@jbuilder clean

coverage: clean
	@BISECT_ENABLE=YES jbuilder runtest
	@bisect-ppx-report -I _build/default/ -html _coverage/ \
	  `find . -name 'bisect*.out'`

test:
	@jbuilder runtest --force

.PHONY: all build clean coverage test
