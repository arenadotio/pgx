all: build

build:
	@jbuilder build @install

clean:
	@rm -f `find . -name 'bisect*.out'`
	@jbuilder clean

coverage: clean
	@BISECT_ENABLE=YES jbuilder runtest
	@bisect-ppx-report -I _build/default/ -html _coverage/ \
	  `find . -name 'bisect*.out'`

test:
	@jbuilder runtest

.PHONY: all build clean coverage test
