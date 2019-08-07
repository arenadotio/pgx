all: build

build:
	@dune build @install @examples

clean:
	@rm -rf `find . -name 'bisect*.out'` _coverage
	@dune clean

coverage: clean
	@BISECT_ENABLE=YES dune runtest
	@bisect-ppx-report -I _build/default/ -html _coverage/ \
	  `find . -name 'bisect*.out'`

coveralls: clean
	@BISECT_ENABLE=YES dune runtest
	@bisect-ppx-report -I _build/default/ -coveralls coverage.json \
	  -service-name circle-ci -repo-token $(TOKEN) \
	  -service-job-id ${CIRCLE_BUILD_NUM} \
	  `find . -name 'bisect*.out'`
	PR_NAME= echo ${CI_PULL_REQUEST} | sed 's:.*/::'
	@sed -i '0,/{/{"service_pull_request": "$(PR_NAME)",' ./coverage.json
	@curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs

test:
	@dune runtest --force

# until we have https://github.com/ocaml/opam-publish/issues/38
REPO=../opam-repository
PACKAGES=$(REPO)/packages

pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	rm -f $(PACKAGES)/$*/$*.opam
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)

.PHONY: all build clean coverage opam-pkg test
