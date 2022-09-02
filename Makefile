.PHONY: all build build-deps fmt fmt-check install dev-deps test
.PHONY: clean distclean

DEV_DEPS := merlin ocamlformat odoc ppx_expect ppx_inline_test

ODOC_TARGET:=_build/docs/.


all: build

build:
	opam exec -- dune build @install
	./scripts/copy-bin.sh binary_encoding

build-deps:
	if ! [ -e _opam ]; then \
	   opam switch create . 4.10.0 ; \
	fi
	opam install ./*.opam --deps-only


.PHONY: doc-common odoc view
doc-common: build
	mkdir -p _build/docs
	mkdir -p doc

odoc: doc-common
	mkdir -p ${ODOC_TARGET}
	opam exec -- dune build @doc
	rsync -auv --delete _build/default/_doc/_html/. ${ODOC_TARGET}

doc: doc-common odoc
	cp -rf _build/docs/. doc/.

view:
	xdg-open file://$$(pwd)/_build/docs/index.html

fmt:
	opam exec -- dune build @fmt --auto-promote

fmt-check:
	opam exec -- dune build @fmt

install:
	opam pin -y --no-action -k path .
	opam install -y .

opam:
	opam pin -k path .

uninstall:
	opam uninstall .

dev-deps:
	opam install ./*.opam --deps-only --with-doc --with-test

test:
	opam exec -- dune build @runtest

clean:
	rm -rf _build

distclean: clean
	rm -rf _opam


