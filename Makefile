default: test

test: test.native

%.native: 
	ocamlbuild -use-ocamlfind $@
	mv $@ $*
	./$*

.PHONY: test default
