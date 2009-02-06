VERSION = 0.5

ifndef PREFIX
  PREFIX := /usr/local
endif
export PREFIX


.PHONY: default all opt install clean

default: opt

all: cppo.ml
	ocamlc -o cppo cppo.ml

opt: cppo.ml
	ocamlopt -o cppo cppo.ml

cppo.ml: cppo.mll
	ocamllex cppo.mll

install:
	install cppo $(PREFIX)/bin

clean:
	rm -f *.cm[iox] cppo.ml
