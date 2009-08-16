VERSION = 0.5

ifndef PREFIX
  PREFIX = /usr/local
endif
export PREFIX


.PHONY: default all opt install clean

default: opt

# ML = cppo_types.ml cppo_parser.mli cppo_parser.ml cppo.ml
ML = cppo_types.ml cppo_test_parser.mli cppo_test_parser.ml \
     cppo_test_lexer.ml cppo_eval.ml

all: $(ML)
	ocamlc -o cppo -dtypes str.cma $(ML)

opt: $(ML)
	ocamlopt -o cppo -dtypes str.cmxa $(ML)

cppo_test_parser.ml: cppo_test_parser.mly cppo_types.ml
	ocamlyacc cppo_test_parser.mly

cppo_test_lexer.ml: cppo_test_lexer.mll cppo_types.ml cppo_test_parser.ml
	ocamllex cppo_test_lexer.mll

#cppo.ml: cppo.mll cppo_types.ml
#	ocamllex cppo.mll

#cppo_parser.ml: cppo_parser.mly cppo_types.ml
#	ocamlyacc cppo_parser.mly

install:
	install cppo $(PREFIX)/bin

clean:
	rm -f *.cm[iox] *.annot cppo_parser.mli cppo_parser.ml cppo.ml
