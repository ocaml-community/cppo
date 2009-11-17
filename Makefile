VERSION = 0.9.0

ifndef OCAMLYACC
  OCAMLYACC = ocamlyacc
  #OCAMLYACC = menhir
endif
export OCAMLYACC

ifndef PREFIX
  PREFIX = /usr/local
endif
export PREFIX


.PHONY: default all opt install clean

default: opt

# ML = cppo_types.ml cppo_parser.mli cppo_parser.ml cppo.ml
ML = cppo_types.ml \
     cppo_parser.mli cppo_parser.ml \
     cppo_lexer.ml \
     cppo_eval.ml cppo_main.ml

all: $(ML)
	ocamlc -o cppo -dtypes str.cma $(ML)

opt: $(ML)
	ocamlopt -o cppo -dtypes str.cmxa $(ML)

cppo_lexer.ml: cppo_lexer.mll cppo_types.ml cppo_parser.ml
	ocamllex cppo_lexer.mll


ifeq ($(DEV),true)
cppo_parser.mli cppo_parser.ml: cppo_parser.mly cppo_types.ml
	menhir -v cppo_parser.mly
else
cppo_parser.mli cppo_parser.ml: cppo_parser.mly cppo_types.ml
	$(OCAMLYACC) cppo_parser.mly
endif

install:
	install cppo $(PREFIX)/bin

clean:
	rm -f *.cm[iox] *.o *.annot *.conflicts *.automaton \
		cppo \
		cppo_parser.mli cppo_parser.ml \
		cppo_test_parser.mli cppo_test_parser.ml \
		cppo_test_lexer.ml


SUBDIRS = testdata examples
SVNURL = svn://svn.forge.ocamlcore.org/svnroot/cppo

archive:
	@echo "Making archive for version $(VERSION)"
	if [ -n "$$(svn status -q)" ]; then \
		echo "*** There are uncommitted changes, aborting. ***" >&2; \
		exit 1; \
	fi
	rm -rf /tmp/cppo /tmp/cppo-$(VERSION) && \
	 	cd /tmp && \
		svn co "$(SVNURL)" && \
		for x in "." $(SUBDIRS); do \
			rm -rf /tmp/cppo/$$x/.svn; \
		done && \
		cd /tmp && cp -r cppo cppo-$(VERSION) && \
		tar czf cppo.tar.gz cppo && \
		tar cjf cppo.tar.bz2 cppo && \
		tar czf cppo-$(VERSION).tar.gz cppo-$(VERSION) && \
		tar cjf cppo-$(VERSION).tar.bz2 cppo-$(VERSION)
	mv /tmp/cppo.tar.gz /tmp/cppo.tar.bz2 .
	mv /tmp/cppo-$(VERSION).tar.gz /tmp/cppo-$(VERSION).tar.bz2 .
	cp cppo.tar.gz cppo.tar.bz2 $$WWW/
	cp cppo-$(VERSION).tar.gz cppo-$(VERSION).tar.bz2 $$WWW/
	cp cppo.tar.gz cppo.tar.bz2 ../releases/
	cp cppo-$(VERSION).tar.gz cppo-$(VERSION).tar.bz2 ../releases/
	cd ../releases && svn commit -m "cppo version $(VERSION)"
	cp LICENSE $$WWW/cppo-license.txt
	cp Changes $$WWW/cppo-changes.txt
	echo 'let cppo_version = "$(VERSION)"' \
		> $$WWW/cppo-version.ml
