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

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif
export BINDIR

.PHONY: default all opt install clean

default: opt

# ML = cppo_types.ml cppo_parser.mli cppo_parser.ml cppo.ml
ML = cppo_version.ml cppo_types.ml \
     cppo_parser.mli cppo_parser.ml \
     cppo_lexer.ml \
     cppo_eval.ml cppo_main.ml

all: $(ML)
	ocamlc -o cppo -dtypes $(ML)

opt: $(ML)
	ocamlopt -o cppo -dtypes $(ML)

install:
	install -m 0755 cppo $(BINDIR) || \
		install -m 0755 cppo.exe $(BINDIR)

cppo_version.ml: Makefile
	echo 'let cppo_version = "$(VERSION)"' > cppo_version.ml


cppo_lexer.ml: cppo_lexer.mll cppo_types.ml cppo_parser.ml
	ocamllex cppo_lexer.mll


ifeq ($(DEV),true)
cppo_parser.mli cppo_parser.ml: cppo_parser.mly cppo_types.ml
	menhir -v cppo_parser.mly
else
cppo_parser.mli cppo_parser.ml: cppo_parser.mly cppo_types.ml
	$(OCAMLYACC) cppo_parser.mly
endif

clean:
	rm -f *.cm[iox] *.o *.annot *.conflicts *.automaton \
		cppo \
		cppo_parser.mli cppo_parser.ml cppo_version.ml

SUBDIRS = testdata examples
SVNURL = svn://svn.forge.ocamlcore.org/svnroot/cppo

archive:
	@echo "Making archive for version $(VERSION)"
	@if [ -z "$$WWW" ]; then \
		echo '*** Environment variable WWW is undefined ***' >&2; \
		exit 1; \
	fi
	@if [ -n "$$(svn status -q)" ]; then \
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
	mv /tmp/cppo.tar.gz /tmp/cppo.tar.bz2 ../releases
	mv /tmp/cppo-$(VERSION).tar.gz /tmp/cppo-$(VERSION).tar.bz2 ../releases
	cp ../releases/cppo.tar.gz $$WWW/
	cp ../releases/cppo.tar.bz2 $$WWW/
	cp ../releases/cppo-$(VERSION).tar.gz $$WWW/
	cp ../releases/cppo-$(VERSION).tar.bz2 $$WWW/
	cd ../releases && svn commit -m "cppo version $(VERSION)"
	cp LICENSE $$WWW/cppo-license.txt
	cp Changes $$WWW/cppo-changes.txt
	echo 'let cppo_version = "$(VERSION)"' \
		> $$WWW/cppo-version.ml
