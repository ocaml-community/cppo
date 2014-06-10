VERSION = 0.9.3

ifeq "$(shell ocamlc -config |grep os_type)" "os_type: Win32"
EXE=.exe
else
EXE=
endif

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



BEST := opt
NATDYNLINK ?= $(shell if [ -f `ocamlc -where`/dynlink.cmxa ]; then echo YES; else echo NO; fi)


OCAMLBUILD_IMPL := ocamlbuild_cppo.cma

ifeq "${BEST}" "opt"
OCAMLBUILD_IMPL += ocamlbuild_cppo.cmxa ocamlbuild_cppo.a
ifeq "${NATDYNLINK}" "YES"
OCAMLBUILD_IMPL += ocamlbuild_cppo.cmxs
endif
endif

OCAMLBUILD_INSTALL = ocamlbuild_plugin/_build/ocamlbuild_cppo.cmi \
                     $(addprefix ocamlbuild_plugin/_build/,$(OCAMLBUILD_IMPL))


.PHONY: default all opt install clean test

default: opt ocamlbuild

ML = cppo_version.ml cppo_types.ml \
     cppo_parser.mli cppo_parser.ml \
     cppo_lexer.ml \
     cppo_command.ml \
     cppo_eval.ml cppo_main.ml

OCAMLBUILD_ML = ocamlbuild_cppo.ml

all: $(ML)
	ocamlc -o cppo$(EXE) -dtypes unix.cma $(ML)

opt: $(ML)
	ocamlopt -o cppo$(EXE) -dtypes unix.cmxa $(ML)

ocamlbuild:
	cd ocamlbuild_plugin && ocamlbuild -use-ocamlfind $(OCAMLBUILD_IMPL)

install: install-bin install-lib

install-bin:
	install -m 0755 cppo $(BINDIR) || \
		install -m 0755 cppo.exe $(BINDIR)

install-lib:
	ocamlfind install -patch-version ${VERSION} "cppo_ocamlbuild" META $(OCAMLBUILD_INSTALL)

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

test:
	cd testdata; $(MAKE)

clean:
	rm -f *.cm[iox] *.o *.annot *.conflicts *.automaton \
		cppo \
		cppo_parser.mli cppo_parser.ml cppo_lexer.ml cppo_version.ml
	cd examples; $(MAKE) clean
	cd ocamlbuild_plugin; ocamlbuild -clean

SUBDIRS = testdata examples
SVNURL = svn+ssh://mjambon@svn.forge.ocamlcore.org/svnroot/cppo/trunk/cppo

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
	$(MAKE) && ./cppo -help > $$WWW/cppo-help.txt
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
	cd ../releases && \
		svn add cppo.tar.gz cppo.tar.bz2 \
			cppo-$(VERSION).tar.gz cppo-$(VERSION).tar.bz2 && \
		svn commit -m "cppo version $(VERSION)"
	cp README $$WWW/cppo-manual-$(VERSION).txt
	cp LICENSE $$WWW/cppo-license.txt
	cp Changes $$WWW/cppo-changes.txt
	echo 'let cppo_version = "$(VERSION)"' \
		> $$WWW/cppo-version.ml
