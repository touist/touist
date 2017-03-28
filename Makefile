#
# These targets are generated when using `oasis setup` and aim to build
# the touist compiler (touistc)
#
# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

#
# These targets are here for building everything that is needed before
# launching the real build process (i.e., compiling the ocaml code):
# 1. git-produced 'version.ml' for embedding the version number,
# 2. menhir-produced 'parser.messages' and 'parser_messages.ml' which are
#    the syntax error messages that must be compiled to `.ml`
#
# The purpose of this makefile is only to be called (by the oasis-generated
# setup.ml) when using ./configure + make
#

.PHONY: missing FORCE pre-build clean-pre-build

SRC=src
# If git is present, then ROOT_GIT will be set with the project root path
# elsewise, ROOT_GIT will not be defined at all

targets = $(SRC)/parser_messages.ml $(SRC)/version.ml
pre-build: $(targets)

# Produced by menhir
%parser.messages: %parser.mly
	if [ -f $@ ]; then menhir $< --update-errors $@ > tmp && mv tmp $@; fi
	if [ ! -f $@ ]; then menhir $< --list-errors > $@; fi

# Produced by menhir
%parser_messages.ml: %parser.messages %parser.mly
	menhir --compile-errors $^ > $@

# Produced by git
%version.ml: FORCE
	@echo "let version=\"`if which git 1>&2 2>/dev/null; then git describe --tags; else echo "n/a"; fi`\"" > $@
# A dummy target to force version.ml to rebuild each time
FORCE:

clean-pre-build:
	cd $(SRC) && rm -f parser_messages.ml version.ml

# For finding the errors that should be in parser.messages but are not
# because parser.mly has been updated and some new errors appeared.
missing:
	menhir --list-errors src/parser.mly > parser.messages_updated
	menhir --compare-errors parser.messages_updated --compare-errors src/parser.messages --list-errors src/parser.mly

#
# These targets aim to build the java GUI in support/gui/
# It basically calls `ant` (which is like `make` but for java)
# and checks that all the necessary 
#

.PHONY: build-gui clean-gui run-gui smt

# This target builds 'touistc'
support/gui/external/touistc: check-opam-packages build
	cp _build/src/touistc.native support/gui/touistc


build-gui: check-requirements support/gui/external/touistc support/gui/external/minisat.jar
	cd support/gui/ &&\
	ant -e jar
	@echo "Done! Now you can run touist with 'make run-gui'"

run-gui: support/gui/build/*
	cd support/gui/ && java -jar touist.jar

clean-gui:
	cd support/gui/ && ant clean

smt:
	@echo "You must download yices-smt2 (there is a binary for mac, win, linux):"
	@echo "     http://yices.csl.sri.com"
	@echo "Then, put the binary yices-smt2 into support/gui/external/"
	@echo "Finally, launch 'make' again "


##### Check for tools/programs required ######
check-requirements: opam m4 ant git
	@command -v javac || (echo \
	"javac is not installed.\n \
	Install Java JDK with\n\n    sudo apt install default-jdk'.\n" && exit 2)
	@command -v ocamlfind || (echo \
	"ocamlfind is not installed.\n\
	Install it with\n\n    opam install -y ocamlfind\n" && exit 3)

%:
	@command -v $@ || (echo \
	"$@ is not installed.\n\
	Install it with\n\n    sudo apt install $@\n" && exit 5)


check-opam-packages:
	@ocamlfind query menhirLib fileutils minisat \
		|| (echo \
	    'Install opam packages with:\n\n'\
		'  opam init && eval `opam config env`\n'\
		'  opam install -y menhir fileutils\n'\
		'  opam pin add -y minisat'\
	    ''\''https://github.com/maelvalais/ocaml-minisat.git#v0.0.2'\''\n'\
	&& exit 6)
