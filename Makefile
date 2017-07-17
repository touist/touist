#
# These targets are generated when using `oasis setup` and aim to build
# the touist compiler (touist)
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

.PHONY: missing FORCE pre-build clean-pre-build aadjnzdljah

targets = src/lib/version.ml src/lib/parser_messages.ml
pre-build: $(targets)

# Produced by menhir
%parser.messages: %parser.mly
	if [ -f $@ ]; then menhir $< --update-errors $@ > tmp && mv tmp $@; fi
	if [ ! -f $@ ]; then menhir $< --list-errors > $@; fi

# Produced by menhir
%parser_messages.ml: %parser.messages %parser.mly
	@mkdir -p $(dir $@)
	menhir --compile-errors $^ > $@

# Generate _build/src/version.ml that contains the version number;
# If we are in a git repo, use `git describe --tags`
# If we aren't, use `Version:` in _oasis
ifeq ($(shell test -d .git && echo yes),yes)
# If we are in a git repo, whenever the content of
# .git/ changes, we want to run the rule %version.ml.
src/lib/version.ml: .git/HEAD .git/refs/heads/
endif
src/lib/version.ml: src/lib/version.cppo.ml
	@mkdir -p $(dir $@)
	@[ $(HAS_YICES2) ] || (echo "Please set the HAS_YICES2 var, e.g., 'make $@ HAS_YICES2=true'" && exit 1)
	@[ $(HAS_QBF) ] || (echo "Please set the HAS_QBF var, e.g., 'make $@ HAS_QBF=true'" && exit 1)
	@[ $(VERSION) ] || (echo "Please set the VERSION var, e.g., 'make $@ VERSION=0.0.2'" && exit 1)
	@HAS_GIT_TAG=$$(test -d .git && which git 2>&1 >/dev/null && echo true || echo false);\
	[ "$$HAS_GIT_TAG" = true ] && GIT_TAG=$$(git describe --tags);\
	cppo $< -o $@ -D "HAS_YICES2 $(HAS_YICES2)" -D "HAS_QBF $(HAS_QBF)" \
				  -D "VERSION \"$(VERSION)\"" \
	              -D "GIT_TAG \"$$GIT_TAG\"" -D "HAS_GIT_TAG $$HAS_GIT_TAG" ;\
	echo "Updated $@ to $(VERSION) (git: $$GIT_TAG)"
# NOTE: when using the $V variable, I must escape
# it with $$V. If I don't, 'make' will replace it
# before the shell is able to expand it.

clean-pre-build:
	rm -Rf $(targets)

# For finding the errors that should be in parser.messages but are not
# because parser.mly has been updated and some new errors appeared.
missing:
	menhir --list-errors src/lib/parser.mly > parser.messages_updated
	menhir --compare-errors parser.messages_updated --compare-errors src/lib/parser.messages --list-errors src/lib/parser.mly

#
# These targets aim to build the java GUI in support/gui/
# It basically calls `./gradlew` (which is like `make` but for java)
# and checks that all the necessary
#

.PHONY: build-gui clean-gui run-gui smt

# This target builds 'touist'
support/gui/external/touist: check-opam-packages build
	cp _build/src/touist.native support/gui/external/touist


build-gui: check-requirements support/gui/external/touist support/gui/external/minisat.jar
	cd support/gui/ &&\
	./gradlew jar
	@echo "Done! Now you can run touist with './gradlew run'"

clean-gui:
	cd support/gui/ && ./gradlew clean

smt:
	@echo "You must download yices-smt2 (there is a binary for mac, win, linux):"
	@echo "     http://yices.csl.sri.com"
	@echo "Then, put the binary yices-smt2 into support/gui/external/"
	@echo "Finally, launch 'make' again "


##### Check for tools/programs required ######
check-requirements: opam m4 git
	@command -v javac || (echo \
	"javac is not installed.\n \
	Install Java JDK with\n\n    sudo apt install default-jdk'.\n" && exit 2)
	@command -v ocamlfind || (echo \
	"ocamlfind is not installed.\n\
	Install it with\n\n    opam install -y ocamlfind\n" && exit 3)

opam m4 git madoko madoko-local:
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

manual:
	make -C docs
