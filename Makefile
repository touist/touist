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

.PHONY: missing FORCE pre-build clean-pre-build

targets = src/lib/parser_messages.ml src/cli/version.ml
pre-build: $(targets)

# Produced by menhir
%parser.messages: %parser.mly
	if [ -f $@ ]; then menhir $< --update-errors $@ > tmp && mv tmp $@; fi
	if [ ! -f $@ ]; then menhir $< --list-errors > $@; fi

# Produced by menhir
%parser_messages.ml: %parser.messages %parser.mly
	menhir --compile-errors $^ > $@

# Generate src/version.ml that contains the version number;
# If we are in a git repo, use `git describe --tags`
# If we aren't, use `Version:` in _oasis
ifeq ($(shell test -d .git && echo yes),yes)
# If we are in a git repo, whenever the content of
# .git/ changes, we want to run the rule %version.ml.
git_prerequisite=.git/HEAD .git/refs/heads
endif
%version.ml: _oasis $(git_prerequisite)
	@V="";\
	if test -d .git && which git 2>&1 >/dev/null; then\
		V="`git describe --tags` (git)";\
	else\
		V="v`grep 'Version:  *[0-9][0-9\.]*' _oasis |\
		tr -d ' ' | cut -d: -f2` (opam)";\
	fi;\
	echo "let version=\"$$V\"" > $@;\
	echo "Updated $@ to $$V"
# NOTE: when using the $V variable, I must escape
# it with $$V. If I don't, 'make' will replace it
# before the shell is able to expand it.

clean-pre-build:
	rm -f $(targets)

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

# This target builds 'touist'
support/gui/external/touist: check-opam-packages build
	cp _build/src/touist.native support/gui/external/touist


build-gui: check-requirements support/gui/external/touist support/gui/external/minisat.jar
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

#
# This target will do the necessary to update the version number.
# At first, the version number was only stored using the git tag.
# But because of opam, the version number must be set in _oasis
# before I do `git describe --tags`...
# So I wrote this target to ease the process of 'Bumping' the version
# number.
.PHONY: version
version: awk opam-publish oasis2opam oasis
	@[ $(VERSION) ] || (echo "Please set the VERSION var, e.g., 'make $@ VERSION=3.0.2'" && exit 1)
	@echo $(VERSION) | grep "[0-9]\.[0-9]\.[0-9]" >/dev/null || (echo "VERSION must be of the form 3.0.2" && exit 1)
	@echo "\033[92mChanging the 'Version:' field in '_oasis' from $$(oasis query version) to $(VERSION)\033[0m"
	@sed "s/^\(Version: *\)[0-9][0-9\.]*$$/\1$(VERSION)/" _oasis > a && mv a _oasis
	@echo "\033[92mRunning 'oasis setup' to update setup.ml, src/META\033[0m"
	@oasis setup
	@echo "\033[92mRunning 'oasis2opam --local -y' to update opam/opam\033[0m"
	@oasis2opam --local -y
	@echo "\033[92mRunning 'git add setup.ml _oasis src/META opam/*'\033[0m"
	git add setup.ml _oasis src/META opam/*
	@echo "\033[92mCommiting with message 'Bump to v$(VERSION)'\033[0m"
	git commit -m "Bump to v$(VERSION)"
	@echo "================="
	@echo "\033[91mNow, you can do:\033[0m"
	@echo "    git tag -s v$(VERSION)"
	@echo "    git push --tags"
	@echo "    oasis2opam https://github.com/touist/touist/archive/v$(VERSION).tar.gz"
	@echo "    opam-publish submit touist.$(VERSION)"
	@echo "\033[91mor cancel the commit that has been done with:\033[0m"
	@echo "    git reset HEAD^"


