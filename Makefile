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
# 1. git-produced 'touistVersion.ml' for embedding the version number,
# 2. menhir-produced 'parser.messages' and 'touistParserMsgs.ml' which are
#    the syntax error messages that must be compiled to `.ml`
#
# The purpose of this makefile is only to be called (by the oasis-generated
# setup.ml) when using ./configure + make
#

.PHONY: missing FORCE pre-build clean-pre-build aadjnzdljah

targets = src/lib/touistVersion.ml src/lib/touistParserMsgs.ml
pre-build: $(targets)

# Produced by menhir
%parser.messages: %touistParser.mly
	if [ -f $@ ]; then menhir $< --update-errors $@ > tmp && mv tmp $@; fi
	if [ ! -f $@ ]; then menhir $< --list-errors > $@; fi

# Produced by menhir
%touistParserMsgs.ml: %parser.messages %touistParser.mly
	@mkdir -p $(dir $@)
	menhir --compile-errors $^ > $@

# Generate _build/src/touistVersion.ml that contains the version number;
# If we are in a git repo, use `git describe --tags`
# If we aren't, use `Version:` in _oasis
ifeq ($(shell test -d .git && echo yes),yes)
# If we are in a git repo, whenever the content of
# .git/ changes, we want to run the rule %touistVersion.ml.
src/lib/touistVersion.ml: .git/HEAD .git/refs/heads/
endif
src/lib/touistVersion.ml: src/lib/touistVersion.cppo.ml
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
# because touistParser.mly has been updated and some new errors appeared.
missing:
	menhir --list-errors src/lib/touistParser.mly > parser.messages_updated
	menhir --compare-errors parser.messages_updated --compare-errors src/lib/parser.messages --list-errors src/lib/touistParser.mly

#
# These targets aim to build the java GUI in support/gui/
# It basically calls `./gradlew` (which is like `make` but for java)
# and checks that all the necessary
#

.PHONY: build-gui clean-gui run-gui

build-gui: build check-requirements check-opam-packages
	cd support/gui/ && ./gradlew build
	@echo "Done! Now you can run touist with 'make run-gui'"

run-gui: build-gui
	cd support/gui/ && ./gradlew run

clean-gui:
	cd support/gui/ && ./gradlew clean


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

OCAMLDOC=

# This is a hack. Because `make doc` will do four directories for documentation,
# i.e., one per library (touist.docdir, touist_qbf.docdir, ...), we want to
# regroup them in touist.docdir. This command will:
# - get the modules names of the form TouistXXXX (WARNING: each module name
#   beginning with TouistXXXX must be on a different line and must be
#   preceeded by at least 3 spaces)
# - only keep the module names that have a .cmi in the _build directory,
#   which means that they have been successfully compiled
# - call ocamldoc -d touist.docdir -html -load ... on these modules
doc-unified: setup.data build
	$(SETUP) -doc $(DOCFLAGS)
	cd _build; \
	cat ../_oasis | grep -wo '    *Touist[A-Z][A-Za-z]*' | sed "s/ //g" | while read f; do \
		if R=$$(find "src/lib" -iname "*$$f.cmi"); then \
			echo $$R | sed 's/\(.*\)\.cmi/-load \1.odoc/'; \
		fi; \
	done > cmd
	cd _build; ocamlfind ocamldoc -html -d src/lib/touist.docdir \
		-t "API reference for the TouIST library $$(git describe --tags || echo version unknown)" $$(cat cmd)