#
# Just a simple makefile for helping people get into the project
build:
	@jbuilder build

install:
	@jbuilder install

uninstall:
	@jbuilder uninstall

clean:
	@jbuilder clean

test:
	@jbuilder runtest

# For finding the errors that should be in parser.messages but are not
# because parser.mly has been updated and some new errors appeared.
missing:
	@jbuilder build @src/lib/missing

doc:
	@jbuilder build @doc
#
# These targets aim to build the java GUI in support/gui/
# It basically calls `./gradlew` (which is like `make` but for java)
# and checks that all the necessary
#

build-gui: build check-requirements check-opam-packages
	cd support/gui/ && ./gradlew build
	@echo "Done! Now you can run touist with 'make run-gui'"

run-gui: build-gui
	cd support/gui/ && ./gradlew run

clean-gui:
	cd support/gui/ && ./gradlew clean

.PHONY: build-gui clean-gui run-gui missing test clean uninstall install build
