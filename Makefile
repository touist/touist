all: help check-requirements build
help:
	@echo "============"
	@echo "WARNING: this Makefile is just here to ease the build"
	@echo "process. Multiple programs are required for this Makefile to work."
	@echo "Please see ./INSTALL.md to get the whole picture if 'make' fails."
	@echo "OPTIONNAL: you can enable SMT by calling 'make smt' and then 'make'"
	@echo "============"


# This target builds 'touistc'
touist-gui/external/touistc: touist-translator/*
	cd touist-translator && \
	./configure --bindir ../touist-gui/external/ && \
	make && \
	make install


build: touist-gui/external/touistc touist-gui/external/minisat.jar
	cd touist-gui &&\
	ant -e jar
	@echo "Done! Now you can run touist with 'make run'"

run: touist-gui/build/*
	cd touist-gui && java -jar touist.jar

clean:
	cd touist-gui && ant clean
	cd touist-translator && make clean


smt:
	@echo "You must download yices-smt2 (there is a binary for mac, win, linux):"
	@echo "     http://yices.csl.sri.com"
	@echo "Then, put the binary yices-smt2 into touist-gui/external/"
	@echo "Finally, launch 'make'"


##### Check for tools/programs required ######
JAVAC := $(shell command -v javac 2> /dev/null)
ANT := $(shell command -v ant 2> /dev/null)
OPAM := $(shell command -v opam 2> /dev/null)
MENHIR := $(shell command -v menhir 2> /dev/null)


check-requirements:
ifndef JAVAC
    $(error "javac is not installed. Install Java JDK.")
endif
ifndef ANT
    $(error "ant is not installed. Install it with 'apt-get install ant'")
endif
ifndef OPAM
    $(error "opam is not installed. Install it with 'apt-get install opam'")
endif
ifndef MENHIR
    $(error "menhir is not installed. Install it with 'opam install menhir'.\n\
		         NOTE: you must also install fileutils by 'opam install fileutils'")
endif
