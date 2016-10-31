.PHONY: all help build run clean zip
all: help check-requirements build
help:
	@echo "============"
	@echo "WARNING: this Makefile is just here to ease the build"
	@echo "process. Multiple programs are required for this Makefile to work."
	@echo "Please see ./INSTALL.md to get the whole picture if 'make' fails."
	@echo "OPTIONNAL: you can enable SMT by calling 'make smt' and then 'make'"
	@echo "============"


# This target builds 'touistc'
touist-gui/external/touistc: touist-translator/* check-opam-packages
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
	cd touist-translator && make clean distclean


smt:
	@echo "You must download yices-smt2 (there is a binary for mac, win, linux):"
	@echo "     http://yices.csl.sri.com"
	@echo "Then, put the binary yices-smt2 into touist-gui/external/"
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
