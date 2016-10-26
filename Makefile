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
	cd touist-translator && make clean


smt:
	@echo "You must download yices-smt2 (there is a binary for mac, win, linux):"
	@echo "     http://yices.csl.sri.com"
	@echo "Then, put the binary yices-smt2 into touist-gui/external/"
	@echo "Finally, launch 'make' again "


##### Check for tools/programs required ######
check-requirements:
	@command -v javac || (echo \
	"javac is not installed. \
	Install Java JDK with 'apt-get default-jdk'." && exit 2)
	@command -v ant || (echo \
	"ant is not installed.\
	Install it with 'apt-get install ant'." && exit 3)
	@command -v opam || (echo \
	"opam is not installed.\
	Install it with 'apt-get install opam'." && exit 4)
	@command -v ocamlfind || (echo \
	"ocamlfind is not installed.\
	Install it with 'opam install ocamlfind'." && exit 5)

check-opam-packages:
	@ocamlfind query menhirLib fileutils minisat str \
		|| (echo "Install it with 'opam install <package>'" && exit 6)
