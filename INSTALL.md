## Building 

The Touist project is made of two different sub-projects:
- the _touist-translator_, which is a command-line compiler from `.touistl` to `dimacs`,
- the _touist-gui_, which is the main graphical interface.

Steps:

1. go to `./touist-translator` and build `touistc.native`
   (see `touist-translator/INSTALL.md` for more details)
2. rename to `touistc` and move it to `./touist-gui/external`
3. go to `./touist-gui` and build it 
   (see `touist-gui/INSTALL.md` for more details)

Same steps with the Makefile structure:

    touist-gui/external/touistc:
    	cd touist-translator; make; \
    	cp _build/src/touistc.native ../touist-gui/external/touistc
    	chmod ugo+x touist-gui/external/touistc

    all: touist-gui/external/touistc \
         touist-gui/external/yices-smt2 \
         touist-gui/external/minisat.jar
    	cd touist-gui;\
    	ant zip

## Running

See `touist-gui/INSTALL.md`

