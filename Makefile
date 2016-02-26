all:
	@echo "WARNING: this Makefile is just here to give"
	@echo "an overview of the build process. Please see"
	@echo "./INSTALL.md to get the whole picture."
	@echo ""
	@echo "If you want to build with this Makefile anyway,"
	@echo "here is what you should try:"
	@echo "   make build"

touist-gui/external/touistc:
	cd touist-translator; make; \
    cp _build/src/touistc.native ../touist-gui/external/touistc
	chmod ugo+x touist-gui/external/touistc

build: touist-gui/external/touistc \
       touist-gui/external/yices-smt2 \
       touist-gui/external/minisat.jar
	cd touist-gui;\
	ant zip

clean:
	cd touist-gui;\
	ant clean
	cd touist-translator;\
	make clean
