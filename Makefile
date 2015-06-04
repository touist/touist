all: touist-gui/external/yices-smt2 touist-gui/external/touistc
	cd touist-gui;\
	ant zip
touist-gui/external/touistc:
	cd touist-translator; make; \
	cp _build/src/touistc.native ../touist-gui/external/touistc
	chmod ugo+x touist-gui/external/touistc


