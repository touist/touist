all:
	cd touist-translator; make; \
		cp _build/src/touistc.native ../touist-gui/external/touistc
	cd touist-gui;\
		ant zip
