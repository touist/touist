warning:
	@echo "WARNING: this Makefile is just here to give"
	@echo "an overview of the build process. Please see"
	@echo "./INSTALL.md to get the whole picture."
	@echo ""
	@echo "If you want to build with this Makefile anyway,"
	@echo "here is what you should try:"
	@echo "   make build"
	@echo "but it probably won't work because of missing"
	@echo "tools for building."

# This target builds `touistc`
touist-gui/external/touistc:
	cd touist-translator && \
	./configure --bindir ../touist-gui/external/ && \
	make && \
	make install

touist-gui/external/yices-smt2: 
	@echo "You must unzip yices-smt2.zip and put"
	@echo "the right yices-smt2 binary (for your platform)"	
	@echo "into touist-gui/external/."

build: touist-gui/external/touistc \
       touist-gui/external/yices-smt2 \
       touist-gui/external/minisat.jar
	cd touist-gui &&\
	ant zip

clean:
	cd touist-gui && ant clean
	cd touist-translator && make clean
