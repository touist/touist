#
# Makefile
# Maël Valais, 2017-06-18 13:04
#

all: html

out/%.html: %.mdk bibliography.bib
	madoko $^ -mversion:$$(git describe --tags)

out/%.pdf: %.mdk bibliography.bib
	madoko $^ -mversion:$$(git describe --tags) --pdf

html: out/reference-manual.html
pdf: out/reference-manual.pdf

html-open: html
ifeq ($(shell uname -s),Darwin)
	open out/reference-manual.html -g
endif
pdf-open: pdf
ifeq ($(shell uname -s),Darwin)
	open out/reference-manual.pdf -g
endif

clean:
	rm -Rf out/
# vim:ft=make
#
