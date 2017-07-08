#
# Makefile
# Maël Valais, 2017-06-18 13:04
#

all: html

out/%.html: %.mdk
	madoko $^ -mversion:$$(git describe --tags)

out/%.pdf: %.mdk
	madoko $^ -mversion:$$(git describe --tags) --pdf

html: out/reference-manual.html
pdf: out/reference-manual.pdf

open-html: html
ifeq ($(shell uname -s),Darwin)
	open out/reference-manual.html -g
endif
open-pdf: pdf
ifeq ($(shell uname -s),Darwin)
	open out/reference-manual.pdf -g
endif

# vim:ft=make
#
