#
# Makefile
# Maël Valais, 2017-06-18 13:04
#

all: html open

html:
	madoko reference-manual.mdk -mversion:$$(git describe --tags)

pdf:
	madoko reference-manual.mdk -mversion:$$(git describe --tags) --pdf

# MacOS only
open:
	open out/reference-manual.html -g

# vim:ft=make
#
