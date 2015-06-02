#! /bin/sh
#
# generate-version-number.sh
# Copyright (C) 2015 Mael Valais <mael.valais@univ-tlse3.fr>
#
# Distributed under terms of the MIT license.
#
# This script creates the `src/version.ml` file. 
# `src/version.ml` is used for integrating the version
# number used in git into the `touistc` app.
# 
# Behaviour:
# 	- if `src/version.ml` exists or doesn't exist and that `git` exists,
#	then re-creates `src/version.ml` with:
#			`let version="the git version number"`
# 	- if git doesn't exist and that `src/version.ml` doesn't exist, we
# 	create a file with:
#			`let version="n/a"`

cd src
if which git || ((! which git) && [ ! -f version.ml ]); then
	echo let version=\"`if which git > /dev/null; then git describe --tags; else echo "n/a"; fi;`\" > version.ml; 
fi
cd ..

