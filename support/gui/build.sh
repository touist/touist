#! /usr/bin/env bash
set -e
JARPATH=$1 # relative to current dir
MOD=$2
ROOT_DIR=$PWD

# copy original jar into place
mkdir -p "$ROOT_DIR/build/out"
cp "$ROOT_DIR/$JARPATH" "$ROOT_DIR/build/out/$MOD.jar"

# extract original jar
rm -rf build/classes-extracted
mkdir build/classes-extracted
cd build/classes-extracted
jar xf "$ROOT_DIR/$JARPATH"

# generate module-info.java
cd "$ROOT_DIR"
jdeps --generate-module-info build/work "$1"
# compile module-info.java
cd "$ROOT_DIR/build/work/$MOD"
javac -p "$MOD" -d "$ROOT_DIR"/build/classes-extracted module-info.java

# update output jar
jar uf "$ROOT_DIR/build/out/$MOD.jar" -C "$ROOT_DIR/build/classes-extracted" module-info.class

cd "$ROOT_DIR"
echo "Ready."
echo "now, do:"
echo "/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/bin/jlink --add-modules touist --limit-modules touist --launcher touist=touist/touist.TouIST --output dist --module-path build/out/touist.jar:/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/jmods --limit-modules touist --no-man-pages --no-header-files --strip-debug --compress 2"