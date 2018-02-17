#! /usr/bin/env bash

# This script takes a shadowJar (fat-jar) touist.jar
# and turns it into a module-enabled jar and produce
# a dist/ folder with embedded JRE using jlink.

set -e

./gradlew shadowJar
JARPATH=build/libs/touist.jar # relative to current dir
MOD=touist
ROOT_DIR=$PWD

# copy original jar into place
mkdir -p "$ROOT_DIR/build/out"
cp "$ROOT_DIR/$JARPATH" "$ROOT_DIR/build/out/$MOD.jar"

# extract original jar
rm -rf build/classes-extracted
mkdir build/classes-extracted
cd build/classes-extracted
jar xf "$ROOT_DIR/$JARPATH"

# compile module-info.java
cd "$ROOT_DIR/resources/jlink/$MOD"
javac -p "$MOD" -d "$ROOT_DIR"/build/classes-extracted module-info.java

# update output jar
jar uf "$ROOT_DIR/build/out/$MOD.jar" -C "$ROOT_DIR/build/classes-extracted" module-info.class

cd "$ROOT_DIR"

rm -Rf build/dist
"$(/usr/libexec/java_home)"/bin/jlink --add-modules touist \
--limit-modules touist --launcher touist=touist/touist.TouIST \
--module-path build/out/touist.jar:"$(/usr/libexec/java_home)"/jmods \
--no-man-pages --no-header-files --strip-debug --compress 2 \
--output build/dist

cat > build/dist/touist <<'EOF'
#! /bin/sh
"$(dirname "$0")"/bin/java -Dtouist.dir="$(dirname "$0")" -Dtouist.externalRelativeDir=./external -Dtouist.saveRelativeDir=. -m touist/touist.TouIST
EOF
chmod +x build/dist/touist


# remind me to generate module-info.java:
echo "Reminder: don't forget to update if necessary 'modules-info.java':"
echo "    jdeps --generate-module-info resources/jlink $1"