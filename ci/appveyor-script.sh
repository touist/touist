# This script must be launched with 'bash -e' to make the whole script fail if
# one command fails. Or you can use the command 'set -e' here to do the same.
# The flag 'set -x' will print a '+' followed by the command that is executed.
set -e
set -x

TOUIST_BUILD_DIR=$PWD

cd $HOME
curl -L https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam32.tar.xz | tar xJ
bash opam32/install.sh
opam init -y -a default "https://github.com/fdopen/opam-repository-mingw.git#opam2" -c "ocaml-variants.4.07.1+mingw32c" --disable-sandboxing
eval $(opam config env)
opam update
opam install -y ocamlfind menhir minisat ounit zarith depext-cygwinports jbuilder re cmdliner
opam pin add -y qbf https://github.com/c-cube/ocaml-qbf.git

if ! ocamlfind query yices2; then
	# We want a static libgmp.a. The mingw64-i686-gmp version only contains a
	# shared-only version of libgmp. I managed to upload a static version of
	# libgmp.a in the ocamlyices2's releases. We simply override the installed
	# shared libgmp.a with our static libgmp.a.
	curl -L https://github.com/maelvalais/ocamlyices2/releases/download/v0.0.3/gmp-6.1.2-static-i686-w64-mingw32.tar.gz | tar xz

	# Now, download the ocamlyices2 tarball and build the ocaml library
	curl -L https://github.com/maelvalais/ocamlyices2/archive/v0.0.3.tar.gz | tar xz
	cd ocamlyices2-0.0.3
	./configure --host=i686-w64-mingw32 --with-static-gmp=$HOME/gmp-6.1.2-static-i686-w64-mingw32/libgmp.a
	make
	make install
fi

cd "$TOUIST_BUILD_DIR"
# Build touist
jbuilder build

# Because 'core.autocrlf input' is set, parser.messages is checked-out using
# LF endings. But menhir re-generates it, thus producing CRLF endings instead.
# Solution: dos2unix on it each time...
dos2unix src/lib/parser.messages

jbuilder install

# 'jbuilder runtest' and 'jbuilder uninstall' are done in appveyor.yml

# Build the actual TouIST.exe
cd support/gui
TERM=dumb ./gradlew createExeZip createJarZip
ARCH=windows-x86

# Add $ARCH to the end of the .zip filename
for f in $(find build/distributions -name "TouIST*"); do
	mv "$f" $(echo $f | sed "s/\(.*\)\.zip$/\1-${ARCH}.zip/")
done

ls build/distributions
cd "$TOUIST_BUILD_DIR"

git status
if ! git status 2>/dev/null | tail -n1 | grep "nothing.*clean"; then
	echo 'STOP!!! The build ends in a dirty state; see the diff:'
	git diff
	exit 1
fi

# Now that we are done with TouIST-v1.2.3-windows-x86, lets create the
# touist-cli-v1.2.3-windows-x86.zip
cd _build/default/src/
mv ./*.exe touist-cli-"$(git describe --tags)"-${ARCH}.exe
cd "$TOUIST_BUILD_DIR"
mv _build/default/src/touist-cli* _build/
