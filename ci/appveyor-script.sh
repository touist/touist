# This script must be launched with 'bash -e' to make the whole script fail if
# one command fails. Or you can use the command 'set -e' here to do the same.
# The flag 'set -x' will print a '+' followed by the command that is executed.
set -e
set -x

TOUIST_BUILD_DIR=$PWD

cd $HOME
curl -L https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.1/opam32.tar.xz | tar xJ
bash opam32/install.sh
opam init -y -a mingw https://github.com/fdopen/opam-repository-mingw.git --comp 4.03.0+mingw32c --switch 4.03.0+mingw32c
eval `opam config env`
opam install -y fileutils menhir minisat cppo zarith depext-cygwinports ounit
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

cd $TOUIST_BUILD_DIR
# Build touist. I do not install it because --prefix + 'make install' seems
# to be buggy with oasis...
# I also totally removed the 'make install' because each time there would be an
# error: 'touist' is already installed. First, I was doing 'make uninstall'
# before 'make install' but even though, it is not working (this is because
# make install must be run first, the setup.log info is needed...).
# So I abandoned. More info in the issue:
# https://forge.ocamlcore.org/tracker/?func=detail&group_id=54&aid=758&atid=291
ocamlfind remove touist
./configure --bindir support/gui/external --enable-tests --enable-yices2 --enable-qbf
make
make install # Only install binary 'touist' as long as --enable-lib is not given

# Build the Java GUI touist.jar (in support/gui)
# First, put the SMT solver into support/gui/external/
if [ ! -d /cygdrive/c/yices ]; then
    curl -L -o yices.zip "http://yices.csl.sri.com/cgi-bin/yices2-newnewdownload.cgi?file=yices-2.4.2-i686-pc-mingw32-static-gmp.zip&accept=I+Agree"
    unzip -q yices.zip
    rm yices.zip
    mv yices* /cygdrive/c/yices
    cp /cygdrive/c/yices/bin/yices-smt2.exe support/gui/external
    cp /cygdrive/c/yices/bin/libyices.dll support/gui/external
fi

# Build the actual TouIST.exe
cd support/gui
./gradlew createExeZip
artifacts=$(find build/distributions -name "TouIST*" | head -1)
echo $artifacts
cd ../..