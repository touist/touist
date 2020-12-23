#! /bin/sh

set -x
set -e

HOST=x86_64-w64-mingw32

opam install -y ocamlfind
if ! opam exec -- ocamlfind query yices2; then
    curl -L https://github.com/maelvls/ocamlyices2/releases/download/v0.0.4/gmp-6.2.1-static-x86_64-w64-mingw32.tar.gz | tar xz
    libgmp=$PWD/gmp-6.2.1-static-x86_64-w64-mingw32/libgmp.a

    export CFLAGS="-I$(dirname "$libgmp")"
    export LDFLAGS="-L$(dirname "$libgmp")"

    opam install zarith

    curl -L https://github.com/maelvls/ocamlyices2/archive/v0.0.4.tar.gz | tar xz
    cd ocamlyices2-0.0.4
    opam exec -- ./configure --host=$HOST --with-static-gmp="$libgmp" CFLAGS="-I$(dirname "$libgmp")"
    opam exec -- make
    opam exec -- make install
    cd ..
fi

# reg query 'HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment'
