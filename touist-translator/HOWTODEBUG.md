Some information about how to debug/build this lexer/parser
===========================================================

To be able to debug those files:
- I use `utop`
- I open `utop`

    #use "set_ext.ml";;
	module StringSet = Make(String);;
	let s = StringSet.empty;;
	let s = StringSet.add "a" s;;
	let s = StringSet.add "b" s;;
	let s = StringSet.add "c" s;;
	
To debug this mess, first go to the translator folder
	
	cd touist-translator

The touistc must be in .byte instead of .native. In
`_oasis`, I replaced the line with `CompiledObject: byte`.
You must then re-generate the build scripts using `oasis`:

    oasis setup

Then make/make install:

	./configure --enable-debug --bindir .
	make && make install
	
For a short test:

    echo 'begin formula $a end formula' | ./touistc -sat /dev/stdin -o /dev/stdout -table /dev/stdout

Now with the debugger. Note that I installed `rlwrap` to be
able to use the arrows to go through the command history.

    rlwrap ocamldebug touistc.byte -sat test/atleast.touistl

The commands I used:
- `r` to run, `f` to finish and be able to re-run
- `break @eval 366` to set a breakpoint on src/eval.ml at line 366
- `s` to step forward when you reached a breakpoint
- `b` or `backstep` to do a backward step when you what to understand what 
  happened just before the crash/exception
  
# Not using `make`
Using directly `ocamlbuild`:

    ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --table --inspection -v -la 2" -ocamlc "ocamlc -g" -package fileutils,str,menhirLib touistc.byte

Or whith `-tag debug` instead of -ocamlc "ocamlc -g":

    ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --table --inspection -v -la 2" -package fileutils,str,menhirLib touistc.byte -tag debug

# RE-building `parser_messages.ml` using `parser.messages`

    menhir parser.mly --list-errors > parser.messages
    menhir parser.mly --update-errors parser.messages > tmp && mv tmp parser.messages
    menhir --compile-errors parser.messages parser.mly > parser_messages.ml

# Testing with ocaml interpreter
For utop or ocaml users: to open the FilePath module
from the fileutils package, do the following:

    #use "topfind";;
    #require "fileutils";;

If it doesn't work, check if the fileutils package is installed:

    #list;; (uses the topfind library from the ocamlfind package)
Then you can open the FilePath module:

    open FilePath;;
