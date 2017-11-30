Some information about how to debug/build this lexer/parser
===========================================================
I wrote this file to remember how I am doing to debug the parser: how to
build it using jbuilder, how understand the automaton behaviour... I hope
it will help!

## Build and install ##

The build products are in `_build/default`.

    jbuilder build
    jbuilder install
    jbuilder uninstall

For example, `touist` is in `_build/default/src/main.exe`

## Running touist in development ##

Either use `jbuild exec` or `_build/default/src/main.exe`:

    echo '$a' | jbuilder exec touist -- - --sat
    echo '$a' | _build/default/src/main.exe - --sat

## Using utop during developement ##

    jbuilder utop src/lib        # Touist.* available
    jbuilder utop src/lib/smt    # Touist.* and Touist_smt available

## Edit how the build is made ##
The build is handled by jbuilder; see the `jbuild` in each lib/src dir.

## Run the debugger ##

If you want to run ocamldebug, you must build touist as a .bc (bytecode)
instead of .exe (native).

To build one the bytecode version in `_build/default/src/main.bc`:

    jbuilder build src/main.bc

Note: the option `-menhir "--trace"` -> will show what the automaton does.

Now with the debugger. Note that I installed `rlwrap` to be able to use the
arrows to go through the command history.

    rlwrap ocamldebug _build/default/src/main.bc --sat test/atleast.touistl

The commands I used:
- `r` to run, `f` to finish and be able to re-run
- `break @eval 366` to set a breakpoint on src/eval.ml at line 366
- `s` to step forward when you reached a breakpoint
- `b` or `backstep` to do a backward step when you what to understand what
  happened just before the crash/exception



The syntax errors in parser.messages
====================================
One of the new features of the touist compiler is to be able to give real
and understandable syntax errors (when you forget to close a parenthesis...).
To do so, we use a feature of menhir that allows to pick a message among a
list of hand-written error messages. The thing is that there are more than
180 of these messages...

So developers can improve the error syntax messages by editing
`parser.messages` and then re-generating `parserMsgs.ml`.


## Re-building `parserMsgs.ml` using `parser.messages` ##

Rebuilding this file is automatic.

You can see what rules are missing from `parser.messages`:

    make missing

which checks if no error messages are missing in parser.messages
(see section below 'Missing error cases')

Most of the time, the only command you want to use is the 3.

1. To generate the `parser.messages` for the first time, use:

        menhir parser.mly --list-errors > parser.messages

2. To update the existing `parser.messages` whenever you modify the `parser.mly`:

        menhir parser.mly --update-errors parser.messages > tmp && mv tmp parser.messages

3. To regenerate the final file `parserMsgs.ml` whenever you edit the error
messages in `parser.messages`:

        menhir --compile-errors parser.messages parser.mly > parserMsgs.ml

## Missing error cases in parser.messages
When updating parser.mly, you might sometimes create new error states
that do not appear in your already-written parser.messages.
This often leads to the error message:
```
1:14: syntax error after ',' and before 'a'.
This is an unknown syntax error (92).
Please report this problem to the compiler vendor.
```
meaning that the state 92 isn't in parserMsgs.ml, and thus not in
parser.messages. To fix that, anytime you modify parser.mly, check that
no new errors have been intruduced:

```
menhir --list-errors src/parser.mly > parser.messages_fresh
menhir --compare-errors parser.messages_fresh --compare-errors src/lib/parser.messages--list-errors src/parser.mly
```

## Testing your hand-written messages ##
There are two ways to test if the messages actually work. The first one
is simply to launch `./touist` with a wrong syntax touistl file. If you built
using ocamlbuild and the option --trace in -menhir "" block, it will show you
the steps of the automaton. It is REALLY helpful to understand why an expression
is not parsed as expected. For example (WARNING: this is the old syntax with
begin formula end formula):

```
jbuilder exec -- touist --sat /dev/stdin -o /dev/stdout --table /dev/stdout << eof
begin
eof

State 0:
Lookahead token is now BEGIN (0-5)
Shifting (BEGIN) to state 1
State 1:
Lookahead token is now EOF (7-7)
Initiating error handling
/dev/stdin:2:1: syntax error after 'begin' and before ''.
You must specify (at least) a "formula" bloc,
or a "sets" bloc followed by a "formula" bloc.
Example:
    begin sets    ... end sets
    begin formula ... end formula
Debug: Automaton state: 1 (see src/parser.messages)
```

The second way is to focus on tokens (the capital letters words like BEGIN)
and to ask menhir to see if this sequence of tokens triggers an error:

```
menhir --interpret-error parser.mly << eof
BEGIN FORMULA ATLEAST LPAREN VAR COMMA RPAREN
eof
```


Prototyping ocaml functions using toplevel
==========================================
*toplevel* = the interpreter that prompts when you launch `ocaml` with no file.

Just launch utop. I wrote a .ocamlinit that should load everything fine.
Just make sure touist has also been compiled in bytecode mode:

    jbuilder build src/main.bc

Unexpected exception: where does it come from?
=============================================
Backtrace for exceptions is disabled by default as it needs more computation.
To enable backtracing on exception, you just have to set to 'b' the
OCAMLRUNPARAM env variable:

    OCAMLRUNPARAM=b jbuilder exec -- touist
