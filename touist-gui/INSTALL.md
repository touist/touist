## Building the Touist GUI 

### Prerequisites: tools, resources, libs and binaries

First, check if you have `ant`, `git`, `java` and `javac` in PATH. Also,
to be able to build, you must have cloned the whole depot (and not only 
downloaded de sources). To install the tools:

    sudo apt-get install ant git java

Then, you must check/add the external binaries in `external`:
- `external/touistc` (see `touist-translator/INSTALL.md`) must be compiled 
   and moved to `external`
- `external/yices-smt2` must be unziped from SMT.zip (located in `libs`)
- `external/minisat.jar` is the SAT solver; we build the .jar from the 
   `./temporary-solver/src` sources and then move `minisat.jar` to
   `external`.
   NOTE: this `.jar` is not inside `libs` because it is called as an external
   command (`java -jar minisat.jar`)

The stuff inside `libs` and `resources` come with the git depot, so you
can just leave their content as they come.

### Building for debugging/testing

    ant build
    ant run

Instead of `ant run`, you can also run the program by

    CLASSPATH=`ls libs/* | tr "\n" ":" | sed "s/:$//"`
	CLASSPATH=$CLASSPATH:resources:build 
	export CLASSPATH
	java touist.TouIST

### Building it into a `.jar` for end-users
We call "distribute" (dist) when we want to ship the program to users.
Here are the commands you can use to prepare the release (note that
calling `ant zip` will also run `ant dist` and `ant jar`):

    ant jar
	ant dist
	ant zip

The command `ant jar` puts all the `.class`, `./resources` and `./libs`
inside a `touist.jar`.

The command `ant dist` creates a directory `dist`; 
it will contain:

    dist 
      |--touist.jar
      |--external
           |--touistc
           |--minisat.jar
           |--yices-smt2

The command `ant zip` just renames the `dist` folder to something
more specific (with number version and architecture).


## Notes on the version numbers displayed inside the program

To display a version number, you must have git installed. The
version number displayed is the one you get by calling

    git describe --tags

More information in the comments of `build.xml`.
