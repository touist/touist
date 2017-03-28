## Building the Touist GUI 

### Prerequisites: tools, resources, libs and binaries

First, check if you have `ant`, `git` and `java` in PATH. Also, to be able 
to build, you must have cloned the whole depot (and not only the sources).
To install the tools:

    sudo apt-get install ant git java

Then, you must check/add the external binaries in `external`:

- `external/touist` (see `INSTALL.md`) must be compiled 
   and moved to `external`. We keep a version [in our Google Drive](
   https://drive.google.com/folderview?id=0B5mz8k-t6PT0cW5FSTBxNmgxUjQ&usp=sharing);
   just rename to `touist`.
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

### Building using an IDE (Eclipse...)

Make sure your IDE (eclipse, netbeans...) knows the right _classpath_:

1. Every `.jar` in `./libs/` must be **INDIVIDUALLY** in the classpath
2. The `./build` path (or any folder in which you build the project)
   must be in the classpath
3. The `./ressources` folder (for the .xml file ressource)

#### Eclipse
In Eclipse, you can edit the _classpath_ going into
project properties > java build path > Build tab.

#### Netbeans
In Netbeans, you can edit what is in the _classpath_ in  
the project properties:  
![netbeans classpath](http://img15.hostingpics.net/pics/705361Capturedcran20150329184627.png)

### Add the project to your IDE
**I recommend to create a new project in your IDE with the starting point `./touist-gui` instead of your touist-project-root-folder**. 
You basically just have to create a project using _Java Project with Existing Sources_ (both in Eclipse or Netbeans).

In Netbeans, to add a project with existing sources:  
![netbeans add project](http://img15.hostingpics.net/pics/611633Capturedcran20150329183200.png)


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
           |--touist
           |--minisat.jar
           |--yices-smt2

The command `ant zip` just renames the `dist` folder to something
more specific (with number version and architecture).


## Notes

### Notes on the version numbers displayed inside the program
To display a version number, you must have git installed. The
version number displayed is the one you get by calling

    git describe --tags

More information in the comments of `build.xml`.


### Errors with Netbeans
While doing the GUI, we have had a lot of issues the the `.form` used by netbeans. We didn't know it first, but you must not change the parts of code where it is writen _Do NOT modify this code_. We were using other IDEs like Eclipse, and unlike Netbeans, Eclipse does not "gray out" the parts of code you should not edit.
Other problem with Netbeans: sometimes, it cannot load classes. Just rebuild, it should work.
