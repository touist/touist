## Building the Touist GUI 

### Prerequisites: tools, resources, libs and binaries

First, check if you have `git` and `java` in PATH. Also, to be able to build,
you must have git-cloned the repository. To install the tools:

    sudo apt install git default-jdk    # Ubuntu
    brew cask install java              # MacOS

### Building for debugging/testing

    ./gradlew build
    ./gradlew run

### Building using an IDE (Eclipse...)

I recommend you to open `support/gui` in the Java IDE (not the root of the
TouIST repository).

You can open `support/gui` with IntelliJ IDEA (tested, Gradle is included by
default), Eclipse (tested using the Gradle plugin) and Netbeans (tested using
the Gradle plugin) using the 'Import Gradle project' or something like that.
Gradle relieves you from the burden of managing the classpath.

### Building it into a `.jar` for end-users

Gradle has three commands to build distributable zip in `build/distributions`:

    ./gradlew createMacAppZip   # Contains TouIST.app (macOS)
    ./gradlew createExeZip      # Contains TouIST.exe and lib/ (Windows)
    ./gradlew distZip           # Contains bin/ + lib/ (macOS, Windows, Linux)

### Editing the graphical interface (`.form`)

The files `.form` (e.g., ParentEditorPanel.form) have been created a _long_
time ago using Netbeans. It is painful as hell to maintain because
unexplainable errors can show up at any time...

While doing the GUI, we have had a lot of issues the the `.form` used by
netbeans. We didn't know it first, but you must not change the parts of
code where it is writen _Do NOT modify this code_. We were using other IDEs
like Eclipse, and unlike Netbeans, Eclipse does not "gray out" the parts of
code you should not edit. Other problem with Netbeans: sometimes, it cannot
load classes. Just rebuild, it should work.
