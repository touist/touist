TouIST main program
===================

This folder contains the source code for the main program of the TouIST project. It is named "ui" because of it's main feature : be the user interface for the solver and "bigand" translator.

## Get the right `src` folder in your IDE
You should check that, when you open the project as a java project, the `./touist-gui/src` path is considered as the only `src` path (in Eclipse: in project properties > java build path > Source tab: check that `./touist-gui/src` is the only item).

## Build and run the touist-gui project
###1. Check the `classpath`
Make sure your IDE (eclipse, netbeans...) knows the right _classpath_:

1. Every `.jar` in `touist-gui/libs/` must be in the classpath
2. The `./touist-gui/build` path (or any folder in which you build the project) must be in the classpath

In Eclipse, you can check that in project properties > java build path > Build tab.

###2. Check `external` and `ressources` folders
To run the whole thing, check first what is the _running_ directory. In Eclipse or Netbeans, the running directory is the root folder of your project. We will call it `./` from now on.
Now, check if the following external binaries/jars are at the right place:

1. If you run `Touist.class` from `./` (the root project folder):
  - you must have `./external/touistc` (copy it from `./touist-gui/external`)
  - you must have `./external/minisat.jar` (copy it from `./touist-gui/external`)
  - you must have `./ressources/touistTheme.xml` (copy it from `./touist-gui/ressources`)
2. If you run `Touist.class` from `./touist-gui/`, then there is no need to copy anything because `external` is already here.

##3. Use `build.xml` with Ant
The `build.xml` contains the different targets needed for compilation and distribution.

## Conception/architecture
![conception](https://www.lucidchart.com/publicSegments/view/54f46f57-1ff4-46e0-b146-65000a009e9c/image.png)
