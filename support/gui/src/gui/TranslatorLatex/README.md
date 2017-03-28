## How the coloring in Touist is made

We use `jflex` as a lexer generator and `cup` as the parser generator.

To download them:

- [jflex](http://jflex.de/download.html)
- [cup](http://www2.cs.tum.edu/projects/cup/install.php)

Build `jflex`: go to the unziped folder and do:

    ant gettools
    ant jar

The jflex program is `jflex-1.6.1/build/jflex-1.6.1.jar`.  
The cup program is `java-cup-bin-11b-20151001/java-cup-11b.jar`.  

Now, put those `.jar` in the `TranslatorLatex` folder and do:
   
    java -jar jflex-1.6.1.jar Latex.flex
    java -jar java-cup-11b.jar Latex.cup
	

## Known errors

If `jflex` tells you it cannot handle that many warnings, then use the command:

    java -jar java-cup-11b.jar -expect 900 Latex.cup
