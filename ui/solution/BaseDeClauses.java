/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Skander
 */

class FormatException extends Exception {
    public FormatException() {}

    public FormatException(String message) {
        super(message);
    }
}

public class BaseDeClauses {
    private List<String> formules;

    /**
     * Construct a set of clauses
     */
    public BaseDeClauses() {
        formules = new ArrayList<>();
    }

    /**
     *
     * @return the list of formules
     */
    public List<String> getFormules() {
        return formules;
    }


    /**
     * Import the list of formules from a file
     * @param path of the file containing the list of formules and sets
     * @throws FileNotFoundException if the path of the file is invalid
     * @throws IOException if any I/O exception occurs during file reading
     * @throws FormatException if the filedefines incorrectly the formula
     *                         section
     */
    public void uploadFile(String path) throws FileNotFoundException,
                                               IOException,
                                               FormatException {
        BufferedReader in = new BufferedReader(new FileReader(path));
        String line;

        boolean formulaSection = false;
        int indexLine = 0;

        while((line = in.readLine()) != null) {

            indexLine ++;

            String []words = line.replace("\\s+"," ").split(" ");
            if(words.length == 2) {
                words[0] = words[0].toLowerCase();
                words[1] = words[1].toLowerCase();



                if(words.equals(new String[]{"begins","sets"})) {
                    if(formulaSection == true) {
                        String message = "Syntax error at line " + indexLine
                                       + ". Formula begin section already"
                                       + " declared.";
                        throw new FormatException(message);
                    }
                    else {
                        formulaSection = true;
                    }
                }
                else if(words.equals(new String[]{"ends","sets"})) {
                    if(formulaSection == false) {
                        String message = "Syntax error at line " + indexLine
                                       + ". No formula section is opened.";
                        throw new FormatException(message);
                    }
                    else {
                        formulaSection = false;
                    }
                }
            }
            else if(formulaSection) {
                addFormule(line);
            }
        }
    }

    public void saveToFile(String path) {
	// TODO
    }

    /**
     * Add a formule to the current list of formules
     * @param A formule
     */

    public void addFormule(String formule) {
        formules.add(formule);
    }


    /**
     * Remove a formule at a given index
     * @param index
     * @return the formule removed
     */
    public String removeFormule(int index) {
        return formules.remove(index);
    }


}
