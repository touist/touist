/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/olzd/touist
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser General Public License
 * (LGPL) version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * Contributors:
 *     Alexis Comte, Abdelwahab Heba, Olivier Lezaud,
 *     Skander Ben Slimane, Maël Valais
 *
 */

package solution;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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
    private List<String> sets;

    private static final String errorBeginFormula = "Syntax error at line %d"
                                       + ". Formula begin section already"
                                       + " declared.";
    private static final String errorEndFormula =  "Syntax error at line %d"
                                       + ". No formula section is opened.";
    private static final String errorBeginSet =
                                errorBeginFormula.replace("Formula","Set");
    private static final String errorEndSet =
                                errorEndFormula.replace("formula","set");

    /**
     * Construct a set of clauses
     */
    public BaseDeClauses() {
        formules = new ArrayList<>();
        sets = new ArrayList<>();
    }

    /**
     *
     * @return the list of formules
     */
    public List<String> getFormules() {
        return formules;
    }

    /**
     * @return the list of sets
     */
    public List<String> getSets() {
        return sets;
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
        boolean setSection = false;
        int indexLine = 0;

        while((line = in.readLine()) != null) {

            indexLine ++;

            String []words = line.replace("\\s+"," ").split(" ");
            if(words.length == 2) {
                words[0] = words[0].toLowerCase();
                words[1] = words[1].toLowerCase();


                if(Arrays.equals(words,new String[]{"begin","formula"})) {
                    if(formulaSection == true) {
                        String message = String.format(errorBeginFormula,indexLine);
                        throw new FormatException(message);
                    }
                    else {
                        formulaSection = true;
                    }
                }
                else if(Arrays.equals(words,new String[]{"end","formula"})) {
                    if(formulaSection == false) {
                        String message = String.format(errorEndFormula,indexLine);
                        throw new FormatException(message);
                    }
                    else {
                        formulaSection = false;
                    }
                }
                else if(Arrays.equals(words,new String[]{"begin","sets"})) {
                    if(setSection == true) {
                        String message = String.format(errorBeginSet,indexLine);
                        throw new FormatException(message);
                    }
                    else {
                        setSection = true;
                    }
                }
                else if(Arrays.equals(words,new String[]{"end","sets"})) {
                    if(setSection == false) {
                        String message = String.format(errorEndSet,indexLine);
                        throw new FormatException(message);
                    }
                    else {
                        setSection = false;
                    }
                }

            }
            else if(formulaSection) {
                addFormule(line);
            }
            else if(setSection) {
                addSet(line);
            }
        }
    }

    public void saveToFile(String path) throws IOException {
        int sizeBuffer = 8192;
        BufferedWriter writer = new BufferedWriter(new FileWriter(path), sizeBuffer);

	if(!sets.isEmpty()) {
            writer.write("begins sets\n");
            for(String set:sets) {
                writer.write(set+"\n");
            }
            writer.write("ends sets\n");
        }
        if(!formules.isEmpty()) {
            writer.write("begins formules\n");
            for(String formule:formules) {
                writer.write(formule+"\n");
            }
            writer.write("ends sets\n");
        }

        writer.flush();
        writer.close();
    }

    /**
     * Add a formule to the current list of formules
     * @param A formule
     */

    public void addFormule(String formule) {
        formules.add(formule);
    }

    /**
     * Add a set to the current list of sets
     * @param A set
     */

    public void addSet(String set) {
        sets.add(set);
    }


    /**
     * Remove a formule at a given index
     * @param index
     * @return the formule removed
     */
    public String removeFormule(int index) {
        return formules.remove(index);
    }

    /**
     * Remove a set at a given index
     * @param index
     * @return the set removed
     */
    public String removeSet(int index) {
        return sets.remove(index);
    }

    public static void main(String[] args) {
        System.out.println("essai");
    }

}
