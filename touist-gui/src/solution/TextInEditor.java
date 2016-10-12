/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/touist/touist
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

public class TextInEditor {
    private String textInEditor;

    /**
     * Construct a set of clauses
     */
    public TextInEditor() {
        textInEditor = new String();
    }

    /**
     *
     * @return the list of formules
     */
    public String get() {
        return textInEditor;
    }

    public void set(String text) {
        this.textInEditor = text;
    }
    

    /**
     * Import the list of formules from a file
     * @param path of the file containing the list of formules and sets
     * @throws FileNotFoundException if the path of the file is invalid
     * @throws IOException if any I/O exception occurs during file reading
     * @throws FormatException if the filedefines incorrectly the formula
     *                         section
     */
    public void loadFile(String path) throws FileNotFoundException,
                                               IOException,
                                               FormatException {
        BufferedReader in = new BufferedReader(new FileReader(path));
        String line;
        while((line = in.readLine()) != null) {
            System.out.println(line);
            String []words = line.replace("\\s+"," ").split(" ");
            textInEditor = textInEditor.concat(words + "\n");
        }
    }

    public void saveToFile(String path) throws IOException {
        int sizeBuffer = 8192;
        BufferedWriter writer = new BufferedWriter(new FileWriter(path), sizeBuffer);
        writer.write(this.get());
        writer.flush();
        writer.close();
    }
}
