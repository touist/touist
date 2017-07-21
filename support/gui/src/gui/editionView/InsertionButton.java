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

package gui.editionView;

import gui.MainFrame;
import gui.State;
import gui.TranslatorLatex.TranslationLatex;
import gui.editionView.editor.Editor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JButton;

import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

/**
 *
 * @author Skander
 */
public class InsertionButton extends JButton {

    private final Editor editorTextArea;
    private final String codeToInsert;
    private ArrayList<Integer> snipets;
    private final String latex;

    public InsertionButton(Editor editorTextArea, final String codeToInsert, final String latex, ArrayList<Integer> snipets) {
        this.editorTextArea = editorTextArea;
        this.codeToInsert = codeToInsert;
        this.snipets = snipets;
        this.latex = latex;

        this.setContentAreaFilled(false);
        //this.setBorderPainted(false);

        addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                switch(((MainFrame)(getRootPane().getParent())).state) {
                    case EDITION :
                        ((MainFrame)(getRootPane().getParent())).state = State.EDITION;
                        insertAtCaret(codeToInsert + "\n");
                        break;
                    case EDITION_ERROR :
                        ((MainFrame)(getRootPane().getParent())).state = State.EDITION_ERROR;
                        insertAtCaret(codeToInsert + "\n");
                        break;
                    case NO_RESULT :
                        // impossible
                        break;
                    case SINGLE_RESULT :
                        // impossible
                        break;
                    case FIRST_RESULT :
                        // impossible
                        break;
                    case MIDDLE_RESULT :
                        // impossible
                        break;
                    case LAST_RESULT :
                        // impossible
                        break;
                    default :
                        System.out.println("Undefined action set for the state : " + ((MainFrame)(getRootPane().getParent())).state);
                }
            }
        });

        try {
            TeXFormula formula = new TeXFormula(this.latex);
            TeXIcon ti = formula.createTeXIcon(TeXConstants.ALIGN_TOP, 15);
            this.setIcon(ti);
        } catch (Exception ex) {
            System.err.println("Erreur lors de la traduction dun bouton "+codeToInsert);
        }


        this.setFocusable(false);
        this.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        this.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    }

    public InsertionButton(Editor editorTextArea, String codeToInsert, final String latex, ArrayList<Integer> snipets, String aide) {
        this(editorTextArea, codeToInsert, latex, snipets);
        setToolTipText(aide);

    }

    public InsertionButton(Editor editorTextArea, String codeToInsert, final String latex, ArrayList<Integer> snipets, String aide, String latexFormula) {
        this(editorTextArea, codeToInsert, latex, snipets,aide);
        TeXFormula formula = new TeXFormula(latexFormula);
        TeXIcon ti = formula.createTeXIcon(TeXConstants.ALIGN_TOP, 15);
        this.setIcon(ti);
    }


    /**
     * Insert text at the caret position in the textArea.
     * @param text
     */
    private void insertAtCaret(String text) {
        Integer caretPosition = editorTextArea.getCaretPosition();

        // insert is better than setText: setText entirely remove previous text then make an insert operation
        editorTextArea.insert(text, caretPosition);

        for(int snippetBegin = 0; snippetBegin < snipets.size(); snippetBegin+=2) {
            int snippetEnd = snippetBegin + 1;
            editorTextArea.addSnipet(caretPosition+snipets.get(snippetBegin),caretPosition+snipets.get(snippetEnd));
        }
    }
}
