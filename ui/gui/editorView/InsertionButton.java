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

package gui.editorView;

import gui.MainFrame;
import gui.State;


/**
 * Cette classe représente un bouton qui permet d'insérer du texteScheme
 * dans un champ Scheme
 * Par exemple, le bouton pour insérer "(<phi> and <psi>)"
 * @author François Schwarzentruber
 */
public class InsertionButton extends javax.swing.JButton  {

    /**
     * the text the button inserts when we clicked
     */
    private final String schemeCodeToInsert;

    /**
     * the container of FormulaPanels that are looked and if one
     * of them is under focus, the text will be inserted in it.
     */
    private final FormulaTablePanel formulaTablePane;




    /**
     *
     * @param formulaTablePane
     * @param texteScheme
     */
    public InsertionButton(FormulaTablePanel formulaTablePane, String texteScheme) {
        this.formulaTablePane = formulaTablePane;
        this.schemeCodeToInsert = texteScheme;
        try
        {
            // TODO fix this
            //this.setIcon(LaTEX.latexCodeToImageIcon(FormulaPanel.formulaSchemeStringToLatexCode(texteScheme)));
            this.setText(texteScheme);
        }
        catch(Exception e)
        {
            System.out.println("Erreur d'insertion de bouton dans la palette : problème avec le code latex de "+ texteScheme);
        }
         addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                switch(((MainFrame)getRootPane().getParent()).state) {
                    case EDIT_SINGLE :
                        ((MainFrame)getRootPane().getParent()).state = State.EDIT_SINGLE;
                        inserer(schemeCodeToInsert);
                        break;
                    case EDIT_MULTIPLE :
                        ((MainFrame)getRootPane().getParent()).state = State.EDIT_MULTIPLE;
                        inserer(schemeCodeToInsert);
                        break;
                    case SINGLE_RESULT :
                        // impossible
                        break;
                    case FIRST_RESULT :
                        // impossible
                        break;
                    case INTER_RESULT :
                        // impossible
                        break;
                    case LAST_RESULT :
                        // impossible
                        break;
                    default :
                        System.out.println("Undefined action set for the state : " + ((MainFrame)getRootPane().getParent()).state);
                }
            }
        });
        this.setFocusable(false);
        this.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        //this.setName("bigor"); // NOI18N
        this.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        // this.setMaximumSize(new Dimension(this.getIcon().getIconWidth(),this.getIcon().getIconHeight()));
        // this.setPreferredSize(new Dimension(this.getIcon().getIconWidth(),this.getIcon().getIconHeight()));
    }


    /**
     * Insert the text in the focused textField.
     * @param texte text to insert in the focused textField at the caret position
     */
    private void inserer(String text)
    {
        FormulaPanel panel = formulaTablePane.getFocusedFormulaPanel();
        if (panel != null) {
            panel.insertAtCaret(text);
        }
    }


    public InsertionButton(FormulaTablePanel formulaTablePanel, String texteScheme, String aide) {
        this(formulaTablePanel, texteScheme);
        setToolTipText(aide);

    }








}
