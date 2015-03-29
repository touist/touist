/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.editionView;

import gui.MainFrame;
import gui.State;
import gui.editionView.editor.Editor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import javax.swing.JButton;

/**
 *
 * @author Skander
 */
public class InsertionButton extends JButton {
    
    private final Editor editorTextArea;
    private final String codeToInsert;
    private ArrayList<Integer> snipets;
    
    public InsertionButton(Editor editorTextArea, String codeToInsert, ArrayList<Integer> snipets) {
        this.editorTextArea = editorTextArea;
        this.codeToInsert = codeToInsert;
        this.snipets = snipets;
        
        this.setText(codeToInsert);
        
        addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                switch(((MainFrame)(getRootPane().getParent())).state) {
                    case EDITION :
                        ((MainFrame)(getRootPane().getParent())).state = State.EDITION;
                        insertAtCaret(codeToInsert);
                        break;
                    case EDITION_ERROR :
                        ((MainFrame)(getRootPane().getParent())).state = State.EDITION_ERROR;
                        insertAtCaret(codeToInsert);
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
        
        this.setFocusable(false);
        this.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        this.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    }
    
    public InsertionButton(Editor editorTextArea, String codeToInsert, ArrayList<Integer> snipets, String aide) {
        this(editorTextArea, codeToInsert, snipets);
        setToolTipText(aide);
        
    }
    
    
    /**
     * Insert text at the caret position in the textArea.
     * @param text 
     */
    private void insertAtCaret(String text) {
        if (editorTextArea.hasFocus()) {
            
            Integer caretPosition = editorTextArea.getCaretPosition();
            
            // insert is better than setText: setText entirely remove previous text then make an insert operation
            editorTextArea.insert(text, caretPosition);

            for(int snippetBegin = 0; snippetBegin < snipets.size(); snippetBegin+=2) {
                int snippetEnd = snippetBegin + 1;
                editorTextArea.addSnipet(caretPosition+snipets.get(snippetBegin),caretPosition+snipets.get(snippetEnd));
            }
        }
        //TODO update latex schematic area
    }
}
