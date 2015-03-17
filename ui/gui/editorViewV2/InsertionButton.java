/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.editorViewV2;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JTextArea;

/**
 *
 * @author Skander
 */
public class InsertionButton extends JButton {
    
    private final JTextArea textArea;
    private final String codeToInsert;
    
    public InsertionButton(JTextArea text, String codeToInsert) {
        this.textArea = text;
        this.codeToInsert = codeToInsert;
        
        this.setText(codeToInsert);
        
        addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                insertAtCaret(codeToInsert);
            }
        });
        
        this.setFocusable(false);
        this.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        this.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    }
    
    public InsertionButton(JTextArea text, String codeToInsert, String aide) {
        this(text, codeToInsert);
        setToolTipText(aide);
        
    }
    
    
    /**
     * Insert text at the caret position in the textArea.
     * @param text 
     */
    private void insertAtCaret(String text) {
        if (textArea.hasFocus()) {
            String newText = textArea.getText().substring(0, textArea.getCaretPosition())
                    + text
                    + textArea.getText().substring(textArea.getCaretPosition());
            textArea.setText(newText);
        }
        //TODO update latex schematic area
    }
}
