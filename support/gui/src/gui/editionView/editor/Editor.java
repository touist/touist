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

package gui.editionView.editor;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.Theme;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;
import org.fife.ui.rtextarea.RTextScrollPane;

/**
 *
 * @author alexis
 */
public class Editor extends RSyntaxTextArea  {
    
    private ArrayList<Integer> snipetsBegin;
    private ArrayList<Integer> snipetsEnd;
    
    class SnippetListener implements DocumentListener {
        
        
        
        @Override
        public void insertUpdate(DocumentEvent e) {
            int debInsert = e.getOffset();
            int len = e.getLength();
            
            for(int i = 0; i < snipetsBegin.size(); i++) {
                int snipetBegin = snipetsBegin.get(i);
                int snipetEnd = snipetsEnd.get(i);
                if(snipetBegin >= debInsert){
                    snipetsBegin.set(i, snipetBegin + len);
                    snipetsEnd.set(i, snipetEnd + len);
                }
                else if(snipetEnd >= debInsert) {
                    snipetsBegin.remove(i);
                    snipetsEnd.remove(i--);
                }
            }
        
        }

        
        
        
        @Override
        public void removeUpdate(DocumentEvent e) {
            int debRemove = e.getOffset();
            int len = e.getLength();
            for(int i = 0; i < snipetsBegin.size(); i++) {
                int snipetBegin = snipetsBegin.get(i);
                int snipetEnd = snipetsEnd.get(i);
                if(snipetBegin <= debRemove + len - 1 && snipetEnd >= debRemove) {
                    snipetsBegin.remove(i);
                    snipetsEnd.remove(i--);
                }
                else if(snipetBegin >= debRemove){
                    snipetsBegin.set(i, snipetBegin - len);
                    snipetsEnd.set(i, snipetEnd - len);
                }
            }
        }

        // WARNING This function is not intercepting characters update at all
        // but lines update. If we change a character to another, remove and insert are
        // called
        @Override
        public void changedUpdate(DocumentEvent e) {
        }
    }
    
    class SnippetRightAction extends AbstractAction {
        
        Editor E;
        
        public SnippetRightAction(Editor E) {
            super();
            this.E = E;
        }
        
        public void actionPerformed(ActionEvent e) {
            int curr = E.getSelectionEnd();
            for(int i = 0; i < snipetsBegin.size(); i++) {
                if(snipetsBegin.get(i) >= curr) {
                    E.setSelectionStart(snipetsBegin.get(i));
                    E.setSelectionEnd(snipetsEnd.get(i)+1);
                    return;
                }
            }
            if(snipetsBegin.size() != 0) {
                E.setSelectionStart(snipetsBegin.get(0));
                E.setSelectionEnd(snipetsEnd.get(0)+1);
            }
            
        }
    }
    
    class SnippetLeftAction extends AbstractAction {
        
        Editor E;
        
        public SnippetLeftAction(Editor E) {
            super();
            this.E = E;
        }
        
        public void actionPerformed(ActionEvent e) {
            int curr = E.getSelectionStart();
            for(int i = snipetsBegin.size()-1; i >= 0; i--) {
                if(snipetsBegin.get(i) < curr) {
                    E.setSelectionStart(snipetsBegin.get(i));
                    E.setSelectionEnd(snipetsEnd.get(i)+1);
                    return;
                }
            }
            if(snipetsBegin.size() != 0) {
                E.setSelectionStart(snipetsBegin.get(snipetsBegin.size()-1));
                E.setSelectionEnd(snipetsEnd.get(snipetsEnd.size()-1)+1);
            }
            
        }
    }
    
    
    
    
    public Editor() throws IOException {
        
        AbstractTokenMakerFactory atmf = (AbstractTokenMakerFactory)TokenMakerFactory.getDefaultInstance();
        
        // Class generated by jflex 
        atmf.putMapping("sat", "gui.editionView.editor.TouistlTokenMaker");
        atmf.putMapping("smt", "gui.editionView.editor.TouistlTokenMaker");
        atmf.putMapping("qbf", "gui.editionView.editor.TouistlTokenMaker");
        this.setSyntaxEditingStyle("sat");
        
        // Defines the color, font police and style of the different tokens
        Theme t = Theme.load(this.getClass().getResourceAsStream("/touistTheme.xml"));
        t.apply(this);
        
        snipetsBegin = new ArrayList<Integer>();
        snipetsEnd = new ArrayList<Integer>();
        
        // Note: I disabled ctrl+left and ctrl+right for moving through snippet tokens
        // because it was disabling the possibilty of moving through the text word by word on
        // Windows.
        
        /*
        this.getDocument().addDocumentListener(new SnippetListener());
        String rightKeyStrokeAndKey = "control RIGHT";
        KeyStroke rightKeyStroke = KeyStroke.getKeyStroke(rightKeyStrokeAndKey);
        this.getInputMap().put(rightKeyStroke, rightKeyStrokeAndKey);
        this.getActionMap().put(rightKeyStrokeAndKey, new SnippetRightAction(this));
        String leftKeyStrokeAndKey = "control LEFT";
        KeyStroke leftKeyStroke = KeyStroke.getKeyStroke(leftKeyStrokeAndKey);
        this.getInputMap().put(leftKeyStroke, leftKeyStrokeAndKey);
        this.getActionMap().put(leftKeyStrokeAndKey, new SnippetLeftAction(this));
        */
        

    }
    
    public void addSnipet(int begin,int end) {
        snipetsBegin.add(begin);
        snipetsEnd.add(end);
        Collections.sort(snipetsBegin);
        Collections.sort(snipetsEnd);
    }
    
    
/* Example of Editor with line displaying in left column */
    
    public static void main(String args[]) {
        JFrame  frame = new JFrame ();
        Editor E = null;
        
        try {
             E = new Editor();
        }
        catch (IOException e) {
            System.err.println("Erreur lancement éditeur");
        }
        frame.add(E, BorderLayout.CENTER);
        frame.setSize(300,100);
        
        BufferedReader in = null ;
        try {
            in = new BufferedReader(new FileReader("compiler/test/foo.touistl"));
            StringBuilder sb = new StringBuilder();
            String line;
            while((line = in.readLine()) != null) {
                sb.append(line+"\n");
            }
            E.setText(sb.toString());
        }
        catch (IOException e) {
            System.err.println("Erreur ouverture du fichier test");
        }
        
        RTextScrollPane sp = new RTextScrollPane(E);
        sp.setLineNumbersEnabled(true);
        sp.setFoldIndicatorEnabled(true);
        
        E.addParser(new ErrorParser());
        
        frame.add(sp, BorderLayout.CENTER);
        frame.setVisible(true);
    }
    
}
