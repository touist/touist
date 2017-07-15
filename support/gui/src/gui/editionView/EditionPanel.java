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

import gui.AbstractComponentPanel;
import gui.TranslatorLatex.TranslationLatex;
import gui.editionView.editor.Editor;
import gui.editionView.editor.ErrorParser;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.*;

import javax.swing.JLabel;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.ui.rsyntaxtextarea.parser.Parser;
import org.fife.ui.rtextarea.RTextScrollPane;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

/**
 *
 * @author Skander
 */
public class EditionPanel extends AbstractComponentPanel {

    private Editor editorTextArea;

	private ErrorParser parser;
    private int rightPanelWidth;
    private JLabel latexLabel;
    private int zoom = 0;
    
    /**
     * Creates new form EditorPanel
     */
    
    public void reparseLatex() {
	    
		try {
			TranslationLatex tr = new TranslationLatex("", this.getEditorTextArea().getSyntaxEditingStyle(), true);
			setLatex(tr.getFormula());
		} catch (Exception e) {
			System.out.println("reparseLatex(): could not parse to latex");
		}
    }
    
    public void setLatex(String latex) {
    	TeXFormula formula = new TeXFormula(latex);
	    TeXIcon ti = formula.createTeXIcon(TeXConstants.ALIGN_TOP, 20+zoom);
	    latexLabel.setIcon(ti);
    }
    
    public void zoom(int step) {
        zoom += step;
        zoom = Math.min(200,zoom);
        zoom = Math.max(-19,zoom);
        reparseLatex();
    }    
    
    class ScaleLatexListener implements MouseWheelListener {
        
        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
            if(e.paramString().contains("modifiers=Ctrl")) {
                zoom(-e.getWheelRotation());
            }
            else {
                latexScroller.getMouseWheelListeners()[0].mouseWheelMoved(e);
            }
        }
        
    }
    
    public EditionPanel() {
        initComponents();
        // Editor textArea set-up
        try {
             editorTextArea = new Editor();
             editorTextArea.addParser(parser = new ErrorParser(this));
             editorTextArea.setSyntaxEditingStyle("sat");
        }
        catch (IOException e) {
            System.err.println("Erreur lancement éditeur");
        }
        
        latexView.addMouseWheelListener(new ScaleLatexListener());
        
        latexView.setLayout(new FlowLayout());
        latexView.add(latexLabel = new JLabel(),FlowLayout.LEFT);
        latexLabel.setVisible(true);


        editorTextArea.addCaretListener(new CaretListener() {

            @Override
            public void caretUpdate(CaretEvent e) {
                // +1 car par défaut, on compte à partir de 0.
                ((ParentEditionPanel)getParent()).setJLabelCaretPositionText(
                        (editorTextArea.getCaretLineNumber() + 1)
                        + ":"
                        + (editorTextArea.getCaretOffsetFromLineStart() + 1)
                );
            }
        });
        
        
        RTextScrollPane sp = new RTextScrollPane(editorTextArea);
        sp.setLineNumbersEnabled(true);
        sp.setFoldIndicatorEnabled(true);
        editorContainer.add(sp, BorderLayout.CENTER);
        
        snippetsContainer.setEditorTextArea(editorTextArea);  
        codeAndLatexView.setResizeWeight(0.5);
        snippetsAndCodeAndLatex.setDividerSize(3);
    }
    
    public void initPalette() {
        snippetsContainer.initPaletteContent();
        snippetsAndCodeAndLatex.setDividerLocation(120);
    }

    public String getText() {
        return editorTextArea.getText();
    }
    
    public void setText(String text) {
        editorTextArea.setText(text);
    }

    /**
     * Import the list of formules from a file
     * @param path of the file containing the list of formules and sets
     * @throws IOException if any I/O exception occurs during file reading
     */
    public static String open(String path) throws IOException {
        String text = "";
        BufferedReader in = new BufferedReader(new FileReader(path));
        String line;
        while((line = in.readLine()) != null) {
            text = text.concat(line + "\n");
        }
        return text;
    }

    public void loadIntoTextEditor(String path) throws IOException {
        setText(open(path));
    }

    public void saveToFile(String path) throws IOException {
        int sizeBuffer = 8192;
        BufferedWriter writer = new BufferedWriter(new FileWriter(path), sizeBuffer);
        writer.write(this.getText());
        writer.flush();
        writer.close();
    }
    
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        snippetsAndCodeAndLatex = new javax.swing.JSplitPane();
        codeAndLatexView = new javax.swing.JSplitPane();
        editorContainer = new javax.swing.JPanel();
        latexScroller = new javax.swing.JScrollPane();
        latexView = new javax.swing.JPanel();
        snippetsScroller = new javax.swing.JScrollPane();
        snippetsContainer = new gui.editionView.SnippetContainer(editorTextArea);

        codeAndLatexView.setDividerLocation(400);
        codeAndLatexView.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        codeAndLatexView.setOneTouchExpandable(true);

        editorContainer.setLayout(new java.awt.BorderLayout());
        codeAndLatexView.setLeftComponent(editorContainer);

        javax.swing.GroupLayout latexViewLayout = new javax.swing.GroupLayout(latexView);
        latexView.setLayout(latexViewLayout);
        latexViewLayout.setHorizontalGroup(
            latexViewLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 324, Short.MAX_VALUE)
        );
        latexViewLayout.setVerticalGroup(
            latexViewLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 495, Short.MAX_VALUE)
        );

        latexScroller.setViewportView(latexView);

        codeAndLatexView.setRightComponent(latexScroller);

        snippetsAndCodeAndLatex.setRightComponent(codeAndLatexView);

        snippetsScroller.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        snippetsScroller.setViewportView(snippetsContainer);

        snippetsAndCodeAndLatex.setLeftComponent(snippetsScroller);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(snippetsAndCodeAndLatex, javax.swing.GroupLayout.DEFAULT_SIZE, 600, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(snippetsAndCodeAndLatex, javax.swing.GroupLayout.Alignment.TRAILING)
        );
    }// </editor-fold>//GEN-END:initComponents
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JSplitPane codeAndLatexView;
    private javax.swing.JPanel editorContainer;
    private javax.swing.JScrollPane latexScroller;
    private javax.swing.JPanel latexView;
    private javax.swing.JSplitPane snippetsAndCodeAndLatex;
    private gui.editionView.SnippetContainer snippetsContainer;
    private javax.swing.JScrollPane snippetsScroller;
    // End of variables declaration//GEN-END:variables

    @Override
    public void updateLanguage() {
        snippetsContainer.updateLanguage();
    }

	public Editor getEditorTextArea() {
		return editorTextArea;
	}

	public ErrorParser getParser() {
		return parser;
	}
}
