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

package gui;

import gui.menu.ResultsMenuBar;
import gui.menu.EditionMenuBar;
import entity.Model;
import gui.editionView.ParentEditionPanel;
import gui.resultsView.ResultsPanel;

import java.awt.CardLayout;
import java.io.IOException;
import java.util.ListIterator;
import java.util.Locale;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

import solution.ModelList;
import solution.Solver;
import touist.TouistProperties;
import translation.TranslatorSAT;
import translation.TranslatorSMT;

/**
 *
 * @author Skander
 */
public class MainFrame extends javax.swing.JFrame {
    private TranslatorSAT translatorSAT = new TranslatorSAT();
    private TranslatorSMT translatorSMT = new TranslatorSMT();
    private Solver solver;
    private ModelList models;
    private SolverSelection solverSelection = new SolverSelection(this);

    public State state;
    private String defaultDirectoryPath = ".";

    public final static String EDITOR_PANEL = "editor_panel";
    public final static String RESULTS_PANEL = "results_panel";
    private JPanel cards;
    private ParentEditionPanel editorPanel1;
    private ResultsPanel resultsPanel1;
    private Lang lang;
    
    private ResultsMenuBar resultsMenuBar;
    private EditionMenuBar editionMenuBar;
    
    public Lang getLang() {
        return lang;
    }
    
    public void setDefaultDirectoryPath(String path) {
        defaultDirectoryPath = path;
        //TODO save the path in a config file
    }

    /**
     * Creates new form MainFrame
     */
    public MainFrame() {
        initComponents();
        lang = new Lang(Locale.getDefault());
        
    	cards = new JPanel(new CardLayout());
    	editorPanel1 = new ParentEditionPanel(this);
        resultsPanel1 = new ResultsPanel();
        resultsMenuBar = new ResultsMenuBar(this);
        editionMenuBar = new EditionMenuBar(this);
        
        state = State.EDITION;
        add(cards);
        cards.add(editorPanel1, EDITOR_PANEL);
        cards.add(resultsPanel1, RESULTS_PANEL);
        setViewToEditor();

        editorPanel1.updateComboBoxSelectedSolver();
        
        try {
            setIconImage(ImageIO.read(this.getClass().getResourceAsStream("/images/logo64.png")));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
        this.setJMenuBar(editionMenuBar);
        updateLanguage();
    }
    
    public void updateLanguage() {
        editorPanel1.updateLanguage();
        resultsPanel1.updateLanguage();
        resultsMenuBar.updateLanguage();
        editionMenuBar.updateLanguage();
    }

    public Solver getSolver() {
        return solver;
    }

    public void setSolver(Solver solver) {
        this.solver = solver;
    }

    public ModelList getModelList() {
        return models;
    }

    public void setModels(ModelList models) {
        this.models = models;
    }

    public TranslatorSAT getTranslatorSAT() {
        return translatorSAT;
    }
    
    public TranslatorSMT getTranslatorSMT() {
        return translatorSMT;
    }
    
    public ResultsPanel getResultsPanel1() {
        return resultsPanel1;
    }
    
    public ParentEditionPanel getEditorPanel1() {
        return editorPanel1;
    }

    public SolverSelection getSolverSelection() {
        return solverSelection;
    }
    
    public void updateResultsPanelIterator(ListIterator<Model> iter) {
        resultsPanel1.updateIterator(iter);
    }

    public void setViewToEditor() {
        this.setJMenuBar(editionMenuBar);
        ((CardLayout)cards.getLayout()).show(cards, EDITOR_PANEL);
    }

    public void setViewToResults() {
        this.setJMenuBar(resultsMenuBar);
        resultsPanel1.applyRestrictions();
        ((CardLayout)cards.getLayout()).show(cards, RESULTS_PANEL);
    }
    
    public void setResultView(Model m) {
        resultsPanel1.setActModel(m);
        resultsPanel1.setResult();
    }
    
    public void setResultsPanelEmpty() {
       resultsPanel1.setEmpty();
    }
    
    public void setLanguage(Locale language) {
        lang.setLanguage(language);
        this.repaint();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setMinimumSize(new java.awt.Dimension(800, 600));
        getContentPane().setLayout(new java.awt.CardLayout());

        pack();
    }// </editor-fold>//GEN-END:initComponents

    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
			public void run() {
                new MainFrame().setVisible(true);
                
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
}