/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import entity.Model;
import gui.editionView.ParentEditionPanel;
import gui.resultsView.ResultsPanel;

import java.awt.CardLayout;
import java.io.File;
import java.io.IOException;
import java.util.ListIterator;
import java.util.Locale;
import javax.imageio.ImageIO;

import javax.swing.JPanel;

import solution.BaseDeClauses;
import solution.ModelList;
import solution.Solver;
import translation.TranslatorSAT;

/**
 *
 * @author Skander
 */
public class MainFrame extends javax.swing.JFrame {

    private BaseDeClauses clause = new BaseDeClauses();
    private TranslatorSAT translator = new TranslatorSAT("external"+File.separatorChar+"touistc");
    private Solver solver;
    private ModelList models;

    public State state;

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

    /**
     * Creates new form MainFrame
     */
    public MainFrame() {
        
        lang = new Lang(Locale.ENGLISH);
        setLanguage(
        		(lang.getSupportedLanguages().contains(Locale.getDefault()))
        		?Locale.getDefault():Locale.ENGLISH);
        
    	cards = new JPanel(new CardLayout());
    	editorPanel1 = new ParentEditionPanel(this);
        resultsPanel1 = new ResultsPanel(this);
        resultsMenuBar = new ResultsMenuBar(this);
        editionMenuBar = new EditionMenuBar(this);
        
        state = State.EDITION;
        add(cards);
        cards.add(editorPanel1, EDITOR_PANEL);
        cards.add(resultsPanel1, RESULTS_PANEL);
        setViewToEditor();

        initComponents();
        
        try {
            setIconImage(ImageIO.read(this.getClass().getResourceAsStream("/images/logo64.png")));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
        this.setJMenuBar(editionMenuBar);
        updateLanguage();
    }
    
    public void updateLanguage() {
        this.setTitle(lang.getWord(Lang.FRAME_TITLE));
        editorPanel1.updateLanguage();
        resultsPanel1.updateLanguage();
        resultsMenuBar.updateLanguage();
        editionMenuBar.updateLanguage();
    }

    public BaseDeClauses getClause() {
        return clause;
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

    public TranslatorSAT getTranslator() {
        return translator;
    }
    
    public ResultsPanel getResultsPanel1() {
        return resultsPanel1;
    }
    
    public ParentEditionPanel getEditorPanel1() {
        return editorPanel1;
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

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
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