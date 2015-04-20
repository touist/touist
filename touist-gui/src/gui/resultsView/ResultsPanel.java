/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.resultsView;

import entity.Literal;
import entity.Model;
import gui.AbstractComponentPanel;
import gui.Lang;
import gui.State;
import java.awt.Color;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import java.util.ListIterator;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import solution.NotSatisfiableException;
import solution.SolverExecutionException;

/**
 *
 * @author Skander
 */
public class ResultsPanel extends AbstractComponentPanel {

    class RegexListener implements DocumentListener {
        
         @Override
        public void insertUpdate(DocumentEvent e) {
            setResult();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            setResult();
        }
        
        @Override
        public void changedUpdate(DocumentEvent e) {
            setResult();
        }
    }
    
    class ResultTableCellRenderer extends DefaultTableCellRenderer {
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            Component composant =  super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            if(row%2 == 0){
                composant.setBackground(Color.WHITE);
            }
            else {
                composant.setBackground(new Color(236,247,249));
            }
            return composant;
        }
    }
    
    class ResultTableModel extends DefaultTableModel
    {
        public ResultTableModel(Object []o, int n){
            super(o,n);
        }
        @Override
        public boolean isCellEditable(int rowIndex,int columnIndex){
		return false;
	}
    }
    
    private int currentModelIndex = 0;
    ListIterator<Model> iter;
    Model actModel;
    ExportDialog exportDialog;

    /**
     * Creates new form ResultsPanel
     */
    public ResultsPanel() {
        exportDialog = new ExportDialog();
        initComponents();
    }

    /**
     * Update the models iterator
     */
    public void updateIterator(ListIterator<Model> iter) {
        this.iter = iter;
    }
    
    public void setActModel(Model m) {
        actModel = m;
    }

    public void setResult() {
        
        boolean falseLiterals = falseCheckBox.isSelected();
        boolean trueLiterals = trueCheckBox.isSelected();
        
        String regex = searchTextField.getText();
        Pattern pattern = null;
        try {
            pattern = Pattern.compile(regex);
        } catch (PatternSyntaxException e) {
            regex = "";
        }
        
        
        ResultTableModel model = (ResultTableModel) jTable1.getModel();
        model.setNumRows(0);
        ArrayList<Literal> literals = (ArrayList<Literal>) actModel.literals;
        for(int i = 0; i < literals.size(); i++) {
            String name = literals.get(i).getLiteral();
            boolean value = literals.get(i).isLiteral_positivity();
            
            if(regex!="" && !pattern.matcher(name).find()){
                continue;
            }
            if(falseLiterals && !value){
                model.addRow(new String[]{name,"False"});
            } else if(trueLiterals && value){
                model.addRow(new String[]{name,"True"});
            }
        }
    }
    
    public void setEmpty(){
        ResultTableModel model = (ResultTableModel) jTable1.getModel();
        model.setNumRows(0);
    }
    
    public void exportModel() throws IOException {
        final JFileChooser fc = new JFileChooser();
        fc.setFileFilter(new FileNameExtensionFilter("Text files(txt, text)","txt","text"));
        fc.addChoosableFileFilter(new FileNameExtensionFilter("Latex files(latex)","tex"));
        fc.setAcceptAllFileFilterUsed(false);
        int returnVal = fc.showOpenDialog(this);
        
        if(returnVal == JFileChooser.APPROVE_OPTION){
            String filename = fc.getSelectedFile().getName();
            String extension = (filename.contains(".")?filename.substring(filename.lastIndexOf("."),filename.length()):"txt");
            Object[] options1 = { "Try This Number", "Choose A Random Number",
                "Quit" };
            int result = JOptionPane.showConfirmDialog(null, exportDialog,"Format d'export",JOptionPane.DEFAULT_OPTION);
            if(result == JOptionPane.YES_OPTION){
                StringBuilder sb = new StringBuilder();
                
                String prefix = exportDialog.getPrefixValue();
                String separator = exportDialog.getSeparatorValue();
                String suffix = exportDialog.getSuffixValue();
                System.err.println(prefix);
                
                ArrayList<Literal> literals = (ArrayList<Literal>) actModel.literals;
                for(int i = 0; i < literals.size(); i++) {
                    sb.append(prefix+literals.get(i).getLiteral()+separator+(literals.get(i).isLiteral_positivity()?"true":"false")+suffix+"\n");
                }

                BufferedWriter out = new BufferedWriter(new FileWriter(fc.getSelectedFile().getAbsolutePath()));
                out.write(sb.toString());
                out.close();
            }
            
        }
    }

    /**
     * Enable the next and previous buttons depending on the frame state.
     */
    public void applyRestrictions() {
        switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                jButtonNext.setEnabled(false);
                jButtonPrevious.setEnabled(false);
                break;
            case SINGLE_RESULT :
                jButtonNext.setEnabled(false);
                jButtonPrevious.setEnabled(false);
                break;
            case FIRST_RESULT :
                jButtonNext.setEnabled(true);
                jButtonPrevious.setEnabled(false);
                break;
            case MIDDLE_RESULT :
                jButtonNext.setEnabled(true);
                jButtonPrevious.setEnabled(true);
                break;
            case LAST_RESULT :
                jButtonNext.setEnabled(false);
                jButtonPrevious.setEnabled(true);
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jButtonEditor = new javax.swing.JButton();
        jButtonPrevious = new javax.swing.JButton();
        jButtonNext = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        trueCheckBox = new javax.swing.JCheckBox();
        falseCheckBox = new javax.swing.JCheckBox();
        searchTextField = new javax.swing.JTextField();
        jButtonExport = new javax.swing.JButton();

        setMinimumSize(new java.awt.Dimension(400, 300));

        jLabel1.setText("Résultats");

        jButtonEditor.setText("Retour en édition");
        jButtonEditor.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonEditorActionPerformed(evt);
            }
        });

        jButtonPrevious.setText("Précédent");
        jButtonPrevious.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPreviousActionPerformed(evt);
            }
        });

        jButtonNext.setText("Suivant");
        jButtonNext.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonNextActionPerformed(evt);
            }
        });

        jTable1.setModel(new ResultTableModel(
            new String [] {
                "Name", "Value"
            },0
        ));
        jTable1.setAutoCreateRowSorter(true);
        jTable1.setDefaultRenderer(Object.class, new ResultTableCellRenderer());
        jTable1.setGridColor(Color.BLACK);
        jTable1.setShowGrid(true);
        jTable1.getRowSorter().toggleSortOrder(0);
        jTable1.setRowSelectionAllowed(false);
        jScrollPane2.setViewportView(jTable1);

        trueCheckBox.setSelected(true);
        trueCheckBox.setText("true");
        trueCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                trueCheckBoxActionPerformed(evt);
            }
        });

        falseCheckBox.setSelected(true);
        falseCheckBox.setText("false");
        falseCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                falseCheckBoxActionPerformed(evt);
            }
        });

        searchTextField.getDocument().addDocumentListener(new RegexListener());

        jButtonExport.setText("Exporter");
        jButtonExport.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonExportActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(32, 32, 32)
                        .addComponent(trueCheckBox)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(falseCheckBox)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(searchTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 333, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonEditor))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jButtonPrevious)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonNext)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonExport)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jButtonEditor)
                    .addComponent(trueCheckBox)
                    .addComponent(falseCheckBox)
                    .addComponent(searchTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 291, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonPrevious)
                    .addComponent(jButtonNext)
                    .addComponent(jButtonExport))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonEditorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonEditorActionPerformed
        switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                getFrame().getSolver().close();
                setState(State.EDITION);
                getFrame().setViewToEditor();
                break;
            case SINGLE_RESULT :
                getFrame().getSolver().close();
                setState(State.EDITION);
                getFrame().setViewToEditor();
                break;
            case FIRST_RESULT :
                getFrame().getSolver().close();
                setState(State.EDITION);
                getFrame().setViewToEditor();
                break;
            case MIDDLE_RESULT :
                getFrame().getSolver().close();
                setState(State.EDITION);
                getFrame().setViewToEditor();
                break;
            case LAST_RESULT :
                getFrame().getSolver().close();
                setState(State.EDITION);
                getFrame().setViewToEditor();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        getFrame().setViewToEditor();
        this.updateUI();
    }//GEN-LAST:event_jButtonEditorActionPerformed

    /*
    Afficher le model précédent m
    Si m est le premier
    alors on passe à l'état FIRST_RESULT
    sinon à INTER_RESULT
    */
    private State previousButtonHandler() {
        this.setActModel(iter.previous());
        this.setResult();
        if (iter.hasPrevious()) {
            return State.MIDDLE_RESULT;
        } else {
            return State.FIRST_RESULT;
        }
    }

    private void jButtonPreviousActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonPreviousActionPerformed
        Model m;
        switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                // interdit
                break;
            case SINGLE_RESULT :
                // interdit
                break;
            case FIRST_RESULT :
                // interdit
                break;
            case MIDDLE_RESULT :
                setState(previousButtonHandler());
                applyRestrictions();
                break;
            case LAST_RESULT :
                setState(previousButtonHandler());
                applyRestrictions();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_jButtonPreviousActionPerformed

    /*
    Affiche le model suivant m
    si m est le dernier model de models (la liste des models calculés)
    alors demander au solveur de chercher un autre model
        si le solveur ne trouve pas, passe en état LAST_RESULT
        sinon on passe en INTER_RESULT
    */
    private State nextButtonHandler() {
        this.setActModel(iter.next());
        this.setResult();
        
        if (iter.hasNext()){
            return State.MIDDLE_RESULT;
        } else {
            return State.LAST_RESULT;
        }
    }

    private void jButtonNextActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonNextActionPerformed
        Model m;
        switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                // interdit
                break;
            case SINGLE_RESULT :
                // interdit
                break;
            case FIRST_RESULT :
                setState(nextButtonHandler());
                applyRestrictions();
                break;
            case MIDDLE_RESULT :
                setState(nextButtonHandler());
                applyRestrictions();
                break;
            case LAST_RESULT :
                // interdit
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_jButtonNextActionPerformed

    private void falseCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_falseCheckBoxActionPerformed
        switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                // interdit
                break;
            case SINGLE_RESULT :
                // interdit
                break;
            case FIRST_RESULT :
            case MIDDLE_RESULT :
            case LAST_RESULT :
                this.setResult();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_falseCheckBoxActionPerformed

    private void trueCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_trueCheckBoxActionPerformed
        switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                // interdit
                break;
            case SINGLE_RESULT :
                // interdit
                break;
            case FIRST_RESULT :
            case MIDDLE_RESULT :
            case LAST_RESULT :
                this.setResult();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_trueCheckBoxActionPerformed

    private void jButtonExportActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonExportActionPerformed
         switch(getState()) {
            case EDITION :
                // impossible
                break;
            case EDITION_ERROR :
                // impossible
                break;
            case NO_RESULT :
                // interdit
                break;
            case SINGLE_RESULT :
            case FIRST_RESULT :
            case MIDDLE_RESULT :
            case LAST_RESULT :
                try {
                    this.exportModel();
                } catch(IOException e) {
                    JOptionPane.showMessageDialog(this, "Error during export","Export failure",JOptionPane.ERROR_MESSAGE);
                }
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_jButtonExportActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox falseCheckBox;
    private javax.swing.JButton jButtonEditor;
    private javax.swing.JButton jButtonExport;
    private javax.swing.JButton jButtonNext;
    private javax.swing.JButton jButtonPrevious;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField searchTextField;
    private javax.swing.JCheckBox trueCheckBox;
    // End of variables declaration//GEN-END:variables

    @Override
    public void updateLanguage() {
        jLabel1.setText(getFrame().getLang().getWord(Lang.RESULTS_TEXT));
        jButtonPrevious.setText(getFrame().getLang().getWord(Lang.RESULTS_PREVIOUS));
        jButtonNext.setText(getFrame().getLang().getWord(Lang.RESULTS_NEXT));
        jButtonEditor.setText(getFrame().getLang().getWord(Lang.RESULTS_RETURN));
        jButtonExport.setText(getFrame().getLang().getWord(Lang.RESULTS_EXPORT));
        //trueCheckBox.setText(getFrame().getLang().getWord(Lang.RESULTS_TRUE));
        //falseCheckBox.setText(getFrame().getLang().getWord(Lang.RESULTS_FALSE));
        jTable1.getColumnModel().getColumn(0).setHeaderValue(getFrame().getLang().getWord(Lang.RESULTS_NAME));
        jTable1.getColumnModel().getColumn(1).setHeaderValue(getFrame().getLang().getWord(Lang.RESULTS_VALUE));
    
    }
}
