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

package gui.resultsView;

import entity.Literal;
import entity.Model;
import gui.AbstractComponentPanel;
import gui.Lang;
import gui.State;

import java.awt.Color;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.File;
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

    public void setShowOthersCheckbox(boolean b) {
        showOtherLiterals.setEnabled(b);
    }

    /**
     * Creates new form ResultsPanel
     */
    public ResultsPanel() {
        exportDialog = new ExportDialog();
        initComponents();
        literalsTable.setCellSelectionEnabled(true);
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

    /**
     * This method will be called when
     *   1) the 'regex' text field is modified
     *   2) the 'true' or 'false' check boxes are modified
     */
    public void setResult() {
        
        boolean falseLiterals = showFalseLiterals.isSelected();
        boolean trueLiterals = showTrueLiterals.isSelected();
        boolean otherLiterals = showOtherLiterals.isSelected();
        
        String regex = filterLiterals.getText();
        Pattern pattern = null;
        boolean useRegex;
        try {
            pattern = Pattern.compile(regex);
            useRegex = true;
        } catch (PatternSyntaxException e) {
            useRegex = false;
        }
        
        String trueText = getFrame().getLang().getWord("ResultsPanel.trueText");
        String falseText = getFrame().getLang().getWord("ResultsPanel.falseText");
        
        ResultTableModel model = (ResultTableModel) literalsTable.getModel();
        model.setNumRows(0);
        ArrayList<Literal> literals = (ArrayList<Literal>) actModel.literals;
        for(int i = 0; i < literals.size(); i++) {
            String name = literals.get(i).getLiteral();
            if (useRegex && !pattern.matcher(name).find()) {
                continue;
            }

            // Case 1. Literal values are stored as booleans
            if (literals.get(i).getArithmetic_value()==null){
                boolean value = literals.get(i).isLiteral_positivity();
                if (falseLiterals && (value == false)) model.addRow(new String[]{name, falseText});
                else if (trueLiterals && (value == true)) model.addRow(new String[]{name, trueText});
            }

            // Case 2. Literal values are stored as strings
            else {
                String value = literals.get(i).getArithmetic_value();
                if (value.equals("0")) {
                    if (falseLiterals) model.addRow(new String[]{name, falseText});
                }
                else if (value.equals("1")) {
                    if (trueLiterals) model.addRow(new String[]{name, trueText});
                }
                else {
                    if (otherLiterals) model.addRow(new String[]{name, value});
                }
            }
        }
    }
    
    public void setEmpty(){
        ResultTableModel model = (ResultTableModel) literalsTable.getModel();
        model.setNumRows(0);
    }
    
    public void exportModel() throws IOException {
        final JFileChooser fc = new JFileChooser(new File(System.getProperty("user.dir")));
        fc.setFileFilter(new FileNameExtensionFilter("Text files(txt, text)","txt","text"));
        fc.addChoosableFileFilter(new FileNameExtensionFilter("Latex files(latex)","tex"));
        fc.setAcceptAllFileFilterUsed(false);
        int returnVal = fc.showDialog(this,getFrame().getLang().getWord(Lang.RESULTS_FILE_CHOOSER));
        
        String trueText = getFrame().getLang().getWord("ResultsPanel.trueText");
        String falseText = getFrame().getLang().getWord("ResultsPanel.falseText");
        
        if(returnVal == JFileChooser.APPROVE_OPTION){
            String filename = fc.getSelectedFile().getName();
            String extension = (filename.contains(".")?filename.substring(filename.lastIndexOf("."),filename.length()):"txt");
            int result = JOptionPane.showConfirmDialog(null, exportDialog,"Format d'export",JOptionPane.DEFAULT_OPTION);
            if(result == JOptionPane.YES_OPTION){
                StringBuilder sb = new StringBuilder();
                
                String prefix = exportDialog.getPrefixValue();
                String separator = exportDialog.getSeparatorValue();
                String suffix = exportDialog.getSuffixValue();
                
                
                ArrayList<Literal> literals = (ArrayList<Literal>) actModel.literals;
                for(int i = 0; i < literals.size(); i++) {
                    String left = exportDialog.getLeftValue()=="litteral"?literals.get(i).getLiteral():(literals.get(i).isLiteral_positivity()?trueText:falseText);
                    String right = exportDialog.getRightValue()=="litteral"?literals.get(i).getLiteral():(literals.get(i).isLiteral_positivity()?trueText:falseText);
                    sb.append(prefix+left+separator+right+suffix+"\n");
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
                nextModel.setEnabled(false);
                previousModel.setEnabled(false);
                break;
            case SINGLE_RESULT :
                nextModel.setEnabled(false);
                previousModel.setEnabled(false);
                break;
            case FIRST_RESULT :
                nextModel.setEnabled(true);
                previousModel.setEnabled(false);
                break;
            case MIDDLE_RESULT :
                nextModel.setEnabled(true);
                previousModel.setEnabled(true);
                break;
            case LAST_RESULT :
                nextModel.setEnabled(false);
                previousModel.setEnabled(true);
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

        resultsLabel = new javax.swing.JLabel();
        backToEditor = new javax.swing.JButton();
        previousModel = new javax.swing.JButton();
        nextModel = new javax.swing.JButton();
        literals = new javax.swing.JScrollPane();
        literalsTable = new javax.swing.JTable();
        showTrueLiterals = new javax.swing.JCheckBox();
        showFalseLiterals = new javax.swing.JCheckBox();
        filterLiterals = new javax.swing.JTextField();
        exportModel = new javax.swing.JButton();
        showOtherLiterals = new javax.swing.JCheckBox();

        setMinimumSize(new java.awt.Dimension(400, 300));

        resultsLabel.setText("Résultats");

        backToEditor.setText("Retour en édition");
        backToEditor.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                backToEditorActionPerformed(evt);
            }
        });

        previousModel.setText("Précédent");
        previousModel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                previousModelActionPerformed(evt);
            }
        });

        nextModel.setText("Suivant");
        nextModel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                nextModelActionPerformed(evt);
            }
        });

        literalsTable.setModel(new ResultTableModel(
            new String [] {
                "Name", "Value"
            },0
        ));
        literalsTable.setSelectionForeground(new java.awt.Color(0, 51, 204));
        literalsTable.setAutoCreateRowSorter(true);
        literalsTable.setDefaultRenderer(Object.class, new ResultTableCellRenderer());
        literalsTable.setGridColor(Color.BLACK);
        literalsTable.setShowGrid(true);
        literalsTable.getRowSorter().toggleSortOrder(0);
        literalsTable.setRowSelectionAllowed(false);
        literals.setViewportView(literalsTable);

        showTrueLiterals.setSelected(true);
        showTrueLiterals.setText("true");
        showTrueLiterals.setPreferredSize(new java.awt.Dimension(57, 16));
        showTrueLiterals.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showTrueLiteralsActionPerformed(evt);
            }
        });

        showFalseLiterals.setSelected(true);
        showFalseLiterals.setText("false");
        showFalseLiterals.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showFalseLiteralsActionPerformed(evt);
            }
        });

        filterLiterals.getDocument().addDocumentListener(new RegexListener());

        exportModel.setText("Export");
        exportModel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportModelActionPerformed(evt);
            }
        });

        showOtherLiterals.setSelected(true);
        showOtherLiterals.setText("others");
        showOtherLiterals.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showOtherLiteralsActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(literals)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(resultsLabel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(showTrueLiterals, javax.swing.GroupLayout.PREFERRED_SIZE, 57, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(showFalseLiterals)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(showOtherLiterals)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(filterLiterals, javax.swing.GroupLayout.DEFAULT_SIZE, 281, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(backToEditor))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(previousModel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(nextModel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(exportModel)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(resultsLabel)
                    .addComponent(backToEditor)
                    .addComponent(showTrueLiterals, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(showFalseLiterals)
                    .addComponent(filterLiterals, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(showOtherLiterals))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(literals, javax.swing.GroupLayout.DEFAULT_SIZE, 291, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(previousModel)
                    .addComponent(nextModel)
                    .addComponent(exportModel))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private void backToEditorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_backToEditorActionPerformed
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
    }//GEN-LAST:event_backToEditorActionPerformed

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

    private void previousModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_previousModelActionPerformed
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
    }//GEN-LAST:event_previousModelActionPerformed

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

    private void nextModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nextModelActionPerformed
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
    }//GEN-LAST:event_nextModelActionPerformed

    private void showFalseLiteralsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showFalseLiteralsActionPerformed
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
                this.setResult();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_showFalseLiteralsActionPerformed

    private void showTrueLiteralsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showTrueLiteralsActionPerformed
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
                this.setResult();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_showTrueLiteralsActionPerformed

    private void showOtherLiteralsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showOtherLiteralsActionPerformed
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
                this.setResult();
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
        this.updateUI();
    }//GEN-LAST:event_showOtherLiteralsActionPerformed

    private void exportModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportModelActionPerformed
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
    }//GEN-LAST:event_exportModelActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton backToEditor;
    private javax.swing.JButton exportModel;
    private javax.swing.JTextField filterLiterals;
    private javax.swing.JScrollPane literals;
    private javax.swing.JTable literalsTable;
    private javax.swing.JButton nextModel;
    private javax.swing.JButton previousModel;
    private javax.swing.JLabel resultsLabel;
    private javax.swing.JCheckBox showFalseLiterals;
    private javax.swing.JCheckBox showOtherLiterals;
    private javax.swing.JCheckBox showTrueLiterals;
    // End of variables declaration//GEN-END:variables

    @Override
    public void updateLanguage() {
        resultsLabel.setText(getFrame().getLang().getWord(Lang.RESULTS_TEXT));
        previousModel.setText(getFrame().getLang().getWord(Lang.RESULTS_PREVIOUS));
        nextModel.setText(getFrame().getLang().getWord(Lang.RESULTS_NEXT));
        backToEditor.setText(getFrame().getLang().getWord(Lang.RESULTS_RETURN));
        exportModel.setText(getFrame().getLang().getWord(Lang.RESULTS_EXPORT));
        exportModel.setToolTipText(getFrame().getLang().getWord("ResultsPanel.jButtonExport.tooltip"));
        showTrueLiterals.setText(getFrame().getLang().getWord("ResultsPanel.trueText"));
        showFalseLiterals.setText(getFrame().getLang().getWord("ResultsPanel.falseText"));
        literalsTable.getColumnModel().getColumn(0).setHeaderValue(getFrame().getLang().getWord(Lang.RESULTS_NAME));
        literalsTable.getColumnModel().getColumn(1).setHeaderValue(getFrame().getLang().getWord(Lang.RESULTS_VALUE));
        showFalseLiterals.setText(getFrame().getLang().getWord("ResultsPanel.falseText"));
        showTrueLiterals.setText(getFrame().getLang().getWord("ResultsPanel.trueText"));
        showOtherLiterals.setText(getFrame().getLang().getWord("ResultsPanel.othersText"));
        filterLiterals.setToolTipText(getFrame().getLang().getWord("ResultsPanel.searchTextField.tooltip"));
    }
}
