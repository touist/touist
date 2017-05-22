/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import gui.editionView.editor.Editor;

/**
 *
 * @author Skander
 */
public class SolverSelection {
	private MainFrame m;
    public enum SolverType {
            SAT, 
            QF_LRA, 
            QF_LIA, 
            QF_RDL, 
            QF_IDL,
            QBF
    };
    
    private SolverType selectedSolver = SolverType.SAT;
    
    public SolverSelection(MainFrame m) {
    	this.m = m;
    }

    public SolverType getSelectedSolver() {
        return selectedSolver;
    }
    
    public void setSelectedSolver(SolverType solverType) {
        this.selectedSolver = solverType;
        if(selectedSolver == SolverType.SAT) {
        	m.getEditorPanel1().getEditor().getEditorTextArea().setSyntaxEditingStyle("sat");
        } else if(selectedSolver == SolverType.QBF) {
        	m.getEditorPanel1().getEditor().getEditorTextArea().setSyntaxEditingStyle("qbf");
        } else
        	m.getEditorPanel1().getEditor().getEditorTextArea().setSyntaxEditingStyle("smt");
        m.getEditorPanel1().getEditor().getEditorTextArea().forceReparsing(0);
    }    
    
    public SolverType[] getSupportedSolvers() {
        return SolverType.values();
    }
}
