/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

/**
 *
 * @author Skander
 */
public class SolverSelection {
    public enum SolverType {
            SAT4J, 
            QF_LRA, 
            QF_LIA, 
            QF_RDL, 
            QF_IDL
    };
    
    private SolverType selectedSolver = SolverType.SAT4J;
    
    public SolverSelection() {
        
    }

    public SolverType getSelectedSolver() {
        return selectedSolver;
    }
    
    public void setSelectedSolver(SolverType solverType) {
        this.selectedSolver = solverType;
    }    
    
    public SolverType[] getSupportedSolvers() {
        return SolverType.values();
    }
}
