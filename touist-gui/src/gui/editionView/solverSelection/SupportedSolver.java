/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.editionView.solverSelection;

import solution.BaseDeClauses;
import solution.Solver;
import translation.Translator;

/**
 *
 * @author Skander
 */
public enum SupportedSolver {
    SAT ("SAT", "Basic Solver."), 
    SMT ("SMT", "Support Arithmetic operations.");
    
    private String name;
    private String description;

    private SupportedSolver(String name, String description) {
        this.name = name;
        this.description = description;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }
    
    // is s needed in parameter ?
    public void solve(BaseDeClauses b, Translator t, Solver s) {
        switch(this) {
            case SAT :
                //TODO
                break;
            case SMT :
                //TODO
                break;
            default:
        }
    }
}
