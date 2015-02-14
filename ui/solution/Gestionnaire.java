/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import solution.solvers.SolverSAT;
import translation.ResultatOcaml;
import translation.Traducteur;

/**
 *
 * @author Skander
 */
public class Gestionnaire {
    Traducteur traducteur;
    Models models;
    ResultatOcaml ocaml;
    SolverSAT solverSAT;
    
    public void preparation(BaseDeClauses clauses) {
        
    }
    
    public Model computeModel() {
        return new Model();
    }
}
