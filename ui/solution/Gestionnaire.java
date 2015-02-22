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
 * This class is the high-level launcher for translation
 * into DIMACS and then solving using the external solveur.
 * @author Maël
 */
public class Gestionnaire {
    Traducteur traducteur;
    Models models;
    ResultatOcaml ocaml;
    SolverSAT solverSAT;

    /**
     * Translates the ClauseBase using the Traducteur
     * and producing a ResultatOcaml object.
     * @param clauses
     */
    public void preparation(BaseDeClauses clauses) {
	// 1. Save the ClauseBase into a temporary .bigand file
	clauses.saveToFile("clauses");
	// 2. Call the translator (bigand to dimacs)
	traducteur = new Traducteur();
	traducteur.appelTraducteurOcaml(); // XXX How does it work?
    }

    /**
     * Asks the external solver if this DIMACS file has models
     * @return the next model, NULL if no model exists
     * @note pour le moment, fait appel a calculerModelSAT.
     * Plus tard on pourra préciser le solveur à utiliser.
     */
    public Model computeModel() {
        return new Model();
    }
}
