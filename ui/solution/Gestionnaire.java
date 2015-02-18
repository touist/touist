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
 * Une instance Gestionnaire est le "lanceur" de haut niveau
 * de la traduction puis de la résolution.
 * @author Maël
 */
public class Gestionnaire {
    Traducteur traducteur;
    Models models;
    ResultatOcaml ocaml;
    SolverSAT solverSAT;

    /**
     * Traduit la base de clauses en utilisant Traducteur
     * et en produisant ResultatOcaml
     * @param clauses
     */
    public void preparation(BaseDeClauses clauses) {

    }

    /**
     * Lance une demande de résolution au solveur
     * @return le modèle suivant, NULL si non consistant
     * (insatisfiable)
     */
    public Model computeModel() {
        return new Model();
    }
}
