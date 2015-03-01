/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.IOException;

import entity.Model;

/**
 * A sub-class must implement Solver. The inherited class allows the user to
 * launch the solver program with the right DIMACS input, to give the user an
 * iterable Models (or null if no model found) and to stop the solver program
 * (that is running background).
 *
 * @author Abdel
 * @modified Maël
 */
public abstract class Solver {

	public Solver() {
		super();
	}

	/**
	 * Launch the solver program in background. Does not check satisfiability
	 * but waits for the ModelsIterator.hasNext() to retrieve the next model.
	 * @param dimacsFilesPath The DIMACS file path
	 * @return the iterable Models instance.
	 * @throws IOException
	 */
	public abstract void launch() throws IOException;

	/**
	 * Kills the solver program process
	 */
	public abstract void close();

	/**
	 * Gives the list of models on which the user can iterate. The only way to
	 * get the "next" models is to iterate. The size of Models is not known
	 * unless hasNext() returns false.
	 * Use this method after using launch().
	 * @return the models
	 */
	public abstract Models getModels();

	/**
	 * ONLY used by ModelsIterator
	 * @throws IOException
	 */

	protected abstract Model nextModel() throws IOException;

	/**
	 *
	 * @param rawModelOutput The output
	 * @return a model with, if a literalMap was given, the translated literal.
	 * If no literalMap is given, the Model stores the literal as given by the
	 * solver (an integer).
	 */
	protected abstract Model parseModel(String[] rawModelOutput);
}
