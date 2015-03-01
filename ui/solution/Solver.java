/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/olzd/touist
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
	public abstract ModelList getModels();

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
