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

package solution;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import entity.Model;

/**
 * This class is the Model container. An instance of Models allows the user to
 * iterate (with iterate()) on the different models. When using
 * ModelsIterator.hasNext(), the Solver.nextModel() will be called and the next
 * model will be automatically retrieved.
 */
public class ModelList implements Iterable<Model> {
	private List<Model> models = new ArrayList<Model>();
	private Solver solverInterface; // The solver that created this inst. of
									// Models

	/**
	 * Create an instance of Models; you need to pass the Solver instance that
	 * created this instance of Models because Models will call
	 * Solver.nextModel() when you do ModelsIterator.hasNext(). This constructor
	 * is protected because it should only be used by Solver and not by the end
	 * user.
	 *
	 * @param solverThatProducesTheModels
	 */
	protected ModelList(Solver solverThatProducesTheModels) {
		this.solverInterface = solverThatProducesTheModels;
	}

	/**
	 * This iterator allows the user to iterate over the Model instances. The
	 * whole list of Model is not computed before getting the first Model: this
	 * list is built "on the fly", when the user uses ModelsIterator.hasNext().
	 */
	@Override
	public ListIterator<Model> iterator() {
		return new ModelListIterator(models, solverInterface);
	}

	/**
	 * Exports the models already computed.
	 * @param fileName
	 */
	public void exportTo(String fileName) {
		// TODO increment #2
	}
}
