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
import java.util.List;
import java.util.ListIterator;

import entity.Model;

public class ModelListIterator implements ListIterator<Model> {
	private Solver solverInterface;
	private List<Model> models;
	private int currentPosition;

	/**
	 * @param models The collection that comes from Models
	 * @param solverInterface The instance of Solver that produces the new
	 * Model-s
	 */
	public ModelListIterator(List<Model> models, Solver solverInterface) {
		super();
		this.models = models;
		this.solverInterface = solverInterface;
		currentPosition = -1;
	}

	@Override
	public boolean hasNext() {
		Model m = null;
		try {
			m = solverInterface.nextModel();
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (m == null) { // No models left
			solverInterface.close();
		} else {
			models.add(m);
		}
		return currentPosition + 1 < models.size();
	}

	@Override
	public Model next() {
		return models.get(++currentPosition);
	}

	@Override
	public boolean hasPrevious() {
		return currentPosition > 0;
	}

	@Override
	public Model previous() {
		return models.get(--currentPosition);
	}

	@Override
	public int nextIndex() {
		return ++currentPosition;
	}

	@Override
	public int previousIndex() {
		return --currentPosition;
	}

	@Override
	public void remove() {
		models.remove(currentPosition);
	}

	@Override
	public void set(Model m) {
		models.set(currentPosition, m);
	}

	@Override
	public void add(Model m) {
		models.add(currentPosition, m);
	}

}
