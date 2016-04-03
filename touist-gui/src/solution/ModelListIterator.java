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

import java.io.IOException;
import java.util.List;
import java.util.ListIterator;

import entity.Model;
import entity.LexicographicalTree;
import java.util.Set;

public class ModelListIterator implements ListIterator<Model> {
	private Solver solverInterface;
	private List<Model> models;
        private LexicographicalTree alreadyPresent;
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
                this.alreadyPresent = new LexicographicalTree();
	}

	@Override
	public boolean hasNext() {
		boolean hasNext = false;
		if (currentPosition == models.size() - 1) { // Need to get a new model?
			Model nextModel = null;
			try {
				nextModel = solverInterface.nextModel();
			} catch (IOException e) {
				System.err.println("hasNext(): I/O exception: "+e.getMessage());
				return false;
			} catch (SolverExecutionException e) {
				System.err.println("hasNext(): "+e.getMessage());
			}
			
			// Added for filtering '&45' literals
			if(nextModel != null && alreadyPresent.contains(nextModel)) {
				return hasNext();
			}

			if (nextModel == null) { // No models left
				solverInterface.close();
				hasNext = false;
				System.out.println("hasNext(): there is no more models");
			} else {
				hasNext = true;
				models.add(nextModel);
                                alreadyPresent.add(nextModel);
			}
		} else { // Models have already been retrieved (e.g. because of
					// previous())
			hasNext = true;
		}
		return hasNext;
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
