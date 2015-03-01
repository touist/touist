/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.IOException;
import java.util.List;
import java.util.ListIterator;

public class ModelsIterator implements ListIterator<Model> {
	private Solver solverInterface;
	private List<Model> models;
	private int currentPosition;

	/**
	 * @param models The collection that comes from Models
	 * @param solverInterface The instance of Solver that produces the new
	 * Model-s
	 */
	public ModelsIterator(List<Model> models, Solver solverInterface) {
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
