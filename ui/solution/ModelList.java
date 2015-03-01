/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
	public Iterator<Model> iterator() {
		return new ModelsIterator(models, solverInterface);
	}

	/**
	 * Exports the models already computed.
	 * @param fileName
	 */
	public void exportTo(String fileName) {
		// TODO increment #2
	}
}
