/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 */

public class Models implements Iterable<Model> {
    private List<Model> models = new ArrayList<Model>();
    private Solver solverInterface;

    public Models(Solver solverThatProducesTheModels) {
	this.solverInterface = solverThatProducesTheModels;
    }

    @Override
    public Iterator<Model> iterator() {
	return new ModelsIterator(models,solverInterface);
    }

    public void export() {
        // TODO incrément 2
    }
}
