/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author Skander
 * @Modified by Abdel
 */
//Be careful
//Null Return means not exist.
public class Models implements Iterable<Model> {
    private Collection<Model> models = new ArrayList<Model>();
    private Solver solverInterface;

    public Models(Solver solverThatProducesTheModels) {

    }

    public void addModel(Model model) {
        models.add(model);
    }

    @Override
    public Iterator<Model> iterator() {
	// TODO do it !
	return new ModelsIterator(models,solverInterface);
    }

    public void export() {
        //Seconde Increment
    }
}
