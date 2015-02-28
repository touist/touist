/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;


public class ModelsIterator implements ListIterator<Model>{
    private Solver solverInterface;
    private Collection<Model> models;

    /**
     * @param models The collection that comes from Models
     * @param solverInterface The instance of Solver that produces the new Model-s
     */
    public ModelsIterator(Collection<Model> models, Solver solverInterface) {
	super();
	this.models = models;
	this.solverInterface = solverInterface;
    }

    @Override
    public boolean hasNext() {
	Model m = solverInterface.computeModel();
	if(m == null) {
	    // No more models, we can end the translation program
	    solverInterface.stop();
	} else {

	}
	return false;
    }


    @Override
    public Model next() {
	// TODO Auto-generated method stub
	return null;
    }


    @Override
    public boolean hasPrevious() {
	// TODO Auto-generated method stub
	return false;
    }


    @Override
    public Model previous() {
	// TODO Auto-generated method stub
	return null;
    }


    @Override
    public int nextIndex() {
	// TODO Auto-generated method stub
	return 0;
    }


    @Override
    public int previousIndex() {
	// TODO Auto-generated method stub
	return 0;
    }


    @Override
    public void remove() {
	// TODO Auto-generated method stub

    }


    @Override
    public void set(Model e) {
	// TODO Auto-generated method stub

    }


    @Override
    public void add(Model e) {
	// TODO Auto-generated method stub

    }


    @Override
    public Iterator<Model> iterator() {
	// TODO Auto-generated method stub
	return null;
    }




}
