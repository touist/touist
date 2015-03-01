/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package entity;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Abdel
 * @modified by Maël
 */
public class Model implements Iterable<Literal>{

	public List<Literal> literals = new ArrayList<Literal>();

	public void addLiteral(Literal literal) {
		literals.add(literal);
	}

	@Override
	public String toString() {
		// TODO Please write a proper toString
		String out = "";
		for (Literal s : literals) {
			out = out + " " + s.getLiteral();
		}
		return out;
	}

	@Override
	public Iterator<Literal> iterator() {
		return literals.iterator();
	}


}
