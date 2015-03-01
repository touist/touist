/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package entity;

import entity.Literal;
import java.util.ArrayList;

/**
 *
 * @author Skander
 * @Modified By Abdel
 */
public class Model {

	// private StringBuffer br;
	// Contain Literals item Valuated True and Satisfy Logical Problem.
	public ArrayList<Literal> literals = new ArrayList<Literal>();

	public void addLiteral(Literal literalName) {
		literals.add(literalName);
	}

	@Override
	public String toString() {
		// TODO Skander, please write the proper toString
		String out = "";
		for (Literal s : literals) {
			out = out + " " + s.getLiteral();
		}
		return out;
	}
}
