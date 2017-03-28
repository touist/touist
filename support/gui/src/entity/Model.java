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

	/**
	 * WARNING: two models are equals only
	 * when the literals are at the same place
	 * in the arraylist
	 * @note Added for filtering '&45' literals (issue #88)
	 */
	@Override
	public boolean equals(Object obj) {
		Model m = (Model)obj;
		boolean areEqual = true; 
		if(this.literals.size() != m.literals.size()) {
			areEqual = false;
		}
		for (int i = 0; i < this.literals.size() && areEqual; i++) {
			if(!this.literals.get(i).equals(m.literals.get(i))) {
				areEqual = false;
			}
		}
		return areEqual;
	}

	public void addLiteral(Literal literal) {
                literals.add(literal);
	}

	@Override
	public String toString() {
		// TODO Please write a proper toString
		String out = "";
		for (Literal s : literals) {
			out = out + " ("+ s.getLiteral()+" "+((s.getArithmetic_value()==null)?s.isLiteral_positivity():s.getArithmetic_value())+") ";
		}
		return out;
	}

	@Override
	public Iterator<Literal> iterator() {
		return literals.iterator();
	}


}
