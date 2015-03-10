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
			out = out + " " + s.getLiteral()+ " Valuated : "+s.isLiteral_positivity()+"\n";
		}
		return out;
	}

	@Override
	public Iterator<Literal> iterator() {
		return literals.iterator();
	}


}
