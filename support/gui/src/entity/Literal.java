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

/**
 *
 * @author blida
 */
public class Literal {
    private String literal;
    private boolean literal_positivity;
    private String arithmetic_value=null;
    
    /**
     * Added for filtering '&45' literals (issue #88)
     */
    @Override
	public boolean equals(Object obj) {
		Literal l = (Literal)obj;
		return this.literal_positivity == l.literal_positivity
				&& this.literal == l.literal;
	}

    public Literal(String get, boolean b) {
       literal=get;
       literal_positivity=b;
    }

    public Literal(String get, String b) {
       literal=get;
       arithmetic_value=b;
    }
    public Literal(String rawLiteral) {
        literal=rawLiteral;
    }
    
    public String getArithmetic_value() {
        return arithmetic_value;
    }

    public void setArithmetic_value(String arithmetic_value) {
        this.arithmetic_value = arithmetic_value;
    }
    
    public String getLiteral() {
        return literal;
    }

    public void setLiteral(String literal) {
        this.literal = literal;
    }

    public boolean isLiteral_positivity() {
        return literal_positivity;
    }

    public void setLiteral_positivity(boolean literal_positivity) {
        this.literal_positivity = literal_positivity;
    }

}
