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

package gui;

import javax.swing.Icon;

import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

/**
 * Cette classe permet de générer une image à partir du code LaTEX
 * @author François Schwarzentruber
 */
 public class LaTEX {


/**
 *
 * @param codeLaTEX
 * @return une image qui représente ce qu'affiche LaTEX à la place du code codeLaTEX
 */
    static Icon latexCodeToImageIcon(String codeLaTEX)
    {
         TeXFormula formula = new TeXFormula(codeLaTEX);
         TeXIcon ti = formula.createTeXIcon(TeXConstants.STYLE_DISPLAY, 20);

         return ti;

    }

}
