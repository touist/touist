/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
