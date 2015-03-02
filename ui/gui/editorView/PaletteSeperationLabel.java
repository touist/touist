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

package gui.editorView;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JLabel;

/**
 * représente les morceaux de texte qu'il y a entre les boutons pour insérer du texte
 * Ces morceaux de texte ne sont que décorations.
 * @author François Schwarzentruber
 */
public class PaletteSeperationLabel extends JLabel {

    public PaletteSeperationLabel(String s) {
        super("   " + s + "    ");
        setOpaque(true);
        setBackground(Color.LIGHT_GRAY);
        setForeground(Color.white);
        setFont(new Font(getFont().getName(), 0, 14));



    }



}
