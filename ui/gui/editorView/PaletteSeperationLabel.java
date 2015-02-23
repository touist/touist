/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
