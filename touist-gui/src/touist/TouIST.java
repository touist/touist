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

package touist;

import gui.MainFrame;
import javax.swing.JOptionPane;

/**
 *
 * @author Skander
 */
public class TouIST {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        String version = System.getProperty("java.version");
        if(Float.valueOf(version.substring(0,3)) < 1.6) {
            JOptionPane.showMessageDialog(null, "Your java version is "+version+" but version higher or equal to 1.6 is required");
            return;
        }
    	System.out.println("main(): running app from folder '"
    			+ System.getProperty("user.dir")+"'");
        MainFrame frame = new MainFrame();
        frame.setVisible(true);
    }
}
