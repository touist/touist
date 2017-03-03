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

package touist;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JOptionPane;
import javax.swing.UIManager;

import gui.MainFrame;
import solution.SolverExecutionException;
/**
 *
 * @author Skander
 */
public class TouIST {
	private static TouistProperties properties = new TouistProperties();

	/**
	 * @param args the command line arguments
	 */
	private static String CurrentPath=System.getProperty("user.dir");
	public static void main(String[] args) throws IOException, InterruptedException, FileNotFoundException, SolverExecutionException {
		String version = System.getProperty("java.version");
		if(Float.valueOf(version.substring(0,3)) < 1.7) {
			JOptionPane.showMessageDialog(null, "Your java version is "+version+" but version higher or equal to 1.7 is required");
			return;
		}
		// Better user interface integration with macOS
		if(System.getProperty("os.name").toLowerCase().contains("mac")) {
			System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Touist");
			try {
				UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			} catch (Exception e) {}
		}
		if(args.length==0) {
			System.out.println("main(): running app from folder '"
					+ System.getProperty("user.dir")+"'");
			MainFrame frame = new MainFrame();
			frame.setVisible(true);
		}
		else{
			String path="";
			File f;

		System.out.println("main(): running app from folder '"
				+ System.getProperty("user.dir")+"'");
		MainFrame frame = new MainFrame();
		frame.setVisible(true);
	}
}

