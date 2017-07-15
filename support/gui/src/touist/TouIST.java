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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.prefs.Preferences;

import javax.swing.JOptionPane;

import gui.MainFrame;
import solution.SolverExecutionException;

/**
 *
 * @author Skander
 */
public class TouIST {
	public static TouistProperties properties = new TouistProperties();
	private static MainFrame frame;

	/**
	 * @param args the command line arguments
	 */
	public static void main(String[] args) throws IOException, InterruptedException, FileNotFoundException, SolverExecutionException {
		String version = System.getProperty("java.version");
		if(Float.valueOf(version.substring(0,3)) < 1.7) {
			JOptionPane.showMessageDialog(null, "Your java version is "+version+" but version higher or equal to 1.7 is required");
			return;
		}

		System.out.println("TouIST: running app from folder '"+ System.getProperty("user.dir")+"'");
		System.out.println("* External binaries are in '"+getTouistExternalDir()+"'");
		System.out.println("* Files are saved in '"+getWhereToSave()+"'");
		frame = new MainFrame();
		frame.setVisible(true);
		
		if(args.length > 0) {
			frame.getEditorPanel1().open(args[0]);
		}
	}
	/**
	 * We use this for getting the actual place where touist.jar is located in.
	 * We do not use getProperty("user.dir") because on linux, it returns (when
	 * opening by clicking on touist.jar) the $HOME instead of the actual place where
	 * touist.jar is.
	 * @return
	 */
	public static String getTouistDir() {
		String pathToJar = ClassLoader.getSystemClassLoader().getResource(".").toString();
		// URISyntaxException should ne ever be thrown because we expect getResource(".")
		// to give a correct URL
		String path = "";
		try {
			path = (new File(new URI(pathToJar))).getAbsolutePath();
		} catch (URISyntaxException e) {
			System.err.println("Something went wrong when trying to get where touist.jar is located:\n" + e.getMessage());
		}
		return path;
	}
	public static String getTouistExternalDir() {
		String relativePath = System.getProperty("touist.externalRelativeDir");
		if(relativePath == null) relativePath = "external";
		return checkPath(TouIST.getTouistDir() + File.separator + relativePath);
	}
	public static String getTouistBin() {
		return checkPath(TouIST.getTouistExternalDir() + File.separator + "touist"); 
	}
	public static String getWhereToSave() {
		String relativePath = System.getProperty("touist.saveRelativeDir");
		if(relativePath == null) relativePath = "..";
		return checkPath(TouIST.getTouistDir() + File.separator + relativePath);
	}
	
	public static String checkPath(String path) {
		Path p = FileSystems.getDefault().getPath(path);
		return p.normalize().toString();
	}
}

