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

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.util.Properties;

import javax.swing.*;

import gui.MainFrame;
import solution.SolverExecutionException;

/**
 *
 * @author Skander
 */
public class TouIST {
	private static MainFrame frame;
	public static TextAreaLog textAreaLog;

	/**
	 * @param args the command line arguments
	 */
	public static void main(String[] args) throws IOException, InterruptedException, FileNotFoundException, SolverExecutionException {
		loadProperties();
		String version = System.getProperty("java.version");
		if(Float.valueOf(version.substring(0,3)) < 1.7) {
			JOptionPane.showMessageDialog(null, "Your java version is "+version+" but version higher or equal to 1.7 is required");
			return;
		}
		textAreaLog = new TextAreaLog();
		System.out.println("TouIST: running app from folder '"+ System.getProperty("user.dir")+"'");
		System.out.println("* External binaries are in '"+getTouistExternalDir()+"'");
		System.out.println("* Files saved in '"+getWhereToSave()+"', temp in '"+getWhereToSaveTemp()+"'");
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
		String path = "";
		final String resourcePath = ".";
		try {
			String pathToJar = ClassLoader.getSystemClassLoader().getResource(resourcePath).toString();
			// URISyntaxException should ne ever be thrown because we expect getResource(".")
			// to give a correct URL
			path = (new File(new URI(pathToJar))).getAbsolutePath();
		} catch (URISyntaxException e) {
			System.err.println("Something went wrong when trying to get where touist.jar is located:\n" + e.getMessage());
		} catch (NullPointerException e) {
			System.err.println("The path '"+resourcePath+"' does not belong to the Class-Path\n");
			System.exit(1);
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
	public static String getWhereToSaveTemp() {
		String relativePath = System.getProperty("touist.tempInHomeRelativeDir");
		if(relativePath == null) return getWhereToSave();
		else {
			String path = checkPath(System.getProperty("user.dir") + File.separator + relativePath);
			Path p = Paths.get(relativePath);
			if(!Files.exists(p,LinkOption.NOFOLLOW_LINKS)) {
				try {
					Files.createDirectories(p);
				} catch (IOException e) {
					System.err.println("Could not create directory '"+path+"'");
					e.printStackTrace();
					System.exit(1);
				}
			}
			return p.toString();
		}
	}
	public static String checkPath(String path) {
		Path p = FileSystems.getDefault().getPath(path);
		return p.normalize().toString();
	}

	private static void loadProperties() {
		try {
			Properties prop = new Properties();
			InputStream in = ClassLoader.getSystemClassLoader().getResourceAsStream("version.properties");
			if(in == null) throw new IOException();
			else prop.load(in);
			// System properties are merged into our new prop in order to make
			// sure that they are not overriden by our property file
			prop.putAll(System.getProperties());
			System.setProperties(prop);
		} catch (IOException e) {
			System.out.println("Warning: the property file version.properties was not found");
		}
	}
}

