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

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Note: to use that class, write:
 * 		private static TouistProperties properties = new TouistProperties();
 * @author maelv
 *
 */
public class TouistProperties {
	private static Properties prop = new Properties();
	public TouistProperties() {
		try {
			InputStream in = this.getClass().getClassLoader().getResourceAsStream("version.properties");
			if(in == null) throw new IOException();
			else prop.load(in);
		} catch (IOException e) {
			System.out.println("Warning: the property file version.properties was not found");
		}
	}
    
	public String getProperty(String property) {
		String res = prop.getProperty(property);
		return (res != null)?res:"";
	}
}
