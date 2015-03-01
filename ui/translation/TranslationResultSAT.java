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

package translation;

import java.util.HashMap;
import java.util.Map;

/**
* @author Abdel
* @modified by Maël
*/
public class TranslationResultSAT implements TranslationResult{

	    private String dimacsFilePath;
	    private Map<Integer, String> literaux = new HashMap<Integer,String>();

	    @Override
		public void setDimacsFilePath(String dimacsFilePath) {
	        this.dimacsFilePath = dimacsFilePath;
	    }

	    @Override
		public void addLiteraux(int cleP,String P) {
	        this.literaux.putIfAbsent(cleP, P);
	    }
	    @Override
		public String getDimacsFilePath() {
	        return dimacsFilePath;
	    }
	    @Override
		public Map<Integer, String> getLiteraux() {
	        return literaux;
	    }


}
