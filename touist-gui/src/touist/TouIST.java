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

import entity.Model;
import gui.MainFrame;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Scanner;

import solution.NotSatisfiableException;
import solution.SolverExecutionException;
import solution.SolverTestSAT4J;
import translation.Translator;

/**
 *
 * @author Skander
 */
public class TouIST {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
    	System.out.println("main(): running app from folder '"
    			+ Paths.get("").toAbsolutePath().toString()+"'");
        MainFrame frame = new MainFrame();
        frame.setVisible(true);
    }


/*
 * Alternative mains for tests
 */
	public static void main2(String[] args) {
		/*
		 * TRANSLATOR Tests
		 */
		Translator translator = new Translator("compiler/touistc.native");
		boolean worked = false;
		try {
			worked = translator.translate("compiler/test/foo.touistl");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if(worked) {
			System.out.println("OK, la traduction a bien terminé");
			translator.getLiteralsMap().toString();
		}
		else {
			System.out.println("NOK, la traduction a échoué");
		}


		/*
		 * SOLVER Tests
		 */
		SolverTestSAT4J solver = new SolverTestSAT4J(translator.getDimacsFilePath(),
				translator.getLiteralsMap());
		try {
			solver.launch();
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (solver.isSatisfiable()) {
			System.out.println("Satisfiable");
		} else {
			System.out.println("Insatisfiable");
			System.exit(0);
		}
		Scanner sc = new Scanner(System.in);
		String answer;
		boolean continuer = true;
		Model m;

		Iterator<Model> it = null;
		try {
			it = solver.getModelList().iterator();
		} catch (NotSatisfiableException | SolverExecutionException e) {
			e.printStackTrace();
		}
		while (it.hasNext() && continuer) {
			m = it.next();
			System.out.println("Modèle : " + m.toString());

			System.out.println("Contiuner ? o/n");
			answer = sc.nextLine();
			continuer = (answer.charAt(0) == 'o');
		}
		sc.close();
		solver.close();

	}
}
