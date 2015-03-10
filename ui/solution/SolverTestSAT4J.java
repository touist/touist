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

package solution;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import entity.Literal;
import entity.Model;

/**
 * This class is a first implementation of the "Solver" abstract class. It
 * allows the user to use the "sat4j-sat.jar" program that Abdel prepared for
 * testing purpose.
 *
 * the next model.
 * @author Abdel
 */
public class SolverTestSAT4J extends Solver {
	private Process p;
	private PrintWriter stdin;
	private BufferedReader stderr;
	private BufferedReader stdout;

	private String dimacsFilePath;
	private Map<Integer, String> literalsMap; // "table de correspondance"

	private ModelList models = null;
	private boolean hasFoundModels = false;
	private boolean solverIsRunning = false;

	/**
	 * This is the main constructor used by the user after he translated the
	 * BIGAND file to a DIMACS file (and the "literalsMap" associated).
	 * @param dimacsFilePath the DIMACS file
	 * @param literalsMap the "literals map" ("table de correspondance")
	 */
	public SolverTestSAT4J(String dimacsFilePath,
			Map<Integer, String> literalsMap) {
		this.dimacsFilePath = dimacsFilePath;
		this.literalsMap = literalsMap;
		this.p = null;
		this.stdin = null;
		models = new ModelList(this);
	}

	/**
	 * This constructor is useful when the user wants to solve a problem without
	 * using a "literalsMap" ("table de correspondance"). Hence the user has
	 * only to pass a DIMACS file path.
	 * @warning ONLY FOR TESTS PURPOSE
	 * @param dimacsFilePath
	 */
	public SolverTestSAT4J(String dimacsFilePath) {
		this.dimacsFilePath = dimacsFilePath;
		this.literalsMap = null;
		this.p = null;
		this.stdin = null;
		models = new ModelList(this);
	}

	@Override
	public void launch() throws IOException {
		// TODO We should be able to re-use the Solver instance
		// TODO We should be warned if the "java -cp" command fails because it
		// can't find the files

		/*
		 * MINISAT TESTING PROGRAM Behaviour ".:MiniSat:MiniSat/sat4j-sat.jar"
		 * is the search path for binaries RETURN VALUES: 1 = unsatisfiable 2 =
		 * parse issue 3 = wrong dimacs content 4 = error with the streamreader
		 * 5 = solver timeout
		 */
		String command = "java -cp .:MiniSat:MiniSat/sat4j-sat.jar Minisat "
				+ getDimacsFilePath();
		this.p = Runtime.getRuntime().exec(command);
		stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		stdout = new BufferedReader(new InputStreamReader(p.getInputStream()));
		stdin = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));

		try {
			// We check if the solver program has been actually launched:
			if (p.waitFor(1, TimeUnit.SECONDS) && !p.isAlive()
					&& p.exitValue() > 1) {
				solverIsRunning = false;
				String error = "launch(): Error while launching external solver\n";
				error += "launch(): external solver returned "
						+ Integer.toString(p.exitValue()) + "\n";
				error += "launch(): command used: '" + command + "'\n";
				while (stdout.ready())
					error += "launch(): '" + stdout.readLine() + "'\n";
				while (stderr.ready())
					error += "launch(): '" + stderr.readLine() + "'\n";
				throw new IOException(error);
			} else if (!p.isAlive() && p.exitValue() == 1) { // Unsatisfiable (return=1)
				hasFoundModels = false;
				solverIsRunning = false;
			} else { // The external solver is running as expected
				System.out.println("launch(): the external solver is now running");
				solverIsRunning = true;
				hasFoundModels = true;
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	@Override
	public boolean isSatisfiable() {
		return hasFoundModels;
	}

	@Override
	public ModelList getModelList() throws NotSatisfiableException,
			SolverExecutionException {
		if (models == null)
			throw new SolverExecutionException(
					"getModelList(): solver hasn't been launched yet");
		if (!hasFoundModels)
			throw new NotSatisfiableException();
		return models;
	}

	@Override
	public void close() {
		stdin.println("\n0");
		stdin.close();
		this.p.destroy();
		solverIsRunning = false;
	}

	@Override
	protected Model nextModel() throws IOException, NotSatisfiableException {
		if (!solverIsRunning)
			throw new IOException("nextModel(): solver was already closed!");
		stdin.println("1");
		stdin.flush();
                //Beug Fonction
//		while (!stdout.ready()) {
//			try { 
//				p.waitFor(100, TimeUnit.MILLISECONDS);
//			} catch (InterruptedException e) {
//				e.printStackTrace();
//			}
//		}
                //use isAlive to detect if the communication is up or not
                //if it isn't: it mean that we have no next model and an externel solver is done.
                if (p.isAlive()){
                while(!stdout.ready()){
                //the attente can be 1ms of 100ms or 101ms,
                }
		String[] elmts = stdout.readLine().split(" ");
                System.out.println(elmts);
                return parseModel(elmts);
                }
                //no model
                //see ModelListeIterator.HasNext()
                //to see controle of this call method.
		return null;
	}

	@Override
	protected Model parseModel(String[] rawModelOutput)
			throws NotSatisfiableException {
		// TODO The parser should be able to handle the "-3" (negation)
		if (!hasFoundModels)
			throw new NotSatisfiableException();
		Model model = new Model();
		for (String rawLiteral : rawModelOutput) {
			int literalInt = Integer.parseInt(rawLiteral);
			if (literalInt != 0) {
				int literalCode = (literalInt > 0 ? literalInt : literalInt * (-1));
				if (getLiteralsMap().get(literalCode) != null) {
					model.addLiteral(new Literal(getLiteralsMap().get(
							literalCode), literalInt > 0));
				} else {
					model.addLiteral(new Literal(rawLiteral, literalInt > 0));
				}
			}
		}
		return model;
	}

	/**
	 * ONLY used by Models
	 * @return the DIMACS file path
	 */
	protected String getDimacsFilePath() {
		return dimacsFilePath;
	}

	/**
	 * ONLY used by Models
	 * @return the literalsMap (DIMACS integer to string names)
	 */
	protected Map<Integer, String> getLiteralsMap() {
		return literalsMap;
	}

	protected boolean isStillRunning() {
		return solverIsRunning;
	}
}
