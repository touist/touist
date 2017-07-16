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

package solution;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Arrays;
import java.util.Map;

import entity.Literal;
import entity.Model;
import touist.TouIST;

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

	private ModelList models;

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
	public SolverTestSAT4J() {
		this.literalsMap = null;
		this.p = null;
		this.stdin = null;
		models = new ModelList(this);
	}

    /**
     * For java jre 1.6 and 1.7 compatibility (p.isAlive() is java jre >= 1.8)
     */
	private boolean isAlive(Process process) {
	    try {
	        process.exitValue();
	        return false;
	    } catch (Exception e) {
	        return true;
	    }
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

		String pathminisat = TouIST.getTouistExternalDir() + File.separator + "minisat.jar";

		String [] command = { "java", "-jar",pathminisat, this.dimacsFilePath} ;
		System.out.println("launch(): cmd executed: "+Arrays.toString(command));
		this.p = Runtime.getRuntime().exec(command);
		stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		stdout = new BufferedReader(new InputStreamReader(p.getInputStream()));
		stdin = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));
	}

	@Override
	public ModelList getModelList() throws SolverExecutionException {
		return models;
	}

	@Override
	public void close() {
		stdin.println("\n0");
		stdin.close();
		this.p.destroy();
		System.out.println("close(): solver has been closed correctly");
	}

	@Override
	protected Model nextModel() throws IOException, SolverExecutionException {
		final int WAIT_FOR_MODEL_TIMEOUT = 5000000; // ms
		if (p == null) // Should not happen
			throw new SolverExecutionException("nextModel(): exception: launch() has not been called");
		if (!isAlive(p)) { // The solver is already done
			return null;
		}
		String[] rawLiterals;
		Model modelParsed = null;
		stdin.println("1"); // tells the solver to give the next model 
		stdin.flush();
		// We wait for any output from the solver unless we get a timeout
		final long timeout = System.currentTimeMillis() + WAIT_FOR_MODEL_TIMEOUT;
		while(!stdout.ready() && isAlive(p) && System.currentTimeMillis() < timeout){
			// Active waiting (I know, it is a bad way to do it!)
			try {
				synchronized (this) { // for JavaRE6 compliance
					this.wait(10);
				}
			} catch (InterruptedException e) {
				// TODO I added this wait to avoid active complete waiting
				e.printStackTrace();
			}
		}
		// Case 1 : we got some text to read from stdout
		if(stdout.ready()) {
			rawLiterals = stdout.readLine().split(" ");
			modelParsed = parseModel(rawLiterals);
		}
		// Case 2 : no text but solver still running
		if(!stdout.ready() && System.currentTimeMillis() >= timeout) { // Nothing has been read
			throw new SolverExecutionException("nextModel(): exception: "
					+ "the solver didn't give any output (timeout = "
					+Integer.toString(WAIT_FOR_MODEL_TIMEOUT)+"ms)");
		}
		return modelParsed;
	}

	@Override
	protected Model parseModel(String[] rawModelOutput) {
		// TODO The parser should be able to handle the "-3" (negation)
		Model model = new Model();
		for (String rawLiteral : rawModelOutput) {
			int literalInt = Integer.parseInt(rawLiteral);
			if (literalInt != 0) { // '0' means 'end of model'
				int literalCode = (literalInt > 0 ? literalInt : literalInt * (-1));
				String literalString = getLiteralsMap().get(literalCode);
				if (literalString != null) {
					// Added for filtering '&45' literals (issue #88)
					if(literalString.charAt(0) != '&') {
						model.addLiteral(new Literal(getLiteralsMap().get(
								literalCode), literalInt > 0));
					}
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
}
