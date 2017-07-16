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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;

import entity.Literal;
import entity.Model;
import translation.TranslationError;

/**
 * This class is a first implementation of the "Solver" abstract class. It
 * allows the user to use the "sat4j-sat.jar" program that Abdel prepared for
 * testing purpose.
 *
 * the next model.
 * @author Abdel
 */
public class SolverQBF extends Solver {
	private Process p;
	private PrintWriter stdin;
	private BufferedReader reader;
	private BufferedReader stderr;
	private BufferedReader stdout;
	private List<String> options = new ArrayList<>();
	public List<TranslationError> errors = new ArrayList<TranslationError>();

	private ModelList models;

	/**
	 * This is the main constructor used by the user after he translated the
	 * BIGAND file to a DIMACS file (and the "literalsMap" associated).
	 * @param dimacsFilePath the DIMACS file
	 * @param literalsMap the "literals map" ("table de correspondance")
	 */
	public SolverQBF(BufferedReader reader) {
		this.reader = reader;
		this.p = null;
		this.stdin = null;
		models = new ModelList(this);
	}
	public SolverQBF(BufferedReader reader, List<String> options) {
		this.options = options;
		this.reader = reader;
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
	public void launch() throws IOException, InterruptedException {
		// TODO We should be able to re-use the Solver instance
		// TODO We should be warned if the "java -cp" command fails because it
		// can't find the files

		String pathtouist = touist.TouIST.getTouistBin();
		
		List<String> cmd = new ArrayList<String>();
		
		cmd.add(pathtouist);
		cmd.add("--qbf");
		cmd.add("-");
		cmd.add("--solve");
		cmd.add("--error-format");
		cmd.add("%l:%L:%b:%B: %t: %m");
		cmd.addAll(options);
		
        System.out.println("translate_solve(): cmd executed: "+cmd.toString());
		
        this.p = Runtime.getRuntime().exec(cmd.toArray(new String[0]));

        stdout = new BufferedReader(new InputStreamReader(p.getInputStream()));
		stderr = new BufferedReader(new InputStreamReader(this.p.getErrorStream()));
        stdin = new PrintWriter(new OutputStreamWriter(p.getOutputStream()));
        String s = "";
        while ((s = reader.readLine())!=null) {
        	stdin.println(s + "\n");
        }
        stdin.flush();
        stdin.close();
	}

	@Override
	public ModelList getModelList() throws SolverExecutionException {
		return models;
	}

	@Override
	public void close() {
		stdin.println("\n0");
		stdin.write(0);
		stdin.close();
		this.p.destroy();
		System.out.println("close(): solver has been closed correctly");
	}

	public final static int OK              = 0;
	public final static int UNKNOWN         = 1;
	public final static int CMD_USAGE       = 2;
	public final static int CMD_UNSUPPORTED = 3;
	public final static int TOUIST_SYNTAX   = 4;
	public final static int TOUIST_TIMEOUT  = 5;
	public final static int TOUIST_MEMORY   = 6;
	public final static int TOUIST_UNKNOWN  = 7;
	public final static int SOLVER_UNSAT    = 8;
	public final static int SOLVER_UNKNOWN  = 9;
	public final static int SOLVER_TIMEOUT  = 10;
	public final static int SOLVER_MEMORY   = 11;

	@Override
	protected Model nextModel() throws IOException, SolverExecutionException {
		final int WAIT_FOR_MODEL_TIMEOUT = 5000000; // ms
		if (p == null) // Should not happen
			throw new SolverExecutionException("nextModel(): exception: launch() has not been called");

		Model modelParsed = null;
		// We wait for any output from the solver unless we get a timeout
		boolean no_timeout = waitResult(WAIT_FOR_MODEL_TIMEOUT);
		// Case 1 : we got some text to read from stdout
		if(stdout.ready() && p.exitValue() == 0) {
			String assignements = "";
			while(stdout.ready())
				assignements += stdout.readLine() + "\n";
			modelParsed = parseModel(assignements.split("\\n"));
		}
		// Case 2 : no text but solver still running
		if(!stdout.ready() && ! no_timeout) { // Nothing has been read
			throw new SolverExecutionException("nextModel(): timeout = "
					+Integer.toString(WAIT_FOR_MODEL_TIMEOUT)+"ms)");
		}
		
		if(p.exitValue() == SOLVER_UNSAT)
			return modelParsed;
		else if(p.exitValue() != OK) {
			throw new SolverExecutionException("nextModel(): touist returned error code "+Integer.toString(p.exitValue())+"\n"+errors.toString());
		} else
			return modelParsed;
	}

	@Override
	protected Model parseModel(String[] rawModelOutput) {
		Model model = new Model();
		for (String line : rawModelOutput) {
			Scanner scan = new Scanner(line);
			Pattern p1 = Pattern.compile("^([0-9]|\\?) (.*)$");
			if(p1.matcher(line).find()) {
				scan.findInLine(p1);
				MatchResult r = scan.match();
				model.addLiteral(new Literal(r.group(2),r.group(1)));
			}
			scan.close();
		}
		return model;
	}

	/**
	 * ONLY used by Models
	 * @return the DIMACS file path
	 */
	protected BufferedReader getReader() {
		return reader;
	}

	public int getReturnCode() {
		return p.exitValue();
	}
	
	public List<TranslationError> getErrors() {
		return errors;
	}
	
	/**
	 * 
	 * @param timeout in milliseconds
	 * @return true if the result has been given before the timeout
	 * @throws IOException
	 */
	public boolean waitResult(int timeout) throws IOException {
		final long timeout_time = System.currentTimeMillis() + timeout;
		while(!stdout.ready() && isAlive(p) && System.currentTimeMillis() < timeout_time){
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
		if(p.exitValue() != OK) {
			String linesStdErr = "";
			while (stderr.ready())
				linesStdErr += stderr.readLine() + "\n";
			errors = TranslationError.parse(linesStdErr);
		}
		return System.currentTimeMillis() < timeout_time;
	}
}
