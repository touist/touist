/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/FredMaris/touist
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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * @author Abdel
 * @Modified by Mael
 */
public class TranslatorSMT {
	final private String outputFilePath = "out.smt2";
	final private String outputTableFilePath = "out.table";
	private String translatorProgramFilePath;
	private Map<Integer,String> literalsMap = new HashMap<Integer,String>();
	private List<TranslationError> errors;
	private String currentPath = System.getProperty("user.dir");
	private Process p;

	public TranslatorSMT(String translatorProgramFilePath) {
		this.translatorProgramFilePath = translatorProgramFilePath;
	}

	/**
	 * Calls the translator/compiler to transform the ".touistl" file to a
	 * ".dimacs" file (along with a "mapping" file). This method also calls the
	 * parsing methods parseErrors and (if the translation actually
	 * passed) parseLiteralsMapFile.
	 *
	 * @param touistlFilePath is the touistl (i.e. file produced by the GUI or
	 * given by the user) 
	 *
	 * @return true if the translation went well (migth have generated some
	 * warning though), false of fatal errors (syntax/semantic errors) happened
	 *
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public boolean translate(String touistlFilePath, String logic) throws IOException, InterruptedException {
		/* return_code from the Touistl translator (see touistc.ml):
  		| OK -> 0
  		| COMPILE_WITH_LINE_NUMBER_ERROR -> 1
  		| COMPILE_NO_LINE_NUMBER_ERROR -> 2
  		| OTHER -> 3
		 */
                 System.out.println("le fichier ...."+touistlFilePath);
		final int OK = 0;
		final int COMPILE_WITH_LINE_NUMBER_ERROR = 1;
		final int COMPILE_NO_LINE_NUMBER_ERROR = 2;
		final int OTHER = 3;
		/*
		 * Syntax of errors COMPILE_WITH_LINE_NUMBER_ERROR:
		 * num_row:num_col: message
		 */
		// Check if translatorProgramFilePath is there
		String path = currentPath + File.separatorChar + translatorProgramFilePath;
		String [] cmd = {path.toString(),"-o", outputFilePath,"-smt2", logic, touistlFilePath};
		System.out.println("translate(): cmd executed: "+cmd);
		this.p = Runtime.getRuntime().exec(cmd);
		int return_code = p.waitFor();
		BufferedReader stdout = new BufferedReader(new InputStreamReader(
				this.p.getInputStream()));
		List<String> linesStdout = new ArrayList<String>();
		while (stdout.ready()) {
			linesStdout.add(stdout.readLine());
		}
		BufferedReader stderr = new BufferedReader(new InputStreamReader(
				this.p.getErrorStream()));
		List<String> linesStdErr = new ArrayList<String>();
		while (stderr.ready()) {
			linesStdErr.add(stderr.readLine());
		}
		stderr.close();
		stdout.close();
		errors = new ArrayList<TranslationError>();
		if(return_code == COMPILE_WITH_LINE_NUMBER_ERROR) {
			System.err.println("translate(): the translator returned errors");
			int num_line; int num_col;
			String message_error;
			for (String errMessage : linesStdErr) {
				System.err.println("translate(): "+errMessage);
				StringTokenizer tokenizer = new StringTokenizer(errMessage,":");
				num_line = Integer.parseInt(tokenizer.nextToken());
				num_col = Integer.parseInt(tokenizer.nextToken());
				message_error = tokenizer.nextToken();
				errors.add(new TranslationError(num_line,num_col,message_error));
			}
		}
		if(return_code == COMPILE_NO_LINE_NUMBER_ERROR) {
			System.err.println("translate(): the translator returned errors");
			for (String errMessage : linesStdErr) {
				System.err.println("translate(): "+errMessage);
				errors.add(new TranslationError(errMessage));
			}
		}
		if(return_code == OK) {
			// Nothing
		}
		return return_code == OK;
	}

	public Process getP(){
		return p;
	}

	/**
	 * Allows the user to get the path of the generated DIMACS file.
	 * @return the file path
	 */
	public String getSMTFilePath() {
		return currentPath+File.separatorChar+outputFilePath;
	}

	/**
	 * Allows the user to get a list of the errors generated by the translator.
	 * See the Error class.
	 * @return the list of warnings
	 */
	public List<TranslationError> getErrors() {
		return errors;
	}
}
