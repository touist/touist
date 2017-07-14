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

package translation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import touist.TouIST;

/**
 * @author Abdel
 * @Modified by Mael
 */
public class TranslatorSAT {
	final private String outputFilePath = touist.TouIST.getWhereToSave() + File.separator + "out.cnf";
	final private String outputTableFilePath = touist.TouIST.getWhereToSave() + File.separator + "out.table";
	private Map<Integer,String> literalsMap = new HashMap<Integer,String>();
	private List<TranslationError> errors = new ArrayList<TranslationError>();
	private Process p;
	private List<String> options = new ArrayList<String>();

	public TranslatorSAT() {
	}
	public TranslatorSAT(List<String> options) {
		this.options = options;
	}

	public boolean translate(String touistlFilePath) throws IOException, InterruptedException {
		BufferedReader reader = new BufferedReader(new FileReader(touistlFilePath));
		return translate(reader); 
	}

	public boolean translate(StringReader str) throws IOException, InterruptedException {
		BufferedReader reader = new BufferedReader(str);
		return translate(reader); 
	}
	/**
	 * Calls the translator/compiler to transform the ".bigand" file to a
	 * ".dimacs" file (along with a "mapping" file). This method also calls the
	 * parsing methods parseErrors and (if the translation actually
	 * passed) parseLiteralsMapFile.
	 *
	 * @param touistlFilePath is the name of the file that the
	 * translator/compiler is going to compute. The translation is handled by a
	 * third-part program called by an system "exec" command. The translator can
	 * return three kind of things : - A ".dimacs" file and a "mapping" file if
	 * everything goes well. The mapping file allows the user to get the
	 * original litteral name instead of the integer notation used by the
	 * ".dimacs" file. Note that it can be used after the solver gives results.
	 * - If anything goes wrong (syntax error, semantic error, wrong type...),
	 * the translator gives his error message on the "stderr" file.
	 *
	 * @return true if the translation went well (migth have generated some
	 * warning though), false of fatal errors (syntax/semantic errors) happened
	 *
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public boolean translate(BufferedReader reader) throws IOException, InterruptedException {
		/* return_code from the Touistl translator (see touist.ml):
  		| OK -> 0
  		| COMPILE_WITH_LINE_NUMBER_ERROR -> 1
  		| COMPILE_NO_LINE_NUMBER_ERROR -> 2
  		| OTHER -> 3
		 */
		final int OK = 0;
		final int COMPILE_WITH_LINE_NUMBER_ERROR = 1;
		final int COMPILE_NO_LINE_NUMBER_ERROR = 2;
		final int OTHER = 3;
		/*
		 * Syntax of errors COMPILE_WITH_LINE_NUMBER_ERROR:
		 * num_row:num_col: message
		 */
		// Check if translatorProgramFilePath is there
		
		String pathtouist = touist.TouIST.getTouistBin();

		List<String> cmd = new ArrayList<String>();
		
		cmd.add(pathtouist);
		cmd.add("--sat");
		cmd.add("-");
		cmd.add("--table");
		cmd.add(outputTableFilePath);
		cmd.add("-o");
		cmd.add(outputFilePath);
		cmd.add("--error-format");
		cmd.add("%l:%c:%b:%B: %t: %m");
		cmd.addAll(options);
		
        System.out.println("translate(): cmd executed: "+cmd.toString());
		
        this.p = Runtime.getRuntime().exec(cmd.toArray(new String[0]));

        BufferedWriter toProcess = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
        String s = "";
        while ((s = reader.readLine())!=null) {
        	toProcess.write(s + "\n");
        }
        toProcess.flush();
        toProcess.close();
		
        int return_code = p.waitFor();
        
		BufferedReader fromProcess = new BufferedReader(new InputStreamReader(p.getInputStream()));
		List<String> linesStdout = new ArrayList<String>();
		while (fromProcess.ready())
			linesStdout.add(fromProcess.readLine());

		BufferedReader fromProcessErr = new BufferedReader(new InputStreamReader(
				this.p.getErrorStream()));
		String linesStdErr = "";
		while (fromProcessErr.ready()) {
			linesStdErr += fromProcessErr.readLine() + "\n";
		}
		fromProcessErr.close();
		fromProcess.close();

		errors = TranslationError.parse(linesStdErr);
		
		if(return_code == OK) {
			parseLiteralsMapFile(outputTableFilePath);
		}
		return return_code == OK;
	}

	/**
	 * Allows the user to get the literalsMap that contains the matching table
	 * between the DIMACS integers and the real literals names. This map is used
	 * by the Solver instance.
	 * @warning This table is NOT the same as the table returned by Solver class
	 * @return the map
	 */
	public Map<Integer,String> getLiteralsMap() {
		return literalsMap;
	}
        
        public Process getP(){
            return p;
        }

	/**
	 * Allows the user to get the path of the generated DIMACS file.
	 * @return the file path
	 */
	public String getDimacsFilePath() {
		return outputFilePath;
	}

	/**
	 * Allows the user to get a list of the errors generated by the translator.
	 * See the Error class.
	 * @return the list of warnings
	 */
	public List<TranslationError> getErrors() {
		return errors;
	}

	/**
	 * Method used by Translator.translate() to parse the literals map file that
	 * holds the matching names for the given DIMACS integers.
	 * @param literalsMapFilePath
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	private void parseLiteralsMapFile(String literalsMapFilePath)
			throws FileNotFoundException, IOException {
		/*
		 * Example of Table/map file:
		 * 		A(b) 3
		 * 		B(a) 4
		 * 		C(a) 1
		 * 		B(b) 2
		 */
		File TR = new File(literalsMapFilePath);
		BufferedReader br = new BufferedReader(new FileReader(TR));
		String line = "";
		while (br.ready()) {
			line = br.readLine();
                        int sep = line.lastIndexOf(' ');
			String literalString = line.substring(0,sep);
			int literalCode = Integer.parseInt(line.substring(sep+1));
			literalsMap.put(literalCode, literalString);
		}
		br.close();
	}
}
