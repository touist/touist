/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package translation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

/**
 * @author Skander
 * @Modified by Abdel
 */
public class Translator {
	private Process p;
	private String dimacsFilePath;
	private Map<Integer, String> literalsMap;
	private List<Error> errors;
	private List<Warning> warnings;

	public class Message {
		private int rowInCode;
		private int columnInCode;
		private String sampleOfCode;

		public Message(int rowInCode, int columnInCode, String sampleOfCode) {
			this.rowInCode = rowInCode;
			this.columnInCode = columnInCode;
			this.sampleOfCode = sampleOfCode;
		}

		@Override
		public String toString() {
			// TODO enhance this message display
			return "Message [rowInCode=" + rowInCode + ", columnInCode="
					+ columnInCode + ", sampleOfCode=" + sampleOfCode + "]";
		}
	}

	public class Error extends Message {
		public Error(int rowInCode, int columnInCode, String sampleOfCode) {
			super(rowInCode, columnInCode, sampleOfCode);
		}

	}

	public class Warning extends Message {
		public Warning(int rowInCode, int columnInCode, String sampleOfCode) {
			super(rowInCode, columnInCode, sampleOfCode);
		}
	}

	/**
	 * XXX Que fait cette fonction exactement ?
	 * @return
	 */
	public Translator() {
		this.dimacsFilePath = null;
	}

	/**
	 * Calls the translator/compiler to transform the ".bigand" file to a
	 * ".dimacs" file (along with a "mapping" file).
	 *
	 * @param bigandFilePath is the name of the file that the
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
	 */
	public boolean traduire(String bigandFilePath) throws IOException {
		// Heyy Olivier, Get Commande to Run Ocaml Program like this
		this.p = Runtime.getRuntime().exec("ocamlbuild ......");
		StringBuffer br = new StringBuffer();
		// Response will put 2Lines: first contain dimacs path & seconde TR path
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				p.getInputStream()));
		String line = "";
		while ((line = reader.readLine()) != null) {
			br.append(line + "\n");
		}
		dimacsFilePath = br.toString().split("\n")[0];
		parseLiteralsMapFile(br.toString().split("\n")[1]);
		return false; // XXX
	}

	public Map<Integer, String> getLiteralsMap() {
		// TODO Parse the literal map file
		return literalsMap;
	}

	public String getDimacsFilePath() {
		return dimacsFilePath;
	}

	public List<Warning> getWarnings() {
		if (errors == null)
			parseErrorsAndWarnings();
		return warnings;
	}

	public List<Error> getErrors(String stderr) {
		if (errors == null)
			parseErrorsAndWarnings();
		return errors;
	}

	private void parseErrorsAndWarnings() {
		// TODO
	}

	/**
	 * XXX Can someone explain what this method is meant to do?
	 * @param literalsMapFilePath
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	private void parseLiteralsMapFile(String literalsMapFilePath)
			throws FileNotFoundException, IOException {
		File TR = new File(literalsMapFilePath);
		BufferedReader br = new BufferedReader(new FileReader(TR));
		String line = "";
		while ((line = br.readLine()) != null) {
			literalsMap.put(Integer.parseInt(line.split(" ")[0]),
					line.split(" ")[1]);
		}
		br.close();
	}

}
