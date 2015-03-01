/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Map;

/**
 * This class
 * - launches the solver program with the DIMACS input
 * - if this DIMACS is satisfiable, gives an iterable Models
 * - gives a way to get the next Model and do the translation with the
 * literalsMap (DIMACS integers to literal names)
 *
 * the next model.
 * @author Abdel
 */
public class Solver {
	private Process p;
	private PrintWriter out;
	private String dimacsFilePath;
	private Map<Integer, String> literalsMap;

	public Solver(String dimacsFilePath) {
		this.dimacsFilePath = dimacsFilePath;
		this.literalsMap = null;
	}

	public Solver(String dimacsFilePath, Map<Integer, String> literalsMap) {
		this.dimacsFilePath = dimacsFilePath;
		this.literalsMap = literalsMap;
	}

	public Models launch() {
		return null;
	}

	/**
	 * ONLY used by ModelsIterator
	 * @return null if no more model (or if not satisfiable)
	 * @throws IOException
	 */
	protected Model nextModel() throws IOException {
		out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));
		out.println("1");
		out.flush();
		out.close();
		StringBuffer br = new StringBuffer();
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				p.getInputStream()));
		String line = "";
		while ((line = reader.readLine()) != null) {
			br.append(line + "\n");
		}
		return parseModel(br.toString().split(" "));
	}

	/**
	 *
	 * @param rawModelOutput The output
	 * @return a model with, if a literalMap was given, the translated literal.
	 * If no literalMap is given, the Model stores the literal as given by the
	 * solver (an integer).
	 */
	private Model parseModel(String[] rawModelOutput) {
		Model model = new Model();
		for (String rawLiteral : rawModelOutput) {
			if (literalsMap != null) {
				model.addLiteral(literalsMap.get(Integer.parseInt(rawLiteral)));
			} else {
				model.addLiteral(rawLiteral);
			}
		}
		return model;
	}

	public Process start(String dimacsFilesPath) throws IOException {
		this.p = Runtime.getRuntime().exec(
				"java -cp .:sat4j-sat.jar Minisat " + dimacsFilesPath);
		return p;
	}

	/**
	 * Kills the solver program process
	 */
	public void close() {
		out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));
		out.println("\n0");
		out.close();
		this.p.destroy();
	}
}
