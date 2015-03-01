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
 * This class is a first implementation of the "Solver" abstract class. It
 * allows the user to use the "sat4j-sat.jar" program that Abdel prepared for
 * testing purpose.
 *
 * the next model.
 * @author Abdel
 */
public class SolverTestSAT4J extends Solver {
	private Process p;
	private PrintWriter out;

	public SolverTestSAT4J(String dimacsFilePath) {
		super(dimacsFilePath);
		this.p = null;
		this.out = null;
	}

	public SolverTestSAT4J(String dimacsFilePath,
			Map<Integer, String> literalsMap) {
		super(dimacsFilePath, literalsMap);
		this.p = null;
		this.out = null;
	}

	@Override
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

	@Override
	protected Model parseModel(String[] rawModelOutput) {
		Model model = new Model();
		for (String rawLiteral : rawModelOutput) {
			if (getLiteralsMap() != null) {
				model.addLiteral(getLiteralsMap().get(
						Integer.parseInt(rawLiteral)));
			} else {
				model.addLiteral(rawLiteral);
			}
		}
		return model;
	}

	@Override
	public Models launch() throws IOException {
		Models theIterableModels = new Models(this);
		this.p = Runtime.getRuntime().exec(
				"java -cp .:sat4j-sat.jar Minisat " + getDimacsFilePath());
		return theIterableModels;
	}

	@Override
	public void close() {
		out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));
		out.println("\n0");
		out.close();
		this.p.destroy();
	}
}
