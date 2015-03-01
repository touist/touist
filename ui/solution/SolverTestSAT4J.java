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
import java.util.Iterator;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

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


	public SolverTestSAT4J(String dimacsFilePath) {
		super(dimacsFilePath);
		this.p = null;
		this.stdin = null;
	}

	public SolverTestSAT4J(String dimacsFilePath,
			Map<Integer, String> literalsMap) {
		super(dimacsFilePath, literalsMap);
		this.p = null;
		this.stdin = null;
	}

	@Override
	protected Model nextModel() throws IOException {
		stdin.println("1");
		stdin.flush();
		stdin.close();
		StringBuffer br = new StringBuffer();
		String line = "";
		while ((line = stdout.readLine()) != null) {
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
		// TODO We should be warned if the "java -cp" command fails because it
		// can't find the files
		Models theIterableModels = new Models(this);
		String command = "java -cp .:sat4j-sat.jar Minisat " + getDimacsFilePath();
		this.p = Runtime.getRuntime().exec(command);
		stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		stdout = new BufferedReader(new InputStreamReader(p.getInputStream()));
		stdin = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));

		System.out.println(stdout.readLine());
		try {
			if(p.waitFor(1, TimeUnit.SECONDS) && p.exitValue()==1) {
				System.out.println(command);
				System.out.println(stderr.readLine());
			} else {
				System.out.println("Le programme sat4j-sat.jar semble s'être lancé");
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		return theIterableModels;
	}

	@Override
	public void close() {
		stdin = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
				p.getOutputStream())));
		stdin.println("\n0");
		stdin.close();
		this.p.destroy();
	}

	public static void main(String[] args) {
		SolverTestSAT4J solverInterfaceWrittenByAbdel = new SolverTestSAT4J(
				"/Users/maelv/prog/touist/bin/term1_gr_2pin_w4.shuffled.cnf");
		Models models = null;
		try {
			models = solverInterfaceWrittenByAbdel.launch();
		} catch (IOException e) {
			e.printStackTrace();
		}

		Iterator<Model> it = models.iterator();
		if (it.hasNext()) {
			System.out.println("Satisfiable");
		} else {
			System.out.println("Insatisfiable");
		}
		Scanner sc = new Scanner(System.in);
		String answer;
		boolean continuer = true;
		Model m;

		while (it.hasNext() && continuer) {
			System.out.println("Model : ");
			m = it.next();
			m.toString();

			System.out.println("Contiuner ? o/n");
			answer = sc.nextLine();
			continuer = (answer.charAt(0) == 'o');
		}
		sc.close();
		solverInterfaceWrittenByAbdel.close();

	}
}
