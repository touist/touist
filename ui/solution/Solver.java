/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.IOException;
import java.util.Map;

/**
 * A sub-class must implement Solver. The inherited class allows the user to
 * launch the solver program with the right DIMACS input, to give the user an
 * iterable Models (or null if no model found) and to stop the solver program
 * (that is running background).
 *
 * @author Abdel
 * @modified Maël
 */
public abstract class Solver {
	private String dimacsFilePath;
	private Map<Integer, String> literalsMap; // "table de correspondance"

	/**
	 * This constructor is useful when the user wants to solve a problem without
	 * using a "literalsMap" ("table de correspondance"). Hence the user has
	 * only to pass a DIMACS file path.
	 * @param dimacsFilePath
	 */
	public Solver(String dimacsFilePath) {
		this.dimacsFilePath = dimacsFilePath;
		this.literalsMap = null;
	}

	/**
	 * This is the main constructor used by the user after he translated the
	 * BIGAND file to a DIMACS file (and the "literalsMap" associated).
	 * @param dimacsFilePath the DIMACS file
	 * @param literalsMap the "literals map" ("table de correspondance")
	 */
	public Solver(String dimacsFilePath, Map<Integer, String> literalsMap) {
		this.dimacsFilePath = dimacsFilePath;
		this.literalsMap = literalsMap;
	}

	/**
	 * Launch the solver program in background. Does not check satisfiability
	 * but waits for the ModelsIterator.hasNext() to retrieve the next model.
	 * @param dimacsFilesPath The DIMACS file path
	 * @return the iterable Models instance.
	 * @throws IOException
	 */
	public abstract Models launch() throws IOException;

	/**
	 * Kills the solver program process
	 */
	public abstract void close();

	/**
	 * ONLY used by ModelsIterator
	 * @return null if no more model (or if not satisfiable)
	 * @throws IOException
	 */
	protected abstract Model nextModel() throws IOException;

	/**
	 *
	 * @param rawModelOutput The output
	 * @return a model with, if a literalMap was given, the translated literal.
	 * If no literalMap is given, the Model stores the literal as given by the
	 * solver (an integer).
	 */
	protected abstract Model parseModel(String[] rawModelOutput);

	protected String getDimacsFilePath() {
		return dimacsFilePath;
	}

	protected Map<Integer, String> getLiteralsMap() {
		return literalsMap;
	}
}
