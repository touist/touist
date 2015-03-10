package solution;

public class SolverExecutionException extends Exception {
	private String solverError;

	public SolverExecutionException(String solverError) {
		super();
		this.solverError = solverError;
	}

	@Override
	public String toString() {
		return "SolverException: " + solverError;
	}
}
