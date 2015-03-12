package solution;

public class SolverExecutionException extends Exception {
	private String solverError;

	public SolverExecutionException(String solverError) {
		super();
		this.solverError = solverError;
	}

	@Override
	public String getMessage() {
		return solverError;
	}

	@Override
	public String toString() {
		return "SolverExecutionException: " + solverError;
	}
}
