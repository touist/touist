package translation;

import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;


/**
 * This class holds one error that might be generated by the translator,
 * with the format (row,column,error_text)
 * @author maelv
 */
public class TranslationError {
	private int rowInCode;
	private int columnInCode;
	private boolean rowAndColumnKnown,posStartEndKnown;
	private int posStart,posEnd; // absolute positions in token flow
	private String errorMessage;

	public TranslationError(int rowInCode, int columnInCode, 
			int posStart, int posEnd, String errorMessage) {
		this.rowInCode = rowInCode;
		this.columnInCode = columnInCode;
		this.errorMessage = errorMessage;
		this.posStart = posStart;
		this.posEnd = posEnd;
		rowAndColumnKnown = true;
		posStartEndKnown = true;
	}
	
	public TranslationError(int rowInCode, int columnInCode, String errorMessage) {
		this.rowInCode = rowInCode;
		this.columnInCode = columnInCode;
		this.errorMessage = errorMessage;
		rowAndColumnKnown = true;
		posStartEndKnown = false;
	}
	
	public TranslationError(String errorMessage) {
		this.errorMessage = errorMessage;
		rowAndColumnKnown = false;
		posStartEndKnown = false;
	}

	@Override
	public String toString() {
		return (hasRowAndColumn())
				?"line "+rowInCode+", col "+columnInCode+": "+errorMessage
				:errorMessage;
	}

	public boolean hasRowAndColumn() {
		return rowAndColumnKnown;
	}
	
	/**
	 * posFromStart and posLineFromStart are known
	 */
	public boolean hasPosStartEnd() {
		return posStartEndKnown;
	}
	
	public int getRowInCode() {
		return rowInCode;
	}

	public boolean isPosFromStartKnown() {
		return posStartEndKnown;
	}

	public int getPosStart() {
		return posStart;
	}
	public int getPosEnd() {
		return posEnd;
	}

	public int getColumnInCode() {
		return columnInCode;
	}

	public String getErrorMessage() {
		return errorMessage;
	}
	
	/**
	 * Takes what is outputed by touistc and translates into error messages
	 * TODO: for now, only ONE error is parsed!
	 * @param error
	 * @return
	 */
	public static ArrayList<TranslationError> parse(String errorOutput) {
		ArrayList<TranslationError> errors = new ArrayList<TranslationError>();
		StringTokenizer tokenizer = new StringTokenizer(errorOutput,":");
		ArrayList<Integer> pos = new ArrayList<Integer>();
		String cur = "";
		Boolean ok = true;
		for (int i = 0; i < 5 && ok; i++) {
			try {
				cur = tokenizer.nextToken();
				pos.add(Integer.parseInt(cur));
			} catch (Exception e) {
				ok = false;
			}
		}
		while(tokenizer.hasMoreTokens()) { cur += ":"+tokenizer.nextToken(); }
		if(pos.size()==2) { // meaning that only 23:10: has been returned
			errors.add(new TranslationError(pos.get(0),pos.get(1),cur));
		} 
		else if(pos.size()==4) { // meaning that 23:10:465:470: has been returned
			// format: 'num_line:num_col:token_start:token_end:' 
			errors.add(new TranslationError(pos.get(0),pos.get(1),pos.get(2),pos.get(3),cur));
		}
		else {
			errors.add(new TranslationError(cur));
		}
		return errors;
	}
}