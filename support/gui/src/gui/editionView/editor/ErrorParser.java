package gui.editionView.editor;

import java.util.List;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.parser.AbstractParser;
import org.fife.ui.rsyntaxtextarea.parser.DefaultParseResult;
import org.fife.ui.rsyntaxtextarea.parser.ParseResult;

import gui.TranslatorLatex.TranslationLatex;
import gui.editionView.EditionPanel;
import translation.TranslationError;

public class ErrorParser extends AbstractParser {
	private List<TranslationError> bufferErrors;
	public EditionPanel edition = null;

	public ErrorParser(EditionPanel e) {
		edition = e;
	}
	public ErrorParser() {}
	
	@Override // lang can be set using editor.setSyntaxEditingStyle
	public ParseResult parse(RSyntaxDocument code, String lang) {
		DefaultParseResult result = new DefaultParseResult(this);
		if((lang != "sat" && lang != "smt" && lang != "qbf") || code.getLength()==0)
			return result;
		if(bufferErrors == null) {
			try {
				TranslationLatex tr = new TranslationLatex(code.getText(0, code.getLength()), lang, true);
				bufferErrors = tr.getErrors();
				if (tr.getFormula().length() != 0 && edition != null) {
					edition.setLatex(tr.getFormula());
				}

			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		for (TranslationError error : bufferErrors) {
			result.addNotice(new ErrorParserNotice(this, error));
		}
		bufferErrors = null;
		return result;
	}
	
	/**
	 * When a call to the translator has been made somewhere else than using linter(),
	 * this function allows to use these errors instead of calling linter() when
	 * the Editor will call parser
	 * @param errors
	 */
	public void linterFromExisting(List<TranslationError> errors) {
		bufferErrors = errors;
	}
}
