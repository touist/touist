package gui.editionView.editor;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.swing.text.BadLocationException;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.parser.AbstractParser;
import org.fife.ui.rsyntaxtextarea.parser.DefaultParseResult;
import org.fife.ui.rsyntaxtextarea.parser.ParseResult;

import translation.TranslationError;
import translation.TranslatorSAT;

public class ErrorParser extends AbstractParser {
	private List<TranslationError> bufferErrors;
	
	
	@Override
	public ParseResult parse(RSyntaxDocument code, String lang) {
		DefaultParseResult result = new DefaultParseResult(this);
		lang = "sat";
		if(lang != "sat" || code.getLength()==0)
			return result;
		if(bufferErrors == null) {
			try {
				bufferErrors = linter(new StringReader(code.getText(0, code.getLength())));
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
	
	public List<TranslationError> linter(StringReader s) throws IOException, InterruptedException {
		TranslatorSAT translator = null;
		List<String> options = new ArrayList<String>();
		options.add("--linter");
		translator = new TranslatorSAT(options);
		translator.translate(s);
		return translator.getErrors();
	}
}
