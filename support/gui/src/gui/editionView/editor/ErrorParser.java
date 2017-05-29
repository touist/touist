package gui.editionView.editor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.parser.AbstractParser;
import org.fife.ui.rsyntaxtextarea.parser.DefaultParseResult;
import org.fife.ui.rsyntaxtextarea.parser.ParseResult;
import org.fife.ui.rsyntaxtextarea.parser.ParserNotice;

import solution.SolverExecutionException;
import solution.SolverQBF;
import translation.TranslationError;
import translation.TranslatorSAT;
import translation.TranslatorSMT;

public class ErrorParser extends AbstractParser {
	private List<TranslationError> bufferErrors; 
	
	
	@Override // lang can be set using editor.setSyntaxEditingStyle
	public ParseResult parse(RSyntaxDocument code, String lang) {
		DefaultParseResult result = new DefaultParseResult(this);
		if((lang != "sat" && lang != "smt" && lang != "qbf") || code.getLength()==0)
			return result;
		if(bufferErrors == null) {
			try {
				if(lang == "sat") {
					bufferErrors = linterSAT(new StringReader(code.getText(0, code.getLength())));
				} else if (lang == "smt") {
					bufferErrors = linterSMT(new StringReader(code.getText(0, code.getLength())));
				} else if (lang == "qbf") {
					bufferErrors = linterQBF(new StringReader(code.getText(0, code.getLength())));
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
	
	public List<TranslationError> linterSAT(StringReader s) throws IOException, InterruptedException {
		List<String> options = new ArrayList<String>();
		options.add("--linter");
		TranslatorSAT translator = new TranslatorSAT(options);
		translator.translate(s);
		return translator.getErrors();
	}
	public List<TranslationError> linterSMT(StringReader s) throws IOException, InterruptedException {
		List<String> options = new ArrayList<String>();
		options.add("--linter");
		TranslatorSMT translator = new TranslatorSMT(options);
		translator.translate(s, "QF_IDL"); // QF_IDL is an arbitrary SMT2 logic
		return translator.getErrors();
	}
	public List<TranslationError> linterQBF(StringReader s) throws IOException, InterruptedException {
		List<String> options = new ArrayList<String>();
		options.add("--linter");
		
		SolverQBF touist = new SolverQBF(new BufferedReader(s), options);
		touist.launch();
		touist.waitResult(10000);
		return touist.getErrors();
	}
}
