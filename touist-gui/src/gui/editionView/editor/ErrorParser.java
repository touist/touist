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

	@Override
	public ParseResult parse(RSyntaxDocument code, String lang) {
		DefaultParseResult result = new DefaultParseResult(this);
		lang = "sat";
		if(lang == "sat" && code.getLength()!=0) {
			TranslatorSAT translator = null;
			List<String> options = new ArrayList<String>();
			options.add("--linter");
			translator = new TranslatorSAT("external"+File.separatorChar+"touistc",options);
			try {
				translator.translate(new StringReader(code.getText(0, code.getLength())));
				for (TranslationError error : translator.getErrors()) {
					result.addNotice(new ErrorParserNotice(this, error));
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return result;
	}
}
