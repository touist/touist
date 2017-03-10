package gui.editionView.editor;

import java.awt.Color;

import org.fife.ui.rsyntaxtextarea.parser.Parser;
import org.fife.ui.rsyntaxtextarea.parser.ParserNotice;

import translation.TranslationError;
import translation.TranslationError.Type;

/**
 * An error notice
 * @author mvalais
 * Inspired from https://github.com/peq/pscript-lang/, 
 * file: Editor/src/wursteditor/rsyntax/WurstParserNotice.java
 */
public class ErrorParserNotice implements ParserNotice {

	private TranslationError err;
	private ErrorParser parser;
	
	public ErrorParserNotice(ErrorParser parser, TranslationError e) {
		this.parser = parser;
		this.err = e;
	}
	
	@Override
	public int compareTo(ParserNotice o) {
		return 0;
	}

	@Override
	public boolean containsPosition(int i) {
		return err.getPosStart()-1 <= i && i <= err.getPosEnd()+1;
	}

	@Override
	public Color getColor() {
		return (err.getType() == Type.ERROR)
				? Color.RED : Color.BLUE;
	}

	@Override
	public boolean getKnowsOffsetAndLength() {
		return err.hasRowAndColumn() && err.hasPosStartEnd();
	}

	@Override
	public int getLength() {
		return err.getPosEnd()-err.getPosStart();
	}

	@Override
	public Level getLevel() {
		return err.getType() == Type.ERROR ?
				ParserNotice.Level.ERROR : ParserNotice.Level.WARNING;
	}

	@Override
	public int getLine() {
		return err.getRowInCode();
	}

	@Override
	public String getMessage() {
		return err.getErrorMessage();
	}

	@Override
	public int getOffset() {
		return err.getPosStart();
	}

	@Override
	public Parser getParser() {
		return parser;
	}

	@Override
	public boolean getShowInEditor() {
		return true;
	}

	@Override
	public String getToolTipText() {
		if(getKnowsOffsetAndLength()) {
			return "line "+err.getRowInCode()+", col "+err.getColumnInCode()+": "+err.getTypeString()+": "+getMessage();
		}
		else {
			return "unknown line and col: "+getMessage();
		}
	}

}
