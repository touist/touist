/*
 * 04/27/2010
 *
 * TouistlTokenMaker.java - Scanner for Touistl.
 * 
 * This library is distributed under a modified BSD license.  See the included
 * RSyntaxTextArea.License.txt file for details.
 */
package gui.editionView.editor;

import java.io.*;
import javax.swing.text.Segment;

import org.fife.ui.rsyntaxtextarea.*;


%%

%public
%class TouistlTokenMaker
%extends AbstractJFlexCTokenMaker
%unicode
%type org.fife.ui.rsyntaxtextarea.Token


%{


	/**
	 * Constructor.  This must be here because JFlex does not generate a
	 * no-parameter constructor.
	 */
	public TouistlTokenMaker() {
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param tokenType The token's type.
	 * @see #addToken(int, int, int)
	 */
	private void addHyperlinkToken(int start, int end, int tokenType) {
		int so = start + offsetShift;
		addToken(zzBuffer, start,end, tokenType, so, true);
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param tokenType The token's type.
	 */
	private void addToken(int tokenType) {
		addToken(zzStartRead, zzMarkedPos-1, tokenType);
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param tokenType The token's type.
	 * @see #addHyperlinkToken(int, int, int)
	 */
	private void addToken(int start, int end, int tokenType) {
		int so = start + offsetShift;
		addToken(zzBuffer, start,end, tokenType, so, false);
	}


	/**
	 * Adds the token specified to the current linked list of tokens.
	 *
	 * @param array The character array.
	 * @param start The starting offset in the array.
	 * @param end The ending offset in the array.
	 * @param tokenType The token's type.
	 * @param startOffset The offset in the document at which this token
	 *                    occurs.
	 * @param hyperlink Whether this token is a hyperlink.
	 */
	@Override
	public void addToken(char[] array, int start, int end, int tokenType,
						int startOffset, boolean hyperlink) {
		super.addToken(array, start,end, tokenType, startOffset, hyperlink);
		zzStartRead = zzMarkedPos;
	}


	/**
	 * {@inheritDoc}
	 */
	public String[] getLineCommentStartAndEnd(int languageIndex) {
		return new String[] { "//", null };
	}


	/**
	 * Returns the first token in the linked list of tokens generated
	 * from <code>text</code>.  This method must be implemented by
	 * subclasses so they can correctly implement syntax highlighting.
	 *
	 * @param text The text from which to get tokens.
	 * @param initialTokenType The token type we should start with.
	 * @param startOffset The offset into the document at which
	 *        <code>text</code> starts.
	 * @return The first <code>Token</code> in a linked list representing
	 *         the syntax highlighted text.
	 */
	public Token getTokenList(Segment text, int initialTokenType, int startOffset) {

		resetTokenList();
		this.offsetShift = -text.offset + startOffset;

		// Start off in the proper state.
		int state = Token.NULL;
		switch (initialTokenType) {
			default:
				state = Token.NULL;
		}

		s = text;
		try {
			yyreset(zzReader);
			yybegin(state);
			return yylex();
		} catch (IOException ioe) {
			ioe.printStackTrace();
			return new TokenImpl();
		}

	}


	/**
	 * Refills the input buffer.
	 *
	 * @return      <code>true</code> if EOF was reached, otherwise
	 *              <code>false</code>.
	 */
	private boolean zzRefill() { // Patched version to keep!
		return zzCurrentPos>=s.offset+s.count;
	}


	/**
	 * Resets the scanner to read from a new input stream.
	 * Does not close the old reader.
	 *
	 * All internal variables are reset, the old input stream 
	 * <b>cannot</b> be reused (internal buffer is discarded and lost).
	 * Lexical state is set to <tt>YY_INITIAL</tt>.
	 *
	 * @param reader   the new input stream 
	 */
	public final void yyreset(java.io.Reader reader) { // Patched version to keep!
		// 's' has been updated.
		zzBuffer = s.array;
		/*
		 * We replaced the line below with the two below it because zzRefill
		 * no longer "refills" the buffer (since the way we do it, it's always
		 * "full" the first time through, since it points to the segment's
		 * array).  So, we assign zzEndRead here.
		 */
		//zzStartRead = zzEndRead = s.offset;
		zzStartRead = s.offset;
		zzEndRead = zzStartRead + s.count - 1;
		zzCurrentPos = zzMarkedPos = zzPushbackPos = s.offset;
		zzLexicalState = YYINITIAL;
		zzReader = reader;
		zzAtBOL  = true;
		zzAtEOF  = false;
	}


%}

Digits 		= [0-9]
Alpha 		= [a-zA-Z]
Empty 		= [ \t\f]+
Special 	= [_]
Identifier 	= ({Special}|{Digits})*{Alpha}({Alpha}|{Special}|{Digits})*
Integer 	= {Digits}+
Double		= {Digits}+\.{Digits}+
Var 		= (\${Identifier})|(\?{Identifier})
Comment     = ";;"[^\n]*

%%

<YYINITIAL> {   

   
	/* Keywords */   
	"data" |   
	"if" |
	"then" |
	"else"  |
	"when"
			{ addToken(Token.RESERVED_WORD); }    

	/* Functions */   
	"let" |
	"bigand" |   
	"bigor" |   
	"in" |   
	"not" |
	"or" |
	"and" |
	"empty" |
	"subset" |
	"card" |
	"int" |	
	"sqrt" |
	"mod"|
	"end" |
	"atleast" |
	"exact" |
	"atmost" |
	"inter" |
    "union" |
    "subset" |
    "diff" |
    "forall" |
	"exists" { addToken(Token.FUNCTION); }
   
   
	{Empty}				{ addToken(Token.WHITESPACE); }   
   
	"true" |
	"false" |
	"Top"	|
	"Bot"					{ addToken(Token.LITERAL_BOOLEAN); }

   	{Var}					{ addToken(Token.VARIABLE); }



	/* Operators. */   
	"+" | "," | "-" |
	".." |
	"/" | "!=" | ":" | "<" | "=" | "==" | ">" | "=>" | "<=>" | "<=" |
	"[" | "]" | "*" 	{ addToken(Token.OPERATOR); }
	
	"(" | ")" { addToken(Token.SEPARATOR); }
   
	/* Numbers */   
	{Integer}			{ addToken(Token.LITERAL_NUMBER_DECIMAL_INT); }   
   	{Double}			{ addToken(Token.LITERAL_NUMBER_FLOAT); }   


   	{Comment} | "\\\\"	{ addToken(Token.COMMENT_MARKUP); }

	{Identifier}				{ addToken(Token.IDENTIFIER);}

	/* Ended with a line not in a string or comment. */   
	\n |   
	<<EOF>>						{ addNullToken(); return firstToken; }   
   
	/* Catch any other (unhandled) characters. */   

	({Alpha}|{Digits})+							{ addToken(Token.ERROR_IDENTIFIER); }   
    .											{ addToken(Token.ERROR_IDENTIFIER); }
}   
   
