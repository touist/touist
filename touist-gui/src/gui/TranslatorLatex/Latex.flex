import java_cup.runtime.*;
%%
%class Lexi
%unicode
%line
%column
%cup
%eofval{
  return new java_cup.runtime.Symbol(<CUPSYM>.EOF);
%eofval}
%eofclose
%{
	private Symbol symbol(int type) {
		return new Symbol(type, yyline, yycolumn);
	}
	private Symbol symbol(int type, Object value) {
		return new Symbol(type, yyline, yycolumn, value);
	}
%}


Digits 		= [0-9]
Alpha 		= [a-zA-Z]
Empty 		= [ \r\n\t\f]+
Special 	= [_]
Newline 	= [\r\n] | (\r\n)
Identifier 	= ({Special}|{Digits})*{Alpha}({Alpha}|{Special}|{Digits})*
Integer 	= {Digits}+
Double		= {Digits}+\.{Digits}+
Var 		= (\${Identifier})|(\?{Identifier})
Nb 			= {Integer}|{Double}
Comment     = ";;"[^\n]*

%%

	"begin"			{return symbol(sym.BEGIN); }
	"end"			{ return symbol(sym.END); }
	"sets"			{ return symbol(sym.SETS); }
	"formula"		{ return symbol(sym.FORMULA); }
	"if"			{ return symbol(sym.IF); }
	"then"			{ return symbol(sym.THEN); }
	"else"			{ return symbol(sym.ELSE); }
    "when" 			{ return symbol(sym.WHEN); }
   
	/* Data types */   
	"constant"		{ return symbol(sym.CONSTANT); }
	"set"			{ return symbol(sym.SET); }
	"float"			{ return symbol(sym.FLOAT); }
	"int"			{ return symbol(sym.INT); }
   
   
	/* Functions */   
	"bigand"		{ return symbol(sym.BIGAND); }
	"bigor"			{ return symbol(sym.BIGOR); }
	"in"			{ return symbol(sym.IN); }
	"not"			{ return symbol(sym.NOT); }
	"or"			{ return symbol(sym.OR); }
	"xor" 			{ return symbol(sym.XOR); }
	"and"			{ return symbol(sym.AND); }
	"empty"			{ return symbol(sym.EMPTY); }
	"subset"		{ return symbol(sym.SUBSET); }
	"card"			{ return symbol(sym.CARD); }
	"sqrt"			{ return symbol(sym.SQRT); }
	"mod"			{ return symbol(sym.MOD); }
   	"union"			{ return symbol(sym.UNION); }
   	"inter"			{ return symbol(sym.INTER); }
   	"diff"			{ return symbol(sym.DIFF); }

   
   	";;"[^\n]*		{}
	{Empty}			{ }
    //{Newline} 		{ return symbol(sym.NEWLINE); }
   
	"true"			{ return symbol(sym.TRUE); }
	"false"			{ return symbol(sym.FALSE); }
	"Top"			{ return symbol(sym.TOP); }
	"Bot"			{ return symbol(sym.BOT); }


   	{Var}			{ return symbol(sym.VAR,new String(yytext())); }


   	"."				{ return symbol(sym.DOT); }
   	"="				{ return symbol(sym.AFFECT); }
   	"=="			{ return symbol(sym.EQUAL); }
   	"!="			{ return symbol(sym.DIFFERENT);}
   	"<"				{ return symbol(sym.LESSER);}
   	"<="			{ return symbol(sym.LESSER_OR_EQUAL);}
   	">"				{ return symbol(sym.GREATER);}
   	">="			{ return symbol(sym.GREATER_OR_EQUAL);}
   	"&&"			{ return symbol(sym.BOOL_AND);}
   	"||"			{ return symbol(sym.BOOL_OR);}
   	"=>"			{ return symbol(sym.IMPLIQUE);}
   	"<=>"			{ return symbol(sym.EQUIV);}
   	"("				{ return symbol(sym.LPAR);}
   	")"				{ return symbol(sym.RPAR);}
   	"+"				{ return symbol(sym.ADD);}
   	"*"				{ return symbol(sym.MULTIPLY);}
   	"-"				{ return symbol(sym.SUBSTRACT);}
   	"/"				{ return symbol(sym.DIVIDE);}
   	"["				{ return symbol(sym.LCRO);}
   	"]"				{ return symbol(sym.RCRO);}
   	".."			{ return symbol(sym.DODOT);}
   	","				{ return symbol(sym.COMMA);}
   	":"				{ return symbol(sym.TWODOT);}		// todo: verify twodot term

   
	/* Numbers */   
	{Integer}			{ return symbol(sym.INTEGER,new String(yytext())); }
   
   	{Double}			{ return symbol(sym.DOUBLENUMBER,new String(yytext())); }
   


   	{Comment}			{ return symbol(sym.COMMENT,new String(yytext())); }


	{Identifier}			{ return symbol(sym.IDENTIFIER,new String(yytext())); }


 	<<EOF>>						{ return symbol(sym.EOF); }
	/* Catch any other (unhandled) characters. */   

	{Alpha}				{ return symbol(sym.ALPHA,new String(yytext())); }
   
    .					{ return symbol(sym.ERROR_IDENTIFIER,new String(yytext())); }