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

	"begin"			{ System.out.println(yytext()+" BEGIN");return symbol(sym.BEGIN); }
	"end"			{ System.out.println(yytext()+" END");return symbol(sym.END); }
	"sets"			{ System.out.println(yytext()+" SETS");return symbol(sym.SETS); }
	"formula"		{ System.out.println(yytext()+" FORMULA");return symbol(sym.FORMULA); }
	"if"			{ System.out.println(yytext()+" IF");return symbol(sym.IF); }
	"then"			{ System.out.println(yytext()+" THEN");return symbol(sym.THEN); }
	"else"			{ System.out.println(yytext()+" ELSE");return symbol(sym.ELSE); }
    "when" 			{ System.out.println(yytext()+" WHEN");return symbol(sym.WHEN); }
   
	/* Data types */   
	"constant"		{ System.out.println(yytext()+" CONSTANT");return symbol(sym.CONSTANT); }
	"set"			{ System.out.println(yytext()+" SET");return symbol(sym.SET); }
	"float"			{ System.out.println(yytext()+" FLOAT");return symbol(sym.FLOAT); }
	"int"			{ System.out.println(yytext()+" INT");return symbol(sym.INT); }
   
   
	/* Functions */   
	"bigand"		{ System.out.println(yytext()+" BIGAND");return symbol(sym.BIGAND); }
	"bigor"			{ System.out.println(yytext()+" BIGOR");return symbol(sym.BIGOR); }
	"in"			{ System.out.println(yytext()+" IN");return symbol(sym.IN); }
	"not"			{ System.out.println(yytext()+" NOT");return symbol(sym.NOT); }
	"or"			{ System.out.println(yytext()+" OR");return symbol(sym.OR); }
	"xor" 			{ System.out.println(yytext()+" XOR");return symbol(sym.XOR); }
	"and"			{ System.out.println(yytext()+" AND");return symbol(sym.AND); }
	"empty"			{ System.out.println(yytext()+" EMPTY");return symbol(sym.EMPTY); }
	"subset"		{ System.out.println(yytext()+" SUBSET");return symbol(sym.SUBSET); }
	"card"			{ System.out.println(yytext()+" CARD");return symbol(sym.CARD); }
	"sqrt"			{ System.out.println(yytext()+" SQRT");return symbol(sym.SQRT); }
	"mod"			{ System.out.println(yytext()+" MOD");return symbol(sym.MOD); }
   	"union"			{ System.out.println(yytext()+" UNION");return symbol(sym.UNION); }
   	"inter"			{ System.out.println(yytext()+" INTER");return symbol(sym.INTER); }
   	"diff"			{ System.out.println(yytext()+" DIFF");return symbol(sym.DIFF); }

   
	{Empty}			{  }
    //{Newline} 		{ System.out.println(yytext()+" NEWLINE");return symbol(sym.NEWLINE); }
   
	"true"			{ System.out.println(yytext()+" TRUE");return symbol(sym.TRUE); }
	"false"			{ System.out.println(yytext()+" FALSE");return symbol(sym.FALSE); }
	"Top"			{ System.out.println(yytext()+" TOP");return symbol(sym.TOP); }
	"Bot"			{ System.out.println(yytext()+" BOT");return symbol(sym.BOT); }


   	{Var}			{ System.out.println(yytext()+" VAR");return symbol(sym.VAR,new String(yytext())); }


   	"."				{ System.out.println(yytext()+" DOT");return symbol(sym.DOT); }
   	"="				{ System.out.println(yytext()+" AFFECT");return symbol(sym.AFFECT); }
   	"=="			{ System.out.println(yytext()+" EQUAL");return symbol(sym.EQUAL); }
   	"!="			{ System.out.println(yytext()+" DIFFERENT");return symbol(sym.DIFFERENT);}
   	"<"				{ System.out.println(yytext()+" LESSER");return symbol(sym.LESSER);}
   	"<="			{ System.out.println(yytext()+" LESSER_OR_EQUAL");return symbol(sym.LESSER_OR_EQUAL);}
   	">"				{ System.out.println(yytext()+" GREATER");return symbol(sym.GREATER);}
   	">="			{ System.out.println(yytext()+" GREATER_OR_EQUAL");return symbol(sym.GREATER_OR_EQUAL);}
   	"&&"			{ System.out.println(yytext()+" BOOL_AND");return symbol(sym.BOOL_AND);}
   	"||"			{ System.out.println(yytext()+" BOOL_OR");return symbol(sym.BOOL_OR);}
   	"=>"			{ System.out.println(yytext()+" IMPLIQUE");return symbol(sym.IMPLIQUE);}
   	"<=>"			{ System.out.println(yytext()+" EQUIV");return symbol(sym.EQUIV);}
   	"("				{ System.out.println(yytext()+" LPAR");return symbol(sym.LPAR);}
   	")"				{ System.out.println(yytext()+" RPAR");return symbol(sym.RPAR);}
   	"+"				{ System.out.println(yytext()+" ADD");return symbol(sym.ADD);}
   	"*"				{ System.out.println(yytext()+" MULTIPLY");return symbol(sym.MULTIPLY);}
   	"-"				{ System.out.println(yytext()+" SUBSTRACT");return symbol(sym.SUBSTRACT);}
   	"/"				{ System.out.println(yytext()+" DIVIDE");return symbol(sym.DIVIDE);}
   	"["				{ System.out.println(yytext()+" LCRO");return symbol(sym.LCRO);}
   	"]"				{ System.out.println(yytext()+" RCRO");return symbol(sym.RCRO);}
   	".."			{ System.out.println(yytext()+" DODOT");return symbol(sym.DODOT);}
   	","				{ System.out.println(yytext()+" COMMA");return symbol(sym.COMMA);}
   	":"				{ System.out.println(yytext()+" TWODOT");return symbol(sym.TWODOT);}		// todo: verify twodot term

   
	/* Numbers */   
	{Integer}			{ System.out.println(yytext()+" INTEGER");return symbol(sym.INTEGER,new String(yytext())); }
   
   	{Double}			{ System.out.println(yytext()+" DOUBLENUMBER");return symbol(sym.DOUBLENUMBER,new String(yytext())); }
   


   	{Comment}			{ System.out.println(yytext()+" COMMENT");return symbol(sym.COMMENT,new String(yytext())); }


	{Identifier}			{ System.out.println(yytext()+" IDENTIFIER");return symbol(sym.IDENTIFIER,new String(yytext())); }


 	<<EOF>>						{ return symbol(sym.EOF); }
	/* Catch any other (unhandled) characters. */   

	{Alpha}				{ System.out.println(yytext()+" ALPHA");return symbol(sym.ALPHA,new String(yytext())); }
   
    .					{ System.out.println(yytext()+" ERROR_IDENTIFIER");return symbol(sym.ERROR_IDENTIFIER,new String(yytext())); }