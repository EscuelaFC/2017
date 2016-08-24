import java_cup.runtime.*;

%%
%public
%class Lexer
%unicode
%cup
%line
%column
%function next_token
%type java_cup.runtime.Symbol

%eofval{
    return new Symbol(sym.EOF);
%eofval}
%eofclose

%state CADENA

%{
  StringBuffer cadena = new StringBuffer();
  int numErr = 0;
%}




FinDeLinea = \r|\n|\r\n
Espacio = {FinDeLinea}|[ \t\f]
Identificador = [:jletter:][:jletterdigit:]*
Numero = 0 | [1-9][0-9]*
Simbolos =  \* | \+ | < | > | <= | >= | == | \/ | =
Entero = "entero"
Comentarios = ({Espacio})*({Simbolos})*(({Espacio})([A-Za-z1-9])*)*({Espacio})
%%

<YYINITIAL> {
  {Espacio}                 {}  
  {Comentarios}             {}  
  {Numero}                  {return new Symbol(sym.NUMERO, yyline, yycolumn);}
  {Simbolos}                {return new Symbol(sym.SIMBOLO, yyline, yycolumn);}
  "programa"                {return new Symbol(sym.PROGRAMA, yyline, yycolumn);}
  "mientras"                {return new Symbol(sym.MIENTRAS, yyline, yycolumn);}
  "{"			                  {return new Symbol(sym.LLAVE_IZQ, yyline, yycolumn);}
  "}"		                    {return new Symbol(sym.LLAVE_DER, yyline, yycolumn);}
  "escribe"	                {return new Symbol(sym.OP_ESCRITURA, yyline, yycolumn);}
  "para"                    {return new Symbol(sym.PARA, yyline, yycolumn);}
  "desde"                   {return new Symbol(sym.DESDE, yyline, yycolumn);}
  "hasta"                   {return new Symbol(sym.HASTA, yyline, yycolumn);}
  "si"                      {return new Symbol(sym.SI, yyline, yycolumn);}
  "si no"                   {return new Symbol(sym.SI_NO, yyline, yycolumn);}
  "entonces"                {return new Symbol(sym.ENTONCES, yyline, yycolumn);}
  ";"			                  {return new Symbol(sym.MARCADOR_FIN_SENTENCIA, yyline, yycolumn);}
  {Identificador}           {return new Symbol(sym.ID, yyline, yycolumn);}  
  \"				                {cadena.setLength(0);yybegin(CADENA);}  
}

<CADENA> {
\"		{yybegin(YYINITIAL);return new Symbol(sym.LITERAL_CADENA, yyline, yycolumn,cadena.toString());}
[^\n\r\"\\]+ {cadena.append(yytext());}

}

/* si la entrada no cumple ninguna de las reglas se considera un error */
[^]                              {numErr++;return new Symbol(sym.error, yyline, yycolumn,yytext());}