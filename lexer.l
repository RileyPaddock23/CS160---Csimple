%option yylineno
%pointer

%{
    #include <cstdlib>
    #include <cstring>
    #include "ast.hpp"
    #include "parser.hpp"

    /* void yyerror(const char *); */
%}

alpha [a-zA-Z]+
decimal 0|([1-9][0-9]*)
hex 0[X|x][0-9A-F]+
octal 0[0-7]*
binary [0|1]+b
%x comment

%%
"/%"                    { BEGIN(comment); }
<comment>"%/"        { BEGIN(INITIAL); }
<comment>[^%]*
<comment>"%"
<comment>"%"[^\/]
<comment><<EOF>>        { yyterminate();  }
[ \t\n\r] {}

&&  { return AND;}
&   { return REF;}
==  {return EQ;}
=   { return IS;}
\/  { return DIV;}
\<= { return LEQ;}
\>= { return GEQ;}
\>  { return GT;}
\<  { return LT;}
- { return MINUS;}
!=  { return NEQ;}
!   { return NOT;}
\|\|    { return OR;}
\+  { return PLUS;}
\*  { return TIMES;}
\^  { return DEREF;}

;   { return SEMI;}
:   { return COLON;}
,   { return COMMA;}
\|  { return ABS;}
\{  { return BRACKO;}
\}  { return BRACKC;}
\(  { return PARENO;}
\)  { return PARENC;}
\[  { return SBRACKO;}
\]  { return SBRACKC;}

boolean { return BOOL;}
char    { return CHAR;}
integer { return INT;}
string  { return STR;}
intptr  { return INTPTR;}
charptr { return CHARPTR;}
if  { return IF;}
else    { return ELSE;}
while   { return WHILE;}
var { return VAR;}
procedure   {return PROC;}
return  { return RETURN;}
null    {return KNULL;}

true  {yylval.u_base_int = 1; return BOOL_VAL;}
false  {yylval.u_base_int = 0; return BOOL_VAL;}
\'[^\'\n\r]\' {yylval.u_base_int = (int)strdup(yytext)[1]; return CHAR_VAL;}
{decimal} {yylval.u_base_int = (int)strtol(yytext, 0, 10);
                                        return INT_VAL; }
{hex} {yylval.u_base_int = (int)strtol(yytext, 0, 16);
                                        return INT_VAL; }
{octal} {yylval.u_base_int = (int)strtol(yytext, 0, 8);
                                        return INT_VAL; }
{binary} {yylval.u_base_int = (int)strtol(yytext, 0, 2);
                                        return INT_VAL; }
\"[^\"]*\"  {   int l  = strlen(yytext);
                yytext[l-1]='\0';
                yylval.u_base_charptr = strdup(yytext+1); 
                return STRING_VAL;}

{alpha}({alpha}|[0-9]|"_")*     {yylval.u_base_charptr = strdup(yytext);
                                return ID;}



.                   yyerror((char *) yytext);

%%

/** You shall not pass!
 *  You should not  have to do or edit anything past this.
 */

int yywrap(void) {
    return 1;
}
