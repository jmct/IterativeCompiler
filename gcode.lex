/*** C Definitions ***/
%{
typedef enum {
    Instruction,
    Label,
    Argument,
    END
} tokenTag;

union {
    int intVal;
    char* strVal;
} yyval;

%}
/* Flex Definitions */
%option noyywrap

instr   [A-Z][A-Za-z]+:?
label   [[:graph:]]+:?
arg     [0-9]+

%%

{instr}     { yyval.strVal = yytext; return Instruction; }
{arg}       { yyval.intVal = atoi(yytext); return Argument; }
{label}     { yyval.strVal = yytext; return Label; }

[ ]*        {/*printf(" ");*/}
[\n]*       {/*printf("\n");*/}

<<EOF>>     { return END; }

.           printf("Something we weren't expecting: \"%s\"", yytext);

