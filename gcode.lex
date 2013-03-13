/*** C Definitions ***/
%{
enum token {
    Instruction,
    Label,
    Argument
};

%}
/* Flex Definitions */
%option noyywrap

instr   [A-Z][A-Za-z]+:?
label   [[:graph:]]+:?
arg     [0-9]+

%%

{instr}       printf("Instruction");
{arg}        printf("Arg");
{label}       printf("Label");

[ ]*        printf(" ");
[\n]*       printf("\n");

.           printf("Something we weren't expecting: \"%s\"", yytext);

%%
main() {
    yylex();
}
