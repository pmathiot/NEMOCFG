/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/
%option warn
%option noyywrap

%x parameter
%s character
%x donottreat
%s fortran77style
%s fortran90style
%{
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * yyin;
#define MAX_INCLUDE_DEPTH 30
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_input = 1;
int newlinef90 = 0;
char tmpc;
#define PRINT_LINE_NUM()     // { fprintf(stderr,"== Parsing l.%4d...\n", line_num_input); }
#define INCREMENT_LINE_NUM() { line_num_input++; PRINT_LINE_NUM(); }

/******************************************************************************/
/**************PETITS PB NON PREVUS *******************************************/
/******************************************************************************/
/* NEXTLINF77 un ligne fortran 77 peut commencer par -      &a=b or on        */
/*            a prevu seulement       & a=b avec l'espace entre le symbole    */
/*            de la 7eme et le debut de la ligne de commande                  */
/*            le ! est aussi interdit comme symbole de la 7 eme colonne       */
/*            Normalement NEXTLINEF77 \n+[ ]{5}[^ ]                           */
/******************************************************************************/
#define YY_USER_ACTION  if (firstpass == 0) ECHO;

void out_of_donottreat(void);

%}

REAL8 "real*8"[ \t]*"(a-h,o-z)"

SLASH       "/"
DSLASH      "/"[ \t]*"/"
HEXA        Z\'[0-9a-fA-F]+\'
NAME        [a-zA-Z\_][a-zA-Z0-9\_]*
INTEGER     [0-9]+

EXPONENT    [edq][-+]?{INTEGER}

BEG_DNT         ^[C!]"$AGRIF_DO_NOT_TREAT"[ \t]*\n
END_DNT         ^[C!]"$AGRIF_END_DO_NOT_TREAT"[ \t]*\n

BEG_INTERFACE   ^[ \t]*interface
END_INTERFACE   ^[ \t]*end[ \t]*interface.*\n

ASSIGNTYPE      "assignment"[ \t]*"("[ \t]*[-+=]+[ \t]*")"

COMM_F77        ^([Cc*](([ \t]*\n)|([^AaHhOo\n].*\n)))
COMM_F90        ^[ \t]*!.*\n
COMM_F90_2      !.*
NEXTLINEF90     "&".*\n+
NEXTLINEF77     [\n \t]*\n[ \t]{5}("&"|"+"|"$"|"*"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"."|"#")

LABEL           ^(((" "|[0-9]){1,5})|([ \t]{1,5}))[ &]+

%%
  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

{REAL8}                     { return TOK_REAL8; }
subroutine                  { return TOK_SUBROUTINE; }
program                     { return TOK_PROGRAM; }
allocate                    { inallocate = 1; return TOK_ALLOCATE; }
nullify			            { return TOK_NULLIFY; }
null[ ]*\([ ]*\)            { return TOK_NULL_PTR; }
deallocate                  { inallocate = 1; return TOK_DEALLOCATE; }
result                      { return TOK_RESULT; }
function                    { return TOK_FUNCTION; }
end[ \t]*program            { strcpy(yylval.na,fortran_text); return TOK_ENDPROGRAM;}
end[ \t]*module             { strcpy(yylval.na,fortran_text); return TOK_ENDMODULE; }
end[ \t]*subroutine         { strcpy(yylval.na,fortran_text); return TOK_ENDSUBROUTINE;}
end[ \t]*function           { strcpy(yylval.na,fortran_text); return TOK_ENDFUNCTION;}
end                         { strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
include                     { pos_curinclude = setposcur()-9; return TOK_INCLUDE;}
^[ \t]*use[ ]+              { strcpy(yylval.na,fortran_text);
                              tmpc = (char) input(); unput(tmpc);
                              if ( ( tmpc >= 'a' && tmpc <= 'z' ) ||
                                   ( tmpc >= 'A' && tmpc <= 'Z' )  )  return TOK_USE;
                              else                                    return TOK_NAME;
                            }
rewind                      { return TOK_REWIND; }
implicit                    { return TOK_IMPLICIT; }
none                        { return TOK_NONE; }
call                        { return TOK_CALL; }
.true.                      { return TOK_TRUE; }
.false.                     { return TOK_FALSE; }
\=\>                        { return TOK_POINT_TO; }
{ASSIGNTYPE}                { strcpy(yylval.na,fortran_text); return TOK_ASSIGNTYPE;}
\*\*                        { strcpy(yylval.na,fortran_text); return TOK_DASTER; }
\.[ \t]*eqv\.               { strcpy(yylval.na,fortran_text); return TOK_EQV; }
\.[ \t]*eq\.                { strcpy(yylval.na,fortran_text); return TOK_EQ;  }
\.[ \t]*gt\.                { strcpy(yylval.na,fortran_text); return TOK_GT;  }
\.[ \t]*ge\.                { strcpy(yylval.na,fortran_text); return TOK_GE;  }
\.[ \t]*lt\.                { strcpy(yylval.na,fortran_text); return TOK_LT;  }
\.[ \t]*le\.                { strcpy(yylval.na,fortran_text); return TOK_LE;  }
\.[ \t]*neqv\.              { strcpy(yylval.na,fortran_text); return TOK_NEQV;}
\.[ \t]*ne\.                { strcpy(yylval.na,fortran_text); return TOK_NE;  }
\.[ \t]*not\.               { strcpy(yylval.na,fortran_text); return TOK_NOT; }
\.[ \t]*or\.                { strcpy(yylval.na,fortran_text); return TOK_OR;  }
\.[ \t]*xor\.               { strcpy(yylval.na,fortran_text); return TOK_XOR; }
\.[ \t]*and\.               { strcpy(yylval.na,fortran_text); return TOK_AND; }
module                      { return TOK_MODULE; }
while                       { return TOK_WHILE; }
concurrent                  { return TOK_CONCURRENT; }
end[ \t]*do                 { return TOK_ENDDO; }
do                          { return TOK_PLAINDO;}
real                        { strcpy(yylval.na,fortran_text); return TOK_REAL; }
integer                     { strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
logical                     { strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
character                   { strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
{HEXA}                      { strcpy(yylval.na,fortran_text); return TOK_HEXA;}
double[ \t]*precision       { strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
double[ \t]*complex         { strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
complex                     { return TOK_COMPLEX; }
allocatable                 { return TOK_ALLOCATABLE; }
close                       { return TOK_CLOSE; }
inquire                     { return TOK_INQUIRE; }
dimension                   { return TOK_DIMENSION; }
pause                       { return TOK_PAUSE; }
equivalence                 { return TOK_EQUIVALENCE; }
stop                        { return TOK_STOP; }
where                       { return TOK_WHERE; }
end[ \t]*where              { return TOK_ENDWHERE; }
else[ \t]*where[ \t]*\(     { return TOK_ELSEWHEREPAR; }
else[ \t]*where             { return TOK_ELSEWHERE; }
^[ \t]*contains             { return TOK_CONTAINS; }
only                        { return TOK_ONLY; }
parameter                   { return TOK_PARAMETER; }
recursive                   { return TOK_RECURSIVE; }
common                      { return TOK_COMMON; }
^[ \t]*global[ \t]+         { return TOK_GLOBAL; }
external                    { return TOK_EXTERNAL; }
intent                      { return TOK_INTENT; }
pointer                     { return TOK_POINTER; }
optional                    { return TOK_OPTIONAL; }
save                        { return TOK_SAVE; }
^[ \t]*type[ \t]*\(         { pos_cur_decl = setposcur()-5; return TOK_TYPEPAR; }
^[ \t]*type[ \t\,]+         { return TOK_TYPE; }
end[ \t]*type               { return TOK_ENDTYPE; }
stat                        { if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
open                        { return TOK_OPEN; }
return                      { return TOK_RETURN; }
exit[^(]                    { return TOK_EXIT; }
print                       { return TOK_PRINT; }
module[ \t]*procedure       { return TOK_PROCEDURE; }
read                        { return TOK_READ; }
namelist                    { return TOK_NAMELIST; }
write                       { return TOK_WRITE; }
flush                       { return TOK_FLUSH; }
target                      { return TOK_TARGET; }
public                      { return TOK_PUBLIC; }
private                     { return TOK_PRIVATE; }
in                          { strcpy(yylval.na,fortran_text); return TOK_IN; }
^[ \t]*data[ \t]+           { pos_curdata = setposcur()-strlen(fortran_text); Init_List_Data_Var(); return TOK_DATA; }
continue                    { return TOK_CONTINUE; }
go[ \t]*to                  { return TOK_PLAINGOTO; }
out                         { strcpy(yylval.na,fortran_text); return TOK_OUT; }
inout                       { strcpy(yylval.na,fortran_text); return TOK_INOUT; }
intrinsic                   { return TOK_INTRINSIC; }
then                        { return TOK_THEN; }
else[ \t]*if                { return TOK_ELSEIF; }
else                        { return TOK_ELSE; }
end[ \t]*if                 { return TOK_ENDIF; }
if                          { return TOK_LOGICALIF; }
sum[ \t]*\(                 { return TOK_SUM; }
max[ \t]*\(                 { return TOK_MAX; }
tanh                        { return TOK_TANH; }
maxval                      { return TOK_MAXVAL; }
trim                        { return TOK_TRIM; }
sqrt\(                      { return TOK_SQRT; }
select[ \t]*case            { return TOK_SELECTCASE; }
^[ \t]*case[ \t]*           { return TOK_CASE; }
default                     { return TOK_DEFAULT; }
end[ \t]*select             { return TOK_ENDSELECT; }
file[ \t]*\=                { return TOK_FILE; }
unit[ \t]*\=                { return TOK_UNIT; }
fmt[ \t]*\=                 { return TOK_FMT; }
nml[ \t]*\=                 { return TOK_NML; }
end[ \t]*\=                 { return TOK_END; }
eor[ \t]*\=                 { return TOK_EOR; }
err[ \t]*\=                 { return TOK_ERR; }
exist[ \t]*\=               { return TOK_EXIST; }
min[ \t]*\(                 { return TOK_MIN; }
nint                        { return TOK_NINT; }
float                       { return TOK_FLOAT; }
exp                         { return TOK_EXP; }
cos                         { return TOK_COS; }
cosh                        { return TOK_COSH; }
acos                        { return TOK_ACOS; }
sin                         { return TOK_SIN; }
sinh                        { return TOK_SINH; }
asin                        { return TOK_ASIN; }
log                         { return TOK_LOG; }
tan                         { return TOK_TAN; }
atan                        { return TOK_ATAN; }
cycle                       { return TOK_CYCLE; }
abs[ \t]*\(                 { return TOK_ABS; }
mod                         { return TOK_MOD; }
sign[ \t]*\(                { return TOK_SIGN; }
minloc                      { return TOK_MINLOC; }
maxloc                      { return TOK_MAXLOC; }
minval                      { return TOK_MINVAL; }
backspace                   { return TOK_BACKSPACE; }
::                          { return TOK_FOURDOTS;  }
\({SLASH}                   { return TOK_LEFTAB; }
{SLASH}\)                   { return TOK_RIGHTAB; }
format[ \t]*\((.|{NEXTLINEF90}|{NEXTLINEF77})*\)  {
                              return TOK_FORMAT; }
{SLASH}                     { strcpy(yylval.na,fortran_text); return TOK_SLASH; }
DSLASH                      { strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
(\')[^']*&{0,1}\n[ \t]*&{0,1}[^']*(\') {
                              strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
(\')[^']*(\')             { strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
(\")[^"]*(\")             { strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
{BEG_INTERFACE}             { BEGIN(donottreat); }
<donottreat>{END_INTERFACE} { out_of_donottreat(); return '\n'; }
{NAME}                      { strcpy(yylval.na,fortran_text); return TOK_NAME; }
({INTEGER}\.[0-9]*)/[^"and."|"false."|"true."|"eq."|"or."|"gt."|"ge."|"lt."|"le."|"not."|"ne."] {  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
(({INTEGER}\.[0-9]+|[0-9]*\.{INTEGER}){EXPONENT}?)|{INTEGER}(\.)?{EXPONENT}                     {  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
{INTEGER}                   { strcpy(yylval.na,fortran_text); return TOK_CSTINT; }
\$                          {}
\.                          {}
\(|\)|:|\[|\]|\+|\-|\*      { strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
\%                          { strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
\;                          { return TOK_SEMICOLON; }
\,                          { return (int) *fortran_text; }
\=                          { return (int) *fortran_text; }
\<                          { return (int) *fortran_text; }
\>                          { return (int) *fortran_text; }
\n                          { INCREMENT_LINE_NUM() ; return '\n'; }
^[ ]*$                      {}
[ \t]+                      {}
{LABEL}                     { if (newlinef90 == 0) return TOK_LABEL; else newlinef90 = 0; }
{NEXTLINEF90}               { INCREMENT_LINE_NUM() ; newlinef90=1; }
{NEXTLINEF77}               { INCREMENT_LINE_NUM() ; }

{BEG_DNT}                   { INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
<donottreat>{END_DNT}       { out_of_donottreat(); return '\n'; }
<donottreat>.*\n            { INCREMENT_LINE_NUM() ; }
<fortran77style>{COMM_F77}  { INCREMENT_LINE_NUM() ; }
{COMM_F90}                  { INCREMENT_LINE_NUM() ; }
{COMM_F90_2}                {}
%%

void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}
