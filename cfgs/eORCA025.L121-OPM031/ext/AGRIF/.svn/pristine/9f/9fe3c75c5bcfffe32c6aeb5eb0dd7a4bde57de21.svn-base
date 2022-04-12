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
/* "http ://www.cecill.info".                                                  */
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

%{
#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

extern int line_num_input;
extern char *fortran_text;

char c_selectorname[LONG_M];
char ligne[LONG_M];
char truename[LONG_VNAME];
char identcopy[LONG_VNAME];
int c_selectorgiven=0;
listvar *curlistvar;
typedim c_selectordim;
listcouple *coupletmp;
int removeline=0;
listvar *test;

int fortran_error(const char *s)
{
    printf("%s line %d, file %s motclef = |%s|\n", s, line_num_input, cur_filename, fortran_text);
    exit(1);
}

%}

%union {
    char        na[LONG_M];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;
}

%left ','
%nonassoc ':'
%right '='
%left TOK_EQV TOK_NEQV
%left TOK_OR TOK_XOR
%left TOK_AND
%left TOK_NOT
%nonassoc TOK_LT TOK_GT TOK_LE TOK_GE TOK_EQ TOK_NE
%left TOK_DSLASH
%left '+' '-'
%left '*' TOK_SLASH
%right TOK_DASTER

%token TOK_SEMICOLON
%token TOK_PARAMETER
%token TOK_RESULT
%token TOK_ONLY
%token TOK_INCLUDE
%token TOK_SUBROUTINE
%token TOK_PROGRAM
%token TOK_FUNCTION
%token TOK_FORMAT
%token TOK_MAX
%token TOK_TANH
%token TOK_WHERE
%token TOK_ELSEWHEREPAR
%token TOK_ELSEWHERE
%token TOK_ENDWHERE
%token TOK_MAXVAL
%token TOK_TRIM
%token TOK_NULL_PTR
%token TOK_SUM
%token TOK_SQRT
%token TOK_CASE
%token TOK_SELECTCASE
%token TOK_FILE
%token TOK_UNIT
%token TOK_FMT
%token TOK_NML
%token TOK_END
%token TOK_EOR
%token TOK_ERR
%token TOK_EXIST
%token TOK_MIN
%token TOK_FLOAT
%token TOK_EXP
%token TOK_COS
%token TOK_COSH
%token TOK_ACOS
%token TOK_NINT
%token TOK_CYCLE
%token TOK_SIN
%token TOK_SINH
%token TOK_ASIN
%token TOK_EQUIVALENCE
%token TOK_BACKSPACE
%token TOK_LOG
%token TOK_TAN
%token TOK_ATAN
%token TOK_RECURSIVE
%token TOK_ABS
%token TOK_MOD
%token TOK_SIGN
%token TOK_MINLOC
%token TOK_MAXLOC
%token TOK_EXIT
%token TOK_MINVAL
%token TOK_PUBLIC
%token TOK_PRIVATE
%token TOK_ALLOCATABLE
%token TOK_RETURN
%token TOK_THEN
%token TOK_ELSEIF
%token TOK_ELSE
%token TOK_ENDIF
%token TOK_PRINT
%token TOK_PLAINGOTO
%token TOK_LOGICALIF
%token TOK_PLAINDO
%token TOK_CONTAINS
%token TOK_ENDDO
%token TOK_MODULE
%token TOK_ENDMODULE
%token TOK_WHILE
%token TOK_CONCURRENT
%token TOK_ALLOCATE
%token TOK_OPEN
%token TOK_CLOSE
%token TOK_INQUIRE
%token TOK_WRITE
%token TOK_FLUSH
%token TOK_READ
%token TOK_REWIND
%token TOK_DEALLOCATE
%token TOK_NULLIFY
%token TOK_DIMENSION
%token TOK_ENDSELECT
%token TOK_EXTERNAL
%token TOK_INTENT
%token TOK_INTRINSIC
%token TOK_NAMELIST
%token TOK_DEFAULT
%token TOK_OPTIONAL
%token TOK_POINTER
%token TOK_CONTINUE
%token TOK_SAVE
%token TOK_TARGET
%token TOK_IMPLICIT
%token TOK_NONE
%token TOK_CALL
%token TOK_STAT
%token TOK_POINT_TO
%token TOK_COMMON
%token TOK_GLOBAL
%token TOK_LEFTAB
%token TOK_RIGHTAB
%token TOK_PAUSE
%token TOK_PROCEDURE
%token TOK_STOP
%token TOK_REAL8
%token TOK_FOURDOTS
%token <na> TOK_HEXA
%token <na> TOK_ASSIGNTYPE
%token <na> TOK_OUT
%token <na> TOK_INOUT
%token <na> TOK_IN
%token <na> TOK_USE
%token <na> TOK_DSLASH
%token <na> TOK_DASTER
%token <na> TOK_EQ
%token <na> TOK_EQV
%token <na> TOK_GT
%token <na> TOK_LT
%token <na> TOK_GE
%token <na> TOK_NE
%token <na> TOK_NEQV
%token <na> TOK_LE
%token <na> TOK_OR
%token <na> TOK_XOR
%token <na> TOK_NOT
%token <na> TOK_AND
%token <na> TOK_TRUE
%token <na> TOK_FALSE
%token <na> TOK_LABEL
%token <na> TOK_TYPE
%token <na> TOK_TYPEPAR
%token <na> TOK_ENDTYPE
%token <na> TOK_REAL
%token <na> TOK_INTEGER
%token <na> TOK_LOGICAL
%token <na> TOK_DOUBLEPRECISION
%token <na> TOK_ENDSUBROUTINE
%token <na> TOK_ENDFUNCTION
%token <na> TOK_ENDPROGRAM
%token <na> TOK_ENDUNIT
%token <na> TOK_CHARACTER
%token <na> TOK_CHAR_CONSTANT
%token <na> TOK_CHAR_CUT
%token <na> TOK_DATA
%token <na> TOK_CHAR_MESSAGE
%token <na> TOK_CSTREAL
%token <na> TOK_COMPLEX
%token <na> TOK_DOUBLECOMPLEX
%token <na> TOK_NAME
%token <na> TOK_SLASH
%token <na> TOK_CSTINT
%token ','
%token ':'
%token '('
%token ')'
%token '<'
%token '>'
%type <l> dcl
%type <l> after_type
%type <l> dimension
%type <l> paramlist
%type <l> args
%type <l> arglist
%type <lc> only_list
%type <lc> only_name
%type <lc> rename_list
%type <lc> rename_name
%type <d> dims
%type <d> dimlist
%type <dim1> dim
%type <v> paramitem
%type <na> comblock
%type <na> name_routine
%type <na> opt_name
%type <na> type
%type <na> word_endsubroutine
%type <na> word_endfunction
%type <na> word_endprogram
%type <na> word_endunit
%type <na> typespec
%type <na> string_constant
%type <na> simple_const
%type <na> ident
%type <na> intent_spec
%type <na> signe
%type <na> opt_signe
%type <na> filename
%type <na> attribute
%type <na> complex_const
%type <na> begin_array
%type <na> clause
%type <na> arg
%type <na> uexpr
%type <na> minmaxlist
%type <na> lhs
%type <na> vec
%type <na> outlist
%type <na> other
%type <na> dospec
%type <na> expr_data
%type <na> structure_component
%type <na> array_ele_substring_func_ref
%type <na> funarglist
%type <na> funarg
%type <na> funargs
%type <na> triplet
%type <na> substring
%type <na> opt_substring
%type <na> opt_expr
%type <na> optexpr
%type <lnn> data_stmt_value_list
%type <lnn> datanamelist
%type <na> after_slash
%type <na> after_equal
%type <na> predefinedfunction
%type <na> expr
%type <na> ubound
%type <na> operation
%type <na> proper_lengspec
%type <lnn> use_name_list
%type <lnn> public

%%
input :
      | input line
      ;
line :  line-break
      | suite_line_list
      | TOK_LABEL suite_line_list
      | error {yyerrok;yyclearin;}
      ;
line-break:
        '\n' fin_line
      | TOK_SEMICOLON
      | line-break '\n' fin_line
      | line-break TOK_SEMICOLON
      | line-break TOK_LABEL
      ;
suite_line_list :
        suite_line
      | suite_line_list TOK_SEMICOLON '\n'
      | suite_line_list TOK_SEMICOLON suite_line
      ;
suite_line :
        entry fin_line     /* subroutine, function, module                    */
      | spec fin_line      /* declaration                                     */
      | TOK_INCLUDE filename fin_line
        {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
      | execution-part-construct
      ;

fin_line : { pos_cur = setposcur(); }
      ;

opt_recursive :         { isrecursive = 0; }
      | TOK_RECURSIVE   { isrecursive = 1; }
      ;

opt_result :                                { is_result_present = 0; }
      | TOK_RESULT arglist_after_result     { is_result_present = 1; }
      ;

entry : opt_recursive TOK_SUBROUTINE name_routine arglist
        {
            insubroutinedeclare = 1;
            if ( firstpass )
                Add_SubroutineArgument_Var_1($4);
            else
                WriteBeginof_SubLoop();
        }
      | TOK_PROGRAM name_routine
        {
            insubroutinedeclare = 1;
            inprogramdeclare = 1;
            /* in the second step we should write the head of       */
            /*    the subroutine sub_loop_<subroutinename>          */
            if ( ! firstpass )
                WriteBeginof_SubLoop();
        }
      | opt_recursive TOK_FUNCTION name_routine arglist opt_result
        {
            insubroutinedeclare = 1;
            strcpy(DeclType, "");
            /* we should to list of the subroutine argument the  */
            /*    name of the function which has to be defined   */
            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1($4);
                if ( ! is_result_present )
                    Add_FunctionType_Var_1($3);
            }
            else
            /* in the second step we should write the head of    */
            /*    the subroutine sub_loop_<subroutinename>       */
                WriteBeginof_SubLoop();
        }
      | TOK_MODULE TOK_NAME
        {
            GlobalDeclaration = 0;
            strcpy(curmodulename,$2);
            strcpy(subroutinename,"");
            Add_NameOfModule_1($2);
            if ( inmoduledeclare == 0 )
            {
                /* To know if there are in the module declaration    */
                inmoduledeclare = 1;
                /* to know if a module has been met                  */
                inmodulemeet = 1;
                /* to know if we are after the keyword contains      */
                aftercontainsdeclare = 0 ;
            }
        }
      ;

/* R312 : label */
label: TOK_CSTINT
     | label TOK_CSTINT
     ;

name_routine :  TOK_NAME    { strcpy($$, $1); strcpy(subroutinename, $1); }
      ;
filename :      TOK_CHAR_CONSTANT { Add_Include_1($1); }
      ;
arglist :               { if ( firstpass ) $$=NULL; }
      | '(' ')'         { if ( firstpass ) $$=NULL; }
      | '(' args ')'    { if ( firstpass ) $$=$2; }
      ;
arglist_after_result:
      | '(' ')'
      | '(' args ')'    { if ( firstpass ) Add_SubroutineArgument_Var_1($2); }
      ;
args :  arg
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($1,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                $$ = settype("",curlistvar);
            }
        }
      | args ',' arg
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($3,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                $$ = insertvar($1,curvar);
            }
        }
      ;
arg : TOK_NAME  { strcpy($$,$1);  }
      | '*'     { strcpy($$,"*"); }
      ;
spec :  type after_type
      | TOK_TYPE opt_spec opt_sep opt_name  { inside_type_declare = 1; }
      | TOK_ENDTYPE opt_name                { inside_type_declare = 0; }
      | TOK_POINTER list_couple
      | before_parameter '(' paramlist ')'
        {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1($3);
                    else                        Add_GlobalParameter_Var_1($3);
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out, pos_cur_decl, pos_end-pos_cur_decl);
                }
            }
            VariableIsParameter =  0 ;
        }
      | before_parameter paramlist
        {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1($2);
                    else                        Add_GlobalParameter_Var_1($2);
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                }
            }
            VariableIsParameter =  0 ;
        }
      | common
      | save
        {
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
        }
      | implicit
      | dimension
        {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    Add_Globliste_1($1);
                    /* if variableparamlists has been declared in a subroutine   */
                    if ( insubroutinedeclare )     Add_Dimension_Var_1($1);
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
                }
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
      | public
        {
            if (firstpass == 0)
            {
                if ($1)
                {
                    removeglobfromlist(&($1));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic($1);
                }
            }
        }
      | private
      | use_stat
      | module_proc_stmt
      | namelist
      | TOK_BACKSPACE '(' expr ')'
      | TOK_EXTERNAL opt_sep use_name_list
      | TOK_INTRINSIC opt_sep use_intrinsic_list
      | TOK_EQUIVALENCE list_expr_equi
      | data_stmt '\n'
        {
            /* we should remove the data declaration                */
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curdata,pos_end-pos_curdata);

            if ( aftercontainsdeclare == 1  && firstpass == 0 )
            {
                ReWriteDataStatement_0(fortran_out);
                pos_end = setposcur();
            }
        }
      ;
opt_spec :
      | access_spec
        {
            PublicDeclare = 0 ;
            PrivateDeclare = 0 ;
        }
      ;
name_intrinsic :
        TOK_SUM
      | TOK_TANH
      | TOK_MAXVAL
      | TOK_MIN
      | TOK_MINVAL
      | TOK_TRIM
      | TOK_SQRT
      | TOK_NINT
      | TOK_FLOAT
      | TOK_EXP
      | TOK_COS
      | TOK_COSH
      | TOK_ACOS
      | TOK_SIN
      | TOK_SINH
      | TOK_ASIN
      | TOK_LOG
      | TOK_TAN
      | TOK_ATAN
      | TOK_MOD
      | TOK_SIGN
      | TOK_MINLOC
      | TOK_MAXLOC
      | TOK_NAME
      ;
use_intrinsic_list :
                               name_intrinsic
      | use_intrinsic_list ',' name_intrinsic
      ;
list_couple :
                        '(' list_expr ')'
      | list_couple ',' '(' list_expr ')'
      ;
list_expr_equi :
                           expr_equi
      | list_expr_equi ',' expr_equi
      ;
expr_equi : '(' list_expr_equi1 ')'
      ;
list_expr_equi1 :
                            ident dims
      | list_expr_equi1 ',' ident dims
      ;
list_expr :
                      expr
      | list_expr ',' expr
      ;
opt_sep :
      | TOK_FOURDOTS
      ;
after_type :
        dcl nodimsgiven
        {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( ! inside_type_declare )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01($1);
                pos_cur_decl = setposcur();
                if ( firstpass == 0 && GlobalDeclaration == 0
                                    && insubroutinedeclare == 0 )
                {
                    fprintf(fortran_out,"\n#include \"Module_Declar_%s.h\"\n", curmodulename);
                    sprintf(ligne, "Module_Declar_%s.h", curmodulename);
                    module_declar = open_for_write(ligne);
                    GlobalDeclaration = 1 ;
                    pos_cur_decl = setposcur();
                }
                $$ = $1;

                if ( firstpass )
                {
                    Add_Globliste_1($1);
                    if ( insubroutinedeclare )
                    {
                        if ( pointerdeclare ) Add_Pointer_Var_From_List_1($1);
                        Add_Parameter_Var_1($1);
                    }
                    else
                        Add_GlobalParameter_Var_1($1);

                    /* If there's a SAVE declaration in module's subroutines we should    */
                    /*    remove it from the subroutines declaration and add it in the    */
                    /*    global declarations                                             */
                    if ( aftercontainsdeclare && SaveDeclare )
                    {
                        if ( inmodulemeet ) Add_SubroutineDeclarationSave_Var_1($1);
                        else                Add_Save_Var_dcl_1($1);
                    }
                }
            }
            else
            {
                $$ = (listvar *) NULL;
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            GlobalDeclarationType = 0;
        }
      | before_function name_routine arglist
        {
            insubroutinedeclare = 1;

            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1($3);
                Add_FunctionType_Var_1($2);
            }
            else
                WriteBeginof_SubLoop();

            strcpy(nameinttypename,"");
        }
      ;
before_function :   TOK_FUNCTION    { functiondeclarationisdone = 1; }
      ;
before_parameter :  TOK_PARAMETER   { VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
      ;

data_stmt :             /* R534 */
        TOK_DATA data_stmt_set_list

data_stmt_set_list :
        data_stmt_set
      | data_stmt_set_list opt_comma data_stmt_set

data_stmt_set :         /* R535 */
        TOK_NAME TOK_SLASH data_stmt_value_list TOK_SLASH
        {
            createstringfromlistname(ligne,$3);
            if (firstpass == 1) Add_Data_Var_1(&List_Data_Var,$1,ligne);
            else                Add_Data_Var_1(&List_Data_Var_Cur,$1,ligne);
        }
      | datanamelist TOK_SLASH data_stmt_value_list TOK_SLASH
        {
            if (firstpass == 1)  Add_Data_Var_Names_01(&List_Data_Var,$1,$3);
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,$1,$3);
        }
      | '(' lhs ',' dospec ')' TOK_SLASH data_stmt_value_list TOK_SLASH
        {
            createstringfromlistname(ligne,$7);
            printf("###################################################################################################################\n");
            printf("## CONV Error : data_implied_do statements (R537) are not yet supported. Please complain to the proper authorities.\n");
            printf("l.%4d -- data_stmt_set : ( lhs , dospec ) /data_stmt_value_list/ -- lhs=|%s| dospec=|%s| data_stmt_value_list=|%s|\n",
                line_num_input,$2,$4,ligne);
            printf("## But, are you SURE you NEED a DATA construct ?\n");
            printf("###################################################################################################################\n");
            exit(1);
        }
      ;

data_stmt_value_list :
        expr_data                           { $$ = Insertname(NULL,$1,0); }
      | expr_data ',' data_stmt_value_list  { $$ = Insertname($3,$1,1);   }
      ;

save :  before_save varsave
      | before_save comblock varsave
      | save opt_comma comblock opt_comma varsave
      | save ',' varsave
      ;
before_save :
        TOK_SAVE        { pos_cursave = setposcur()-4; }
      ;
varsave :
      | TOK_NAME dims   { if ( ! inside_type_declare ) Add_Save_Var_1($1,$2); }
      ;
datanamelist :
        TOK_NAME                        { $$ = Insertname(NULL,$1,0); }
      | TOK_NAME '(' expr ')'           { printf("l.%4d -- INSTRUCTION NON TRAITEE : INITIALISATION DE DATA AVEC EXPRESSION\n",line_num_input); exit(0); }
      | datanamelist ',' datanamelist   { $$ = concat_listname($1,$3); }
      ;
expr_data :
        opt_signe simple_const      { sprintf($$,"%s%s",$1,$2);  }
      | expr_data '+' expr_data     { sprintf($$,"%s+%s",$1,$3); }
      | expr_data '-' expr_data     { sprintf($$,"%s-%s",$1,$3); }
      | expr_data '*' expr_data     { sprintf($$,"%s*%s",$1,$3); }
      | expr_data '/' expr_data     { sprintf($$,"%s/%s",$1,$3); }
      ;
opt_signe :     { strcpy($$,""); }
      | signe   { strcpy($$,$1); }
      ;
namelist :
        TOK_NAMELIST ident
      | TOK_NAMELIST comblock ident
      | namelist opt_comma comblock opt_comma ident
      | namelist ',' ident
      ;
before_dimension :
        TOK_DIMENSION
        {
            positioninblock = 0;
            pos_curdimension = setposcur()-9;
        }

dimension :
        before_dimension opt_comma TOK_NAME dims lengspec
        {
            printf("l.%4d -- dimension : before_dimension opt_comma TOK_NAME = |%s| -- MHCHECK\n",line_num_input,$3);
            if ( inside_type_declare ) break;
            curvar = createvar($3,$4);
            CreateAndFillin_Curvar("", curvar);
            curlistvar=insertvar(NULL, curvar);
            $$ = settype("",curlistvar);
            strcpy(vallengspec,"");
        }
      | dimension ',' TOK_NAME dims lengspec
        {
            printf("l.%4d -- dimension : dimension ',' TOK_NAME dims lengspec = |%s| -- MHCHECK\n",line_num_input,$3);
            if ( inside_type_declare ) break;
            curvar = createvar($3,$4);
            CreateAndFillin_Curvar("", curvar);
            curlistvar = insertvar($1, curvar);
            $$ = curlistvar;
            strcpy(vallengspec,"");
        }
      ;
private :
        TOK_PRIVATE '\n'
      | TOK_PRIVATE opt_sep use_name_list
      ;
public :
        TOK_PUBLIC '\n'                     { $$ = (listname *) NULL; }
      | TOK_PUBLIC opt_sep use_name_list    { $$ = $3; }
      ;
use_name_list :
        TOK_NAME                            { $$ = Insertname(NULL,$1,0); }
      | TOK_ASSIGNTYPE                      { $$ = Insertname(NULL,$1,0); }
      | use_name_list ',' TOK_NAME          { $$ = Insertname($1,$3,0);   }
      | use_name_list ',' TOK_ASSIGNTYPE    { $$ = Insertname($1,$3,0);   }
      ;
common :
        before_common var_common_list
        {
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
      | before_common comblock var_common_list
        {
            if ( inside_type_declare ) break;
            sprintf(charusemodule,"%s",$2);
            Add_NameOfCommon_1($2,subroutinename);
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
      | common opt_comma comblock opt_comma var_common_list
        {
            if ( inside_type_declare ) break;
            sprintf(charusemodule,"%s",$3);
            Add_NameOfCommon_1($3,subroutinename);
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
      ;
before_common :
        TOK_COMMON              { positioninblock = 0; pos_curcommon = setposcur()-6;   }
      | TOK_GLOBAL TOK_COMMON   { positioninblock = 0; pos_curcommon = setposcur()-6-7; }
      ;
var_common_list :
        var_common                      { if ( ! inside_type_declare ) Add_Common_var_1(); }
      | var_common_list ',' var_common  { if ( ! inside_type_declare ) Add_Common_var_1(); }
      ;
var_common :
        TOK_NAME dims
        {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,$1);
            commondim = $2;
        }
      ;
comblock :
        TOK_DSLASH
        {
            strcpy($$,"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
      | TOK_SLASH TOK_NAME TOK_SLASH
        {
            strcpy($$,$2);
            positioninblock=0;
            strcpy(commonblockname,$2);
        }
      ;
opt_comma :
      | ','
      ;
paramlist :
        paramitem                   { $$=insertvar(NULL,$1); }
      | paramlist ',' paramitem     { $$=insertvar($1,$3);   }
      ;
paramitem :
        TOK_NAME '=' expr
        {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,$1);
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            strcpy(curvar->v_initialvalue,$3);
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length($3,14);
            $$ = curvar;
        }
      ;
module_proc_stmt :
        TOK_PROCEDURE proc_name_list
      ;
proc_name_list :
        TOK_NAME
      | proc_name_list ',' TOK_NAME
      ;
implicit :
        TOK_IMPLICIT TOK_NONE
        {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_end-13,13);
            }
        }
      | TOK_IMPLICIT TOK_REAL8
      ;
dcl :   options TOK_NAME dims lengspec initial_value
        {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar($2,curdim);
                else                curvar = createvar($2,$3);
                CreateAndFillin_Curvar(DeclType, curvar);
                curlistvar = insertvar(NULL, curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                $$=settype(DeclType,curlistvar);
            }
            strcpy(vallengspec,"");
        }
      | dcl ',' TOK_NAME dims lengspec initial_value
        {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar($3, curdim);
                else                curvar = createvar($3, $4);
                CreateAndFillin_Curvar($1->var->v_typevar,curvar);
                strcpy(curvar->v_typevar, $1->var->v_typevar);
                curvar->v_catvar = get_cat_var(curvar);
                curlistvar = insertvar($1, curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                $$=curlistvar;
            }
            strcpy(vallengspec,"");
        }
      ;
nodimsgiven : { dimsgiven = 0; }
      ;
type :  typespec selector               { strcpy(DeclType,$1);  }
      | before_character c_selector     { strcpy(DeclType,"character");  }
      | typespec '*' TOK_CSTINT         { strcpy(DeclType,$1); strcpy(nameinttypename,$3);  }
      | TOK_TYPEPAR attribute ')'       { strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
      ;
c_selector :
      | '*' TOK_CSTINT              { c_selectorgiven = 1; strcpy(c_selectorname,$2); }
      | '*' '(' c_attribute ')'     { c_star = 1;}
      | '(' c_attribute ')'
      ;
c_attribute :
        TOK_NAME clause opt_clause
      | TOK_NAME '=' clause opt_clause
      | clause opt_clause
      ;
before_character : TOK_CHARACTER    { pos_cur_decl = setposcur()-9; }
      ;
typespec :
        TOK_INTEGER         { strcpy($$,"integer"); pos_cur_decl = setposcur()-7; }
      | TOK_LOGICAL         { strcpy($$,"logical"); pos_cur_decl = setposcur()-7; }
      | TOK_REAL            { strcpy($$,"real");    pos_cur_decl = setposcur()-4; }
      | TOK_COMPLEX         { strcpy($$,"complex"); pos_cur_decl = setposcur()-7; }
      | TOK_DOUBLECOMPLEX   { strcpy($$,"double complex"); pos_cur_decl = setposcur()-14; }
      | TOK_DOUBLEPRECISION { pos_cur_decl = setposcur()-16; strcpy($$,"real"); strcpy(nameinttypename,"8"); }
      ;
lengspec :
      | '*' proper_lengspec {strcpy(vallengspec,$2);}
      ;
proper_lengspec :
        expr        { sprintf($$,"*%s",$1); }
      | '(' '*' ')' { strcpy($$,"*(*)"); }
      ;
selector :
      | '*' proper_selector
      | '(' attribute ')'
      ;
proper_selector : expr
      | '(' '*' ')'
      ;
attribute :
        TOK_NAME clause
      | TOK_NAME '=' clause
        {
            if ( strstr($3,"0.d0") )
            {
                strcpy(nameinttypename,"8");
                strcpy(NamePrecision,"");
            }
            else
                sprintf(NamePrecision,"%s = %s",$1,$3);
        }
      | TOK_NAME        { strcpy(NamePrecision,$1); }
      | TOK_CSTINT      { strcpy(NamePrecision,$1); }
      | TOK_ASSIGNTYPE  { strcpy(NamePrecision,$1); }
      ;
clause :
        expr   { strcpy(CharacterSize,$1);  strcpy($$,$1);  }
      | '*'    { strcpy(CharacterSize,"*"); strcpy($$,"*"); }
      ;
opt_clause :
      | ',' TOK_NAME clause
      ;
options :
      | TOK_FOURDOTS
      | ',' attr_spec_list TOK_FOURDOTS
      ;
attr_spec_list : attr_spec
      | attr_spec_list ',' attr_spec
      ;
attr_spec :
        TOK_PARAMETER       { VariableIsParameter = 1; }
      | access_spec
      | TOK_ALLOCATABLE     { Allocatabledeclare = 1; }
      | TOK_DIMENSION dims  { dimsgiven = 1; curdim = $2; }
      | TOK_EXTERNAL        { ExternalDeclare = 1; }
      | TOK_INTENT '(' intent_spec ')'
                            { strcpy(IntentSpec,$3); }
      | TOK_INTRINSIC
      | TOK_OPTIONAL        { optionaldeclare = 1 ; }
      | TOK_POINTER         { pointerdeclare = 1 ; }
      | TOK_SAVE            { SaveDeclare = 1 ; }
      | TOK_TARGET          { Targetdeclare = 1; }
      ;
intent_spec :
        TOK_IN          { strcpy($$,$1); }
      | TOK_OUT         { strcpy($$,$1); }
      | TOK_INOUT       { strcpy($$,$1); }
      ;
access_spec :
        TOK_PUBLIC      { PublicDeclare = 1;  }
      | TOK_PRIVATE     { PrivateDeclare = 1; }
      ;
dims :  { $$ = (listdim*) NULL; }
      | '(' dimlist ')'
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=$2;
        }
      ;
dimlist :
        dim
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=insertdim(NULL,$1);
        }
      | dimlist ',' dim
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) $$=insertdim($1,$3);
        }
      ;
dim :   ubound              { strcpy($$.first,"1"); strcpy($$.last,$1); Save_Length($1,1); }
      | ':'                 { strcpy($$.first,"");  strcpy($$.last,"");                    }
      | expr ':'            { strcpy($$.first,$1);  Save_Length($1,2); strcpy($$.last,""); }
      | ':' expr            { strcpy($$.first,"");  strcpy($$.last,$2); Save_Length($2,1); }
      | expr ':' ubound     { strcpy($$.first,$1);  Save_Length($1,2); strcpy($$.last,$3); Save_Length($3,1); }
      ;
ubound :
        '*'                 { strcpy($$,"*"); }
      | expr                { strcpy($$,$1);  }
      ;
expr :  uexpr               { strcpy($$,$1); }
      | complex_const       { strcpy($$,$1); }
      | predefinedfunction  { strcpy($$,$1); }
      | '(' expr ')'        { sprintf($$,"(%s)",$2); }
      ;

predefinedfunction :
        TOK_SUM minmaxlist ')'          { sprintf($$,"SUM(%s)",$2);}
      | TOK_MAX minmaxlist ')'          { sprintf($$,"MAX(%s)",$2);}
      | TOK_TANH '(' minmaxlist ')'     { sprintf($$,"TANH(%s)",$3);}
      | TOK_MAXVAL '(' minmaxlist ')'   { sprintf($$,"MAXVAL(%s)",$3);}
      | TOK_MIN minmaxlist ')'          { sprintf($$,"MIN(%s)",$2);}
      | TOK_MINVAL '(' minmaxlist ')'   { sprintf($$,"MINVAL(%s)",$3);}
      | TOK_TRIM '(' expr ')'           { sprintf($$,"TRIM(%s)",$3);}
      | TOK_SQRT expr ')'               { sprintf($$,"SQRT(%s)",$2);}
      | TOK_REAL '(' minmaxlist ')'     { sprintf($$,"REAL(%s)",$3);}
      | TOK_NINT '(' expr ')'           { sprintf($$,"NINT(%s)",$3);}
      | TOK_FLOAT '(' expr ')'          { sprintf($$,"FLOAT(%s)",$3);}
      | TOK_EXP '(' expr ')'            { sprintf($$,"EXP(%s)",$3);}
      | TOK_COS '(' expr ')'            { sprintf($$,"COS(%s)",$3);}
      | TOK_COSH '(' expr ')'           { sprintf($$,"COSH(%s)",$3);}
      | TOK_ACOS '(' expr ')'           { sprintf($$,"ACOS(%s)",$3);}
      | TOK_SIN '(' expr ')'            { sprintf($$,"SIN(%s)",$3);}
      | TOK_SINH '(' expr ')'           { sprintf($$,"SINH(%s)",$3);}
      | TOK_ASIN '(' expr ')'           { sprintf($$,"ASIN(%s)",$3);}
      | TOK_LOG '(' expr ')'            { sprintf($$,"LOG(%s)",$3);}
      | TOK_TAN '(' expr ')'            { sprintf($$,"TAN(%s)",$3);}
      | TOK_ATAN '(' expr ')'           { sprintf($$,"ATAN(%s)",$3);}
      | TOK_ABS expr ')'                { sprintf($$,"ABS(%s)",$2);}
      | TOK_MOD '(' minmaxlist ')'      { sprintf($$,"MOD(%s)",$3);}
      | TOK_SIGN minmaxlist ')'         { sprintf($$,"SIGN(%s)",$2);}
      | TOK_MINLOC '(' minmaxlist ')'   { sprintf($$,"MINLOC(%s)",$3);}
      | TOK_MAXLOC '(' minmaxlist ')'   { sprintf($$,"MAXLOC(%s)",$3);}
      ;
minmaxlist : expr {strcpy($$,$1);}
      | minmaxlist ',' expr     { sprintf($$,"%s,%s",$1,$3); }
      ;
uexpr : lhs                     { strcpy($$,$1); }
      | simple_const            { strcpy($$,$1); }
      | vec                     { strcpy($$,$1); }
      | expr operation          { sprintf($$,"%s%s",$1,$2); }
      | signe expr %prec '*'    { sprintf($$,"%s%s",$1,$2); }
      | TOK_NOT expr            { sprintf($$,"%s%s",$1,$2); }
      ;
signe : '+'        { strcpy($$,"+"); }
      | '-'        { strcpy($$,"-"); }
      ;

operation :
        '+' expr %prec '+'          { sprintf($$,"+%s",$2); }
      | '-' expr %prec '+'          { sprintf($$,"-%s",$2); }
      | '*' expr                    { sprintf($$,"*%s",$2); }
      | TOK_DASTER expr             { sprintf($$,"%s%s",$1,$2); }
      | TOK_EQ expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_EQV expr %prec TOK_EQV  { sprintf($$,"%s%s",$1,$2); }
      | TOK_GT expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | '>' expr %prec TOK_EQ       { sprintf($$," > %s",$2); }
      | '<' expr %prec TOK_EQ       { sprintf($$," < %s",$2); }
      | '>''=' expr %prec TOK_EQ    { sprintf($$," >= %s",$3); }
      | '<''=' expr %prec TOK_EQ    { sprintf($$," <= %s",$3); }
      | TOK_LT expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_GE expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_LE expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_NE expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_NEQV expr %prec TOK_EQV { sprintf($$,"%s%s",$1,$2); }
      | TOK_XOR expr                { sprintf($$,"%s%s",$1,$2); }
      | TOK_OR expr                 { sprintf($$,"%s%s",$1,$2); }
      | TOK_AND expr                { sprintf($$,"%s%s",$1,$2); }
      | TOK_SLASH after_slash       { sprintf($$,"%s",$2); }
      | '=' after_equal             { sprintf($$,"%s",$2); }

after_slash :                   { strcpy($$,""); }
      | expr                    { sprintf($$,"/%s",$1); }
      | '=' expr %prec TOK_EQ   { sprintf($$,"/= %s",$2);}
      | TOK_SLASH expr          { sprintf($$,"//%s",$2); }
      ;
after_equal :
        '=' expr %prec TOK_EQ   { sprintf($$,"==%s",$2); }
      | expr                    { sprintf($$,"= %s",$1); }
      ;

lhs :   ident                           { strcpy($$,$1); }
      | structure_component             { strcpy($$,$1); }
      | array_ele_substring_func_ref    { strcpy($$,$1); }
      ;

beforefunctionuse :
        {
            agrif_parentcall = 0;
            if ( !strcasecmp(identcopy, "Agrif_Parent") )   agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
      ;
array_ele_substring_func_ref :
        begin_array                                         { strcpy($$,$1); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
      | begin_array substring                               { sprintf($$," %s %s ",$1,$2); }
      | structure_component '(' funarglist ')'              { sprintf($$," %s ( %s )",$1,$3); }
      | structure_component '(' funarglist ')' substring    { sprintf($$," %s ( %s ) %s ",$1,$3,$5); }
      ;
begin_array :
        ident '(' funarglist ')'
        {
            if ( inside_type_declare ) break;
            sprintf($$," %s ( %s )",$1,$3);
            ModifyTheAgrifFunction_0($3);
            agrif_parentcall = 0;
        }
      ;
structure_component :
        lhs '%' declare_after_percent lhs
        {
            sprintf($$," %s %% %s ",$1,$4);
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
      ;
vec :
        TOK_LEFTAB outlist TOK_RIGHTAB   { sprintf($$,"(/%s/)",$2); }
      ;
funarglist :
        beforefunctionuse           { strcpy($$," "); }
      | beforefunctionuse funargs   { strcpy($$,$2); }
      ;
funargs :
        funarg              {  strcpy($$,$1); }
      | funargs ',' funarg  {  sprintf($$,"%s,%s",$1,$3); }
      ;
funarg :
        expr       {strcpy($$,$1);}
      | triplet    {strcpy($$,$1);}
      ;
triplet :
        expr ':' expr           {  sprintf($$,"%s :%s",$1,$3);}
      | expr ':' expr ':' expr  {  sprintf($$,"%s :%s :%s",$1,$3,$5);}
      | ':' expr ':' expr       {  sprintf($$,":%s :%s",$2,$4);}
      | ':' ':' expr            {  sprintf($$,": : %s",$3);}
      | ':' expr                {  sprintf($$,":%s",$2);}
      | expr ':'                {  sprintf($$,"%s :",$1);}
      | ':'                     {  sprintf($$,":");}
      ;
ident : TOK_NAME
        {
            if ( afterpercent == 0 )
            {
                if ( Agrif_in_Tok_NAME($1) ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp($1,"Agrif_Parent") )   agrif_parentcall = 1;
                if ( VariableIsFunction($1) )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp($1,identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,$1);
                    pointedvar = 0;

                    if (variscoupled_0($1)) strcpy(truename, getcoupledname_0($1));
                    else                    strcpy(truename, $1);

                    if ( VarIsNonGridDepend(truename) == 0 && (! Variableshouldberemoved(truename)) )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,$1) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen($1));
                            }
                        }
                        if ( inagrifcallargument != 1 || sameagrifargument ==1 )
                        {
                            Add_UsedInSubroutine_Var_1(truename);
                        }
                    }
                    NotifyAgrifFunction_0(truename);
                }
            }
            else
            {
                afterpercent = 0;
            }
        }
      ;
simple_const :
        TOK_TRUE     { strcpy($$,".TRUE.");}
      | TOK_FALSE    { strcpy($$,".FALSE.");}
      | TOK_NULL_PTR { strcpy($$,"NULL()"); }
      | TOK_CSTINT   { strcpy($$,$1); }
      | TOK_CSTREAL  { strcpy($$,$1); }
      | TOK_HEXA     { strcpy($$,$1); }
      | simple_const TOK_NAME
                     { sprintf($$,"%s%s",$1,$2); }
      | string_constant opt_substring
      ;
string_constant :
        TOK_CHAR_CONSTANT                   { strcpy($$,$1);}
      | string_constant TOK_CHAR_CONSTANT
      | TOK_CHAR_MESSAGE                    { strcpy($$,$1);}
      | TOK_CHAR_CUT                        { strcpy($$,$1);}
      ;
opt_substring :     { strcpy($$," ");}
      | substring   { strcpy($$,$1);}
      ;
substring :
        '(' optexpr ':' optexpr ')' { sprintf($$,"(%s :%s)",$2,$4);}
      ;
optexpr :           { strcpy($$," ");}
      | expr        { strcpy($$,$1);}
      ;
opt_expr :
        '\n'        { strcpy($$," ");}
      | expr        { strcpy($$,$1);}
      ;
initial_value :     { InitialValueGiven = 0; }
      | '=' expr
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 1;
        }
      | TOK_POINT_TO expr
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 2;
        }
      ;
complex_const :
        '(' uexpr ',' uexpr ')' {sprintf($$,"(%s,%s)",$2,$4); }
      ;
use_stat :
        word_use TOK_NAME
        {
            /* if variables has been declared in a subroutine       */
            sprintf(charusemodule, "%s", $2);
            if ( firstpass )
            {
                Add_NameOfModuleUsed_1($2);
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuse_0($2);

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
                }
            }
        }
      | word_use TOK_NAME ',' rename_list
        {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    Add_CouplePointed_Var_1($2,$4);
                    coupletmp = $4;
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne, coupletmp->c_namevar);
                        strcat(ligne, " => ");
                        strcat(ligne, coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                    sprintf(charusemodule,"%s",$2);
                }
                Add_NameOfModuleUsed_1($2);
            }
            if ( inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
            }
        }
      | word_use TOK_NAME ',' TOK_ONLY ':' '\n'
        {
            /* if variables has been declared in a subroutine       */
            sprintf(charusemodule,"%s",$2);
            if ( firstpass )
            {
                Add_NameOfModuleUsed_1($2);
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuseonly_0($2);

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
                }
            }
        }
      | word_use  TOK_NAME ',' TOK_ONLY ':' only_list
        {
            /* if variables has been declared in a subroutine      */
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    Add_CouplePointed_Var_1($2,$6);
                    coupletmp = $6;
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne,coupletmp->c_namevar);
                        if ( strcasecmp(coupletmp->c_namepointedvar,"") )   strcat(ligne," => ");
                        strcat(ligne,coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                    sprintf(charusemodule,"%s",$2);
                }
                Add_NameOfModuleUsed_1($2);
            }
            else /* if ( firstpass == 0 ) */
            {
                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
                    if (oldfortran_out)  variableisglobalinmodule($6,$2,oldfortran_out,pos_curuseold);
                }
                else
                {
                    /* if we are in the module declare and if the    */
                    /* onlylist is a list of global variable         */
                    variableisglobalinmodule($6, $2, fortran_out,pos_curuse);
                }
            }
        }
      ;
word_use :
        TOK_USE
        {
            pos_curuse = setposcur()-strlen($1);
            if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);
        }
      ;
rename_list :
        rename_name
        {
            $$ = $1;
        }
      | rename_list ',' rename_name
        {
            /* insert the variable in the list $1                 */
            $3->suiv = $1;
            $$ = $3;
        }
      ;
rename_name : TOK_NAME TOK_POINT_TO TOK_NAME
        {
            coupletmp = (listcouple *) calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,$3);
            coupletmp->suiv = NULL;
            $$ = coupletmp;
        }
      ;
only_list :
        only_name   {  $$ = $1; }
      | only_list ',' only_name
        {
            /* insert the variable in the list $1                 */
            $3->suiv = $1;
            $$ = $3;
        }
      ;
only_name :
        TOK_NAME TOK_POINT_TO TOK_NAME
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,$3);
            coupletmp->suiv = NULL;
            $$ = coupletmp;
            pointedvar = 1;
            Add_UsedInSubroutine_Var_1($1);
        }
      | TOK_NAME
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            $$ = coupletmp;
        }
      ;

/* R209 : execution-part-construct */
execution-part-construct:
        executable-construct
      | format-stmt
      ;

/* R213 : executable-construct */
executable-construct:
        action-stmt
      | do-construct
      | case-construct
      | if-construct
      | where-construct
      ;

/* R214 : action-stmt */
action-stmt :
        TOK_CONTINUE
      | ident_dims after_ident_dims
      | goto
      | call
      | iofctl ioctl
      | read option_read
      | TOK_WRITE ioctl
      | TOK_WRITE ioctl outlist
      | TOK_REWIND after_rewind
      | TOK_ALLOCATE '(' allocation_list opt_stat_spec ')'          { inallocate = 0; }
      | TOK_DEALLOCATE '(' allocate_object_list opt_stat_spec ')'   { inallocate = 0; }
      | TOK_EXIT optexpr
      | TOK_RETURN opt_expr
      | TOK_CYCLE opt_expr
      | stop opt_expr
      | int_list
      | TOK_NULLIFY '(' pointer_name_list ')'
      | word_endunit
        {
            GlobalDeclaration = 0 ;
            if ( firstpass == 0 && strcasecmp(subroutinename,"") )
            {
                if ( module_declar && insubroutinedeclare == 0 )    fclose(module_declar);
            }
            if ( strcasecmp(subroutinename,"") )
            {
                if ( inmodulemeet == 1 )
                {
                    /* we are in a module                                */
                    if ( insubroutinedeclare == 1 )
                    {
                        /* it is like an end subroutine <name>            */
                        insubroutinedeclare = 0 ;
                        pos_cur = setposcur();
                        closeandcallsubloopandincludeit_0(1);
                        functiondeclarationisdone = 0;
                    }
                    else
                    {
                        /* it is like an end module <name>                */
                        inmoduledeclare = 0 ;
                        inmodulemeet = 0 ;
                    }
                }
                else
                {
                    insubroutinedeclare = 0;
                    pos_cur = setposcur();
                    closeandcallsubloopandincludeit_0(2);
                    functiondeclarationisdone = 0;
                }
            }
            strcpy(subroutinename,"");
        }
      | word_endprogram opt_name
        {
            insubroutinedeclare = 0;
            inprogramdeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(3);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");
        }
      | word_endsubroutine opt_name
        {
            if ( strcasecmp(subroutinename,"") )
            {
                insubroutinedeclare = 0;
                pos_cur = setposcur();
                closeandcallsubloopandincludeit_0(1);
                functiondeclarationisdone = 0;
                strcpy(subroutinename,"");
            }
        }
      | word_endfunction opt_name
        {
            insubroutinedeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(0);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");
        }
      | TOK_ENDMODULE opt_name
        {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, strlen($2)+11);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
      | if-stmt
      | where-stmt
      | TOK_CONTAINS
        {
            if ( inside_type_declare ) break;
            if ( inmoduledeclare )
            {
                if ( firstpass == 0 )
                {
                    RemoveWordCUR_0(fortran_out,9);   // Remove word 'contains'
                    Write_Closing_Module(0);
                }
                inmoduledeclare = 0 ;
                aftercontainsdeclare = 1;
            }
            else if ( insubroutinedeclare )
            {
                incontainssubroutine = 1;
                insubroutinedeclare  = 0;
                incontainssubroutine = 0;
                functiondeclarationisdone = 0;

                if ( firstpass )
                    List_ContainsSubroutine = Addtolistnom(subroutinename, List_ContainsSubroutine, 0);
                else
                    closeandcallsubloop_contains_0();

                strcpy(subroutinename, "");
            }
            else printf("l.%4d -- TOK_CONTAINS -- MHCHECK\n",line_num_input);
        }
      ;

/* R601 : variable */
//variable : expr
//       ;

/* R734 : assignment-stmt */
// assignment-stmt: variable '=' expr
//       ;
assignment-stmt: expr
      ;

/* R741 : where-stmt */
where-stmt: TOK_WHERE '(' mask-expr ')' where-assignment-stmt
      ;

/* R742 : where-construct */
where-construct: where-construct-stmt line-break opt-where-body-construct opt-masked-elsewhere-construct opt-elsewhere-construct end-where-stmt
      ;

opt-where-body-construct:
      | opt-where-body-construct where-body-construct line-break
      ;

opt-masked-elsewhere-construct :
      | opt-masked-elsewhere-construct masked-elsewhere-stmt line-break opt-where-body-construct
      ;

opt-elsewhere-construct:
      | opt-elsewhere-construct elsewhere-stmt line-break opt-where-body-construct
      ;

/* R743 : where-construct-stmt */
where-construct-stmt:
        TOK_WHERE '(' mask-expr ')'
      ;

/* R744 : where-body-construct */
where-body-construct: where-assignment-stmt
      | where-stmt
      | where-construct
      ;

/* R745 : where-assignment-stmt */
where-assignment-stmt: assignment-stmt
      ;

/* R746 : mask-expr */
mask-expr: expr
      ;

/* R747 : masked-elsewhere-stmt */
masked-elsewhere-stmt:
        TOK_ELSEWHEREPAR mask-expr ')'
      | TOK_ELSEWHEREPAR mask-expr ')' TOK_NAME
      ;

/* R748: elsewhere-stmt */
elsewhere-stmt:
        TOK_ELSEWHERE
      | TOK_ELSEWHERE TOK_NAME
      ;

/* R749: end-where-stmt */
end-where-stmt:
        TOK_ENDWHERE
      | TOK_ENDWHERE TOK_NAME
      ;

/* R752 : forall-header */
forall-header :
     ;

/* R801 : block */
block:
      |block execution-part-construct
      |block execution-part-construct line-break
      ;

/* R813 : do-construct */
do-construct:
        block-do-construct
      ;

/* R814 : block-do-construct */
block-do-construct:
        do-stmt line-break do-block end-do
      ;

/* R815 : do-stmt */
do-stmt:
        label-do-stmt
      | nonlabel-do-stmt
      ;

/* R816 : label-do-stmt */
label-do-stmt:
        TOK_NAME ':' TOK_PLAINDO label
      |              TOK_PLAINDO label
      | TOK_NAME ':' TOK_PLAINDO label loop-control
      |              TOK_PLAINDO label loop-control
      ;

/* R817 : nonlabel-do-stmt */
nonlabel-do-stmt:
        TOK_NAME ':' TOK_PLAINDO
      |              TOK_PLAINDO
      | TOK_NAME ':' TOK_PLAINDO loop-control
      |              TOK_PLAINDO loop-control
      ;

/* R818 : loop-control */
loop-control:
        opt_comma do-variable '=' expr ',' expr
      | opt_comma do-variable '=' expr ',' expr ',' expr
      | opt_comma TOK_WHILE '(' expr ')'
      | opt_comma TOK_CONCURRENT forall-header
      ;

/* R819 : do-variable */
do-variable : ident
     ;

/* R820 : do-block */
do-block: block
     ;

/* R821 : end-do */
end-do: end-do-stmt
     | continue-stmt
     ;

/* R822 : end-do-stmt */
end-do-stmt:
        TOK_ENDDO
      | TOK_ENDDO TOK_NAME
      ;

/* R832 : if-construct */
if-construct: if-then-stmt line-break block opt-else-if-stmt-block opt-else-stmt-block end-if-stmt
      ;

opt-else-if-stmt-block:
      | else-if-stmt-block
      | opt-else-if-stmt-block else-if-stmt-block
      ;

else-if-stmt-block:
        else-if-stmt line-break block
      ;

opt-else-stmt-block:
      | else-stmt-block
      | opt-else-stmt-block else-if-stmt-block
      ;

else-stmt-block: else-stmt line-break block
        ;

/* R833 : if-then-stmt */
if-then-stmt:
         TOK_NAME ':' TOK_LOGICALIF '(' expr ')' TOK_THEN
      |               TOK_LOGICALIF '(' expr ')' TOK_THEN
      ;

/* R834 : else-if-stmt */
else-if-stmt:
        TOK_ELSEIF '(' expr ')' TOK_THEN
      | TOK_ELSEIF '(' expr ')' TOK_THEN TOK_NAME
      ;

/* R835 : else-stmt */
else-stmt:
        TOK_ELSE
      | TOK_ELSE TOK_NAME
      ;

/* R836 : end-if-stmt */
end-if-stmt:
        TOK_ENDIF
      | TOK_ENDIF TOK_NAME
      ;

/* R837 : if-stmt */
if-stmt: TOK_LOGICALIF '(' expr ')' action-stmt
        ;

/* R838 : case-construct */
case-construct: select-case-stmt line-break opt_case-stmt-block end-select-stmt
        ;

opt_case-stmt-block:
        | case-stmt-block
        | opt_case-stmt-block case-stmt-block
        ;

case-stmt-block: case-stmt line-break block
        ;

/* R839 : select-case-stmt */
select-case-stmt :
          TOK_NAME ':' TOK_SELECTCASE '(' expr ')'
        |              TOK_SELECTCASE '(' expr ')'
        ;

/* R840 : case-stmt */
case-stmt:
          TOK_CASE case-selector
        | TOK_CASE case-selector TOK_NAME
        ;

/* R840 : end-select-stmt */
end-select-stmt:
          TOK_ENDSELECT
        | TOK_ENDSELECT TOK_NAME
        ;

/* R843 : case-selector */
case-selector:
          '(' case-value-range-list ')'
        | TOK_DEFAULT
        ;

case-value-range-list:
        case-value-range
      | case-value-range-list ',' case-value-range
      ;

/* R844: case-value-range */
case-value-range :
        case-value
      | case-value ':'
      | ':' case-value
      | case-value ':' case-value
      ;

/* R845 : case-value */
case-value: expr
        ;

/* R854 : continue-stmt */
continue-stmt: TOK_CONTINUE
        ;

/* R1001 : format-stmt */
format-stmt: TOK_FORMAT
        ;

word_endsubroutine :
        TOK_ENDSUBROUTINE
        {
            strcpy($$,$1);
            pos_endsubroutine = setposcur()-strlen($1);
            functiondeclarationisdone = 0;
        }
      ;
word_endunit :
        TOK_ENDUNIT
        {
            strcpy($$,$1);
            pos_endsubroutine = setposcur()-strlen($1);
        }
      ;
word_endprogram :
        TOK_ENDPROGRAM
        {
            strcpy($$,$1);
            pos_endsubroutine = setposcur()-strlen($1);
        }
      ;
word_endfunction :
        TOK_ENDFUNCTION
        {
            strcpy($$,$1);
            pos_endsubroutine = setposcur()-strlen($1);
        }
      ;

opt_name : '\n'  {strcpy($$,"");}
      | TOK_NAME {strcpy($$,$1);}
      ;

before_dims : { created_dimensionlist = 0; }
      ;
ident_dims :
        ident before_dims dims dims
        {
            created_dimensionlist = 1;
            if ( ($3 == NULL) || ($4 == NULL) ) break;
            if  ( agrif_parentcall == 1 )
            {
                ModifyTheAgrifFunction_0($3->dim.last);
                agrif_parentcall = 0;
                fprintf(fortran_out," = ");
            }
        }
      | ident_dims '%' declare_after_percent ident before_dims dims dims
        {
            created_dimensionlist = 1;
        }
      ;
int_list :
        TOK_CSTINT
      | int_list ',' TOK_CSTINT
      ;
after_ident_dims :
        '=' expr
      | TOK_POINT_TO expr
      ;
call :  keywordcall opt_call
        {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
      ;
opt_call :
      | '(' opt_callarglist  ')'
      ;
opt_callarglist :
      | callarglist
      ;
keywordcall :
        before_call TOK_FLUSH
      | before_call TOK_NAME
        {
            if (!strcasecmp($2,"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp($2,"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber($2) == 1 )
            {
                incalldeclare = 1;
                inagrifcallargument = 1 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
      ;
before_call : TOK_CALL  { pos_curcall=setposcur()-4; }
      ;
callarglist :
        callarg
      | callarglist ',' callarg
      ;
callarg :
        expr
        {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,$1);
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
      | '*' TOK_CSTINT
      ;

stop :  TOK_PAUSE
      | TOK_STOP
      ;

option_inlist :
      | inlist
      ;
option_read :
        ioctl option_inlist
      | infmt opt_inlist
      ;
opt_inlist :
      | ',' inlist
      ;
ioctl : '(' ctllist ')'
      ;
after_rewind :
        '(' ident ')'
      | '(' TOK_CSTINT ')'
      | TOK_CSTINT
      | '(' uexpr ')'
      | TOK_NAME
      ;
ctllist :
        ioclause
      | ctllist ',' ioclause
      ;
ioclause :
        fexpr
      | '*'
      | TOK_DASTER
      | ident expr dims
      | ident expr '%' declare_after_percent ident_dims
      | ident '(' triplet ')'
      | ident '*'
      | ident TOK_DASTER
      ;

declare_after_percent:      { afterpercent = 1; }
      ;
iofctl :
        TOK_OPEN
      | TOK_CLOSE
      | TOK_FLUSH
      ;
infmt :  unpar_fexpr
      | '*'
      ;

read :  TOK_READ
      | TOK_INQUIRE
      | TOK_PRINT
      ;

fexpr : unpar_fexpr
      | '(' fexpr ')'
      ;
unpar_fexpr :
        lhs
      | simple_const
      | fexpr addop fexpr %prec '+'
      | fexpr '*' fexpr
      | fexpr TOK_SLASH fexpr
      | fexpr TOK_DASTER fexpr
      | addop fexpr %prec '*'
      | fexpr TOK_DSLASH fexpr
      | TOK_FILE expr
      | TOK_UNIT expr
      | TOK_NML expr
      | TOK_FMT expr
      | TOK_EXIST expr
      | TOK_ERR expr
      | TOK_END expr
      | TOK_NAME '=' expr
      | predefinedfunction
      ;
addop : '+'
      | '-'
      ;
inlist : inelt
      | inlist ',' inelt
      ;
// opt_lhs :
//       | lhs
//       ;
inelt : //opt_lhs opt_operation
        lhs opt_operation
      | '(' inlist ')' opt_operation
      | predefinedfunction opt_operation
      | simple_const opt_operation
      | '(' inlist ',' dospec ')'
      ;
opt_operation :
      | operation
      | opt_operation operation
      ;
outlist :
        complex_const       { strcpy($$,$1); }
      | predefinedfunction  { strcpy($$,$1); }
      | uexpr               { strcpy($$,$1); }
      | other               { strcpy($$,$1); }
      | uexpr   ',' expr    { sprintf($$,"%s,%s",$1,$3); }
      | uexpr   ',' other   { sprintf($$,"%s,%s",$1,$3); }
      | other   ',' expr    { sprintf($$,"%s,%s",$1,$3); }
      | other   ',' other   { sprintf($$,"%s,%s",$1,$3); }
      | outlist ',' expr    { sprintf($$,"%s,%s",$1,$3); }
      | outlist ',' other   { sprintf($$,"%s,%s",$1,$3); }
      ;
other :
        '(' uexpr   ',' dospec ')'    { sprintf($$,"(%s,%s)",$2,$4); }
      | '(' outlist ',' dospec ')'    { sprintf($$,"(%s,%s)",$2,$4); }
      | '(' other   ',' dospec ')'    { sprintf($$,"(%s,%s)",$2,$4); }
dospec :
        TOK_NAME '=' expr ',' expr           { sprintf($$,"%s=%s,%s)",$1,$3,$5);}
      | TOK_NAME '=' expr ',' expr ',' expr  { sprintf($$,"%s=%s,%s,%s)",$1,$3,$5,$7);}
      ;
goto :  TOK_PLAINGOTO '(' expr ',' expr ')' ',' expr
      | TOK_PLAINGOTO TOK_CSTINT
      ;
allocation_list :
        allocate_object
      | allocation_list ',' allocate_object
      ;
allocate_object :
        lhs     { Add_Allocate_Var_1($1,curmodulename); }
      ;
allocate_object_list :
        allocate_object
      | allocate_object_list ',' allocate_object
      ;
opt_stat_spec :
      | ',' TOK_STAT '=' lhs
      ;
pointer_name_list :
        ident
      | pointer_name_list ',' ident
      ;

%%

void process_fortran(const char *input_file)
{
    extern FILE *fortran_in;
    extern FILE *fortran_out;

    char output_file[LONG_FNAME];
    char input_fullpath[LONG_FNAME];

    if ( todebug == 1 ) printf("Firstpass == %d \n", firstpass);

     yydebug=0;
/******************************************************************************/
/*  1-  Open input file                                                       */
/******************************************************************************/

    strcpy(cur_filename, input_file);
    sprintf(input_fullpath, "%s/%s", input_dir, input_file);

    fortran_in = fopen(input_fullpath, "r");
    if (! fortran_in)
    {
        printf("Error : File %s does not exist\n", input_fullpath);
        exit(1);
    }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

    line_num_input = 1;
    PublicDeclare = 0;
    PrivateDeclare = 0;
    ExternalDeclare = 0;
    SaveDeclare = 0;
    pointerdeclare = 0;
    optionaldeclare = 0;
    incalldeclare = 0;
    inside_type_declare = 0;
    Allocatabledeclare = 0 ;
    Targetdeclare = 0 ;
    VariableIsParameter =  0 ;
    strcpy(NamePrecision,"");
    c_star = 0 ;
    functiondeclarationisdone = 0;
    insubroutinedeclare = 0 ;
    strcpy(subroutinename," ");
    isrecursive = 0;
    InitialValueGiven = 0 ;
    GlobalDeclarationType = 0;
    inmoduledeclare = 0;
    incontainssubroutine = 0;
    afterpercent = 0;
    aftercontainsdeclare = 1;
    strcpy(nameinttypename,"");

/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/

    sprintf(output_file, "%s/%s", output_dir, input_file);

    if (firstpass == 0) fortran_out = fopen(output_file,"w");

    fortran_parse();

    if (firstpass == 0) NewModule_Creation_0();
    if (firstpass == 0) fclose(fortran_out);
}
