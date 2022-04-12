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
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

int line_num=1;
extern FILE * convert_in;

int convert_error(const char *s)
{
    printf("##\n## ERROR in conv: '%s' (line %d, file: %s)\n##\n", s, line_num, config_file);
    exit(0);
}

%}

%union {
    char na[LONG_M];
}

%token TOK_SEP
%token TOK_KIND
%token TOK_EQUAL
%token TOK_USE
%token TOK_MODULEMAIN      /* name of the module                              */
%token TOK_NOTGRIDDEP      /* Variable which are not grid dependent           */
%token <na> TOK_USEITEM
%token <na> TOK_NAME
%token <na> TOK_CSTINT
%token <na> TOK_PROBTYPE   /* dimension of the problem                        */
%token ','
%token ';'

%%

input :
    | input line ;

line :
      '\n'
    | TOK_PROBTYPE TOK_NAME ';'                             { initdimprob(1,$2,"0","0"); }
    | TOK_PROBTYPE TOK_NAME ',' TOK_NAME ';'                { initdimprob(2,$2, $4,"0"); }
    | TOK_PROBTYPE TOK_NAME ',' TOK_NAME ',' TOK_NAME ';'   { initdimprob(3,$2, $4, $6); }
    | TOK_MODULEMAIN TOK_NAME ';'
        {
            listofmodules = Addtolistnom($2,listofmodules,0);
            Addmoduletothelist($2);
        }
    | TOK_KIND TOK_NAME TOK_EQUAL TOK_CSTINT ';'
        {
            if (!strcasecmp($4,"4"))
            {
                listofkind = Addtolistnom($2,listofkind,4);
            }
            else if (!strcasecmp($4,"8"))
            {
                listofkind = Addtolistnom($2,listofkind,8);
            }
            else
            {
                printf("##\n## Unknown kind type : %s (must be 4 or 8)\n##",$4);
                exit(0);
            }
        }
    | TOK_NOTGRIDDEP TOK_SEP TOK_NAME ';'
        {
            Add_NotGridDepend_Var_1($3);
        }
    | TOK_USE TOK_USEITEM ';'
        {
            if (!strcasecmp($2,"FIXED_GRIDS"))      fixedgrids = 1;
            if (!strcasecmp($2,"ONLY_FIXED_GRIDS")) onlyfixedgrids = 1;
        }
    ;
%%

void print_usage()
{
    printf("usage : conv <config_file> -convfile  <FILENAME>\n");
    printf(" [-workdir <directory>] [-incdir <directory>]\n");
    printf(" [-comdirin   <directory>] [-comdirout <directory>]\n");
    printf(" [-convfile  <FILENAME>] [-SubloopScalar] [-SubloopScalar1] \n");
    printf(" [-free|-fixed]\n");
    exit(0);
}

int main(int argc,char *argv[])
{
    extern FILE * convert_in ;
    FILE *dependglobaloutput;
    int i;
    listnom *parcours;
    listvar *newvar;
    int stylegiven = 0;
    int infreegiven ;
    int infixedgiven ;
    int lengthmainfile;

    char filetoparse[LONG_FNAME];

/******************************************************************************/
/*  1-  Variables initialization                                              */
/******************************************************************************/
    List_Global_Var = (listvar *) NULL;
    List_GlobalParameter_Var = (listvar *) NULL;
    List_Common_Var = (listvar *) NULL;
    List_Allocate_Var = (listallocate *) NULL;
    List_SubroutineWhereAgrifUsed = (listnom *) NULL;
    List_Subroutine_For_Alloc = (listnom *) NULL;
    List_Include = (listusemodule *) NULL;
    List_NameOfModuleUsed = (listusemodule *) NULL;
    listofmoduletmp = (listusemodule *) NULL;
    List_SubroutineDeclaration_Var = (listvar *) NULL;
    List_UsedInSubroutine_Var = (listvar *) NULL;
    List_NotGridDepend_Var = (listvar *) NULL;
    Listofavailableindices = (listindice *) NULL;
    Listofavailableindices_glob = (listindice **) calloc(NB_CAT_VARIABLES,sizeof(listindice *));
    List_CouplePointed_Var = (listvarpointtovar *) NULL;
    List_ModuleUsed_Var = (listvar *) NULL;
    List_ModuleUsedInModuleUsed_Var = (listvar *) NULL;
    List_GlobParamModuleUsed_Var = (listparameter *) NULL;
    List_GlobParamModuleUsedInModuleUsed_Var = (listparameter *) NULL;
    List_SubroutineArgument_Var = (listvar *) NULL;
    List_FunctionType_Var = (listvar *) NULL;
    tmpuselocallist = (listusemodule *) NULL;
    List_ContainsSubroutine = (listnom *) NULL;
    oldfortran_out = (FILE *) NULL;

    if (argc < 2) print_usage();
    
    strcpy(config_file, argv[1]);
    strcpy(work_dir, ".");
    strcpy(input_dir, ".");
    strcpy(output_dir, "AGRIF_MODELFILES");
    strcpy(include_dir, "AGRIF_INC");
    strcpy(filetoparse, "");
    strcpy(subofagrifinitgrids, "");
    strcpy(meetagrifinitgrids, "");
    strcpy(mpiinitvar, "");

    length_last = 0 ;
    length_first = 0 ;
    length_v_vallengspec = 0 ;
    length_v_commoninfile = 0 ;
    length_v_precision = 0 ;
    length_v_IntentSpec = 0 ;
    length_v_initialvalue = 0 ;
    length_v_readedlistdimension = 0 ;
    length_a_nomvar = 0 ;
    length_toprintglob = 0 ;
    length_tmpvargridname = 0 ;
    length_ligne_Subloop = 0 ;
    length_toprint_utilagrif = 0 ;
    length_toprinttmp_utilchar = 0 ;
    length_ligne_writedecl = 0 ;
    length_newname_toamr = 0 ;
    length_newname_writedecl = 0 ;
    length_ligne_toamr = 0 ;
    length_tmpligne_writedecl = 0 ;
    value_char_size = 0 ;
    value_char_size1 = 0 ;
    value_char_size2 = 0 ;
    value_char_size3 = 0 ;
    inallocate = 0;
    infixed = 1;
    infree  = 0;

    onlyfixedgrids=0;
    fixedgrids=0;
    InAgrifParentDef = 0;
    IndicenbmaillesX=0;
    IndicenbmaillesY=0;
    IndicenbmaillesZ=0;
    created_dimensionlist = 1;
    /* current indice in the table tabvars             */
    for ( i=0 ; i<NB_CAT_VARIABLES ; i++)
    {
        indicemaxtabvars[i] = 0;
    }
    SubloopScalar = 0;
    todebug = 0;
    retour77 = 1 ;
    shouldincludempif = 0 ;

    Read_val_max();

/******************************************************************************/
/*  2-  Program arguments                                                     */
/******************************************************************************/

    if ( (convert_in=fopen(config_file,"r")) == NULL )
    {
        printf("##\n## ERROR: the configuration file '%s' doesn't exist.\n##\n", config_file);
        print_usage();
    }

    i=2;
    while ( i < argc )
    {
        if (!strcasecmp(argv[i], "-workdir"))
        {
            strcpy(work_dir,argv[i+1]);
            i++;
        }
        else if (!strcasecmp(argv[i], "-incdir"))
        {
            strcpy(include_dir,argv[i+1]);
            i++;
        }
        else if (!strcasecmp(argv[i], "-comdirin")) /* input directory           */
        {
            strcpy(input_dir,argv[i+1]);
            i++;
        }
        else if (!strcasecmp(argv[i], "-comdirout")) /* output directory         */
        {
            strcpy(output_dir,argv[i+1]);
            i++;
        }
        else if (!strcasecmp(argv[i], "-convfile")) /* file to parse             */
        {
            strcpy(filetoparse, argv[i+1]);
            i++;
            lengthmainfile = strlen(filetoparse);
            if (!strcasecmp(&filetoparse[lengthmainfile-4], ".f90"))
            {
                infixed = 0;
                infree = 1;
            }
            else
            {
                infixed = 1;
                infree = 0;
            }
        }
        else if (!strcasecmp(argv[i], "-free"))
        {
            stylegiven = 1;
            infreegiven  = 1 ;
            infixedgiven = 0;
        }
        else if (!strcasecmp(argv[i], "-fixed"))
        {
            stylegiven = 1;
            infreegiven  = 0;
            infixedgiven = 1;
        }
        else if (!strcasecmp(argv[i], "-SubloopScalar"))
        {
            SubloopScalar = 1 ;
        }
        else if (!strcasecmp(argv[i], "-SubloopScalar1"))
        {
            SubloopScalar = 2 ;
        }
        else if (!strcasecmp(argv[i], "-todebug"))
        {
            todebug = 1 ;
        }
        else if (!strcasecmp(argv[i],"-rm")) { }
        else
        {
            printf("##\n## Unkwon option : %s\n##\n", argv[i]);
            exit(0);
        }
        i++;
    }
    // Check input file
    if ( strlen(filetoparse) == 0 )         // -convfile has not been specified
    {
        printf("##\n## ERROR: please provide a file to parse with -convfile.\n##\n");
        print_usage();
    }
    // Setup input & output directories
    if ( strcasecmp(work_dir, ".") != 0 )   // -workdir has been changed...
    {
        if ( strcasecmp(input_dir,  ".") == 0 )                 // ...and -comdirin  has NOT been changed
        {
            strcpy(input_dir, work_dir);
        }
        if ( strcasecmp(output_dir, "AGRIF_MODELFILES") == 0 )  // ...and -comdirout has NOT been changed
        {
            sprintf(output_dir, "%s/%s", work_dir, "AGRIF_MODELFILES");
        }
        if ( strcasecmp(include_dir, "AGRIF_INC") == 0 )        // ...and -incdir    has NOT been changed
        {
            sprintf(include_dir, "%s/%s", work_dir, "AGRIF_INC");
        }
    }
    if (stylegiven == 1)
    {
        infree  = infreegiven;
        infixed = infixedgiven;
    }

/******************************************************************************/
/*  3-  Parsing of the conv file <name>.in                                    */
/******************************************************************************/

    if ( strstr(filetoparse, ".f90") || strstr(filetoparse, ".F90") ) retour77 = 0;

    convert_parse();

/******************************************************************************/
/*  4-  Preparation of the file parsing                                       */
/******************************************************************************/

    sprintf(dependfilename, "%s/.dependglobal_agrif", work_dir);
    /*                                                                         */
    if ( (dependglobaloutput=fopen(dependfilename, "r")) != NULL )
    {
        for (i=0;i<NB_CAT_VARIABLES;i++)
        {
            fscanf(dependglobaloutput,"%d\n",&indicemaxtabvars[i]);
        }
        fclose(dependglobaloutput);
    }
    Readthedependavailablefile();
    /* Read the .dependnbxnby file which contains indices of nbmaillsX, nbmailleY and nbmailleZ */
    Readthedependnbxnbyfile();
    Read_Subroutine_For_Alloc();

/******************************************************************************/
/*  5-  Parsing of the input file (2 times)                                   */
/******************************************************************************/

    /* Record all variables in list                                            */
    firstpass = 1;
    process_fortran(filetoparse);

    CompleteThelistvarindoloop();
    /* Read list of module used                                                */
    RecordUseModulesVariables();
    /* Read list of module used in module used                                 */
    RecordUseModulesUseModulesVariables();
    /* Save variables are considered as globals ones                           */
    Update_List_Global_Var_From_List_Save_Var();
    /* Update all lists                                                        */
    ListUpdate();

    Clean_List_Global_Var();
    /* Indice tabvars identification                                           */
    IndiceTabvarsIdentification();
    /* Update all lists                                                        */
    ListUpdate();
    /* The allocation subroutine is necessary ????                             */
    New_Allocate_Subroutine_Is_Necessary();
    /* The allocation subroutine is necessary for common list                  */
    New_Allocate_Subroutine_For_Common_Is_Necessary();
    /* Sort List_SubroutineArgument_Var                                        */
    Sort_List_SubroutineArgument_Var();
    /* Clean all lists                                                         */
    ListClean();
    /* Update Indice of List_UsedInSubroutine_Var from module used             */
    List_UsedInSubroutine_Var_Update_From_Module_Used();
    /* Update List_SubroutineWhereAgrifUsed                                    */
    UpdateList_SubroutineWhereAgrifUsed();
    /* Update List_UsedInSubroutine_Var with v_readedlistdimension             */
    UpdateList_UsedInSubroutine_With_dimension();

    ModifyThelistvarindoloop();
    UpdateListDeclarationWithDimensionList();
    GiveTypeOfVariables();

    /* Build new subroutines                                                   */
    firstpass = 0;
    process_fortran(filetoparse);

    newvar = (listvar *) NULL;

    while ( newvar )
    {
        printf("++++ %s %d %s %s %s\n",
            newvar->var->v_nomvar,
            newvar->var->v_nbdim,
            newvar->var->v_subroutinename,
            newvar->var->v_modulename,
            newvar->var->v_typevar);
        newvar = newvar->suiv;
    }

/******************************************************************************/
/*  6-  Write informations in output files                                    */
/******************************************************************************/

    /* Write the .dependglobal_agrif file which contain the max indice         */
    /*    of the tabvars table                                                 */
    sprintf(dependfilename, "%s/.dependglobal_agrif", work_dir);
    dependglobaloutput = fopen(dependfilename, "w");
    for (i=0;i<NB_CAT_VARIABLES;i++)
    {
        fprintf(dependglobaloutput,"%d\n",indicemaxtabvars[i]);
    }
    fclose(dependglobaloutput);
    /* Write the list of available indice                                      */
    Writethedependavailablefile();
    /* Write the .dependnbxnby file which contains indices of nbmaillsX,       */
    /*    nbmailleY and nbmailleZ                                              */
    Writethedependnbxnbyfile();
    /* Write the .depend<namefile> file which contain general informations     */
    /*    about variable of this file                                          */
    parcours = List_NameOfModule;
    while( parcours )
    {
        Writethedependlistofmoduleused(parcours->o_nom);
        WritedependParameterList(parcours->o_nom);
        Writethedependfile(parcours->o_nom,List_Global_Var);
        parcours=parcours->suiv;
    }
    parcours = List_NameOfCommon;
    while( parcours )
    {
        Writethedependfile(parcours->o_nom,List_Common_Var);
        parcours=parcours->suiv;
    }
    Write_Subroutine_For_Alloc();

/******************************************************************************/
/*  7-  Create files in AGRIF_INC directory                                   */
/******************************************************************************/

    creefichieramr();

    Write_val_max();

    if ( todebug == 1 ) printf("Out of CONV \n");
    return 0;
}
