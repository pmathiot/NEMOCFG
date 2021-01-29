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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decl.h"
/******************************************************************************/
/*                          Add_Data_Var_1                                    */
/******************************************************************************/
/* This subroutine is used to add a record to List_Data_Var                   */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ data +--->+ data +--->+ data +--->+  data+             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void Add_Data_Var_1 (listvar **curlist,char *name,char *values)
{
  listvar *newvar;
  char ligne[LONG_M];

//  if ( firstpass == 1 )
//  {
     newvar=(listvar *)calloc(1,sizeof(listvar));
     newvar->var=(variable *)calloc(1,sizeof(variable));
     /*                                                                       */
     Init_Variable(newvar->var);
     /*                                                                       */
     if ( inmoduledeclare == 1 ) newvar->var->v_module=1;
     strcpy(newvar->var->v_nomvar,name);
     strcpy(newvar->var->v_subroutinename,subroutinename);
     strcpy(newvar->var->v_modulename,curmodulename);
     strcpy(newvar->var->v_commoninfile,cur_filename);
     if (strchr(values,',') && strncasecmp(values,"'",1))
        sprintf(ligne,"(/%s/)",values);
     else
        strcpy(ligne,values);
       
     strcpy(newvar->var->v_initialvalue,ligne);
     Save_Length(ligne,14);
     newvar->suiv = NULL;
     if ( ! (*curlist) )
     {
        *curlist  = newvar ;
     }
     else
     {
        newvar->suiv = *curlist;
        *curlist = newvar;
     }
//  }
}

void Add_Data_Var_Names_01 (listvar **curlist,listname *l1,listname *l2)
{
    listvar *newvar;
    listvar *tmpvar;
    listname *tmpvar1;
    listname *tmpvar2;  
    variable *found_var = NULL;
    
    tmpvar1 = l1;
    tmpvar2 = l2;
  
    while (tmpvar1)
    {
        newvar = (listvar *) calloc(1,sizeof(listvar));
        newvar->var = (variable *) calloc(1,sizeof(variable));

        Init_Variable(newvar->var);

        if ( inmoduledeclare == 1 ) newvar->var->v_module=1;
     
        found_var = get_variable_in_list_from_name(List_Common_Var, tmpvar1->n_name);
        if ( ! found_var )  found_var = get_variable_in_list_from_name(List_Global_Var,tmpvar1->n_name);
        if ( ! found_var )  found_var = get_variable_in_list_from_name(List_SubroutineDeclaration_Var,tmpvar1->n_name);
        
        if ( found_var && found_var->v_nbdim > 0 )
        {
            printf("##############################################################################################################\n");
            printf("## CONV Error : arrays in data_stmt_object lists not yet supported. Please complain to the proper authorities.\n");
            printf("##   variable name : %s (in %s:%s:%s)\n", found_var->v_nomvar, found_var->v_modulename,
                                                              found_var->v_subroutinename, found_var->v_commonname);
            exit(1);
        }
        
        strcpy(newvar->var->v_nomvar,tmpvar1->n_name);
        strcpy(newvar->var->v_subroutinename,subroutinename);
        strcpy(newvar->var->v_modulename,curmodulename);
        strcpy(newvar->var->v_commoninfile,cur_filename);
        strcpy(newvar->var->v_initialvalue,tmpvar2->n_name);

        Save_Length(tmpvar2->n_name,14);

        newvar->suiv = NULL;
     
        if ( *curlist != NULL )
        {
            tmpvar = *curlist;
            while (tmpvar->suiv)
                tmpvar = tmpvar->suiv;
            tmpvar->suiv = newvar;
        }
        else
        {
            *curlist  = newvar ;
        }
     
        tmpvar1 = tmpvar1->suiv;
        tmpvar2 = tmpvar2->suiv;  
    }
}
