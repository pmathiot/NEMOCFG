#if defined key_agrif
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif2model.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
SUBROUTINE Agrif2Model
      !!---------------------------------------------
      !!   *** ROUTINE Agrif2Model ***
      !!--------------------------------------------- 
END SUBROUTINE Agrif2model

SUBROUTINE Agrif_Set_numberofcells(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Set_numberofcells ***
      !!--------------------------------------------- 
   USE Agrif_Grids
   IMPLICIT NONE

   TYPE(Agrif_Grid), POINTER :: Agrif_Gr

   IF ( ASSOCIATED(Agrif_Curgrid) )THEN
#include "SetNumberofcells.h"
   ENDIF

END SUBROUTINE Agrif_Set_numberofcells

SUBROUTINE Agrif_Get_numberofcells(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Get_numberofcells ***
      !!--------------------------------------------- 
   USE Agrif_Grids
   IMPLICIT NONE

   TYPE(Agrif_Grid), POINTER :: Agrif_Gr

   IF ( ASSOCIATED(Agrif_Curgrid) ) THEN
#include "GetNumberofcells.h"
   ENDIF

END SUBROUTINE Agrif_Get_numberofcells

SUBROUTINE Agrif_Allocationcalls(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Allocationscalls ***
      !!--------------------------------------------- 
   USE Agrif_Grids 
#include "include_use_Alloc_agrif.h"
   IMPLICIT NONE

   TYPE(Agrif_Grid), POINTER :: Agrif_Gr

#include "allocations_calls_agrif.h"

END SUBROUTINE Agrif_Allocationcalls

SUBROUTINE Agrif_probdim_modtype_def()
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_probdim_modtype_def ***
      !!--------------------------------------------- 
   USE Agrif_Types
   IMPLICIT NONE

#include "modtype_agrif.h"
#include "probdim_agrif.h"
#include "keys_agrif.h"

   RETURN

END SUBROUTINE Agrif_probdim_modtype_def

SUBROUTINE Agrif_clustering_def()
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_clustering_def ***
      !!--------------------------------------------- 
   IMPLICIT NONE

   RETURN

END SUBROUTINE Agrif_clustering_def

#else
SUBROUTINE Agrif2Model
      !!---------------------------------------------
      !!   *** ROUTINE Agrif2Model ***
      !!--------------------------------------------- 
   WRITE(*,*) 'Impossible to bet here'
END SUBROUTINE Agrif2model
#endif
