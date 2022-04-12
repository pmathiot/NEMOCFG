MODULE trcnam_my_trc
   !!======================================================================
   !!                      ***  MODULE trcnam_my_trc  ***
   !! TOP :   initialisation of some run parameters for MY_TRC bio-model
   !!======================================================================
   !! History :      !  2007  (C. Ethe, G. Madec) Original code
   !!                !  2016  (C. Ethe, T. Lovato) Revised architecture
   !!----------------------------------------------------------------------
   !! trc_nam_my_trc      : MY_TRC model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_my_trc   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcnam_my_trc.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_my_trc
      !!----------------------------------------------------------------------
      !!                     ***  trc_nam_my_trc  ***  
      !!
      !! ** Purpose :   read MY_TRC namelist
      !!
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_nam_my_trc : read MY_TRC namelists'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~~'
      !
   END SUBROUTINE trc_nam_my_trc
   
   !!======================================================================
END MODULE trcnam_my_trc
