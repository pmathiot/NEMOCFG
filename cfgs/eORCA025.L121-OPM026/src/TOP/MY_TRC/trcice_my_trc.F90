MODULE trcice_my_trc
   !!======================================================================
   !!                         ***  MODULE trcice_my_trc  ***
   !!----------------------------------------------------------------------
   !! trc_ice_my_trc       : MY_TRC model seaice coupling routine
   !!----------------------------------------------------------------------
   !! History :        !  2016  (C. Ethe, T. Lovato) Revised architecture
   !!----------------------------------------------------------------------
   USE par_trc         ! TOP parameters
   USE oce_trc         ! Ocean variables
   USE trc             ! TOP variables

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ice_ini_my_trc       ! called by trcice.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcice_my_trc.F90 10069 2018-08-28 14:12:24Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ice_ini_my_trc
      !!----------------------------------------------------------------------
      !!                     ***  trc_ice_my_trc  ***
      !!
      !!----------------------------------------------------------------------
      !
      !
   END SUBROUTINE trc_ice_ini_my_trc

   !!======================================================================
END MODULE trcice_my_trc
