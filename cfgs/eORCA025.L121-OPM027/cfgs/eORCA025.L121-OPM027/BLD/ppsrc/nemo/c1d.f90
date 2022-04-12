










MODULE c1d
   !!======================================================================
   !!                     ***  MODULE  c1d  ***
   !! Ocean domain  :  1D configuration
   !!=====================================================================
   !! History :  2.0  !  2004-09 (C. Ethe)     Original code
   !!            3.0  !  2008-04 (G. Madec)    adaptation to SBC
   !!            3.5  !  2013-10 (D. Calvert)  add namelist
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Dummy module :                           No use of 1D configuration
   !!----------------------------------------------------------------------
   USE par_kind         ! kind parameters
   LOGICAL, PUBLIC, PARAMETER ::   lk_c1d = .FALSE.   !: 1D config. flag de-activated
   REAL(wp)                   ::   rn_lat1d, rn_lon1d
   LOGICAL , PUBLIC           ::   ln_c1d_locpt = .FALSE. 
CONTAINS
   SUBROUTINE c1d_init               ! Dummy routine
   END SUBROUTINE c1d_init

   !!======================================================================
END MODULE c1d
