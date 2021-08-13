MODULE trcini_age
   !!======================================================================
   !!                         ***  MODULE trcini_age  ***
   !! TOP :   initialisation of the AGE tracer
   !!======================================================================
   !! History :   2.0  !  2007-12  (G. Nurser, G. Madec, C. Ethe ) Original code
   !!----------------------------------------------------------------------
   !! trc_ini_age   : MY_TRC model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc
   USE trc
   USE trcnam_age
   USE trcsms_age

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_age   ! called by trcini.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcini_age.F90 10070 2018-08-28 14:30:54Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ini_age
      !!----------------------------------------------------------------------
      !!                     ***  trc_ini_age  ***  
      !!
      !! ** Purpose :   initialization for AGE model
      !!
      !!----------------------------------------------------------------------
      INTEGER    ::  jn
      CHARACTER(len = 20)  ::  cltra
      !!----------------------------------------------------------------------
      !
      CALL trc_nam_age
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_ini_age: passive tracer age'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
      IF(lwp) WRITE(numout,*)

      rryear  = 1._wp / ( nyear_len(1) * rday )    ! recip number of seconds in one year

      !! BUG in s-coordinate this does not work!
      nlb_age = MINLOC( gdepw_1d, mask = gdepw_1d > rn_age_depth, dim = 1 ) ! shallowest W level Below age_depth
                                                                            !  = shallowest T level wholly below age_depth
      nl_age  = nlb_age - 1                                                 ! deepest    W level Above age_depth
                                                                            !  = T level surrounding age_depth

      nla_age = nl_age - 1                                                   ! deepest    T level wholly above age_depth

      frac_kill_age = ( rn_age_depth - gdepw_1d(nl_age) ) / e3t_1d(nl_age)      ! fraction of level nl_age above age_depth
      frac_add_age  = 1._wp -  frac_kill_age                                    ! fraction of level nl_age below age_depth

      
      IF( .NOT. ln_rsttr ) trn(:,:,:,jp_age) = 0.
      !
   END SUBROUTINE trc_ini_age

   !!======================================================================
END MODULE trcini_age
