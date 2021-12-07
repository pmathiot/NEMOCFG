MODULE p2zsms
   !!======================================================================
   !!                         ***  MODULE p2zsms  ***
   !! TOP :   Time loop of LOBSTER model
   !!======================================================================
   !! History :   1.0  !            M. Levy
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   p2zsms        :  Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE oce_trc          !
   USE trc
   USE sms_pisces
   USE p2zbio
   USE p2zopt
   USE p2zsed
   USE p2zexp
   USE trd_oce
   USE trdtrc_oce
   USE trdtrc
   USE trdmxl_trc

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p2z_sms    ! called in p2zsms.F90

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p2zsms.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_sms( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_sms  ***
      !!
      !! ** Purpose :  Managment of the call to Biological sources and sinks 
      !!               routines of LOBSTER bio-model 
      !!
      !! ** Method  : - ???
      !! --------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      !
      INTEGER ::   jn   ! dummy loop index
      !! --------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_sms')
      !
      CALL p2z_opt( kt )      ! optical model
      CALL p2z_bio( kt )      ! biological model
      CALL p2z_sed( kt )      ! sedimentation model
      CALL p2z_exp( kt )      ! export
      !
      IF( l_trdtrc ) THEN
         DO jn = jp_pcs0, jp_pcs1
           CALL trd_trc( tra(:,:,:,jn), jn, jptra_sms, kt )   ! save trends
         END DO
      END IF
      !
      IF ( lwm .AND. kt == nittrc000 ) CALL FLUSH    ( numonp )     ! flush output namelist PISCES
      IF( ln_timing )   CALL timing_stop('p2z_sms')
      !
   END SUBROUTINE p2z_sms

   !!======================================================================
END MODULE p2zsms
