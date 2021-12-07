MODULE isfrst
   !!======================================================================
   !!                       ***  MODULE  isfrst  ***
   !! iceshelf restart module :read/write iceshelf variables from/in restart
   !!======================================================================
   !! History :  4.1  !  2019-07  (P. Mathiot) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfrst : read/write iceshelf variables in/from restart
   !!----------------------------------------------------------------------
   !
   USE par_oce, ONLY: jpi,jpj,jpk,jpts ! time and space domain
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   !
   IMPLICIT NONE

   PRIVATE

   PUBLIC isfrst_read, isfrst_write ! iceshelf restart read and write 

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   ! 
   SUBROUTINE isfrst_read(cdisf, ptsc, pfwf, ptsc_b, pfwf_b )
      !!---------------------------------------------------------------------
      !!
      !!   isfrst_read : read iceshelf variables from restart
      !!
      !!-------------------------- OUT --------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(  out) :: pfwf_b
      REAL(wp), DIMENSION(jpi,jpj,jpts), INTENT(  out) :: ptsc_b
      !!-------------------------- IN  --------------------------------------
      CHARACTER(LEN=3)                 , INTENT(in   ) :: cdisf
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(in   ) :: pfwf
      REAL(wp), DIMENSION(jpi,jpj,jpts), INTENT(in   ) :: ptsc
      !!----------------------------------------------------------------------
      CHARACTER(LEN=256) :: cfwf_b, chc_b, csc_b
      !!----------------------------------------------------------------------
      !
      ! define variable name
      cfwf_b = 'fwfisf_'//TRIM(cdisf)//'_b'
      chc_b  = 'isf_hc_'//TRIM(cdisf)//'_b'
      csc_b  = 'isf_sc_'//TRIM(cdisf)//'_b'
      !
      ! read restart
      IF( iom_varid( numror, cfwf_b, ldstop = .FALSE. ) > 0 ) THEN
         IF(lwp) WRITE(numout,*) '          nit000-1 isf tracer content forcing fields read in the restart file'
         CALL iom_get( numror, jpdom_autoglo, cfwf_b, pfwf_b(:,:)        , ldxios = lrxios )   ! before ice shelf melt
         CALL iom_get( numror, jpdom_autoglo, chc_b , ptsc_b (:,:,jp_tem), ldxios = lrxios )   ! before ice shelf heat flux
         CALL iom_get( numror, jpdom_autoglo, csc_b , ptsc_b (:,:,jp_sal), ldxios = lrxios )   ! before ice shelf heat flux
      ELSE
         pfwf_b(:,:)   = pfwf(:,:)
         ptsc_b(:,:,:) = ptsc(:,:,:)
      ENDIF
      !
      IF( lwxios ) THEN
         CALL iom_set_rstw_var_active(TRIM(chc_b ))
         CALL iom_set_rstw_var_active(TRIM(csc_b ))
         CALL iom_set_rstw_var_active(TRIM(cfwf_b))
      ENDIF

   END SUBROUTINE isfrst_read
   ! 
   SUBROUTINE isfrst_write(kt, cdisf, ptsc, pfwf )
      !!---------------------------------------------------------------------
      !!
      !!   isfrst_write : write iceshelf variables in restart
      !!
      !!-------------------------- IN  --------------------------------------
      INTEGER                          , INTENT(in   ) :: kt
      CHARACTER(LEN=3)                 , INTENT(in   ) :: cdisf
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(in   ) :: pfwf
      REAL(wp), DIMENSION(jpi,jpj,jpts), INTENT(in   ) :: ptsc
      !!---------------------------------------------------------------------
      CHARACTER(LEN=256) :: cfwf_b, chc_b, csc_b
      !!---------------------------------------------------------------------
      !
      ! ocean output print
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'isf : isf fwf and heat fluxes written in ocean restart file ',   &
         &                    'at it= ', kt,' date= ', ndastp
      IF(lwp) WRITE(numout,*) '~~~~'
      !
      ! define variable name
      cfwf_b = 'fwfisf_'//TRIM(cdisf)//'_b'
      chc_b  = 'isf_hc_'//TRIM(cdisf)//'_b'
      csc_b  = 'isf_sc_'//TRIM(cdisf)//'_b'
      !
      ! write restart variable
      IF( lwxios ) CALL iom_swap( cwxios_context )
      CALL iom_rstput( kt, nitrst, numrow, cfwf_b, pfwf(:,:)       , ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, chc_b , ptsc(:,:,jp_tem), ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, csc_b , ptsc(:,:,jp_sal), ldxios = lwxios )
      IF( lwxios ) CALL iom_swap( cxios_context )
      !
   END SUBROUTINE isfrst_write
   !
END MODULE isfrst
