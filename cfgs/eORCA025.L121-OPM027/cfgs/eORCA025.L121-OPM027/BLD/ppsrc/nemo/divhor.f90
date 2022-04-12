










MODULE divhor
   !!==============================================================================
   !!                       ***  MODULE  divhor  ***
   !! Ocean diagnostic variable : now horizontal divergence
   !!==============================================================================
   !! History :  1.0  ! 2002-09  (G. Madec, E. Durand)  Free form, F90
   !!             -   ! 2005-01  (J. Chanut) Unstructured open boundaries
   !!             -   ! 2003-08  (G. Madec)  merged of cur and div, free form, F90
   !!             -   ! 2005-01  (J. Chanut, A. Sellar) unstructured open boundaries
   !!            3.3  ! 2010-09  (D.Storkey and E.O'Dea) bug fixes for BDY module
   !!             -   ! 2010-10  (R. Furner, G. Madec) runoff and cla added directly here
   !!            3.7  ! 2014-01  (G. Madec) suppression of velocity curl from in-core memory
   !!             -   ! 2014-12  (G. Madec) suppression of cross land advection option
   !!             -   ! 2015-10  (G. Madec) add velocity and rnf flag in argument of div_hor
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   div_hor    : Compute the horizontal divergence field
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce, ONLY : ln_rnf      ! river runoff
   USE sbcrnf , ONLY : sbc_rnf_div ! river runoff 
   USE isf_oce, ONLY : ln_isf      ! ice shelf
   USE isfhdiv, ONLY : isf_hdiv    ! ice shelf
   !
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   div_hor    ! routine called by step.F90 and istate.F90

   !! * Substitutions
   !!----------------------------------------------------------------------
   !!                   ***  vectopt_loop_substitute  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute the inner loop start/end indices with CPP macro
   !!                allow unrolling of do-loop (useful with vector processors)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: vectopt_loop_substitute.h90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: divhor.F90 12737 2020-04-10 17:55:11Z jchanut $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE div_hor( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE div_hor  ***
      !!                    
      !! ** Purpose :   compute the horizontal divergence at now time-step
      !!
      !! ** Method  :   the now divergence is computed as :
      !!         hdivn = 1/(e1e2t*e3t) ( di[e2u*e3u un] + dj[e1v*e3v vn] )
      !!      and correct with runoff inflow (div_rnf) and cross land flow (div_cla) 
      !!
      !! ** Action  : - update hdivn, the now horizontal divergence
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk    ! dummy loop indices
      REAL(wp) ::   zraur, zdep   ! local scalars
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('div_hor')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'div_hor : horizontal velocity divergence '
         IF(lwp) WRITE(numout,*) '~~~~~~~   '
         hdivn(:,:,:) = 0._wp    ! initialize hdivn for the halos at the first time step
      ENDIF
      !
      DO jk = 1, jpkm1                                      !==  Horizontal divergence  ==!
         DO jj = 2, jpjm1
            DO ji = 2, jpim1   ! vector opt.
               hdivn(ji,jj,jk) = (  e2u(ji  ,jj) * e3u_n(ji  ,jj,jk) * un(ji  ,jj,jk)      &
                  &               - e2u(ji-1,jj) * e3u_n(ji-1,jj,jk) * un(ji-1,jj,jk)      &
                  &               + e1v(ji,jj  ) * e3v_n(ji,jj  ,jk) * vn(ji,jj  ,jk)      &
                  &               - e1v(ji,jj-1) * e3v_n(ji,jj-1,jk) * vn(ji,jj-1,jk)  )   &
                  &            * r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk)
            END DO  
         END DO  
      END DO
      !
      !
      IF( ln_rnf )                      CALL sbc_rnf_div( hdivn )       !==  runoffs           ==!   (update hdivn field)
      !
      !
      IF( ln_isf )                      CALL isf_hdiv( kt, hdivn )      !==  ice shelf         ==!   (update hdivn field)
      !
      CALL lbc_lnk( 'divhor', hdivn, 'T', 1. )   !   (no sign change)
      !
      IF( ln_timing )   CALL timing_stop('div_hor')
      !
   END SUBROUTINE div_hor
   
   !!======================================================================
END MODULE divhor
