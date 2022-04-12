MODULE isfload
   !!======================================================================
   !!                       ***  MODULE  isfload  ***
   !! isfload module :  compute ice shelf load (needed for the hpg)
   !!======================================================================
   !! History :  4.1  !  2019-09  (P. Mathiot) original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfload      : compute ice shelf load
   !!----------------------------------------------------------------------

   USE isf_oce, ONLY: cn_isfload, rn_isfload_T, rn_isfload_S ! ice shelf variables

   USE dom_oce, ONLY: e3w_n, gdept_n, risfdep, mikt          ! vertical scale factor
   USE eosbn2 , ONLY: eos                                    ! eos routine

   USE lib_mpp, ONLY: ctl_stop                               ! ctl_stop routine
   USE in_out_manager                                        ! 

   IMPLICIT NONE

   PRIVATE

   PUBLIC isf_load

CONTAINS

   SUBROUTINE isf_load ( pisfload )
      !!--------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_load  ***
      !!
      !! ** Purpose : compute the ice shelf load
      !!
      !!--------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pisfload
      !!-------------------------- IN  -------------------------------------
      !!----------------------------------------------------------------------
      !
      ! quality test: ice shelf in a stratify/uniform ocean should not drive any flow.
      !               the smaller the residual flow is, the better it is.
      !
      ! ice shelf cavity
      SELECT CASE ( cn_isfload )
      CASE ( 'uniform' )
         CALL isf_load_uniform ( pisfload )
      CASE DEFAULT
         CALL ctl_stop('STOP','method cn_isfload to compute ice shelf load does not exist (isomip), check your namelist')
      END SELECT
      !
   END SUBROUTINE isf_load

   SUBROUTINE isf_load_uniform( pisfload )
      !!--------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_load  ***
      !!
      !! ** Purpose : compute the ice shelf load
      !!
      !! ** Method  : The ice shelf is assumed to be in hydro static equilibrium
      !!              in water at -1.9 C and 34.4 PSU. Weight of the ice shelf is
      !!              integrated from top to bottom.
      !!
      !!--------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pisfload
      !!-------------------------- IN  -------------------------------------
      !!--------------------------------------------------------------------
      INTEGER  :: ji, jj, jk
      INTEGER  :: ikt
      REAL(wp)                          :: znad        ! 
      REAL(wp), DIMENSION(jpi,jpj)      :: zrhdtop_isf ! water density    displaced by the ice shelf (at the interface)
      REAL(wp), DIMENSION(jpi,jpj,jpts) :: zts_top     ! water properties displaced by the ice shelf   
      REAL(wp), DIMENSION(jpi,jpj,jpk)  :: zrhd        ! water density    displaced by the ice shelf
      !!----------------------------------------------------------------------
      !
      znad = 1._wp                     !- To use density and not density anomaly
      !
      !                                !- assume water displaced by the ice shelf is at T=rn_isfload_T and S=rn_isfload_S (rude)
      zts_top(:,:,jp_tem) = rn_isfload_T   ;   zts_top(:,:,jp_sal) = rn_isfload_S
      !
      DO jk = 1, jpk                   !- compute density of the water displaced by the ice shelf 
         CALL eos( zts_top(:,:,:), gdept_n(:,:,jk), zrhd(:,:,jk) )
      END DO
      !
      !                                !- compute rhd at the ice/oce interface (ice shelf side)
      CALL eos( zts_top , risfdep, zrhdtop_isf )
      !
      !                                !- Surface value + ice shelf gradient
      pisfload(:,:) = 0._wp                       ! compute pressure due to ice shelf load 
      DO jj = 1, jpj                         ! (used to compute hpgi/j for all the level from 1 to miku/v)
         DO ji = 1, jpi                      ! divided by 2 later
            ikt = mikt(ji,jj)
            !
            IF ( ikt > 1 ) THEN
               !
               ! top layer of the ice shelf
               pisfload(ji,jj) = pisfload(ji,jj) + (znad + zrhd(ji,jj,1) ) * e3w_n(ji,jj,1)
               !
               ! core layers of the ice shelf
               DO jk = 2, ikt-1
                  pisfload(ji,jj) = pisfload(ji,jj) + (2._wp * znad + zrhd(ji,jj,jk-1) + zrhd(ji,jj,jk)) * e3w_n(ji,jj,jk)
               END DO
               !
               ! deepest part of the ice shelf (between deepest T point and ice/ocean interface
               pisfload(ji,jj) = pisfload(ji,jj) + (2._wp * znad + zrhdtop_isf(ji,jj) + zrhd(ji,jj,ikt-1)) &
                  &                                              * ( risfdep(ji,jj) - gdept_n(ji,jj,ikt-1) )
               !
            END IF
         END DO
      END DO
      !
   END SUBROUTINE isf_load_uniform

END MODULE isfload
