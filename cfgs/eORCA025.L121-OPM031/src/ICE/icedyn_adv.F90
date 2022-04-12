MODULE icedyn_adv
   !!======================================================================
   !!                       ***  MODULE icedyn_adv   ***
   !!   sea-ice: advection
   !!======================================================================
   !! History :  4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn_adv   : advection of sea ice variables
   !!----------------------------------------------------------------------
   USE phycst         ! physical constant
   USE dom_oce        ! ocean domain
   USE sbc_oce , ONLY : nn_fsbc   ! frequency of sea-ice call
   USE ice            ! sea-ice: variables
   USE icevar         ! sea-ice: operations
   USE icedyn_adv_pra ! sea-ice: advection scheme (Prather)
   USE icedyn_adv_umx ! sea-ice: advection scheme (ultimate-macho)
   USE icectl         ! sea-ice: control prints
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE timing         ! Timing
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn_adv        ! called by icestp
   PUBLIC   ice_dyn_adv_init   ! called by icedyn

   INTEGER ::              nice_adv   ! choice of the type of advection scheme
   !                                        ! associated indices:
   INTEGER, PARAMETER ::   np_advPRA = 1   ! Prather scheme
   INTEGER, PARAMETER ::   np_advUMx = 2   ! Ultimate-Macho scheme
   !
   ! ** namelist (namdyn_adv) **
   INTEGER         ::   nn_UMx       ! order of the UMx advection scheme   
   !
   !! * Substitution
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icedyn_adv.F90 13284 2020-07-09 15:12:23Z smasson $
   !! Software governed by the CeCILL licence     (./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_dyn_adv( kt ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE ice_dyn_adv ***
      !!                    
      !! ** purpose : advection of sea ice
      !!
      !! ** method  : One can choose between 
      !!     a) an Ultimate-Macho scheme (with order defined by nn_UMx) => ln_adv_UMx
      !!     b) and a second order Prather scheme => ln_adv_Pra
      !!
      !! ** action :
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! number of iteration
      !!---------------------------------------------------------------------
      !
      ! controls
      IF( ln_timing    )   CALL timing_start('icedyn_adv')                                                             ! timing
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'icedyn_adv', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ice_dyn_adv: sea-ice advection'
         WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
      !
      !---------------!
      !== Advection ==!
      !---------------!
      SELECT CASE( nice_adv )
      !                                !-----------------------!
      CASE( np_advUMx )                ! ULTIMATE-MACHO scheme !
         !                             !-----------------------!
         CALL ice_dyn_adv_umx( nn_UMx, kt, u_ice, v_ice, h_i, h_s, h_ip, &
            &                          ato_i, v_i, v_s, sv_i, oa_i, a_i, a_ip, v_ip, v_il, e_s, e_i )
         !                             !-----------------------!
      CASE( np_advPRA )                ! PRATHER scheme        !
         !                             !-----------------------!
         CALL ice_dyn_adv_pra(         kt, u_ice, v_ice, h_i, h_s, h_ip, &
            &                          ato_i, v_i, v_s, sv_i, oa_i, a_i, a_ip, v_ip, v_il, e_s, e_i )
      END SELECT

      !------------
      ! diagnostics
      !------------
      diag_trp_ei(:,:) = SUM(SUM( e_i (:,:,1:nlay_i,:) - e_i_b (:,:,1:nlay_i,:), dim=4 ), dim=3 ) * r1_rdtice
      diag_trp_es(:,:) = SUM(SUM( e_s (:,:,1:nlay_s,:) - e_s_b (:,:,1:nlay_s,:), dim=4 ), dim=3 ) * r1_rdtice
      diag_trp_sv(:,:) = SUM(     sv_i(:,:,:)          - sv_i_b(:,:,:)                  , dim=3 ) * r1_rdtice
      diag_trp_vi(:,:) = SUM(     v_i (:,:,:)          - v_i_b (:,:,:)                  , dim=3 ) * r1_rdtice
      diag_trp_vs(:,:) = SUM(     v_s (:,:,:)          - v_s_b (:,:,:)                  , dim=3 ) * r1_rdtice
      IF( iom_use('icemtrp') )   CALL iom_put( 'icemtrp' ,  diag_trp_vi * rhoi          )   ! ice mass transport
      IF( iom_use('snwmtrp') )   CALL iom_put( 'snwmtrp' ,  diag_trp_vs * rhos          )   ! snw mass transport
      IF( iom_use('salmtrp') )   CALL iom_put( 'salmtrp' ,  diag_trp_sv * rhoi * 1.e-03 )   ! salt mass transport (kg/m2/s)
      IF( iom_use('dihctrp') )   CALL iom_put( 'dihctrp' , -diag_trp_ei                 )   ! advected ice heat content (W/m2)
      IF( iom_use('dshctrp') )   CALL iom_put( 'dshctrp' , -diag_trp_es                 )   ! advected snw heat content (W/m2)

      ! controls
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'icedyn_adv', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      IF( ln_icectl    )   CALL ice_prt     (kt, iiceprt, jiceprt,-1, ' - ice dyn & trp - ')                           ! prints
      IF( ln_timing    )   CALL timing_stop ('icedyn_adv')                                                             ! timing
      !
   END SUBROUTINE ice_dyn_adv


   SUBROUTINE ice_dyn_adv_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dyn_adv_init  ***
      !!
      !! ** Purpose :   Physical constants and parameters linked to the ice
      !!                dynamics
      !!
      !! ** Method  :   Read the namdyn_adv namelist and check the ice-dynamic
      !!                parameter values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namdyn_adv
      !!-------------------------------------------------------------------
      INTEGER ::   ios, ioptio   ! Local integer output status for namelist read
      !!
      NAMELIST/namdyn_adv/ ln_adv_Pra, ln_adv_UMx, nn_UMx
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )         ! Namelist namdyn_adv in reference namelist : Ice dynamics
      READ  ( numnam_ice_ref, namdyn_adv, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namdyn_adv in reference namelist' )
      REWIND( numnam_ice_cfg )         ! Namelist namdyn_adv in configuration namelist : Ice dynamics
      READ  ( numnam_ice_cfg, namdyn_adv, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namdyn_adv in configuration namelist' )
      IF(lwm) WRITE( numoni, namdyn_adv )
      !
      IF(lwp) THEN                     ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_dyn_adv_init: ice parameters for ice dynamics '
         WRITE(numout,*) '~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namdyn_adv:'
         WRITE(numout,*) '      type of advection scheme (Prather)             ln_adv_Pra = ', ln_adv_Pra 
         WRITE(numout,*) '      type of advection scheme (Ulimate-Macho)       ln_adv_UMx = ', ln_adv_UMx 
         WRITE(numout,*) '         order of the Ultimate-Macho scheme          nn_UMx     = ', nn_UMx
      ENDIF
      !
      !                             !== set the choice of ice advection ==!
      ioptio = 0 
      IF( ln_adv_Pra ) THEN   ;   ioptio = ioptio + 1   ;   nice_adv = np_advPRA    ;   ENDIF
      IF( ln_adv_UMx ) THEN   ;   ioptio = ioptio + 1   ;   nice_adv = np_advUMx    ;   ENDIF
      IF( ioptio /= 1 )   CALL ctl_stop( 'ice_dyn_adv_init: choose one and only one ice adv. scheme (ln_adv_Pra or ln_adv_UMx)' )
      !
      IF( ln_adv_Pra )   CALL adv_pra_init  !* read or initialize all required files
      !
   END SUBROUTINE ice_dyn_adv_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icedyn_adv

