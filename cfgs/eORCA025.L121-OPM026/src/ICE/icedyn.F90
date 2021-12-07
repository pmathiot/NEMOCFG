MODULE icedyn
   !!======================================================================
   !!                     ***  MODULE  icedyn  ***
   !!   Sea-Ice dynamics : master routine for sea ice dynamics 
   !!======================================================================
   !! history :  4.0  ! 2018  (C. Rousset)  original code SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn       : dynamics of sea ice
   !!   ice_dyn_init  : initialization and namelist read
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE dom_oce        ! ocean space and time domain
   USE ice            ! sea-ice: variables
   USE icedyn_rhg     ! sea-ice: rheology
   USE icedyn_adv     ! sea-ice: advection
   USE icedyn_rdgrft  ! sea-ice: ridging/rafting
   USE icecor         ! sea-ice: corrections
   USE icevar         ! sea-ice: operations
   USE icectl         ! sea-ice: control prints
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE lbclnk         ! lateral boundary conditions (or mpp links)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn        ! called by icestp.F90
   PUBLIC   ice_dyn_init   ! called by icestp.F90
   
   INTEGER ::              nice_dyn   ! choice of the type of dynamics
   !                                        ! associated indices:
   INTEGER, PARAMETER ::   np_dynALL     = 1   ! full ice dynamics               (rheology + advection + ridging/rafting + correction)
   INTEGER, PARAMETER ::   np_dynRHGADV  = 2   ! pure dynamics                   (rheology + advection) 
   INTEGER, PARAMETER ::   np_dynADV1D   = 3   ! only advection 1D - test case from Schar & Smolarkiewicz 1996
   INTEGER, PARAMETER ::   np_dynADV2D   = 4   ! only advection 2D w prescribed vel.(rn_uvice + advection)
   !
   ! ** namelist (namdyn) **
   LOGICAL  ::   ln_dynALL        ! full ice dynamics                      (rheology + advection + ridging/rafting + correction)
   LOGICAL  ::   ln_dynRHGADV     ! no ridge/raft & no corrections         (rheology + advection)
   LOGICAL  ::   ln_dynADV1D      ! only advection in 1D w ice convergence (test case from Schar & Smolarkiewicz 1996)
   LOGICAL  ::   ln_dynADV2D      ! only advection in 2D w prescribed vel. (rn_uvice + advection)
   REAL(wp) ::   rn_uice          !    prescribed u-vel (case np_dynADV1D & np_dynADV2D)
   REAL(wp) ::   rn_vice          !    prescribed v-vel (case np_dynADV1D & np_dynADV2D)
   
   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icedyn.F90 13284 2020-07-09 15:12:23Z smasson $
   !! Software governed by the CeCILL licence     (./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_dyn( kt )
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE ice_dyn  ***
      !!               
      !! ** Purpose :   this routine manages sea ice dynamics
      !!
      !! ** Action : - calculation of friction in case of landfast ice
      !!             - call ice_dyn_rhg    = rheology
      !!             - call ice_dyn_adv    = advection
      !!             - call ice_dyn_rdgrft = ridging/rafting
      !!             - call ice_cor        = corrections if fields are out of bounds
      !!--------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! ice time step
      !!
      INTEGER  ::   ji, jj        ! dummy loop indices
      REAL(wp) ::   zcoefu, zcoefv
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdivu_i
      !!--------------------------------------------------------------------
      !
      ! controls
      IF( ln_timing )   CALL timing_start('icedyn')
      !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*)'ice_dyn: sea-ice dynamics'
         WRITE(numout,*)'~~~~~~~'
      ENDIF
      !                      
      ! retrieve thickness from volume for landfast param. and UMx advection scheme
      WHERE( a_i(:,:,:) >= epsi20 )
         h_i(:,:,:) = v_i(:,:,:) / a_i_b(:,:,:)
         h_s(:,:,:) = v_s(:,:,:) / a_i_b(:,:,:)
      ELSEWHERE
         h_i(:,:,:) = 0._wp
         h_s(:,:,:) = 0._wp
      END WHERE
      !
      WHERE( a_ip(:,:,:) >= epsi20 )
         h_ip(:,:,:) = v_ip(:,:,:) / a_ip(:,:,:)
         h_il(:,:,:) = v_il(:,:,:) / a_ip(:,:,:)
      ELSEWHERE
         h_ip(:,:,:) = 0._wp
         h_il(:,:,:) = 0._wp
      END WHERE
      !
      !
      SELECT CASE( nice_dyn )          !-- Set which dynamics is running

      CASE ( np_dynALL )           !==  all dynamical processes  ==!
         !
         CALL ice_dyn_rhg   ( kt )                                          ! -- rheology  
         CALL ice_dyn_adv   ( kt )                                          ! -- advection of ice
         CALL ice_dyn_rdgrft( kt )                                          ! -- ridging/rafting 
         CALL ice_cor       ( kt , 1 )                                      ! -- Corrections
         !
      CASE ( np_dynRHGADV  )       !==  no ridge/raft & no corrections ==!
         !
         CALL ice_dyn_rhg   ( kt )                                          ! -- rheology  
         CALL ice_dyn_adv   ( kt )                                          ! -- advection of ice
         CALL Hpiling                                                       ! -- simple pile-up (replaces ridging/rafting)
         CALL ice_var_zapsmall                                              ! -- zap small areas
         !
      CASE ( np_dynADV1D )         !==  pure advection ==!   (1D)
         !
         ! --- monotonicity test from Schar & Smolarkiewicz 1996 --- !
         ! CFL = 0.5 at a distance from the bound of 1/6 of the basin length
         ! Then for dx = 2m and dt = 1s => rn_uice = u (1/6th) = 1m/s 
         DO jj = 1, jpj
            DO ji = 1, jpi
               zcoefu = ( REAL(jpiglo+1)*0.5 - REAL(ji+nimpp-1) ) / ( REAL(jpiglo+1)*0.5 - 1. )
               zcoefv = ( REAL(jpjglo+1)*0.5 - REAL(jj+njmpp-1) ) / ( REAL(jpjglo+1)*0.5 - 1. )
               u_ice(ji,jj) = rn_uice * 1.5 * SIGN( 1., zcoefu ) * ABS( zcoefu ) * umask(ji,jj,1)
               v_ice(ji,jj) = rn_vice * 1.5 * SIGN( 1., zcoefv ) * ABS( zcoefv ) * vmask(ji,jj,1)
            END DO
         END DO
         ! ---
         CALL ice_dyn_adv   ( kt )                                          ! -- advection of ice
         !
      CASE ( np_dynADV2D )         !==  pure advection ==!   (2D w prescribed velocities)
         !
         u_ice(:,:) = rn_uice * umask(:,:,1)
         v_ice(:,:) = rn_vice * vmask(:,:,1)
         !CALL RANDOM_NUMBER(u_ice(:,:)) ; u_ice(:,:) = u_ice(:,:) * 0.1 + rn_uice * 0.9 * umask(:,:,1)
         !CALL RANDOM_NUMBER(v_ice(:,:)) ; v_ice(:,:) = v_ice(:,:) * 0.1 + rn_vice * 0.9 * vmask(:,:,1)
         ! ---
         CALL ice_dyn_adv   ( kt )                                          ! -- advection of ice

      END SELECT
      !
      !
      ! diagnostics: divergence at T points 
      IF( iom_use('icediv') ) THEN
         !
         SELECT CASE( nice_dyn )

         CASE ( np_dynADV1D , np_dynADV2D )

            ALLOCATE( zdivu_i(jpi,jpj) )
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  zdivu_i(ji,jj) = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
                     &             + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1) ) * r1_e1e2t(ji,jj)
               END DO
            END DO
            CALL lbc_lnk( 'icedyn', zdivu_i, 'T', 1. )
            ! output
            CALL iom_put( 'icediv' , zdivu_i )

            DEALLOCATE( zdivu_i )

         END SELECT
         !
      ENDIF
      !
      ! controls
      IF( ln_timing )   CALL timing_stop ('icedyn')
      !
   END SUBROUTINE ice_dyn


   SUBROUTINE Hpiling
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE Hpiling  ***
      !!
      !! ** Purpose : Simple conservative piling comparable with 1-cat models
      !!
      !! ** Method  : pile-up ice when no ridging/rafting
      !!
      !! ** input   : a_i
      !!-------------------------------------------------------------------
      INTEGER ::   jl         ! dummy loop indices
      !!-------------------------------------------------------------------
      ! controls
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'Hpiling', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      !
      at_i(:,:) = SUM( a_i(:,:,:), dim=3 )
      DO jl = 1, jpl
         WHERE( at_i(:,:) > epsi20 )
            a_i(:,:,jl) = a_i(:,:,jl) * (  1._wp + MIN( rn_amax_2d(:,:) - at_i(:,:) , 0._wp ) / at_i(:,:)  )
         END WHERE
      END DO
      ! controls
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'Hpiling', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      !
   END SUBROUTINE Hpiling


   SUBROUTINE ice_dyn_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dyn_init  ***
      !!
      !! ** Purpose : Physical constants and parameters linked to the ice
      !!      dynamics
      !!
      !! ** Method  :  Read the namdyn namelist and check the ice-dynamic
      !!       parameter values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namdyn
      !!-------------------------------------------------------------------
      INTEGER ::   ios, ioptio   ! Local integer output status for namelist read
      !!
      NAMELIST/namdyn/ ln_dynALL, ln_dynRHGADV, ln_dynADV1D, ln_dynADV2D, rn_uice, rn_vice,  &
         &             rn_ishlat ,                                                           &
         &             ln_landfast_L16, rn_lf_depfra, rn_lf_bfr, rn_lf_relax, rn_lf_tensile
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )         ! Namelist namdyn in reference namelist : Ice dynamics
      READ  ( numnam_ice_ref, namdyn, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namdyn in reference namelist' )
      REWIND( numnam_ice_cfg )         ! Namelist namdyn in configuration namelist : Ice dynamics
      READ  ( numnam_ice_cfg, namdyn, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namdyn in configuration namelist' )
      IF(lwm) WRITE( numoni, namdyn )
      !
      IF(lwp) THEN                     ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_dyn_init: ice parameters for ice dynamics '
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namdyn:'
         WRITE(numout,*) '      Full ice dynamics      (rhg + adv + ridge/raft + corr) ln_dynALL       = ', ln_dynALL
         WRITE(numout,*) '      No ridge/raft & No cor (rhg + adv)                     ln_dynRHGADV    = ', ln_dynRHGADV
         WRITE(numout,*) '      Advection 1D only      (Schar & Smolarkiewicz 1996)    ln_dynADV1D     = ', ln_dynADV1D
         WRITE(numout,*) '      Advection 2D only      (rn_uvice + adv)                ln_dynADV2D     = ', ln_dynADV2D
         WRITE(numout,*) '         with prescribed velocity given by   (u,v)_ice = (rn_uice,rn_vice)   = (', rn_uice,',',rn_vice,')'
         WRITE(numout,*) '      lateral boundary condition for sea ice dynamics        rn_ishlat       = ', rn_ishlat
         WRITE(numout,*) '      Landfast: param from Lemieux 2016                      ln_landfast_L16 = ', ln_landfast_L16
         WRITE(numout,*) '         fraction of ocean depth that ice must reach         rn_lf_depfra    = ', rn_lf_depfra
         WRITE(numout,*) '         maximum bottom stress per unit area of contact      rn_lf_bfr       = ', rn_lf_bfr
         WRITE(numout,*) '         relax time scale (s-1) to reach static friction     rn_lf_relax     = ', rn_lf_relax
         WRITE(numout,*) '         isotropic tensile strength                          rn_lf_tensile   = ', rn_lf_tensile
         WRITE(numout,*)
      ENDIF
      !                             !== set the choice of ice dynamics ==!
      ioptio = 0 
      !      !--- full dynamics                               (rheology + advection + ridging/rafting + correction)
      IF( ln_dynALL    ) THEN   ;   ioptio = ioptio + 1   ;   nice_dyn = np_dynALL       ;   ENDIF
      !      !--- dynamics without ridging/rafting and corr   (rheology + advection)
      IF( ln_dynRHGADV ) THEN   ;   ioptio = ioptio + 1   ;   nice_dyn = np_dynRHGADV    ;   ENDIF
      !      !--- advection 1D only - test case from Schar & Smolarkiewicz 1996
      IF( ln_dynADV1D  ) THEN   ;   ioptio = ioptio + 1   ;   nice_dyn = np_dynADV1D     ;   ENDIF
      !      !--- advection 2D only with prescribed ice velocities (from namelist)
      IF( ln_dynADV2D  ) THEN   ;   ioptio = ioptio + 1   ;   nice_dyn = np_dynADV2D     ;   ENDIF
      !
      IF( ioptio /= 1 )    CALL ctl_stop( 'ice_dyn_init: one and only one ice dynamics option has to be defined ' )
      !
      !                                      !--- Lateral boundary conditions
      IF     (      rn_ishlat == 0.                ) THEN   ;   IF(lwp) WRITE(numout,*) '   ===>>>   ice lateral  free-slip'
      ELSEIF (      rn_ishlat == 2.                ) THEN   ;   IF(lwp) WRITE(numout,*) '   ===>>>   ice lateral  no-slip'
      ELSEIF ( 0. < rn_ishlat .AND. rn_ishlat < 2. ) THEN   ;   IF(lwp) WRITE(numout,*) '   ===>>>   ice lateral  partial-slip'
      ELSEIF ( 2. < rn_ishlat                      ) THEN   ;   IF(lwp) WRITE(numout,*) '   ===>>>   ice lateral  strong-slip'
      ENDIF
      !                                      !--- Landfast ice
      IF( .NOT.ln_landfast_L16 )   tau_icebfr(:,:) = 0._wp
      !
      CALL ice_dyn_rdgrft_init          ! set ice ridging/rafting parameters
      CALL ice_dyn_rhg_init             ! set ice rheology parameters
      CALL ice_dyn_adv_init             ! set ice advection parameters
      !
   END SUBROUTINE ice_dyn_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif 

   !!======================================================================
END MODULE icedyn
