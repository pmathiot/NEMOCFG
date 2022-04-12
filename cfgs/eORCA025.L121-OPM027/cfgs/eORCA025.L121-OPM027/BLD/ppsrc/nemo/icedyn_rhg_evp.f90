










MODULE icedyn_rhg_evp
   !!======================================================================
   !!                     ***  MODULE  icedyn_rhg_evp  ***
   !!   Sea-Ice dynamics : rheology Elasto-Viscous-Plastic
   !!======================================================================
   !! History :   -   !  2007-03  (M.A. Morales Maqueda, S. Bouillon) Original code
   !!            3.0  !  2008-03  (M. Vancoppenolle) adaptation to new model
   !!             -   !  2008-11  (M. Vancoppenolle, S. Bouillon, Y. Aksenov) add surface tilt in ice rheolohy 
   !!            3.3  !  2009-05  (G.Garric)    addition of the evp case
   !!            3.4  !  2011-01  (A. Porter)   dynamical allocation 
   !!            3.5  !  2012-08  (R. Benshila) AGRIF
   !!            3.6  !  2016-06  (C. Rousset)  Rewriting + landfast ice + mEVP (Bouillon 2013)
   !!            3.7  !  2017     (C. Rousset)  add aEVP (Kimmritz 2016-2017)
   !!            4.0  !  2018     (many people) SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn_rhg_evp : computes ice velocities from EVP rheology
   !!   rhg_evp_rst     : read/write EVP fields in ice restart
   !!----------------------------------------------------------------------
   USE phycst         ! Physical constant
   USE dom_oce        ! Ocean domain
   USE sbc_oce , ONLY : ln_ice_embd, nn_fsbc, ssh_m
   USE sbc_ice , ONLY : utau_ice, vtau_ice, snwice_mass, snwice_mass_b
   USE ice            ! sea-ice: ice variables
   USE icevar         ! ice_var_sshdyn
   USE icedyn_rdgrft  ! sea-ice: ice strength
   USE bdy_oce , ONLY : ln_bdy 
   USE bdyice 
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE lbclnk         ! lateral boundary conditions (or mpp links)
   USE prtctl         ! Print control

   USE netcdf         ! NetCDF library for convergence test
   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn_rhg_evp   ! called by icedyn_rhg.F90
   PUBLIC   rhg_evp_rst       ! called by icedyn_rhg.F90

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

   !! for convergence tests
   INTEGER ::   ncvgid   ! netcdf file id
   INTEGER ::   nvarid   ! netcdf variable id
   REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zmsk00, zmsk15
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icedyn_rhg_evp.F90 13646 2020-10-20 15:33:01Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_dyn_rhg_evp( kt, pstress1_i, pstress2_i, pstress12_i, pshear_i, pdivu_i, pdelta_i )
      !!-------------------------------------------------------------------
      !!                 ***  SUBROUTINE ice_dyn_rhg_evp  ***
      !!                             EVP-C-grid
      !!
      !! ** purpose : determines sea ice drift from wind stress, ice-ocean
      !!  stress and sea-surface slope. Ice-ice interaction is described by 
      !!  a non-linear elasto-viscous-plastic (EVP) law including shear 
      !!  strength and a bulk rheology (Hunke and Dukowicz, 2002).	
      !!
      !!  The points in the C-grid look like this, dear reader
      !!
      !!                              (ji,jj)
      !!                                 |
      !!                                 |
      !!                      (ji-1,jj)  |  (ji,jj)
      !!                             ---------   
      !!                            |         |
      !!                            | (ji,jj) |------(ji,jj)
      !!                            |         |
      !!                             ---------   
      !!                     (ji-1,jj-1)     (ji,jj-1)
      !!
      !! ** Inputs  : - wind forcing (stress), oceanic currents
      !!                ice total volume (vt_i) per unit area
      !!                snow total volume (vt_s) per unit area
      !!
      !! ** Action  : - compute u_ice, v_ice : the components of the 
      !!                sea-ice velocity vector
      !!              - compute delta_i, shear_i, divu_i, which are inputs
      !!                of the ice thickness distribution
      !!
      !! ** Steps   : 0) compute mask at F point
      !!              1) Compute ice snow mass, ice strength 
      !!              2) Compute wind, oceanic stresses, mass terms and
      !!                 coriolis terms of the momentum equation
      !!              3) Solve the momentum equation (iterative procedure)
      !!              4) Recompute delta, shear and divergence
      !!                 (which are inputs of the ITD) & store stress
      !!                 for the next time step
      !!              5) Diagnostics including charge ellipse
      !!
      !! ** Notes   : There is the possibility to use aEVP from the nice work of Kimmritz et al. (2016 & 2017)
      !!              by setting up ln_aEVP=T (i.e. changing alpha and beta parameters).
      !!              This is an upgraded version of mEVP from Bouillon et al. 2013
      !!              (i.e. more stable and better convergence)
      !!
      !! References : Hunke and Dukowicz, JPO97
      !!              Bouillon et al., Ocean Modelling 2009
      !!              Bouillon et al., Ocean Modelling 2013
      !!              Kimmritz et al., Ocean Modelling 2016 & 2017
      !!-------------------------------------------------------------------
      INTEGER                 , INTENT(in   ) ::   kt                                    ! time step
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   pstress1_i, pstress2_i, pstress12_i   !
      REAL(wp), DIMENSION(:,:), INTENT(  out) ::   pshear_i  , pdivu_i   , pdelta_i      !
      !!
      INTEGER ::   ji, jj       ! dummy loop indices
      INTEGER ::   jter         ! local integers
      !
      REAL(wp) ::   zrhoco                                              ! rau0 * rn_cio
      REAL(wp) ::   zdtevp, z1_dtevp                                    ! time step for subcycling
      REAL(wp) ::   ecc2, z1_ecc2                                       ! square of yield ellipse eccenticity
      REAL(wp) ::   zalph1, z1_alph1, zalph2, z1_alph2                  ! alpha coef from Bouillon 2009 or Kimmritz 2017
      REAl(wp) ::   zbetau, zbetav
      REAL(wp) ::   zm1, zm2, zm3, zmassU, zmassV, zvU, zvV             ! ice/snow mass and volume
      REAL(wp) ::   zp_delf, zds2, zdt, zdt2, zdiv, zdiv2               ! temporary scalars
      REAL(wp) ::   zTauO, zTauB, zRHS, zvel                            ! temporary scalars
      REAL(wp) ::   zkt                                                 ! isotropic tensile strength for landfast ice
      REAL(wp) ::   zvCr                                                ! critical ice volume above which ice is landfast
      !
      REAL(wp) ::   zintb, zintn                                        ! dummy argument
      REAL(wp) ::   zfac_x, zfac_y
      REAL(wp) ::   zshear, zdum1, zdum2
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zdelta, zp_delt                 ! delta and P/delta at T points
      REAL(wp), DIMENSION(jpi,jpj) ::   zten_i                          ! tension
      REAL(wp), DIMENSION(jpi,jpj) ::   zbeta                           ! beta coef from Kimmritz 2017
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zdt_m                           ! (dt / ice-snow_mass) on T points
      REAL(wp), DIMENSION(jpi,jpj) ::   zaU  , zaV                      ! ice fraction on U/V points
      REAL(wp), DIMENSION(jpi,jpj) ::   zmU_t, zmV_t                    ! (ice-snow_mass / dt) on U/V points
      REAL(wp), DIMENSION(jpi,jpj) ::   zmf                             ! coriolis parameter at T points
      REAL(wp), DIMENSION(jpi,jpj) ::   v_oceU, u_oceV, v_iceU, u_iceV  ! ocean/ice u/v component on V/U points
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zds                             ! shear
      REAL(wp), DIMENSION(jpi,jpj) ::   zs1, zs2, zs12                  ! stress tensor components
      REAL(wp), DIMENSION(jpi,jpj) ::   zsshdyn                         ! array used for the calculation of ice surface slope:
      !                                                                 !    ocean surface (ssh_m) if ice is not embedded
      !                                                                 !    ice bottom surface if ice is embedded   
      REAL(wp), DIMENSION(jpi,jpj) ::   zfU  , zfV                      ! internal stresses
      REAL(wp), DIMENSION(jpi,jpj) ::   zspgU, zspgV                    ! surface pressure gradient at U/V points
      REAL(wp), DIMENSION(jpi,jpj) ::   zCorU, zCorV                    ! Coriolis stress array
      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_ai, ztauy_ai              ! ice-atm. stress at U-V points
      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_oi, ztauy_oi              ! ice-ocean stress at U-V points
      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_bi, ztauy_bi              ! ice-OceanBottom stress at U-V points (landfast)
      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_base, ztauy_base          ! ice-bottom stress at U-V points (landfast)
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zmsk01x, zmsk01y                ! dummy arrays
      REAL(wp), DIMENSION(jpi,jpj) ::   zmsk00x, zmsk00y                ! mask for ice presence
      REAL(wp), DIMENSION(jpi,jpj) ::   zfmask                          ! mask at F points for the ice

      REAL(wp), PARAMETER          ::   zepsi  = 1.0e-20_wp             ! tolerance parameter
      REAL(wp), PARAMETER          ::   zmmin  = 1._wp                  ! ice mass (kg/m2)  below which ice velocity becomes very small
      REAL(wp), PARAMETER          ::   zamin  = 0.001_wp               ! ice concentration below which ice velocity becomes very small
      !! --- check convergence
      REAL(wp), DIMENSION(jpi,jpj) ::   zu_ice, zv_ice
      !! --- diags
      REAL(wp) ::   zsig1, zsig2, zsig12, zfac, z1_strength
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zsig_I, zsig_II, zsig1_p, zsig2_p         
      !! --- SIMIP diags
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_xmtrp_ice ! X-component of ice mass transport (kg/s)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_ymtrp_ice ! Y-component of ice mass transport (kg/s)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_xmtrp_snw ! X-component of snow mass transport (kg/s)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_ymtrp_snw ! Y-component of snow mass transport (kg/s)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_xatrp     ! X-component of area transport (m2/s)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_yatrp     ! Y-component of area transport (m2/s)      
      !!-------------------------------------------------------------------

      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_dyn_rhg_evp: EVP sea-ice rheology'
      !
      ! for diagnostics and convergence tests
      ALLOCATE( zmsk00(jpi,jpj), zmsk15(jpi,jpj) )
      DO jj = 1, jpj
         DO ji = 1, jpi
            zmsk00(ji,jj) = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi06  ) ) ! 1 if ice    , 0 if no ice
            zmsk15(ji,jj) = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - 0.15_wp ) ) ! 1 if 15% ice, 0 if less
         END DO
      END DO
      !
      !!gm for Clem:  OPTIMIZATION:  I think zfmask can be computed one for all at the initialization....
      !------------------------------------------------------------------------------!
      ! 0) mask at F points for the ice
      !------------------------------------------------------------------------------!
      ! ocean/land mask
      DO jj = 1, jpjm1
         DO ji = 1, jpim1      ! NO vector opt.
            zfmask(ji,jj) = tmask(ji,jj,1) * tmask(ji+1,jj,1) * tmask(ji,jj+1,1) * tmask(ji+1,jj+1,1)
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_rhg_evp', zfmask, 'F', 1._wp )

      ! Lateral boundary conditions on velocity (modify zfmask)
      DO jj = 2, jpjm1
         DO ji = 2, jpim1   ! vector opt.
            IF( zfmask(ji,jj) == 0._wp ) THEN
               zfmask(ji,jj) = rn_ishlat * MIN( 1._wp , MAX( umask(ji,jj,1), umask(ji,jj+1,1), &
                  &                                          vmask(ji,jj,1), vmask(ji+1,jj,1) ) )
            ENDIF
         END DO
      END DO
      DO jj = 2, jpjm1
         IF( zfmask(1,jj) == 0._wp ) THEN
            zfmask(1  ,jj) = rn_ishlat * MIN( 1._wp , MAX( vmask(2,jj,1), umask(1,jj+1,1), umask(1,jj,1) ) )
         ENDIF
         IF( zfmask(jpi,jj) == 0._wp ) THEN
            zfmask(jpi,jj) = rn_ishlat * MIN( 1._wp , MAX( umask(jpi,jj+1,1), vmask(jpim1,jj,1), umask(jpi,jj-1,1) ) )
        ENDIF
      END DO
      DO ji = 2, jpim1
         IF( zfmask(ji,1) == 0._wp ) THEN
            zfmask(ji, 1 ) = rn_ishlat * MIN( 1._wp , MAX( vmask(ji+1,1,1), umask(ji,2,1), vmask(ji,1,1) ) )
         ENDIF
         IF( zfmask(ji,jpj) == 0._wp ) THEN
            zfmask(ji,jpj) = rn_ishlat * MIN( 1._wp , MAX( vmask(ji+1,jpj,1), vmask(ji-1,jpj,1), umask(ji,jpjm1,1) ) )
         ENDIF
      END DO
      CALL lbc_lnk( 'icedyn_rhg_evp', zfmask, 'F', 1._wp )

      !------------------------------------------------------------------------------!
      ! 1) define some variables and initialize arrays
      !------------------------------------------------------------------------------!
      zrhoco = rau0 * rn_cio 

      ! ecc2: square of yield ellipse eccenticrity
      ecc2    = rn_ecc * rn_ecc
      z1_ecc2 = 1._wp / ecc2

      ! alpha parameters (Bouillon 2009)
      IF( .NOT. ln_aEVP ) THEN
         zdtevp   = rdt_ice / REAL( nn_nevp )
         zalph1 =   2._wp * rn_relast * REAL( nn_nevp )
         zalph2 = zalph1 * z1_ecc2

         z1_alph1 = 1._wp / ( zalph1 + 1._wp )
         z1_alph2 = 1._wp / ( zalph2 + 1._wp )
      ELSE
         zdtevp   = rdt_ice
         ! zalpha parameters set later on adaptatively
      ENDIF
      z1_dtevp = 1._wp / zdtevp
         
      ! Initialise stress tensor 
      zs1 (:,:) = pstress1_i (:,:) 
      zs2 (:,:) = pstress2_i (:,:)
      zs12(:,:) = pstress12_i(:,:)

      ! Ice strength
      CALL ice_strength

      ! landfast param from Lemieux(2016): add isotropic tensile strength (following Konig Beatty and Holland, 2010)
      IF( ln_landfast_L16 ) THEN   ;   zkt = rn_lf_tensile
      ELSE                         ;   zkt = 0._wp
      ENDIF
      !
      !------------------------------------------------------------------------------!
      ! 2) Wind / ocean stress, mass terms, coriolis terms
      !------------------------------------------------------------------------------!
      ! sea surface height
      !    embedded sea ice: compute representative ice top surface
      !    non-embedded sea ice: use ocean surface for slope calculation
      zsshdyn(:,:) = ice_var_sshdyn( ssh_m, snwice_mass, snwice_mass_b)

      DO jj = 2, jpjm1
         DO ji = 2, jpim1

            ! ice fraction at U-V points
            zaU(ji,jj) = 0.5_wp * ( at_i(ji,jj) * e1e2t(ji,jj) + at_i(ji+1,jj) * e1e2t(ji+1,jj) ) * r1_e1e2u(ji,jj) * umask(ji,jj,1)
            zaV(ji,jj) = 0.5_wp * ( at_i(ji,jj) * e1e2t(ji,jj) + at_i(ji,jj+1) * e1e2t(ji,jj+1) ) * r1_e1e2v(ji,jj) * vmask(ji,jj,1)

            ! Ice/snow mass at U-V points
            zm1 = ( rhos * vt_s(ji  ,jj  ) + rhoi * vt_i(ji  ,jj  ) )
            zm2 = ( rhos * vt_s(ji+1,jj  ) + rhoi * vt_i(ji+1,jj  ) )
            zm3 = ( rhos * vt_s(ji  ,jj+1) + rhoi * vt_i(ji  ,jj+1) )
            zmassU = 0.5_wp * ( zm1 * e1e2t(ji,jj) + zm2 * e1e2t(ji+1,jj) ) * r1_e1e2u(ji,jj) * umask(ji,jj,1)
            zmassV = 0.5_wp * ( zm1 * e1e2t(ji,jj) + zm3 * e1e2t(ji,jj+1) ) * r1_e1e2v(ji,jj) * vmask(ji,jj,1)

            ! Ocean currents at U-V points
            v_oceU(ji,jj)   = 0.25_wp * ( v_oce(ji,jj) + v_oce(ji,jj-1) + v_oce(ji+1,jj) + v_oce(ji+1,jj-1) ) * umask(ji,jj,1)
            u_oceV(ji,jj)   = 0.25_wp * ( u_oce(ji,jj) + u_oce(ji-1,jj) + u_oce(ji,jj+1) + u_oce(ji-1,jj+1) ) * vmask(ji,jj,1)

            ! Coriolis at T points (m*f)
            zmf(ji,jj)      = zm1 * ff_t(ji,jj)

            ! dt/m at T points (for alpha and beta coefficients)
            zdt_m(ji,jj)    = zdtevp / MAX( zm1, zmmin )
            
            ! m/dt
            zmU_t(ji,jj)    = zmassU * z1_dtevp
            zmV_t(ji,jj)    = zmassV * z1_dtevp
            
            ! Drag ice-atm.
            ztaux_ai(ji,jj) = zaU(ji,jj) * utau_ice(ji,jj)
            ztauy_ai(ji,jj) = zaV(ji,jj) * vtau_ice(ji,jj)

            ! Surface pressure gradient (- m*g*GRAD(ssh)) at U-V points
            zspgU(ji,jj)    = - zmassU * grav * ( zsshdyn(ji+1,jj) - zsshdyn(ji,jj) ) * r1_e1u(ji,jj)
            zspgV(ji,jj)    = - zmassV * grav * ( zsshdyn(ji,jj+1) - zsshdyn(ji,jj) ) * r1_e2v(ji,jj)

            ! masks
            zmsk00x(ji,jj) = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zmassU ) )  ! 0 if no ice
            zmsk00y(ji,jj) = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zmassV ) )  ! 0 if no ice

            ! switches
            IF( zmassU <= zmmin .AND. zaU(ji,jj) <= zamin ) THEN   ;   zmsk01x(ji,jj) = 0._wp
            ELSE                                                   ;   zmsk01x(ji,jj) = 1._wp   ;   ENDIF
            IF( zmassV <= zmmin .AND. zaV(ji,jj) <= zamin ) THEN   ;   zmsk01y(ji,jj) = 0._wp
            ELSE                                                   ;   zmsk01y(ji,jj) = 1._wp   ;   ENDIF

         END DO
      END DO
      CALL lbc_lnk_multi( 'icedyn_rhg_evp', zmf, 'T', 1., zdt_m, 'T', 1. )
      !
      !                                  !== Landfast ice parameterization ==!
      !
      IF( ln_landfast_L16 ) THEN         !-- Lemieux 2016
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               ! ice thickness at U-V points
               zvU = 0.5_wp * ( vt_i(ji,jj) * e1e2t(ji,jj) + vt_i(ji+1,jj) * e1e2t(ji+1,jj) ) * r1_e1e2u(ji,jj) * umask(ji,jj,1)
               zvV = 0.5_wp * ( vt_i(ji,jj) * e1e2t(ji,jj) + vt_i(ji,jj+1) * e1e2t(ji,jj+1) ) * r1_e1e2v(ji,jj) * vmask(ji,jj,1)
               ! ice-bottom stress at U points
               zvCr = zaU(ji,jj) * rn_lf_depfra * hu_n(ji,jj)
               ztaux_base(ji,jj) = - rn_lf_bfr * MAX( 0._wp, zvU - zvCr ) * EXP( -rn_crhg * ( 1._wp - zaU(ji,jj) ) )
               ! ice-bottom stress at V points
               zvCr = zaV(ji,jj) * rn_lf_depfra * hv_n(ji,jj)
               ztauy_base(ji,jj) = - rn_lf_bfr * MAX( 0._wp, zvV - zvCr ) * EXP( -rn_crhg * ( 1._wp - zaV(ji,jj) ) )
               ! ice_bottom stress at T points
               zvCr = at_i(ji,jj) * rn_lf_depfra * ht_n(ji,jj)
               tau_icebfr(ji,jj) = - rn_lf_bfr * MAX( 0._wp, vt_i(ji,jj) - zvCr ) * EXP( -rn_crhg * ( 1._wp - at_i(ji,jj) ) )
            END DO
         END DO
         CALL lbc_lnk( 'icedyn_rhg_evp', tau_icebfr(:,:), 'T', 1. )
         !
      ELSE                               !-- no landfast
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               ztaux_base(ji,jj) = 0._wp
               ztauy_base(ji,jj) = 0._wp
            END DO
         END DO
      ENDIF

      !------------------------------------------------------------------------------!
      ! 3) Solution of the momentum equation, iterative procedure
      !------------------------------------------------------------------------------!
      !
      !                                               ! ==================== !
      DO jter = 1 , nn_nevp                           !    loop over jter    !
         !                                            ! ==================== !        
         l_full_nf_update = jter == nn_nevp   ! false: disable full North fold update (performances) for iter = 1 to nn_nevp-1
         !
         ! convergence test
         IF( nn_rhg_chkcvg == 1 .OR. nn_rhg_chkcvg == 2  ) THEN
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zu_ice(ji,jj) = u_ice(ji,jj) * umask(ji,jj,1) ! velocity at previous time step
                  zv_ice(ji,jj) = v_ice(ji,jj) * vmask(ji,jj,1)
               END DO
            END DO
         ENDIF

         ! --- divergence, tension & shear (Appendix B of Hunke & Dukowicz, 2002) --- !
         DO jj = 1, jpjm1
            DO ji = 1, jpim1

               ! shear at F points
               zds(ji,jj) = ( ( u_ice(ji,jj+1) * r1_e1u(ji,jj+1) - u_ice(ji,jj) * r1_e1u(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj)   &
                  &         + ( v_ice(ji+1,jj) * r1_e2v(ji+1,jj) - v_ice(ji,jj) * r1_e2v(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj)   &
                  &         ) * r1_e1e2f(ji,jj) * zfmask(ji,jj)

            END DO
         END DO

         DO jj = 2, jpjm1
            DO ji = 2, jpim1 ! no vector loop

               ! shear**2 at T points (doc eq. A16)
               zds2 = ( zds(ji,jj  ) * zds(ji,jj  ) * e1e2f(ji,jj  ) + zds(ji-1,jj  ) * zds(ji-1,jj  ) * e1e2f(ji-1,jj  )  &
                  &   + zds(ji,jj-1) * zds(ji,jj-1) * e1e2f(ji,jj-1) + zds(ji-1,jj-1) * zds(ji-1,jj-1) * e1e2f(ji-1,jj-1)  &
                  &   ) * 0.25_wp * r1_e1e2t(ji,jj)
              
               ! divergence at T points
               zdiv  = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
                  &    + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1)   &
                  &    ) * r1_e1e2t(ji,jj)
               zdiv2 = zdiv * zdiv
               
               ! tension at T points
               zdt  = ( ( u_ice(ji,jj) * r1_e2u(ji,jj) - u_ice(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
                  &   - ( v_ice(ji,jj) * r1_e1v(ji,jj) - v_ice(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
                  &   ) * r1_e1e2t(ji,jj)
               zdt2 = zdt * zdt
               
               ! delta at T points
               zdelta(ji,jj) = SQRT( zdiv2 + ( zdt2 + zds2 ) * z1_ecc2 )  

            END DO
         END DO
         CALL lbc_lnk( 'icedyn_rhg_evp', zdelta, 'T', 1._wp )
         
         ! P/delta at T points
         DO jj = 1, jpj
            DO ji = 1, jpi
               zp_delt(ji,jj) = strength(ji,jj) / ( zdelta(ji,jj) + rn_creepl )
            END DO
         END DO

         DO jj = 2, jpj    ! loop ends at jpi,jpj so that no lbc_lnk are needed for zs1 and zs2
            DO ji = 2, jpi ! no vector loop

               ! divergence at T points (duplication to avoid communications)
               zdiv  = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
                  &    + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1)   &
                  &    ) * r1_e1e2t(ji,jj)
               
               ! tension at T points (duplication to avoid communications)
               zdt  = ( ( u_ice(ji,jj) * r1_e2u(ji,jj) - u_ice(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
                  &   - ( v_ice(ji,jj) * r1_e1v(ji,jj) - v_ice(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
                  &   ) * r1_e1e2t(ji,jj)

               ! alpha for aEVP
               !   gamma = 0.5*P/(delta+creepl) * (c*pi)**2/Area * dt/m
               !   alpha = beta = sqrt(4*gamma)
               IF( ln_aEVP ) THEN
                  zalph1   = MAX( 50._wp, rpi * SQRT( 0.5_wp * zp_delt(ji,jj) * r1_e1e2t(ji,jj) * zdt_m(ji,jj) ) )
                  z1_alph1 = 1._wp / ( zalph1 + 1._wp )
                  zalph2   = zalph1
                  z1_alph2 = z1_alph1
                  ! explicit:
                  ! z1_alph1 = 1._wp / zalph1
                  ! z1_alph2 = 1._wp / zalph1
                  ! zalph1 = zalph1 - 1._wp
                  ! zalph2 = zalph1
               ENDIF
               
               ! stress at T points (zkt/=0 if landfast)
               zs1(ji,jj) = ( zs1(ji,jj)*zalph1 + zp_delt(ji,jj) * ( zdiv*(1._wp + zkt) - zdelta(ji,jj)*(1._wp - zkt) ) ) * z1_alph1
               zs2(ji,jj) = ( zs2(ji,jj)*zalph2 + zp_delt(ji,jj) * ( zdt * z1_ecc2 * (1._wp + zkt) ) ) * z1_alph2
             
            END DO
         END DO

         ! Save beta at T-points for further computations
         IF( ln_aEVP ) THEN
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zbeta(ji,jj) = MAX( 50._wp, rpi * SQRT( 0.5_wp * zp_delt(ji,jj) * r1_e1e2t(ji,jj) * zdt_m(ji,jj) ) )
               END DO
            END DO
         ENDIF
         
         DO jj = 1, jpjm1
            DO ji = 1, jpim1

               ! alpha for aEVP
               IF( ln_aEVP ) THEN
                  zalph2   = MAX( zbeta(ji,jj), zbeta(ji+1,jj), zbeta(ji,jj+1), zbeta(ji+1,jj+1) )
                  z1_alph2 = 1._wp / ( zalph2 + 1._wp )
                  ! explicit:
                  ! z1_alph2 = 1._wp / zalph2
                  ! zalph2 = zalph2 - 1._wp
               ENDIF
               
               ! P/delta at F points
               zp_delf = 0.25_wp * ( zp_delt(ji,jj) + zp_delt(ji+1,jj) + zp_delt(ji,jj+1) + zp_delt(ji+1,jj+1) )
               
               ! stress at F points (zkt/=0 if landfast)
               zs12(ji,jj)= ( zs12(ji,jj) * zalph2 + zp_delf * ( zds(ji,jj) * z1_ecc2 * (1._wp + zkt) ) * 0.5_wp ) * z1_alph2

            END DO
         END DO

         ! --- Ice internal stresses (Appendix C of Hunke and Dukowicz, 2002) --- !
         DO jj = 2, jpjm1
            DO ji = 2, jpim1               
               !                   !--- U points
               zfU(ji,jj) = 0.5_wp * ( ( zs1(ji+1,jj) - zs1(ji,jj) ) * e2u(ji,jj)                                             &
                  &                  + ( zs2(ji+1,jj) * e2t(ji+1,jj) * e2t(ji+1,jj) - zs2(ji,jj) * e2t(ji,jj) * e2t(ji,jj)    &
                  &                    ) * r1_e2u(ji,jj)                                                                      &
                  &                  + ( zs12(ji,jj) * e1f(ji,jj) * e1f(ji,jj) - zs12(ji,jj-1) * e1f(ji,jj-1) * e1f(ji,jj-1)  &
                  &                    ) * 2._wp * r1_e1u(ji,jj)                                                              &
                  &                  ) * r1_e1e2u(ji,jj)
               !
               !                !--- V points
               zfV(ji,jj) = 0.5_wp * ( ( zs1(ji,jj+1) - zs1(ji,jj) ) * e1v(ji,jj)                                             &
                  &                  - ( zs2(ji,jj+1) * e1t(ji,jj+1) * e1t(ji,jj+1) - zs2(ji,jj) * e1t(ji,jj) * e1t(ji,jj)    &
                  &                    ) * r1_e1v(ji,jj)                                                                      &
                  &                  + ( zs12(ji,jj) * e2f(ji,jj) * e2f(ji,jj) - zs12(ji-1,jj) * e2f(ji-1,jj) * e2f(ji-1,jj)  &
                  &                    ) * 2._wp * r1_e2v(ji,jj)                                                              &
                  &                  ) * r1_e1e2v(ji,jj)
               !
               !                !--- ice currents at U-V point
               v_iceU(ji,jj) = 0.25_wp * ( v_ice(ji,jj) + v_ice(ji,jj-1) + v_ice(ji+1,jj) + v_ice(ji+1,jj-1) ) * umask(ji,jj,1)
               u_iceV(ji,jj) = 0.25_wp * ( u_ice(ji,jj) + u_ice(ji-1,jj) + u_ice(ji,jj+1) + u_ice(ji-1,jj+1) ) * vmask(ji,jj,1)
               !
            END DO
         END DO
         !
         ! --- Computation of ice velocity --- !
         !  Bouillon et al. 2013 (eq 47-48) => unstable unless alpha, beta vary as in Kimmritz 2016 & 2017
         !  Bouillon et al. 2009 (eq 34-35) => stable
         IF( MOD(jter,2) == 0 ) THEN ! even iterations
            !
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  !                 !--- tau_io/(v_oce - v_ice)
                  zTauO = zaV(ji,jj) * zrhoco * SQRT( ( v_ice (ji,jj) - v_oce (ji,jj) ) * ( v_ice (ji,jj) - v_oce (ji,jj) )  &
                     &                              + ( u_iceV(ji,jj) - u_oceV(ji,jj) ) * ( u_iceV(ji,jj) - u_oceV(ji,jj) ) )
                  !                 !--- Ocean-to-Ice stress
                  ztauy_oi(ji,jj) = zTauO * ( v_oce(ji,jj) - v_ice(ji,jj) )
                  !
                  !                 !--- tau_bottom/v_ice
                  zvel  = 5.e-05_wp + SQRT( v_ice(ji,jj) * v_ice(ji,jj) + u_iceV(ji,jj) * u_iceV(ji,jj) )
                  zTauB = ztauy_base(ji,jj) / zvel
                  !                 !--- OceanBottom-to-Ice stress
                  ztauy_bi(ji,jj) = zTauB * v_ice(ji,jj)
                  !
                  !                 !--- Coriolis at V-points (energy conserving formulation)
                  zCorV(ji,jj)  = - 0.25_wp * r1_e2v(ji,jj) *  &
                     &    ( zmf(ji,jj  ) * ( e2u(ji,jj  ) * u_ice(ji,jj  ) + e2u(ji-1,jj  ) * u_ice(ji-1,jj  ) )  &
                     &    + zmf(ji,jj+1) * ( e2u(ji,jj+1) * u_ice(ji,jj+1) + e2u(ji-1,jj+1) * u_ice(ji-1,jj+1) ) )
                  !
                  !                 !--- Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zRHS = zfV(ji,jj) + ztauy_ai(ji,jj) + zCorV(ji,jj) + zspgV(ji,jj) + ztauy_oi(ji,jj)
                  !
                  !                 !--- landfast switch => 0 = static  friction : TauB > RHS & sign(TauB) /= sign(RHS)
                  !                                         1 = sliding friction : TauB < RHS
                  rswitch = 1._wp - MIN( 1._wp, ABS( SIGN( 1._wp, zRHS + ztauy_base(ji,jj) ) - SIGN( 1._wp, zRHS ) ) )
                  !
                  IF( ln_aEVP ) THEN !--- ice velocity using aEVP (Kimmritz et al 2016 & 2017)
                     zbetav = MAX( zbeta(ji,jj), zbeta(ji,jj+1) )
                     v_ice(ji,jj) = ( (          rswitch   * ( zmV_t(ji,jj) * ( zbetav * v_ice(ji,jj) + v_ice_b(ji,jj) )         & ! previous velocity
                        &                                    + zRHS + zTauO * v_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmV_t(ji,jj) * ( zbetav + 1._wp ) + zTauO - zTauB ) & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) * (  v_ice_b(ji,jj)                                                   & 
                        &                                     + v_ice  (ji,jj) * MAX( 0._wp, zbetav - zdtevp * rn_lf_relax )     & ! static friction => slow decrease to v=0
                        &                                    ) / ( zbetav + 1._wp )                                              &
                        &             ) * zmsk01y(ji,jj) + v_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01y(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin
                        &           )   * zmsk00y(ji,jj)
                  ELSE               !--- ice velocity using EVP implicit formulation (cf Madec doc & Bouillon 2009)
                     v_ice(ji,jj) = ( (          rswitch   * ( zmV_t(ji,jj) * v_ice(ji,jj)                                       & ! previous velocity
                        &                                    + zRHS + zTauO * v_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmV_t(ji,jj) + zTauO - zTauB )                      & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) *   v_ice(ji,jj) * MAX( 0._wp, 1._wp - zdtevp * rn_lf_relax )         & ! static friction => slow decrease to v=0
                        &             ) * zmsk01y(ji,jj) + v_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01y(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin
                        &            )  * zmsk00y(ji,jj)
                  ENDIF
               END DO
            END DO
            CALL lbc_lnk( 'icedyn_rhg_evp', v_ice, 'V', -1. )
            !
            IF( ln_bdy )   CALL bdy_ice_dyn( 'V' )
            !
            DO jj = 2, jpjm1
               DO ji = 2, jpim1          
                  !                 !--- tau_io/(u_oce - u_ice)
                  zTauO = zaU(ji,jj) * zrhoco * SQRT( ( u_ice (ji,jj) - u_oce (ji,jj) ) * ( u_ice (ji,jj) - u_oce (ji,jj) )  &
                     &                              + ( v_iceU(ji,jj) - v_oceU(ji,jj) ) * ( v_iceU(ji,jj) - v_oceU(ji,jj) ) )
                  !                 !--- Ocean-to-Ice stress
                  ztaux_oi(ji,jj) = zTauO * ( u_oce(ji,jj) - u_ice(ji,jj) )
                  !
                  !                 !--- tau_bottom/u_ice
                  zvel  = 5.e-05_wp + SQRT( v_iceU(ji,jj) * v_iceU(ji,jj) + u_ice(ji,jj) * u_ice(ji,jj) )
                  zTauB = ztaux_base(ji,jj) / zvel
                  !                 !--- OceanBottom-to-Ice stress
                  ztaux_bi(ji,jj) = zTauB * u_ice(ji,jj)
                  !
                  !                 !--- Coriolis at U-points (energy conserving formulation)
                  zCorU(ji,jj)  =   0.25_wp * r1_e1u(ji,jj) *  &
                     &    ( zmf(ji  ,jj) * ( e1v(ji  ,jj) * v_ice(ji  ,jj) + e1v(ji  ,jj-1) * v_ice(ji  ,jj-1) )  &
                     &    + zmf(ji+1,jj) * ( e1v(ji+1,jj) * v_ice(ji+1,jj) + e1v(ji+1,jj-1) * v_ice(ji+1,jj-1) ) )
                  !
                  !                 !--- Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zRHS = zfU(ji,jj) + ztaux_ai(ji,jj) + zCorU(ji,jj) + zspgU(ji,jj) + ztaux_oi(ji,jj)
                  !
                  !                 !--- landfast switch => 0 = static  friction : TauB > RHS & sign(TauB) /= sign(RHS)
                  !                                         1 = sliding friction : TauB < RHS
                  rswitch = 1._wp - MIN( 1._wp, ABS( SIGN( 1._wp, zRHS + ztaux_base(ji,jj) ) - SIGN( 1._wp, zRHS ) ) )
                  !
                  IF( ln_aEVP ) THEN !--- ice velocity using aEVP (Kimmritz et al 2016 & 2017)
                     zbetau = MAX( zbeta(ji,jj), zbeta(ji+1,jj) )
                     u_ice(ji,jj) = ( (          rswitch   * ( zmU_t(ji,jj) * ( zbetau * u_ice(ji,jj) + u_ice_b(ji,jj) )         & ! previous velocity
                        &                                    + zRHS + zTauO * u_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmU_t(ji,jj) * ( zbetau + 1._wp ) + zTauO - zTauB ) & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) * (  u_ice_b(ji,jj)                                                   &
                        &                                     + u_ice  (ji,jj) * MAX( 0._wp, zbetau - zdtevp * rn_lf_relax )     & ! static friction => slow decrease to v=0
                        &                                    ) / ( zbetau + 1._wp )                                              &
                        &             ) * zmsk01x(ji,jj) + u_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01x(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin 
                        &           )   * zmsk00x(ji,jj)
                  ELSE               !--- ice velocity using EVP implicit formulation (cf Madec doc & Bouillon 2009)
                     u_ice(ji,jj) = ( (          rswitch   * ( zmU_t(ji,jj) * u_ice(ji,jj)                                       & ! previous velocity
                        &                                    + zRHS + zTauO * u_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmU_t(ji,jj) + zTauO - zTauB )                      & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) *   u_ice(ji,jj) * MAX( 0._wp, 1._wp - zdtevp * rn_lf_relax )         & ! static friction => slow decrease to v=0
                        &             ) * zmsk01x(ji,jj) + u_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01x(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin 
                        &           )   * zmsk00x(ji,jj)
                  ENDIF
               END DO
            END DO
            CALL lbc_lnk( 'icedyn_rhg_evp', u_ice, 'U', -1. )
            !
            IF( ln_bdy )   CALL bdy_ice_dyn( 'U' )
            !
         ELSE ! odd iterations
            !
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  !                 !--- tau_io/(u_oce - u_ice)
                  zTauO = zaU(ji,jj) * zrhoco * SQRT( ( u_ice (ji,jj) - u_oce (ji,jj) ) * ( u_ice (ji,jj) - u_oce (ji,jj) )  &
                     &                              + ( v_iceU(ji,jj) - v_oceU(ji,jj) ) * ( v_iceU(ji,jj) - v_oceU(ji,jj) ) )
                  !                 !--- Ocean-to-Ice stress
                  ztaux_oi(ji,jj) = zTauO * ( u_oce(ji,jj) - u_ice(ji,jj) )
                  !
                  !                 !--- tau_bottom/u_ice
                  zvel  = 5.e-05_wp + SQRT( v_iceU(ji,jj) * v_iceU(ji,jj) + u_ice(ji,jj) * u_ice(ji,jj) )
                  zTauB = ztaux_base(ji,jj) / zvel
                  !                 !--- OceanBottom-to-Ice stress
                  ztaux_bi(ji,jj) = zTauB * u_ice(ji,jj)
                  !
                  !                 !--- Coriolis at U-points (energy conserving formulation)
                  zCorU(ji,jj)  =   0.25_wp * r1_e1u(ji,jj) *  &
                     &    ( zmf(ji  ,jj) * ( e1v(ji  ,jj) * v_ice(ji  ,jj) + e1v(ji  ,jj-1) * v_ice(ji  ,jj-1) )  &
                     &    + zmf(ji+1,jj) * ( e1v(ji+1,jj) * v_ice(ji+1,jj) + e1v(ji+1,jj-1) * v_ice(ji+1,jj-1) ) )
                  !
                  !                 !--- Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zRHS = zfU(ji,jj) + ztaux_ai(ji,jj) + zCorU(ji,jj) + zspgU(ji,jj) + ztaux_oi(ji,jj)
                  !
                  !                 !--- landfast switch => 0 = static  friction : TauB > RHS & sign(TauB) /= sign(RHS)
                  !                                         1 = sliding friction : TauB < RHS
                  rswitch = 1._wp - MIN( 1._wp, ABS( SIGN( 1._wp, zRHS + ztaux_base(ji,jj) ) - SIGN( 1._wp, zRHS ) ) )
                  !
                  IF( ln_aEVP ) THEN !--- ice velocity using aEVP (Kimmritz et al 2016 & 2017)
                     zbetau = MAX( zbeta(ji,jj), zbeta(ji+1,jj) )
                     u_ice(ji,jj) = ( (          rswitch   * ( zmU_t(ji,jj) * ( zbetau * u_ice(ji,jj) + u_ice_b(ji,jj) )         & ! previous velocity
                        &                                    + zRHS + zTauO * u_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmU_t(ji,jj) * ( zbetau + 1._wp ) + zTauO - zTauB ) & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) * (  u_ice_b(ji,jj)                                                   &
                        &                                     + u_ice  (ji,jj) * MAX( 0._wp, zbetau - zdtevp * rn_lf_relax )     & ! static friction => slow decrease to v=0
                        &                                    ) / ( zbetau + 1._wp )                                              &
                        &             ) * zmsk01x(ji,jj) + u_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01x(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin 
                        &           )   * zmsk00x(ji,jj)
                  ELSE               !--- ice velocity using EVP implicit formulation (cf Madec doc & Bouillon 2009)
                     u_ice(ji,jj) = ( (          rswitch   * ( zmU_t(ji,jj) * u_ice(ji,jj)                                       & ! previous velocity
                        &                                    + zRHS + zTauO * u_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmU_t(ji,jj) + zTauO - zTauB )                      & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) *   u_ice(ji,jj) * MAX( 0._wp, 1._wp - zdtevp * rn_lf_relax )         & ! static friction => slow decrease to v=0
                        &             ) * zmsk01x(ji,jj) + u_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01x(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin
                        &           )   * zmsk00x(ji,jj)
                  ENDIF
               END DO
            END DO
            CALL lbc_lnk( 'icedyn_rhg_evp', u_ice, 'U', -1. )
            !
            IF( ln_bdy )   CALL bdy_ice_dyn( 'U' )
            !
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  !                 !--- tau_io/(v_oce - v_ice)
                  zTauO = zaV(ji,jj) * zrhoco * SQRT( ( v_ice (ji,jj) - v_oce (ji,jj) ) * ( v_ice (ji,jj) - v_oce (ji,jj) )  &
                     &                              + ( u_iceV(ji,jj) - u_oceV(ji,jj) ) * ( u_iceV(ji,jj) - u_oceV(ji,jj) ) )
                  !                 !--- Ocean-to-Ice stress
                  ztauy_oi(ji,jj) = zTauO * ( v_oce(ji,jj) - v_ice(ji,jj) )
                  !
                  !                 !--- tau_bottom/v_ice
                  zvel  = 5.e-05_wp + SQRT( v_ice(ji,jj) * v_ice(ji,jj) + u_iceV(ji,jj) * u_iceV(ji,jj) )
                  zTauB = ztauy_base(ji,jj) / zvel
                  !                 !--- OceanBottom-to-Ice stress
                  ztauy_bi(ji,jj) = zTauB * v_ice(ji,jj)
                  !
                  !                 !--- Coriolis at v-points (energy conserving formulation)
                  zCorV(ji,jj)  = - 0.25_wp * r1_e2v(ji,jj) *  &
                     &    ( zmf(ji,jj  ) * ( e2u(ji,jj  ) * u_ice(ji,jj  ) + e2u(ji-1,jj  ) * u_ice(ji-1,jj  ) )  &
                     &    + zmf(ji,jj+1) * ( e2u(ji,jj+1) * u_ice(ji,jj+1) + e2u(ji-1,jj+1) * u_ice(ji-1,jj+1) ) )
                  !
                  !                 !--- Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zRHS = zfV(ji,jj) + ztauy_ai(ji,jj) + zCorV(ji,jj) + zspgV(ji,jj) + ztauy_oi(ji,jj)
                  !
                  !                 !--- landfast switch => 0 = static  friction : TauB > RHS & sign(TauB) /= sign(RHS)
                  !                                         1 = sliding friction : TauB < RHS
                  rswitch = 1._wp - MIN( 1._wp, ABS( SIGN( 1._wp, zRHS + ztauy_base(ji,jj) ) - SIGN( 1._wp, zRHS ) ) )
                  !
                  IF( ln_aEVP ) THEN !--- ice velocity using aEVP (Kimmritz et al 2016 & 2017)
                     zbetav = MAX( zbeta(ji,jj), zbeta(ji,jj+1) )
                     v_ice(ji,jj) = ( (          rswitch   * ( zmV_t(ji,jj) * ( zbetav * v_ice(ji,jj) + v_ice_b(ji,jj) )         & ! previous velocity
                        &                                    + zRHS + zTauO * v_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmV_t(ji,jj) * ( zbetav + 1._wp ) + zTauO - zTauB ) & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) * (  v_ice_b(ji,jj)                                                   &
                        &                                     + v_ice  (ji,jj) * MAX( 0._wp, zbetav - zdtevp * rn_lf_relax )     & ! static friction => slow decrease to v=0
                        &                                    ) / ( zbetav + 1._wp )                                              & 
                        &             ) * zmsk01y(ji,jj) + v_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01y(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin
                        &           )   * zmsk00y(ji,jj)
                  ELSE               !--- ice velocity using EVP implicit formulation (cf Madec doc & Bouillon 2009)
                     v_ice(ji,jj) = ( (          rswitch   * ( zmV_t(ji,jj) * v_ice(ji,jj)                                       & ! previous velocity
                        &                                    + zRHS + zTauO * v_ice(ji,jj)                                       & ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                        &                                    ) / MAX( zepsi, zmV_t(ji,jj) + zTauO - zTauB )                      & ! m/dt + tau_io(only ice part) + landfast
                        &            + ( 1._wp - rswitch ) *   v_ice(ji,jj) * MAX( 0._wp, 1._wp - zdtevp * rn_lf_relax )         & ! static friction => slow decrease to v=0
                        &             ) * zmsk01y(ji,jj) + v_oce(ji,jj) * 0.01_wp * ( 1._wp - zmsk01y(ji,jj) )                   & ! v_ice = v_oce/100 if mass < zmmin & conc < zamin
                        &           )   * zmsk00y(ji,jj)
                  ENDIF
               END DO
            END DO
            CALL lbc_lnk( 'icedyn_rhg_evp', v_ice, 'V', -1. )
            !
            IF( ln_bdy )   CALL bdy_ice_dyn( 'V' )
            !
         ENDIF

         ! convergence test
         IF( nn_rhg_chkcvg == 2 )   CALL rhg_cvg( kt, jter, nn_nevp, u_ice, v_ice, zu_ice, zv_ice )
         !
         !                                                ! ==================== !
      END DO                                              !  end loop over jter  !
      !                                                   ! ==================== !
      IF( ln_aEVP )   CALL iom_put( 'beta_evp' , zbeta )
      !
      !------------------------------------------------------------------------------!
      ! 4) Recompute delta, shear and div (inputs for mechanical redistribution) 
      !------------------------------------------------------------------------------!
      DO jj = 1, jpjm1
         DO ji = 1, jpim1

            ! shear at F points
            zds(ji,jj) = ( ( u_ice(ji,jj+1) * r1_e1u(ji,jj+1) - u_ice(ji,jj) * r1_e1u(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj)   &
               &         + ( v_ice(ji+1,jj) * r1_e2v(ji+1,jj) - v_ice(ji,jj) * r1_e2v(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj)   &
               &         ) * r1_e1e2f(ji,jj) * zfmask(ji,jj)

         END DO
      END DO           
      
      DO jj = 2, jpjm1
         DO ji = 2, jpim1 ! no vector loop
            
            ! tension**2 at T points
            zdt  = ( ( u_ice(ji,jj) * r1_e2u(ji,jj) - u_ice(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
               &   - ( v_ice(ji,jj) * r1_e1v(ji,jj) - v_ice(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
               &   ) * r1_e1e2t(ji,jj)
            zdt2 = zdt * zdt
            
            zten_i(ji,jj) = zdt

            ! shear**2 at T points (doc eq. A16)
            zds2 = ( zds(ji,jj  ) * zds(ji,jj  ) * e1e2f(ji,jj  ) + zds(ji-1,jj  ) * zds(ji-1,jj  ) * e1e2f(ji-1,jj  )  &
               &   + zds(ji,jj-1) * zds(ji,jj-1) * e1e2f(ji,jj-1) + zds(ji-1,jj-1) * zds(ji-1,jj-1) * e1e2f(ji-1,jj-1)  &
               &   ) * 0.25_wp * r1_e1e2t(ji,jj)
            
            ! shear at T points
            pshear_i(ji,jj) = SQRT( zdt2 + zds2 )

            ! divergence at T points
            pdivu_i(ji,jj) = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
               &             + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1)   &
               &             ) * r1_e1e2t(ji,jj)
            
            ! delta at T points
            zfac            = SQRT( pdivu_i(ji,jj) * pdivu_i(ji,jj) + ( zdt2 + zds2 ) * z1_ecc2 ) ! delta  
            rswitch         = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zfac ) ) ! 0 if delta=0
            pdelta_i(ji,jj) = zfac + rn_creepl * rswitch ! delta+creepl

         END DO
      END DO
      CALL lbc_lnk_multi( 'icedyn_rhg_evp', pshear_i, 'T', 1., pdivu_i, 'T', 1., pdelta_i, 'T', 1., zten_i, 'T', 1., &
         &                                  zs1     , 'T', 1., zs2    , 'T', 1., zs12    , 'F', 1. )
      
      ! --- Store the stress tensor for the next time step --- !
      pstress1_i (:,:) = zs1 (:,:)
      pstress2_i (:,:) = zs2 (:,:)
      pstress12_i(:,:) = zs12(:,:)
      !

      !------------------------------------------------------------------------------!
      ! 5) diagnostics
      !------------------------------------------------------------------------------!
      ! --- ice-ocean, ice-atm. & ice-oceanbottom(landfast) stresses --- !
      IF(  iom_use('utau_oi') .OR. iom_use('vtau_oi') .OR. iom_use('utau_ai') .OR. iom_use('vtau_ai') .OR. &
         & iom_use('utau_bi') .OR. iom_use('vtau_bi') ) THEN
         !
         CALL lbc_lnk_multi( 'icedyn_rhg_evp', ztaux_oi, 'U', -1., ztauy_oi, 'V', -1., ztaux_ai, 'U', -1., ztauy_ai, 'V', -1., &
            &                                  ztaux_bi, 'U', -1., ztauy_bi, 'V', -1. )
         !
         CALL iom_put( 'utau_oi' , ztaux_oi * zmsk00 )
         CALL iom_put( 'vtau_oi' , ztauy_oi * zmsk00 )
         CALL iom_put( 'utau_ai' , ztaux_ai * zmsk00 )
         CALL iom_put( 'vtau_ai' , ztauy_ai * zmsk00 )
         CALL iom_put( 'utau_bi' , ztaux_bi * zmsk00 )
         CALL iom_put( 'vtau_bi' , ztauy_bi * zmsk00 )
      ENDIF
       
      ! --- divergence, shear and strength --- !
      IF( iom_use('icediv') )   CALL iom_put( 'icediv' , pdivu_i  * zmsk00 )   ! divergence
      IF( iom_use('iceshe') )   CALL iom_put( 'iceshe' , pshear_i * zmsk00 )   ! shear
      IF( iom_use('icestr') )   CALL iom_put( 'icestr' , strength * zmsk00 )   ! strength

      ! --- Stress tensor invariants (SIMIP diags) --- !
      IF( iom_use('normstr') .OR. iom_use('sheastr') ) THEN
         !
         ALLOCATE( zsig_I(jpi,jpj) , zsig_II(jpi,jpj) )
         !         
         DO jj = 1, jpj
            DO ji = 1, jpi
            
               ! Ice stresses
               ! sigma1, sigma2, sigma12 are some useful recombination of the stresses (Hunke and Dukowicz MWR 2002, Bouillon et al., OM2013)
               ! These are NOT stress tensor components, neither stress invariants, neither stress principal components
               ! I know, this can be confusing...
               zfac             =   strength(ji,jj) / ( pdelta_i(ji,jj) + rn_creepl ) 
               zsig1            =   zfac * ( pdivu_i(ji,jj) - pdelta_i(ji,jj) )
               zsig2            =   zfac * z1_ecc2 * zten_i(ji,jj)
               zsig12           =   zfac * z1_ecc2 * pshear_i(ji,jj)
               
               ! Stress invariants (sigma_I, sigma_II, Coon 1974, Feltham 2008)
               zsig_I (ji,jj)   =   zsig1 * 0.5_wp                                           ! 1st stress invariant, aka average normal stress, aka negative pressure
               zsig_II(ji,jj)   =   SQRT ( MAX( 0._wp, zsig2 * zsig2 * 0.25_wp + zsig12 ) )  ! 2nd  ''       '', aka maximum shear stress
               
            END DO
         END DO         
         !
         ! Stress tensor invariants (normal and shear stress N/m) - SIMIP diags - definitions following Coon (1974) and Feltham (2008)
         IF( iom_use('normstr') )   CALL iom_put( 'normstr', zsig_I (:,:) * zmsk00(:,:) ) ! Normal stress
         IF( iom_use('sheastr') )   CALL iom_put( 'sheastr', zsig_II(:,:) * zmsk00(:,:) ) ! Maximum shear stress
         
         DEALLOCATE ( zsig_I, zsig_II )
         
      ENDIF

      ! --- Normalized stress tensor principal components --- !
      ! This are used to plot the normalized yield curve, see Lemieux & Dupont, 2020
      ! Recommendation 1 : we use ice strength, not replacement pressure
      ! Recommendation 2 : need to use deformations at PREVIOUS iterate for viscosities
!!$      IF( iom_use('sig1_pnorm') .OR. iom_use('sig2_pnorm') ) THEN
!!$         !
!!$         ALLOCATE( zsig1_p(jpi,jpj) , zsig2_p(jpi,jpj) , zsig_I(jpi,jpj) , zsig_II(jpi,jpj) )         
!!$         !         
!!$         DO jj = 1, jpj
!!$            DO ji = 1, jpi
!!$            
!!$               ! Ice stresses computed with **viscosities** (delta, p/delta) at **previous** iterates 
!!$               !                        and **deformations** at current iterates
!!$               !                        following Lemieux & Dupont (2020)
!!$               zfac             =   zp_delt(ji,jj)
!!$               zsig1            =   zfac * ( pdivu_i(ji,jj) - ( zdelta(ji,jj) + rn_creepl ) )
!!$               zsig2            =   zfac * z1_ecc2 * zten_i(ji,jj)
!!$               zsig12           =   zfac * z1_ecc2 * pshear_i(ji,jj)
!!$               
!!$               ! Stress invariants (sigma_I, sigma_II, Coon 1974, Feltham 2008), T-point
!!$               zsig_I(ji,jj)    =   zsig1 * 0.5_wp                                           ! 1st stress invariant, aka average normal stress, aka negative pressure
!!$               zsig_II(ji,jj)   =   SQRT ( MAX( 0._wp, zsig2 * zsig2 * 0.25_wp + zsig12 ) )  ! 2nd  ''       '', aka maximum shear stress
!!$      
!!$               ! Normalized  principal stresses (used to display the ellipse)
!!$               z1_strength      =   1._wp / MAX( 1._wp, strength(ji,jj) )
!!$               zsig1_p(ji,jj)   =   ( zsig_I(ji,jj) + zsig_II(ji,jj) ) * z1_strength
!!$               zsig2_p(ji,jj)   =   ( zsig_I(ji,jj) - zsig_II(ji,jj) ) * z1_strength
!!$            END DO
!!$         END DO               
!!$         !
!!$         CALL iom_put( 'sig1_pnorm' , zsig1_p ) 
!!$         CALL iom_put( 'sig2_pnorm' , zsig2_p ) 
!!$      
!!$         DEALLOCATE( zsig1_p , zsig2_p , zsig_I, zsig_II )
!!$         
!!$      ENDIF

      ! --- SIMIP --- !
      IF(  iom_use('dssh_dx') .OR. iom_use('dssh_dy') .OR. &
         & iom_use('corstrx') .OR. iom_use('corstry') .OR. iom_use('intstrx') .OR. iom_use('intstry') ) THEN
         !
         CALL lbc_lnk_multi( 'icedyn_rhg_evp', zspgU, 'U', -1., zspgV, 'V', -1., &
            &                                  zCorU, 'U', -1., zCorV, 'V', -1., zfU, 'U', -1., zfV, 'V', -1. )

         CALL iom_put( 'dssh_dx' , zspgU * zmsk00 )   ! Sea-surface tilt term in force balance (x)
         CALL iom_put( 'dssh_dy' , zspgV * zmsk00 )   ! Sea-surface tilt term in force balance (y)
         CALL iom_put( 'corstrx' , zCorU * zmsk00 )   ! Coriolis force term in force balance (x)
         CALL iom_put( 'corstry' , zCorV * zmsk00 )   ! Coriolis force term in force balance (y)
         CALL iom_put( 'intstrx' , zfU   * zmsk00 )   ! Internal force term in force balance (x)
         CALL iom_put( 'intstry' , zfV   * zmsk00 )   ! Internal force term in force balance (y)
      ENDIF

      IF(  iom_use('xmtrpice') .OR. iom_use('ymtrpice') .OR. &
         & iom_use('xmtrpsnw') .OR. iom_use('ymtrpsnw') .OR. iom_use('xatrp') .OR. iom_use('yatrp') ) THEN
         !
         ALLOCATE( zdiag_xmtrp_ice(jpi,jpj) , zdiag_ymtrp_ice(jpi,jpj) , &
            &      zdiag_xmtrp_snw(jpi,jpj) , zdiag_ymtrp_snw(jpi,jpj) , zdiag_xatrp(jpi,jpj) , zdiag_yatrp(jpi,jpj) )
         !
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               ! 2D ice mass, snow mass, area transport arrays (X, Y)
               zfac_x = 0.5 * u_ice(ji,jj) * e2u(ji,jj) * zmsk00(ji,jj)
               zfac_y = 0.5 * v_ice(ji,jj) * e1v(ji,jj) * zmsk00(ji,jj)

               zdiag_xmtrp_ice(ji,jj) = rhoi * zfac_x * ( vt_i(ji+1,jj) + vt_i(ji,jj) ) ! ice mass transport, X-component
               zdiag_ymtrp_ice(ji,jj) = rhoi * zfac_y * ( vt_i(ji,jj+1) + vt_i(ji,jj) ) !        ''           Y-   ''

               zdiag_xmtrp_snw(ji,jj) = rhos * zfac_x * ( vt_s(ji+1,jj) + vt_s(ji,jj) ) ! snow mass transport, X-component
               zdiag_ymtrp_snw(ji,jj) = rhos * zfac_y * ( vt_s(ji,jj+1) + vt_s(ji,jj) ) !          ''          Y-   ''

               zdiag_xatrp(ji,jj)     = zfac_x * ( at_i(ji+1,jj) + at_i(ji,jj) )        ! area transport,      X-component
               zdiag_yatrp(ji,jj)     = zfac_y * ( at_i(ji,jj+1) + at_i(ji,jj) )        !        ''            Y-   ''

            END DO
         END DO

         CALL lbc_lnk_multi( 'icedyn_rhg_evp', zdiag_xmtrp_ice, 'U', -1., zdiag_ymtrp_ice, 'V', -1., &
            &                                  zdiag_xmtrp_snw, 'U', -1., zdiag_ymtrp_snw, 'V', -1., &
            &                                  zdiag_xatrp    , 'U', -1., zdiag_yatrp    , 'V', -1. )

         CALL iom_put( 'xmtrpice' , zdiag_xmtrp_ice )   ! X-component of sea-ice mass transport (kg/s)
         CALL iom_put( 'ymtrpice' , zdiag_ymtrp_ice )   ! Y-component of sea-ice mass transport 
         CALL iom_put( 'xmtrpsnw' , zdiag_xmtrp_snw )   ! X-component of snow mass transport (kg/s)
         CALL iom_put( 'ymtrpsnw' , zdiag_ymtrp_snw )   ! Y-component of snow mass transport
         CALL iom_put( 'xatrp'    , zdiag_xatrp     )   ! X-component of ice area transport
         CALL iom_put( 'yatrp'    , zdiag_yatrp     )   ! Y-component of ice area transport

         DEALLOCATE( zdiag_xmtrp_ice , zdiag_ymtrp_ice , &
            &        zdiag_xmtrp_snw , zdiag_ymtrp_snw , zdiag_xatrp , zdiag_yatrp )

      ENDIF
      !
      ! --- convergence tests --- !
      IF( nn_rhg_chkcvg == 1 .OR. nn_rhg_chkcvg == 2 ) THEN
         IF( iom_use('uice_cvg') ) THEN
            IF( ln_aEVP ) THEN   ! output: beta * ( u(t=nn_nevp) - u(t=nn_nevp-1) )
               CALL iom_put( 'uice_cvg', MAX( ABS( u_ice(:,:) - zu_ice(:,:) ) * zbeta(:,:) * umask(:,:,1) , &
                  &                           ABS( v_ice(:,:) - zv_ice(:,:) ) * zbeta(:,:) * vmask(:,:,1) ) * zmsk15(:,:) )
            ELSE                 ! output: nn_nevp * ( u(t=nn_nevp) - u(t=nn_nevp-1) )
               CALL iom_put( 'uice_cvg', REAL( nn_nevp ) * MAX( ABS( u_ice(:,:) - zu_ice(:,:) ) * umask(:,:,1) , &
                  &                                             ABS( v_ice(:,:) - zv_ice(:,:) ) * vmask(:,:,1) ) * zmsk15(:,:) )
            ENDIF
         ENDIF
      ENDIF      
      !
      DEALLOCATE( zmsk00, zmsk15 )
      !
   END SUBROUTINE ice_dyn_rhg_evp


   SUBROUTINE rhg_cvg( kt, kiter, kitermax, pu, pv, pub, pvb )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE rhg_cvg  ***
      !!                     
      !! ** Purpose :   check convergence of oce rheology
      !!
      !! ** Method  :   create a file ice_cvg.nc containing the convergence of ice velocity
      !!                during the sub timestepping of rheology so as:
      !!                  uice_cvg = MAX( u(t+1) - u(t) , v(t+1) - v(t) )
      !!                This routine is called every sub-iteration, so it is cpu expensive
      !!
      !! ** Note    :   for the first sub-iteration, uice_cvg is set to 0 (too large otherwise)   
      !!----------------------------------------------------------------------
      INTEGER ,                 INTENT(in) ::   kt, kiter, kitermax       ! ocean time-step index
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pu, pv, pub, pvb          ! now and before velocities
      !!
      INTEGER           ::   it, idtime, istatus
      INTEGER           ::   ji, jj          ! dummy loop indices
      REAL(wp)          ::   zresm           ! local real 
      CHARACTER(len=20) ::   clname
      REAL(wp), DIMENSION(jpi,jpj) ::   zres           ! check convergence
      !!----------------------------------------------------------------------

      ! create file
      IF( kt == nit000 .AND. kiter == 1 ) THEN
         !
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'rhg_cvg : ice rheology convergence control'
            WRITE(numout,*) '~~~~~~~'
         ENDIF
         !
         IF( lwm ) THEN
            clname = 'ice_cvg.nc'
            IF( .NOT. Agrif_Root() )   clname = TRIM(Agrif_CFixed())//"_"//TRIM(clname)
            istatus = NF90_CREATE( TRIM(clname), NF90_CLOBBER, ncvgid )
            istatus = NF90_DEF_DIM( ncvgid, 'time'  , NF90_UNLIMITED, idtime )
            istatus = NF90_DEF_VAR( ncvgid, 'uice_cvg', NF90_DOUBLE   , (/ idtime /), nvarid )
            istatus = NF90_ENDDEF(ncvgid)
         ENDIF
         !
      ENDIF

      ! time
      it = ( kt - 1 ) * kitermax + kiter
      
      ! convergence
      IF( kiter == 1 ) THEN ! remove the first iteration for calculations of convergence (always very large)
         zresm = 0._wp
      ELSE
         DO jj = 1, jpj
            DO ji = 1, jpi
               zres(ji,jj) = MAX( ABS( pu(ji,jj) - pub(ji,jj) ) * umask(ji,jj,1), &
                  &               ABS( pv(ji,jj) - pvb(ji,jj) ) * vmask(ji,jj,1) ) * zmsk15(ji,jj)
            END DO
         END DO
         zresm = MAXVAL( zres )
         CALL mpp_max( 'icedyn_rhg_evp', zresm )   ! max over the global domain
      ENDIF

      IF( lwm ) THEN
         ! write variables
         istatus = NF90_PUT_VAR( ncvgid, nvarid, (/zresm/), (/it/), (/1/) )
         ! close file
         IF( kt == nitend - nn_fsbc + 1 )   istatus = NF90_CLOSE(ncvgid)
      ENDIF
      
   END SUBROUTINE rhg_cvg


   SUBROUTINE rhg_evp_rst( cdrw, kt )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE rhg_evp_rst  ***
      !!                     
      !! ** Purpose :   Read or write RHG file in restart file
      !!
      !! ** Method  :   use of IOM library
      !!----------------------------------------------------------------------
      CHARACTER(len=*) , INTENT(in) ::   cdrw   ! "READ"/"WRITE" flag
      INTEGER, OPTIONAL, INTENT(in) ::   kt     ! ice time-step
      !
      INTEGER  ::   iter            ! local integer
      INTEGER  ::   id1, id2, id3   ! local integers
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN        ! Read/initialize
         !                                   ! ---------------
         IF( ln_rstart ) THEN                   !* Read the restart file
            !
            id1 = iom_varid( numrir, 'stress1_i' , ldstop = .FALSE. )
            id2 = iom_varid( numrir, 'stress2_i' , ldstop = .FALSE. )
            id3 = iom_varid( numrir, 'stress12_i', ldstop = .FALSE. )
            !
            IF( MIN( id1, id2, id3 ) > 0 ) THEN      ! fields exist
               CALL iom_get( numrir, jpdom_autoglo, 'stress1_i' , stress1_i  )
               CALL iom_get( numrir, jpdom_autoglo, 'stress2_i' , stress2_i  )
               CALL iom_get( numrir, jpdom_autoglo, 'stress12_i', stress12_i )
            ELSE                                     ! start rheology from rest
               IF(lwp) WRITE(numout,*)
               IF(lwp) WRITE(numout,*) '   ==>>>   previous run without rheology, set stresses to 0'
               stress1_i (:,:) = 0._wp
               stress2_i (:,:) = 0._wp
               stress12_i(:,:) = 0._wp
            ENDIF
         ELSE                                   !* Start from rest
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '   ==>>>   start from rest: set stresses to 0'
            stress1_i (:,:) = 0._wp
            stress2_i (:,:) = 0._wp
            stress12_i(:,:) = 0._wp
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
         !                                   ! -------------------
         IF(lwp) WRITE(numout,*) '---- rhg-rst ----'
         iter = kt + nn_fsbc - 1             ! ice restarts are written at kt == nitrst - nn_fsbc + 1
         !
         CALL iom_rstput( iter, nitrst, numriw, 'stress1_i' , stress1_i  )
         CALL iom_rstput( iter, nitrst, numriw, 'stress2_i' , stress2_i  )
         CALL iom_rstput( iter, nitrst, numriw, 'stress12_i', stress12_i )
         !
      ENDIF
      !
   END SUBROUTINE rhg_evp_rst

   

   !!==============================================================================
END MODULE icedyn_rhg_evp
