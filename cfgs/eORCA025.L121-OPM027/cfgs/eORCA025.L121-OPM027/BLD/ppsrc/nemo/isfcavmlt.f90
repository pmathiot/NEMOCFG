










MODULE isfcavmlt
   !!======================================================================
   !!                       ***  MODULE  isfcavmlt  ***
   !! ice shelf module :  update surface ocean boundary condition under ice
   !!                   shelves
   !!======================================================================
   !! History :  4.0  !  2019-09  (P. Mathiot) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfcav_mlt    : compute or read ice shelf fwf/heat fluxes in the ice shelf cavity
   !!----------------------------------------------------------------------

   USE isf_oce                  ! ice shelf
   USE isftbl , ONLY: isf_tbl   ! ice shelf depth average
   USE isfutils,ONLY: debug     ! debug subroutine

   USE dom_oce                            ! ocean space and time domain
   USE oce    , ONLY: tsn                 ! ocean dynamics and tracers
   USE phycst , ONLY: rcp, rau0, rau0_rcp ! physical constants
   USE eosbn2 , ONLY: eos_fzp             ! equation of state

   USE in_out_manager              ! I/O manager
   USE iom        , ONLY: iom_put  ! I/O library
   USE fldread    , ONLY: fld_read, FLD, FLD_N !
   USE lib_fortran, ONLY: glob_sum !
   USE lib_mpp    , ONLY: ctl_stop !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   isfcav_mlt

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

! -------------------------------------------------------------------------------------------------------
! -------------------------------- PUBLIC SUBROUTINE ----------------------------------------------------
! -------------------------------------------------------------------------------------------------------

   SUBROUTINE isfcav_mlt(kt, pgt, pgs , pttbl, pstbl, &
      &                           pqhc, pqoce, pqfwf  )
      !!----------------------------------------------------------------------
      !!
      !!                          ***  ROUTINE isfcav_mlt  ***
      !!
      !! ** Purpose    : compute or read ice shelf fwf/heat fluxes in the ice shelf cavity
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pqhc, pqoce, pqfwf  ! heat and fwf fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER, INTENT(in) :: kt
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pgt  , pgs    ! gamma t and gamma s
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pttbl, pstbl  ! top boundary layer tracer
      !!---------------------------------------------------------------------
      !
      ! compute latent heat and melt (2d)
      SELECT CASE ( cn_isfcav_mlt )
      CASE ( 'spe' )   ! ice shelf melt specified (read input file, and heat fluxes derived from
         CALL isfcav_mlt_spe( kt, pstbl,               &
            &                  pqhc, pqoce, pqfwf  )
      CASE ( '2eq' )   !  ISOMIP  formulation (2 equations) for volume flux (Hunter et al., 2006)
         CALL isfcav_mlt_2eq( pgt, pttbl, pstbl,       &
            &                  pqhc , pqoce, pqfwf )
      CASE ( '3eq' )   ! ISOMIP+ formulation (3 equations) for volume flux (Asay-Davis et al., 2015)
         CALL isfcav_mlt_3eq( pgt, pgs , pttbl, pstbl, &
            &                  pqhc, pqoce, pqfwf  )
      CASE ( 'oasis' ) ! fwf pass trough oasis
         CALL isfcav_mlt_oasis( kt, pstbl,              &
            &                   pqhc, pqoce, pqfwf  )
      CASE DEFAULT
         CALL ctl_stop('STOP', 'unknown isf melt formulation : cn_isfcav (should not see this)')
      END SELECT
      !
      IF (ln_isfdebug) THEN
         CALL debug( 'isfcav_mlt:', pqhc (:,:) )
         CALL debug( 'isfcav_mlt:', pqoce(:,:) )
         CALL debug( 'isfcav_mlt:', pqfwf(:,:) )
      END IF
      !
   END SUBROUTINE isfcav_mlt

! -------------------------------------------------------------------------------------------------------
! -------------------------------- PRIVATE SUBROUTINE ---------------------------------------------------
! -------------------------------------------------------------------------------------------------------

   SUBROUTINE isfcav_mlt_spe(kt, pstbl,          &  ! <<== in
      &                      pqhc , pqoce, pqfwf )  ! ==>> out
      !!----------------------------------------------------------------------
      !!
      !!                          ***  ROUTINE isfcav_mlt_spe  ***
      !!
      !! ** Purpose    : - read ice shelf melt from forcing file
      !!                 - compute ocea-ice heat flux (assuming it is equal to latent heat)
      !!                 - compute heat content flux
      !!---------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pqhc, pqoce, pqfwf  ! heat content, latent heat and fwf fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER                     , INTENT(in   ) :: kt                  ! current time step
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pstbl               ! salinity in tbl
      !!--------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: ztfrz                              ! tbl freezing temperature
      !!--------------------------------------------------------------------
      !
      ! Compute freezing temperature
      CALL eos_fzp( pstbl(:,:), ztfrz(:,:), risfdep(:,:) )
      !
      ! read input file
      CALL fld_read ( kt, 1, sf_isfcav_fwf )
      !
      ! define fwf and qoce
      ! ocean heat flux is assume to be equal to the latent heat
      pqfwf(:,:) = - sf_isfcav_fwf(1)%fnow(:,:,1)      ! fwf                ( >0 out)
      pqoce(:,:) = - pqfwf(:,:) * rLfusisf             ! ocean heat flux    ( >0 out)
      pqhc (:,:) =   pqfwf(:,:) * ztfrz(:,:) * rcp     ! heat content flux  ( >0 out)
      !
      ! output freezing point at the interface
      CALL iom_put('isftfrz_cav', ztfrz )
      !
   END SUBROUTINE isfcav_mlt_spe

   SUBROUTINE isfcav_mlt_2eq(pgt , pttbl, pstbl, &  ! <<== in
      &                      pqhc, pqoce, pqfwf  )  ! ==>> out
      !!----------------------------------------------------------------------
      !!
      !!                          ***  ROUTINE isfcav_mlt_2eq  ***
      !!
      !! ** Purpose    : Compute ice shelf fwf/heqt fluxes using ISOMIP formulation (Hunter et al., 2006)
      !!
      !! ** Method     : The ice shelf melt latent heat is defined as being equal to the ocean/ice heat flux.
      !!                 From this we can derived the fwf, ocean/ice heat flux and the heat content flux as being :
      !!                   qfwf  = Gammat * Rau0 * Cp * ( Tw - Tfrz ) / Lf 
      !!                   qhoce = qlat
      !!                   qhc   = qfwf * Cp * Tfrz
      !!
      !! ** Reference  : Hunter,  J.  R.:  Specification  for  test  models  of  ice  shelf  cavities,  
      !!                 Tech.  Rep.  June,  Antarctic  Climate  &  Ecosystems  Cooperative  Research  Centre,  available  at:  
      !!                 http://staff.acecrc.org.au/~bkgalton/ISOMIP/test_cavities.pdf (last access: 21 July 2016), 2006.
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pqhc, pqoce, pqfwf  ! hean content, ocean-ice heat and fwf fluxes
      !!-------------------------- IN  -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pgt           ! temperature exchange coeficient
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pttbl, pstbl  ! temperature and salinity in top boundary layer
      !!--------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: ztfrz         ! freezing temperature
      REAL(wp), DIMENSION(jpi,jpj) :: zthd          ! thermal driving
      !!--------------------------------------------------------------------
      !
      ! Calculate freezing temperature
      CALL eos_fzp( pstbl(:,:), ztfrz(:,:), risfdep(:,:) )
      !
      ! thermal driving
      zthd (:,:) = ( pttbl(:,:) - ztfrz(:,:) ) * mskisf_cav(:,:)
      !
      ! compute ocean-ice heat flux and then derive fwf assuming that ocean heat flux equal latent heat
      pqfwf(:,:) = - pgt(:,:) * rau0_rcp * zthd(:,:) / rLfusisf    ! fresh water flux  ( > 0 out )
      pqoce(:,:) = - pqfwf(:,:) * rLfusisf                         ! ocea-ice flux     ( > 0 out )
      pqhc (:,:) =   pqfwf(:,:) * ztfrz(:,:) * rcp                 ! heat content flux ( > 0 out )
      !
      ! output thermal driving and freezinpoint at the ice shelf interface
      CALL iom_put('isfthermald_cav', zthd )
      CALL iom_put('isftfrz_cav'    , ztfrz )
      !
   END SUBROUTINE isfcav_mlt_2eq

   SUBROUTINE isfcav_mlt_3eq(pgt, pgs , pttbl, pstbl, &  ! <<== in
      &                           pqhc, pqoce, pqfwf  )  ! ==>> out
      !!----------------------------------------------------------------------
      !!
      !!                          ***  ROUTINE isfcav_mlt_3eq  ***
      !!
      !! ** Purpose    : Compute ice shelf fwf/heqt fluxes using the 3 equation formulation 
      !!
      !! ** Method     : The melt rate is determined considering the heat balance, the salt balance
      !!                 at the phase change interface and a linearisation of the equation of state.
      !!
      !! ** Reference  : - Holland, D. M. and Jenkins, A.,
      !!                   Modeling Thermodynamic Ice-Ocean Interactions at the Base of an Ice Shelf,
      !!                   J. Phys. Oceanogr., 29, 1999.
      !!                 - Asay-Davis, X. S., Cornford, S. L., Durand, G., Galton-Fenzi, B. K., Gladstone, 
      !!                   R. M., Gudmundsson, G. H., Hattermann, T., Holland, D. M., Holland, D., Holland, 
      !!                   P. R., Martin, D. F., Mathiot, P., Pattyn, F., and Seroussi, H.:
      !!                   Experimental design for three interrelated marine ice sheet and ocean model intercomparison projects: 
      !!                   MISMIP v. 3 (MISMIP +), ISOMIP v. 2 (ISOMIP +) and MISOMIP v. 1 (MISOMIP1), 
      !!                   Geosci. Model Dev., 9, 2471-2497, https://doi.org/10.5194/gmd-9-2471-2016, 2016. 
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pqhc, pqoce, pqfwf  ! latent heat and fwf fluxes
      !!-------------------------- IN  -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pgt  , pgs          ! heat/salt exchange coeficient
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pttbl, pstbl        ! mean temperature and salinity in top boundary layer
      !!--------------------------------------------------------------------
      REAL(wp) :: zeps1,zeps2,zeps3,zeps4,zeps6,zeps7       ! dummy local scalar for quadratic equation resolution
      REAL(wp) :: zaqe,zbqe,zcqe,zaqer,zdis,zsfrz,zcfac     ! dummy local scalar for quadratic equation resolution
      REAL(wp) :: zeps = 1.e-20
      REAL(wp), DIMENSION(jpi,jpj) :: ztfrz         ! freezing point
      REAL(wp), DIMENSION(jpi,jpj) :: zqcon         ! conductive flux through the ice shelf
      REAL(wp), DIMENSION(jpi,jpj) :: zthd          ! thermal driving
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      !!--------------------------------------------------------------------
      !
      ! compute upward heat flux zhtflx and upward water flux zwflx
      ! Resolution of a 3d equation from equation 24, 25 and 26 (note conduction through the ice has been added to Eq 24)
      DO jj = 1, jpj
         DO ji = 1, jpi
            !
            ! compute coeficient to solve the 2nd order equation
            zeps1 = rau0_rcp * pgt(ji,jj)
            zeps2 = rLfusisf * rau0 * pgs(ji,jj)
            zeps3 = rhoisf * rcpisf * rkappa / MAX(risfdep(ji,jj),zeps)
            zeps4 = risf_lamb2 + risf_lamb3 * risfdep(ji,jj)
            zeps6 = zeps4 - pttbl(ji,jj)
            zeps7 = zeps4 - rtsurf
            !
            ! solve the 2nd order equation to find zsfrz
            zaqe  = risf_lamb1 * (zeps1 + zeps3)
            zaqer = 0.5_wp / MIN(zaqe,-zeps)
            zbqe  = zeps1 * zeps6 + zeps3 * zeps7 - zeps2
            zcqe  = zeps2 * pstbl(ji,jj)
            zdis  = zbqe * zbqe - 4.0_wp * zaqe * zcqe               
            !
            ! Presumably zdis can never be negative because gammas is very small compared to gammat
            zsfrz=(-zbqe - SQRT(zdis)) * zaqer
            IF ( zsfrz < 0.0_wp ) zsfrz=(-zbqe + SQRT(zdis)) * zaqer  ! check this if this if is needed
            !
            ! compute t freeze (eq. 25)
            ztfrz(ji,jj) = zeps4 + risf_lamb1 * zsfrz
            !
            ! thermal driving
            zthd(ji,jj) = ( pttbl(ji,jj) - ztfrz(ji,jj) )
            !
            ! compute the upward water and heat flux (eq. 24 and eq. 26)
            pqfwf(ji,jj) = rau0     * pgs(ji,jj) * ( zsfrz - pstbl(ji,jj) ) / MAX(zsfrz,zeps) ! fresh water flux    (> 0 out)
            pqoce(ji,jj) = rau0_rcp * pgt(ji,jj) * zthd (ji,jj)                               ! ocean-ice heat flux (> 0 out)
            pqhc (ji,jj) = rcp      * pqfwf(ji,jj) * ztfrz(ji,jj)                             ! heat content   flux (> 0 out)
            !
            zqcon(ji,jj) = zeps3 * ( ztfrz(ji,jj) - rtsurf )
            !
         END DO
      END DO
      !
      ! output conductive heat flux through the ice
      CALL iom_put('qconisf', zqcon(:,:) * mskisf_cav(:,:) )
      !
      ! output thermal driving and freezing point at the interface
      CALL iom_put('isfthermald_cav', zthd (:,:) * mskisf_cav(:,:) )
      CALL iom_put('isftfrz_cav'    , ztfrz(:,:) * mskisf_cav(:,:) )
      !
   END SUBROUTINE isfcav_mlt_3eq

   SUBROUTINE isfcav_mlt_oasis(kt, pstbl,          &  ! <<== in
      &                        pqhc , pqoce, pqfwf )  ! ==>> out
      !!----------------------------------------------------------------------
      !!                          ***  ROUTINE isfcav_mlt_oasis  ***
      !!
      !! ** Purpose    : scale the fwf read from input file by the total amount received by the sbccpl interface
      !!
      !! ** Purpose    : - read ice shelf melt from forcing file => pattern
      !!                 - total amount of fwf is given by sbccpl (fwfisf_cpl)
      !!                 - scale fwf and compute heat fluxes
      !!
      !!---------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pqhc, pqoce, pqfwf  ! heat content, latent heat and fwf fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER                     , INTENT(in   ) :: kt                  ! current time step
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pstbl               ! salinity in tbl
      !!--------------------------------------------------------------------
      REAL(wp)                     :: zfwf_fld, zfwf_oasis               ! total fwf in the forcing fields (pattern) and from the oasis interface (amount)
      REAL(wp), DIMENSION(jpi,jpj) :: ztfrz                              ! tbl freezing temperature
      REAL(wp), DIMENSION(jpi,jpj) :: zfwf                               ! 2d fwf map after scaling
      !!--------------------------------------------------------------------
      !
      ! Calculate freezing temperature
      CALL eos_fzp( pstbl(:,:), ztfrz(:,:), risfdep(:,:) )
      !
      ! read input file
      CALL fld_read ( kt, 1, sf_isfcav_fwf )
      !
      ! ice shelf 2d map
      zfwf(:,:) = - sf_isfcav_fwf(1)%fnow(:,:,1)
      !
      ! compute glob sum from input file
      ! (PM) should consider delay sum as in fwb (1 time step offset if I well understood)
      zfwf_fld = glob_sum('isfcav_mlt', e1e2t(:,:) * zfwf(:,:))
      !
      ! compute glob sum from atm->oce ice shelf fwf
      ! (PM) should consider delay sum as in fwb (1 time step offset if I well understood)
      zfwf_oasis = glob_sum('isfcav_mlt', e1e2t(:,:) * fwfisf_oasis(:,:))
      !
      ! scale fwf
      zfwf(:,:) = zfwf(:,:) * zfwf_oasis / zfwf_fld
      ! 
      ! define fwf and qoce
      ! ocean heat flux is assume to be equal to the latent heat
      pqfwf(:,:) =   zfwf(:,:)                         ! fwf                ( >0 out)
      pqoce(:,:) = - pqfwf(:,:) * rLfusisf             ! ocean heat flux    ( >0 out)
      pqhc (:,:) =   pqfwf(:,:) * ztfrz(:,:) * rcp     ! heat content flux  ( >0 out)
      !
      CALL iom_put('isftfrz_cav', ztfrz * mskisf_cav(:,:) )
      !
   END SUBROUTINE isfcav_mlt_oasis

END MODULE isfcavmlt
