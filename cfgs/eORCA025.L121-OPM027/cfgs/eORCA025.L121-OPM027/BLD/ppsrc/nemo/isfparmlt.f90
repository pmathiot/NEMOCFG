










MODULE isfparmlt
   !!======================================================================
   !!                       ***  MODULE  isfparmlt  ***
   !! Ice shelf parametrisation module :  update surface ocean boundary condition under ice
   !!                   shelf using an ice shelf melt parametrisation
   !!======================================================================
   !! History :  4.0  ! original code
   !!----------------------------------------------------------------------

   USE isf_oce                  ! ice shelf
   USE isftbl , ONLY: isf_tbl   ! ice shelf depth average

   USE dom_oce                  ! ocean space and time domain
   USE oce    , ONLY: tsn       ! ocean dynamics and tracers
   USE phycst , ONLY: rcp, rau0 ! physical constants
   USE eosbn2 , ONLY: eos_fzp   ! equation of state

   USE in_out_manager              ! I/O manager
   USE iom        , ONLY: iom_put  ! I/O library
   USE fldread    , ONLY: fld_read, FLD, FLD_N !
   USE lib_fortran, ONLY: glob_sum !
   USE lib_mpp    , ONLY: ctl_stop !

   IMPLICIT NONE

   PRIVATE

   PUBLIC  isfpar_mlt 
   
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

! -------------------------------------------------------------------------------------------------------
! -------------------------------- PUBLIC SUBROUTINE ----------------------------------------------------
! -------------------------------------------------------------------------------------------------------

  SUBROUTINE isfpar_mlt( kt, pqhc, pqoce, pqfwf )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt  ***
      !!
      !! ** Purpose : Compute Salt and Heat fluxes related to ice_shelf 
      !!              melting and freezing 
      !!
      !! ** Method  :  2 parameterizations are available according
      !!                        1 : Specified melt flux
      !!                        2 : Beckmann & Goose parameterization
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) :: pqfwf, pqoce, pqhc  ! fresh water, ice-ocean heat and heat content fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !!---------------------------------------------------------------------
      !
      ! Choose among the available ice shelf parametrisation
      SELECT CASE ( cn_isfpar_mlt )
      CASE ( 'spe' )    ! specified runoff in depth (Mathiot et al., 2017 in preparation)
         CALL isfpar_mlt_spe(kt, pqhc, pqoce, pqfwf)
      CASE ( 'bg03' )    ! Beckmann and Goosse parametrisation 
         CALL isfpar_mlt_bg03(kt, pqhc, pqoce, pqfwf)
      CASE ( 'oasis' )
         CALL isfpar_mlt_oasis( kt, pqhc, pqoce, pqfwf)
      CASE DEFAULT
         CALL ctl_stop('STOP', 'unknown isf melt formulation : cn_isfpar (should not see this)')
      END SELECT
      !
   END SUBROUTINE isfpar_mlt

! -------------------------------------------------------------------------------------------------------
! -------------------------------- PRIVATE SUBROUTINE ---------------------------------------------------
! -------------------------------------------------------------------------------------------------------

   SUBROUTINE isfpar_mlt_spe(kt, pqhc, pqoce, pqfwf)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt_spe  ***
      !!
      !! ** Purpose : prescribed ice shelf melting in case ice shelf cavities are closed.
      !!              data read into a forcing files.
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) :: pqhc, pqfwf, pqoce  ! fresh water and ice-ocean heat fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER,  INTENT(in) :: kt
      !!--------------------------------------------------------------------
      INTEGER :: jk
      REAL(wp), DIMENSION(jpi,jpj,jpk)  :: ztfrz3d
      REAL(wp), DIMENSION(jpi,jpj)      :: ztfrz
      !!--------------------------------------------------------------------
      !
      ! 0. ------------Read specified runoff
      CALL fld_read ( kt, 1, sf_isfpar_fwf   )
      !
      ! compute ptfrz
      ! 1. ------------Mean freezing point
      DO jk = 1,jpk
         CALL eos_fzp(tsn(:,:,jk,jp_sal), ztfrz3d(:,:,jk), gdept_n(:,:,jk))
      END DO
      CALL isf_tbl(ztfrz3d, ztfrz, 'T', misfkt_par, rhisf_tbl_par, misfkb_par, rfrac_tbl_par )
      !
      pqfwf(:,:) = - sf_isfpar_fwf(1)%fnow(:,:,1)      ! fresh water flux from the isf (fwfisf <0 mean melting) 
      pqoce(:,:) =   pqfwf(:,:) * rLfusisf             ! ocean/ice shelf flux assume to be equal to latent heat flux
      pqhc (:,:) =   pqfwf(:,:) * ztfrz(:,:) * rcp     ! heat content flux 
      !
      CALL iom_put('isftfrz_par', ztfrz )
      !
   END SUBROUTINE isfpar_mlt_spe

   SUBROUTINE isfpar_mlt_bg03(kt, pqhc, pqoce, pqfwf)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt_bg03  ***
      !!
      !! ** Purpose : compute an estimate of ice shelf melting and 
      !!              latent, ocean-ice and heat content heat fluxes
      !!              in case cavities are closed based on the far fields T and S properties. 
      !!
      !! ** Method  : The ice shelf melt is computed as proportional to the differences between the 
      !!              mean temperature and mean freezing point in front of the ice shelf averaged 
      !!              over the ice shelf min ice shelf draft and max ice shelf draft and the freezing point
      !!
      !! ** Reference : Beckmann and Goosse (2003), "A parameterization of ice shelf-ocean
      !!                interaction for climate models", Ocean Modelling 5(2003) 157-170.
      !!----------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) :: pqhc, pqfwf, pqoce  ! fresh water and ice-ocean heat fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER,  INTENT(in) :: kt
      !!--------------------------------------------------------------------
      INTEGER :: jk
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ztfrz3d        ! freezing point
      REAL(wp), DIMENSION(jpi,jpj)     :: ztfrz          ! freezing point
      REAL(wp), DIMENSION(jpi,jpj)     :: ztavg          ! temperature avg
      !!----------------------------------------------------------------------
      !
      ! 0. ------------Mean freezing point
      DO jk = 1,jpk
         CALL eos_fzp(tsn(:,:,jk,jp_sal), ztfrz3d(:,:,jk), gdept_n(:,:,jk))
      END DO
      CALL isf_tbl(ztfrz3d, ztfrz, 'T', misfkt_par, rhisf_tbl_par, misfkb_par, rfrac_tbl_par )
      !
      ! 1. ------------Mean temperature
      CALL isf_tbl(tsn(:,:,jk,jp_tem), ztavg, 'T', misfkt_par, rhisf_tbl_par, misfkb_par, rfrac_tbl_par )
      !
      ! 2. ------------Net heat flux and fresh water flux due to the ice shelf
      pqoce(:,:) =   rau0 * rcp * rn_gammat0 * risfLeff(:,:) * e1t(:,:) * ( ztavg(:,:) - ztfrz(:,:) ) * r1_e1e2t(:,:)
      pqfwf(:,:) = - pqoce(:,:) / rLfusisf             ! derived from the latent heat flux
      pqhc (:,:) =   pqfwf(:,:) * ztfrz(:,:) * rcp     ! heat content flux 
      !
      ! 3. ------------BG03 output
      ! output ttbl
      CALL iom_put('ttbl_par', ztavg(:,:) * mskisf_par(:,:) )
      !
      ! output thermal driving
      CALL iom_put('isfthermald_par',( ztfrz(:,:) - ztavg(:,:) ) * mskisf_par(:,:))
      !
      ! output freezing point used to define the thermal driving and heat content fluxes
      CALL iom_put('isftfrz_par', ztfrz )
      !
   END SUBROUTINE isfpar_mlt_bg03

   SUBROUTINE isfpar_mlt_oasis(kt, pqhc , pqoce, pqfwf )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt_oasis  ***
      !!
      !! ** Purpose    : scale the fwf read from input file by the total amount received by the sbccpl interface
      !!
      !! ** Purpose    : - read ice shelf melt from forcing file and scale it by the input file total amount => pattern
      !!                 - compute total amount of fwf given by sbccpl (fwfisf_oasis)
      !!                 - scale fwf and compute heat fluxes
      !!
      !!---------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pqhc, pqoce, pqfwf  ! heat content, latent heat and fwf fluxes
      !!-------------------------- IN  -------------------------------------
      INTEGER                     , INTENT(in   ) :: kt                  ! current time step
      !!--------------------------------------------------------------------
      INTEGER                           :: jk                            ! loop index
      REAL(wp)                          :: zfwf_fld, zfwf_oasis          ! total fwf in the forcing fields (pattern) and from the cpl interface (amount)
      REAL(wp), DIMENSION(jpi,jpj)      :: ztfrz                         ! tbl freezing temperature
      REAL(wp), DIMENSION(jpi,jpj)      :: zfwf                          ! 2d fwf map after scaling
      REAL(wp), DIMENSION(jpi,jpj,jpk)  :: ztfrz3d
      !!--------------------------------------------------------------------
      !
      ! 0. ------------Read specified runoff
      CALL fld_read ( kt, 1, sf_isfpar_fwf   )
      !
      ! 1. ------------Mean freezing point (needed for heat content flux)
      DO jk = 1,jpk
         CALL eos_fzp(tsn(:,:,jk,jp_sal), ztfrz3d(:,:,jk), gdept_n(:,:,jk))
      END DO
      CALL isf_tbl(ztfrz3d, ztfrz, 'T', misfkt_par, rhisf_tbl_par, misfkb_par, rfrac_tbl_par )
      !
      ! 2. ------------Scale isf melt pattern with total amount from oasis
      ! ice shelf 2d map
      zfwf(:,:) = - sf_isfpar_fwf(1)%fnow(:,:,1)
      !
      ! compute glob sum from input file
      ! (PM) should we consider delay sum as in fwb ? (it will offset by 1 time step if I understood well)
      zfwf_fld = glob_sum('isfcav_mlt', e1e2t(:,:) * zfwf(:,:))
      !
      ! compute glob sum from atm->oce ice shelf fwf
      ! (PM) should we consider delay sum as in fwb ?
      zfwf_oasis = glob_sum('isfcav_mlt', e1e2t(:,:) * fwfisf_oasis(:,:))
      !
      ! scale fwf
      zfwf(:,:) = zfwf(:,:) * zfwf_oasis / zfwf_fld
      ! 
      ! 3. -----------Define fwf and qoce
      ! ocean heat flux is assume to be equal to the latent heat
      pqfwf(:,:) =   zfwf(:,:)                         ! fwf                ( >0 out )
      pqoce(:,:) = - pqfwf(:,:) * rLfusisf             ! ocean heat flux    ( >0 out ) (assumed to be the latent heat flux)
      pqhc (:,:) =   pqfwf(:,:) * ztfrz(:,:) * rcp     ! heat content flux  ( >0 out )
      !
      CALL iom_put('isftfrz_par', ztfrz )
      !
   END SUBROUTINE isfpar_mlt_oasis

END MODULE isfparmlt
