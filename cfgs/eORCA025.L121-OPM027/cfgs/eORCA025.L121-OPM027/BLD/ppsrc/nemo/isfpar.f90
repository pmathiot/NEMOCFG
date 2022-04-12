










MODULE isfpar
   !!======================================================================
   !!                       ***  MODULE  isfpar  ***
   !! ice shelf module :  update ocean boundary condition under ice
   !!                   shelf
   !!======================================================================
   !! History :  3.2  !  2011-02  (C.Harris  ) Original code isf cav
   !!            X.X  !  2006-02  (C. Wang   ) Original code bg03
   !!            3.4  !  2013-03  (P. Mathiot) Merging + parametrization
   !!            4.1  !  2019-09  (P. Mathiot) Restructuration
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfpar       : compute ice shelf melt using a prametrisation of ice shelf cavities
   !!----------------------------------------------------------------------
   USE isf_oce        ! ice shelf
   !
   USE isfrst   , ONLY: isfrst_write, isfrst_read ! ice shelf restart read/write subroutine
   USE isftbl   , ONLY: isf_tbl_ktop, isf_tbl_lvl ! ice shelf top boundary layer properties subroutine
   USE isfparmlt, ONLY: isfpar_mlt                ! ice shelf melt formulation subroutine
   USE isfdiags , ONLY: isf_diags_flx             ! ice shelf diags subroutine
   USE isfutils , ONLY: debug, read_2dcstdta      ! ice shelf debug subroutine
   !
   USE dom_oce  , ONLY: bathy          ! ocean space and time domain
   USE par_oce  , ONLY: jpi,jpj        ! ocean space and time domain
   USE phycst   , ONLY: r1_rau0_rcp    ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE fldread        ! read input field at current time step
   USE lbclnk         ! lbc_lnk

   IMPLICIT NONE
   PRIVATE

   PUBLIC   isf_par, isf_par_init

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
 
   SUBROUTINE isf_par( kt, ptsc, pqfwf )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE isf_par ***      
      !!
      !! ** Purpose : compute the heat and fresh water due to ice shelf melting/freezing using a parametrisation 
      !!
      !! ** Comment : in isf_par and all its call tree, 
      !!              'tbl' means parametrisation layer (ie how the far field temperature/salinity is computed) 
      !!              instead of in a proper top boundary layer as at the ice shelf ocean interface
      !!              as the action to compute the properties of the tbl or the parametrisation layer are the same,
      !!              (ie average T/S over a specific depth (can be across multiple levels))
      !!              the name tbl was kept.
      !!
      !!---------------------------------------------------------------------
      !!-------------------------- OUT --------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(inout) :: pqfwf
      REAL(wp), DIMENSION(jpi,jpj,jpts), INTENT(inout) :: ptsc
      !!-------------------------- IN  --------------------------------------
      INTEGER, INTENT(in) ::   kt                                           ! ocean time step
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: zqoce, zqhc, zqlat, zqh
      !!---------------------------------------------------------------------
      !
      ! compute heat content, latent heat and melt fluxes (2d)
      CALL isfpar_mlt( kt, zqhc, zqoce, pqfwf  )
      !
      ! compute heat and water flux ( > 0 out )
      pqfwf(:,:) = pqfwf(:,:) * mskisf_par(:,:)
      zqoce(:,:) = zqoce(:,:) * mskisf_par(:,:)
      zqhc (:,:) = zqhc(:,:)  * mskisf_par(:,:)
      !
      ! compute heat content flux ( > 0 out )
      zqlat(:,:) = pqfwf(:,:) * rLfusisf    ! 2d latent heat flux (W/m2)
      !
      ! total heat flux ( > 0 out )
      zqh(:,:) = ( zqhc (:,:) + zqoce(:,:) )
      !
      ! lbclnk on melt and heat fluxes
      CALL lbc_lnk_multi( 'isfmlt', zqh, 'T', 1., pqfwf, 'T', 1.)
      !
      ! output fluxes
      CALL isf_diags_flx( misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, 'par', pqfwf, zqoce, zqlat, zqhc)
      !
      ! set temperature content
      ptsc(:,:,jp_tem) = zqh(:,:) * r1_rau0_rcp
      !
      ! write restart variables (qoceisf, qhcisf, fwfisf for now and before)
      IF (lrst_oce) CALL isfrst_write(kt, 'par', ptsc, pqfwf)
      !
      IF ( ln_isfdebug ) THEN
         CALL debug('isf_par: ptsc T',ptsc(:,:,1))
         CALL debug('isf_par: ptsc S',ptsc(:,:,2))
         CALL debug('isf_par: pqfwf fwf',pqfwf(:,:))
      END IF
      !
   END SUBROUTINE isf_par

   SUBROUTINE isf_par_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_par_init  ***
      !!
      !! ** Purpose : initialisation of the variable needed for the parametrisation of ice shelf melt
      !!
      !!----------------------------------------------------------------------
      INTEGER               :: ierr
      REAL(wp), DIMENSION(jpi,jpj) :: ztblmax, ztblmin
      !!----------------------------------------------------------------------
      !
      ! allocation
      CALL isf_alloc_par()
      !
      ! initialisation
      misfkt_par(:,:)     = 1         ; misfkb_par(:,:)       = 1         
      rhisf_tbl_par(:,:)  = 1e-20     ; rfrac_tbl_par(:,:)    = 0.0_wp
      !
      ! define isf tbl tickness, top and bottom indice
      CALL read_2dcstdta(TRIM(sn_isfpar_zmax%clname), TRIM(sn_isfpar_zmax%clvar), ztblmax)
      CALL read_2dcstdta(TRIM(sn_isfpar_zmin%clname), TRIM(sn_isfpar_zmin%clvar), ztblmin)
      !
      ! mask ice shelf parametrisation location
      ztblmax(:,:) = ztblmax(:,:) * ssmask(:,:)
      ztblmin(:,:) = ztblmin(:,:) * ssmask(:,:)
      !
      ! if param used under an ice shelf overwrite ztblmin by the ice shelf draft
      WHERE ( risfdep > 0._wp .AND. ztblmin > 0._wp )
         ztblmin(:,:) = risfdep(:,:)
      END WHERE
      !
      ! ensure ztblmax <= bathy
      WHERE ( ztblmax(:,:) > bathy(:,:) )
         ztblmax(:,:) = bathy(:,:)
      END WHERE
      !
      ! compute ktop and update ztblmin to gdepw_0(misfkt_par) 
      CALL isf_tbl_ktop(ztblmin, misfkt_par) !   out: misfkt_par
      !                                      ! inout: ztblmin
      !
      ! initial tbl thickness
      rhisf0_tbl_par(:,:) = ztblmax(:,:) - ztblmin(:,:)
      !
      ! define iceshelf parametrisation mask
      mskisf_par = 0
      WHERE ( rhisf0_tbl_par(:,:) > 0._wp )
         mskisf_par(:,:) = 1._wp
      END WHERE
      !
      ! read par variable from restart
      IF ( ln_rstart ) CALL isfrst_read('par', risf_par_tsc, fwfisf_par, risf_par_tsc_b, fwfisf_par_b)
      !
      SELECT CASE ( TRIM(cn_isfpar_mlt) )
         !
      CASE ( 'spe' )
         !
         ALLOCATE( sf_isfpar_fwf(1), STAT=ierr )
         ALLOCATE( sf_isfpar_fwf(1)%fnow(jpi,jpj,1), sf_isfpar_fwf(1)%fdta(jpi,jpj,1,2) )
         CALL fld_fill( sf_isfpar_fwf, (/ sn_isfpar_fwf /), cn_isfdir, 'isf_par_init', 'read fresh water flux isf data', 'namisf' )
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ==>>>   ice melt read from forcing field (cn_isfmlt_par = spe)'
         !
      CASE ( 'bg03' )
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ==>>>   bg03 parametrisation (cn_isfmlt_par = bg03)'
         !
         ! read effective length
         CALL read_2dcstdta(TRIM(sn_isfpar_Leff%clname), TRIM(sn_isfpar_Leff%clvar), risfLeff)
         risfLeff = risfLeff*1000.0_wp           !: convertion in m
         !
      CASE ( 'oasis' )
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ==>>>    isf melt provided by OASIS (cn_isfmlt_par = oasis)'
         !
      CASE DEFAULT
         CALL ctl_stop( 'sbc_isf_init: wrong value of nn_isf' )
      END SELECT
      !
   END SUBROUTINE isf_par_init

END MODULE isfpar
