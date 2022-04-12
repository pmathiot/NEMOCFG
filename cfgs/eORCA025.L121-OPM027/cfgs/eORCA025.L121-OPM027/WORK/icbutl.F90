MODULE icbutl
   !!======================================================================
   !!                       ***  MODULE  icbutl  ***
   !! Icebergs:  various iceberg utility routines
   !!======================================================================
   !! History : 3.3.1 !  2010-01  (Martin&Adcroft) Original code
   !!            -    !  2011-03  (Madec)          Part conversion to NEMO form
   !!            -    !                            Removal of mapping from another grid
   !!            -    !  2011-04  (Alderson)       Split into separate modules
   !!           4.2   !  2020-07  (P. Mathiot)     simplification of interpolation routine
   !!                 !                            and add Nacho Merino work
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   icb_utl_interp   :
   !!   icb_utl_pos      : compute bottom left corner indice, weight and mask
   !!   icb_utl_bilin_h  : interpolation field to icb position
   !!   icb_utl_bilin_e  : interpolation of scale factor to icb position
   !!----------------------------------------------------------------------
   USE par_oce                             ! ocean parameters
   USE oce,    ONLY: tsn, un, vn
   USE dom_oce                             ! ocean domain
   USE in_out_manager                      ! IO parameters
   USE lbclnk                              ! lateral boundary condition
   USE lib_mpp                             ! MPI code and lk_mpp in particular
   USE icb_oce                             ! define iceberg arrays
   USE sbc_oce                             ! ocean surface boundary conditions
#if defined key_si3
   USE ice,    ONLY: u_ice, v_ice, hm_i    ! SI3 variables
   USE icevar                              ! ice_var_sshdyn
   USE sbc_ice, ONLY: snwice_mass, snwice_mass_b
#endif

   IMPLICIT NONE
   PRIVATE

   INTERFACE icb_utl_bilin_h
      MODULE PROCEDURE icb_utl_bilin_2d_h, icb_utl_bilin_3d_h
   END INTERFACE

   PUBLIC   icb_utl_copy          ! routine called in icbstp module
   PUBLIC   icb_utl_getkb         ! routine called in icbdyn and icbthm modules
   PUBLIC   test_icb_utl_getkb    ! routine called in icbdyn and icbthm modules
   PUBLIC   icb_utl_zavg          ! routine called in icbdyn and icbthm modules
   PUBLIC   icb_utl_interp        ! routine called in icbdyn, icbthm modules
   PUBLIC   icb_utl_bilin_h       ! routine called in icbdyn module
   PUBLIC   icb_utl_add           ! routine called in icbini.F90, icbclv, icblbc and icbrst modules
   PUBLIC   icb_utl_delete        ! routine called in icblbc, icbthm modules
   PUBLIC   icb_utl_destroy       ! routine called in icbstp module
   PUBLIC   icb_utl_track         ! routine not currently used, retain just in case
   PUBLIC   icb_utl_print_berg    ! routine called in icbthm module
   PUBLIC   icb_utl_print         ! routine called in icbini, icbstp module
   PUBLIC   icb_utl_count         ! routine called in icbdia, icbini, icblbc, icbrst modules
   PUBLIC   icb_utl_incr          ! routine called in icbini, icbclv modules
   PUBLIC   icb_utl_yearday       ! routine called in icbclv, icbstp module
   PUBLIC   icb_utl_mass          ! routine called in icbdia module
   PUBLIC   icb_utl_heat          ! routine called in icbdia module

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: icbutl.F90 13263 2020-07-08 07:55:54Z ayoung $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE icb_utl_copy( )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE icb_utl_copy  ***
      !!
      !! ** Purpose :   iceberg initialization.
      !!
      !! ** Method  : - blah blah
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(0:jpi+1,0:jpj+1) :: ztmp
#if defined key_si3
      REAL(wp), DIMENSION(jpi,jpj) :: zssh_lead_m    !    ocean surface (ssh_m) if ice is not embedded
      !                                              !    ocean surface in leads if ice is embedded   
#endif
      INTEGER :: jk   ! vertical loop index
      !
      ! copy nemo forcing arrays into iceberg versions with extra halo
      ! only necessary for variables not on T points
      ! and ssh which is used to calculate gradients
      !
      ! surface forcing
      !
      ssu_e(1:jpi,1:jpj) = ssu_m(:,:) * umask(:,:,1)
      ssv_e(1:jpi,1:jpj) = ssv_m(:,:) * vmask(:,:,1)
      sst_e(1:jpi,1:jpj) = sst_m(:,:)
      sss_e(1:jpi,1:jpj) = sss_m(:,:)
      fr_e (1:jpi,1:jpj) = fr_i (:,:)
      ua_e (1:jpi,1:jpj) = utau (:,:) * umask(:,:,1) ! maybe mask useless because mask applied in sbcblk
      va_e (1:jpi,1:jpj) = vtau (:,:) * vmask(:,:,1) ! maybe mask useless because mask applied in sbcblk
      !
      CALL lbc_lnk_icb( 'icbutl', ssu_e, 'U', -1._wp, 1, 1 )
      CALL lbc_lnk_icb( 'icbutl', ssv_e, 'V', -1._wp, 1, 1 )
      CALL lbc_lnk_icb( 'icbutl', ua_e, 'U', -1._wp, 1, 1 )
      CALL lbc_lnk_icb( 'icbutl', va_e, 'V', -1._wp, 1, 1 )
#if defined key_si3
      hi_e(1:jpi, 1:jpj) = hm_i (:,:)  
      ui_e(1:jpi, 1:jpj) = u_ice(:,:)
      vi_e(1:jpi, 1:jpj) = v_ice(:,:)
      !      
      ! compute ssh slope using ssh_lead if embedded
      zssh_lead_m(:,:) = ice_var_sshdyn(ssh_m, snwice_mass, snwice_mass_b)
      ssh_e(1:jpi, 1:jpj) = zssh_lead_m(:,:) * tmask(:,:,1)
      !
      CALL lbc_lnk_icb( 'icbutl', ui_e , 'U', -1._wp, 1, 1 )
      CALL lbc_lnk_icb( 'icbutl', vi_e , 'V', -1._wp, 1, 1 )
#else
      ssh_e(1:jpi, 1:jpj) = ssh_m(:,:) * tmask(:,:,1)         
#endif
      !
      ! (PM) could be improve with a 3d lbclnk gathering both variables
      ! should be done once extra haloe generalised
      IF ( ln_M2016 ) THEN
         DO jk = 1,jpk
            ! uoce
            ztmp(1:jpi,1:jpj) = un(:,:,jk)
            CALL lbc_lnk_icb( 'icbutl', ztmp, 'U', -1._wp, 1, 1 )
            uoce_e(:,:,jk) = ztmp(:,:)
            !
            ! voce
            ztmp(1:jpi,1:jpj) = vn(:,:,jk)
            CALL lbc_lnk_icb( 'icbutl', ztmp, 'V', -1._wp, 1, 1 )
            voce_e(:,:,jk) = ztmp(:,:)
         END DO
         toce_e(1:jpi,1:jpj,1:jpk) = tsn(:,:,:,1)
         e3t_e (1:jpi,1:jpj,1:jpk) = e3t_n(:,:,:)
      END IF
      !
   END SUBROUTINE icb_utl_copy


   SUBROUTINE icb_utl_interp( pi, pj, pe1 , pssu, pui, pua, pssh_i,     &
      &                               pe2 , pssv, pvi, pva, pssh_j,     &
      &                               psst, psss, pcn, phi, pff, plon, plat, &
      &                               ptoce, puoce, pvoce, pe3t       )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE icb_utl_interp  ***
      !!
      !! ** Purpose :   interpolation
      !!
      !! ** Method  : - interpolate from various ocean arrays onto iceberg position
      !!
      !!       !!gm  CAUTION here I do not care of the slip/no-slip conditions
      !!             this can be done later (not that easy to do...)
      !!             right now, U is 0 in land so that the coastal value of velocity parallel to the coast
      !!             is half the off shore value, wile the normal-to-the-coast value is zero.
      !!             This is OK as a starting point.
      !!       !!pm  HARD CODED: - rho_air now computed in sbcblk (what are the effect ?)
      !!                         - drag coefficient (should it be namelist parameter ?)
      !!
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ) ::   pi , pj                        ! position in (i,j) referential
      REAL(wp), INTENT(  out), OPTIONAL ::   pe1, pe2                       ! i- and j scale factors
      REAL(wp), INTENT(  out), OPTIONAL ::   pssu, pssv, pui, pvi, pua, pva   ! ocean, ice and wind speeds
      REAL(wp), INTENT(  out), OPTIONAL ::   pssh_i, pssh_j                 ! ssh i- & j-gradients
      REAL(wp), INTENT(  out), OPTIONAL ::   psst, psss, pcn, phi, pff      ! SST, SSS, ice concentration, ice thickness, Coriolis
      REAL(wp), INTENT(  out), OPTIONAL ::   plat, plon                     ! position
      REAL(wp), DIMENSION(jpk), INTENT(  out), OPTIONAL ::   ptoce, puoce, pvoce, pe3t   ! 3D variables
      !
      REAL(wp), DIMENSION(4) :: zwT  , zwU  , zwV  , zwF   ! interpolation weight
      REAL(wp), DIMENSION(4) :: zmskF, zmskU, zmskV, zmskT ! mask
      REAL(wp), DIMENSION(4) :: zwTp, zmskTp, zwTm, zmskTm
      REAL(wp), DIMENSION(4,jpk) :: zw1d
      INTEGER                :: iiT, iiU, iiV, iiF, ijT, ijU, ijV, ijF ! bottom left corner
      INTEGER                :: iiTp, iiTm, ijTp, ijTm
      REAL(wp) ::   zcd, zmod       ! local scalars
      !!----------------------------------------------------------------------
      !
      ! get position, weight and mask 
      CALL icb_utl_pos( pi, pj, 'T', iiT, ijT, zwT, zmskT )
      CALL icb_utl_pos( pi, pj, 'U', iiU, ijU, zwU, zmskU )
      CALL icb_utl_pos( pi, pj, 'V', iiV, ijV, zwV, zmskV )
      CALL icb_utl_pos( pi, pj, 'F', iiF, ijF, zwF, zmskF )
      !
      ! metrics and coordinates
      IF ( PRESENT(pe1 ) ) pe1 = icb_utl_bilin_e( e1t, e1u, e1v, e1f, pi, pj )      ! scale factors
      IF ( PRESENT(pe2 ) ) pe2 = icb_utl_bilin_e( e2t, e2u, e2v, e2f, pi, pj )
      IF ( PRESENT(plon) ) plon= icb_utl_bilin_h( rlon_e, iiT, ijT, zwT, .true.  )
      IF ( PRESENT(plat) ) plat= icb_utl_bilin_h( rlat_e, iiT, ijT, zwT, .false. )
      !
      IF ( PRESENT(pssu) ) pssu = icb_utl_bilin_h( ssu_e, iiU, ijU, zwU        , .false. )         ! ocean velocities
      IF ( PRESENT(pssv) ) pssv = icb_utl_bilin_h( ssv_e, iiV, ijV, zwV        , .false. )         !
      IF ( PRESENT(psst) ) psst = icb_utl_bilin_h( sst_e, iiT, ijT, zwT * zmskT, .false. ) ! sst
      IF ( PRESENT(psss) ) psss = icb_utl_bilin_h( sss_e, iiT, ijT, zwT * zmskT, .false. ) ! sss
      IF ( PRESENT(pcn ) ) pcn  = icb_utl_bilin_h( fr_e , iiT, ijT, zwT * zmskT, .false. ) ! ice concentration
      IF ( PRESENT(pff ) ) pff  = icb_utl_bilin_h( ff_e , iiF, ijF, zwF        , .false. )         ! Coriolis parameter
      !
      IF ( PRESENT(pua) .AND. PRESENT(pva) ) THEN
         pua  = icb_utl_bilin_h( ua_e, iiU, ijU, zwU * zmskU, .false. ) ! 10m wind
         pva  = icb_utl_bilin_h( va_e, iiV, ijV, zwV * zmskV, .false. ) ! here (ua,va) are stress => rough conversion from stress to speed
         zcd  = 1.22_wp * 1.5e-3_wp                               ! air density * drag coefficient 
         zmod = 1._wp / MAX(  1.e-20, SQRT(  zcd * SQRT( pua*pua + pva*pva)  )  )
         pua  = pua * zmod                                       ! note: stress module=0 necessarly implies ua=va=0
         pva  = pva * zmod
      END IF
      !
#if defined key_si3
      IF ( PRESENT(pui) ) pui = icb_utl_bilin_h( ui_e , iiU, ijU, zwU        , .false. ) ! sea-ice velocities
      IF ( PRESENT(pvi) ) pvi = icb_utl_bilin_h( vi_e , iiV, ijV, zwV        , .false. )
      IF ( PRESENT(phi) ) phi = icb_utl_bilin_h( hi_e , iiT, ijT, zwT * zmskT, .false. ) ! ice thickness
#else
      IF ( PRESENT(pui) ) pui = 0._wp
      IF ( PRESENT(pvi) ) pvi = 0._wp
      IF ( PRESENT(phi) ) phi = 0._wp
#endif
      !
      ! Estimate SSH gradient in i- and j-direction (centred evaluation)
      IF ( PRESENT(pssh_i) .AND. PRESENT(pssh_j) ) THEN
         CALL icb_utl_pos( pi+0.1, pj    , 'T', iiTp, ijTp, zwTp, zmskTp )
         CALL icb_utl_pos( pi-0.1, pj    , 'T', iiTm, ijTm, zwTm, zmskTm )
         !
         IF ( .NOT. PRESENT(pe1) ) pe1 = icb_utl_bilin_e( e1t, e1u, e1v, e1f, pi, pj )
         pssh_i = ( icb_utl_bilin_h( ssh_e, iiTp, ijTp, zwTp*zmskTp, .false. ) -   &
            &       icb_utl_bilin_h( ssh_e, iiTm, ijTm, zwTm*zmskTm, .false. )  ) / ( 0.2_wp * pe1 )
         !
         CALL icb_utl_pos( pi    , pj+0.1, 'T', iiTp, ijTp, zwTp, zmskTp )
         CALL icb_utl_pos( pi    , pj-0.1, 'T', iiTm, ijTm, zwTm, zmskTm )
         !
         IF ( .NOT. PRESENT(pe2) ) pe2 = icb_utl_bilin_e( e2t, e2u, e2v, e2f, pi, pj )
         pssh_j = ( icb_utl_bilin_h( ssh_e, iiTp, ijTp, zwTp*zmskTp, .false. ) -   &
            &       icb_utl_bilin_h( ssh_e, iiTm, ijTm, zwTm*zmskTm, .false. )  ) / ( 0.2_wp * pe2 )
      END IF
      !
      ! 3d interpolation
      IF ( PRESENT(puoce) .AND. PRESENT(pvoce) ) THEN
         ! no need to mask as 0 is a valid data for land
         zw1d(1,:) = zwU(1) ; zw1d(2,:) = zwU(2) ; zw1d(3,:) = zwU(3) ; zw1d(4,:) = zwU(4) ;
         puoce(:) = icb_utl_bilin_h( uoce_e , iiU, ijU, zw1d )

         zw1d(1,:) = zwV(1) ; zw1d(2,:) = zwV(2) ; zw1d(3,:) = zwV(3) ; zw1d(4,:) = zwV(4) ;
         pvoce(:) = icb_utl_bilin_h( voce_e , iiV, ijV, zw1d )
      END IF

      IF ( PRESENT(ptoce) ) THEN
         ! for temperature we need to mask the weight properly
         ! no need of extra halo as it is a T point variable
         zw1d(1,:) = tmask(iiT  ,ijT  ,:) * zwT(1) * zmskT(1)
         zw1d(2,:) = tmask(iiT+1,ijT  ,:) * zwT(2) * zmskT(2)
         zw1d(3,:) = tmask(iiT  ,ijT+1,:) * zwT(3) * zmskT(3)
         zw1d(4,:) = tmask(iiT+1,ijT+1,:) * zwT(4) * zmskT(4)
         ptoce(:) = icb_utl_bilin_h( toce_e , iiT, ijT, zw1d )
      END IF
      !
      IF ( PRESENT(pe3t)  ) pe3t(:)  = e3t_e(iiT,ijT,:)    ! as in Nacho tarball need to be fix once we are able to reproduce Nacho results
      !
   END SUBROUTINE icb_utl_interp

   SUBROUTINE icb_utl_pos( pi, pj, cd_type, kii, kij, pw, pmsk )
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION icb_utl_bilin  ***
      !!
      !! ** Purpose :   bilinear interpolation at berg location depending on the grid-point type
      !!                this version deals with extra halo points
      !!
      !!       !!gm  CAUTION an optional argument should be added to handle
      !!             the slip/no-slip conditions  ==>>> to be done later
      !!
      !!----------------------------------------------------------------------
      REAL(wp)              , INTENT(IN)  ::   pi, pj    ! targeted coordinates in (i,j) referential
      CHARACTER(len=1)      , INTENT(IN)  ::   cd_type   ! point type
      REAL(wp), DIMENSION(4), INTENT(OUT) ::   pw, pmsk  ! weight and mask
      INTEGER ,               INTENT(OUT) ::   kii, kij  ! bottom left corner position in local domain
      !
      REAL(wp) :: zwi, zwj ! distance to bottom left corner
      INTEGER  :: ierr 
      !
      !!----------------------------------------------------------------------
      !
      SELECT CASE ( cd_type )
      CASE ( 'T' )
         ! note that here there is no +0.5 added
         ! since we're looking for four T points containing quadrant we're in of 
         ! current T cell
         kii = MAX(0, INT( pi     ))
         kij = MAX(0, INT( pj     ))    ! T-point
         zwi = pi - REAL(kii,wp)
         zwj = pj - REAL(kij,wp)
      CASE ( 'U' )
         kii = MAX(0, INT( pi-0.5_wp ))
         kij = MAX(0, INT( pj     ))    ! U-point
         zwi = pi - 0.5_wp - REAL(kii,wp)
         zwj = pj - REAL(kij,wp)
      CASE ( 'V' )
         kii = MAX(0, INT( pi     ))
         kij = MAX(0, INT( pj-0.5_wp ))    ! V-point
         zwi = pi - REAL(kii,wp)
         zwj = pj - 0.5_wp - REAL(kij,wp)
      CASE ( 'F' )
         kii = MAX(0, INT( pi-0.5_wp ))
         kij = MAX(0, INT( pj-0.5_wp ))    ! F-point
         zwi = pi - 0.5_wp - REAL(kii,wp)
         zwj = pj - 0.5_wp - REAL(kij,wp)
      END SELECT
      !
      ! compute weight
      pw(1) = (1._wp-zwi) * (1._wp-zwj)
      pw(2) =        zwi  * (1._wp-zwj)
      pw(3) = (1._wp-zwi) *        zwj
      pw(4) =        zwi  *        zwj
      !
      ! find position in this processor. Prevent near edge problems (see #1389)
      !
      IF (TRIM(cd_type) == 'T' ) THEN
         ierr = 0
         IF    ( kii <  mig( 1 ) ) THEN   ;  ierr = ierr + 1
         ELSEIF( kii >= mig(jpi) ) THEN   ;  ierr = ierr + 1
         ENDIF
         !
         IF    ( kij <  mjg( 1 ) ) THEN   ;   ierr = ierr + 1
         ELSEIF( kij >= mjg(jpj) ) THEN   ;   ierr = ierr + 1
         ENDIF
         !
         IF ( ierr > 0 ) THEN
            WRITE(numout,*) 'bottom left corner T point out of bound'
            WRITE(numout,*) pi, kii, mig( 1 ), mig(jpi)
            WRITE(numout,*) pj, kij, mjg( 1 ), mjg(jpj)
            WRITE(numout,*) pmsk
            CALL ctl_stop('STOP','icb_utl_bilin_h: an icebergs coordinates is out of valid range (out of bound error)')
         END IF
      END IF
      !
      ! find position in this processor. Prevent near edge problems (see #1389)
      ! (PM) will be useless if extra halo is used in NEMO
      !
      IF    ( kii <= mig(1)-1 ) THEN   ;   kii = 0
      ELSEIF( kii  > mig(jpi) ) THEN   ;   kii = jpi
      ELSE                             ;   kii = mi1(kii)
      ENDIF
      IF    ( kij <= mjg(1)-1 ) THEN   ;   kij = 0
      ELSEIF( kij  > mjg(jpj) ) THEN   ;   kij = jpj
      ELSE                             ;   kij = mj1(kij)
      ENDIF
      !
      ! define mask array 
      ! land value is not used in the interpolation
      SELECT CASE ( cd_type )
      CASE ( 'T' )
         pmsk = (/tmask_e(kii,kij), tmask_e(kii+1,kij), tmask_e(kii,kij+1), tmask_e(kii+1,kij+1)/)
      CASE ( 'U' )
         pmsk = (/umask_e(kii,kij), umask_e(kii+1,kij), umask_e(kii,kij+1), umask_e(kii+1,kij+1)/)
      CASE ( 'V' )
         pmsk = (/vmask_e(kii,kij), vmask_e(kii+1,kij), vmask_e(kii,kij+1), vmask_e(kii+1,kij+1)/)
      CASE ( 'F' )
         ! F case only used for coriolis, ff_f is not mask so zmask = 1
         pmsk = 1.
      END SELECT
   END SUBROUTINE icb_utl_pos

   REAL(wp) FUNCTION icb_utl_bilin_2d_h( pfld, pii, pij, pw, pllon )
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION icb_utl_bilin  ***
      !!
      !! ** Purpose :   bilinear interpolation at berg location depending on the grid-point type
      !!                this version deals with extra halo points
      !!
      !!       !!gm  CAUTION an optional argument should be added to handle
      !!             the slip/no-slip conditions  ==>>> to be done later
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(0:jpi+1,0:jpj+1), INTENT(in) ::   pfld      ! field to be interpolated
      REAL(wp), DIMENSION(4)              , INTENT(in) ::   pw        ! weight
      LOGICAL                             , INTENT(in) ::   pllon     ! input data is a longitude
      INTEGER ,                             INTENT(in) ::   pii, pij  ! bottom left corner
      !
      REAL(wp), DIMENSION(4) :: zdat ! input data
      !!----------------------------------------------------------------------
      !
      ! data
      zdat(1) = pfld(pii  ,pij  )
      zdat(2) = pfld(pii+1,pij  )
      zdat(3) = pfld(pii  ,pij+1)
      zdat(4) = pfld(pii+1,pij+1)
      !
      IF( pllon .AND. MAXVAL(zdat) - MINVAL(zdat) > 90._wp ) THEN
         WHERE( zdat < 0._wp ) zdat = zdat + 360._wp
      ENDIF
      !
      ! compute interpolated value
      icb_utl_bilin_2d_h = ( zdat(1)*pw(1) + zdat(2)*pw(2) + zdat(3)*pw(3) + zdat(4)*pw(4) ) / MAX(1.e-20, pw(1)+pw(2)+pw(3)+pw(4)) 
      !
      IF( pllon .AND. icb_utl_bilin_2d_h > 180._wp ) icb_utl_bilin_2d_h = icb_utl_bilin_2d_h - 360._wp
      !
   END FUNCTION icb_utl_bilin_2d_h

   FUNCTION icb_utl_bilin_3d_h( pfld, pii, pij, pw )
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION icb_utl_bilin  ***
      !!
      !! ** Purpose :   bilinear interpolation at berg location depending on the grid-point type
      !!                this version deals with extra halo points
      !!
      !!       !!gm  CAUTION an optional argument should be added to handle
      !!             the slip/no-slip conditions  ==>>> to be done later
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(0:jpi+1,0:jpj+1, jpk), INTENT(in) ::   pfld      ! field to be interpolated
      REAL(wp), DIMENSION(4,jpk)               , INTENT(in) ::   pw        ! weight
      INTEGER ,                                  INTENT(in) ::   pii, pij  ! bottom left corner
      REAL(wp), DIMENSION(jpk) :: icb_utl_bilin_3d_h
      !
      REAL(wp), DIMENSION(4,jpk) :: zdat ! input data
      INTEGER :: jk
      !!----------------------------------------------------------------------
      !
      ! data
      zdat(1,:) = pfld(pii  ,pij  ,:)
      zdat(2,:) = pfld(pii+1,pij  ,:)
      zdat(3,:) = pfld(pii  ,pij+1,:)
      zdat(4,:) = pfld(pii+1,pij+1,:)
      !
      ! compute interpolated value
      DO jk=1,jpk
         icb_utl_bilin_3d_h(jk) =   ( zdat(1,jk)*pw(1,jk) + zdat(2,jk)*pw(2,jk) + zdat(3,jk)*pw(3,jk) + zdat(4,jk)*pw(4,jk) ) &
            &                     /   MAX(1.e-20, pw(1,jk)+pw(2,jk)+pw(3,jk)+pw(4,jk)) 
      END DO
      !
   END FUNCTION icb_utl_bilin_3d_h

   REAL(wp) FUNCTION icb_utl_bilin_e( pet, peu, pev, pef, pi, pj )
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION dom_init  ***
      !!
      !! ** Purpose :   bilinear interpolation at berg location of horizontal scale factor
      !! ** Method  :   interpolation done using the 4 nearest grid points among
      !!                t-, u-, v-, and f-points.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pet, peu, pev, pef   ! horizontal scale factor to be interpolated at t-,u-,v- & f-pts
      REAL(wp)                , INTENT(IN) ::   pi , pj              ! iceberg position
      !
      ! weights corresponding to corner points of a T cell quadrant
      REAL(wp) ::   zi, zj          ! local real
      INTEGER  ::   ii, ij          ! bottom left corner coordinate in local domain
      !
      ! values at corner points of a T cell quadrant
      ! 00 = bottom left, 10 = bottom right, 01 = top left, 11 = top right
      REAL(wp) ::   ze00, ze10, ze01, ze11
      !!----------------------------------------------------------------------
      !
      ! cannot used iiT because need ii/ij reltaive to global indices not local one
      ii = MAX(1, INT( pi ))   ;   ij = MAX(1, INT( pj ))            ! left bottom T-point (i,j) indices
      ! 
      ! fractional box spacing
      ! 0   <= zi < 0.5, 0   <= zj < 0.5   -->  NW quadrant of current T cell
      ! 0.5 <= zi < 1  , 0   <= zj < 0.5   -->  NE quadrant
      ! 0   <= zi < 0.5, 0.5 <= zj < 1     -->  SE quadrant
      ! 0.5 <= zi < 1  , 0.5 <= zj < 1     -->  SW quadrant

      zi = pi - REAL(ii,wp)          !!gm use here mig, mjg arrays
      zj = pj - REAL(ij,wp)

      ! conversion to local domain (no need to do a sanity check already done in icbpos)
      ii = mi1(ii)
      ij = mj1(ij)
      !
      IF(    0.0_wp <= zi .AND. zi < 0.5_wp   ) THEN
         IF( 0.0_wp <= zj .AND. zj < 0.5_wp        )   THEN        !  NE quadrant
            !                                                      !             i=I       i=I+1/2
            ze01 = pev(ii  ,ij  )   ;   ze11 = pef(ii  ,ij  )      !   j=J+1/2    V ------- F
            ze00 = pet(ii  ,ij  )   ;   ze10 = peu(ii  ,ij  )      !   j=J        T ------- U
            zi = 2._wp * zi
            zj = 2._wp * zj
         ELSE                                                      !  SE quadrant
            !                                                                    !             i=I       i=I+1/2
            ze01 = pet(ii  ,ij+1)   ;   ze11 = peu(ii  ,ij+1)      !   j=J+1      T ------- U
            ze00 = pev(ii  ,ij  )   ;   ze10 = pef(ii  ,ij  )      !   j=J+1/2    V ------- F
            zi = 2._wp *  zi
            zj = 2._wp * (zj-0.5_wp)
         ENDIF
      ELSE
         IF( 0.0_wp <= zj .AND. zj < 0.5_wp        )   THEN        !  NW quadrant
            !                                                                    !             i=I       i=I+1/2
            ze01 = pef(ii  ,ij  )   ;   ze11 = pev(ii+1,ij)        !   j=J+1/2    F ------- V
            ze00 = peu(ii  ,ij  )   ;   ze10 = pet(ii+1,ij)        !   j=J        U ------- T
            zi = 2._wp * (zi-0.5_wp)
            zj = 2._wp *  zj
         ELSE                                                      !  SW quadrant
            !                                                                    !             i=I+1/2   i=I+1
            ze01 = peu(ii  ,ij+1)   ;   ze11 = pet(ii+1,ij+1)      !   j=J+1      U ------- T
            ze00 = pef(ii  ,ij  )   ;   ze10 = pev(ii+1,ij  )      !   j=J+1/2    F ------- V
            zi = 2._wp * (zi-0.5_wp)
            zj = 2._wp * (zj-0.5_wp)
         ENDIF
      ENDIF
      !
      icb_utl_bilin_e = ( ze01 * (1._wp-zi) + ze11 * zi ) *        zj    &
         &            + ( ze00 * (1._wp-zi) + ze10 * zi ) * (1._wp-zj)
      !
   END FUNCTION icb_utl_bilin_e

   SUBROUTINE icb_utl_getkb( kb, pe3, pD )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE icb_utl_getkb         ***
      !!
      !! ** Purpose :   compute the latest level affected by icb
      !!
      !!----------------------------------------------------------------------
      INTEGER,                INTENT(out):: kb
      REAL(wp), DIMENSION(:), INTENT(in) :: pe3
      REAL(wp),               INTENT(in) :: pD
      !!
      INTEGER  :: jk
      REAL(wp) :: zdepw
      !!----------------------------------------------------------------------
      !!
      zdepw = pe3(1) ; kb = 2
      DO WHILE ( zdepw <  pD)
         zdepw = zdepw + pe3(kb)
         kb = kb + 1
      END DO
      kb = MIN(kb - 1,jpk)
   END SUBROUTINE

   SUBROUTINE icb_utl_zavg(pzavg, pdat, pe3, pD, kb )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE icb_utl_getkb         ***
      !!
      !! ** Purpose :   compute the vertical average of ocean properties affected by icb
      !!
      !!----------------------------------------------------------------------
      INTEGER,                INTENT(in ) :: kb        ! deepest level affected by icb
      REAL(wp), DIMENSION(:), INTENT(in ) :: pe3, pdat ! vertical profile
      REAL(wp),               INTENT(in ) :: pD        ! draft
      REAL(wp),               INTENT(out) :: pzavg     ! z average
      !!----------------------------------------------------------------------
      INTEGER  :: jk
      REAL(wp) :: zdep
      !!----------------------------------------------------------------------
      pzavg = 0.0 ; zdep = 0.0
      DO jk = 1,kb-1
         pzavg = pzavg + pe3(jk)*pdat(jk)
         zdep  = zdep  + pe3(jk)
      END DO
      ! if kb is limited by mbkt  => bottom value is used between bathy and icb tail
      ! if kb not limited by mbkt => ocean value over mask is used (ie 0.0 for u, v)
      pzavg = ( pzavg + (pD - zdep)*pdat(kb)) / pD
   END SUBROUTINE

   SUBROUTINE icb_utl_add( bergvals, ptvals )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE icb_utl_add           ***
      !!
      !! ** Purpose :   add a new berg to the iceberg list
      !!
      !!----------------------------------------------------------------------
      TYPE(iceberg), INTENT(in)           ::   bergvals
      TYPE(point)  , INTENT(in)           ::   ptvals
      !
      TYPE(iceberg), POINTER ::   new => NULL()
      !!----------------------------------------------------------------------
      !
      new => NULL()
      CALL icb_utl_create( new, bergvals, ptvals )
      CALL icb_utl_insert( new )
      new => NULL()     ! Clear new
      !
   END SUBROUTINE icb_utl_add         


   SUBROUTINE icb_utl_create( berg, bergvals, ptvals )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE icb_utl_create  ***
      !!
      !! ** Purpose :   add a new berg to the iceberg list
      !!
      !!----------------------------------------------------------------------
      TYPE(iceberg), INTENT(in) ::   bergvals
      TYPE(point)  , INTENT(in) ::   ptvals
      TYPE(iceberg), POINTER    ::   berg
      !
      TYPE(point)  , POINTER    ::   pt
      INTEGER                   ::   istat
      !!----------------------------------------------------------------------
      !
      IF( ASSOCIATED(berg) )   CALL ctl_stop( 'icebergs, icb_utl_create: berg already associated' )
      ALLOCATE(berg, STAT=istat)
      IF( istat /= 0 ) CALL ctl_stop( 'failed to allocate iceberg' )
      berg%number(:) = bergvals%number(:)
      berg%mass_scaling = bergvals%mass_scaling
      berg%prev => NULL()
      berg%next => NULL()
      !
      ALLOCATE(pt, STAT=istat)
      IF( istat /= 0 ) CALL ctl_stop( 'failed to allocate first iceberg point' )
      pt = ptvals
      berg%current_point => pt
      !
   END SUBROUTINE icb_utl_create


   SUBROUTINE icb_utl_insert( newberg )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_insert  ***
      !!
      !! ** Purpose :   add a new berg to the iceberg list
      !!
      !!----------------------------------------------------------------------
      TYPE(iceberg), POINTER  ::   newberg
      !
      TYPE(iceberg), POINTER  ::   this, prev, last
      !!----------------------------------------------------------------------
      !
      IF( ASSOCIATED( first_berg ) ) THEN
         last => first_berg
         DO WHILE (ASSOCIATED(last%next))
            last => last%next
         ENDDO
         newberg%prev => last
         last%next    => newberg
         last         => newberg
      ELSE                       ! list is empty so create it
         first_berg => newberg
      ENDIF
      !
   END SUBROUTINE icb_utl_insert


   REAL(wp) FUNCTION icb_utl_yearday(kmon, kday, khr, kmin, ksec)
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION icb_utl_yearday  ***
      !!
      !! ** Purpose :   
      !!
      ! sga - improved but still only applies to 365 day year, need to do this properly
      !
      !!gm  all these info are already known in daymod, no???
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)     :: kmon, kday, khr, kmin, ksec
      ! 
      INTEGER, DIMENSION(12)  :: imonths = (/ 0,31,28,31,30,31,30,31,31,30,31,30 /)
      !!----------------------------------------------------------------------
      !
      icb_utl_yearday = REAL( SUM( imonths(1:kmon) ), wp )
      icb_utl_yearday = icb_utl_yearday + REAL(kday-1,wp) + (REAL(khr,wp) + (REAL(kmin,wp) + REAL(ksec,wp)/60.)/60.)/24.
      !
   END FUNCTION icb_utl_yearday

   !!-------------------------------------------------------------------------

   SUBROUTINE icb_utl_delete( first, berg )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_delete  ***
      !!
      !! ** Purpose :   
      !!
      !!----------------------------------------------------------------------
      TYPE(iceberg), POINTER :: first, berg
      !!----------------------------------------------------------------------
      ! Connect neighbors to each other
      IF ( ASSOCIATED(berg%prev) ) THEN
        berg%prev%next => berg%next
      ELSE
        first => berg%next
      ENDIF
      IF (ASSOCIATED(berg%next)) berg%next%prev => berg%prev
      !
      CALL icb_utl_destroy(berg)
      !
   END SUBROUTINE icb_utl_delete


   SUBROUTINE icb_utl_destroy( berg )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_destroy  ***
      !!
      !! ** Purpose :   remove a single iceberg instance
      !!
      !!----------------------------------------------------------------------
      TYPE(iceberg), POINTER :: berg
      !!----------------------------------------------------------------------
      !
      ! Remove any points
      IF( ASSOCIATED( berg%current_point ) )   DEALLOCATE( berg%current_point )
      !
      DEALLOCATE(berg)
      !
   END SUBROUTINE icb_utl_destroy


   SUBROUTINE icb_utl_track( knum, cd_label, kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_track  ***
      !!
      !! ** Purpose :   
      !!
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(nkounts)    :: knum       ! iceberg number
      CHARACTER(len=*)               :: cd_label   ! 
      INTEGER                        :: kt         ! timestep number
      ! 
      TYPE(iceberg), POINTER         :: this
      LOGICAL                        :: match
      INTEGER                        :: k
      !!----------------------------------------------------------------------
      !
      this => first_berg
      DO WHILE( ASSOCIATED(this) )
         match = .TRUE.
         DO k = 1, nkounts
            IF( this%number(k) /= knum(k) ) match = .FALSE.
         END DO
         IF( match )   CALL icb_utl_print_berg(this, kt)
         this => this%next
      END DO
      !
   END SUBROUTINE icb_utl_track


   SUBROUTINE icb_utl_print_berg( berg, kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_print_berg  ***
      !!
      !! ** Purpose :   print one
      !!
      !!----------------------------------------------------------------------
      TYPE(iceberg), POINTER :: berg
      TYPE(point)  , POINTER :: pt
      INTEGER                :: kt      ! timestep number
      !!----------------------------------------------------------------------
      !
      IF (nn_verbose_level == 0) RETURN
      pt => berg%current_point
      WRITE(numicb, 9200) kt, berg%number(1), &
                   pt%xi, pt%yj, pt%lon, pt%lat, pt%uvel, pt%vvel,  &
                   pt%ssu, pt%ssv, pt%ua, pt%va, pt%ui, pt%vi
      CALL flush( numicb )
 9200 FORMAT(5x,i5,2x,i10,6(2x,2f10.4))
      !
   END SUBROUTINE icb_utl_print_berg


   SUBROUTINE icb_utl_print( cd_label, kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_print  ***
      !!
      !! ** Purpose :   print many
      !!
      !!----------------------------------------------------------------------
      CHARACTER(len=*)       :: cd_label
      INTEGER                :: kt             ! timestep number
      ! 
      INTEGER                :: ibergs, inbergs
      TYPE(iceberg), POINTER :: this
      !!----------------------------------------------------------------------
      !
      IF (nn_verbose_level == 0) RETURN
      this => first_berg
      IF( ASSOCIATED(this) ) THEN
         WRITE(numicb,'(a," pe=(",i3,")")' ) cd_label, narea
         WRITE(numicb,'(a8,4x,a6,12x,a5,15x,a7,19x,a3,17x,a5,17x,a5,17x,a5)' )   &
            &         'timestep', 'number', 'xi,yj','lon,lat','u,v','ssu,ssv','ua,va','ui,vi'
      ENDIF
      DO WHILE( ASSOCIATED(this) )
        CALL icb_utl_print_berg(this, kt)
        this => this%next
      END DO
      ibergs = icb_utl_count()
      inbergs = ibergs
      CALL mpp_sum('icbutl', inbergs)
      IF( ibergs > 0 )   WRITE(numicb,'(a," there are",i5," bergs out of",i6," on PE ",i4)')   &
         &                                  cd_label, ibergs, inbergs, narea
      !
   END SUBROUTINE icb_utl_print


   SUBROUTINE icb_utl_incr()
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_utl_incr  ***
      !!
      !! ** Purpose :   
      !!
      ! Small routine for coping with very large integer values labelling icebergs
      ! num_bergs is a array of integers
      ! the first member is incremented in steps of jpnij starting from narea
      ! this means each iceberg is labelled with a unique number
      ! when this gets to the maximum allowed integer the second and subsequent members are 
      ! used to count how many times the member before cycles
      !!----------------------------------------------------------------------
      INTEGER ::   ii, ibig
      !!----------------------------------------------------------------------

      ibig = HUGE(num_bergs(1))
      IF( ibig-jpnij < num_bergs(1) ) THEN
         num_bergs(1) = narea
         DO ii = 2,nkounts
            IF( num_bergs(ii) == ibig ) THEN
               num_bergs(ii) = 0
               IF( ii == nkounts ) CALL ctl_stop('Sorry, run out of iceberg number space')
            ELSE
               num_bergs(ii) = num_bergs(ii) + 1
               EXIT
            ENDIF
         END DO
      ELSE
         num_bergs(1) = num_bergs(1) + jpnij
      ENDIF
      !
   END SUBROUTINE icb_utl_incr


   INTEGER FUNCTION icb_utl_count()
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION icb_utl_count  ***
      !!
      !! ** Purpose :   
      !!----------------------------------------------------------------------
      TYPE(iceberg), POINTER :: this
      !!----------------------------------------------------------------------
      !
      icb_utl_count = 0
      this => first_berg
      DO WHILE( ASSOCIATED(this) )
         icb_utl_count = icb_utl_count+1
         this => this%next
      END DO
      !
   END FUNCTION icb_utl_count


   REAL(wp) FUNCTION icb_utl_mass( first, justbits, justbergs )
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION icb_utl_mass  ***
      !!
      !! ** Purpose :   compute the mass all iceberg, all berg bits or all bergs.
      !!----------------------------------------------------------------------
      TYPE(iceberg)      , POINTER  ::   first
      TYPE(point)        , POINTER  ::   pt
      LOGICAL, INTENT(in), OPTIONAL ::   justbits, justbergs
      !
      TYPE(iceberg), POINTER ::   this
      !!----------------------------------------------------------------------
      icb_utl_mass = 0._wp
      this => first
      !
      IF( PRESENT( justbergs  ) ) THEN
         DO WHILE( ASSOCIATED( this ) )
            pt => this%current_point
            icb_utl_mass = icb_utl_mass + pt%mass         * this%mass_scaling
            this => this%next
         END DO
      ELSEIF( PRESENT(justbits) ) THEN
         DO WHILE( ASSOCIATED( this ) )
            pt => this%current_point
            icb_utl_mass = icb_utl_mass + pt%mass_of_bits * this%mass_scaling
            this => this%next
         END DO
      ELSE
         DO WHILE( ASSOCIATED( this ) )
            pt => this%current_point
            icb_utl_mass = icb_utl_mass + ( pt%mass + pt%mass_of_bits ) * this%mass_scaling
            this => this%next
         END DO
      ENDIF
      !
   END FUNCTION icb_utl_mass


   REAL(wp) FUNCTION icb_utl_heat( first, justbits, justbergs )
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION icb_utl_heat  ***
      !!
      !! ** Purpose :   compute the heat in all iceberg, all bergies or all bergs.
      !!----------------------------------------------------------------------
      TYPE(iceberg)      , POINTER  ::   first
      LOGICAL, INTENT(in), OPTIONAL ::   justbits, justbergs
      !
      TYPE(iceberg)      , POINTER  ::   this
      TYPE(point)        , POINTER  ::   pt
      !!----------------------------------------------------------------------
      icb_utl_heat = 0._wp
      this => first
      !
      IF( PRESENT( justbergs  ) ) THEN
         DO WHILE( ASSOCIATED( this ) )
            pt => this%current_point
            icb_utl_heat = icb_utl_heat + pt%mass         * this%mass_scaling * pt%heat_density
            this => this%next
         END DO
      ELSEIF( PRESENT(justbits) ) THEN
         DO WHILE( ASSOCIATED( this ) )
            pt => this%current_point
            icb_utl_heat = icb_utl_heat + pt%mass_of_bits * this%mass_scaling * pt%heat_density
            this => this%next
         END DO
      ELSE
         DO WHILE( ASSOCIATED( this ) )
            pt => this%current_point
            icb_utl_heat = icb_utl_heat + ( pt%mass + pt%mass_of_bits ) * this%mass_scaling * pt%heat_density
            this => this%next
         END DO
      ENDIF
      !
   END FUNCTION icb_utl_heat

   SUBROUTINE test_icb_utl_getkb
      INTEGER :: ikb
      REAL(wp) :: zD, zout
      REAL(wp), DIMENSION(jpk) :: ze3, zin
      WRITE(numout,*) 'Test icb_utl_getkb : '
      zD = 0.0 ; ze3= 20.0
      WRITE(numout,*) 'INPUT : zD = ',zD,' ze3 = ',ze3(1)
      CALL icb_utl_getkb(ikb, ze3, zD)
      WRITE(numout,*) 'OUTPUT : kb = ',ikb

      zD = 8000000.0 ; ze3= 20.0
      WRITE(numout,*) 'INPUT : zD = ',zD,' ze3 = ',ze3(1)
      CALL icb_utl_getkb(ikb, ze3, zD)
      WRITE(numout,*) 'OUTPUT : kb = ',ikb

      zD = 80.0 ; ze3= 20.0
      WRITE(numout,*) 'INPUT : zD = ',zD,' ze3 = ',ze3(1)
      CALL icb_utl_getkb(ikb, ze3, zD)
      WRITE(numout,*) 'OUTPUT : kb = ',ikb

      zD = 85.0 ; ze3= 20.0
      WRITE(numout,*) 'INPUT : zD = ',zD,' ze3 = ',ze3(1)
      CALL icb_utl_getkb(ikb, ze3, zD)
      WRITE(numout,*) 'OUTPUT : kb = ',ikb

      zD = 75.0 ; ze3= 20.0
      WRITE(numout,*) 'INPUT : zD = ',zD,' ze3 = ',ze3(1)
      CALL icb_utl_getkb(ikb, ze3, zD)
      WRITE(numout,*) 'OUTPUT : kb = ',ikb

      WRITE(numout,*) '=================================='
      WRITE(numout,*) 'Test icb_utl_zavg'
      zD = 0.0 ; ze3= 20.0 ; zin=1.0
      CALL icb_utl_getkb(ikb, ze3, zD)
      CALL icb_utl_zavg(zout, zin, ze3, zD, ikb)
      WRITE(numout,*) 'INPUT  : zD = ',zD,' ze3 = ',ze3(1),' zin = ', zin, ' ikb = ',ikb
      WRITE(numout,*) 'OUTPUT : zout = ',zout

      zD = 50.0 ; ze3= 20.0 ; zin=1.0; zin(3:jpk) = 0.0
      CALL icb_utl_getkb(ikb, ze3, zD)
      CALL icb_utl_zavg(zout, zin, ze3, zD, ikb)
      WRITE(numout,*) 'INPUT  : zD = ',zD,' ze3 = ',ze3(1),' zin = ', zin, ' ikb = ',ikb
      WRITE(numout,*) 'OUTPUT : zout = ',zout
      CALL FLUSH(numout)

      zD = 80.0 ; ze3= 20.0 ; zin=1.0; zin(3:jpk) = 0.0
      CALL icb_utl_getkb(ikb, ze3, zD)
      CALL icb_utl_zavg(zout, zin, ze3, zD, ikb)
      WRITE(numout,*) 'INPUT  : zD = ',zD,' ze3 = ',ze3(1),' zin = ', zin, ' ikb = ',ikb
      WRITE(numout,*) 'OUTPUT : zout = ',zout

      zD = 80 ; ze3= 20.0 ; zin=1.0 ; zin(3:jpk) = 0.0
      CALL icb_utl_getkb(ikb, ze3, zD)
      ikb = 2
      CALL icb_utl_zavg(zout, zin, ze3, zD, ikb)
      WRITE(numout,*) 'INPUT  : zD = ',zD,' ze3 = ',ze3(1),' zin = ', zin, ' ikb = ',ikb
      WRITE(numout,*) 'OUTPUT : zout = ',zout

      CALL FLUSH(numout)

   END SUBROUTINE test_icb_utl_getkb

   !!======================================================================
END MODULE icbutl
