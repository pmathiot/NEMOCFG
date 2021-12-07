MODULE isfcav
   !!======================================================================
   !!                       ***  MODULE  isfcav  ***
   !! Ice shelf cavity module :  update ice shelf melting under ice
   !!                            shelf
   !!======================================================================
   !! History :  3.2  !  2011-02  (C.Harris  ) Original code isf cav
   !!            3.4  !  2013-03  (P. Mathiot) Merging + parametrization
   !!            4.1  !  2019-09  (P. Mathiot) Split ice shelf cavity and ice shelf parametrisation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isf_cav       : update ice shelf melting under ice shelf
   !!----------------------------------------------------------------------
   USE isf_oce        ! ice shelf public variables
   !
   USE isfrst   , ONLY: isfrst_write, isfrst_read ! ice shelf restart read/write subroutine
   USE isfutils , ONLY: debug          ! ice shelf debug subroutine
   USE isftbl   , ONLY: isf_tbl        ! ice shelf top boundary layer properties subroutine
   USE isfcavmlt, ONLY: isfcav_mlt     ! ice shelf melt formulation subroutine
   USE isfcavgam, ONLY: isfcav_gammats ! ice shelf melt exchange coeficient subroutine
   USE isfdiags , ONLY: isf_diags_flx  ! ice shelf diags subroutine
   !
   USE oce      , ONLY: tsn                    ! ocean tracers
   USE par_oce  , ONLY: jpi,jpj                ! ocean space and time domain
   USE phycst   , ONLY: grav,rau0,r1_rau0_rcp  ! physical constants
   USE eosbn2   , ONLY: ln_teos10               ! use ln_teos10 or not
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE fldread        ! read input field at current time step
   USE lbclnk         ! lbclnk

   IMPLICIT NONE

   PRIVATE

   PUBLIC   isf_cav, isf_cav_init ! routine called in isfmlt

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE isf_cav( kt, ptsc, pqfwf )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE isf_cav  ***
      !!
      !! ** Purpose :   handle surface boundary condition under ice shelf
      !!
      !! ** Method  :   based on Mathiot et al. (2017)
      !!
      !! ** Action  :   - compute geometry of the Losch top bournary layer (see Losch et al. 2008)
      !!                - depending on the chooses option
      !!                   - compute temperature/salt in the tbl
      !!                   - compute exchange coeficient
      !!                   - compute heat and fwf fluxes
      !!                   - output
      !!---------------------------------------------------------------------
      !!-------------------------- OUT --------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(inout) :: pqfwf  ! ice shelf melt (>0 out)
      REAL(wp), DIMENSION(jpi,jpj,jpts), INTENT(inout) :: ptsc   ! T & S ice shelf cavity contents
      !!-------------------------- IN  --------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !!---------------------------------------------------------------------
      LOGICAL :: lit
      INTEGER :: nit
      REAL(wp) :: zerr
      REAL(wp), DIMENSION(jpi,jpj) :: zqlat, zqoce, zqhc, zqh  ! heat fluxes
      REAL(wp), DIMENSION(jpi,jpj) :: zqoce_b                  !
      REAL(wp), DIMENSION(jpi,jpj) :: zgammat, zgammas         ! exchange coeficient
      REAL(wp), DIMENSION(jpi,jpj) :: zttbl, zstbl             ! temp. and sal. in top boundary layer
      !!---------------------------------------------------------------------
      !
      ! compute T/S/U/V for the top boundary layer
      CALL isf_tbl(tsn(:,:,:,jp_tem), zttbl(:,:),'T', misfkt_cav, rhisf_tbl_cav, misfkb_cav, rfrac_tbl_cav )
      CALL isf_tbl(tsn(:,:,:,jp_sal), zstbl(:,:),'T', misfkt_cav, rhisf_tbl_cav, misfkb_cav, rfrac_tbl_cav )
      ! 
      ! output T/S/U/V for the top boundary layer
      CALL iom_put('ttbl_cav',zttbl(:,:) * mskisf_cav(:,:))
      CALL iom_put('stbl'    ,zstbl(:,:) * mskisf_cav(:,:))
      !
      ! initialisation
      IF (TRIM(cn_gammablk) == 'vel_stab' ) zqoce_b (:,:) = ptsc(:,:,jp_tem) * rau0_rcp ! last time step total heat fluxes (to speed up convergence)
      !
      ! compute ice shelf melting
      nit = 1 ; lit = .TRUE.
      DO WHILE ( lit )    ! maybe just a constant number of iteration as in blk_core is fine
         !
         ! compute gammat everywhere (2d)
         ! useless if melt specified
         IF ( TRIM(cn_isfcav_mlt) .NE. 'spe' ) THEN
            CALL isfcav_gammats( zttbl, zstbl, zqoce  , pqfwf,  &
               &                               zgammat, zgammas )
         END IF
         !   
         ! compute tfrz, latent heat and melt (2d)
         CALL isfcav_mlt(kt, zgammat, zgammas, zttbl, zstbl, &
            &                         zqhc   , zqoce, pqfwf  )
         !
         ! define if we need to iterate
         SELECT CASE ( cn_gammablk )
         CASE ( 'spe','vel' )
            ! no convergence needed
            lit = .FALSE.
         CASE ( 'vel_stab' )
            ! compute error between 2 iterations
            zerr = MAXVAL(ABS(zqoce(:,:) - zqoce_b(:,:)))
            !
            ! define if iteration needed
            IF (nit >= 100) THEN              ! too much iteration
               CALL ctl_stop( 'STOP', 'isf_cav: vel_stab gamma formulation had too many iterations ...' )
            ELSE IF ( zerr <= 0.01_wp ) THEN  ! convergence is achieve
               lit = .FALSE.
            ELSE                              ! converge is not yet achieve
               nit = nit + 1
               zqoce_b(:,:) = zqoce(:,:)
            END IF
         END SELECT
         !
      END DO
      !
      ! compute heat and water flux ( > 0 out )
      pqfwf(:,:) = pqfwf(:,:) * mskisf_cav(:,:)
      zqoce(:,:) = zqoce(:,:) * mskisf_cav(:,:)
      zqhc (:,:) = zqhc(:,:)  * mskisf_cav(:,:)
      !
      ! compute heat content flux ( > 0 out )
      zqlat(:,:) = - pqfwf(:,:) * rLfusisf    ! 2d latent heat flux (W/m2)
      !
      ! total heat flux ( >0 out )
      zqh(:,:) = ( zqhc (:,:) + zqoce(:,:) )
      !
      ! lbclnk on melt
      CALL lbc_lnk_multi( 'isfmlt', zqh, 'T', 1., pqfwf, 'T', 1.)
      !
      ! output fluxes
      CALL isf_diags_flx( misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, 'cav', pqfwf, zqoce, zqlat, zqhc)
      !
      ! set temperature content
      ptsc(:,:,jp_tem) = - zqh(:,:) * r1_rau0_rcp
      !
      ! write restart variables (qoceisf, qhcisf, fwfisf for now and before)
      IF (lrst_oce) CALL isfrst_write(kt, 'cav', ptsc, pqfwf)
      !
      IF ( ln_isfdebug ) THEN
         CALL debug('isf_cav: ptsc T',ptsc(:,:,1))
         CALL debug('isf_cav: ptsc S',ptsc(:,:,2))
         CALL debug('isf_cav: pqfwf fwf',pqfwf(:,:))
      END IF
      !
   END SUBROUTINE isf_cav

   SUBROUTINE isf_cav_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_cav_init ***
      !!
      !! ** Purpose : initialisation of variable needed to compute melt under an ice shelf
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      !
      !==============
      ! 0: allocation
      !==============
      !
      CALL isf_alloc_cav()
      !
      !==================
      ! 1: initialisation
      !==================
      !
      ! top and bottom level of the 'top boundary layer'
      misfkt_cav(:,:)    = mikt(:,:) ; misfkb_cav(:,:)    = 1
      !
      ! thickness of 'tbl' and fraction of bottom cell affected by 'tbl'
      rhisf_tbl_cav(:,:) = 0.0_wp    ; rfrac_tbl_cav(:,:) = 0.0_wp
      !
      ! cavity mask
      mskisf_cav(:,:)    = (1._wp - tmask(:,:,1)) * ssmask(:,:)
      !
      !================
      ! 2: read restart
      !================
      !
      ! read cav variable from restart
      IF ( ln_rstart ) CALL isfrst_read('cav', risf_cav_tsc, fwfisf_cav, risf_cav_tsc_b, fwfisf_cav_b)
      !
      !==========================================
      ! 3: specific allocation and initialisation (depending of scheme choice)
      !==========================================
      !
      SELECT CASE ( TRIM(cn_isfcav_mlt) )
      CASE( 'spe' )

         ALLOCATE( sf_isfcav_fwf(1), STAT=ierr )
         ALLOCATE( sf_isfcav_fwf(1)%fnow(jpi,jpj,1), sf_isfcav_fwf(1)%fdta(jpi,jpj,1,2) )
         CALL fld_fill( sf_isfcav_fwf, (/ sn_isfcav_fwf /), cn_isfdir, 'isf_cav_init', 'read fresh water flux isf data', 'namisf' )

         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '  ==>> The ice shelf melt inside the cavity is read from forcing files'

      CASE( '2eq' )
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '  ==>> The original ISOMIP melt formulation is used to compute melt under the ice shelves'

      CASE( '3eq' )
         ! coeficient for linearisation of potential tfreez
         ! Crude approximation for pressure (but commonly used)
         IF ( ln_teos10 ) THEN   ! linearisation from Jourdain et al. (2017)
            risf_lamb1 =-0.0564_wp
            risf_lamb2 = 0.0773_wp
            risf_lamb3 =-7.8633e-8 * grav * rau0
         ELSE                  ! linearisation from table 4 (Asay-Davis et al., 2015)
            risf_lamb1 =-0.0573_wp
            risf_lamb2 = 0.0832_wp
            risf_lamb3 =-7.5300e-8 * grav * rau0
         ENDIF

         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '  ==>> The 3 equations melt formulation is used to compute melt under the ice shelves'

      CASE DEFAULT
         CALL ctl_stop(' cn_isfcav_mlt method unknown (spe, 2eq, 3eq), check namelist')
      END SELECT
      !
   END SUBROUTINE isf_cav_init

END MODULE isfcav
