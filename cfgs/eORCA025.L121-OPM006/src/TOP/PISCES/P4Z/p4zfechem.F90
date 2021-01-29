MODULE p4zfechem
   !!======================================================================
   !!                         ***  MODULE p4zfechem  ***
   !! TOP :   PISCES Compute iron chemistry and scavenging
   !!======================================================================
   !! History :   3.5  !  2012-07 (O. Aumont, A. Tagliabue, C. Ethe) Original code
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p4z_fechem       : Compute remineralization/scavenging of iron
   !!   p4z_fechem_init  : Initialisation of parameters for remineralisation
   !!   p4z_fechem_alloc : Allocate remineralisation variables
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p4zche          ! chemical model
   USE p4zsbc           ! Boundary conditions from sediments
   USE prtctl_trc      ! print control for debugging
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_fechem        ! called in p4zbio.F90
   PUBLIC   p4z_fechem_init   ! called in trcsms_pisces.F90

   LOGICAL          ::   ln_ligvar    !: boolean for variable ligand concentration following Tagliabue and voelker
   REAL(wp), PUBLIC ::   xlam1        !: scavenging rate of Iron 
   REAL(wp), PUBLIC ::   xlamdust     !: scavenging rate of Iron by dust 
   REAL(wp), PUBLIC ::   ligand       !: ligand concentration in the ocean 
   REAL(wp), PUBLIC ::   kfep         !: rate constant for nanoparticle formation

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zfechem.F90 13284 2020-07-09 15:12:23Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_fechem( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_fechem  ***
      !!
      !! ** Purpose :   Compute remineralization/scavenging of iron
      !!
      !! ** Method  :   A simple chemistry model of iron from Aumont and Bopp (2006)
      !!                based on one ligand and one inorganic form
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step
      !
      INTEGER  ::   ji, jj, jk, jic, jn
      REAL(wp) ::   zdep, zlam1a, zlam1b, zlamfac
      REAL(wp) ::   zkeq, zfeequi, zfesatur, zfecoll, fe3sol
      REAL(wp) ::   zdenom1, zscave, zaggdfea, zaggdfeb, zcoag
      REAL(wp) ::   ztrc, zdust
      REAL(wp) ::   zdenom2
      REAL(wp) ::   zzFeL1, zzFeL2, zzFe2, zzFeP, zzFe3, zzstrn2
      REAL(wp) ::   zrum, zcodel, zargu, zlight
      REAL(wp) ::   zkox, zkph1, zkph2, zph, zionic, ztligand
      REAL(wp) ::   za, zb, zc, zkappa1, zkappa2, za0, za1, za2
      REAL(wp) ::   zxs, zfunc, zp, zq, zd, zr, zphi, zfff, zp3, zq2
      REAL(wp) ::   ztfe, zoxy, zhplus, zxlam
      REAL(wp) ::   zaggliga, zaggligb
      REAL(wp) ::   dissol, zligco
      REAL(wp) :: zrfact2
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zTL1, zFe3, ztotlig, precip, zFeL1
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zcoll3d, zscav3d, zlcoll3d
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_fechem')
      !

      ! Total ligand concentration : Ligands can be chosen to be constant or variable
      ! Parameterization from Tagliabue and Voelker (2011)
      ! -------------------------------------------------
      IF( ln_ligvar ) THEN
         ztotlig(:,:,:) =  0.09 * trb(:,:,:,jpdoc) * 1E6 + ligand * 1E9
         ztotlig(:,:,:) =  MIN( ztotlig(:,:,:), 10. )
      ELSE
        IF( ln_ligand ) THEN  ;   ztotlig(:,:,:) = trb(:,:,:,jplgw) * 1E9
        ELSE                  ;   ztotlig(:,:,:) = ligand * 1E9
        ENDIF
      ENDIF

      ! ------------------------------------------------------------
      !  from Aumont and Bopp (2006)
      ! This model is based on one ligand and Fe' 
      ! Chemistry is supposed to be fast enough to be at equilibrium
      ! ------------------------------------------------------------
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zTL1(ji,jj,jk)  = ztotlig(ji,jj,jk)
               zkeq            = fekeq(ji,jj,jk)
               zfesatur        = zTL1(ji,jj,jk) * 1E-9
               ztfe            = trb(ji,jj,jk,jpfer) 
               ! Fe' is the root of a 2nd order polynom
               zFe3 (ji,jj,jk) = ( -( 1. + zfesatur * zkeq - zkeq * ztfe )               &
                  &              + SQRT( ( 1. + zfesatur * zkeq - zkeq * ztfe )**2       &
                  &              + 4. * ztfe * zkeq) ) / ( 2. * zkeq )
               zFe3 (ji,jj,jk) = zFe3(ji,jj,jk) * 1E9
               zFeL1(ji,jj,jk) = MAX( 0., trb(ji,jj,jk,jpfer) * 1E9 - zFe3(ji,jj,jk) )
           END DO
         END DO
      END DO
         !

      zdust = 0.         ! if no dust available
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               ! Scavenging rate of iron. This scavenging rate depends on the load of particles of sea water. 
               ! This parameterization assumes a simple second order kinetics (k[Particles][Fe]).
               ! Scavenging onto dust is also included as evidenced from the DUNE experiments.
               ! --------------------------------------------------------------------------------------
               zhplus  = max( rtrn, hi(ji,jj,jk) )
               fe3sol  = fesol(ji,jj,jk,1) * ( zhplus**3 + fesol(ji,jj,jk,2) * zhplus**2  &
               &         + fesol(ji,jj,jk,3) * zhplus + fesol(ji,jj,jk,4)     &
               &         + fesol(ji,jj,jk,5) / zhplus )
               !
               zfeequi = zFe3(ji,jj,jk) * 1E-9
               zfecoll = 0.5 * zFeL1(ji,jj,jk) * 1E-9
               ! precipitation of Fe3+, creation of nanoparticles
               precip(ji,jj,jk) = MAX( 0., ( zFe3(ji,jj,jk) * 1E-9 - fe3sol ) ) * kfep * xstep
               !
               ztrc   = ( trb(ji,jj,jk,jppoc) + trb(ji,jj,jk,jpgoc) + trb(ji,jj,jk,jpcal) + trb(ji,jj,jk,jpgsi) ) * 1.e6 
               IF( ln_dust )  zdust  = dust(ji,jj) / ( wdust / rday ) * tmask(ji,jj,jk) &
               &  * EXP( -gdept_n(ji,jj,jk) / 540. )
               IF (ln_ligand) THEN
                  zxlam  = xlam1 * MAX( 1.E-3, EXP(-2 * etot(ji,jj,jk) / 10. ) * (1. - EXP(-2 * trb(ji,jj,jk,jpoxy) / 100.E-6 ) ))
               ELSE
                  zxlam  = xlam1 * 1.0
               ENDIF
               zlam1b = 3.e-5 + xlamdust * zdust + zxlam * ztrc
               zscave = zfeequi * zlam1b * xstep

               ! Compute the different ratios for scavenging of iron
               ! to later allocate scavenged iron to the different organic pools
               ! ---------------------------------------------------------
               zdenom1 = zxlam * trb(ji,jj,jk,jppoc) / zlam1b
               zdenom2 = zxlam * trb(ji,jj,jk,jpgoc) / zlam1b

               !  Increased scavenging for very high iron concentrations found near the coasts 
               !  due to increased lithogenic particles and let say it is unknown processes (precipitation, ...)
               !  -----------------------------------------------------------
               zlamfac = MAX( 0.e0, ( gphit(ji,jj) + 55.) / 30. )
               zlamfac = MIN( 1.  , zlamfac )
               zdep    = MIN( 1., 1000. / gdept_n(ji,jj,jk) )
               zcoag   = 1E-4 * ( 1. - zlamfac ) * zdep * xstep * trb(ji,jj,jk,jpfer)

               !  Compute the coagulation of colloidal iron. This parameterization 
               !  could be thought as an equivalent of colloidal pumping.
               !  It requires certainly some more work as it is very poorly constrained.
               !  ----------------------------------------------------------------
               zlam1a   = ( 0.369  * 0.3 * trb(ji,jj,jk,jpdoc) + 102.4  * trb(ji,jj,jk,jppoc) ) * xdiss(ji,jj,jk)    &
                   &      + ( 114.   * 0.3 * trb(ji,jj,jk,jpdoc) )
               zaggdfea = zlam1a * xstep * zfecoll
               !
               zlam1b   = 3.53E3 * trb(ji,jj,jk,jpgoc) * xdiss(ji,jj,jk)
               zaggdfeb = zlam1b * xstep * zfecoll
               !
               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) - zscave - zaggdfea - zaggdfeb &
               &                     - zcoag - precip(ji,jj,jk)
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zscave * zdenom1 + zaggdfea
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zscave * zdenom2 + zaggdfeb
               zscav3d(ji,jj,jk)   = zscave
               zcoll3d(ji,jj,jk)   = zaggdfea + zaggdfeb
               !
            END DO
         END DO
      END DO
      !
      !  Define the bioavailable fraction of iron
      !  ----------------------------------------
      biron(:,:,:) = trb(:,:,:,jpfer) 
      !
      IF( ln_ligand ) THEN
         !
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zlam1a   = ( 0.369  * 0.3 * trb(ji,jj,jk,jpdoc) + 102.4  * trb(ji,jj,jk,jppoc) ) * xdiss(ji,jj,jk)    &
                      &    + ( 114.   * 0.3 * trb(ji,jj,jk,jpdoc) )
                  !
                  zlam1b   = 3.53E3 *   trb(ji,jj,jk,jpgoc) * xdiss(ji,jj,jk)
                  zligco   = 0.5 * trn(ji,jj,jk,jplgw)
                  zaggliga = zlam1a * xstep * zligco
                  zaggligb = zlam1b * xstep * zligco
                  tra(ji,jj,jk,jplgw) = tra(ji,jj,jk,jplgw) - zaggliga - zaggligb
                  zlcoll3d(ji,jj,jk)  = zaggliga + zaggligb
               END DO
            END DO
         END DO
         !
         plig(:,:,:) =  MAX( 0., ( ( zFeL1(:,:,:) * 1E-9 ) / ( trb(:,:,:,jpfer) +rtrn ) ) )
         !
      ENDIF
      !  Output of some diagnostics variables
      !     ---------------------------------
      IF( lk_iomput ) THEN
         IF( knt == nrdttrc ) THEN
            zrfact2 = 1.e3 * rfact2r  ! conversion from mol/L/timestep into mol/m3/s
            IF( iom_use("Fe3")  )  THEN
               zFe3(:,:,jpk) = 0.  ;  CALL iom_put("Fe3" , zFe3(:,:,:) * tmask(:,:,:) )   ! Fe3+
            ENDIF
            IF( iom_use("FeL1") )  THEN
              zFeL1(:,:,jpk) = 0.  ;  CALL iom_put("FeL1", zFeL1(:,:,:) * tmask(:,:,:) )   ! FeL1
            ENDIF
            IF( iom_use("TL1")  )  THEN
              zTL1(:,:,jpk) = 0.   ;  CALL iom_put("TL1" , zTL1(:,:,:) * tmask(:,:,:) )   ! TL1
            ENDIF
            CALL iom_put("Totlig" , ztotlig(:,:,:)       * tmask(:,:,:) )   ! TL
            CALL iom_put("Biron"  , biron  (:,:,:)  * 1e9 * tmask(:,:,:) )   ! biron
            IF( iom_use("FESCAV") )  THEN
               zscav3d (:,:,jpk) = 0.  ;  CALL iom_put("FESCAV" , zscav3d(:,:,:)  * 1e9 * tmask(:,:,:) * zrfact2 )
            ENDIF
            IF( iom_use("FECOLL") ) THEN
               zcoll3d (:,:,jpk) = 0.  ;   CALL iom_put("FECOLL" , zcoll3d(:,:,:)  * 1e9 * tmask(:,:,:) * zrfact2 )
            ENDIF
            IF( iom_use("LGWCOLL")) THEN
               zlcoll3d(:,:,jpk) = 0.  ;  CALL iom_put("LGWCOLL", zlcoll3d(:,:,:) * 1e9 * tmask(:,:,:) * zrfact2 )
            ENDIF
         ENDIF
      ENDIF

      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('fechem')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_fechem')
      !
   END SUBROUTINE p4z_fechem


   SUBROUTINE p4z_fechem_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_fechem_init  ***
      !!
      !! ** Purpose :   Initialization of iron chemistry parameters
      !!
      !! ** Method  :   Read the nampisfer namelist and check the parameters
      !!      called at the first timestep
      !!
      !! ** input   :   Namelist nampisfer
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer 
      !!
      NAMELIST/nampisfer/ ln_ligvar, xlam1, xlamdust, ligand, kfep 
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_rem_init : Initialization of iron chemistry parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      REWIND( numnatp_ref )
      READ  ( numnatp_ref, nampisfer, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampisfer in reference namelist' )

      REWIND( numnatp_cfg )
      READ  ( numnatp_cfg, nampisfer, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampisfer in configuration namelist' )
      IF(lwm) WRITE( numonp, nampisfer )

      IF(lwp) THEN                     ! control print
         WRITE(numout,*) '   Namelist : nampisfer'
         WRITE(numout,*) '      variable concentration of ligand          ln_ligvar    =', ln_ligvar
         WRITE(numout,*) '      scavenging rate of Iron                   xlam1        =', xlam1
         WRITE(numout,*) '      scavenging rate of Iron by dust           xlamdust     =', xlamdust
         WRITE(numout,*) '      ligand concentration in the ocean         ligand       =', ligand
         WRITE(numout,*) '      rate constant for nanoparticle formation  kfep         =', kfep
      ENDIF
      ! 
   END SUBROUTINE p4z_fechem_init
   
   !!======================================================================
END MODULE p4zfechem
