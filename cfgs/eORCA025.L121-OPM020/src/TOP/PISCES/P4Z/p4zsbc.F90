MODULE p4zsbc
   !!======================================================================
   !!                         ***  MODULE p4sbc  ***
   !! TOP :   PISCES surface boundary conditions of external inputs of nutrients
   !!======================================================================
   !! History :   3.5  !  2012-07 (O. Aumont, C. Ethe) Original code
   !!----------------------------------------------------------------------
   !!   p4z_sbc        :  Read and interpolate time-varying nutrients fluxes
   !!   p4z_sbc_init   :  Initialization of p4z_sbc
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE iom             !  I/O manager
   USE fldread         !  time interpolation

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sbc
   PUBLIC   p4z_sbc_init   

   LOGICAL , PUBLIC ::   ln_dust      !: boolean for dust input from the atmosphere
   LOGICAL , PUBLIC ::   ln_solub     !: boolean for variable solubility of atmospheric iron
   LOGICAL , PUBLIC ::   ln_river     !: boolean for river input of nutrients
   LOGICAL , PUBLIC ::   ln_ndepo     !: boolean for atmospheric deposition of N
   LOGICAL , PUBLIC ::   ln_ironsed   !: boolean for Fe input from sediments
   LOGICAL , PUBLIC ::   ln_hydrofe   !: boolean for Fe input from hydrothermal vents
   REAL(wp), PUBLIC ::   sedfeinput   !: Coastal release of Iron
   REAL(wp), PUBLIC ::   dustsolub    !: Solubility of the dust
   REAL(wp), PUBLIC ::   mfrac        !: Mineral Content of the dust
   REAL(wp), PUBLIC ::   icefeinput   !: Iron concentration in sea ice
   REAL(wp), PUBLIC ::   wdust        !: Sinking speed of the dust 
   REAL(wp), PUBLIC ::   nitrfix      !: Nitrogen fixation rate   
   REAL(wp), PUBLIC ::   diazolight   !: Nitrogen fixation sensitivty to light 
   REAL(wp), PUBLIC ::   concfediaz   !: Fe half-saturation Cste for diazotrophs 
   REAL(wp)         ::   hratio       !: Fe:3He ratio assumed for vent iron supply
   REAL(wp)         ::   distcoast    !: Distance off the coast for Iron from sediments
   REAL(wp), PUBLIC ::   lgw_rath     !: Weak ligand ratio from hydro sources

   LOGICAL , PUBLIC ::   ll_sbc
   LOGICAL          ::   ll_solub

   INTEGER , PARAMETER  :: jpriv  = 7   !: Maximum number of river input fields
   INTEGER , PARAMETER  :: jr_dic = 1   !: index of dissolved inorganic carbon
   INTEGER , PARAMETER  :: jr_doc = 2   !: index of dissolved organic carbon
   INTEGER , PARAMETER  :: jr_din = 3   !: index of dissolved inorganic nitrogen
   INTEGER , PARAMETER  :: jr_don = 4   !: index of dissolved organic nitrogen
   INTEGER , PARAMETER  :: jr_dip = 5   !: index of dissolved inorganic phosporus
   INTEGER , PARAMETER  :: jr_dop = 6   !: index of dissolved organic phosphorus
   INTEGER , PARAMETER  :: jr_dsi = 7   !: index of dissolved silicate

   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_dust      ! structure of input dust
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_solub     ! structure of input dust
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_river     ! structure of input riverdic
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_ndepo     ! structure of input nitrogen deposition
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_ironsed   ! structure of input iron from sediment
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_hydrofe   ! structure of input iron from hydrothermal vents

   INTEGER , PARAMETER ::   nbtimes = 365                          ! maximum number of times record in a file
   INTEGER             ::   ntimes_dust, ntimes_riv, ntimes_ndep   ! number of time steps in a file
   INTEGER             ::   ntimes_solub, ntimes_hydro             ! number of time steps in a file

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dust  , solub    !: dust fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rivdic, rivalk   !: river input fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rivdin, rivdip   !: river input fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rivdon, rivdop   !: river input fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rivdoc           !: river input fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rivdsi           !: river input fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nitdep           !: atmospheric N deposition 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ironsed          !: Coastal supply of iron
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hydrofe          !: Hydrothermal vent supply of iron

   REAL(wp), PUBLIC :: sedsilfrac, sedcalfrac
   REAL(wp), PUBLIC :: rivalkinput, rivdicinput
   REAL(wp), PUBLIC :: rivdininput, rivdipinput, rivdsiinput

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zsbc.F90 13284 2020-07-09 15:12:23Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sbc( kt )
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_sbc  ***
      !!
      !! ** purpose :   read and interpolate the external sources of nutrients
      !!
      !! ** method  :   read the files and interpolate the appropriate variables
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !
      INTEGER  ::   ji, jj 
      REAL(wp) ::   zcoef, zyyss
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_sbc')
      !
      ! Compute dust at nit000 or only if there is more than 1 time record in dust file
      IF( ln_dust ) THEN
         IF( kt == nit000 .OR. ( kt /= nit000 .AND. ntimes_dust > 1 ) ) THEN
            CALL fld_read( kt, 1, sf_dust )
            dust(:,:) = MAX( rtrn, sf_dust(1)%fnow(:,:,1) ) * ( 1.0 - fr_i(:,:) )
         ENDIF
      ENDIF
      !
      IF( ll_solub ) THEN
         IF( kt == nit000 .OR. ( kt /= nit000 .AND. ntimes_solub > 1 ) ) THEN
            CALL fld_read( kt, 1, sf_solub )
            solub(:,:) = sf_solub(1)%fnow(:,:,1)
         ENDIF
      ENDIF

      ! N/P and Si releases due to coastal rivers
      ! Compute river at nit000 or only if there is more than 1 time record in river file
      ! -----------------------------------------
      IF( ln_river ) THEN
         IF( kt == nit000 .OR. ( kt /= nit000 .AND. ntimes_riv > 1 ) ) THEN
            CALL fld_read( kt, 1, sf_river )
            IF( ln_p4z ) THEN
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zcoef = ryyss * e1e2t(ji,jj) * h_rnf(ji,jj) 
                     rivalk(ji,jj) =   sf_river(jr_dic)%fnow(ji,jj,1)  &
                        &              * 1.E3        / ( 12. * zcoef + rtrn )
                     rivdic(ji,jj) =   sf_river(jr_dic)%fnow(ji,jj,1)  &
                        &              * 1.E3         / ( 12. * zcoef + rtrn )
                     rivdin(ji,jj) =   sf_river(jr_din)%fnow(ji,jj,1)  &
                        &              * 1.E3 / rno3 / ( 14. * zcoef + rtrn )
                     rivdip(ji,jj) =   sf_river(jr_dip)%fnow(ji,jj,1)  &
                        &              * 1.E3 / po4r / ( 31. * zcoef + rtrn )
                     rivdsi(ji,jj) =   sf_river(jr_dsi)%fnow(ji,jj,1)  &
                        &              * 1.E3        / ( 28.1 * zcoef + rtrn )
                     rivdoc(ji,jj) =   sf_river(jr_doc)%fnow(ji,jj,1)  &
                        &              * 1.E3        / ( 12. * zcoef + rtrn ) 
                  END DO
               END DO
            ELSE    !  ln_p5z
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zcoef = ryyss * e1e2t(ji,jj) * h_rnf(ji,jj) 
                     rivalk(ji,jj) =   sf_river(jr_dic)%fnow(ji,jj,1)                                    &
                        &              * 1.E3        / ( 12. * zcoef + rtrn )
                     rivdic(ji,jj) = ( sf_river(jr_dic)%fnow(ji,jj,1) ) &
                        &              * 1.E3 / ( 12. * zcoef + rtrn ) * tmask(ji,jj,1)
                     rivdin(ji,jj) = ( sf_river(jr_din)%fnow(ji,jj,1) ) &
                        &              * 1.E3 / rno3 / ( 14. * zcoef + rtrn ) * tmask(ji,jj,1)
                     rivdip(ji,jj) = ( sf_river(jr_dip)%fnow(ji,jj,1) ) &
                        &              * 1.E3 / po4r / ( 31. * zcoef + rtrn ) * tmask(ji,jj,1)
                     rivdon(ji,jj) = ( sf_river(jr_don)%fnow(ji,jj,1) ) &
                        &              * 1.E3 / rno3 / ( 14. * zcoef + rtrn ) * tmask(ji,jj,1)
                     rivdop(ji,jj) = ( sf_river(jr_dop)%fnow(ji,jj,1) ) &
                        &              * 1.E3 / po4r / ( 31. * zcoef + rtrn ) * tmask(ji,jj,1)
                     rivdsi(ji,jj) =   sf_river(jr_dsi)%fnow(ji,jj,1)  &
                        &              * 1.E3        / ( 28.1 * zcoef + rtrn )
                     rivdoc(ji,jj) =   sf_river(jr_doc)%fnow(ji,jj,1)  &
                        &              * 1.E3        / ( 12. * zcoef + rtrn )
                  END DO
               END DO
            ENDIF
         ENDIF
      ENDIF

      ! Compute N deposition at nit000 or only if there is more than 1 time record in N deposition file
      IF( ln_ndepo ) THEN
         IF( kt == nit000 .OR. ( kt /= nit000 .AND. ntimes_ndep > 1 ) ) THEN
             zcoef = 14. * rno3
             CALL fld_read( kt, 1, sf_ndepo )
             nitdep(:,:) = MAX( rtrn, sf_ndepo(1)%fnow(:,:,1) / zcoef / e3t_n(:,:,1) )
         ENDIF
         IF( .NOT.ln_linssh ) THEN
            zcoef = 14. * rno3
            nitdep(:,:) = MAX( rtrn, sf_ndepo(1)%fnow(:,:,1) / zcoef / e3t_n(:,:,1) )
         ENDIF
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('p4z_sbc')
      !
   END SUBROUTINE p4z_sbc


   SUBROUTINE p4z_sbc_init
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_sbc_init  ***
      !!
      !! ** purpose :   initialization of the external sources of nutrients
      !!
      !! ** method  :   read the files and compute the budget
      !!                called at the first timestep (nittrc000)
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      INTEGER  :: ji, jj, jk, jm, ifpr
      INTEGER  :: ii0, ii1, ij0, ij1
      INTEGER  :: numdust, numsolub, numriv, numiron, numdepo, numhydro
      INTEGER  :: ierr, ierr1, ierr2, ierr3
      INTEGER  :: ios                 ! Local integer output status for namelist read
      INTEGER  :: ik50                !  last level where depth less than 50 m
      INTEGER  :: isrow             ! index for ORCA1 starting row
      REAL(wp) :: zexpide, zdenitide, zmaskt, zsurfc, zsurfp,ze3t, ze3t2, zcslp
      REAL(wp) :: ztimes_dust, ztimes_riv, ztimes_ndep 
      REAL(wp), DIMENSION(:), ALLOCATABLE :: rivinput
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zriver, zcmask
      !
      CHARACTER(len=100) ::  cn_dir          ! Root directory for location of ssr files
      TYPE(FLD_N), DIMENSION(jpriv) ::  slf_river    ! array of namelist informations on the fields to read
      TYPE(FLD_N) ::   sn_dust, sn_solub, sn_ndepo, sn_ironsed, sn_hydrofe   ! informations about the fields to be read
      TYPE(FLD_N) ::   sn_riverdoc, sn_riverdic, sn_riverdsi   ! informations about the fields to be read
      TYPE(FLD_N) ::   sn_riverdin, sn_riverdon, sn_riverdip, sn_riverdop
      !!
      NAMELIST/nampissbc/cn_dir, sn_dust, sn_solub, sn_riverdic, sn_riverdoc, sn_riverdin, sn_riverdon,     &
        &                sn_riverdip, sn_riverdop, sn_riverdsi, sn_ndepo, sn_ironsed, sn_hydrofe, &
        &                ln_dust, ln_solub, ln_river, ln_ndepo, ln_ironsed, ln_ironice, ln_hydrofe,    &
        &                sedfeinput, distcoast, dustsolub, icefeinput, wdust, mfrac, nitrfix, diazolight, concfediaz, &
        &                hratio, lgw_rath
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_sbc_init : initialization of the external sources of nutrients '
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !                            !* set file information
      REWIND( numnatp_ref )              ! Namelist nampissbc in reference namelist : Pisces external sources of nutrients
      READ  ( numnatp_ref, nampissbc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampissbc in reference namelist' )
      REWIND( numnatp_cfg )              ! Namelist nampissbc in configuration namelist : Pisces external sources of nutrients
      READ  ( numnatp_cfg, nampissbc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampissbc in configuration namelist' )
      IF(lwm) WRITE ( numonp, nampissbc )

      IF(lwp) THEN
         WRITE(numout,*) '   Namelist : nampissbc '
         WRITE(numout,*) '      dust input from the atmosphere           ln_dust     = ', ln_dust
         WRITE(numout,*) '      Variable solubility of iron input        ln_solub    = ', ln_solub
         WRITE(numout,*) '      river input of nutrients                 ln_river    = ', ln_river
         WRITE(numout,*) '      atmospheric deposition of n              ln_ndepo    = ', ln_ndepo
         WRITE(numout,*) '      Fe input from sediments                  ln_ironsed  = ', ln_ironsed
         WRITE(numout,*) '      Fe input from seaice                     ln_ironice  = ', ln_ironice
         WRITE(numout,*) '      fe input from hydrothermal vents         ln_hydrofe  = ', ln_hydrofe
         WRITE(numout,*) '      coastal release of iron                  sedfeinput  = ', sedfeinput
         WRITE(numout,*) '      distance off the coast                   distcoast   = ', distcoast
         WRITE(numout,*) '      solubility of the dust                   dustsolub   = ', dustsolub
         WRITE(numout,*) '      Mineral Fe content of the dust           mfrac       = ', mfrac
         WRITE(numout,*) '      Iron concentration in sea ice            icefeinput  = ', icefeinput
         WRITE(numout,*) '      sinking speed of the dust                wdust       = ', wdust
         WRITE(numout,*) '      nitrogen fixation rate                   nitrfix     = ', nitrfix
         WRITE(numout,*) '      nitrogen fixation sensitivty to light    diazolight  = ', diazolight
         WRITE(numout,*) '      Fe half-saturation cste for diazotrophs  concfediaz  = ', concfediaz
         WRITE(numout,*) '      Fe to 3He ratio assumed for vent iron supply hratio  = ', hratio
         IF( ln_ligand ) THEN
            WRITE(numout,*) '      Weak ligand ratio from sed hydro sources  lgw_rath   = ', lgw_rath
         ENDIF
      END IF

      IF( ln_dust .OR. ln_river .OR. ln_ndepo ) THEN   ;   ll_sbc = .TRUE.
      ELSE                                             ;   ll_sbc = .FALSE.
      ENDIF

      IF( ln_dust .AND. ln_solub ) THEN                ;   ll_solub = .TRUE.
      ELSE                                             ;   ll_solub = .FALSE.
      ENDIF

      ! dust input from the atmosphere
      ! ------------------------------
      IF( ln_dust ) THEN 
         !
         IF(lwp) WRITE(numout,*) '    initialize dust input from atmosphere '
         IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
         !
         ALLOCATE( dust(jpi,jpj) )    ! allocation
         !
         ALLOCATE( sf_dust(1), STAT=ierr )           !* allocate and fill sf_sst (forcing structure) with sn_sst
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_init: unable to allocate sf_dust structure' )
         !
         CALL fld_fill( sf_dust, (/ sn_dust /), cn_dir, 'p4z_sed_init', 'Atmospheric dust deposition', 'nampissed' )
                                   ALLOCATE( sf_dust(1)%fnow(jpi,jpj,1)   )
         IF( sn_dust%ln_tint )     ALLOCATE( sf_dust(1)%fdta(jpi,jpj,1,2) )
         !
         IF( Agrif_Root() ) THEN   !  Only on the master grid
            ! Get total input dust ; need to compute total atmospheric supply of Si in a year
            CALL iom_open (  TRIM( sn_dust%clname ) , numdust )
            ntimes_dust = iom_getszuld( numdust )   ! get number of record in file
         END IF
      END IF

      ! Solubility of dust deposition of iron
      ! Only if ln_dust and ln_solubility set to true (ll_solub = .true.)
      ! -----------------------------------------------------------------
      IF( ll_solub ) THEN
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   ll_solub=T , initialize variable solubility of Fe '
         !
         ALLOCATE( solub(jpi,jpj) )    ! allocation
         !
         ALLOCATE( sf_solub(1), STAT=ierr )           !* allocate and fill sf_sst (forcing structure) with sn_sst
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_init: unable to allocate sf_solub structure' )
         !
         CALL fld_fill( sf_solub, (/ sn_solub /), cn_dir, 'p4z_sed_init', 'Solubility of atm. iron ', 'nampissed' )
                                   ALLOCATE( sf_solub(1)%fnow(jpi,jpj,1)   )
         IF( sn_solub%ln_tint )    ALLOCATE( sf_solub(1)%fdta(jpi,jpj,1,2) )
         ! get number of record in file
         CALL iom_open (  TRIM( sn_solub%clname ) , numsolub )
         ntimes_solub = iom_getszuld( numsolub )   ! get number of record in file
         CALL iom_close( numsolub )
      ENDIF

      ! nutrient input from rivers
      ! --------------------------
      IF( ln_river ) THEN
         !
         slf_river(jr_dic) = sn_riverdic   ;   slf_river(jr_doc) = sn_riverdoc   ;   slf_river(jr_din) = sn_riverdin 
         slf_river(jr_don) = sn_riverdon   ;   slf_river(jr_dip) = sn_riverdip   ;   slf_river(jr_dop) = sn_riverdop
         slf_river(jr_dsi) = sn_riverdsi  
         !
         ALLOCATE( rivdic(jpi,jpj), rivalk(jpi,jpj), rivdin(jpi,jpj), rivdip(jpi,jpj), rivdsi(jpi,jpj), rivdoc(jpi,jpj) ) 
         IF( ln_p5z )  ALLOCATE( rivdon(jpi,jpj), rivdop(jpi,jpj) )
         !
         ALLOCATE( sf_river(jpriv), rivinput(jpriv), STAT=ierr1 )    !* allocate and fill sf_river (forcing structure) with sn_river_
         rivinput(:) = 0._wp

         IF( ierr1 > 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_init: unable to allocate sf_irver structure' )
         !
         CALL fld_fill( sf_river, slf_river, cn_dir, 'p4z_sed_init', 'Input from river ', 'nampissed' )
         DO ifpr = 1, jpriv
                                          ALLOCATE( sf_river(ifpr)%fnow(jpi,jpj,1  ) )
            IF( slf_river(ifpr)%ln_tint ) ALLOCATE( sf_river(ifpr)%fdta(jpi,jpj,1,2) )
         END DO
         IF( Agrif_Root() ) THEN   !  Only on the master grid
            ! Get total input rivers ; need to compute total river supply in a year
            DO ifpr = 1, jpriv
               CALL iom_open ( TRIM( slf_river(ifpr)%clname ), numriv )
               ntimes_riv = iom_getszuld( numriv )
               ALLOCATE( zriver(jpi,jpj,ntimes_riv) )
               DO jm = 1, ntimes_riv
                  CALL iom_get( numriv, jpdom_data, TRIM( slf_river(ifpr)%clvar ), zriver(:,:,jm), jm )
               END DO
               CALL iom_close( numriv )
               ztimes_riv = 1._wp / REAL(ntimes_riv, wp) 
               DO jm = 1, ntimes_riv
                  rivinput(ifpr) = rivinput(ifpr) + glob_sum( 'p4zsbc', zriver(:,:,jm) * tmask(:,:,1) * ztimes_riv ) 
               END DO
               DEALLOCATE( zriver)
            END DO
            ! N/P and Si releases due to coastal rivers
            ! -----------------------------------------
            rivdicinput = (rivinput(jr_dic) + rivinput(jr_doc) ) * 1E3 / 12._wp
            rivdininput = (rivinput(jr_din) + rivinput(jr_don) ) * 1E3 / rno3 / 14._wp
            rivdipinput = (rivinput(jr_dip) + rivinput(jr_dop) ) * 1E3 / po4r / 31._wp
            rivdsiinput = rivinput(jr_dsi) * 1E3 / 28.1_wp
            rivalkinput = rivinput(jr_dic) * 1E3 / 12._wp
            !
         ENDIF
      ELSE
         rivdicinput = 0._wp
         rivdininput = 0._wp
         rivdipinput = 0._wp
         rivdsiinput = 0._wp
         rivalkinput = 0._wp
      END IF 
      ! nutrient input from dust
      ! ------------------------
      IF( ln_ndepo ) THEN
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   ln_ndepo=T , initialize the nutrient input by dust from NetCDF file'
         !
         ALLOCATE( nitdep(jpi,jpj) )    ! allocation
         !
         ALLOCATE( sf_ndepo(1), STAT=ierr3 )           !* allocate and fill sf_sst (forcing structure) with sn_sst
         IF( ierr3 > 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_init: unable to allocate sf_ndepo structure' )
         !
         CALL fld_fill( sf_ndepo, (/ sn_ndepo /), cn_dir, 'p4z_sed_init', 'Nutrient atmospheric depositon ', 'nampissed' )
                                   ALLOCATE( sf_ndepo(1)%fnow(jpi,jpj,1)   )
         IF( sn_ndepo%ln_tint )    ALLOCATE( sf_ndepo(1)%fdta(jpi,jpj,1,2) )
         !
         IF( Agrif_Root() ) THEN   !  Only on the master grid
            ! Get total input dust ; need to compute total atmospheric supply of N in a year
            CALL iom_open ( TRIM( sn_ndepo%clname ), numdepo )
            ntimes_ndep = iom_getszuld( numdepo )
         ENDIF
      ENDIF

      ! coastal and island masks
      ! ------------------------
      IF( ln_ironsed ) THEN     
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   ln_ironsed=T , computation of an island mask to enhance coastal supply of iron'
         !
         ALLOCATE( ironsed(jpi,jpj,jpk) )    ! allocation
         !
         CALL iom_open ( TRIM( sn_ironsed%clname ), numiron )
         ALLOCATE( zcmask(jpi,jpj,jpk) )
         CALL iom_get  ( numiron, jpdom_data, TRIM( sn_ironsed%clvar ), zcmask(:,:,:), 1 )
         CALL iom_close( numiron )
         !
         ik50 = 5        !  last level where depth less than 50 m
         DO jk = jpkm1, 1, -1
            IF( gdept_1d(jk) > 50. )   ik50 = jk - 1
         END DO
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' Level corresponding to 50m depth ',  ik50,' ', gdept_1d(ik50+1)
         DO jk = 1, ik50
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1
                  ze3t   = e3t_0(ji,jj,jk)
                  zsurfc =  e1u(ji,jj) * ( 1. - umask(ji  ,jj  ,jk) )   &
                          + e1u(ji,jj) * ( 1. - umask(ji-1,jj  ,jk) )   &
                          + e2v(ji,jj) * ( 1. - vmask(ji  ,jj  ,jk) )   &
                          + e2v(ji,jj) * ( 1. - vmask(ji  ,jj-1,jk) )
                  zsurfp = zsurfc * ze3t / e1e2t(ji,jj)
                  ! estimation of the coastal slope : 5 km off the coast
                  ze3t2 = ze3t * ze3t
                  zcslp = SQRT( ( distcoast*distcoast + ze3t2 ) / ze3t2 )
                  !
                  zcmask(ji,jj,jk) = zcmask(ji,jj,jk) + zcslp * zsurfp
               END DO
            END DO
         END DO
         !
         CALL lbc_lnk( 'p4zsbc', zcmask , 'T', 1. )      ! lateral boundary conditions on cmask   (sign unchanged)
         !
         DO jk = 1, jpk
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zexpide   = MIN( 8.,( gdept_n(ji,jj,jk) / 500. )**(-1.5) )
                  zdenitide = -0.9543 + 0.7662 * LOG( zexpide ) - 0.235 * LOG( zexpide )**2
                  zcmask(ji,jj,jk) = zcmask(ji,jj,jk) * MIN( 1., EXP( zdenitide ) / 0.5 )
               END DO
            END DO
         END DO
         ! Coastal supply of iron
         ! -------------------------
         ironsed(:,:,jpk) = 0._wp
         DO jk = 1, jpkm1
            ironsed(:,:,jk) = sedfeinput * zcmask(:,:,jk) / ( e3t_0(:,:,jk) * rday )
         END DO
         DEALLOCATE( zcmask)
      ENDIF
      !
      ! Iron from Hydrothermal vents
      ! ------------------------
      IF( ln_hydrofe ) THEN
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   ln_hydrofe=T , Input of iron from hydrothermal vents'
         !
         ALLOCATE( hydrofe(jpi,jpj,jpk) )    ! allocation
         !
         CALL iom_open ( TRIM( sn_hydrofe%clname ), numhydro )
         CALL iom_get  ( numhydro, jpdom_data, TRIM( sn_hydrofe%clvar ), hydrofe(:,:,:), 1 )
         CALL iom_close( numhydro )
         !
         DO jk = 1, jpk
            hydrofe(:,:,jk) = ( hydrofe(:,:,jk) * hratio ) / ( e1e2t(:,:) * e3t_0(:,:,jk) * ryyss + rtrn ) / 1000._wp
         ENDDO
         !
      ENDIF
      ! 
      IF( ll_sbc ) CALL p4z_sbc( nit000 ) 
      !
      IF(lwp) THEN 
         WRITE(numout,*)
         WRITE(numout,*) '    Total input of elements from river supply'
         WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    N Supply   : ', rivdininput*rno3*1E3/1E12*14.,' TgN/yr'
         WRITE(numout,*) '    Si Supply  : ', rivdsiinput*1E3/1E12*28.1    ,' TgSi/yr'
         WRITE(numout,*) '    P Supply   : ', rivdipinput*1E3*po4r/1E12*31.,' TgP/yr'
         WRITE(numout,*) '    Alk Supply : ', rivalkinput*1E3/1E12         ,' Teq/yr'
         WRITE(numout,*) '    DIC Supply : ', rivdicinput*1E3*12./1E12     ,' TgC/yr'
         WRITE(numout,*) 
      ENDIF
      !
      sedsilfrac = 0.03     ! percentage of silica loss in the sediments
      sedcalfrac = 0.6      ! percentage of calcite loss in the sediments
      !
   END SUBROUTINE p4z_sbc_init

   !!======================================================================
END MODULE p4zsbc
