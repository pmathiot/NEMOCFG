MODULE p4zsink
   !!======================================================================
   !!                         ***  MODULE p4zsink  ***
   !! TOP :  PISCES  vertical flux of particulate matter due to gravitational sinking
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Change aggregation formula
   !!             3.5  !  2012-07  (O. Aumont) Introduce potential time-splitting
   !!----------------------------------------------------------------------
   !!   p4z_sink       :  Compute vertical flux of particulate matter due to gravitational sinking
   !!   p4z_sink_init  :  Unitialisation of sinking speed parameters
   !!   p4z_sink_alloc :  Allocate sinking speed variables
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE trcsink         !  General routine to compute sedimentation
   USE prtctl_trc      !  print control for debugging
   USE iom             !  I/O manager
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sink         ! called in p4zbio.F90
   PUBLIC   p4z_sink_init    ! called in trcsms_pisces.F90
   PUBLIC   p4z_sink_alloc

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sinking, sinking2  !: POC sinking fluxes 
   !                                                          !  (different meanings depending on the parameterization)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sinkingn, sinking2n  !: POC sinking fluxes 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sinkingp, sinking2p  !: POC sinking fluxes 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sinkcal, sinksil   !: CaCO3 and BSi sinking fluxes
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sinkfer            !: Small BFe sinking fluxes
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sinkfer2           !: Big iron sinking fluxes

   INTEGER  :: ik100

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zsink.F90 12276 2019-12-20 11:14:26Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!   'standard sinking parameterisation'                  ???
   !!----------------------------------------------------------------------

   SUBROUTINE p4z_sink ( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink  ***
      !!
      !! ** Purpose :   Compute vertical flux of particulate matter due to 
      !!                gravitational sinking
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, knt
      INTEGER  ::   ji, jj, jk
      CHARACTER (len=25) :: charout
      REAL(wp) :: zmax, zfact
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_sink')

      ! Initialization of some global variables
      ! ---------------------------------------
      prodpoc(:,:,:) = 0.
      conspoc(:,:,:) = 0.
      prodgoc(:,:,:) = 0.
      consgoc(:,:,:) = 0.

      !
      !    Sinking speeds of detritus is increased with depth as shown
      !    by data and from the coagulation theory
      !    -----------------------------------------------------------
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1,jpi
               zmax  = MAX( heup_01(ji,jj), hmld(ji,jj) )
               zfact = MAX( 0., gdepw_n(ji,jj,jk+1) - zmax ) / wsbio2scale
               wsbio4(ji,jj,jk) = wsbio2 + MAX(0., ( wsbio2max - wsbio2 )) * zfact
            END DO
         END DO
      END DO

      ! limit the values of the sinking speeds to avoid numerical instabilities  
      wsbio3(:,:,:) = wsbio

      !
      !  Initializa to zero all the sinking arrays 
      !   -----------------------------------------
      sinking (:,:,:) = 0.e0
      sinking2(:,:,:) = 0.e0
      sinkcal (:,:,:) = 0.e0
      sinkfer (:,:,:) = 0.e0
      sinksil (:,:,:) = 0.e0
      sinkfer2(:,:,:) = 0.e0

      !   Compute the sedimentation term using p4zsink2 for all the sinking particles
      !   -----------------------------------------------------
      CALL trc_sink( kt, wsbio3, sinking , jppoc, rfact2 )
      CALL trc_sink( kt, wsbio3, sinkfer , jpsfe, rfact2 )
      CALL trc_sink( kt, wsbio4, sinking2, jpgoc, rfact2 )
      CALL trc_sink( kt, wsbio4, sinkfer2, jpbfe, rfact2 )
      CALL trc_sink( kt, wsbio4, sinksil , jpgsi, rfact2 )
      CALL trc_sink( kt, wsbio4, sinkcal , jpcal, rfact2 )

      IF( ln_p5z ) THEN
         sinkingn (:,:,:) = 0.e0
         sinking2n(:,:,:) = 0.e0
         sinkingp (:,:,:) = 0.e0
         sinking2p(:,:,:) = 0.e0

         !   Compute the sedimentation term using p4zsink2 for all the sinking particles
         !   -----------------------------------------------------
         CALL trc_sink( kt, wsbio3, sinkingn , jppon, rfact2 )
         CALL trc_sink( kt, wsbio3, sinkingp , jppop, rfact2 )
         CALL trc_sink( kt, wsbio4, sinking2n, jpgon, rfact2 )
         CALL trc_sink( kt, wsbio4, sinking2p, jpgop, rfact2 )
      ENDIF

     ! Total carbon export per year
     IF( iom_use( "tcexp" ) .OR. ( ln_check_mass .AND. kt == nitend .AND. knt == nrdttrc )  )  &
        &   t_oce_co2_exp = glob_sum( 'p4zsink', ( sinking(:,:,ik100) + sinking2(:,:,ik100) ) * e1e2t(:,:) * tmask(:,:,1) )
     !
     IF( lk_iomput .AND.  knt == nrdttrc ) THEN
       zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
       !
       CALL iom_put( "EPC100"  , ( sinking(:,:,ik100) + sinking2(:,:,ik100) ) * zfact * tmask(:,:,1) ) ! Export of carbon at 100m 
       CALL iom_put( "EPFE100" , ( sinkfer(:,:,ik100) + sinkfer2(:,:,ik100) ) * zfact * tmask(:,:,1) ) ! Export of iron at 100m 
       CALL iom_put( "EPCAL100", sinkcal(:,:,ik100) * zfact * tmask(:,:,1) )      ! Export of calcite at 100m 
       CALL iom_put( "EPSI100" , sinksil(:,:,ik100) * zfact * tmask(:,:,1) )          ! Export of bigenic silica at 100m 
       CALL iom_put( "EXPC"    , ( sinking(:,:,:) + sinking2(:,:,:) ) * zfact * tmask(:,:,:) ) ! Export of carbon in the water column 
       CALL iom_put( "EXPFE"   , ( sinkfer(:,:,:) + sinkfer2(:,:,:) ) * zfact * tmask(:,:,:) ) ! Export of iron  
       CALL iom_put( "EXPCAL"  , sinkcal(:,:,:) * zfact * tmask(:,:,:) )      ! Export of calcite 
       CALL iom_put( "EXPSI"   , sinksil(:,:,:) * zfact * tmask(:,:,:) )      ! Export of bigenic silica
       CALL iom_put( "tcexp"   , t_oce_co2_exp * zfact )   ! molC/s
       ! 
      ENDIF
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('sink')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_sink')
      !
   END SUBROUTINE p4z_sink


   SUBROUTINE p4z_sink_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_sink_init  ***
      !!----------------------------------------------------------------------
      INTEGER :: jk
      !!----------------------------------------------------------------------
      !
      ik100 = 10        !  last level where depth less than 100 m
      DO jk = jpkm1, 1, -1
         IF( gdept_1d(jk) > 100. )  ik100 = jk - 1
      END DO
      IF (lwp) WRITE(numout,*)
      IF (lwp) WRITE(numout,*) ' Level corresponding to 100m depth ',  ik100 + 1
      IF (lwp) WRITE(numout,*)
      !
      t_oce_co2_exp = 0._wp
      !
   END SUBROUTINE p4z_sink_init

   INTEGER FUNCTION p4z_sink_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(2)
      !!----------------------------------------------------------------------
      !
      ierr(:) = 0
      !
      ALLOCATE( sinking(jpi,jpj,jpk) , sinking2(jpi,jpj,jpk)                    ,     &                
         &      sinkcal(jpi,jpj,jpk) , sinksil (jpi,jpj,jpk)                    ,     &                
         &      sinkfer2(jpi,jpj,jpk)                                           ,     &                
         &      sinkfer(jpi,jpj,jpk)                                            , STAT=ierr(1) )                
         !
      IF( ln_p5z    ) ALLOCATE( sinkingn(jpi,jpj,jpk), sinking2n(jpi,jpj,jpk)   ,     &
         &                      sinkingp(jpi,jpj,jpk), sinking2p(jpi,jpj,jpk)   , STAT=ierr(2) )
      !
      p4z_sink_alloc = MAXVAL( ierr )
      IF( p4z_sink_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p4z_sink_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p4z_sink_alloc
   
   !!======================================================================
END MODULE p4zsink
