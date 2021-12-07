MODULE p4zlim
   !!======================================================================
   !!                         ***  MODULE p4zlim  ***
   !! TOP :   PISCES 
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-04  (O. Aumont, C. Ethe) Limitation for iron modelled in quota 
   !!----------------------------------------------------------------------
   !!   p4z_lim        :   Compute the nutrients limitation terms 
   !!   p4z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE oce_trc         ! Shared ocean-passive tracers variables
   USE trc             ! Tracers defined
   USE sms_pisces      ! PISCES variables
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC p4z_lim    
   PUBLIC p4z_lim_init    
   PUBLIC p4z_lim_alloc

   !! * Shared module variables
   REAL(wp), PUBLIC ::  concnno3    !:  NO3, PO4 half saturation   
   REAL(wp), PUBLIC ::  concdno3    !:  Phosphate half saturation for diatoms  
   REAL(wp), PUBLIC ::  concnnh4    !:  NH4 half saturation for phyto  
   REAL(wp), PUBLIC ::  concdnh4    !:  NH4 half saturation for diatoms
   REAL(wp), PUBLIC ::  concnfer    !:  Iron half saturation for nanophyto 
   REAL(wp), PUBLIC ::  concdfer    !:  Iron half saturation for diatoms  
   REAL(wp), PUBLIC ::  concbno3    !:  NO3 half saturation  for bacteria 
   REAL(wp), PUBLIC ::  concbnh4    !:  NH4 half saturation for bacteria
   REAL(wp), PUBLIC ::  xsizedia    !:  Minimum size criteria for diatoms
   REAL(wp), PUBLIC ::  xsizephy    !:  Minimum size criteria for nanophyto
   REAL(wp), PUBLIC ::  xsizern     !:  Size ratio for nanophytoplankton
   REAL(wp), PUBLIC ::  xsizerd     !:  Size ratio for diatoms
   REAL(wp), PUBLIC ::  xksi1       !:  half saturation constant for Si uptake 
   REAL(wp), PUBLIC ::  xksi2       !:  half saturation constant for Si/C 
   REAL(wp), PUBLIC ::  xkdoc       !:  2nd half-sat. of DOC remineralization  
   REAL(wp), PUBLIC ::  concbfe     !:  Fe half saturation for bacteria 
   REAL(wp), PUBLIC ::  oxymin      !:  half saturation constant for anoxia
   REAL(wp), PUBLIC ::  qnfelim     !:  optimal Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qdfelim     !:  optimal Fe quota for diatoms
   REAL(wp), PUBLIC ::  caco3r      !:  mean rainratio 

   !!* Phytoplankton limitation terms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanono3   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatno3   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanonh4   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatnh4   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanopo4   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatpo4   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimphy    !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdia    !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimnfe    !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdfe    !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimsi     !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimbac    !: ??
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimbacl   !: ??
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   concdfe    !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   concnfe    !: ???

   ! Coefficient for iron limitation
   REAL(wp) ::  xcoef1   = 0.0016  / 55.85  
   REAL(wp) ::  xcoef2   = 1.21E-5 * 14. / 55.85 / 7.625 * 0.5 * 1.5
   REAL(wp) ::  xcoef3   = 1.15E-4 * 14. / 55.85 / 7.625 * 0.5 

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zlim.F90 10069 2018-08-28 14:12:24Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_lim( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!              for the various phytoplankton species
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt, knt
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zlim1, zlim2, zlim3, zlim4, zno3, zferlim
      REAL(wp) ::   zconcd, zconcd2, zconcn, zconcn2
      REAL(wp) ::   z1_trbdia, z1_trbphy, ztem1, ztem2, zetot1, zetot2
      REAL(wp) ::   zdenom, zratio, zironmin
      REAL(wp) ::   zconc1d, zconc1dnh4, zconc0n, zconc0nnh4   
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_lim')
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               
               ! Tuning of the iron concentration to a minimum level that is set to the detection limit
               !-------------------------------------
               zno3    = trb(ji,jj,jk,jpno3) / 40.e-6
               zferlim = MAX( 3e-11 * zno3 * zno3, 5e-12 )
               zferlim = MIN( zferlim, 7e-11 )
               trb(ji,jj,jk,jpfer) = MAX( trb(ji,jj,jk,jpfer), zferlim )

               ! Computation of a variable Ks for iron on diatoms taking into account
               ! that increasing biomass is made of generally bigger cells
               !------------------------------------------------
               zconcd   = MAX( 0.e0 , trb(ji,jj,jk,jpdia) - xsizedia )
               zconcd2  = trb(ji,jj,jk,jpdia) - zconcd
               zconcn   = MAX( 0.e0 , trb(ji,jj,jk,jpphy) - xsizephy )
               zconcn2  = trb(ji,jj,jk,jpphy) - zconcn
               z1_trbphy   = 1. / ( trb(ji,jj,jk,jpphy) + rtrn )
               z1_trbdia   = 1. / ( trb(ji,jj,jk,jpdia) + rtrn )

               concdfe(ji,jj,jk) = MAX( concdfer, ( zconcd2 * concdfer + concdfer * xsizerd * zconcd ) * z1_trbdia )
               zconc1d           = MAX( concdno3, ( zconcd2 * concdno3 + concdno3 * xsizerd * zconcd ) * z1_trbdia )
               zconc1dnh4        = MAX( concdnh4, ( zconcd2 * concdnh4 + concdnh4 * xsizerd * zconcd ) * z1_trbdia )

               concnfe(ji,jj,jk) = MAX( concnfer, ( zconcn2 * concnfer + concnfer * xsizern * zconcn ) * z1_trbphy )
               zconc0n           = MAX( concnno3, ( zconcn2 * concnno3 + concnno3 * xsizern * zconcn ) * z1_trbphy )
               zconc0nnh4        = MAX( concnnh4, ( zconcn2 * concnnh4 + concnnh4 * xsizern * zconcn ) * z1_trbphy )

               ! Michaelis-Menten Limitation term for nutrients Small bacteria
               ! -------------------------------------------------------------
               zdenom = 1. /  ( concbno3 * concbnh4 + concbnh4 * trb(ji,jj,jk,jpno3) + concbno3 * trb(ji,jj,jk,jpnh4) )
               xnanono3(ji,jj,jk) = trb(ji,jj,jk,jpno3) * concbnh4 * zdenom
               xnanonh4(ji,jj,jk) = trb(ji,jj,jk,jpnh4) * concbno3 * zdenom
               !
               zlim1    = xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk)
               zlim2    = trb(ji,jj,jk,jppo4) / ( trb(ji,jj,jk,jppo4) + concbnh4 )
               zlim3    = trb(ji,jj,jk,jpfer) / ( concbfe + trb(ji,jj,jk,jpfer) )
               zlim4    = trb(ji,jj,jk,jpdoc) / ( xkdoc   + trb(ji,jj,jk,jpdoc) )
               xlimbacl(ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
               xlimbac (ji,jj,jk) = MIN( zlim1, zlim2, zlim3 ) * zlim4

               ! Michaelis-Menten Limitation term for nutrients Small flagellates
               ! -----------------------------------------------
               zdenom = 1. /  ( zconc0n * zconc0nnh4 + zconc0nnh4 * trb(ji,jj,jk,jpno3) + zconc0n * trb(ji,jj,jk,jpnh4) )
               xnanono3(ji,jj,jk) = trb(ji,jj,jk,jpno3) * zconc0nnh4 * zdenom
               xnanonh4(ji,jj,jk) = trb(ji,jj,jk,jpnh4) * zconc0n    * zdenom
               !
               zlim1    = xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk)
               zlim2    = trb(ji,jj,jk,jppo4) / ( trb(ji,jj,jk,jppo4) + zconc0nnh4 )
               zratio   = trb(ji,jj,jk,jpnfe) * z1_trbphy 
               zironmin = xcoef1 * trb(ji,jj,jk,jpnch) * z1_trbphy + xcoef2 * zlim1 + xcoef3 * xnanono3(ji,jj,jk)
               zlim3    = MAX( 0.,( zratio - zironmin ) / qnfelim )
               xnanopo4(ji,jj,jk) = zlim2
               xlimnfe (ji,jj,jk) = MIN( 1., zlim3 )
               xlimphy (ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
               !
               !   Michaelis-Menten Limitation term for nutrients Diatoms
               !   ----------------------------------------------
               zdenom   = 1. / ( zconc1d * zconc1dnh4 + zconc1dnh4 * trb(ji,jj,jk,jpno3) + zconc1d * trb(ji,jj,jk,jpnh4) )
               xdiatno3(ji,jj,jk) = trb(ji,jj,jk,jpno3) * zconc1dnh4 * zdenom
               xdiatnh4(ji,jj,jk) = trb(ji,jj,jk,jpnh4) * zconc1d    * zdenom
               !
               zlim1    = xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk)
               zlim2    = trb(ji,jj,jk,jppo4) / ( trb(ji,jj,jk,jppo4) + zconc1dnh4  )
               zlim3    = trb(ji,jj,jk,jpsil) / ( trb(ji,jj,jk,jpsil) + xksi(ji,jj) )
               zratio   = trb(ji,jj,jk,jpdfe) * z1_trbdia
               zironmin = xcoef1 * trb(ji,jj,jk,jpdch) * z1_trbdia + xcoef2 * zlim1 + xcoef3 * xdiatno3(ji,jj,jk)
               zlim4    = MAX( 0., ( zratio - zironmin ) / qdfelim )
               xdiatpo4(ji,jj,jk) = zlim2
               xlimdfe (ji,jj,jk) = MIN( 1., zlim4 )
               xlimdia (ji,jj,jk) = MIN( zlim1, zlim2, zlim3, zlim4 )
               xlimsi  (ji,jj,jk) = MIN( zlim1, zlim2, zlim4 )
           END DO
         END DO
      END DO

      ! Compute the fraction of nanophytoplankton that is made of calcifiers
      ! --------------------------------------------------------------------
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zlim1 =  ( trb(ji,jj,jk,jpno3) * concnnh4 + trb(ji,jj,jk,jpnh4) * concnno3 )    &
                  &   / ( concnno3 * concnnh4 + concnnh4 * trb(ji,jj,jk,jpno3) + concnno3 * trb(ji,jj,jk,jpnh4) ) 
               zlim2  = trb(ji,jj,jk,jppo4) / ( trb(ji,jj,jk,jppo4) + concnnh4 )
               zlim3  = trb(ji,jj,jk,jpfer) / ( trb(ji,jj,jk,jpfer) +  5.E-11   )
               ztem1  = MAX( 0., tsn(ji,jj,jk,jp_tem) )
               ztem2  = tsn(ji,jj,jk,jp_tem) - 10.
               zetot1 = MAX( 0., etot_ndcy(ji,jj,jk) - 1.) / ( 4. + etot_ndcy(ji,jj,jk) ) 
               zetot2 = 30. / ( 30. + etot_ndcy(ji,jj,jk) ) 

               xfracal(ji,jj,jk) = caco3r * MIN( zlim1, zlim2, zlim3 )                  &
                  &                       * ztem1 / ( 0.1 + ztem1 )                     &
                  &                       * MAX( 1., trb(ji,jj,jk,jpphy) * 1.e6 / 2. )  &
                  &                       * zetot1 * zetot2               &
                  &                       * ( 1. + EXP(-ztem2 * ztem2 / 25. ) )         &
                  &                       * MIN( 1., 50. / ( hmld(ji,jj) + rtrn ) )
               xfracal(ji,jj,jk) = MIN( 0.8 , xfracal(ji,jj,jk) )
               xfracal(ji,jj,jk) = MAX( 0.02, xfracal(ji,jj,jk) )
            END DO
         END DO
      END DO
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               ! denitrification factor computed from O2 levels
               nitrfac(ji,jj,jk) = MAX(  0.e0, 0.4 * ( 6.e-6  - trb(ji,jj,jk,jpoxy) )    &
                  &                                / ( oxymin + trb(ji,jj,jk,jpoxy) )  )
               nitrfac(ji,jj,jk) = MIN( 1., nitrfac(ji,jj,jk) )
               !
               ! denitrification factor computed from NO3 levels
               nitrfac2(ji,jj,jk) = MAX( 0.e0,       ( 1.E-6 - trb(ji,jj,jk,jpno3) )  &
                  &                                / ( 1.E-6 + trb(ji,jj,jk,jpno3) ) )
               nitrfac2(ji,jj,jk) = MIN( 1., nitrfac2(ji,jj,jk) )
            END DO
         END DO
      END DO
      !
      IF( lk_iomput .AND. knt == nrdttrc ) THEN        ! save output diagnostics
        CALL iom_put( "xfracal", xfracal(:,:,:) * tmask(:,:,:) )  ! euphotic layer deptht
        CALL iom_put( "LNnut"  , xlimphy(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LDnut"  , xlimdia(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LNFe"   , xlimnfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "LDFe"   , xlimdfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_lim')
      !
   END SUBROUTINE p4z_lim


   SUBROUTINE p4z_lim_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of nutrient limitation parameters
      !!
      !! ** Method  :   Read the nampislim namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampislim
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !
      NAMELIST/namp4zlim/ concnno3, concdno3, concnnh4, concdnh4, concnfer, concdfer, concbfe,   &
         &                concbno3, concbnh4, xsizedia, xsizephy, xsizern, xsizerd,          & 
         &                xksi1, xksi2, xkdoc, qnfelim, qdfelim, caco3r, oxymin
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_lim_init : initialization of nutrient limitations'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      REWIND( numnatp_ref )
      READ  ( numnatp_ref, namp4zlim, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namp4zlim in reference namelist' )

      REWIND( numnatp_cfg )
      READ  ( numnatp_cfg, namp4zlim, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namp4zlim in configuration namelist' )
      IF(lwm) WRITE( numonp, namp4zlim )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp4zlim'
         WRITE(numout,*) '      mean rainratio                           caco3r    = ', caco3r
         WRITE(numout,*) '      NO3 half saturation of nanophyto         concnno3  = ', concnno3
         WRITE(numout,*) '      NO3 half saturation of diatoms           concdno3  = ', concdno3
         WRITE(numout,*) '      NH4 half saturation for phyto            concnnh4  = ', concnnh4
         WRITE(numout,*) '      NH4 half saturation for diatoms          concdnh4  = ', concdnh4
         WRITE(numout,*) '      half saturation constant for Si uptake   xksi1     = ', xksi1
         WRITE(numout,*) '      half saturation constant for Si/C        xksi2     = ', xksi2
         WRITE(numout,*) '      half-sat. of DOC remineralization        xkdoc     = ', xkdoc
         WRITE(numout,*) '      Iron half saturation for nanophyto       concnfer  = ', concnfer
         WRITE(numout,*) '      Iron half saturation for diatoms         concdfer  = ', concdfer
         WRITE(numout,*) '      size ratio for nanophytoplankton         xsizern   = ', xsizern
         WRITE(numout,*) '      size ratio for diatoms                   xsizerd   = ', xsizerd
         WRITE(numout,*) '      NO3 half saturation of bacteria          concbno3  = ', concbno3
         WRITE(numout,*) '      NH4 half saturation for bacteria         concbnh4  = ', concbnh4
         WRITE(numout,*) '      Minimum size criteria for diatoms        xsizedia  = ', xsizedia
         WRITE(numout,*) '      Minimum size criteria for nanophyto      xsizephy  = ', xsizephy
         WRITE(numout,*) '      Fe half saturation for bacteria          concbfe   = ', concbfe
         WRITE(numout,*) '      halk saturation constant for anoxia       oxymin   =' , oxymin
         WRITE(numout,*) '      optimal Fe quota for nano.               qnfelim   = ', qnfelim
         WRITE(numout,*) '      Optimal Fe quota for diatoms             qdfelim   = ', qdfelim
      ENDIF
      !
      nitrfac (:,:,jpk) = 0._wp
      nitrfac2(:,:,jpk) = 0._wp
      xfracal (:,:,jpk) = 0._wp
      xlimphy (:,:,jpk) = 0._wp
      xlimdia (:,:,jpk) = 0._wp
      xlimnfe (:,:,jpk) = 0._wp
      xlimdfe (:,:,jpk) = 0._wp
      !
   END SUBROUTINE p4z_lim_init


   INTEGER FUNCTION p4z_lim_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim_alloc  ***
      !!----------------------------------------------------------------------
      USE lib_mpp , ONLY: ctl_stop
      !!----------------------------------------------------------------------

      !*  Biological arrays for phytoplankton growth
      ALLOCATE( xnanono3(jpi,jpj,jpk), xdiatno3(jpi,jpj,jpk),       &
         &      xnanonh4(jpi,jpj,jpk), xdiatnh4(jpi,jpj,jpk),       &
         &      xnanopo4(jpi,jpj,jpk), xdiatpo4(jpi,jpj,jpk),       &
         &      xlimphy (jpi,jpj,jpk), xlimdia (jpi,jpj,jpk),       &
         &      xlimnfe (jpi,jpj,jpk), xlimdfe (jpi,jpj,jpk),       &
         &      xlimbac (jpi,jpj,jpk), xlimbacl(jpi,jpj,jpk),       &
         &      concnfe (jpi,jpj,jpk), concdfe (jpi,jpj,jpk),       &
         &      xlimsi  (jpi,jpj,jpk), STAT=p4z_lim_alloc )
      !
      IF( p4z_lim_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p4z_lim_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p4z_lim_alloc

   !!======================================================================
END MODULE p4zlim
