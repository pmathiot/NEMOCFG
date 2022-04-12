MODULE trcatm_c14
   !!======================================================================
   !!                     ***  MODULE  trcatm_c14  ***
   !! TOP :  read and manages atmospheric values for radiocarbon model  
   !!=====================================================================
   !! History: Based on trcini_c14b & trcsms_c14b : 
   !!          Anne Mouchet
   !!----------------------------------------------------------------------
   !!   trc_atm_c14_ini  : initialize c14atm & pco2atm
   !!   trc_atm_c14      : read and time interpolate  c14atm & pco2atm
   !!----------------------------------------------------------------------
   USE par_trc       !  passive tracers parameters
   USE oce_trc       !  shared variables between ocean and passive tracers
   USE trc           !  passive tracers common variables
   USE sms_c14    ! c14 simulation type, atm default values...

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_atm_c14         ! called in trcsms_c14.F90
   PUBLIC   trc_atm_c14_ini     ! called in trcini_c14.F90
   !
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcatm_c14.F90 10069 2018-08-28 14:12:24Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_atm_c14_ini
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_c14_ini  ***
      !!                    
      !! ** Purpose :   initialisation of sbc for radiocarbon
      !! 
      !! ** Method  : 
      !!----------------------------------------------------------------------
      !
      CHARACTER (len=20)        :: clfile                        ! forcing file name
      INTEGER                   :: ji,jj,jn                        ! dummy loop indice
      INTEGER                   :: ierr1,ierr2,ierr3,izco2         ! temporary integers
      INTEGER                   :: inum1,inum2,incom,iyear         ! temporary integers
      REAL(wp) ::   ys40 = -40.     ! 40 degrees south
      REAL(wp) ::   ys20 = -20.     ! 20 degrees south
      REAL(wp) ::   yn20 =  20.     ! 20 degrees north
      REAL(wp) ::   yn40 =  40.     ! 40 degrees north
      REAL(wp), ALLOCATABLE, DIMENSION(:)   :: zco2, zyrco2  ! temporary arrays for swap
      !
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_atm_c14_ini')
      !
      
      IF( lwp ) WRITE(numout,*) ' '
      IF( lwp ) WRITE(numout,*) ' trc_atm_c14_ini : initialize atm CO2 & C14-ratio '
      IF( lwp ) WRITE(numout,*) ' '
      !
      tyrc14_now = 0._wp   ! initialize
      !
      IF(kc14typ >= 1) THEN  ! Transient atmospheric forcing: CO2
      !
         clfile = TRIM( cfileco2 )
         IF(lwp) WRITE(numout,*) 'Read CO2 atmospheric concentrations file ',clfile
         CALL ctl_opn( inum1, clfile, 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
         REWIND(inum1)
      
         READ(inum1,*) nrecco2,incom
         DO jn = 1, incom        ! Skip over descriptor lines
            READ(inum1,'(1x)')
         END DO
         ALLOCATE( spco2(nrecco2), tyrco2(nrecco2)       , STAT=ierr1 )
         IF( ierr1 /= 0 )   CALL ctl_stop( 'STOP', 'trc_atm_c14_ini: unable to allocate co2 arrays' )
      ! get  CO2 data
         DO jn = 1, nrecco2
            READ(inum1, *)  tyrco2(jn), spco2(jn)
         END DO
         CLOSE(inum1)
      !
         IF(kc14typ==2) THEN
            ALLOCATE( zco2(nrecco2), zyrco2(nrecco2) )
            zco2(:)=spco2(:)
            zyrco2(:)=tyrco2(:)
      ! Set CO2 times on AD time scale & swap records : In CO2 file : youngest first
            DO jn = 1, nrecco2
               izco2=nrecco2-jn+1
               spco2(izco2)=zco2(jn)
               tyrco2(izco2)=1950._wp-zyrco2(jn)         ! BP to AD dates
            END DO
            DEALLOCATE( zco2,zyrco2 )
         ENDIF
      !
      !        ! Transient atmospheric forcing: Bomb C14 & Paleo C14 : open file
      !
         clfile = TRIM( cfilec14 )
         IF (lwp) WRITE(numout,*) 'Read C-14 atmospheric concentrations file ',clfile
         CALL ctl_opn( inum2, clfile, 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
         REWIND(inum2)
      !
      ! Bomb C14: 3 zones for atm C14 !
         IF(kc14typ == 1) THEN  ! Transient atmospheric forcing: Bomb C14
      ! 
            READ(inum2,*) nrecc14,incom
            DO jn = 1, incom        ! Skip over descriptor lines
               READ(inum2,'(1x)')
            END DO
            ALLOCATE( bomb(nrecc14,nc14zon), tyrc14(nrecc14)       , STAT=ierr2 )
            IF( ierr2 /= 0 )   CALL ctl_stop( 'STOP', 'trc_atm_c14_ini: unable to allocate c14 arrays' )
      ! get bomb c14 data
            DO jn = 1, nrecc14
               READ(inum2,*) tyrc14(jn), bomb(jn,1), bomb(jn,2), bomb(jn,3)
            END DO
            CLOSE(inum2)

       ! Linear  interpolation of the C-14 source fonction
       ! in linear latitude bands  (20N,40N) and (20S,40S)
       !------------------------------------------------------
            ALLOCATE( fareaz  (jpi,jpj ,nc14zon) , STAT=ierr3 )
            IF( ierr3 /= 0 )   CALL ctl_stop( 'STOP', 'trc_atm_c14_ini: unable to allocate fareaz' )
      !
            DO jj = 1 , jpj                       ! from C14b package
              DO ji = 1 , jpi
                 IF( gphit(ji,jj) >= yn40 ) THEN
                    fareaz(ji,jj,1) = 0.
                    fareaz(ji,jj,2) = 0.
                    fareaz(ji,jj,3) = 1.
                 ELSE IF( gphit(ji,jj ) <= ys40) THEN
                    fareaz(ji,jj,1) = 1.
                    fareaz(ji,jj,2) = 0.
                    fareaz(ji,jj,3) = 0.
                 ELSE IF( gphit(ji,jj) >= yn20 ) THEN
                    fareaz(ji,jj,1) = 0.
                    fareaz(ji,jj,2) = 2. * ( 1. - gphit(ji,jj) / yn40 )
                    fareaz(ji,jj,3) = 2. * gphit(ji,jj) / yn40 - 1.
                 ELSE IF( gphit(ji,jj) <= ys20 ) THEN
                    fareaz(ji,jj,1) = 2. * gphit(ji,jj) / ys40 - 1.
                    fareaz(ji,jj,2) = 2. * ( 1. - gphit(ji,jj) / ys40 )
                    fareaz(ji,jj,3) = 0.
                 ELSE
                    fareaz(ji,jj,1) = 0.
                    fareaz(ji,jj,2) = 1.
                    fareaz(ji,jj,3) = 0.
                 ENDIF
              END DO
           END DO
      !
         ENDIF
      !
      ! Paleo C14: 1 zone for atm C14 !
         IF(kc14typ == 2) THEN  ! Transient atmospheric forcing: Paleo C14
      ! 
            READ(inum2,*) nrecc14,incom
            DO jn = 1, incom        ! Skip over descriptor lines
               READ(inum2,'(1x)')
            END DO
            ALLOCATE( atmc14(nrecc14), tyrc14(nrecc14)       , STAT=ierr2 )
            IF( ierr2 /= 0 )   CALL ctl_stop( 'STOP', 'trc_atm_c14_ini: unable to allocate c14 arrays' )
      ! get past c14 data
            DO jn = 1, nrecc14
               READ(inum2,*) iyear,incom,incom,atmc14(jn)
               tyrc14(jn)=1950._wp-REAL(iyear,wp)                ! BP to AD dates
            END DO
            CLOSE(inum2)
      !
         ENDIF
      !
      ! Note on dates: 
      !   In files dates have dimension yr; either AD or BP; if BP date is changed into AD here
      !   When dealing with dates previous to 0. AD one needs to set tyrc14_beg to the actual starting year
      !   Do not forget to appropriately set nn_date0 and nn_rstctl in namelist
      !                           AND        nn_rsttr in namelist_top  if offline run
      !   All details are given in NEMO-C14.pdf report
      !  
         tyrc14_now=nyear                                ! actual initial yr - Bomb 
         if(kc14typ == 2) tyrc14_now=nyear+tyrc14_beg-1  ! actual initial yr - Paleo
      !                                     ! we suppose we start on tyrc14_now/01/01 @ 0h
         m1_c14= 1
         m1_co2= 1
         DO jn = 1,nrecco2
            IF ( tyrc14_now >= tyrco2(jn) ) m1_co2 = jn  ! index of first co2 record to consider
         END DO
         DO jn = 1,nrecc14
            IF ( tyrc14_now >= tyrc14(jn) ) m1_c14 = jn  ! index of first c14 record to consider
         END DO
         IF (lwp) WRITE(numout,*) 'Initial yr for experiment', tyrc14_now
         IF (lwp) WRITE(numout,*) '   CO2 & C14 start years:', tyrco2(m1_co2),tyrc14(m1_c14)
      !
         m2_c14= m1_c14
         m2_co2= m1_co2
      !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_atm_c14_ini')
      !
   END SUBROUTINE trc_atm_c14_ini


   SUBROUTINE trc_atm_c14( kt, co2sbc, c14sbc )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_flx  ***
      !!                    
      !! ** Purpose :   provides sbc for co2 & c14 at kt
      !! 
      !! ** Method  :  read files
      !!
      !! ** Action  :   atmospheric values interpolated at time-step kt
      !!----------------------------------------------------------------------
      INTEGER                 , INTENT(in   )   ::   kt       ! ocean time-step
      REAL(wp), DIMENSION(:,:), INTENT(  out)   ::   c14sbc   ! atm c14 ratio
      REAL(wp),                 INTENT(  out)   ::   co2sbc   ! atm co2 p
      INTEGER                                   ::   jz       ! dummy loop indice
      REAL(wp)                              ::   zdint,zint   ! work
      REAL(wp), DIMENSION(nc14zon)              ::   zonbc14  ! work
      !
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_atm_c14')
      !
      IF( kc14typ == 0) THEN
         co2sbc=pco2at
         c14sbc(:,:)=rc14at
      ENDIF
      !
      IF(kc14typ >= 1) THEN  ! Transient C14 & CO2
      !
         tyrc14_now = tyrc14_now + ( rdt / ( rday * nyear_len(1)) )    !  current time step in yr relative to tyrc14_beg
      !
      ! CO2 --------------------------------------------------------
      !
      ! time interpolation of CO2 concentrations     ! if out of record keeps first/last value
         IF( tyrc14_now > tyrco2(m2_co2) ) THEN     ! next interval
           m1_co2 = m2_co2
           m2_co2 = MIN ( m2_co2 + 1 , nrecco2 )
         ENDIF
      !
         zdint  = tyrco2(m2_co2) - tyrco2(m1_co2)
         co2sbc = spco2(m2_co2)                        ! if out of record keeps first/last value
         zint = 0._wp
         IF ( zdint > 0._wp ) THEN                     ! if within record interpolate:
            zint = ( tyrco2(m2_co2) - tyrc14_now ) / zdint
            co2sbc = spco2(m2_co2) + zint * ( spco2(m1_co2) - spco2(m2_co2) )
         ENDIF
      !
         IF( lwp .AND.  kt == nitend )  THEN
            WRITE(numout, '(3(A,F12.4))') 't1/tn/t2:',tyrco2(m1_co2),'/', tyrc14_now,'/',tyrco2(m2_co2)
            WRITE(numout, *) 'CO2:',spco2(m1_co2),co2sbc ,spco2(m2_co2)
         ENDIF
      !
      ! C14 --------------------------------------------------------
      !
      ! time interpolation of C14 concentrations 
         IF ( tyrc14_now > tyrc14(m2_c14) ) THEN     ! next interval
           m1_c14 = m2_c14
           m2_c14 = MIN ( m2_c14 + 1 , nrecc14 )
         ENDIF
         zdint = tyrc14(m2_c14) - tyrc14(m1_c14)
         zint=0._wp
         IF ( zdint > 0._wp ) zint = ( tyrc14(m2_c14) - tyrc14_now ) / zdint ! if  within record
         IF( lwp .AND.  kt == nitend )  &
            &     WRITE(numout,'(3(A,F12.4))') 't1/tn/t2:',tyrc14(m1_c14),'/', tyrc14_now,'/',tyrc14(m2_c14)
      !
      ! ------- Bomb C14  ------------------------------------------
      !
         IF( kc14typ == 1) THEN
      !                                          ! time interpolation
          zonbc14(:) = bomb(m2_c14,:)                ! if out of record keeps first/last value
      !                                          !    if within record interpolate:
          IF ( zdint > 0._wp ) zonbc14(:) = bomb(m2_c14,:) + zint * ( bomb(m1_c14,:)  - bomb(m2_c14,:)  )
      ! 
            IF(lwp .AND.  kt == nitend )  &
                 &      WRITE(numout, *)  'C14:',bomb(m1_c14,1),zonbc14(1),bomb(m2_c14,1)
      !   Transform DeltaC14 --> C14 ratio
            zonbc14(:) = 1._wp + zonbc14(:)/1.d03
      !
      !  For each (i,j)-box, with information from the fractional area
      !  (zonmean), computes area-weighted mean to give the atmospheric C-14
      !  ----------------------------------------------------------------
            c14sbc(:,:) = zonbc14(1) * fareaz(:,:,1)   &
               &          + zonbc14(2) * fareaz(:,:,2)   &
               &          + zonbc14(3) * fareaz(:,:,3)
         ENDIF
      !
      ! ------- Paleo C14  -----------------------------------------
      !
         IF( kc14typ == 2 ) THEN
      !                                          ! time interpolation
            zonbc14(1) = atmc14(m2_c14)          ! if out of record keeps first/last value
         !                                       !    if within record interpolate:
            IF ( zdint > 0._wp ) zonbc14(1) = atmc14(m2_c14) + zint * ( atmc14(m1_c14) - atmc14(m2_c14) )
            IF(lwp .AND.  kt == nitend )  &
                 &      WRITE(numout, *)  'C14: ',atmc14(m1_c14),zonbc14(1),atmc14(m2_c14)
      !   Transform DeltaC14 --> C14 ratio
            c14sbc(:,:) = 1._wp + zonbc14(1)/1.d03
         ENDIF
      !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_atm_c14')
      !
   END SUBROUTINE trc_atm_c14

   !!======================================================================
END MODULE trcatm_c14
