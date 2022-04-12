MODULE trcdta
   !!======================================================================
   !!                     ***  MODULE  trcdta  ***
   !! TOP :  reads passive tracer data 
   !!=====================================================================
   !! History :   1.0  !  2002-04  (O. Aumont)  original code
   !!              -   !  2004-03  (C. Ethe)  module
   !!              -   !  2005-03  (O. Aumont, A. El Moussaoui) F90
   !!            3.4   !  2010-11  (C. Ethe, G. Madec)  use of fldread + dynamical allocation 
   !!            3.5   !  2013-08  (M. Vichi)  generalization for other BGC models
   !!            3.6   !  2015-03  (T. Lovato) revisit code I/O
   !!----------------------------------------------------------------------
#if defined key_top 
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP model 
   !!----------------------------------------------------------------------
   !!   trc_dta    : read and time interpolated passive tracer data
   !!----------------------------------------------------------------------
   USE par_trc       !  passive tracers parameters
   USE oce_trc       !  shared variables between ocean and passive tracers
   USE trc           !  passive tracers common variables
   !
   USE iom           !  I/O manager
   USE lib_mpp       !  MPP library
   USE fldread       !  read input fields

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_dta         ! called in trcini.F90 and trcdmp.F90
   PUBLIC   trc_dta_ini     ! called in trcini.F90 

   INTEGER  , SAVE, PUBLIC                             :: nb_trcdta   ! number of tracers to be initialised with data
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: n_trc_index ! indice of tracer which is initialised with data
   INTEGER  , SAVE, PUBLIC                             :: ntra        ! MAX( 1, nb_trcdta ) to avoid compilation error with bounds checking
   REAL(wp) , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: rf_trfac    ! multiplicative factor for tracer values
!$AGRIF_DO_NOT_TREAT
   TYPE(FLD), SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: sf_trcdta   ! structure of input SST (file informations, fields read)
!$AGRIF_END_DO_NOT_TREAT

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcdta.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_dta_ini(ntrc)
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dta_ini  ***
      !!                    
      !! ** Purpose :   initialisation of passive tracer input data 
      !! 
      !! ** Method  : - Read namtsd namelist
      !!              - allocates passive tracer data structure 
      !!----------------------------------------------------------------------
      INTEGER,INTENT(in) ::   ntrc   ! number of tracers
      !
      INTEGER ::   jl, jn                            ! dummy loop indices
      INTEGER ::   ios, ierr0, ierr1, ierr2, ierr3   ! local integers
      REAL(wp) ::   zfact
      CHARACTER(len=100) ::   clndta, clntrc
      !
      CHARACTER(len=100) ::   cn_dir
      TYPE(FLD_N), ALLOCATABLE, DIMENSION(:) :: slf_i ! array of namelist informations on the fields to read
      TYPE(FLD_N), DIMENSION(jpmaxtrc) :: sn_trcdta
      REAL(wp)   , DIMENSION(jpmaxtrc) :: rn_trfac    ! multiplicative factor for tracer values
      !!
      NAMELIST/namtrc_dta/ sn_trcdta, cn_dir, rn_trfac 
      !!----------------------------------------------------------------------
      !
      IF( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trc_dta_ini : Tracers Initial Conditions (IC)'
         WRITE(numout,*) '~~~~~~~~~~~ '
      ENDIF
      !
      !  Initialisation
      ierr0 = 0  ;  ierr1 = 0  ;  ierr2 = 0  ;  ierr3 = 0  
      ! Compute the number of tracers to be initialised with data
      ALLOCATE( n_trc_index(ntrc), slf_i(ntrc), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_dta_ini: unable to allocate n_trc_index' )   ;   RETURN
      ENDIF
      nb_trcdta      = 0
      n_trc_index(:) = 0
      DO jn = 1, ntrc
         IF( ln_trc_ini(jn) ) THEN
             nb_trcdta       = nb_trcdta + 1 
             n_trc_index(jn) = nb_trcdta 
         ENDIF
      END DO
      !
      ntra = MAX( 1, nb_trcdta )   ! To avoid compilation error with bounds checking
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   number of passive tracers to be initialize by data :', ntra
      ENDIF
      !
      REWIND( numnat_ref )              ! Namelist namtrc_dta in reference namelist : Passive tracer input data
      READ  ( numnat_ref, namtrc_dta, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namtrc_dta_ini in reference namelist' )
      REWIND( numnat_cfg )              ! Namelist namtrc_dta in configuration namelist : Passive tracer input data
      READ  ( numnat_cfg, namtrc_dta, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namtrc_dta_ini in configuration namelist' )
      IF(lwm) WRITE ( numont, namtrc_dta )

      IF( lwp ) THEN
         DO jn = 1, ntrc
            IF( ln_trc_ini(jn) )  THEN    ! open input file only if ln_trc_ini(jn) is true
               clndta = TRIM( sn_trcdta(jn)%clvar ) 
               clntrc = TRIM( ctrcnm   (jn)       ) 
               if (jn > jptra) clntrc='Dummy' ! By pass weird formats in ocean.output if ntrc > jptra
               zfact  = rn_trfac(jn)
               IF( clndta /=  clntrc ) THEN 
                  CALL ctl_warn( 'trc_dta_ini: passive tracer data initialisation    ',   &
                  &              'Input name of data file : '//TRIM(clndta)//   &
                  &              ' differs from that of tracer : '//TRIM(clntrc)//' ')
               ENDIF
               WRITE(numout,*)
               WRITE(numout,'(a, i4,3a,e11.3)') '   Read IC file for tracer number :', &
               &            jn, ', name : ', TRIM(clndta), ', Multiplicative Scaling factor : ', zfact
            ENDIF
         END DO
      ENDIF
      !
      IF( nb_trcdta > 0 ) THEN       !  allocate only if the number of tracer to initialise is greater than zero
         ALLOCATE( sf_trcdta(nb_trcdta), rf_trfac(nb_trcdta), STAT=ierr1 )
         IF( ierr1 > 0 ) THEN
            CALL ctl_stop( 'trc_dta_ini: unable to allocate  sf_trcdta structure' )   ;   RETURN
         ENDIF
         !
         DO jn = 1, ntrc
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
               jl = n_trc_index(jn)
               slf_i(jl)    = sn_trcdta(jn)
               rf_trfac(jl) = rn_trfac(jn)
                                            ALLOCATE( sf_trcdta(jl)%fnow(jpi,jpj,jpk)   , STAT=ierr2 )
               IF( sn_trcdta(jn)%ln_tint )  ALLOCATE( sf_trcdta(jl)%fdta(jpi,jpj,jpk,2) , STAT=ierr3 )
               IF( ierr2 + ierr3 > 0 ) THEN
                 CALL ctl_stop( 'trc_dta_ini : unable to allocate passive tracer data arrays' )   ;   RETURN
               ENDIF
            ENDIF
            !   
         ENDDO
         !                         ! fill sf_trcdta with slf_i and control print
         CALL fld_fill( sf_trcdta, slf_i, cn_dir, 'trc_dta_ini', 'Passive tracer data', 'namtrc' )
         !
      ENDIF
      !
      DEALLOCATE( slf_i )          ! deallocate local field structure
      !
   END SUBROUTINE trc_dta_ini


   SUBROUTINE trc_dta( kt, sf_trcdta, ptrcfac, ptrcdta)
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dta  ***
      !!                    
      !! ** Purpose :   provides passive tracer data at kt
      !! 
      !! ** Method  : - call fldread routine
      !!              - s- or mixed z-s coordinate: vertical interpolation on model mesh
      !!              - ln_trcdmp=F: deallocates the data structure as they are not used
      !!
      !! ** Action  :   sf_trcdta   passive tracer data on meld mesh and interpolated at time-step kt
      !!----------------------------------------------------------------------
      INTEGER                          , INTENT(in   )   ::   kt         ! ocean time-step
      TYPE(FLD), DIMENSION(1)          , INTENT(inout)   ::   sf_trcdta  ! array of information on the field to read
      REAL(wp)                         , INTENT(in   )   ::   ptrcfac    ! multiplication factor
      REAL(wp),  DIMENSION(jpi,jpj,jpk), INTENT(inout  ) ::   ptrcdta    ! 3D data array
      !
      INTEGER ::   ji, jj, jk, jl, jkk, ik    ! dummy loop indices
      REAL(wp)::   zl, zi
      REAL(wp), DIMENSION(jpk) ::  ztp                ! 1D workspace
      CHARACTER(len=100) :: clndta
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_dta')
      !
      IF( nb_trcdta > 0 ) THEN
         !
         ! read data at kt time step
         CALL fld_read( kt, 1, sf_trcdta )
         ptrcdta(:,:,:) = sf_trcdta(1)%fnow(:,:,:) * tmask(:,:,:)
         ! 
         IF( ln_sco ) THEN                !== s- or mixed s-zps-coordinate  ==!
            !
            IF( kt == nit000 .AND. lwp )THEN
               WRITE(numout,*)
               WRITE(numout,*) 'trc_dta: interpolates passive tracer data onto the s- or mixed s-z-coordinate mesh'
            ENDIF
            DO jj = 1, jpj                         ! vertical interpolation of T & S
               DO ji = 1, jpi
                  DO jk = 1, jpk                        ! determines the intepolated T-S profiles at each (i,j) points
                     zl = gdept_n(ji,jj,jk)
                     IF(     zl < gdept_1d(1  ) ) THEN         ! above the first level of data
                        ztp(jk) = ptrcdta(ji,jj,1)
                     ELSEIF( zl > gdept_1d(jpk) ) THEN         ! below the last level of data
                        ztp(jk) = ptrcdta(ji,jj,jpkm1)
                     ELSE                                      ! inbetween : vertical interpolation between jkk & jkk+1
                        DO jkk = 1, jpkm1                                  ! when  gdept(jkk) < zl < gdept(jkk+1)
                           IF( (zl-gdept_1d(jkk)) * (zl-gdept_1d(jkk+1)) <= 0._wp ) THEN
                              zi = ( zl - gdept_1d(jkk) ) / (gdept_1d(jkk+1)-gdept_1d(jkk))
                              ztp(jk) = ptrcdta(ji,jj,jkk) + ( ptrcdta(ji,jj,jkk+1) - ptrcdta(ji,jj,jkk) ) * zi
                           ENDIF
                        END DO
                     ENDIF
                  END DO
                  DO jk = 1, jpkm1
                     ptrcdta(ji,jj,jk) = ztp(jk) * tmask(ji,jj,jk)     ! mask required for mixed zps-s-coord
                  END DO
                  ptrcdta(ji,jj,jpk) = 0._wp
                END DO
            END DO
            ! 
         ELSE                                !==   z- or zps- coordinate   ==!
            ! zps-coordinate (partial steps) interpolation at the last ocean level
!            IF( ln_zps ) THEN
!               DO jj = 1, jpj
!                  DO ji = 1, jpi
!                     ik = mbkt(ji,jj) 
!                     IF( ik > 1 ) THEN
!                        zl = ( gdept_1d(ik) - gdept_0(ji,jj,ik) ) / ( gdept_1d(ik) - gdept_1d(ik-1) )
!                        ptrcdta(ji,jj,ik) = (1.-zl) * ptrcdta(ji,jj,ik) + zl * ptrcdta(ji,jj,ik-1)
!                     ENDIF
!                     ik = mikt(ji,jj)
!                     IF( ik > 1 ) THEN
!                        zl = ( gdept_0(ji,jj,ik) - gdept_1d(ik) ) / ( gdept_1d(ik+1) - gdept_1d(ik) )
!                        ptrcdta(ji,jj,ik) = (1.-zl) * ptrcdta(ji,jj,ik) + zl * ptrcdta(ji,jj,ik+1)
!                     ENDIF
!                  END DO
!              END DO
!            ENDIF
            !
         ENDIF
         !
         ! Scale by multiplicative factor
         ptrcdta(:,:,:) = ptrcdta(:,:,:) * ptrcfac
         !
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('trc_dta')
      !
   END SUBROUTINE trc_dta

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                              NO 3D passive tracer data
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_dta( kt, sf_trcdta, ptrcfac, ptrcdta)        ! Empty routine
      WRITE(*,*) 'trc_dta: You should not have seen this print! error?', kt
   END SUBROUTINE trc_dta
#endif

   !!======================================================================
END MODULE trcdta
