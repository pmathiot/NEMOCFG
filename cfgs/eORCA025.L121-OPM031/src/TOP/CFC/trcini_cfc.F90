MODULE trcini_cfc
   !!======================================================================
   !!                         ***  MODULE trcini_cfc  ***
   !! TOP :   initialisation of the CFC tracers
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) 
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! trc_ini_cfc      : CFC model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcnam_cfc      ! CFC SMS namelist
   USE trcsms_cfc      ! CFC sms trends

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_cfc   ! called by trcini.F90 module

   INTEGER  ::   inum                   ! unit number
   REAL(wp) ::   ylats = -10.           ! 10 degrees south
   REAL(wp) ::   ylatn =  10.           ! 10 degrees north

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcini_cfc.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ini_cfc
      !!----------------------------------------------------------------------
      !!                     ***  trc_ini_cfc  ***  
      !!
      !! ** Purpose :   initialization for cfc model
      !!
      !! ** Method  : - Read the namcfc namelist and check the parameter values
      !!----------------------------------------------------------------------
      INTEGER  ::  ji, jj, jn, jl, jm, js, io, ierr
      INTEGER  ::  iskip = 6   ! number of 1st descriptor lines
      REAL(wp) ::  zyy, zyd
      CHARACTER(len = 20)  ::  cltra
      !!----------------------------------------------------------------------
      !
      CALL trc_nam_cfc
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_ini_cfc: initialisation of CFC chemical model'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~'
      !
      IF(lwp) WRITE(numout,*) 'Read annual atmospheric concentratioins from formatted file : ' // TRIM(clname)
      
      CALL ctl_opn( inum, clname, 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      REWIND(inum)
      
      ! compute the number of year in the file
      ! file starts in 1931 do jn represent the year in the century
      jn = 31 
      DO 
        READ(inum,'(1x)',END=100) 
        jn = jn + 1
      END DO
 100  jpyear = jn - 1 - iskip
      IF ( lwp) WRITE(numout,*) '   --->  ', jpyear ,' years read'
      !                                ! Allocate CFC arrays

      ALLOCATE( p_cfc(jpyear,jphem,3), STAT=ierr )
      IF( ierr > 0 ) THEN
         CALL ctl_stop( 'trc_ini_cfc: unable to allocate p_cfc array' )   ;   RETURN
      ENDIF
      IF( trc_sms_cfc_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_ini_cfc: unable to allocate CFC arrays' )


      ! Initialization of boundaries conditions
      ! --------------------------------------- 
      xphem (:,:)    = 0._wp
      p_cfc(:,:,:)   = 0._wp
      
      ! Initialization of qint in case of  no restart 
      !----------------------------------------------
      qtr_cfc(:,:,:) = 0._wp
      IF( .NOT. ln_rsttr ) THEN    
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'Initialisation of qint ; No restart : qint equal zero '
         ENDIF
         qint_cfc(:,:,:) = 0._wp
         DO jl = 1, jp_cfc
            jn = jp_cfc0 + jl - 1
            trn(:,:,:,jn) = 0._wp
         END DO
      ENDIF

      REWIND(inum)
      
      DO jm = 1, iskip        ! Skip over 1st six descriptor lines
         READ(inum,'(1x)')
      END DO
      ! file starts in 1931 do jn represent the year in the century.jhh
      ! Read file till the end
      jn = 31
      DO 
        READ(inum,*, IOSTAT=io) zyy, p_cfc(jn,1:2,1), p_cfc(jn,1:2,2), p_cfc(jn,1:2,3)
        IF( io < 0 ) exit
        jn = jn + 1
      END DO

      !p_cfc(32,1:2,1) = 5.e-4      ! modify the values of the first years
      !p_cfc(33,1:2,1) = 8.e-4
      !p_cfc(34,1:2,1) = 1.e-6
      !p_cfc(35,1:2,1) = 2.e-3
      !p_cfc(36,1:2,1) = 4.e-3
      !p_cfc(37,1:2,1) = 6.e-3
      !p_cfc(38,1:2,1) = 8.e-3
      !p_cfc(39,1:2,1) = 1.e-2
      IF(lwp) THEN        ! Control print
         WRITE(numout,*)
         WRITE(numout,*) ' Year   c11NH     c11SH     c12NH     c12SH     SF6NH     SF6SH'
         DO jn = 30, jpyear
            WRITE(numout, '( 1I4, 6F10.4)') jn, p_cfc(jn,1:2,1), p_cfc(jn,1:2,2), p_cfc(jn,1:2,3)
         END DO
      ENDIF


      ! Interpolation factor of atmospheric partial pressure
      ! Linear interpolation between 2 hemispheric function of latitud between ylats and ylatn
      !---------------------------------------------------------------------------------------
      zyd = ylatn - ylats      
      DO jj = 1 , jpj
         DO ji = 1 , jpi
            IF(     gphit(ji,jj) >= ylatn ) THEN   ;   xphem(ji,jj) = 1.e0
            ELSEIF( gphit(ji,jj) <= ylats ) THEN   ;   xphem(ji,jj) = 0.e0
            ELSE                                   ;   xphem(ji,jj) = ( gphit(ji,jj) - ylats) / zyd
            ENDIF
         END DO
      END DO
      !
      IF(lwp) WRITE(numout,*) 'Initialization of CFC tracers done'
      IF(lwp) WRITE(numout,*) ' '
      !
   END SUBROUTINE trc_ini_cfc

   !!======================================================================
END MODULE trcini_cfc
