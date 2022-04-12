MODULE trcsink
   !!======================================================================
   !!                         ***  MODULE trcsink  ***
   !! TOP :  vertical flux of particulate matter due to gravitational sinking
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Change aggregation formula
   !!             3.5  !  2012-07  (O. Aumont) Introduce potential time-splitting
   !!             4.0  !  2018-12  (O. Aumont) Generalize the PISCES code to make it usable by any model
   !!----------------------------------------------------------------------
   !!   trc_sink       :  Compute vertical flux of particulate matter due to gravitational sinking
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_sink
   PUBLIC trc_sink_ini

   INTEGER, PUBLIC :: nitermax      !: Maximum number of iterations for sinking

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcsink.F90 10069 2018-08-28 14:12:24Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!   'standard sinking parameterisation'                  ???
   !!----------------------------------------------------------------------

   SUBROUTINE trc_sink ( kt, pwsink, psinkflx, jp_tra, rsfact )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sink  ***
      !!
      !! ** Purpose :   Compute vertical flux of particulate matter due to 
      !!                gravitational sinking
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER , INTENT(in)  :: kt
      INTEGER , INTENT(in)  :: jp_tra    ! tracer index index      
      REAL(wp), INTENT(in)  :: rsfact    ! time step duration
      REAL(wp), INTENT(in)   , DIMENSION(jpi,jpj,jpk) :: pwsink
      REAL(wp), INTENT(inout), DIMENSION(jpi,jpj,jpk) :: psinkflx
      INTEGER  ::   ji, jj, jk
      INTEGER, DIMENSION(jpi, jpj) ::   iiter
      REAL(wp) ::   zfact, zwsmax, zmax
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zwsink
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sink')
      !
      !
      ! OA This is (I hope) a temporary solution for the problem that may 
      ! OA arise in specific situation where the CFL criterion is broken 
      ! OA for vertical sedimentation of particles. To avoid this, a time
      ! OA splitting algorithm has been coded. A specific maximum
      ! OA iteration number is provided and may be specified in the namelist 
      ! OA This is to avoid very large iteration number when explicit free
      ! OA surface is used (for instance). When niter?max is set to 1, 
      ! OA this computation is skipped. The crude old threshold method is 
      ! OA then applied. This also happens when niter exceeds nitermax.
      IF( nitermax == 1 ) THEN
         iiter(:,:) = 1
      ELSE
         DO jj = 1, jpj
            DO ji = 1, jpi
               iiter(ji,jj) = 1
               DO jk = 1, jpkm1
                  IF( tmask(ji,jj,jk) == 1.0 ) THEN
                      zwsmax =  0.5 * e3t_n(ji,jj,jk) * rday / rsfact
                      iiter(ji,jj) =  MAX( iiter(ji,jj), INT( pwsink(ji,jj,jk) / zwsmax ) )
                  ENDIF
               END DO
            END DO
         END DO
         iiter(:,:) = MIN( iiter(:,:), nitermax )
      ENDIF

      DO jk = 1,jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( tmask(ji,jj,jk) == 1.0 ) THEN
                 zwsmax = 0.5 * e3t_n(ji,jj,jk) * rday / rsfact
                 zwsink(ji,jj,jk) = MIN( pwsink(ji,jj,jk), zwsmax * REAL( iiter(ji,jj), wp ) )
               ELSE
                 ! provide a default value so there is no use of undefinite value in trc_sink2 for zwsink2 initialization
                 zwsink(ji,jj,jk) = 0.
               ENDIF
            END DO
         END DO
      END DO

      !  Initializa to zero all the sinking arrays 
      !  -----------------------------------------
      psinkflx(:,:,:) = 0.e0

      !   Compute the sedimentation term using trc_sink2 for the considered sinking particle
      !   -----------------------------------------------------
      CALL trc_sink2( zwsink, psinkflx, jp_tra, iiter, rsfact )
      !
      IF( ln_timing )   CALL timing_stop('trc_sink')
      !
   END SUBROUTINE trc_sink

   SUBROUTINE trc_sink2( pwsink, psinkflx, jp_tra, kiter, rsfact )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sink2  ***
      !!
      !! ** Purpose :   Compute the sedimentation terms for the various sinking
      !!     particles. The scheme used to compute the trends is based
      !!     on MUSCL.
      !!
      !! ** Method  : - this ROUTINE compute not exactly the advection but the
      !!      transport term, i.e.  div(u*tra).
      !!---------------------------------------------------------------------
      INTEGER,  INTENT(in   )                         ::   jp_tra    ! tracer index index      
      REAL(wp), INTENT(in   )                         ::   rsfact    ! duration of time step
      INTEGER,  INTENT(in   ), DIMENSION(jpi,jpj)     ::   kiter     ! number of iterations for time-splitting 
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj,jpk) ::   pwsink    ! sinking speed
      REAL(wp), INTENT(inout), DIMENSION(jpi,jpj,jpk) ::   psinkflx  ! sinking fluxe
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zigma,zew,zign, zflx, zstep
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ztraz, zakz, zwsink2, ztrb 
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sink2')
      !
      ztraz(:,:,:) = 0.e0
      zakz (:,:,:) = 0.e0
      ztrb (:,:,:) = trb(:,:,:,jp_tra)

      DO jk = 1, jpkm1
         zwsink2(:,:,jk+1) = -pwsink(:,:,jk) / rday * tmask(:,:,jk+1) 
      END DO
      zwsink2(:,:,1) = 0.e0


      ! Vertical advective flux
      DO jn = 1, 2
         !  first guess of the slopes interior values
         DO jj = 1, jpj
            DO ji = 1, jpi
               !
               zstep = rsfact / REAL( kiter(ji,jj), wp ) / 2.
               !              
               DO jk = 2, jpkm1
                  ztraz(ji,jj,jk) = ( trb(ji,jj,jk-1,jp_tra) - trb(ji,jj,jk,jp_tra) ) * tmask(ji,jj,jk)
               END DO
               ztraz(ji,jj,1  ) = 0.0
               ztraz(ji,jj,jpk) = 0.0

               ! slopes
               DO jk = 2, jpkm1
                  zign = 0.25 + SIGN( 0.25, ztraz(ji,jj,jk) * ztraz(ji,jj,jk+1) )
                  zakz(ji,jj,jk) = ( ztraz(ji,jj,jk) + ztraz(ji,jj,jk+1) ) * zign
               END DO
         
               ! Slopes limitation
               DO jk = 2, jpkm1
                  zakz(ji,jj,jk) = SIGN( 1., zakz(ji,jj,jk) ) *        &
                     &             MIN( ABS( zakz(ji,jj,jk) ), 2. * ABS(ztraz(ji,jj,jk+1)), 2. * ABS(ztraz(ji,jj,jk) ) )
               END DO
         
               ! vertical advective flux
               DO jk = 1, jpkm1
                  zigma = zwsink2(ji,jj,jk+1) * zstep / e3w_n(ji,jj,jk+1)
                  zew   = zwsink2(ji,jj,jk+1)
                  psinkflx(ji,jj,jk+1) = -zew * ( trb(ji,jj,jk,jp_tra) - 0.5 * ( 1 + zigma ) * zakz(ji,jj,jk) ) * zstep
               END DO
               !
               ! Boundary conditions
               psinkflx(ji,jj,1  ) = 0.e0
               psinkflx(ji,jj,jpk) = 0.e0
         
               DO jk=1,jpkm1
                  zflx = ( psinkflx(ji,jj,jk) - psinkflx(ji,jj,jk+1) ) / e3t_n(ji,jj,jk)
                  trb(ji,jj,jk,jp_tra) = trb(ji,jj,jk,jp_tra) + zflx
               END DO
            END DO
         END DO
      END DO

      DO jk = 1,jpkm1
         DO jj = 1,jpj
            DO ji = 1, jpi
               zflx = ( psinkflx(ji,jj,jk) - psinkflx(ji,jj,jk+1) ) / e3t_n(ji,jj,jk)
               ztrb(ji,jj,jk) = ztrb(ji,jj,jk) + 2. * zflx
            END DO
         END DO
      END DO

      trb(:,:,:,jp_tra) = ztrb(:,:,:)
      psinkflx(:,:,:)   = 2. * psinkflx(:,:,:)
      !
      IF( ln_timing )  CALL timing_stop('trc_sink2')
      !
   END SUBROUTINE trc_sink2

  SUBROUTINE trc_sink_ini
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sink_ini ***
      !!
      !! ** Purpose :   read  namelist options 
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer output status for namelist read
      !!
      NAMELIST/namtrc_snk/ nitermax
      !!----------------------------------------------------------------------
      !
      REWIND( numnat_ref )              ! namtrc_rad in reference namelist 
      READ  ( numnat_ref, namtrc_snk, IOSTAT = ios, ERR = 907)
907   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namtrc_snk in reference namelist' )
      REWIND( numnat_cfg )              ! namtrc_rad in configuration namelist 
      READ  ( numnat_cfg, namtrc_snk, IOSTAT = ios, ERR = 908 )
908   IF( ios > 0 )   CALL ctl_nam ( ios , 'namtrc_snk in configuration namelist' )
      IF(lwm) WRITE( numont, namtrc_snk )

      IF(lwp) THEN                     !   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_sink : Sedimentation of particles '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namtrc_snk : sedimentation of particles'
         WRITE(numout,*) '   Maximum number of iterations   nitermax = ', nitermax
         WRITE(numout,*)
      ENDIF

   END SUBROUTINE trc_sink_ini

   !!======================================================================
END MODULE trcsink
