MODULE p2zopt
   !!======================================================================
   !!                         ***  MODULE p2zopt  ***
   !! TOP :   LOBSTER Compute the light availability in the water column
   !!======================================================================
   !! History :    -   !  1995-05  (M. Levy) Original code
   !!              -   !  1999-09  (J.-M. Andre, M. Levy) 
   !!              -   !  1999-11  (C. Menkes, M.-A. Foujols) itabe initial
   !!              -   !  2000-02  (M.A. Foujols) change x**y par exp(y*log(x))
   !!   NEMO      2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!             3.2  !  2009-04  (C. Ethe, G. Madec)  minor optimisation + style
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   p2z_opt        :   Compute the light availability in the water column
   !!----------------------------------------------------------------------
   USE oce_trc         !
   USE trc
   USE sms_pisces
   USE prtctl_trc      ! Print control for debbuging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p2z_opt   !
   PUBLIC   p2z_opt_init   !

   REAL(wp), PUBLIC ::  xkr0      !: water coefficient absorption in red      
   REAL(wp), PUBLIC ::  xkg0      !: water coefficient absorption in green    
   REAL(wp), PUBLIC ::  xkrp      !: pigment coefficient absorption in red    
   REAL(wp), PUBLIC ::  xkgp      !: pigment coefficient absorption in green  
   REAL(wp), PUBLIC ::  xlr       !: exposant for pigment absorption in red  
   REAL(wp), PUBLIC ::  xlg       !: exposant for pigment absorption in green 
   REAL(wp), PUBLIC ::  rpig      !: chla/chla+phea ratio    
   !                 
   REAL(wp), PUBLIC ::  rcchl     ! Carbone/Chlorophyl ratio [mgC.mgChla-1]
   REAL(wp), PUBLIC ::  redf      ! redfield ratio (C:N) for phyto
   REAL(wp), PUBLIC ::  reddom    ! redfield ratio (C:N) for DOM

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p2zopt.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_opt( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_opt  ***
      !!
      !! ** Purpose :   computes the light propagation in the water column
      !!              and the euphotic layer depth
      !!
      !! ** Method  :   local par is computed in w layers using light propagation
      !!              mean par in t layers are computed by integration
      !!
!!gm please remplace the '???' by true comments
      !! ** Action  :   etot   ???
      !!                neln   ???
      !!---------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt   ! index of the time stepping
      !!
      INTEGER  ::   ji, jj, jk          ! dummy loop indices
      CHARACTER (len=25) ::   charout   ! temporary character
      REAL(wp) ::   zpig                ! log of the total pigment
      REAL(wp) ::   zkr, zkg            ! total absorption coefficient in red and green
      REAL(wp) ::   zcoef               ! temporary scalar
      REAL(wp), DIMENSION(jpi,jpj    ) :: zpar100, zpar0m
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zparr, zparg
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_opt')
      !

      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' p2z_opt : LOBSTER optic-model'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~ '
      ENDIF

      !                                          ! surface irradiance
      !                                          ! ------------------
      IF( ln_dm2dc ) THEN   ;   zpar0m(:,:) = qsr_mean(:,:) * 0.43
      ELSE                  ;   zpar0m(:,:) = qsr     (:,:) * 0.43
      ENDIF
      zpar100(:,:)   = zpar0m(:,:) * 0.01
      zparr  (:,:,1) = zpar0m(:,:) * 0.5
      zparg  (:,:,1) = zpar0m(:,:) * 0.5

      !                                          ! Photosynthetically Available Radiation (PAR)
      zcoef = 12 * redf / rcchl / rpig           ! --------------------------------------
      DO jk = 2, jpk                                  ! local par at w-levels
         DO jj = 1, jpj
            DO ji = 1, jpi
               zpig = LOG(  MAX( TINY(0.), trn(ji,jj,jk-1,jpphy) ) * zcoef  )
               zkr  = xkr0 + xkrp * EXP( xlr * zpig )
               zkg  = xkg0 + xkgp * EXP( xlg * zpig )
               zparr(ji,jj,jk) = zparr(ji,jj,jk-1) * EXP( -zkr * e3t_n(ji,jj,jk-1) )
               zparg(ji,jj,jk) = zparg(ji,jj,jk-1) * EXP( -zkg * e3t_n(ji,jj,jk-1) )
            END DO
        END DO
      END DO
      DO jk = 1, jpkm1                                ! mean par at t-levels
         DO jj = 1, jpj
            DO ji = 1, jpi
               zpig = LOG(  MAX( TINY(0.), trn(ji,jj,jk,jpphy) ) * zcoef  )
               zkr  = xkr0 + xkrp * EXP( xlr * zpig )
               zkg  = xkg0 + xkgp * EXP( xlg * zpig )
               zparr(ji,jj,jk) = zparr(ji,jj,jk) / ( zkr * e3t_n(ji,jj,jk) ) * ( 1 - EXP( -zkr * e3t_n(ji,jj,jk) ) )
               zparg(ji,jj,jk) = zparg(ji,jj,jk) / ( zkg * e3t_n(ji,jj,jk) ) * ( 1 - EXP( -zkg * e3t_n(ji,jj,jk) ) )
               etot (ji,jj,jk) = MAX( zparr(ji,jj,jk) + zparg(ji,jj,jk), 1.e-15 )
            END DO
         END DO
      END DO

      !                                          ! Euphotic layer
      !                                          ! --------------
      neln(:,:) = 1                                   ! euphotic layer level
      DO jk = 1, jpkm1                                ! (i.e. 1rst T-level strictly below EL bottom)
         DO jj = 1, jpj
           DO ji = 1, jpi
              IF( etot(ji,jj,jk) >= zpar100(ji,jj) )   neln(ji,jj) = jk + 1 
           END DO
         END DO
      END DO
      !                                               ! Euphotic layer depth
      DO jj = 1, jpj
         DO ji = 1, jpi
            heup(ji,jj) = gdepw_n(ji,jj,neln(ji,jj))
         END DO
      END DO 


      IF(ln_ctl) THEN      ! print mean trends (used for debugging)
         WRITE(charout, FMT="('opt')")
         CALL prt_ctl_trc_info( charout )
         CALL prt_ctl_trc( tab4d=trn, mask=tmask, clinfo=ctrcnm )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p2z_opt')
      !
   END SUBROUTINE p2z_opt


   SUBROUTINE p2z_opt_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p2z_opt_init  ***
      !!
      !! ** Purpose :  optical parameters 
      !!
      !! ** Method  :   Read the namlobopt namelist and check the parameters
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !!
      NAMELIST/namlobopt/ xkg0, xkr0, xkgp, xkrp, xlg, xlr, rpig
      NAMELIST/namlobrat/ rcchl, redf, reddom
      !!----------------------------------------------------------------------

      REWIND( numnatp_ref )              ! Namelist namlobopt in reference namelist : Lobster options
      READ  ( numnatp_ref, namlobopt, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namlobopt in reference namelist' )

      REWIND( numnatp_cfg )              ! Namelist namlobopt in configuration namelist : Lobster options
      READ  ( numnatp_cfg, namlobopt, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namlobopt in configuration namelist' )
      IF(lwm) WRITE ( numonp, namlobopt )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' Namelist namlobopt'
         WRITE(numout,*) '    green   water absorption coeff                       xkg0  = ', xkg0
         WRITE(numout,*) '    red water absorption coeff                           xkr0  = ', xkr0
         WRITE(numout,*) '    pigment red absorption coeff                         xkrp  = ', xkrp
         WRITE(numout,*) '    pigment green absorption coeff                       xkgp  = ', xkgp
         WRITE(numout,*) '    green chl exposant                                   xlg   = ', xlg
         WRITE(numout,*) '    red   chl exposant                                   xlr   = ', xlr
         WRITE(numout,*) '    chla/chla+phea ratio                                 rpig  = ', rpig
         WRITE(numout,*) ' '
      ENDIF
      !
      REWIND( numnatp_ref )              ! Namelist namlobrat in reference namelist : Lobster ratios
      READ  ( numnatp_ref, namlobrat, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namlobrat in reference namelist' )

      REWIND( numnatp_cfg )              ! Namelist namlobrat in configuration namelist : Lobster ratios
      READ  ( numnatp_cfg, namlobrat, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 ) CALL ctl_nam ( ios , 'namlobrat in configuration namelist' )
      IF(lwm) WRITE ( numonp, namlobrat )

      IF(lwp) THEN
          WRITE(numout,*) ' Namelist namlobrat'
         WRITE(numout,*) '     carbone/chlorophyl ratio                             rcchl = ', rcchl
          WRITE(numout,*) '    redfield ratio  c:n for phyto                        redf      =', redf
          WRITE(numout,*) '    redfield ratio  c:n for DOM                          reddom    =', reddom
          WRITE(numout,*) ' '
      ENDIF
      !
   END SUBROUTINE p2z_opt_init

   !!======================================================================
END MODULE p2zopt
