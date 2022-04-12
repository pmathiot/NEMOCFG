MODULE usrdef_sbc
   !!======================================================================
   !!                     ***  MODULE  usrdef_sbc  ***
   !!
   !!                     ===  GYRE configuration  ===
   !!
   !! User defined :   surface forcing of a user configuration
   !!======================================================================
   !! History :  4.0   ! 2016-03  (S. Flavoni, G. Madec)  user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usrdef_sbc    : user defined surface bounday conditions in GYRE case
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distribued memory computing library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_fortran    !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usrdef_sbc_oce       ! routine called in sbcmod module
   PUBLIC   usrdef_sbc_ice_tau   ! routine called by icestp.F90 for ice dynamics
   PUBLIC   usrdef_sbc_ice_flx   ! routine called by icestp.F90 for ice thermo

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_sbc.F90 10425 2018-12-19 21:54:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usrdef_sbc_oce( kt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE usrdef_sbc  ***
      !!              
      !! ** Purpose :   provide at each time-step the GYRE surface boundary
      !!              condition, i.e. the momentum, heat and freshwater fluxes.
      !!
      !! ** Method  :   analytical seasonal cycle for GYRE configuration.
      !!                CAUTION : never mask the surface stress field !
      !!
      !! ** Action  : - set the ocean surface boundary condition, i.e.   
      !!                   utau, vtau, taum, wndm, qns, qsr, emp, sfx
      !!
      !! Reference : Hazeleger, W., and S. Drijfhout, JPO, 30, 677-695, 2000.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !!
      INTEGER  ::   ji, jj                 ! dummy loop indices
      INTEGER  ::   zyear0                 ! initial year 
      INTEGER  ::   zmonth0                ! initial month
      INTEGER  ::   zday0                  ! initial day
      INTEGER  ::   zday_year0             ! initial day since january 1st
      REAL(wp) ::   ztau     , ztau_sais   ! wind intensity and of the seasonal cycle
      REAL(wp) ::   ztime                  ! time in hour
      REAL(wp) ::   ztimemax , ztimemin    ! 21th June, and 21th decem. if date0 = 1st january
      REAL(wp) ::   ztimemax1, ztimemin1   ! 21th June, and 21th decem. if date0 = 1st january
      REAL(wp) ::   ztimemax2, ztimemin2   ! 21th June, and 21th decem. if date0 = 1st january
      REAL(wp) ::   ztaun                  ! intensity
      REAL(wp) ::   zemp_s, zemp_n, zemp_sais, ztstar
      REAL(wp) ::   zcos_sais1, zcos_sais2, ztrp, zconv, t_star
      REAL(wp) ::   zsumemp, zsurf
      REAL(wp) ::   zrhoa  = 1.22         ! Air density kg/m3
      REAL(wp) ::   zcdrag = 1.5e-3       ! drag coefficient
      REAL(wp) ::   ztx, zty, zmod, zcoef ! temporary variables
      REAL(wp) ::   zyydd                 ! number of days in one year
      !!---------------------------------------------------------------------
      zyydd = REAL(nyear_len(1),wp)

      ! ---------------------------- !
      !  heat and freshwater fluxes  !
      ! ---------------------------- !
      !same temperature, E-P as in HAZELEGER 2000

      zyear0     =   ndate0 / 10000                             ! initial year
      zmonth0    = ( ndate0 - zyear0 * 10000 ) / 100            ! initial month
      zday0      =   ndate0 - zyear0 * 10000 - zmonth0 * 100    ! initial day betwen 1 and 30
      zday_year0 = ( zmonth0 - 1 ) * 30.+zday0                  ! initial day betwen 1 and 360

      ! current day (in hours) since january the 1st of the current year
      ztime = REAL( kt ) * rdt / (rmmss * rhhmm)   &       !  total incrementation (in hours)
         &      - (nyear  - 1) * rjjhh * zyydd             !  minus years since beginning of experiment (in hours)

      ztimemax1 = ((5.*30.)+21.)* 24.                      ! 21th june     at 24h in hours
      ztimemin1 = ztimemax1 + rjjhh * zyydd / 2            ! 21th december        in hours
      ztimemax2 = ((6.*30.)+21.)* 24.                      ! 21th july     at 24h in hours
      ztimemin2 = ztimemax2 - rjjhh * zyydd / 2            ! 21th january         in hours
      !                                                    ! NB: rjjhh * zyydd / 4 = one seasonal cycle in hours

      ! amplitudes
      zemp_S    = 0.7       ! intensity of COS in the South
      zemp_N    = 0.8       ! intensity of COS in the North
      zemp_sais = 0.1
      zTstar    = 28.3      ! intemsity from 28.3 a -5 deg

      ! 1/2 period between 21th June and 21th December and between 21th July and 21th January
      zcos_sais1 = COS( (ztime - ztimemax1) / (ztimemin1 - ztimemax1) * rpi ) 
      zcos_sais2 = COS( (ztime - ztimemax2) / (ztimemax2 - ztimemin2) * rpi )

      ztrp= - 40.e0        ! retroaction term on heat fluxes (W/m2/K)
      zconv = 3.16e-5      ! convertion factor: 1 m/yr => 3.16e-5 mm/s
      DO jj = 1, jpj
         DO ji = 1, jpi
            ! domain from 15 deg to 50 deg between 27 and 28  degC at 15N, -3
            ! and 13 degC at 50N 53.5 + or - 11 = 1/4 period :
            ! 64.5 in summer, 42.5 in winter
            t_star = zTstar * ( 1. + 1. / 50. * zcos_sais2 )                &
               &                    * COS( rpi * (gphit(ji,jj) - 5.)               &
               &                    / ( 53.5 * ( 1 + 11 / 53.5 * zcos_sais2 ) * 2.) )
            ! 23.5 deg : tropics
            qsr (ji,jj) =  230 * COS( 3.1415 * ( gphit(ji,jj) - 23.5 * zcos_sais1 ) / ( 0.9 * 180 ) )
            qns (ji,jj) = ztrp * ( tsb(ji,jj,1,jp_tem) - t_star ) - qsr(ji,jj)
            IF( gphit(ji,jj) >= 14.845 .AND. 37.2 >= gphit(ji,jj) ) THEN    ! zero at 37.8 deg, max at 24.6 deg
               emp  (ji,jj) =   zemp_S * zconv   &
                  &         * SIN( rpi / 2 * (gphit(ji,jj) - 37.2) / (24.6 - 37.2) )  &
                  &         * ( 1 - zemp_sais / zemp_S * zcos_sais1)
            ELSE
               emp (ji,jj) =  - zemp_N * zconv   &
                  &         * SIN( rpi / 2 * (gphit(ji,jj) - 37.2) / (46.8 - 37.2) )  &
                  &         * ( 1 - zemp_sais / zemp_N * zcos_sais1 )
            ENDIF
         END DO
      END DO

      zsumemp = GLOB_SUM( 'usrdef_sbc', emp  (:,:)   ) 
      zsurf   = GLOB_SUM( 'usrdef_sbc', tmask(:,:,1) ) 
      zsumemp = zsumemp / zsurf         ! Default GYRE configuration

      ! freshwater (mass flux) and update of qns with heat content of emp
      emp (:,:) = emp(:,:) - zsumemp * tmask(:,:,1)        ! freshwater flux (=0 in domain average)
      sfx (:,:) = 0.0_wp                                   ! no salt flux
      qns (:,:) = qns(:,:) - emp(:,:) * sst_m(:,:) * rcp   ! evap and precip are at SST


      ! ---------------------------- !
      !       momentum fluxes        !
      ! ---------------------------- !
      ! same wind as in Wico
      !test date0 : ndate0 = 010203
      zyear0  =   ndate0 / 10000
      zmonth0 = ( ndate0 - zyear0 * 10000 ) / 100
      zday0   =   ndate0 - zyear0 * 10000 - zmonth0 * 100
      !Calculates nday_year, day since january 1st
      zday_year0 = (zmonth0-1)*30.+zday0

      !accumulates days of previous months of this year
      ! day (in hours) since january the 1st
      ztime = FLOAT( kt ) * rdt / (rmmss * rhhmm)  &  ! incrementation in hour
         &     - (nyear - 1) * rjjhh * zyydd          !  - nber of hours the precedent years
      ztimemax = ((5.*30.)+21.)* 24.               ! 21th june     in hours
      ztimemin = ztimemax + rjjhh * zyydd / 2      ! 21th december in hours
      !                                            ! NB: rjjhh * zyydd / 4 = 1 seasonal cycle in hours

      ! mean intensity at 0.105 ; srqt(2) because projected with 45deg angle
      ztau = 0.105 / SQRT( 2. )
      ! seasonal oscillation intensity
      ztau_sais = 0.015
      ztaun = ztau - ztau_sais * COS( (ztime - ztimemax) / (ztimemin - ztimemax) * rpi )
      DO jj = 1, jpj
         DO ji = 1, jpi
           ! domain from 15deg to 50deg and 1/2 period along 14deg
           ! so 5/4 of half period with seasonal cycle
           utau(ji,jj) = - ztaun * SIN( rpi * (gphiu(ji,jj) - 15.) / (29.-15.) )
           vtau(ji,jj) =   ztaun * SIN( rpi * (gphiv(ji,jj) - 15.) / (29.-15.) )
         END DO
      END DO

      ! module of wind stress and wind speed at T-point
      zcoef = 1. / ( zrhoa * zcdrag ) 
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vect. opt.
            ztx = utau(ji-1,jj  ) + utau(ji,jj) 
            zty = vtau(ji  ,jj-1) + vtau(ji,jj) 
            zmod = 0.5 * SQRT( ztx * ztx + zty * zty )
            taum(ji,jj) = zmod
            wndm(ji,jj) = SQRT( zmod * zcoef )
         END DO
      END DO
      CALL lbc_lnk_multi( 'usrdef_sbc', taum(:,:), 'T', 1. , wndm(:,:), 'T', 1. )

      ! ---------------------------------- !
      !  control print at first time-step  !
      ! ---------------------------------- !
      IF( kt == nit000 .AND. lwp ) THEN 
         WRITE(numout,*)
         WRITE(numout,*)'usrdef_sbc_oce : analytical surface fluxes for GYRE configuration'               
         WRITE(numout,*)'~~~~~~~~~~~ ' 
         WRITE(numout,*)'           nyear      = ', nyear
         WRITE(numout,*)'           nmonth     = ', nmonth
         WRITE(numout,*)'           nday       = ', nday
         WRITE(numout,*)'           nday_year  = ', nday_year
         WRITE(numout,*)'           ztime      = ', ztime
         WRITE(numout,*)'           ztimemax   = ', ztimemax
         WRITE(numout,*)'           ztimemin   = ', ztimemin
         WRITE(numout,*)'           ztimemax1  = ', ztimemax1
         WRITE(numout,*)'           ztimemin1  = ', ztimemin1
         WRITE(numout,*)'           ztimemax2  = ', ztimemax2
         WRITE(numout,*)'           ztimemin2  = ', ztimemin2
         WRITE(numout,*)'           zyear0     = ', zyear0
         WRITE(numout,*)'           zmonth0    = ', zmonth0
         WRITE(numout,*)'           zday0      = ', zday0
         WRITE(numout,*)'           zday_year0 = ', zday_year0
         WRITE(numout,*)'           zyydd      = ', zyydd
         WRITE(numout,*)'           zemp_S     = ', zemp_S
         WRITE(numout,*)'           zemp_N     = ', zemp_N
         WRITE(numout,*)'           zemp_sais  = ', zemp_sais
         WRITE(numout,*)'           zTstar     = ', zTstar
         WRITE(numout,*)'           zsumemp    = ', zsumemp
         WRITE(numout,*)'           zsurf      = ', zsurf
         WRITE(numout,*)'           ztrp       = ', ztrp
         WRITE(numout,*)'           zconv      = ', zconv
         WRITE(numout,*)'           ndastp     = ', ndastp
         WRITE(numout,*)'           adatrj     = ', adatrj
      ENDIF
      !
   END SUBROUTINE usrdef_sbc_oce


   SUBROUTINE usrdef_sbc_ice_tau( kt )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
   END SUBROUTINE usrdef_sbc_ice_tau


   SUBROUTINE usrdef_sbc_ice_flx( kt, phs, phi )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phs    ! snow thickness
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phi    ! ice thickness
   END SUBROUTINE usrdef_sbc_ice_flx

   !!======================================================================
END MODULE usrdef_sbc
