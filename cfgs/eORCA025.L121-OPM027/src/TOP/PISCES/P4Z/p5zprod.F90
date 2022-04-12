MODULE p5zprod
   !!======================================================================
   !!                         ***  MODULE p5zprod  ***
   !! TOP :  Growth Rate of the two phytoplanktons groups 
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-05  (O. Aumont, C. Ethe) New parameterization of light limitation
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_prod       :   Compute the growth Rate of the two phytoplanktons groups
   !!   p5z_prod_init  :   Initialization of the parameters for growth
   !!   p5z_prod_alloc :   Allocate variables for growth
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zlim
   USE p5zlim          !  Co-limitations of differents nutrients
   USE prtctl_trc      !  print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_prod         ! called in p5zbio.F90
   PUBLIC   p5z_prod_init    ! called in trcsms_pisces.F90
   PUBLIC   p5z_prod_alloc

   !! * Shared module variables
   REAL(wp), PUBLIC ::  pislopen        !:
   REAL(wp), PUBLIC ::  pislopep        !:
   REAL(wp), PUBLIC ::  pisloped        !:
   REAL(wp), PUBLIC ::  xadap           !:
   REAL(wp), PUBLIC ::  excretn         !:
   REAL(wp), PUBLIC ::  excretp         !:
   REAL(wp), PUBLIC ::  excretd         !:
   REAL(wp), PUBLIC ::  bresp           !:
   REAL(wp), PUBLIC ::  thetanpm        !:
   REAL(wp), PUBLIC ::  thetannm        !:
   REAL(wp), PUBLIC ::  thetandm        !:
   REAL(wp), PUBLIC ::  chlcmin         !:
   REAL(wp), PUBLIC ::  grosip          !:

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   zdaylen
   
   REAL(wp) :: r1_rday                !: 1 / rday
   REAL(wp) :: texcretn               !: 1 - excret 
   REAL(wp) :: texcretp               !: 1 - excretp 
   REAL(wp) :: texcretd               !: 1 - excret2        

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zprod.F90 12280 2019-12-21 10:42:44Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p5z_prod( kt , knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_prod  ***
      !!
      !! ** Purpose :   Compute the phytoplankton production depending on
      !!              light, temperature and nutrient availability
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) :: kt, knt
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zsilfac, znanotot, zpicotot, zdiattot, zconctemp, zconctemp2
      REAL(wp) ::   zration, zratiop, zratiof, zmax, zmax2, zsilim, ztn, zadap
      REAL(wp) ::   zpronmax, zpropmax, zprofmax, zrat
      REAL(wp) ::   zlim, zsilfac2, zsiborn, zprod, zprontot, zproptot, zprodtot
      REAL(wp) ::   zprnutmax, zdocprod, zprochln, zprochld, zprochlp
      REAL(wp) ::   zpislopen, zpislopep, zpisloped, thetannm_n, thetandm_n, thetanpm_n
      REAL(wp) ::   zrum, zcodel, zargu, zval, zfeup
      REAL(wp) ::   zfact, zrfact2
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(jpi,jpj    ) :: zmixnano, zmixpico, zmixdiat, zstrn
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpislopeadn, zpislopeadp, zpislopeadd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprnut, zprmaxp, zprmaxn, zprmaxd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprbio, zprpic, zprdia, zysopt
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprchln, zprchlp, zprchld
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprorcan, zprorcap, zprorcad 
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprofed, zprofep, zprofen
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpronewn, zpronewp, zpronewd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zproregn, zproregp, zproregd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpropo4n, zpropo4p, zpropo4d
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprodopn, zprodopp, zprodopd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zrespn, zrespp, zrespd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zcroissn, zcroissp, zcroissd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zmxl_fac, zmxl_chl
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpligprod1, zpligprod2
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_prod')
      !
      zprorcan(:,:,:) = 0._wp ; zprorcap(:,:,:) = 0._wp ; zprorcad(:,:,:) = 0._wp
      zcroissn(:,:,:) = 0._wp ; zcroissp(:,:,:) = 0._wp ; zcroissd(:,:,:) = 0._wp
      zprofed (:,:,:) = 0._wp ; zprofep (:,:,:) = 0._wp ; zprofen (:,:,:) = 0._wp
      zpronewn(:,:,:) = 0._wp ; zpronewp(:,:,:) = 0._wp ; zpronewd(:,:,:) = 0._wp
      zproregn(:,:,:) = 0._wp ; zproregp(:,:,:) = 0._wp ; zproregd(:,:,:) = 0._wp 
      zpropo4n(:,:,:) = 0._wp ; zpropo4p(:,:,:) = 0._wp ; zpropo4d(:,:,:) = 0._wp
      zprdia  (:,:,:) = 0._wp ; zprpic  (:,:,:) = 0._wp ; zprbio  (:,:,:) = 0._wp
      zprodopn(:,:,:) = 0._wp ; zprodopp(:,:,:) = 0._wp ; zprodopd(:,:,:) = 0._wp
      zysopt  (:,:,:) = 0._wp 
      zrespn  (:,:,:) = 0._wp ; zrespp  (:,:,:) = 0._wp ; zrespd  (:,:,:) = 0._wp 

      ! Computation of the optimal production
      zprnut (:,:,:) = 0.65_wp * r1_rday * tgfunc(:,:,:)
      zprmaxn(:,:,:) = ( 0.65_wp * (1. + zpsino3 * qnpmax ) ) * r1_rday * tgfunc(:,:,:)
      zprmaxp(:,:,:) = 0.5 / 0.65 * zprmaxn(:,:,:) 
      zprmaxd(:,:,:) = zprmaxn(:,:,:) 

      ! compute the day length depending on latitude and the day
      zrum = REAL( nday_year - 80, wp ) / REAL( nyear_len(1), wp )
      zcodel = ASIN(  SIN( zrum * rpi * 2._wp ) * SIN( rad * 23.5_wp )  )

      ! day length in hours
      zstrn(:,:) = 0.
      DO jj = 1, jpj
         DO ji = 1, jpi
            zargu = TAN( zcodel ) * TAN( gphit(ji,jj) * rad )
            zargu = MAX( -1., MIN(  1., zargu ) )
            zstrn(ji,jj) = MAX( 0.0, 24. - 2. * ACOS( zargu ) / rad / 15. )
         END DO
      END DO

         ! Impact of the day duration on phytoplankton growth
      DO jk = 1, jpkm1
         DO jj = 1 ,jpj
            DO ji = 1, jpi
               IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                  zval = MAX( 1., zstrn(ji,jj) )
                  IF( gdepw_n(ji,jj,jk+1) <= hmld(ji,jj) ) THEN
                     zval = zval * MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
                  ENDIF
                  zmxl_chl(ji,jj,jk) = zval / 24.
                  zmxl_fac(ji,jj,jk) = 1.5 * zval / ( 12. + zval )
               ENDIF
            END DO
         END DO
      END DO

      zprbio(:,:,:) = zprmaxn(:,:,:) * zmxl_fac(:,:,:)
      zprdia(:,:,:) = zprmaxd(:,:,:) * zmxl_fac(:,:,:)
      zprpic(:,:,:) = zprmaxp(:,:,:) * zmxl_fac(:,:,:)


      ! Maximum light intensity
      zdaylen(:,:) = MAX(1., zstrn(:,:)) / 24.
      WHERE( zstrn(:,:) < 1.e0 ) zstrn(:,:) = 24.

      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                  ! Computation of the P-I slope for nanos and diatoms
                  ztn         = MAX( 0., tsn(ji,jj,jk,jp_tem) - 15. )
                  zadap       = xadap * ztn / ( 2.+ ztn )
                  !
                  zpislopeadn(ji,jj,jk) = pislopen * trb(ji,jj,jk,jpnch)    &
                  &                       /( trb(ji,jj,jk,jpphy) * 12. + rtrn)
                  zpislopeadp(ji,jj,jk) = pislopep * ( 1. + zadap * EXP( -0.25 * epico(ji,jj,jk) ) )   &
                  &                       * trb(ji,jj,jk,jppch) /( trb(ji,jj,jk,jppic) * 12. + rtrn)
                  zpislopeadd(ji,jj,jk) = pisloped * trb(ji,jj,jk,jpdch)    &
                     &                    /( trb(ji,jj,jk,jpdia) * 12. + rtrn)
                  !
                  zpislopen = zpislopeadn(ji,jj,jk) / ( zprbio(ji,jj,jk) * rday * xlimphy(ji,jj,jk) + rtrn )
                  zpislopep = zpislopeadp(ji,jj,jk) / ( zprpic(ji,jj,jk) * rday * xlimpic(ji,jj,jk) + rtrn )
                  zpisloped = zpislopeadd(ji,jj,jk) / ( zprdia(ji,jj,jk) * rday * xlimdia(ji,jj,jk) + rtrn )

                  ! Computation of production function for Carbon
                  !  ---------------------------------------------
                  zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * ( 1.- EXP( -zpislopen * enano(ji,jj,jk) )  )
                  zprpic(ji,jj,jk) = zprpic(ji,jj,jk) * ( 1.- EXP( -zpislopep * epico(ji,jj,jk) )  )
                  zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * ( 1.- EXP( -zpisloped * ediat(ji,jj,jk) )  )

                  ! Computation of production function for Chlorophyll
                  !  -------------------------------------------------
                  zpislopen = zpislopen * zmxl_fac(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
                  zpisloped = zpisloped * zmxl_fac(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
                  zpislopep = zpislopep * zmxl_fac(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
                  zprchln(ji,jj,jk) = zprmaxn(ji,jj,jk) * ( 1.- EXP( -zpislopen * enanom(ji,jj,jk) )  )
                  zprchlp(ji,jj,jk) = zprmaxp(ji,jj,jk) * ( 1.- EXP( -zpislopep * epicom(ji,jj,jk) )  )
                  zprchld(ji,jj,jk) = zprmaxd(ji,jj,jk) * ( 1.- EXP( -zpisloped * ediatm(ji,jj,jk) )  )
               ENDIF
            END DO
         END DO
      END DO

      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi

                IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                  !    Si/C of diatoms
                  !    ------------------------
                  !    Si/C increases with iron stress and silicate availability
                  !    Si/C is arbitrariliy increased for very high Si concentrations
                  !    to mimic the very high ratios observed in the Southern Ocean (silpot2)
                  zlim  = trb(ji,jj,jk,jpsil) / ( trb(ji,jj,jk,jpsil) + xksi1 )
                  zsilim = MIN( zprdia(ji,jj,jk) / ( zprmaxd(ji,jj,jk) + rtrn ), xlimsi(ji,jj,jk) )
                  zsilfac = 3.4 * EXP( -4.23 * zsilim ) * MAX( 0.e0, MIN( 1., 2.2 * ( zlim - 0.5 ) )  ) + 1.e0
                  zsiborn = trb(ji,jj,jk,jpsil) * trb(ji,jj,jk,jpsil) * trb(ji,jj,jk,jpsil)
                  IF (gphit(ji,jj) < -30 ) THEN
                    zsilfac2 = 1. + 2. * zsiborn / ( zsiborn + xksi2**3 )
                  ELSE
                    zsilfac2 = 1. +      zsiborn / ( zsiborn + xksi2**3 )
                  ENDIF
                  zysopt(ji,jj,jk) = grosip * zlim * zsilfac * zsilfac2
              ENDIF
            END DO
         END DO
      END DO

      !  Sea-ice effect on production                                                                               
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zprbio(ji,jj,jk)  = zprbio(ji,jj,jk) * ( 1. - fr_i(ji,jj) )
               zprpic(ji,jj,jk)  = zprpic(ji,jj,jk) * ( 1. - fr_i(ji,jj) ) 
               zprdia(ji,jj,jk)  = zprdia(ji,jj,jk) * ( 1. - fr_i(ji,jj) ) 
               zprnut(ji,jj,jk)  = zprnut(ji,jj,jk) * ( 1. - fr_i(ji,jj) )
            END DO
         END DO
      END DO

      ! Computation of the various production terms of nanophytoplankton 
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                  !  production terms for nanophyto.
                  zprorcan(ji,jj,jk) = zprbio(ji,jj,jk)  * xlimphy(ji,jj,jk) * trb(ji,jj,jk,jpphy) * rfact2
                  !
                  zration = trb(ji,jj,jk,jpnph) / ( trb(ji,jj,jk,jpphy) + rtrn )
                  zratiop = trb(ji,jj,jk,jppph) / ( trb(ji,jj,jk,jpphy) + rtrn )
                  zratiof = trb(ji,jj,jk,jpnfe) / ( trb(ji,jj,jk,jpphy) + rtrn )
                  zprnutmax = zprnut(ji,jj,jk) * fvnuptk(ji,jj,jk) / rno3 * trb(ji,jj,jk,jpphy) * rfact2
                  ! Uptake of nitrogen
                  zrat = MIN( 1., zration / (xqnnmax(ji,jj,jk) + rtrn) ) 
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zpronmax = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpnmin(ji,jj,jk) )   &
                  &          / ( xqpnmax(ji,jj,jk) - xqpnmin(ji,jj,jk) + rtrn ), xlimnfe(ji,jj,jk) ) )
                  zpronewn(ji,jj,jk) = zpronmax * zdaylen(ji,jj) * xnanono3(ji,jj,jk)
                  zproregn(ji,jj,jk) = zpronmax * xnanonh4(ji,jj,jk)
                  ! Uptake of phosphorus
                  zrat = MIN( 1., zratiop / (xqpnmax(ji,jj,jk) + rtrn) )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zpropmax = zprnutmax * zmax * xlimnfe(ji,jj,jk)
                  zpropo4n(ji,jj,jk) = zpropmax * xnanopo4(ji,jj,jk)
                  zprodopn(ji,jj,jk) = zpropmax * xnanodop(ji,jj,jk)
                  ! Uptake of iron
                  zrat = MIN( 1., zratiof / qfnmax )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zprofmax = zprnutmax * qfnmax * zmax
                  zprofen(ji,jj,jk) = zprofmax * xnanofer(ji,jj,jk) * ( 3. - 2.4 * xlimnfe(ji,jj,jk)    &
                  &          / ( xlimnfe(ji,jj,jk) + 0.2 ) ) * (1. + 0.8 * xnanono3(ji,jj,jk) / ( rtrn  &
                  &          + xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk) ) * (1. - xnanofer(ji,jj,jk) ) )
               ENDIF
            END DO
         END DO
      END DO

      ! Computation of the various production terms of picophytoplankton 
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                  !  production terms for picophyto.
                  zprorcap(ji,jj,jk) = zprpic(ji,jj,jk)  * xlimpic(ji,jj,jk) * trb(ji,jj,jk,jppic) * rfact2
                  !
                  zration = trb(ji,jj,jk,jpnpi) / ( trb(ji,jj,jk,jppic) + rtrn )
                  zratiop = trb(ji,jj,jk,jpppi) / ( trb(ji,jj,jk,jppic) + rtrn )
                  zratiof = trb(ji,jj,jk,jppfe) / ( trb(ji,jj,jk,jppic) + rtrn )
                  zprnutmax = zprnut(ji,jj,jk) * fvpuptk(ji,jj,jk) / rno3 * trb(ji,jj,jk,jppic) * rfact2
                  ! Uptake of nitrogen
                  zrat = MIN( 1., zration / (xqnpmax(ji,jj,jk) + rtrn) )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zpronmax = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqppmin(ji,jj,jk) )   &
                  &          / ( xqppmax(ji,jj,jk) - xqppmin(ji,jj,jk) + rtrn ), xlimpfe(ji,jj,jk) ) )
                  zpronewp(ji,jj,jk) = zpronmax * zdaylen(ji,jj) * xpicono3(ji,jj,jk) 
                  zproregp(ji,jj,jk) = zpronmax * xpiconh4(ji,jj,jk)
                  ! Uptake of phosphorus
                  zrat = MIN( 1., zratiop / (xqppmax(ji,jj,jk) + rtrn) )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zpropmax = zprnutmax * zmax * xlimpfe(ji,jj,jk)
                  zpropo4p(ji,jj,jk) = zpropmax * xpicopo4(ji,jj,jk)
                  zprodopp(ji,jj,jk) = zpropmax * xpicodop(ji,jj,jk)
                  ! Uptake of iron
                  zrat = MIN( 1., zratiof / qfpmax )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zprofmax = zprnutmax * qfpmax * zmax
                  zprofep(ji,jj,jk) = zprofmax * xpicofer(ji,jj,jk) * ( 3. - 2.4 * xlimpfe(ji,jj,jk)   &
                  &          / ( xlimpfe(ji,jj,jk) + 0.2 ) ) * (1. + 0.8 * xpicono3(ji,jj,jk) / ( rtrn   &
                  &          + xpicono3(ji,jj,jk) + xpiconh4(ji,jj,jk) ) * (1. - xpicofer(ji,jj,jk) ) )
               ENDIF
            END DO
         END DO
      END DO

      ! Computation of the various production terms of diatoms
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                  !  production terms for diatomees
                  zprorcad(ji,jj,jk) = zprdia(ji,jj,jk) * xlimdia(ji,jj,jk) * trb(ji,jj,jk,jpdia) * rfact2
                  ! Computation of the respiration term according to pahlow 
                  ! & oschlies (2013)
                  !
                  zration = trb(ji,jj,jk,jpndi) / ( trb(ji,jj,jk,jpdia) + rtrn )
                  zratiop = trb(ji,jj,jk,jppdi) / ( trb(ji,jj,jk,jpdia) + rtrn )
                  zratiof = trb(ji,jj,jk,jpdfe) / ( trb(ji,jj,jk,jpdia) + rtrn )
                  zprnutmax = zprnut(ji,jj,jk) * fvduptk(ji,jj,jk) / rno3 * trb(ji,jj,jk,jpdia) * rfact2
                  ! Uptake of nitrogen
                  zrat = MIN( 1., zration / (xqndmax(ji,jj,jk) + rtrn) )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05)) 
                  zpronmax = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpdmin(ji,jj,jk) )   &
                  &          / ( xqpdmax(ji,jj,jk) - xqpdmin(ji,jj,jk) + rtrn ), xlimdfe(ji,jj,jk) ) )
                  zpronewd(ji,jj,jk) = zpronmax * zdaylen(ji,jj) * xdiatno3(ji,jj,jk)
                  zproregd(ji,jj,jk) = zpronmax * xdiatnh4(ji,jj,jk)
                  ! Uptake of phosphorus
                  zrat = MIN( 1., zratiop / (xqpdmax(ji,jj,jk) + rtrn) )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05)) 
                  zpropmax = zprnutmax * zmax * xlimdfe(ji,jj,jk)
                  zpropo4d(ji,jj,jk) = zpropmax * xdiatpo4(ji,jj,jk)
                  zprodopd(ji,jj,jk) = zpropmax * xdiatdop(ji,jj,jk)
                  ! Uptake of iron
                  zrat = MIN( 1., zratiof / qfdmax )
                  zmax = MAX(0., MIN(1., (1. - zrat)/ (1.05 - zrat) * 1.05))
                  zprofmax = zprnutmax * qfdmax * zmax
                  zprofed(ji,jj,jk) = zprofmax * xdiatfer(ji,jj,jk) * ( 3. - 2.4 * xlimdfe(ji,jj,jk)     &
                  &          / ( xlimdfe(ji,jj,jk) + 0.2 ) ) * (1. + 0.8 * xdiatno3(ji,jj,jk) / ( rtrn   &
                  &          + xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk) ) * (1. - xdiatfer(ji,jj,jk) ) )
               ENDIF
            END DO
         END DO
      END DO

      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
                     !  production terms for nanophyto. ( chlorophyll )
                  znanotot = enanom(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
                  zprod = rday * (zpronewn(ji,jj,jk) + zproregn(ji,jj,jk)) * zprchln(ji,jj,jk) * xlimphy(ji,jj,jk)
                  thetannm_n   = MIN ( thetannm, ( thetannm / (1. - 1.14 / 43.4 *tsn(ji,jj,jk,jp_tem)))   &
                  &               * (1. - 1.14 / 43.4 * 20.))
                  zprochln = thetannm_n * zprod / ( zpislopeadn(ji,jj,jk) * znanotot + rtrn )
                  zprochln = MAX(zprochln, chlcmin * 12. * zprorcan (ji,jj,jk) )
                     !  production terms for picophyto. ( chlorophyll )
                  zpicotot = epicom(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
                  zprod = rday * (zpronewp(ji,jj,jk) + zproregp(ji,jj,jk)) * zprchlp(ji,jj,jk) * xlimpic(ji,jj,jk)
                  thetanpm_n   = MIN ( thetanpm, ( thetanpm / (1. - 1.14 / 43.4 *tsn(ji,jj,jk,jp_tem)))   &
                  &               * (1. - 1.14 / 43.4 * 20.))
                  zprochlp = thetanpm_n * zprod / ( zpislopeadp(ji,jj,jk) * zpicotot + rtrn )
                  zprochlp = MAX(zprochlp, chlcmin * 12. * zprorcap(ji,jj,jk) )
                  !  production terms for diatomees ( chlorophyll )
                  zdiattot = ediatm(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
                  zprod = rday * (zpronewd(ji,jj,jk) + zproregd(ji,jj,jk)) * zprchld(ji,jj,jk) * xlimdia(ji,jj,jk)
                  thetandm_n   = MIN ( thetandm, ( thetandm / (1. - 1.14 / 43.4 *tsn(ji,jj,jk,jp_tem)))   &
                  &               * (1. - 1.14 / 43.4 * 20.))
                  zprochld = thetandm_n * zprod / ( zpislopeadd(ji,jj,jk) * zdiattot + rtrn )
                  zprochld = MAX(zprochld, chlcmin * 12. * zprorcad(ji,jj,jk) )
                  !   Update the arrays TRA which contain the Chla sources and sinks
                  tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) + zprochln * texcretn
                  tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) + zprochld * texcretd
                  tra(ji,jj,jk,jppch) = tra(ji,jj,jk,jppch) + zprochlp * texcretp
               ENDIF
            END DO
         END DO
      END DO

      !   Update the arrays TRA which contain the biological sources and sinks
      DO jk = 1, jpkm1
         DO jj = 1, jpj
           DO ji =1 ,jpi
              zprontot = zpronewn(ji,jj,jk) + zproregn(ji,jj,jk)
              zproptot = zpronewp(ji,jj,jk) + zproregp(ji,jj,jk)
              zprodtot = zpronewd(ji,jj,jk) + zproregd(ji,jj,jk)
              zdocprod = excretd * zprorcad(ji,jj,jk) + excretn * zprorcan(ji,jj,jk)  &
              &          + excretp * zprorcap(ji,jj,jk)
              tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) - zpropo4n(ji,jj,jk) - zpropo4d(ji,jj,jk)  &
              &                     - zpropo4p(ji,jj,jk)
              tra(ji,jj,jk,jpno3) = tra(ji,jj,jk,jpno3) - zpronewn(ji,jj,jk) - zpronewd(ji,jj,jk)  &
              &                     - zpronewp(ji,jj,jk)
              tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) - zproregn(ji,jj,jk) - zproregd(ji,jj,jk)  &
              &                     - zproregp(ji,jj,jk)
              tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) + zprorcan(ji,jj,jk) * texcretn    &
                 &                  - zpsino3 * zpronewn(ji,jj,jk) - zpsinh4 * zproregn(ji,jj,jk)   &
                 &                  - zrespn(ji,jj,jk) 
              zcroissn(ji,jj,jk) = tra(ji,jj,jk,jpphy) / rfact2/ (trb(ji,jj,jk,jpphy) + rtrn)
              tra(ji,jj,jk,jpnph) = tra(ji,jj,jk,jpnph) + zprontot * texcretn
              tra(ji,jj,jk,jppph) = tra(ji,jj,jk,jppph) + zpropo4n(ji,jj,jk) * texcretn   &
              &                     + zprodopn(ji,jj,jk) * texcretn
              tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) + zprofen(ji,jj,jk) * texcretn
              tra(ji,jj,jk,jppic) = tra(ji,jj,jk,jppic) + zprorcap(ji,jj,jk) * texcretp     &
                 &                  - zpsino3 * zpronewp(ji,jj,jk) - zpsinh4 * zproregp(ji,jj,jk)   &
                 &                  - zrespp(ji,jj,jk) 
              zcroissp(ji,jj,jk) = tra(ji,jj,jk,jppic) / rfact2/ (trb(ji,jj,jk,jppic) + rtrn)
              tra(ji,jj,jk,jpnpi) = tra(ji,jj,jk,jpnpi) + zproptot * texcretp
              tra(ji,jj,jk,jpppi) = tra(ji,jj,jk,jpppi) + zpropo4p(ji,jj,jk) * texcretp   &
              &                     + zprodopp(ji,jj,jk) * texcretp
              tra(ji,jj,jk,jppfe) = tra(ji,jj,jk,jppfe) + zprofep(ji,jj,jk) * texcretp
              tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) + zprorcad(ji,jj,jk) * texcretd   &
                 &                  - zpsino3 * zpronewd(ji,jj,jk) - zpsinh4 * zproregd(ji,jj,jk)   &
                 &                  - zrespd(ji,jj,jk) 
              zcroissd(ji,jj,jk) = tra(ji,jj,jk,jpdia) / rfact2 / (trb(ji,jj,jk,jpdia) + rtrn)
              tra(ji,jj,jk,jpndi) = tra(ji,jj,jk,jpndi) + zprodtot * texcretd
              tra(ji,jj,jk,jppdi) = tra(ji,jj,jk,jppdi) + zpropo4d(ji,jj,jk) * texcretd   &
              &                     + zprodopd(ji,jj,jk) * texcretd
              tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) + zprofed(ji,jj,jk) * texcretd
              tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) + zprorcad(ji,jj,jk) * zysopt(ji,jj,jk) * texcretd
              tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) + excretd * zprorcad(ji,jj,jk) + excretn * zprorcan(ji,jj,jk)  &
              &                     + excretp * zprorcap(ji,jj,jk)
              tra(ji,jj,jk,jpdon) = tra(ji,jj,jk,jpdon) + excretd * zprodtot + excretn * zprontot   &
              &                     + excretp * zproptot
              tra(ji,jj,jk,jpdop) = tra(ji,jj,jk,jpdop) + excretd * zpropo4d(ji,jj,jk) + excretn * zpropo4n(ji,jj,jk)   &
              &    - texcretn * zprodopn(ji,jj,jk) - texcretd * zprodopd(ji,jj,jk) + excretp * zpropo4p(ji,jj,jk)     &
              &    - texcretp * zprodopp(ji,jj,jk)
              tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) + o2ut * ( zproregn(ji,jj,jk) + zproregd(ji,jj,jk)   &
                 &                + zproregp(ji,jj,jk) ) + ( o2ut + o2nit ) * ( zpronewn(ji,jj,jk)           &
                 &                + zpronewd(ji,jj,jk) + zpronewp(ji,jj,jk) )   &
                 &                - o2ut * ( zrespn(ji,jj,jk) + zrespp(ji,jj,jk) + zrespd(ji,jj,jk) )
              zfeup = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
              tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) - zfeup
              tra(ji,jj,jk,jpsil) = tra(ji,jj,jk,jpsil) - texcretd * zprorcad(ji,jj,jk) * zysopt(ji,jj,jk)
              tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) - zprorcan(ji,jj,jk) - zprorcad(ji,jj,jk) - zprorcap(ji,jj,jk)  &
              &                     + zpsino3 * zpronewn(ji,jj,jk) + zpsinh4 * zproregn(ji,jj,jk)   &
              &                     + zpsino3 * zpronewp(ji,jj,jk) + zpsinh4 * zproregp(ji,jj,jk)   &
              &                     + zpsino3 * zpronewd(ji,jj,jk) + zpsinh4 * zproregd(ji,jj,jk)  &
              &                     + zrespn(ji,jj,jk) + zrespd(ji,jj,jk) + zrespp(ji,jj,jk) 
              tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) + rno3 * ( zpronewn(ji,jj,jk) + zpronewd(ji,jj,jk)  &
              &                     + zpronewp(ji,jj,jk) ) - rno3 * ( zproregn(ji,jj,jk) + zproregd(ji,jj,jk)     &
              &                     + zproregp(ji,jj,jk) ) 
          END DO
        END DO
     END DO
     !
     IF( ln_ligand ) THEN
         zpligprod1(:,:,:) = 0._wp    ;    zpligprod2(:,:,:) = 0._wp        
         DO jk = 1, jpkm1
            DO jj = 1, jpj
              DO ji =1 ,jpi
                 zdocprod = excretd * zprorcad(ji,jj,jk) + excretn * zprorcan(ji,jj,jk) + excretp * zprorcap(ji,jj,jk)
                 zfeup    = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
                 tra(ji,jj,jk,jplgw) = tra(ji,jj,jk,jplgw) + zdocprod * ldocp - zfeup * plig(ji,jj,jk) * lthet
                 zpligprod1(ji,jj,jk) = zdocprod * ldocp
                 zpligprod2(ji,jj,jk) = zfeup * plig(ji,jj,jk) * lthet
              END DO
           END DO
        END DO
     ENDIF


     ! Total primary production per year

    ! Total primary production per year
    IF( iom_use( "tintpp" ) .OR. ( ln_check_mass .AND. kt == nitend .AND. knt == nrdttrc )  )  &
      & tpp = glob_sum( 'p5zprod', ( zprorcan(:,:,:) + zprorcad(:,:,:) + zprorcap(:,:,:) ) * cvol(:,:,:) )

    IF( lk_iomput .AND.  knt == nrdttrc ) THEN
       zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
       !
       CALL iom_put( "PPPHYP"  , zprorcap(:,:,:) * zfact * tmask(:,:,:)   ) ! primary production by picophyto
       CALL iom_put( "PPPHYN"  , zprorcan(:,:,:) * zfact * tmask(:,:,:) )  ! primary production by nanophyto
       CALL iom_put( "PPPHYD"  , zprorcad(:,:,:) * zfact * tmask(:,:,:)   ) ! primary production by diatomes
       CALL iom_put( "PPNEWN"  , zpronewp(:,:,:) * zfact * tmask(:,:,:)    ) ! new primary production by picophyto
       CALL iom_put( "PPNEWN"  , zpronewn(:,:,:) * zfact * tmask(:,:,:)    ) ! new primary production by nanophyto
       CALL iom_put( "PPNEWD"  , zpronewd(:,:,:) * zfact * tmask(:,:,:)   ) ! new primary production by diatomes
       CALL iom_put( "PBSi"    , zprorcad(:,:,:) * zfact * tmask(:,:,:) * zysopt(:,:,:)  ) ! biogenic silica production
       CALL iom_put( "PFeP"    , zprofep(:,:,:) * zfact * tmask(:,:,:)  ) ! biogenic iron production by picophyto
       CALL iom_put( "PFeN"    , zprofen(:,:,:) * zfact * tmask(:,:,:)  ) ! biogenic iron production by nanophyto
       CALL iom_put( "PFeD"    , zprofed(:,:,:) * zfact * tmask(:,:,:)  ) ! biogenic iron production by  diatomes
       IF( ln_ligand ) THEN
         CALL iom_put( "LPRODP"  , zpligprod1(:,:,:) * 1e9 * zfact * tmask(:,:,:) )
         CALL iom_put( "LDETP"   , zpligprod2(:,:,:) * 1e9 * zfact * tmask(:,:,:) )
       ENDIF
       CALL iom_put( "Mumax"   , zprmaxn(:,:,:) * tmask(:,:,:)  ) ! Maximum growth rate
       CALL iom_put( "MuP"     , zprpic(:,:,:) * xlimpic(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for picophyto
       CALL iom_put( "MuN"     , zprbio(:,:,:) * xlimphy(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for nanophyto
       CALL iom_put( "MuD"     , zprdia(:,:,:) * xlimdia(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for diatoms
       CALL iom_put( "LPlight" , zprpic(:,:,:) / (zprmaxp(:,:,:) + rtrn) * tmask(:,:,:)  )  ! light limitation term
       CALL iom_put( "LNlight" , zprbio(:,:,:) / (zprmaxn(:,:,:) + rtrn) * tmask(:,:,:)  )  ! light limitation term
       CALL iom_put( "LDlight" , zprdia(:,:,:) / (zprmaxd(:,:,:) + rtrn) * tmask(:,:,:)   )
       CALL iom_put( "MunetP"  , zcroissp(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for picophyto
       CALL iom_put( "MunetN"  , zcroissn(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for nanophyto
       CALL iom_put( "MunetD"  , zcroissd(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for diatoms
       CALL iom_put( "TPP"     , ( zprorcap(:,:,:) + zprorcan(:,:,:) + zprorcad(:,:,:) ) * zfact * tmask(:,:,:)  )  ! total primary production
       CALL iom_put( "TPNEW"   , ( zpronewp(:,:,:) + zpronewn(:,:,:) + zpronewd(:,:,:) ) * zfact * tmask(:,:,:)  ) ! total new production
       CALL iom_put( "TPBFE"   , ( zprofep (:,:,:) + zprofen (:,:,:) + zprofed (:,:,:) ) * zfact * tmask(:,:,:)  )  ! total biogenic iron production
       CALL iom_put( "tintpp"  , tpp * zfact )  !  global total integrated primary production molC/s
     ENDIF

      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('prod')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_prod')
      !
   END SUBROUTINE p5z_prod


   SUBROUTINE p5z_prod_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_prod_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton production parameters
      !!
      !! ** Method  :   Read the nampisprod namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampisprod
      !!----------------------------------------------------------------------
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zprod/ pislopen, pislopep, pisloped, excretn, excretp, excretd,     &
         &                 thetannm, thetanpm, thetandm, chlcmin, grosip, bresp, xadap
      !!----------------------------------------------------------------------

      REWIND( numnatp_ref )
      READ  ( numnatp_ref, namp5zprod, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zprod in reference namelist' )

      REWIND( numnatp_cfg )
      READ  ( numnatp_cfg, namp5zprod, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zprod in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zprod )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton growth, namp5zprod'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean Si/C ratio                           grosip       =', grosip
         WRITE(numout,*) '    P-I slope                                 pislopen     =', pislopen
         WRITE(numout,*) '    P-I slope  for diatoms                    pisloped     =', pisloped
         WRITE(numout,*) '    P-I slope  for picophytoplankton          pislopep     =', pislopep
         WRITE(numout,*) '    Acclimation factor to low light           xadap        =', xadap
         WRITE(numout,*) '    excretion ratio of nanophytoplankton      excretn      =', excretn
         WRITE(numout,*) '    excretion ratio of picophytoplankton      excretp      =', excretp
         WRITE(numout,*) '    excretion ratio of diatoms                excretd      =', excretd
         WRITE(numout,*) '    basal respiration in phytoplankton        bresp        =', bresp
         WRITE(numout,*) '    Maximum Chl/C in phytoplankton            chlcmin      =', chlcmin
         WRITE(numout,*) '    Minimum Chl/N in nanophytoplankton        thetannm     =', thetannm
         WRITE(numout,*) '    Minimum Chl/N in picophytoplankton        thetanpm     =', thetanpm
         WRITE(numout,*) '    Minimum Chl/N in diatoms                  thetandm     =', thetandm
      ENDIF
      !
      r1_rday   = 1._wp / rday 
      texcretn  = 1._wp - excretn
      texcretp  = 1._wp - excretp
      texcretd  = 1._wp - excretd
      tpp       = 0._wp
      !
   END SUBROUTINE p5z_prod_init


   INTEGER FUNCTION p5z_prod_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_prod_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( zdaylen(jpi,jpj), STAT = p5z_prod_alloc )
      !
      IF( p5z_prod_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p5z_prod_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p5z_prod_alloc
   !!======================================================================
END MODULE p5zprod
