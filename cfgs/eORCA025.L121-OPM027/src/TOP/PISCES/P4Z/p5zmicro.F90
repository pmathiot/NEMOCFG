MODULE p5zmicro
   !!======================================================================
   !!                         ***  MODULE p5zmicro  ***
   !! TOP :   PISCES Compute the sources/sinks for microzooplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_micro       :   Compute the sources/sinks for microzooplankton
   !!   p5z_micro_init  :   Initialize and read the appropriate namelist
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zlim
   USE p5zlim          !  Phytoplankton limitation terms
   USE iom             !  I/O manager
   USE prtctl_trc      !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_micro         ! called in p5zbio.F90
   PUBLIC   p5z_micro_init    ! called in trcsms_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  part        !: part of calcite not dissolved in microzoo guts
   REAL(wp), PUBLIC ::  xprefc     !: microzoo preference for POC 
   REAL(wp), PUBLIC ::  xprefn     !: microzoo preference for nanophyto
   REAL(wp), PUBLIC ::  xprefp     !: microzoo preference for picophyto
   REAL(wp), PUBLIC ::  xprefd     !: microzoo preference for diatoms
   REAL(wp), PUBLIC ::  xprefz     !: microzoo preference for microzoo
   REAL(wp), PUBLIC ::  xthreshdia  !: diatoms feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshpic  !: picophyto feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshphy  !: nanophyto threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshzoo  !: microzoo threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshpoc  !: poc threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthresh     !: feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::  resrat      !: exsudation rate of microzooplankton
   REAL(wp), PUBLIC ::  mzrat       !: microzooplankton mortality rate 
   REAL(wp), PUBLIC ::  grazrat     !: maximal microzoo grazing rate
   REAL(wp), PUBLIC ::  xkgraz      !: Half-saturation constant of assimilation
   REAL(wp), PUBLIC ::  unassc      !: Non-assimilated part of food
   REAL(wp), PUBLIC ::  unassn      !: Non-assimilated part of food
   REAL(wp), PUBLIC ::  unassp      !: Non-assimilated part of food
   REAL(wp), PUBLIC ::  epsher      !: Growth efficiency for microzoo
   REAL(wp), PUBLIC ::  epshermin   !: Minimum growth efficiency for microzoo
   REAL(wp), PUBLIC ::  srespir     !: half sturation constant for grazing 1 
   REAL(wp), PUBLIC ::  ssigma      !: Fraction excreted as semi-labile DOM
   LOGICAL,  PUBLIC ::  bmetexc     !: Use of excess carbon for respiration

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zmicro.F90 12276 2019-12-20 11:14:26Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p5z_micro( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_micro  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for microzooplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::  kt  ! ocean time step
      INTEGER, INTENT(in) ::  knt 
      !
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompadi, zcompaz , zcompaph, zcompapoc, zcompapon, zcompapop
      REAL(wp) :: zcompapi, zgraze  , zdenom, zfact, zfood, zfoodlim
      REAL(wp) :: ztmp1, ztmp2, ztmp3, ztmp4, ztmp5, ztmptot
      REAL(wp) :: zepsherf, zepshert, zepsherv, zrespirc, zrespirn, zrespirp, zbasresb, zbasresi
      REAL(wp) :: zgraztotc, zgraztotn, zgraztotp, zgraztotf, zbasresn, zbasresp, zbasresf
      REAL(wp) :: zgradoc, zgradon, zgradop, zgraref, zgradoct, zgradont, zgradopt, zgrareft
      REAL(wp) :: zexcess, zgraren, zgrarep, zgrarem
      REAL(wp) :: zgrapoc, zgrapon, zgrapop, zgrapof, zprcaca, zmortz
      REAL(wp) :: zrespz, ztortz, zgrasratf, zgrasratn, zgrasratp
      REAL(wp) :: zgraznc, zgraznn, zgraznp, zgrazpoc, zgrazpon, zgrazpop, zgrazpof
      REAL(wp) :: zgrazdc, zgrazdn, zgrazdp, zgrazdf, zgraznf, zgrazz
      REAL(wp) :: zgrazpc, zgrazpn, zgrazpp, zgrazpf, zbeta, zrfact2, zmetexcess
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zgrazing, zfezoo, zzligprod
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_micro')
      !
      zmetexcess = 0.0
      IF ( bmetexc ) zmetexcess = 1.0
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zcompaz = MAX( ( trb(ji,jj,jk,jpzoo) - 1.e-9 ), 0.e0 )
               zfact   = xstep * tgfunc2(ji,jj,jk) * zcompaz

               !   Michaelis-Menten mortality rates of microzooplankton
               !   -----------------------------------------------------
               zrespz = resrat * zfact * ( trb(ji,jj,jk,jpzoo) / ( xkmort + trb(ji,jj,jk,jpzoo) )  &
               &        + 3. * nitrfac(ji,jj,jk) )

               !   Zooplankton mortality. A square function has been selected with
               !   no real reason except that it seems to be more stable and may mimic predation.
               !   ------------------------------------------------------------------------------
               ztortz = mzrat * 1.e6 * zfact * trb(ji,jj,jk,jpzoo) * (1. - nitrfac(ji,jj,jk))

               !   Computation of the abundance of the preys
               !   A threshold can be specified in the namelist
               !   --------------------------------------------
               zcompadi  = MIN( MAX( ( trb(ji,jj,jk,jpdia) - xthreshdia ), 0.e0 ), xsizedia )
               zcompaph  = MAX( ( trb(ji,jj,jk,jpphy) - xthreshphy ), 0.e0 )
               zcompaz   = MAX( ( trb(ji,jj,jk,jpzoo) - xthreshzoo ), 0.e0 )
               zcompapi  = MAX( ( trb(ji,jj,jk,jppic) - xthreshpic ), 0.e0 )
               zcompapoc = MAX( ( trb(ji,jj,jk,jppoc) - xthreshpoc ), 0.e0 )
               
               !   Microzooplankton grazing
               !   ------------------------
               zfood     = xprefn * zcompaph + xprefc * zcompapoc + xprefd * zcompadi   &
               &           + xprefz * zcompaz + xprefp * zcompapi
               zfoodlim  = MAX( 0. , zfood - min(xthresh,0.5*zfood) )
               zdenom    = zfoodlim / ( xkgraz + zfoodlim )
               zgraze    = grazrat * xstep * tgfunc2(ji,jj,jk) * trb(ji,jj,jk,jpzoo) * (1. - nitrfac(ji,jj,jk)) 

               !   An active switching parameterization is used here.
               !   We don't use the KTW parameterization proposed by 
               !   Vallina et al. because it tends to produce to steady biomass
               !   composition and the variance of Chl is too low as it grazes
               !   too strongly on winning organisms. Thus, instead of a square
               !   a 1.5 power value is used which decreases the pressure on the
               !   most abundant species
               !   ------------------------------------------------------------  
               ztmp1 = xprefn * zcompaph**1.5
               ztmp2 = xprefp * zcompapi**1.5
               ztmp3 = xprefc * zcompapoc**1.5
               ztmp4 = xprefd * zcompadi**1.5
               ztmp5 = xprefz * zcompaz**1.5
               ztmptot = ztmp1 + ztmp2 + ztmp3 + ztmp4 + ztmp5 + rtrn
               ztmp1 = ztmp1 / ztmptot
               ztmp2 = ztmp2 / ztmptot
               ztmp3 = ztmp3 / ztmptot
               ztmp4 = ztmp4 / ztmptot
               ztmp5 = ztmp5 / ztmptot

               !   Microzooplankton regular grazing on the different preys
               !   -------------------------------------------------------
               zgraznc   = zgraze  * ztmp1  * zdenom
               zgraznn   = zgraznc * trb(ji,jj,jk,jpnph) / (trb(ji,jj,jk,jpphy) + rtrn)
               zgraznp   = zgraznc * trb(ji,jj,jk,jppph) / (trb(ji,jj,jk,jpphy) + rtrn)
               zgraznf   = zgraznc * trb(ji,jj,jk,jpnfe) / (trb(ji,jj,jk,jpphy) + rtrn)
               zgrazpc   = zgraze  * ztmp2  * zdenom
               zgrazpn   = zgrazpc * trb(ji,jj,jk,jpnpi) / (trb(ji,jj,jk,jppic) + rtrn)
               zgrazpp   = zgrazpc * trb(ji,jj,jk,jpppi) / (trb(ji,jj,jk,jppic) + rtrn)
               zgrazpf   = zgrazpc * trb(ji,jj,jk,jppfe) / (trb(ji,jj,jk,jppic) + rtrn)
               zgrazz    = zgraze  * ztmp5   * zdenom
               zgrazpoc  = zgraze  * ztmp3   * zdenom
               zgrazpon  = zgrazpoc * trb(ji,jj,jk,jppon) / ( trb(ji,jj,jk,jppoc) + rtrn )
               zgrazpop  = zgrazpoc * trb(ji,jj,jk,jppop) / ( trb(ji,jj,jk,jppoc) + rtrn )
               zgrazpof  = zgrazpoc* trb(ji,jj,jk,jpsfe) / (trb(ji,jj,jk,jppoc) + rtrn)
               zgrazdc   = zgraze  * ztmp4  * zdenom
               zgrazdn   = zgrazdc * trb(ji,jj,jk,jpndi) / (trb(ji,jj,jk,jpdia) + rtrn)
               zgrazdp   = zgrazdc * trb(ji,jj,jk,jppdi) / (trb(ji,jj,jk,jpdia) + rtrn)
               zgrazdf   = zgrazdc * trb(ji,jj,jk,jpdfe) / (trb(ji,jj,jk,jpdia) + rtrn)
               !
               zgraztotc = zgraznc + zgrazpoc + zgrazdc + zgrazz + zgrazpc
               zgraztotn = zgraznn + zgrazpn + zgrazpon + zgrazdn + zgrazz * no3rat3
               zgraztotp = zgraznp + zgrazpp + zgrazpop + zgrazdp + zgrazz * po4rat3
               zgraztotf = zgraznf + zgrazpf + zgrazpof + zgrazdf + zgrazz * ferat3
               !
               ! Grazing by microzooplankton
               zgrazing(ji,jj,jk) = zgraztotc

               !   Stoichiometruc ratios of the food ingested by zooplanton 
               !   --------------------------------------------------------
               zgrasratf =  (zgraztotf + rtrn) / ( zgraztotc + rtrn )
               zgrasratn =  (zgraztotn + rtrn) / ( zgraztotc + rtrn )
               zgrasratp =  (zgraztotp + rtrn) / ( zgraztotc + rtrn )

               !   Growth efficiency is made a function of the quality 
               !   and the quantity of the preys
               !   ---------------------------------------------------
               zepshert  = MIN( 1., zgrasratn/ no3rat3, zgrasratp/ po4rat3, zgrasratf / ferat3)
               zbeta     = MAX( 0., (epsher - epshermin) )
               zepsherf  = epshermin + zbeta / ( 1.0 + 0.04E6 * 12. * zfood * zbeta )
               zepsherv  = zepsherf * zepshert

               !   Respiration of microzooplankton
               !   Excess carbon in the food is used preferentially
               !   ------------------------------------------------
               zexcess  = zgraztotc * zepsherf * (1.0 - zepshert) * zmetexcess
               zbasresb = MAX(0., zrespz - zexcess)
               zbasresi = zexcess + MIN(0., zrespz - zexcess)  
               zrespirc = srespir * zepsherv * zgraztotc + zbasresb
               
               !   When excess carbon is used, the other elements in excess
               !   are also used proportionally to their abundance
               !   --------------------------------------------------------
               zexcess  = ( zgrasratn/ no3rat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
               zbasresn = zbasresi * zexcess * zgrasratn 
               zexcess  = ( zgrasratp/ po4rat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
               zbasresp = zbasresi * zexcess * zgrasratp
               zexcess  = ( zgrasratf/ ferat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
               zbasresf = zbasresi * zexcess * zgrasratf

               !   Voiding of the excessive elements as DOM
               !   ----------------------------------------
               zgradoct   = (1. - unassc - zepsherv) * zgraztotc - zbasresi  
               zgradont   = (1. - unassn) * zgraztotn - zepsherv * no3rat3 * zgraztotc - zbasresn
               zgradopt   = (1. - unassp) * zgraztotp - zepsherv * po4rat3 * zgraztotc - zbasresp
               zgrareft   = (1. - unassc) * zgraztotf - zepsherv * ferat3 * zgraztotc - zbasresf

               !  Since only semilabile DOM is represented in PISCES
               !  part of DOM is in fact labile and is then released
               !  as dissolved inorganic compounds (ssigma)
               !  --------------------------------------------------
               zgradoc =  zgradoct * ssigma
               zgradon =  zgradont * ssigma
               zgradop =  zgradopt * ssigma
               zgrarem = (1.0 - ssigma) * zgradoct
               zgraren = (1.0 - ssigma) * zgradont
               zgrarep = (1.0 - ssigma) * zgradopt
               zgraref = zgrareft

               !   Defecation as a result of non assimilated products
               !   --------------------------------------------------
               zgrapoc   = zgraztotc * unassc
               zgrapon   = zgraztotn * unassn
               zgrapop   = zgraztotp * unassp
               zgrapof   = zgraztotf * unassc

               !  Addition of respiration to the release of inorganic nutrients
               !  -------------------------------------------------------------
               zgrarem = zgrarem + zbasresi + zrespirc
               zgraren = zgraren + zbasresn + zrespirc * no3rat3
               zgrarep = zgrarep + zbasresp + zrespirc * po4rat3
               zgraref = zgraref + zbasresf + zrespirc * ferat3

               !   Update of the TRA arrays
               !   ------------------------
               tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) + zgrarep
               tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) + zgraren
               tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) + zgradoc
               !
               IF( ln_ligand ) THEN 
                  tra(ji,jj,jk,jplgw) = tra(ji,jj,jk,jplgw) + zgradoc * ldocz
                  zzligprod(ji,jj,jk) = zgradoc * ldocz
               ENDIF
               !
               tra(ji,jj,jk,jpdon) = tra(ji,jj,jk,jpdon) + zgradon
               tra(ji,jj,jk,jpdop) = tra(ji,jj,jk,jpdop) + zgradop
               tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) - o2ut * zgrarem 
               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) + zgraref
               zfezoo(ji,jj,jk)    = zgraref
               tra(ji,jj,jk,jpzoo) = tra(ji,jj,jk,jpzoo) + zepsherv * zgraztotc - zrespirc - ztortz - zgrazz
               tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) - zgraznc
               tra(ji,jj,jk,jpnph) = tra(ji,jj,jk,jpnph) - zgraznn
               tra(ji,jj,jk,jppph) = tra(ji,jj,jk,jppph) - zgraznp
               tra(ji,jj,jk,jppic) = tra(ji,jj,jk,jppic) - zgrazpc
               tra(ji,jj,jk,jpnpi) = tra(ji,jj,jk,jpnpi) - zgrazpn
               tra(ji,jj,jk,jpppi) = tra(ji,jj,jk,jpppi) - zgrazpp
               tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) - zgrazdc
               tra(ji,jj,jk,jpndi) = tra(ji,jj,jk,jpndi) - zgrazdn
               tra(ji,jj,jk,jppdi) = tra(ji,jj,jk,jppdi) - zgrazdp
               tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) - zgraznc * trb(ji,jj,jk,jpnch)/(trb(ji,jj,jk,jpphy)+rtrn)
               tra(ji,jj,jk,jppch) = tra(ji,jj,jk,jppch) - zgrazpc * trb(ji,jj,jk,jppch)/(trb(ji,jj,jk,jppic)+rtrn)
               tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) - zgrazdc * trb(ji,jj,jk,jpdch)/(trb(ji,jj,jk,jpdia)+rtrn)
               tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) - zgrazdc * trb(ji,jj,jk,jpdsi)/(trb(ji,jj,jk,jpdia)+rtrn)
               tra(ji,jj,jk,jpgsi) = tra(ji,jj,jk,jpgsi) + zgrazdc * trb(ji,jj,jk,jpdsi)/(trb(ji,jj,jk,jpdia)+rtrn)
               tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) - zgraznf
               tra(ji,jj,jk,jppfe) = tra(ji,jj,jk,jppfe) - zgrazpf
               tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) - zgrazdf
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + ztortz + zgrapoc - zgrazpoc 
               prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + ztortz + zgrapoc
               conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zgrazpoc
               tra(ji,jj,jk,jppon) = tra(ji,jj,jk,jppon) + no3rat3 * ztortz + zgrapon - zgrazpon
               tra(ji,jj,jk,jppop) = tra(ji,jj,jk,jppop) + po4rat3 * ztortz + zgrapop - zgrazpop
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + ferat3 * ztortz  + zgrapof - zgrazpof
               !
               ! calcite production
               zprcaca = xfracal(ji,jj,jk) * zgraznc
               prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
               !
               zprcaca = part * zprcaca
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) + zgrarem - zprcaca
               tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) - 2. * zprcaca     &
               &                     + rno3 * zgraren
               tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) + zprcaca
            END DO
         END DO
      END DO
      !
      IF( lk_iomput .AND. knt == nrdttrc ) THEN
        IF( iom_use("GRAZ1") ) THEN  !   Total grazing of phyto by zooplankton
           zgrazing(:,:,jpk) = 0._wp   ; CALL iom_put( "GRAZ1" , zgrazing(:,:,:) * 1.e+3  * rfact2r * tmask(:,:,:) ) 
         ENDIF
         IF( iom_use("FEZOO") ) THEN  
           zfezoo (:,:,jpk) = 0._wp    ; CALL iom_put( "FEZOO" , zfezoo(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:) )
         ENDIF
         IF( ln_ligand ) THEN
            zzligprod(:,:,jpk) = 0._wp ; CALL iom_put( "LPRODZ", zzligprod(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:))
         ENDIF
      ENDIF
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('micro')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_micro')
      !
   END SUBROUTINE p5z_micro


   SUBROUTINE p5z_micro_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_micro_init  ***
      !!
      !! ** Purpose :   Initialization of microzooplankton parameters
      !!
      !! ** Method  :   Read the nampiszoo namelist and check the parameters
      !!                called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampiszoo
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !!
      NAMELIST/namp5zzoo/ part, grazrat, bmetexc, resrat, mzrat, xprefc, xprefn, &
         &                xprefp, xprefd, xprefz, xthreshdia, xthreshphy, &
         &                xthreshpic, xthreshpoc, xthreshzoo, xthresh, xkgraz, &
         &                epsher, epshermin, ssigma, srespir, unassc, unassn, unassp
      !!----------------------------------------------------------------------
      !
      REWIND( numnatp_ref )
      READ  ( numnatp_ref, namp5zzoo, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zzoo in reference namelist' )
      !
      REWIND( numnatp_cfg )
      READ  ( numnatp_cfg, namp5zzoo, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zzoo in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zzoo )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for microzooplankton, nampiszooq'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    part of calcite not dissolved in microzoo guts  part        =', part
         WRITE(numout,*) '    microzoo preference for POC                     xprefc     =', xprefc
         WRITE(numout,*) '    microzoo preference for nano                    xprefn     =', xprefn
         WRITE(numout,*) '    microzoo preference for pico                    xprefp     =', xprefp
         WRITE(numout,*) '    microzoo preference for diatoms                 xprefd     =', xprefd
         WRITE(numout,*) '    microzoo preference for microzoo                xprefz     =', xprefz
         WRITE(numout,*) '    diatoms feeding threshold  for microzoo         xthreshdia  =', xthreshdia
         WRITE(numout,*) '    nanophyto feeding threshold for microzoo        xthreshphy  =', xthreshphy
         WRITE(numout,*) '    picophyto feeding threshold for microzoo        xthreshpic  =', xthreshpic
         WRITE(numout,*) '    poc feeding threshold for microzoo              xthreshpoc  =', xthreshpoc
         WRITE(numout,*) '    microzoo feeding threshold for microzoo         xthreshzoo  =', xthreshzoo
         WRITE(numout,*) '    feeding threshold for microzooplankton          xthresh     =', xthresh
         WRITE(numout,*) '    exsudation rate of microzooplankton             resrat      =', resrat
         WRITE(numout,*) '    microzooplankton mortality rate                 mzrat       =', mzrat
         WRITE(numout,*) '    maximal microzoo grazing rate                   grazrat     =', grazrat
         WRITE(numout,*) '    C egested fraction of fodd by microzoo          unassc      =', unassc
         WRITE(numout,*) '    N egested fraction of fodd by microzoo          unassn      =', unassn
         WRITE(numout,*) '    P egested fraction of fodd by microzoo          unassp      =', unassp
         WRITE(numout,*) '    Efficicency of microzoo growth                  epsher      =', epsher
         WRITE(numout,*) '    Minimum Efficiency of Microzoo growth           epshermin   =', epshermin
         WRITE(numout,*) '    Fraction excreted as semi-labile DOM            ssigma      =', ssigma
         WRITE(numout,*) '    Active respiration                              srespir     =', srespir
         WRITE(numout,*) '    half sturation constant for grazing 1           xkgraz      =', xkgraz
         WRITE(numout,*) '    Use of excess carbon for respiration            bmetexc     =', bmetexc
      ENDIF
      !
   END SUBROUTINE p5z_micro_init

   !!======================================================================
END MODULE p5zmicro
