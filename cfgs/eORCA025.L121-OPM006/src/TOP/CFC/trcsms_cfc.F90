MODULE trcsms_cfc
   !!======================================================================
   !!                      ***  MODULE trcsms_cfc  ***
   !! TOP : CFC main model
   !!======================================================================
   !! History :  OPA  !  1999-10  (JC. Dutay)  original code
   !!  NEMO      1.0  !  2004-03  (C. Ethe) free form + modularity
   !!            2.0  !  2007-12  (C. Ethe, G. Madec)  reorganisation
   !!            4.0  !  2016-11  (T. Lovato) Add SF6, Update Schmidt number
   !!----------------------------------------------------------------------
   !!   trc_sms_cfc  :  compute and add CFC suface forcing to CFC trends
   !!   cfc_init     :  sets constants for CFC surface forcing computation
   !!----------------------------------------------------------------------
   USE oce_trc       ! Ocean variables
   USE par_trc       ! TOP parameters
   USE trc           ! TOP variables
   USE trd_oce
   USE trdtrc
   USE iom           ! I/O library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_cfc         ! called in ???    
   PUBLIC   trc_sms_cfc_alloc   ! called in trcini_cfc.F90

   INTEGER , PUBLIC, PARAMETER ::   jphem  =   2   ! parameter for the 2 hemispheres
   INTEGER , PUBLIC            ::   jpyear         ! Number of years read in input data file (in trcini_cfc)
   INTEGER , PUBLIC            ::   ndate_beg      ! initial calendar date (aammjj) for CFC
   INTEGER , PUBLIC            ::   nyear_res      ! restoring time constant (year)
   INTEGER , PUBLIC            ::   nyear_beg      ! initial year (aa) 
   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   p_cfc    ! partial hemispheric pressure for all CFC
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   xphem    ! spatial interpolation factor for patm
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qtr_cfc  ! flux at surface
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qint_cfc ! cumulative flux 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   atm_cfc  ! partial hemispheric pressure for used CFC
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   patm     ! atmospheric function

   REAL(wp),         ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   soa      ! coefficient for solubility of CFC [mol/l/atm]
   REAL(wp),         ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sob      !    "               "
   REAL(wp),         ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sca      ! coefficients for schmidt number in degrees Celsius
   !                          ! coefficients for conversion
   REAL(wp) ::   xconv1 = 1.0          ! conversion from to 
   REAL(wp) ::   xconv2 = 0.01/3600.   ! conversion from cm/h to m/s: 
   REAL(wp) ::   xconv3 = 1.0e+3       ! conversion from mol/l/atm to mol/m3/atm
   REAL(wp) ::   xconv4 = 1.0e-12      ! conversion from mol/m3/atm to mol/m3/pptv 

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcsms_cfc.F90 12300 2020-01-06 08:31:02Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms_cfc( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_cfc  ***
      !!
      !! ** Purpose :   Compute the surface boundary contition on CFC 11 
      !!             passive tracer associated with air-mer fluxes and add it 
      !!             to the general trend of tracers equations.
      !!
      !! ** Method  : - get the atmospheric partial pressure - given in pico -
      !!              - computation of solubility ( in 1.e-12 mol/l then in 1.e-9 mol/m3)
      !!              - computation of transfert speed ( given in cm/hour ----> cm/s )
      !!              - the input function is given by : 
      !!                speed * ( concentration at equilibrium - concentration at surface )
      !!              - the input function is in pico-mol/m3/s and the
      !!                CFC concentration in pico-mol/m3
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jn, jl, jm
      INTEGER  ::   iyear_beg, iyear_end
      INTEGER  ::   im1, im2, ierr
      REAL(wp) ::   ztap, zdtap        
      REAL(wp) ::   zt1, zt2, zt3, zt4, zv2
      REAL(wp) ::   zsol      ! solubility
      REAL(wp) ::   zsch      ! schmidt number 
      REAL(wp) ::   zpp_cfc   ! atmospheric partial pressure of CFC
      REAL(wp) ::   zca_cfc   ! concentration at equilibrium
      REAL(wp) ::   zak_cfc   ! transfert coefficients
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)  ::   zpatm     ! atmospheric function
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sms_cfc')
      !
      ALLOCATE( zpatm(jphem,jp_cfc), STAT=ierr )
      IF( ierr > 0 ) THEN
         CALL ctl_stop( 'trc_sms_cfc: unable to allocate zpatm array' )   ;   RETURN
      ENDIF

      IF( kt == nittrc000 )   CALL cfc_init

      ! Temporal interpolation
      ! ----------------------
      iyear_beg = nyear - 1900
      IF ( nmonth <= 6 ) THEN
         iyear_beg = iyear_beg - 1
         im1       =  6 - nmonth + 1
         im2       =  6 + nmonth - 1
      ELSE
         im1       = 12 - nmonth + 7
         im2       =      nmonth - 7
      ENDIF
      ! Avoid bad interpolation if starting date is =< 1900
      IF( iyear_beg .LE. 0      )  iyear_beg = 1
      IF( iyear_beg .GE. jpyear )  iyear_beg = jpyear - 1
      !
      iyear_end = iyear_beg + 1

      !                                                  !------------!
      DO jl = 1, jp_cfc                                  !  CFC loop  !
         !                                               !------------!
         jn = jp_cfc0 + jl - 1
         ! time interpolation at time kt
         DO jm = 1, jphem
            zpatm(jm,jl) = (  atm_cfc(iyear_beg, jm, jl) * REAL(im1, wp)  &
               &           +  atm_cfc(iyear_end, jm, jl) * REAL(im2, wp) ) / 12.
         END DO
         
         !                                                         !------------!
         DO jj = 1, jpj                                            !  i-j loop  !
            DO ji = 1, jpi                                         !------------!
 
               ! space interpolation
               zpp_cfc  =       xphem(ji,jj)   * zpatm(1,jl)   &
                  &     + ( 1.- xphem(ji,jj) ) * zpatm(2,jl)

               ! Computation of concentration at equilibrium : in picomol/l
               ! coefficient for solubility for CFC-11/12 in  mol/l/atm
               IF( tmask(ji,jj,1) .GE. 0.5 ) THEN
                  ztap  = ( tsn(ji,jj,1,jp_tem) + 273.16 ) * 0.01
                  zdtap = sob(1,jl) + ztap * ( sob(2,jl) + ztap * sob(3,jl) ) 
                  zsol  =  EXP( soa(1,jl) + soa(2,jl) / ztap + soa(3,jl) * LOG( ztap )   &
                     &                    + soa(4,jl) * ztap * ztap + tsn(ji,jj,1,jp_sal) * zdtap ) 
               ELSE
                  zsol  = 0.e0
               ENDIF
               ! conversion from mol/l/atm to mol/m3/atm and from mol/m3/atm to mol/m3/pptv    
               zsol = xconv4 * xconv3 * zsol * tmask(ji,jj,1)  
               ! concentration at equilibrium
               zca_cfc = xconv1 * zpp_cfc * zsol * tmask(ji,jj,1)             
  
               ! Computation of speed transfert
               !    Schmidt number revised in Wanninkhof (2014)
               zt1  = tsn(ji,jj,1,jp_tem)
               zt2  = zt1 * zt1 
               zt3  = zt1 * zt2
               zt4  = zt2 * zt2
               zsch = sca(1,jl) + sca(2,jl) * zt1 + sca(3,jl) * zt2 + sca(4,jl) * zt3 + sca(5,jl) * zt4

               !    speed transfert : formulae revised in Wanninkhof (2014)
               zv2     = wndm(ji,jj) * wndm(ji,jj)
               zsch    = zsch / 660.
               zak_cfc = ( 0.251 * xconv2 * zv2 / SQRT(zsch) ) * tmask(ji,jj,1)

               ! Input function  : speed *( conc. at equil - concen at surface )
               ! trn in pico-mol/l idem qtr; ak in en m/a
               qtr_cfc(ji,jj,jl) = -zak_cfc * ( trb(ji,jj,1,jn) - zca_cfc )   &
                  &                         * tmask(ji,jj,1) * ( 1. - fr_i(ji,jj) )
               ! Add the surface flux to the trend
               tra(ji,jj,1,jn) = tra(ji,jj,1,jn) + qtr_cfc(ji,jj,jl) / e3t_n(ji,jj,1) 

               ! cumulation of surface flux at each time step
               qint_cfc(ji,jj,jl) = qint_cfc(ji,jj,jl) + qtr_cfc(ji,jj,jl) * rdt
               !                                               !----------------!
            END DO                                             !  end i-j loop  !
         END DO                                                !----------------!
         !                                                  !----------------!
      END DO                                                !  end CFC loop  !
      !
      IF( lrst_trc ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_sms_cfc : cumulated input function fields written in ocean restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         jl = 0
         DO jn = jp_cfc0, jp_cfc1
             jl = jl + 1
            CALL iom_rstput( kt, nitrst, numrtw, 'qint_'//ctrcnm(jn), qint_cfc(:,:,jl) )
         END DO
      ENDIF                                            
      !
      IF( lk_iomput ) THEN
         jl = 0
         DO jn = jp_cfc0, jp_cfc1
            jl = jl + 1
            CALL iom_put( 'qtr_'//TRIM(ctrcnm(jn)) , qtr_cfc (:,:,jl) )
            CALL iom_put( 'qint_'//TRIM(ctrcnm(jn)), qint_cfc(:,:,jl) )
         ENDDO
      END IF
      !
      IF( l_trdtrc ) THEN
          DO jn = jp_cfc0, jp_cfc1
            CALL trd_trc( tra(:,:,:,jn), jn, jptra_sms, kt )   ! save trends
          END DO
      END IF
      !
      IF( ln_timing )   CALL timing_stop('trc_sms_cfc')
      !
   END SUBROUTINE trc_sms_cfc


   SUBROUTINE cfc_init
      !!---------------------------------------------------------------------
      !!                     ***  cfc_init  ***  
      !!
      !! ** Purpose : sets constants for CFC model
      !!---------------------------------------------------------------------
      INTEGER ::   jn, jl   !
      !!----------------------------------------------------------------------
      !
      jn = 0 
      ! coefficient for CFC11 
      !----------------------
      if ( ln_cfc11 ) then
         jn = jn + 1
         ! Solubility
         soa(1,jn) = -229.9261 
         soa(2,jn) =  319.6552
         soa(3,jn) =  119.4471
         soa(4,jn) =  -1.39165

         sob(1,jn) =  -0.142382
         sob(2,jn) =   0.091459
         sob(3,jn) =  -0.0157274

         ! Schmidt number 
         sca(1,jn) = 3579.2
         sca(2,jn) = -222.63
         sca(3,jn) = 7.5749
         sca(4,jn) = -0.14595
         sca(5,jn) = 0.0011874

         ! atm. concentration
         atm_cfc(:,:,jn) = p_cfc(:,:,1)
      endif

      ! coefficient for CFC12 
      !----------------------
      if ( ln_cfc12 ) then
         jn = jn + 1
         ! Solubility
         soa(1,jn) = -218.0971
         soa(2,jn) =  298.9702
         soa(3,jn) =  113.8049
         soa(4,jn) =  -1.39165

         sob(1,jn) =  -0.143566
         sob(2,jn) =   0.091015
         sob(3,jn) =  -0.0153924

         ! schmidt number 
         sca(1,jn) = 3828.1
         sca(2,jn) = -249.86
         sca(3,jn) = 8.7603
         sca(4,jn) = -0.1716
         sca(5,jn) = 0.001408

         ! atm. concentration
         atm_cfc(:,:,jn) = p_cfc(:,:,2)
      endif

      ! coefficient for SF6
      !----------------------
      if ( ln_sf6 ) then
         jn = jn + 1
         ! Solubility
         soa(1,jn) = -80.0343
         soa(2,jn) = 117.232
         soa(3,jn) =  29.5817
         soa(4,jn) =   0.0

         sob(1,jn) =  0.0335183 
         sob(2,jn) = -0.0373942 
         sob(3,jn) =  0.00774862

         ! schmidt number
         sca(1,jn) = 3177.5
         sca(2,jn) = -200.57
         sca(3,jn) = 6.8865
         sca(4,jn) = -0.13335
         sca(5,jn) = 0.0010877
  
         ! atm. concentration
         atm_cfc(:,:,jn) = p_cfc(:,:,3)
       endif

      IF( ln_rsttr ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' Read specific variables from CFC model '
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
         !
         jl = 0
         DO jn = jp_cfc0, jp_cfc1
            jl = jl + 1
            CALL iom_get( numrtr, jpdom_autoglo, 'qint_'//ctrcnm(jn), qint_cfc(:,:,jl) ) 
         END DO
      ENDIF
      IF(lwp) WRITE(numout,*)
      !
   END SUBROUTINE cfc_init


   INTEGER FUNCTION trc_sms_cfc_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_cfc_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( xphem   (jpi,jpj)        , atm_cfc(jpyear,jphem,jp_cfc)  ,    &
         &      qtr_cfc (jpi,jpj,jp_cfc) , qint_cfc(jpi,jpj,jp_cfc)      ,    &
         &      soa(4,jp_cfc)    ,  sob(3,jp_cfc)   ,  sca(5,jp_cfc)     ,    &
         &      STAT=trc_sms_cfc_alloc )
         !
      IF( trc_sms_cfc_alloc /= 0 ) CALL ctl_stop( 'STOP', 'trc_sms_cfc_alloc : failed to allocate arrays.' )
      !
   END FUNCTION trc_sms_cfc_alloc

   !!======================================================================
END MODULE trcsms_cfc
