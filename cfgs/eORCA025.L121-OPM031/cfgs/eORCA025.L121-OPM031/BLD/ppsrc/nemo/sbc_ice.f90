










MODULE sbc_ice
   !!======================================================================
   !!                 ***  MODULE  sbc_ice  ***
   !! Surface module - SI3 & CICE: parameters & variables defined in memory
   !!======================================================================
   !! History :  3.0   !  2006-08  (G. Madec)        Surface module
   !!            3.2   !  2009-06  (S. Masson)       merge with ice_oce
   !!            3.3.1 !  2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.4   !  2011-11  (C. Harris)       CICE added as an option
   !!            4.0   !  2018     (many people)     SI3 compatibility
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_si3' or 'key_cice' :              SI3 or CICE sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE sbc_oce          ! surface boundary condition: ocean
   USE ice              ! SI3 parameters
   USE lib_mpp          ! MPP library
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ice_alloc   ! called in sbcmod.F90 or sbcice_cice.F90

   LOGICAL         , PUBLIC, PARAMETER ::   lk_si3     = .TRUE.   !: SI3 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .FALSE.  !: no CICE 

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qns_ice        !: non solar heat flux over ice                  [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qsr_ice        !: solar heat flux over ice                      [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qla_ice        !: latent flux over ice                          [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dqla_ice       !: latent sensibility over ice                 [W/m2/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dqns_ice       !: non solar heat flux over ice (LW+SEN+LA)    [W/m2/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tn_ice         !: ice surface temperature                          [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   alb_ice        !: ice albedo                                       [-]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qml_ice        !: heat available for snow / ice surface melting     [W/m2] 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qcn_ice        !: heat conduction flux in the layer below surface   [W/m2] 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qtr_ice_top    !: solar flux transmitted below the ice surface      [W/m2]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   utau_ice       !: atmos-ice u-stress. VP: I-pt ; EVP: U,V-pts   [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   vtau_ice       !: atmos-ice v-stress. VP: I-pt ; EVP: U,V-pts   [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_ice        !: sublimation - precip over sea ice          [kg/m2/s]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   topmelt            !: category topmelt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   botmelt            !: category botmelt

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   evap_ice       !: sublimation                              [kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   devap_ice      !: sublimation sensitivity                [kg/m2/s/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qns_oce        !: non solar heat flux over ocean              [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qsr_oce        !: non solar heat flux over ocean              [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qemp_oce       !: heat flux of precip and evap over ocean     [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qemp_ice       !: heat flux of precip and evap over ice       [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qevap_ice      !: heat flux of evap over ice                  [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qprec_ice      !: enthalpy of precip over ice                 [J/m3]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_oce        !: evap - precip over ocean                 [kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wndm_ice       !: wind speed module at T-point                 [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sstfrz         !: sea surface freezing temperature            [degC]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rCdU_ice       !: ice-ocean drag at T-point (<0)               [m/s]


   REAL(wp), PUBLIC, SAVE ::   pp_cldf = 0.81    !: cloud fraction over sea ice, summer CLIO value   [-]

   !! arrays relating to embedding ice in the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass        !: mass of snow and ice at current  ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass_b      !: mass of snow and ice at previous ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_fmass       !: time evolution of mass of snow+ice               [Kg/m2/s]

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbc_ice.F90 13444 2020-08-31 08:58:55Z gsamson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_ice_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  FUNCTION sbc_ice_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(4)
      !!----------------------------------------------------------------------
      ierr(:) = 0

      ALLOCATE( snwice_mass(jpi,jpj) , snwice_mass_b(jpi,jpj), snwice_fmass(jpi,jpj) , STAT=ierr(1) )

      ALLOCATE( qns_ice (jpi,jpj,jpl) , qsr_ice  (jpi,jpj,jpl) ,     &
         &      qla_ice (jpi,jpj,jpl) , dqla_ice (jpi,jpj,jpl) ,     &
         &      dqns_ice(jpi,jpj,jpl) , tn_ice   (jpi,jpj,jpl) , alb_ice    (jpi,jpj,jpl) ,   &
         &      qml_ice (jpi,jpj,jpl) , qcn_ice  (jpi,jpj,jpl) , qtr_ice_top(jpi,jpj,jpl) ,   &
         &      utau_ice(jpi,jpj)     , vtau_ice (jpi,jpj)     , wndm_ice   (jpi,jpj)     ,   &
         &      evap_ice(jpi,jpj,jpl) , devap_ice(jpi,jpj,jpl) , qprec_ice  (jpi,jpj)     ,   &
         &      qemp_ice(jpi,jpj)     , qevap_ice(jpi,jpj,jpl) , qemp_oce   (jpi,jpj)     ,   &
         &      qns_oce (jpi,jpj)     , qsr_oce  (jpi,jpj)     , emp_oce    (jpi,jpj)     ,   &
         &      emp_ice (jpi,jpj)     , sstfrz   (jpi,jpj)     , rCdU_ice   (jpi,jpj)     , STAT= ierr(2) )


      sbc_ice_alloc = MAXVAL( ierr )
      CALL mpp_sum ( 'sbc_ice', sbc_ice_alloc )
      IF( sbc_ice_alloc > 0 )   CALL ctl_warn('sbc_ice_alloc: allocation of arrays failed')
   END FUNCTION sbc_ice_alloc


   !!======================================================================
END MODULE sbc_ice
