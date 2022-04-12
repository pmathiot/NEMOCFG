MODULE icethd_dh
   !!======================================================================
   !!                       ***  MODULE icethd_dh ***
   !!   seaice : thermodynamic growth and melt 
   !!======================================================================
   !! History :       !  2003-05  (M. Vancoppenolle) Original code in 1D
   !!                 !  2005-06  (M. Vancoppenolle) 3D version 
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_dh        : vertical sea-ice growth and melt
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE ice            ! sea-ice: variables
   USE ice1D          ! sea-ice: thermodynamics variables
   USE icethd_sal     ! sea-ice: salinity profiles
   USE icevar         ! for CALL ice_var_snwblow
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_dh        ! called by ice_thd

   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icethd_dh.F90 13642 2020-10-19 22:58:34Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_dh
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd_dh  ***
      !!
      !! ** Purpose :   compute ice and snow thickness changes due to growth/melting
      !!
      !! ** Method  :   Ice/Snow surface melting arises from imbalance in surface fluxes
      !!                Bottom accretion/ablation arises from flux budget
      !!                Snow thickness can increase by precipitation and decrease by sublimation
      !!                If snow load excesses Archmiede limit, snow-ice is formed by
      !!                the flooding of sea-water in the snow
      !!
      !!                - Compute available flux of heat for surface ablation
      !!                - Compute snow and sea ice enthalpies
      !!                - Surface ablation and sublimation
      !!                - Bottom accretion/ablation
      !!                - Snow ice formation
      !!
      !! References : Bitz and Lipscomb, 1999, J. Geophys. Res.
      !!              Fichefet T. and M. Maqueda 1997, J. Geophys. Res., 102(C6), 12609-12646   
      !!              Vancoppenolle, Fichefet and Bitz, 2005, Geophys. Res. Let. 
      !!              Vancoppenolle et al.,2009, Ocean Modelling
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jk       ! dummy loop indices
      INTEGER  ::   iter         ! local integer

      REAL(wp) ::   ztmelts      ! local scalar
      REAL(wp) ::   zdum       
      REAL(wp) ::   zfracs       ! fractionation coefficient for bottom salt entrapment
      REAL(wp) ::   zswi1        ! switch for computation of bottom salinity
      REAL(wp) ::   zswi12       ! switch for computation of bottom salinity
      REAL(wp) ::   zswi2        ! switch for computation of bottom salinity
      REAL(wp) ::   zgrr         ! bottom growth rate
      REAL(wp) ::   zt_i_new     ! bottom formation temperature
      REAL(wp) ::   z1_rho       ! 1/(rhos+rau0-rhoi)

      REAL(wp) ::   zQm          ! enthalpy exchanged with the ocean (J/m2), >0 towards the ocean
      REAL(wp) ::   zEi          ! specific enthalpy of sea ice (J/kg)
      REAL(wp) ::   zEw          ! specific enthalpy of exchanged water (J/kg)
      REAL(wp) ::   zdE          ! specific enthalpy difference (J/kg)
      REAL(wp) ::   zfmdt        ! exchange mass flux x time step (J/m2), >0 towards the ocean

      REAL(wp), DIMENSION(jpij) ::   zqprec      ! energy of fallen snow                       (J.m-3)
      REAL(wp), DIMENSION(jpij) ::   zq_top      ! heat for surface ablation                   (J.m-2)
      REAL(wp), DIMENSION(jpij) ::   zq_bot      ! heat for bottom ablation                    (J.m-2)
      REAL(wp), DIMENSION(jpij) ::   zq_rema     ! remaining heat at the end of the routine    (J.m-2)
      REAL(wp), DIMENSION(jpij) ::   zf_tt       ! Heat budget to determine melting or freezing(W.m-2)
      REAL(wp), DIMENSION(jpij) ::   zevap_rema  ! remaining mass flux from sublimation        (kg.m-2)

      REAL(wp), DIMENSION(jpij) ::   zdh_s_mel   ! snow melt 
      REAL(wp), DIMENSION(jpij) ::   zdh_s_pre   ! snow precipitation 
      REAL(wp), DIMENSION(jpij) ::   zdh_s_sub   ! snow sublimation

      REAL(wp), DIMENSION(jpij,nlay_s) ::   zh_s      ! snw layer thickness
      REAL(wp), DIMENSION(jpij,nlay_i) ::   zh_i      ! ice layer thickness
      REAL(wp), DIMENSION(jpij,nlay_i) ::   zdeltah
      INTEGER , DIMENSION(jpij,nlay_i) ::   icount    ! number of layers vanished by melting 

      REAL(wp), DIMENSION(jpij) ::   zsnw        ! distribution of snow after wind blowing

      REAL(wp) ::   zswitch_sal

      INTEGER  ::   num_iter_max      ! Heat conservation 
      !!------------------------------------------------------------------

      ! Discriminate between time varying salinity and constant
      SELECT CASE( nn_icesal )                  ! varying salinity or not
         CASE( 1 , 3 )   ;   zswitch_sal = 0._wp   ! prescribed salinity profile
         CASE( 2 )       ;   zswitch_sal = 1._wp   ! varying salinity profile
      END SELECT

      ! initialize layer thicknesses and enthalpies
      h_i_old (1:npti,0:nlay_i+1) = 0._wp
      eh_i_old(1:npti,0:nlay_i+1) = 0._wp
      DO jk = 1, nlay_i
         DO ji = 1, npti
            h_i_old (ji,jk) = h_i_1d(ji) * r1_nlay_i
            eh_i_old(ji,jk) = e_i_1d(ji,jk) * h_i_old(ji,jk)
         END DO
      END DO
      !
      !                       ! ============================================== !
      !                       ! Available heat for surface and bottom ablation !
      !                       ! ============================================== !
      !
      IF( ln_cndflx .AND. .NOT.ln_cndemulate ) THEN
         !
         DO ji = 1, npti
            zq_top(ji)     = MAX( 0._wp, qml_ice_1d(ji) * rdt_ice )
         END DO
         !
      ELSE
         !
         DO ji = 1, npti
            zdum           = qns_ice_1d(ji) + qsr_ice_1d(ji) - qtr_ice_top_1d(ji) - qcn_ice_top_1d(ji)
            qml_ice_1d(ji) = zdum * MAX( 0._wp , SIGN( 1._wp, t_su_1d(ji) - rt0 ) )
            zq_top(ji)     = MAX( 0._wp, qml_ice_1d(ji) * rdt_ice )
         END DO
         !
      ENDIF
      !
      DO ji = 1, npti
         zf_tt(ji)         = qcn_ice_bot_1d(ji) + qsb_ice_bot_1d(ji) + fhld_1d(ji) + qtr_ice_bot_1d(ji) * frq_m_1d(ji) 
         zq_bot(ji)        = MAX( 0._wp, zf_tt(ji) * rdt_ice )
      END DO

      ! Ice and snow layer thicknesses
      !-------------------------------
      DO jk = 1, nlay_i
         DO ji = 1, npti
            zh_i(ji,jk) = h_i_1d(ji) * r1_nlay_i
         END DO
      END DO

      DO jk = 1, nlay_s
         DO ji = 1, npti
            zh_s(ji,jk) = h_s_1d(ji) * r1_nlay_s
         END DO
      END DO

      !                       ! ============ !
      !                       !     Snow     !
      !                       ! ============ !
      !
      ! Internal melting
      ! ----------------
      ! IF snow temperature is above freezing point, THEN snow melts (should not happen but sometimes it does)
      DO jk = 1, nlay_s
         DO ji = 1, npti
            IF( t_s_1d(ji,jk) > rt0 ) THEN
               hfx_res_1d    (ji) = hfx_res_1d    (ji) + e_s_1d(ji,jk) * zh_s(ji,jk) * a_i_1d(ji) * r1_rdtice   ! heat flux to the ocean [W.m-2], < 0
               wfx_snw_sum_1d(ji) = wfx_snw_sum_1d(ji) + rhos          * zh_s(ji,jk) * a_i_1d(ji) * r1_rdtice   ! mass flux
               ! updates
               dh_s_mlt(ji)    = dh_s_mlt(ji) - zh_s(ji,jk)
               h_s_1d  (ji)    = h_s_1d(ji) - zh_s(ji,jk)
               zh_s    (ji,jk) = 0._wp
               e_s_1d  (ji,jk) = 0._wp
               t_s_1d  (ji,jk) = rt0
            END IF
         END DO
      END DO         

      ! Snow precipitation
      !-------------------
      CALL ice_var_snwblow( 1. - at_i_1d(1:npti), zsnw(1:npti) )   ! snow distribution over ice after wind blowing

      zdeltah(1:npti,:) = 0._wp
      DO ji = 1, npti
         IF( sprecip_1d(ji) > 0._wp ) THEN
            !
            ! --- precipitation ---
            zdh_s_pre (ji) = zsnw(ji) * sprecip_1d(ji) * rdt_ice * r1_rhos / at_i_1d(ji)   ! thickness change
            zqprec    (ji) = - qprec_ice_1d(ji)                                             ! enthalpy of the precip (>0, J.m-3)
            !
            hfx_spr_1d(ji) = hfx_spr_1d(ji) + zdh_s_pre(ji) * a_i_1d(ji) * zqprec(ji)    * r1_rdtice   ! heat flux from snow precip (>0, W.m-2)
            wfx_spr_1d(ji) = wfx_spr_1d(ji) - rhos          * a_i_1d(ji) * zdh_s_pre(ji) * r1_rdtice   ! mass flux, <0
            
            ! --- melt of falling snow ---
            rswitch              = MAX( 0._wp , SIGN( 1._wp , zqprec(ji) - epsi20 ) )
            zdeltah       (ji,1) = - rswitch * zq_top(ji) / MAX( zqprec(ji) , epsi20 )   ! thickness change
            zdeltah       (ji,1) = MAX( - zdh_s_pre(ji), zdeltah(ji,1) )                 ! bound melting 
            hfx_snw_1d    (ji)   = hfx_snw_1d    (ji) - zdeltah(ji,1) * a_i_1d(ji) * zqprec(ji)    * r1_rdtice   ! heat used to melt snow (W.m-2, >0)
            wfx_snw_sum_1d(ji)   = wfx_snw_sum_1d(ji) - rhos          * a_i_1d(ji) * zdeltah(ji,1) * r1_rdtice   ! snow melting only = water into the ocean (then without snow precip), >0
            
            ! updates available heat + precipitations after melting
            dh_s_mlt (ji) = dh_s_mlt(ji) + zdeltah(ji,1)
            zq_top   (ji) = MAX( 0._wp , zq_top (ji) + zdeltah(ji,1) * zqprec(ji) )      
            zdh_s_pre(ji) = zdh_s_pre(ji) + zdeltah(ji,1)
            
            ! update thickness
            h_s_1d(ji) = MAX( 0._wp , h_s_1d(ji) + zdh_s_pre(ji) )
            !
         ELSE
            !
            zdh_s_pre(ji) = 0._wp
            zqprec   (ji) = 0._wp
            !
         ENDIF
      END DO

      ! recalculate snow layers
      DO jk = 1, nlay_s
         DO ji = 1, npti
            zh_s(ji,jk) = h_s_1d(ji) * r1_nlay_s
         END DO
      END DO

      ! Snow melting
      ! ------------
      ! If heat still available (zq_top > 0), then melt more snow
      zdeltah(1:npti,:) = 0._wp
      zdh_s_mel(1:npti) = 0._wp
      DO jk = 1, nlay_s
         DO ji = 1, npti
            IF( zh_s(ji,jk) > 0._wp .AND. zq_top(ji) > 0._wp ) THEN
               !
               rswitch          = MAX( 0._wp, SIGN( 1._wp, e_s_1d(ji,jk) - epsi20 ) )
               zdeltah  (ji,jk) = - rswitch * zq_top(ji) / MAX( e_s_1d(ji,jk), epsi20 )   ! thickness change
               zdeltah  (ji,jk) = MAX( zdeltah(ji,jk) , - zh_s(ji,jk) )                   ! bound melting
               zdh_s_mel(ji)    = zdh_s_mel(ji) + zdeltah(ji,jk)
               
               hfx_snw_1d(ji)     = hfx_snw_1d(ji)     - zdeltah(ji,jk) * a_i_1d(ji) * e_s_1d (ji,jk) * r1_rdtice   ! heat used to melt snow(W.m-2, >0)
               wfx_snw_sum_1d(ji) = wfx_snw_sum_1d(ji) - rhos           * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice   ! snow melting only = water into the ocean (then without snow precip)
               
               ! updates available heat + thickness
               dh_s_mlt(ji)    = dh_s_mlt(ji) + zdeltah(ji,jk)
               zq_top  (ji)    = MAX( 0._wp , zq_top(ji) + zdeltah(ji,jk) * e_s_1d(ji,jk) )
               h_s_1d  (ji)    = MAX( 0._wp , h_s_1d(ji) + zdeltah(ji,jk) )
               zh_s    (ji,jk) = MAX( 0._wp , zh_s(ji,jk) + zdeltah(ji,jk) )
               !
            ENDIF
         END DO
      END DO

      ! Snow sublimation 
      !-----------------
      ! qla_ice is always >=0 (upwards), heat goes to the atmosphere, therefore snow sublimates
      !    comment: not counted in mass/heat exchange in iceupdate.F90 since this is an exchange with atm. (not ocean)
      zdeltah(1:npti,:) = 0._wp
      DO ji = 1, npti
         IF( evap_ice_1d(ji) > 0._wp ) THEN
            !
            zdh_s_sub (ji)   = MAX( - h_s_1d(ji) , - evap_ice_1d(ji) * r1_rhos * rdt_ice )
            zevap_rema(ji)   = evap_ice_1d(ji) * rdt_ice + zdh_s_sub(ji) * rhos   ! remaining evap in kg.m-2 (used for ice melting later on)
            zdeltah   (ji,1) = MAX( zdh_s_sub(ji), - zdh_s_pre(ji) )
            
            hfx_sub_1d    (ji) = hfx_sub_1d(ji) + &   ! Heat flux by sublimation [W.m-2], < 0 (sublimate snow that had fallen, then pre-existing snow)
               &                 ( zdeltah(ji,1) * zqprec(ji) + ( zdh_s_sub(ji) - zdeltah(ji,1) ) * e_s_1d(ji,1) )  &
               &                 * a_i_1d(ji) * r1_rdtice
            wfx_snw_sub_1d(ji) = wfx_snw_sub_1d(ji) - rhos * a_i_1d(ji) * zdh_s_sub(ji) * r1_rdtice   ! Mass flux by sublimation
            
            ! new snow thickness
            h_s_1d(ji)    =  MAX( 0._wp , h_s_1d(ji) + zdh_s_sub(ji) )
            ! update precipitations after sublimation and correct sublimation
            zdh_s_pre(ji) = zdh_s_pre(ji) + zdeltah(ji,1)
            zdh_s_sub(ji) = zdh_s_sub(ji) - zdeltah(ji,1)
            !
         ELSE
            !
            zdh_s_sub (ji) = 0._wp
            zevap_rema(ji) = 0._wp
            !
         ENDIF
      END DO
      
      ! --- Update snow diags --- !
      DO ji = 1, npti
         dh_s_tot(ji) = zdh_s_mel(ji) + zdh_s_pre(ji) + zdh_s_sub(ji)
      END DO

      ! Update temperature, energy
      !---------------------------
      ! new temp and enthalpy of the snow (remaining snow precip + remaining pre-existing snow)
      DO jk = 1, nlay_s
         DO ji = 1,npti
            rswitch       = MAX( 0._wp , SIGN( 1._wp, h_s_1d(ji) - epsi20 ) )
            e_s_1d(ji,jk) = rswitch / MAX( h_s_1d(ji), epsi20 ) *            &
              &             ( ( zdh_s_pre(ji)              ) * zqprec(ji) +  &
              &               ( h_s_1d(ji) - zdh_s_pre(ji) ) * rhos * ( rcpi * ( rt0 - t_s_1d(ji,jk) ) + rLfus ) )
         END DO
      END DO
      
      !                       ! ============ !
      !                       !     Ice      !
      !                       ! ============ !

      ! Surface ice melting 
      !--------------------
      zdeltah(1:npti,:) = 0._wp ! important
      DO jk = 1, nlay_i
         DO ji = 1, npti
            ztmelts = - rTmlt * sz_i_1d(ji,jk)   ! Melting point of layer k [C]
            
            IF( t_i_1d(ji,jk) >= (ztmelts+rt0) ) THEN   !-- Internal melting

               zEi            = - e_i_1d(ji,jk) * r1_rhoi             ! Specific enthalpy of layer k [J/kg, <0]       
               zdE            = 0._wp                                 ! Specific enthalpy difference (J/kg, <0)
                                                                      ! set up at 0 since no energy is needed to melt water...(it is already melted)
               zdeltah(ji,jk) = MIN( 0._wp , - zh_i(ji,jk) )          ! internal melting occurs when the internal temperature is above freezing     
                                                                      ! this should normally not happen, but sometimes, heat diffusion leads to this
               zfmdt          = - zdeltah(ji,jk) * rhoi               ! Mass flux x time step > 0
                         
               dh_i_itm(ji)   = dh_i_itm(ji) + zdeltah(ji,jk)         ! Cumulate internal melting
               
               zfmdt          = - rhoi * zdeltah(ji,jk)               ! Recompute mass flux [kg/m2, >0]

               hfx_res_1d(ji) = hfx_res_1d(ji) + zfmdt * a_i_1d(ji) * zEi * r1_rdtice                           ! Heat flux to the ocean [W.m-2], <0
               !                                                                                                  ice enthalpy zEi is "sent" to the ocean
               sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * s_i_1d(ji) * r1_rdtice    ! Salt flux
               !                                                                                                  using s_i_1d and not sz_i_1d(jk) is ok
               wfx_res_1d(ji) = wfx_res_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice                 ! Mass flux

            ELSE                                        !-- Surface melting
               
               zEi            = - e_i_1d(ji,jk) * r1_rhoi             ! Specific enthalpy of layer k [J/kg, <0]
               zEw            =    rcp * ztmelts                      ! Specific enthalpy of resulting meltwater [J/kg, <0]
               zdE            =    zEi - zEw                          ! Specific enthalpy difference < 0
               
               zfmdt          = - zq_top(ji) / zdE                    ! Mass flux to the ocean [kg/m2, >0]
               
               zdeltah(ji,jk) = - zfmdt * r1_rhoi                     ! Melt of layer jk [m, <0]
               
               zdeltah(ji,jk) = MIN( 0._wp , MAX( zdeltah(ji,jk) , - zh_i(ji,jk) ) )    ! Melt of layer jk cannot exceed the layer thickness [m, <0]
               
               zq_top(ji)      = MAX( 0._wp , zq_top(ji) - zdeltah(ji,jk) * rhoi * zdE ) ! update available heat
               
               dh_i_sum(ji)   = dh_i_sum(ji) + zdeltah(ji,jk)         ! Cumulate surface melt
               
               zfmdt          = - rhoi * zdeltah(ji,jk)               ! Recompute mass flux [kg/m2, >0]
               
               zQm            = zfmdt * zEw                           ! Energy of the melt water sent to the ocean [J/m2, <0]
               
               sfx_sum_1d(ji) = sfx_sum_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * s_i_1d(ji) * r1_rdtice    ! Salt flux >0
               !                                                                                                  using s_i_1d and not sz_i_1d(jk) is ok)
               hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice                           ! Heat flux [W.m-2], < 0
               hfx_sum_1d(ji) = hfx_sum_1d(ji) - zfmdt * a_i_1d(ji) * zdE * r1_rdtice                           ! Heat flux used in this process [W.m-2], > 0  
               ! 
               wfx_sum_1d(ji) = wfx_sum_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice                 ! Mass flux
               
            END IF
            
            ! Ice sublimation
            ! ---------------
            zdum            = MAX( - ( zh_i(ji,jk) + zdeltah(ji,jk) ) , - zevap_rema(ji) * r1_rhoi )
            zdeltah (ji,jk) = zdeltah (ji,jk) + zdum
            dh_i_sub(ji)    = dh_i_sub(ji)    + zdum
            
            sfx_sub_1d(ji)     = sfx_sub_1d(ji) - rhoi * a_i_1d(ji) * zdum * s_i_1d(ji) * r1_rdtice  ! Salt flux >0
            !                                                                                          clem: flux is sent to the ocean for simplicity
            !                                                                                                but salt should remain in the ice except
            !                                                                                                if all ice is melted. => must be corrected
            hfx_sub_1d(ji)     = hfx_sub_1d(ji) + zdum * e_i_1d(ji,jk) * a_i_1d(ji) * r1_rdtice      ! Heat flux [W.m-2], < 0

            wfx_ice_sub_1d(ji) = wfx_ice_sub_1d(ji) - rhoi * a_i_1d(ji) * zdum * r1_rdtice           ! Mass flux > 0

            ! update remaining mass flux
            zevap_rema(ji)  = zevap_rema(ji) + zdum * rhoi
            
            ! record which layers have disappeared (for bottom melting) 
            !    => icount=0 : no layer has vanished
            !    => icount=5 : 5 layers have vanished
            rswitch       = MAX( 0._wp , SIGN( 1._wp , - ( zh_i(ji,jk) + zdeltah(ji,jk) ) ) ) 
            icount(ji,jk) = NINT( rswitch )
            zh_i(ji,jk)   = MAX( 0._wp , zh_i(ji,jk) + zdeltah(ji,jk) )
                        
            ! update heat content (J.m-2) and layer thickness
            eh_i_old(ji,jk) = eh_i_old(ji,jk) + zdeltah(ji,jk) * e_i_1d(ji,jk)
            h_i_old (ji,jk) = h_i_old (ji,jk) + zdeltah(ji,jk)
         END DO
      END DO
      
      ! update ice thickness
      DO ji = 1, npti
         h_i_1d(ji) =  MAX( 0._wp , h_i_1d(ji) + dh_i_sum(ji) + dh_i_itm(ji) + dh_i_sub(ji) )
      END DO

      ! remaining "potential" evap is sent to ocean
      DO ji = 1, npti
         wfx_err_sub_1d(ji) = wfx_err_sub_1d(ji) - zevap_rema(ji) * a_i_1d(ji) * r1_rdtice  ! <=0 (net evap for the ocean in kg.m-2.s-1)
      END DO


      ! Ice Basal growth 
      !------------------
      ! Basal growth is driven by heat imbalance at the ice-ocean interface,
      ! between the inner conductive flux  (qcn_ice_bot), from the open water heat flux 
      ! (fhld) and the sensible ice-ocean flux (qsb_ice_bot). 
      ! qcn_ice_bot is positive downwards. qsb_ice_bot and fhld are positive to the ice 

      ! If salinity varies in time, an iterative procedure is required, because
      ! the involved quantities are inter-dependent.
      ! Basal growth (dh_i_bog) depends upon new ice specific enthalpy (zEi),
      ! which depends on forming ice salinity (s_i_new), which depends on dh/dt (dh_i_bog)
      ! -> need for an iterative procedure, which converges quickly

      num_iter_max = 1
      IF( nn_icesal == 2 )   num_iter_max = 5  ! salinity varying in time
      
      DO ji = 1, npti
         IF(  zf_tt(ji) < 0._wp  ) THEN
            DO iter = 1, num_iter_max   ! iterations

               ! New bottom ice salinity (Cox & Weeks, JGR88 )
               !--- zswi1  if dh/dt < 2.0e-8
               !--- zswi12 if 2.0e-8 < dh/dt < 3.6e-7 
               !--- zswi2  if dh/dt > 3.6e-7
               zgrr     = MIN( 1.0e-3, MAX ( dh_i_bog(ji) * r1_rdtice , epsi10 ) )
               zswi2    = MAX( 0._wp , SIGN( 1._wp , zgrr - 3.6e-7 ) )
               zswi12   = MAX( 0._wp , SIGN( 1._wp , zgrr - 2.0e-8 ) ) * ( 1.0 - zswi2 )
               zswi1    = 1. - zswi2 * zswi12
               zfracs   = MIN( zswi1  * 0.12 + zswi12 * ( 0.8925 + 0.0568 * LOG( 100.0 * zgrr ) )   &
                  &          + zswi2  * 0.26 / ( 0.26 + 0.74 * EXP ( - 724300.0 * zgrr ) )  , 0.5 )

               s_i_new(ji)   = zswitch_sal * zfracs * sss_1d(ji) + ( 1. - zswitch_sal ) * s_i_1d(ji)  ! New ice salinity

               ztmelts       = - rTmlt * s_i_new(ji)                                                  ! New ice melting point (C)

               zt_i_new      = zswitch_sal * t_bo_1d(ji) + ( 1. - zswitch_sal) * t_i_1d(ji, nlay_i)
               
               zEi           = rcpi * ( zt_i_new - (ztmelts+rt0) ) &                                  ! Specific enthalpy of forming ice (J/kg, <0)
                  &            - rLfus * ( 1.0 - ztmelts / ( zt_i_new - rt0 ) ) + rcp  * ztmelts

               zEw           = rcp  * ( t_bo_1d(ji) - rt0 )                                           ! Specific enthalpy of seawater (J/kg, < 0)

               zdE           = zEi - zEw                                                              ! Specific enthalpy difference (J/kg, <0)

               dh_i_bog(ji)  = rdt_ice * MAX( 0._wp , zf_tt(ji) / ( zdE * rhoi ) )
               
            END DO
            ! Contribution to Energy and Salt Fluxes                                    
            zfmdt          = - rhoi * dh_i_bog(ji)                                                   ! Mass flux x time step (kg/m2, < 0)
            
            hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice                           ! Heat flux to the ocean [W.m-2], >0
            hfx_bog_1d(ji) = hfx_bog_1d(ji) - zfmdt * a_i_1d(ji) * zdE * r1_rdtice                           ! Heat flux used in this process [W.m-2], <0
            
            sfx_bog_1d(ji) = sfx_bog_1d(ji) - rhoi * a_i_1d(ji) * dh_i_bog(ji) * s_i_new(ji) * r1_rdtice     ! Salt flux, <0

            wfx_bog_1d(ji) = wfx_bog_1d(ji) - rhoi * a_i_1d(ji) * dh_i_bog(ji) * r1_rdtice                   ! Mass flux, <0

            ! update heat content (J.m-2) and layer thickness
            eh_i_old(ji,nlay_i+1) = eh_i_old(ji,nlay_i+1) + dh_i_bog(ji) * (-zEi * rhoi)
            h_i_old (ji,nlay_i+1) = h_i_old (ji,nlay_i+1) + dh_i_bog(ji)

         ENDIF

      END DO

      ! Ice Basal melt
      !---------------
      zdeltah(1:npti,:) = 0._wp ! important
      DO jk = nlay_i, 1, -1
         DO ji = 1, npti
            IF(  zf_tt(ji)  >  0._wp  .AND. jk > icount(ji,jk) ) THEN   ! do not calculate where layer has already disappeared by surface melting 

               ztmelts = - rTmlt * sz_i_1d(ji,jk)  ! Melting point of layer jk (C)

               IF( t_i_1d(ji,jk) >= (ztmelts+rt0) ) THEN   !-- Internal melting

                  zEi               = - e_i_1d(ji,jk) * r1_rhoi     ! Specific enthalpy of melting ice (J/kg, <0)
                  zdE               = 0._wp                         ! Specific enthalpy difference   (J/kg, <0)
                                                                    !    set up at 0 since no energy is needed to melt water...(it is already melted)
                  zdeltah   (ji,jk) = MIN( 0._wp , - zh_i(ji,jk) )  ! internal melting occurs when the internal temperature is above freezing     
                                                                    ! this should normally not happen, but sometimes, heat diffusion leads to this

                  dh_i_itm (ji)     = dh_i_itm(ji) + zdeltah(ji,jk)

                  zfmdt             = - zdeltah(ji,jk) * rhoi      ! Mass flux x time step > 0

                  hfx_res_1d(ji) = hfx_res_1d(ji) + zfmdt * a_i_1d(ji) * zEi * r1_rdtice                           ! Heat flux to the ocean [W.m-2], <0
                  !                                                                                                  ice enthalpy zEi is "sent" to the ocean
                  sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * s_i_1d(ji) * r1_rdtice    ! Salt flux
                  !                                                                                                  using s_i_1d and not sz_i_1d(jk) is ok
                  wfx_res_1d(ji) = wfx_res_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice                 ! Mass flux

                  ! update heat content (J.m-2) and layer thickness
                  eh_i_old(ji,jk) = eh_i_old(ji,jk) + zdeltah(ji,jk) * e_i_1d(ji,jk)
                  h_i_old (ji,jk) = h_i_old (ji,jk) + zdeltah(ji,jk)

               ELSE                                        !-- Basal melting

                  zEi             = - e_i_1d(ji,jk) * r1_rhoi                                 ! Specific enthalpy of melting ice (J/kg, <0)
                  zEw             = rcp * ztmelts                                             ! Specific enthalpy of meltwater (J/kg, <0)
                  zdE             = zEi - zEw                                                 ! Specific enthalpy difference   (J/kg, <0)

                  zfmdt           = - zq_bot(ji) / zdE                                        ! Mass flux x time step (kg/m2, >0)

                  zdeltah(ji,jk)  = - zfmdt * r1_rhoi                                         ! Gross thickness change

                  zdeltah(ji,jk)  = MIN( 0._wp , MAX( zdeltah(ji,jk), - zh_i(ji,jk) ) )       ! bound thickness change
                  
                  zq_bot(ji)      = MAX( 0._wp , zq_bot(ji) - zdeltah(ji,jk) * rhoi * zdE )   ! update available heat. MAX is necessary for roundup errors

                  dh_i_bom(ji)    = dh_i_bom(ji) + zdeltah(ji,jk)                             ! Update basal melt

                  zfmdt           = - zdeltah(ji,jk) * rhoi                                   ! Mass flux x time step > 0

                  zQm             = zfmdt * zEw                                               ! Heat exchanged with ocean

                  hfx_thd_1d(ji)  = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice                           ! Heat flux to the ocean [W.m-2], <0  
                  hfx_bom_1d(ji)  = hfx_bom_1d(ji) - zfmdt * a_i_1d(ji) * zdE * r1_rdtice                           ! Heat used in this process [W.m-2], >0  

                  sfx_bom_1d(ji)  = sfx_bom_1d(ji) - rhoi *  a_i_1d(ji) * zdeltah(ji,jk) * s_i_1d(ji) * r1_rdtice   ! Salt flux
                  !                                                                                                   using s_i_1d and not sz_i_1d(jk) is ok
                  
                  wfx_bom_1d(ji)  = wfx_bom_1d(ji) - rhoi * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice                 ! Mass flux

                  ! update heat content (J.m-2) and layer thickness
                  eh_i_old(ji,jk) = eh_i_old(ji,jk) + zdeltah(ji,jk) * e_i_1d(ji,jk)
                  h_i_old (ji,jk) = h_i_old (ji,jk) + zdeltah(ji,jk)
               ENDIF
           
            ENDIF
         END DO
      END DO

      ! Update temperature, energy
      ! --------------------------
      DO ji = 1, npti
         h_i_1d(ji) = MAX( 0._wp , h_i_1d(ji) + dh_i_bog(ji) + dh_i_bom(ji) )
      END DO  

      ! If heat still available then melt more snow
      !-------------------------------------------
      zdeltah(1:npti,:) = 0._wp ! important
      DO ji = 1, npti
         zq_rema (ji)   = zq_top(ji) + zq_bot(ji) 
         rswitch        = 1._wp - MAX( 0._wp, SIGN( 1._wp, - h_s_1d(ji) ) )   ! =1 if snow
         rswitch        = rswitch * MAX( 0._wp, SIGN( 1._wp, e_s_1d(ji,1) - epsi20 ) )
         zdeltah (ji,1) = - rswitch * zq_rema(ji) / MAX( e_s_1d(ji,1), epsi20 )
         zdeltah (ji,1) = MIN( 0._wp , MAX( zdeltah(ji,1) , - h_s_1d(ji) ) ) ! bound melting
         dh_s_tot(ji)   = dh_s_tot(ji) + zdeltah(ji,1)
         h_s_1d  (ji)   = h_s_1d  (ji) + zdeltah(ji,1)
        
         zq_rema(ji)        = zq_rema(ji) + zdeltah(ji,1) * e_s_1d(ji,1)                               ! update available heat (J.m-2)
         hfx_snw_1d(ji)     = hfx_snw_1d(ji) - zdeltah(ji,1) * a_i_1d(ji) * e_s_1d(ji,1) * r1_rdtice   ! Heat used to melt snow, W.m-2 (>0)
         wfx_snw_sum_1d(ji) = wfx_snw_sum_1d(ji) - rhos * a_i_1d(ji) * zdeltah(ji,1) * r1_rdtice       ! Mass flux
         dh_s_mlt(ji)       = dh_s_mlt(ji) + zdeltah(ji,1)
         !    
         ! Remaining heat flux (W.m-2) is sent to the ocean heat budget
         !!!hfx_res_1d(ji) = hfx_res_1d(ji) + ( zq_rema(ji) * a_i_1d(ji) ) * r1_rdtice

         IF( ln_icectl .AND. zq_rema(ji) < 0. .AND. lwp ) WRITE(numout,*) 'ALERTE zq_rema <0 = ', zq_rema(ji)
      END DO

      !
      ! Snow-Ice formation
      ! ------------------
      ! When snow load excesses Archimede's limit, snow-ice interface goes down under sea-level, 
      ! flooding of seawater transforms snow into ice dh_snowice is positive for the ice
      z1_rho = 1._wp / ( rhos+rau0-rhoi )
      DO ji = 1, npti
         !
         dh_snowice(ji) = MAX(  0._wp , ( rhos * h_s_1d(ji) + (rhoi-rau0) * h_i_1d(ji) ) * z1_rho )

         h_i_1d(ji)    = h_i_1d(ji) + dh_snowice(ji)
         h_s_1d(ji)    = h_s_1d(ji) - dh_snowice(ji)

         ! Contribution to energy flux to the ocean [J/m2], >0 (if sst<0)
         zfmdt          = ( rhos - rhoi ) * dh_snowice(ji)    ! <0
         zEw            = rcp * sst_1d(ji)
         zQm            = zfmdt * zEw 
         
         hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice ! Heat flux

         sfx_sni_1d(ji) = sfx_sni_1d(ji) + sss_1d(ji) * a_i_1d(ji) * zfmdt * r1_rdtice ! Salt flux

         ! Case constant salinity in time: virtual salt flux to keep salinity constant
         IF( nn_icesal /= 2 )  THEN
            sfx_bri_1d(ji) = sfx_bri_1d(ji) - sss_1d (ji) * a_i_1d(ji) * zfmdt                  * r1_rdtice  & ! put back sss_m     into the ocean
               &                            - s_i_1d(ji)  * a_i_1d(ji) * dh_snowice(ji) * rhoi * r1_rdtice     ! and get  rn_icesal from the ocean 
         ENDIF

         ! Mass flux: All snow is thrown in the ocean, and seawater is taken to replace the volume
         wfx_sni_1d(ji)     = wfx_sni_1d(ji)     - a_i_1d(ji) * dh_snowice(ji) * rhoi * r1_rdtice
         wfx_snw_sni_1d(ji) = wfx_snw_sni_1d(ji) + a_i_1d(ji) * dh_snowice(ji) * rhos * r1_rdtice

         ! update heat content (J.m-2) and layer thickness
         eh_i_old(ji,0) = eh_i_old(ji,0) + dh_snowice(ji) * e_s_1d(ji,1) + zfmdt * zEw
         h_i_old (ji,0) = h_i_old (ji,0) + dh_snowice(ji)
         
      END DO

      !
      ! Update temperature, energy
      ! --------------------------
      DO ji = 1, npti
         rswitch     = 1._wp - MAX( 0._wp , SIGN( 1._wp , - h_i_1d(ji) ) ) 
         t_su_1d(ji) = rswitch * t_su_1d(ji) + ( 1._wp - rswitch ) * rt0
      END DO

      DO jk = 1, nlay_s
         DO ji = 1,npti
            ! where there is no ice or no snow
            rswitch = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, - h_s_1d(ji) ) ) ) * ( 1._wp - MAX( 0._wp, SIGN(1._wp, - h_i_1d(ji) ) ) )
            ! mass & energy loss to the ocean
            hfx_res_1d(ji) = hfx_res_1d(ji) + ( 1._wp - rswitch ) * &
               &                              ( e_s_1d(ji,jk) * h_s_1d(ji) * r1_nlay_s * a_i_1d(ji) * r1_rdtice )  ! heat flux to the ocean [W.m-2], < 0
            wfx_res_1d(ji) = wfx_res_1d(ji) + ( 1._wp - rswitch ) * &
               &                              ( rhos          * h_s_1d(ji) * r1_nlay_s * a_i_1d(ji) * r1_rdtice )  ! mass flux
            ! update energy (mass is updated in the next loop)
            e_s_1d(ji,jk) = rswitch * e_s_1d(ji,jk)
            ! recalculate t_s_1d from e_s_1d
            t_s_1d(ji,jk) = rt0 + rswitch * ( - e_s_1d(ji,jk) * r1_rhos * r1_rcpi + rLfus * r1_rcpi )
         END DO
      END DO

      ! --- ensure that a_i = 0 & h_s = 0 where h_i = 0 ---
      WHERE( h_i_1d(1:npti) == 0._wp )   
         a_i_1d(1:npti) = 0._wp
         h_s_1d(1:npti) = 0._wp
      END WHERE
      !
   END SUBROUTINE ice_thd_dh

#else
   !!----------------------------------------------------------------------
   !!   Default option                                NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd_dh
