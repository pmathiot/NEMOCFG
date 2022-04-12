










MODULE icethd_sal
   !!======================================================================
   !!                       ***  MODULE icethd_sal ***
   !!   sea-ice : computation of salinity variations in the ice
   !!======================================================================
   !! History :   -   !  2003-05  (M. Vancoppenolle) original code 1-D
   !!            3.0  !  2005-12  (M. Vancoppenolle) adapted to the 3-D version
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!---------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_sal      : salinity variations in the ice
   !!   ice_thd_sal_init : initialization
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE ice            ! sea-ice: variables
   USE ice1D          ! sea-ice: thermodynamics variables
   USE icevar         ! sea-ice: operations
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_sal        ! called by icethd
   PUBLIC   ice_thd_sal_init   ! called by ice_init
   
   ! ** namelist (namthd_sal) **
   REAL(wp) ::   rn_sal_gd     ! restoring salinity for gravity drainage [PSU]
   REAL(wp) ::   rn_time_gd    ! restoring time constant for gravity drainage (= 20 days) [s]
   REAL(wp) ::   rn_sal_fl     ! restoring salinity for flushing [PSU]
   REAL(wp) ::   rn_time_fl    ! restoring time constant for gravity drainage (= 10 days) [s]

   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icethd_sal.F90 13284 2020-07-09 15:12:23Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_sal( ld_sal )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd_sal  ***    
      !!   
      !! ** Purpose :   computes new salinities in the ice
      !!
      !! ** Method  :  3 possibilities
      !!               -> nn_icesal = 1 -> Sice = cst    [ice salinity constant in both time & space] 
      !!               -> nn_icesal = 2 -> Sice = S(z,t) [Vancoppenolle et al. 2005]
      !!               -> nn_icesal = 3 -> Sice = S(z)   [multiyear ice]
      !!---------------------------------------------------------------------
      LOGICAL, INTENT(in) ::   ld_sal          ! gravity drainage and flushing or not 
      !
      INTEGER  ::   ji                         ! dummy loop indices 
      REAL(wp) ::   zs_sni, zds                ! local scalars
      REAL(wp) ::   z1_time_gd, z1_time_fl
      !!---------------------------------------------------------------------

      SELECT CASE ( nn_icesal )
      !
      !               !---------------------------------------------!
      CASE( 2 )       !  time varying salinity with linear profile  !
         !            !---------------------------------------------!
         z1_time_gd = rdt_ice / rn_time_gd
         z1_time_fl = rdt_ice / rn_time_fl
         !
         DO ji = 1, npti
            !
            IF( h_i_1d(ji) > 0._wp ) THEN
               !
               ! --- Update ice salinity from snow-ice and bottom growth --- !
               zs_sni = sss_1d(ji) * ( rhoi - rhos ) * r1_rhoi                           ! salinity of snow ice
               zds    =       ( zs_sni      - s_i_1d(ji) ) * dh_snowice(ji) / h_i_1d(ji) ! snow-ice    
               zds    = zds + ( s_i_new(ji) - s_i_1d(ji) ) * dh_i_bog  (ji) / h_i_1d(ji) ! bottom growth
               ! update salinity (nb: salt flux already included in icethd_dh)
               s_i_1d(ji) = s_i_1d(ji) + zds
               !
               ! --- Update ice salinity from brine drainage and flushing --- !
               IF( ld_sal ) THEN
                  IF( t_su_1d(ji) >= rt0 ) THEN             ! flushing (summer time)
                     zds = - MAX( s_i_1d(ji) - rn_sal_fl , 0._wp ) * z1_time_fl
                  ELSEIF( t_su_1d(ji) <= t_bo_1d(ji) ) THEN ! gravity drainage
                     zds = - MAX( s_i_1d(ji) - rn_sal_gd , 0._wp ) * z1_time_gd
                  ELSE
                     zds = 0._wp
                  ENDIF
                  ! update salinity
                  s_i_1d(ji) = s_i_1d(ji) + zds
                  ! salt flux
                  sfx_bri_1d(ji) = sfx_bri_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * zds * r1_rdtice
               ENDIF
               !
               ! --- salinity must stay inbounds --- !
               zds =       MAX( 0._wp, rn_simin - s_i_1d(ji) ) ! > 0 if s_i < simin
               zds = zds + MIN( 0._wp, rn_simax - s_i_1d(ji) ) ! < 0 if s_i > simax
               ! update salinity
               s_i_1d(ji) = s_i_1d(ji) + zds
               ! salt flux
               sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * zds * r1_rdtice
               !
            ENDIF
            !
         END DO
         !
         ! Salinity profile
         CALL ice_var_salprof1d
         !
         !             !----------------------------------------!
      CASE( 3 )        ! constant salinity with a fixed profile ! (Schwarzacher (1959) multiyear salinity profile (mean = 2.30)
         !             !----------------------------------------!
         CALL ice_var_salprof1d
         !
      END SELECT
      !
   END SUBROUTINE ice_thd_sal


   SUBROUTINE ice_thd_sal_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_thd_sal_init  ***
      !!
      !! ** Purpose :   initialization of ice salinity parameters
      !!
      !! ** Method  :   Read the namthd_sal namelist and check the parameter
      !!                values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namthd_sal
      !!-------------------------------------------------------------------
      INTEGER  ::   ios   ! Local integer
      !!
      NAMELIST/namthd_sal/ nn_icesal, rn_icesal, rn_sal_gd, rn_time_gd,   &
         &                 rn_sal_fl, rn_time_fl, rn_simax , rn_simin 
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namthd_sal in reference namelist : Ice salinity
      READ  ( numnam_ice_ref, namthd_sal, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namthd_sal in reference namelist' )
      REWIND( numnam_ice_cfg )              ! Namelist namthd_sal in configuration namelist : Ice salinity
      READ  ( numnam_ice_cfg, namthd_sal, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namthd_sal in configuration namelist' )
      IF(lwm) WRITE ( numoni, namthd_sal )
      !
      IF(lwp) THEN                           ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_thd_sal_init : Ice parameters for salinity '
         WRITE(numout,*) '~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namthd_sal:'
         WRITE(numout,*) '      switch for salinity                                     nn_icesal  = ', nn_icesal
         WRITE(numout,*) '      bulk salinity value if nn_icesal = 1                    rn_icesal  = ', rn_icesal
         WRITE(numout,*) '      restoring salinity for gravity drainage                 rn_sal_gd  = ', rn_sal_gd
         WRITE(numout,*) '      restoring time for for gravity drainage                 rn_time_gd = ', rn_time_gd
         WRITE(numout,*) '      restoring salinity for flushing                         rn_sal_fl  = ', rn_sal_fl
         WRITE(numout,*) '      restoring time for flushing                             rn_time_fl = ', rn_time_fl
         WRITE(numout,*) '      Maximum tolerated ice salinity                          rn_simax   = ', rn_simax
         WRITE(numout,*) '      Minimum tolerated ice salinity                          rn_simin   = ', rn_simin
      ENDIF
      !
   END SUBROUTINE ice_thd_sal_init


   !!======================================================================
END MODULE icethd_sal
