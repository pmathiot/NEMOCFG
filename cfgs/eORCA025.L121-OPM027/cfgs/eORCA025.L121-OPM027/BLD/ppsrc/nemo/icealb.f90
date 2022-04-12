










MODULE icealb
   !!======================================================================
   !!                       ***  MODULE  icealb  ***
   !! Atmospheric forcing:  Albedo over sea ice
   !!=====================================================================
   !! History :  4.0  !  2017-07  (C. Rousset)       Split ice and ocean albedos
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_alb        : albedo for ice (clear and overcast skies)
   !!   ice_alb_init   : initialisation of albedo computation
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE dom_oce        ! domain: ocean
   USE ice, ONLY: jpl ! sea-ice: number of categories
   USE icevar         ! sea-ice: operations
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_alb_init   ! called in icestp
   PUBLIC   ice_alb        ! called in icesbc.F90 and iceupdate.F90

   REAL(wp), PUBLIC, PARAMETER ::   rn_alb_oce = 0.066   !: ocean or lead albedo (Pegau and Paulson, Ann. Glac. 2001)
   !
   !                             !!* albedo namelist (namalb)
   REAL(wp) ::   rn_alb_sdry      ! dry snow albedo
   REAL(wp) ::   rn_alb_smlt      ! melting snow albedo
   REAL(wp) ::   rn_alb_idry      ! dry ice albedo
   REAL(wp) ::   rn_alb_imlt      ! bare puddled ice albedo
   REAL(wp) ::   rn_alb_dpnd      ! ponded ice albedo

   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icealb.F90 13284 2020-07-09 15:12:23Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_alb( pt_su, ph_ice, ph_snw, ld_pnd_alb, pafrac_pnd, ph_pnd, pcloud_fra, palb_ice )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE ice_alb  ***
      !!          
      !! ** Purpose :   Computation of the albedo of the snow/ice system 
      !!       
      !! ** Method  :   The scheme is "home made" (for cloudy skies) and based on Brandt et al. (J. Climate 2005)
      !!                                                                      and Grenfell & Perovich (JGR 2004)
      !!                  1) Albedo dependency on ice thickness follows the findings from Brandt et al (2005)
      !!                     which are an update of Allison et al. (JGR 1993) ; Brandt et al. 1999
      !!                     0-5cm  : linear function of ice thickness
      !!                     5-150cm: log    function of ice thickness
      !!                     > 150cm: constant
      !!                  2) Albedo dependency on snow thickness follows the findings from Grenfell & Perovich (2004)
      !!                     i.e. it increases as -EXP(-snw_thick/0.02) during freezing and -EXP(-snw_thick/0.03) during melting
      !!                  3) Albedo dependency on clouds is speculated from measurements of Grenfell and Perovich (2004)
      !!                     i.e. cloudy-clear albedo depend on cloudy albedo following a 2d order polynomial law
      !!                  4) The needed 4 parameters are: dry and melting snow, freezing ice and bare puddled ice
      !!
      !!                     compilation of values from literature (reference overcast sky values)
      !!                        rn_alb_sdry = 0.85      ! dry snow
      !!                        rn_alb_smlt = 0.75      ! melting snow
      !!                        rn_alb_idry = 0.60      ! bare frozen ice
      !!                        rn_alb_imlt = 0.50      ! bare puddled ice albedo
      !!                        rn_alb_dpnd = 0.36      ! ponded-ice overcast albedo (Lecomte et al, 2015)
      !!                                                ! early melt pnds 0.27, late melt ponds 0.14 Grenfell & Perovich
      !!                     Perovich et al 2002 (Sheba) => the only dataset for which all types of ice/snow were retrieved
      !!                        rn_alb_sdry = 0.85      ! dry snow
      !!                        rn_alb_smlt = 0.72      ! melting snow
      !!                        rn_alb_idry = 0.65      ! bare frozen ice
      !!                     Brandt et al 2005 (East Antarctica)
      !!                        rn_alb_sdry = 0.87      ! dry snow
      !!                        rn_alb_smlt = 0.82      ! melting snow
      !!                        rn_alb_idry = 0.54      ! bare frozen ice
      !!
      !! ** Note    :   The old parameterization from Shine & Henderson-Sellers (not here anymore) presented several misconstructions
      !!                  1) ice albedo when ice thick. tends to 0 is different than ocean albedo
      !!                  2) for small ice thick. covered with some snow (<3cm?), albedo is larger 
      !!                     under melting conditions than under freezing conditions
      !!                  3) the evolution of ice albedo as a function of ice thickness shows  
      !!                     3 sharp inflexion points (at 5cm, 100cm and 150cm) that look highly unrealistic
      !!
      !! References :   Shine & Henderson-Sellers 1985, JGR, 90(D1), 2243-2250.
      !!                Brandt et al. 2005, J. Climate, vol 18
      !!                Grenfell & Perovich 2004, JGR, vol 109 
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   pt_su        !  ice surface temperature (Kelvin)
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   ph_ice       !  sea-ice thickness
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   ph_snw       !  snow depth
      LOGICAL , INTENT(in   )                   ::   ld_pnd_alb   !  effect of melt ponds on albedo
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   pafrac_pnd   !  melt pond relative fraction (per unit ice area)
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   ph_pnd       !  melt pond depth
      REAL(wp), INTENT(in   ), DIMENSION(:,:)   ::   pcloud_fra   !  cloud fraction
      REAL(wp), INTENT(  out), DIMENSION(:,:,:) ::   palb_ice     !  albedo of ice
      !
      REAL(wp), DIMENSION(jpi,jpj,jpl) :: za_s_fra   ! ice fraction covered by snow
      INTEGER  ::   ji, jj, jl                ! dummy loop indices
      REAL(wp) ::   z1_c1, z1_c2,z1_c3, z1_c4 ! local scalar
      REAL(wp) ::   z1_href_pnd               ! inverse of the characteristic length scale (Lecomte et al. 2015)
      REAL(wp) ::   zalb_pnd, zafrac_pnd      ! ponded sea ice albedo & relative pound fraction
      REAL(wp) ::   zalb_ice, zafrac_ice      ! bare sea ice albedo & relative ice fraction
      REAL(wp) ::   zalb_snw, zafrac_snw      ! snow-covered sea ice albedo & relative snow fraction
      REAL(wp) ::   zalb_cs, zalb_os          ! albedo of ice under clear/overcast sky
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('icealb')
      !
      z1_href_pnd = 1. / 0.05
      z1_c1 = 1. / ( LOG(1.5) - LOG(0.05) ) 
      z1_c2 = 1. / 0.05
      z1_c3 = 1. / 0.02
      z1_c4 = 1. / 0.03
      !
      CALL ice_var_snwfra( ph_snw, za_s_fra )   ! calculate ice fraction covered by snow
      !
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               !
               !---------------------------------------------!
               !--- Specific snow, ice and pond fractions ---!
               !---------------------------------------------!               
               zafrac_snw = za_s_fra(ji,jj,jl)
               IF( ld_pnd_alb ) THEN
                  zafrac_pnd = MIN( pafrac_pnd(ji,jj,jl), 1._wp - zafrac_snw ) ! make sure (a_ip_eff + a_s_fra) <= 1
               ELSE
                  zafrac_pnd = 0._wp
               ENDIF
               zafrac_ice = MAX( 0._wp, 1._wp - zafrac_pnd - zafrac_snw ) ! max for roundoff errors
               !
               !---------------!
               !--- Albedos ---!
               !---------------!               
               !                       !--- Bare ice albedo (for hi > 150cm)
               IF( ld_pnd_alb ) THEN
                  zalb_ice = rn_alb_idry
               ELSE
                  IF( ph_snw(ji,jj,jl) == 0._wp .AND. pt_su(ji,jj,jl) >= rt0 ) THEN   ;   zalb_ice = rn_alb_imlt
                  ELSE                                                                ;   zalb_ice = rn_alb_idry   ;   ENDIF
               ENDIF
               !                       !--- Bare ice albedo (for hi < 150cm)
               IF( 0.05 < ph_ice(ji,jj,jl) .AND. ph_ice(ji,jj,jl) <= 1.5 ) THEN      ! 5cm < hi < 150cm
                  zalb_ice = zalb_ice    + ( 0.18 - zalb_ice   ) * z1_c1 * ( LOG(1.5) - LOG(ph_ice(ji,jj,jl)) )
               ELSEIF( ph_ice(ji,jj,jl) <= 0.05 ) THEN                               ! 0cm < hi < 5cm
                  zalb_ice = rn_alb_oce  + ( 0.18 - rn_alb_oce ) * z1_c2 * ph_ice(ji,jj,jl)
               ENDIF
               !
               !                       !--- Snow-covered ice albedo (freezing, melting cases)
               IF( pt_su(ji,jj,jl) < rt0 ) THEN
                  zalb_snw = rn_alb_sdry - ( rn_alb_sdry - zalb_ice ) * EXP( - ph_snw(ji,jj,jl) * z1_c3 )
               ELSE
                  zalb_snw = rn_alb_smlt - ( rn_alb_smlt - zalb_ice ) * EXP( - ph_snw(ji,jj,jl) * z1_c4 )
               ENDIF
               !                       !--- Ponded ice albedo
               zalb_pnd = rn_alb_dpnd - ( rn_alb_dpnd - zalb_ice ) * EXP( - ph_pnd(ji,jj,jl) * z1_href_pnd ) 
               !
               !                       !--- Surface albedo is weighted mean of snow, ponds and bare ice contributions
               zalb_os = ( zafrac_snw * zalb_snw + zafrac_pnd * zalb_pnd + zafrac_ice * zalb_ice ) * tmask(ji,jj,1)
               !
               zalb_cs = zalb_os - ( - 0.1010 * zalb_os * zalb_os  &
                  &                  + 0.1933 * zalb_os - 0.0148 ) * tmask(ji,jj,1)
               !
               ! albedo depends on cloud fraction because of non-linear spectral effects
               palb_ice(ji,jj,jl) = ( 1._wp - pcloud_fra(ji,jj) ) * zalb_cs + pcloud_fra(ji,jj) * zalb_os

            END DO
         END DO
      END DO
      !
      !
      IF( ln_timing )   CALL timing_stop('icealb')
      !
   END SUBROUTINE ice_alb


   SUBROUTINE ice_alb_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE alb_init  ***
      !!
      !! ** Purpose :   initializations for the albedo parameters
      !!
      !! ** Method  :   Read the namelist namalb
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer output status for namelist read
      !!
      NAMELIST/namalb/ rn_alb_sdry, rn_alb_smlt, rn_alb_idry, rn_alb_imlt, rn_alb_dpnd
      !!----------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namalb in reference namelist : Albedo parameters
      READ  ( numnam_ice_ref, namalb, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namalb in reference namelist' )
      REWIND( numnam_ice_cfg )              ! Namelist namalb in configuration namelist : Albedo parameters
      READ  ( numnam_ice_cfg, namalb, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namalb in configuration namelist' )
      IF(lwm) WRITE( numoni, namalb )
      !
      IF(lwp) THEN                      ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_alb_init: set albedo parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namalb:'
         WRITE(numout,*) '      albedo of dry snow                   rn_alb_sdry = ', rn_alb_sdry
         WRITE(numout,*) '      albedo of melting snow               rn_alb_smlt = ', rn_alb_smlt
         WRITE(numout,*) '      albedo of dry ice                    rn_alb_idry = ', rn_alb_idry
         WRITE(numout,*) '      albedo of bare puddled ice           rn_alb_imlt = ', rn_alb_imlt
         WRITE(numout,*) '      albedo of ponded ice                 rn_alb_dpnd = ', rn_alb_dpnd
      ENDIF
      !
   END SUBROUTINE ice_alb_init


   !!======================================================================
END MODULE icealb
