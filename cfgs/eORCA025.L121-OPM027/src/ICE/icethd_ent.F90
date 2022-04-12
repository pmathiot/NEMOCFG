MODULE icethd_ent
   !!======================================================================
   !!                       ***  MODULE icethd_ent   ***
   !!   sea-ice: redistribution of enthalpy in the ice on the new vertical grid
   !!                       after vertical growth/melt
   !!======================================================================
   !! History :       !  2003-05  (M. Vancoppenolle) Original code in 1D
   !!                 !  2005-07  (M. Vancoppenolle) 3D version 
   !!            3.6  !  2014-05  (C. Rousset)       New version
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_ent   : ice redistribution of enthalpy
   !!----------------------------------------------------------------------
   USE dom_oce        ! domain variables
   USE domain         !
   USE phycst         ! physical constants
   USE ice            ! sea-ice: variables
   USE ice1D          ! sea-ice: thermodynamics variables
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_ent         ! called by icethd and icethd_do

   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icethd_ent.F90 13284 2020-07-09 15:12:23Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
 
   SUBROUTINE ice_thd_ent( qnew )
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE ice_thd_ent  ***
      !!
      !! ** Purpose :
      !!           This routine computes new vertical grids in the ice, 
      !!           and consistently redistributes temperatures. 
      !!           Redistribution is made so as to ensure to energy conservation
      !!
      !!
      !! ** Method  : linear conservative remapping
      !!           
      !! ** Steps : 1) cumulative integrals of old enthalpies/thicknesses
      !!            2) linear remapping on the new layers
      !!
      !! ------------ cum0(0)                        ------------- cum1(0)
      !!                                    NEW      -------------
      !! ------------ cum0(1)               ==>      -------------
      !!     ...                                     -------------
      !! ------------                                -------------
      !! ------------ cum0(nlay_i+2)                 ------------- cum1(nlay_i)
      !!
      !!
      !! References : Bitz & Lipscomb, JGR 99; Vancoppenolle et al., GRL, 2005
      !!-------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   qnew             ! new enthlapies (J.m-3, remapped)
      !
      INTEGER  :: ji         !  dummy loop indices
      INTEGER  :: jk0, jk1   !  old/new layer indices
      !
      REAL(wp), DIMENSION(jpij,0:nlay_i+2) ::   zeh_cum0, zh_cum0   ! old cumulative enthlapies and layers interfaces
      REAL(wp), DIMENSION(jpij,0:nlay_i)   ::   zeh_cum1, zh_cum1   ! new cumulative enthlapies and layers interfaces
      REAL(wp), DIMENSION(jpij)            ::   zhnew               ! new layers thicknesses
      !!-------------------------------------------------------------------

      !--------------------------------------------------------------------------
      !  1) Cumulative integral of old enthalpy * thickness and layers interfaces
      !--------------------------------------------------------------------------
      zeh_cum0(1:npti,0) = 0._wp 
      zh_cum0 (1:npti,0) = 0._wp
      DO jk0 = 1, nlay_i+2
         DO ji = 1, npti
            zeh_cum0(ji,jk0) = zeh_cum0(ji,jk0-1) + eh_i_old(ji,jk0-1)
            zh_cum0 (ji,jk0) = zh_cum0 (ji,jk0-1) + h_i_old (ji,jk0-1)
         END DO
      END DO

      !------------------------------------
      !  2) Interpolation on the new layers
      !------------------------------------
      ! new layer thickesses
      DO ji = 1, npti
         zhnew(ji) = SUM( h_i_old(ji,0:nlay_i+1) ) * r1_nlay_i  
      END DO

      ! new layers interfaces
      zh_cum1(1:npti,0) = 0._wp
      DO jk1 = 1, nlay_i
         DO ji = 1, npti
            zh_cum1(ji,jk1) = zh_cum1(ji,jk1-1) + zhnew(ji)
         END DO
      END DO

      zeh_cum1(1:npti,0:nlay_i) = 0._wp 
      ! new cumulative q*h => linear interpolation
      DO jk0 = 1, nlay_i+2
         DO jk1 = 1, nlay_i-1
            DO ji = 1, npti
               IF( zh_cum1(ji,jk1) <= zh_cum0(ji,jk0) .AND. zh_cum1(ji,jk1) > zh_cum0(ji,jk0-1) ) THEN
                  zeh_cum1(ji,jk1) = ( zeh_cum0(ji,jk0-1) * ( zh_cum0(ji,jk0) - zh_cum1(ji,jk1  ) ) +  &
                     &                 zeh_cum0(ji,jk0  ) * ( zh_cum1(ji,jk1) - zh_cum0(ji,jk0-1) ) )  &
                     &             / ( zh_cum0(ji,jk0) - zh_cum0(ji,jk0-1) )
               ENDIF
            END DO
         END DO
      END DO
      ! to ensure that total heat content is strictly conserved, set:
      zeh_cum1(1:npti,nlay_i) = zeh_cum0(1:npti,nlay_i+2) 

      ! new enthalpies
      DO jk1 = 1, nlay_i
         DO ji = 1, npti
            rswitch      = MAX( 0._wp , SIGN( 1._wp , zhnew(ji) - epsi20 ) ) 
            qnew(ji,jk1) = rswitch * ( zeh_cum1(ji,jk1) - zeh_cum1(ji,jk1-1) ) / MAX( zhnew(ji), epsi20 )
         END DO
      END DO

      ! --- diag error on heat remapping --- !
      ! comment: if input h_i_old and eh_i_old are already multiplied by a_i (as in icethd_do), 
      ! then we should not (* a_i) again but not important since this is just to check that remap error is ~0
      !DO ji = 1, npti
      !   hfx_err_rem_1d(ji) = hfx_err_rem_1d(ji) + a_i_1d(ji) * r1_rdtice *  &
      !      &               ( SUM( qnew(ji,1:nlay_i) ) * zhnew(ji) - SUM( eh_i_old(ji,0:nlay_i+1) ) ) 
      !END DO
      
   END SUBROUTINE ice_thd_ent

#else
   !!----------------------------------------------------------------------
   !!   Default option                                NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd_ent
