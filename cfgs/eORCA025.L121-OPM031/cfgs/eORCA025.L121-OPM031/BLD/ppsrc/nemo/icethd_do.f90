










MODULE icethd_do
   !!======================================================================
   !!                       ***  MODULE icethd_do   ***
   !!   sea-ice: sea ice growth in the leads (open water)  
   !!======================================================================
   !! History :       !  2005-12  (M. Vancoppenolle) Original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_do        : ice growth in open water (=lateral accretion of ice)
   !!   ice_thd_do_init   : initialization
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE sbc_oce , ONLY : sss_m
   USE sbc_ice , ONLY : utau_ice, vtau_ice
   USE ice1D          ! sea-ice: thermodynamics variables
   USE ice            ! sea-ice: variables
   USE icetab         ! sea-ice: 2D <==> 1D
   USE icectl         ! sea-ice: conservation
   USE icethd_ent     ! sea-ice: thermodynamics, enthalpy
   USE icevar         ! sea-ice: operations
   USE icethd_sal     ! sea-ice: salinity profiles
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE lbclnk         ! lateral boundary conditions (or mpp links)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_do        ! called by ice_thd
   PUBLIC   ice_thd_do_init   ! called by ice_stp

   !                          !!** namelist (namthd_do) **
   REAL(wp) ::   rn_hinew      ! thickness for new ice formation (m)
   LOGICAL  ::   ln_frazil     ! use of frazil ice collection as function of wind (T) or not (F)
   REAL(wp) ::   rn_maxfraz    ! maximum portion of frazil ice collecting at the ice bottom
   REAL(wp) ::   rn_vfraz      ! threshold drift speed for collection of bottom frazil ice
   REAL(wp) ::   rn_Cfraz      ! squeezing coefficient for collection of bottom frazil ice

   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icethd_do.F90 13589 2020-10-14 13:35:49Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_do
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE ice_thd_do  ***
      !!  
      !! ** Purpose : Computation of the evolution of the ice thickness and 
      !!              concentration as a function of the heat balance in the leads
      !!       
      !! ** Method  : Ice is formed in the open water when ocean looses heat
      !!              (heat budget of open water is negative) following
      !!
      !!       (dA/dt)acc = F[ (1-A)/(1-a) ] * [ Bl / (Li*h0) ]
      !!          where - h0 is the thickness of ice created in the lead
      !!                - a is a minimum fraction for leads
      !!                - F is a monotonic non-increasing function defined as:
      !!                  F(X)=( 1 - X**exld )**(1.0/exld)
      !!                - exld is the exponent closure rate (=2 default val.)
      !! 
      !! ** Action : - Adjustment of snow and ice thicknesses and heat
      !!                content in brine pockets
      !!             - Updating ice internal temperature
      !!             - Computation of variation of ice volume and mass
      !!             - Computation of a_i after lateral accretion and 
      !!               update h_s_1d, h_i_1d      
      !!------------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      INTEGER  ::   iter             !   -       -
      REAL(wp) ::   ztmelts, zfrazb, zweight, zde                               ! local scalars
      REAL(wp) ::   zgamafr, zvfrx, zvgx, ztaux, ztwogp, zf                     !   -      -
      REAL(wp) ::   ztenagm, zvfry, zvgy, ztauy, zvrel2, zfp, zsqcd , zhicrit   !   -      -
      !
      REAL(wp) ::   zQm          ! enthalpy exchanged with the ocean (J/m2, >0 towards ocean)
      REAL(wp) ::   zEi          ! sea ice specific enthalpy (J/kg)
      REAL(wp) ::   zEw          ! seawater specific enthalpy (J/kg)
      REAL(wp) ::   zfmdt        ! mass flux x time step (kg/m2, >0 towards ocean)
      !
      REAL(wp) ::   zv_newfra
      !
      INTEGER , DIMENSION(jpij) ::   jcat        ! indexes of categories where new ice grows
      REAL(wp), DIMENSION(jpij) ::   zswinew     ! switch for new ice or not
      !
      REAL(wp), DIMENSION(jpij) ::   zv_newice   ! volume of accreted ice
      REAL(wp), DIMENSION(jpij) ::   za_newice   ! fractional area of accreted ice
      REAL(wp), DIMENSION(jpij) ::   zh_newice   ! thickness of accreted ice
      REAL(wp), DIMENSION(jpij) ::   ze_newice   ! heat content of accreted ice
      REAL(wp), DIMENSION(jpij) ::   zs_newice   ! salinity of accreted ice
      REAL(wp), DIMENSION(jpij) ::   zo_newice   ! age of accreted ice
      REAL(wp), DIMENSION(jpij) ::   zdv_res     ! residual volume in case of excessive heat budget
      REAL(wp), DIMENSION(jpij) ::   zda_res     ! residual area in case of excessive heat budget
      REAL(wp), DIMENSION(jpij) ::   zv_frazb    ! accretion of frazil ice at the ice bottom
      REAL(wp), DIMENSION(jpij) ::   zvrel_1d    ! relative ice / frazil velocity (1D vector)
      !
      REAL(wp), DIMENSION(jpij,jpl) ::   zv_b    ! old volume of ice in category jl
      REAL(wp), DIMENSION(jpij,jpl) ::   za_b    ! old area of ice in category jl
      !
      REAL(wp), DIMENSION(jpij,nlay_i,jpl) ::   ze_i_2d !: 1-D version of e_i
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zvrel    ! relative ice / frazil velocity
      !
      REAL(wp) :: zcai = 1.4e-3_wp               ! ice-air drag (clem: should be dependent on coupling/forcing used)
      !!-----------------------------------------------------------------------!

      IF( ln_icediachk )   CALL ice_cons_hsm( 0, 'icethd_do', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft )
      IF( ln_icediachk )   CALL ice_cons2D  ( 0, 'icethd_do',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft )

      at_i(:,:) = SUM( a_i, dim=3 )
      !------------------------------------------------------------------------------!
      ! 1) Collection thickness of ice formed in leads and polynyas
      !------------------------------------------------------------------------------!    
      ! ht_i_new is the thickness of new ice formed in open water
      ! ht_i_new can be either prescribed (ln_frazil=F) or computed (ln_frazil=T)
      ! Frazil ice forms in open water, is transported by wind
      ! accumulates at the edge of the consolidated ice edge
      ! where it forms aggregates of a specific thickness called
      ! collection thickness.

      zvrel(:,:) = 0._wp

      ! Default new ice thickness
      WHERE( qlead(:,:) < 0._wp ) ! cooling
         ht_i_new(:,:) = rn_hinew
      ELSEWHERE
         ht_i_new(:,:) = 0._wp
      END WHERE

      IF( ln_frazil ) THEN
         !
         ht_i_new(:,:) = 0._wp
         !
         ! Physical constants
         zhicrit = 0.04                                          ! frazil ice thickness
         ztwogp  = 2. * rau0 / ( grav * 0.3 * ( rau0 - rhoi ) )  ! reduced grav
         zsqcd   = 1.0 / SQRT( 1.3 * zcai )                      ! 1/SQRT(airdensity*drag)
         zgamafr = 0.03
         !
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               IF ( qlead(ji,jj) < 0._wp ) THEN ! cooling
                  ! -- Wind stress -- !
                  ztaux         = ( utau_ice(ji-1,jj  ) * umask(ji-1,jj  ,1)   &
                     &          +   utau_ice(ji  ,jj  ) * umask(ji  ,jj  ,1) ) * 0.5_wp
                  ztauy         = ( vtau_ice(ji  ,jj-1) * vmask(ji  ,jj-1,1)   &
                     &          +   vtau_ice(ji  ,jj  ) * vmask(ji  ,jj  ,1) ) * 0.5_wp
                  ! Square root of wind stress
                  ztenagm       =  SQRT( SQRT( ztaux * ztaux + ztauy * ztauy ) )

                  ! -- Frazil ice velocity -- !
                  rswitch = MAX( 0._wp, SIGN( 1._wp , ztenagm - epsi10 ) )
                  zvfrx   = rswitch * zgamafr * zsqcd * ztaux / MAX( ztenagm, epsi10 )
                  zvfry   = rswitch * zgamafr * zsqcd * ztauy / MAX( ztenagm, epsi10 )

                  ! -- Pack ice velocity -- !
                  zvgx    = ( u_ice(ji-1,jj  ) * umask(ji-1,jj  ,1)  + u_ice(ji,jj) * umask(ji,jj,1) ) * 0.5_wp
                  zvgy    = ( v_ice(ji  ,jj-1) * vmask(ji  ,jj-1,1)  + v_ice(ji,jj) * vmask(ji,jj,1) ) * 0.5_wp

                  ! -- Relative frazil/pack ice velocity -- !
                  rswitch      = MAX( 0._wp, SIGN( 1._wp , at_i(ji,jj) - epsi10 ) )
                  zvrel2       = MAX(  ( zvfrx - zvgx ) * ( zvfrx - zvgx )   &
                     &               + ( zvfry - zvgy ) * ( zvfry - zvgy ) , 0.15 * 0.15 ) * rswitch
                  zvrel(ji,jj) = SQRT( zvrel2 )

                  ! -- new ice thickness (iterative loop) -- !
                  ht_i_new(ji,jj) = zhicrit +   ( zhicrit + 0.1 )    &
                     &                   / ( ( zhicrit + 0.1 ) * ( zhicrit + 0.1 ) -  zhicrit * zhicrit ) * ztwogp * zvrel2

                  iter = 1
                  DO WHILE ( iter < 20 ) 
                     zf  = ( ht_i_new(ji,jj) - zhicrit ) * ( ht_i_new(ji,jj) * ht_i_new(ji,jj) - zhicrit * zhicrit ) -   &
                        &    ht_i_new(ji,jj) * zhicrit * ztwogp * zvrel2
                     zfp = ( ht_i_new(ji,jj) - zhicrit ) * ( 3.0 * ht_i_new(ji,jj) + zhicrit ) - zhicrit * ztwogp * zvrel2

                     ht_i_new(ji,jj) = ht_i_new(ji,jj) - zf / MAX( zfp, epsi20 )
                     iter = iter + 1
                  END DO
                  !
                  ! bound ht_i_new (though I don't see why it should be necessary)
                  ht_i_new(ji,jj) = MAX( 0.01_wp, MIN( ht_i_new(ji,jj), hi_max(jpl) ) )
                  !
               ENDIF
               !
            END DO 
         END DO 
         ! 
         CALL lbc_lnk_multi( 'icethd_do', zvrel, 'T', 1., ht_i_new, 'T', 1.  )

      ENDIF

      !------------------------------------------------------------------------------!
      ! 2) Compute thickness, salinity, enthalpy, age, area and volume of new ice
      !------------------------------------------------------------------------------!
      ! it occurs if cooling

      ! Identify grid points where new ice forms
      npti = 0   ;   nptidx(:) = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( qlead(ji,jj) < 0._wp ) THEN
               npti = npti + 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
         END DO
      END DO

      ! Move from 2-D to 1-D vectors
      IF ( npti > 0 ) THEN

         CALL tab_2d_1d( npti, nptidx(1:npti), at_i_1d(1:npti)      , at_i        )
         CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d (1:npti,1:jpl), a_i (:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), v_i_2d (1:npti,1:jpl), v_i (:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), sv_i_2d(1:npti,1:jpl), sv_i(:,:,:) )
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               CALL tab_2d_1d( npti, nptidx(1:npti), ze_i_2d(1:npti,jk,jl), e_i(:,:,jk,jl) )
            END DO
         END DO
         CALL tab_2d_1d( npti, nptidx(1:npti), qlead_1d  (1:npti) , qlead      )
         CALL tab_2d_1d( npti, nptidx(1:npti), t_bo_1d   (1:npti) , t_bo       )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_opw_1d(1:npti) , sfx_opw    )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_opw_1d(1:npti) , wfx_opw    )
         CALL tab_2d_1d( npti, nptidx(1:npti), zh_newice (1:npti) , ht_i_new   )
         CALL tab_2d_1d( npti, nptidx(1:npti), zvrel_1d  (1:npti) , zvrel      )

         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_thd_1d(1:npti) , hfx_thd    )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_opw_1d(1:npti) , hfx_opw    )
         CALL tab_2d_1d( npti, nptidx(1:npti), rn_amax_1d(1:npti) , rn_amax_2d )
         CALL tab_2d_1d( npti, nptidx(1:npti), sss_1d    (1:npti) , sss_m      )

         ! Convert units for ice internal energy
         DO jl = 1, jpl
            DO jk = 1, nlay_i               
               WHERE( v_i_2d(1:npti,jl) > 0._wp )
                  ze_i_2d(1:npti,jk,jl) = ze_i_2d(1:npti,jk,jl) / v_i_2d(1:npti,jl) * REAL( nlay_i )
               ELSEWHERE
                  ze_i_2d(1:npti,jk,jl) = 0._wp
               END WHERE
            END DO
         END DO

         ! Keep old ice areas and volume in memory
         zv_b(1:npti,:) = v_i_2d(1:npti,:) 
         za_b(1:npti,:) = a_i_2d(1:npti,:)

         ! --- Salinity of new ice --- ! 
         SELECT CASE ( nn_icesal )
         CASE ( 1 )                    ! Sice = constant 
            zs_newice(1:npti) = rn_icesal
         CASE ( 2 )                    ! Sice = F(z,t) [Vancoppenolle et al (2005)]
            DO ji = 1, npti
               zs_newice(ji) = MIN(  4.606 + 0.91 / zh_newice(ji) , rn_simax , 0.5 * sss_1d(ji) )
            END DO
         CASE ( 3 )                    ! Sice = F(z) [multiyear ice]
            zs_newice(1:npti) =   2.3
         END SELECT

         ! --- Heat content of new ice --- !
         ! We assume that new ice is formed at the seawater freezing point
         DO ji = 1, npti
            ztmelts       = - rTmlt * zs_newice(ji)                  ! Melting point (C)
            ze_newice(ji) =   rhoi * (  rcpi  * ( ztmelts - ( t_bo_1d(ji) - rt0 ) )                     &
               &                      + rLfus * ( 1.0 - ztmelts / MIN( t_bo_1d(ji) - rt0, -epsi10 ) )   &
               &                      - rcp   *         ztmelts )
         END DO

         ! --- Age of new ice --- !
         zo_newice(1:npti) = 0._wp

         ! --- Volume of new ice --- !
         DO ji = 1, npti

            zEi           = - ze_newice(ji) * r1_rhoi              ! specific enthalpy of forming ice [J/kg]

            zEw           = rcp * ( t_bo_1d(ji) - rt0 )            ! specific enthalpy of seawater at t_bo_1d [J/kg]
                                                                   ! clem: we suppose we are already at the freezing point (condition qlead<0 is satisfyied) 
                                                                   
            zdE           = zEi - zEw                              ! specific enthalpy difference [J/kg]
                                              
            zfmdt         = - qlead_1d(ji) / zdE                   ! Fm.dt [kg/m2] (<0) 
                                                                   ! clem: we use qlead instead of zqld (icethd) because we suppose we are at the freezing point   
            zv_newice(ji) = - zfmdt * r1_rhoi

            zQm           = zfmdt * zEw                            ! heat to the ocean >0 associated with mass flux  

            ! Contribution to heat flux to the ocean [W.m-2], >0  
            hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * zEw * r1_rdtice
            ! Total heat flux used in this process [W.m-2]  
            hfx_opw_1d(ji) = hfx_opw_1d(ji) - zfmdt * zdE * r1_rdtice
            ! mass flux
            wfx_opw_1d(ji) = wfx_opw_1d(ji) - zv_newice(ji) * rhoi * r1_rdtice
            ! salt flux
            sfx_opw_1d(ji) = sfx_opw_1d(ji) - zv_newice(ji) * rhoi * zs_newice(ji) * r1_rdtice
         END DO
         
         zv_frazb(1:npti) = 0._wp
         IF( ln_frazil ) THEN
            ! A fraction zfrazb of frazil ice is accreted at the ice bottom
            DO ji = 1, npti
               rswitch       = 1._wp - MAX( 0._wp, SIGN( 1._wp , - at_i_1d(ji) ) )
               zfrazb        = rswitch * ( TANH( rn_Cfraz * ( zvrel_1d(ji) - rn_vfraz ) ) + 1.0 ) * 0.5 * rn_maxfraz
               zv_frazb(ji)  =         zfrazb   * zv_newice(ji)
               zv_newice(ji) = ( 1.0 - zfrazb ) * zv_newice(ji)
            END DO
         END IF
         
         ! --- Area of new ice --- !
         DO ji = 1, npti
            za_newice(ji) = zv_newice(ji) / zh_newice(ji)
         END DO

         !------------------------------------------------------------------------------!
         ! 3) Redistribute new ice area and volume into ice categories                  !
         !------------------------------------------------------------------------------!

         ! --- lateral ice growth --- !
         ! If lateral ice growth gives an ice concentration > amax, then
         ! we keep the excessive volume in memory and attribute it later to bottom accretion
         DO ji = 1, npti
            IF ( za_newice(ji) >  MAX( 0._wp, rn_amax_1d(ji) - at_i_1d(ji) ) ) THEN ! max is for roundoff error
               zda_res(ji)   = za_newice(ji) - MAX( 0._wp, rn_amax_1d(ji) - at_i_1d(ji) )
               zdv_res(ji)   = zda_res  (ji) * zh_newice(ji) 
               za_newice(ji) = MAX( 0._wp, za_newice(ji) - zda_res  (ji) )
               zv_newice(ji) = MAX( 0._wp, zv_newice(ji) - zdv_res  (ji) )
            ELSE
               zda_res(ji) = 0._wp
               zdv_res(ji) = 0._wp
            ENDIF
         END DO

         ! find which category to fill
         DO jl = 1, jpl
            DO ji = 1, npti
               IF( zh_newice(ji) > hi_max(jl-1) .AND. zh_newice(ji) <= hi_max(jl) ) THEN
                  a_i_2d(ji,jl) = a_i_2d(ji,jl) + za_newice(ji)
                  v_i_2d(ji,jl) = v_i_2d(ji,jl) + zv_newice(ji)
                  jcat(ji) = jl
               ENDIF
            END DO
         END DO
         at_i_1d(1:npti) = SUM( a_i_2d(1:npti,:), dim=2 )

         ! Heat content
         DO ji = 1, npti
            jl = jcat(ji)                                                    ! categroy in which new ice is put
            zswinew  (ji) = MAX( 0._wp , SIGN( 1._wp , - za_b(ji,jl) ) )   ! 0 if old ice
         END DO

         DO jk = 1, nlay_i
            DO ji = 1, npti
               jl = jcat(ji)
               rswitch = MAX( 0._wp, SIGN( 1._wp , v_i_2d(ji,jl) - epsi20 ) )
               ze_i_2d(ji,jk,jl) = zswinew(ji)   *   ze_newice(ji) +                                                    &
                  &        ( 1.0 - zswinew(ji) ) * ( ze_newice(ji) * zv_newice(ji) + ze_i_2d(ji,jk,jl) * zv_b(ji,jl) )  &
                  &        * rswitch / MAX( v_i_2d(ji,jl), epsi20 )
            END DO
         END DO

         ! --- bottom ice growth + ice enthalpy remapping --- !
         DO jl = 1, jpl

            ! for remapping
            h_i_old (1:npti,0:nlay_i+1) = 0._wp
            eh_i_old(1:npti,0:nlay_i+1) = 0._wp
            DO jk = 1, nlay_i
               DO ji = 1, npti
                  h_i_old (ji,jk) = v_i_2d(ji,jl) * r1_nlay_i
                  eh_i_old(ji,jk) = ze_i_2d(ji,jk,jl) * h_i_old(ji,jk)
               END DO
            END DO

            ! new volumes including lateral/bottom accretion + residual
            DO ji = 1, npti
               rswitch        = MAX( 0._wp, SIGN( 1._wp , at_i_1d(ji) - epsi20 ) )
               zv_newfra     = rswitch * ( zdv_res(ji) + zv_frazb(ji) ) * a_i_2d(ji,jl) / MAX( at_i_1d(ji) , epsi20 )
               a_i_2d(ji,jl) = rswitch * a_i_2d(ji,jl)               
               v_i_2d(ji,jl) = v_i_2d(ji,jl) + zv_newfra
               ! for remapping
               h_i_old (ji,nlay_i+1) = zv_newfra
               eh_i_old(ji,nlay_i+1) = ze_newice(ji) * zv_newfra
            END DO
            ! --- Ice enthalpy remapping --- !
            CALL ice_thd_ent( ze_i_2d(1:npti,:,jl) ) 
         END DO

         ! --- Update salinity --- !
         DO jl = 1, jpl
            DO ji = 1, npti
               sv_i_2d(ji,jl) = sv_i_2d(ji,jl) + zs_newice(ji) * ( v_i_2d(ji,jl) - zv_b(ji,jl) )
            END DO
         END DO

         ! Change units for e_i
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               ze_i_2d(1:npti,jk,jl) = ze_i_2d(1:npti,jk,jl) * v_i_2d(1:npti,jl) * r1_nlay_i 
            END DO
         END DO

         ! Move 2D vectors to 1D vectors 
         CALL tab_2d_3d( npti, nptidx(1:npti), a_i_2d (1:npti,1:jpl), a_i (:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), v_i_2d (1:npti,1:jpl), v_i (:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), sv_i_2d(1:npti,1:jpl), sv_i(:,:,:) )
          DO jl = 1, jpl
            DO jk = 1, nlay_i
               CALL tab_1d_2d( npti, nptidx(1:npti), ze_i_2d(1:npti,jk,jl), e_i(:,:,jk,jl) )
            END DO
         END DO
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_opw_1d(1:npti), sfx_opw )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_opw_1d(1:npti), wfx_opw )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_thd_1d(1:npti), hfx_thd )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_opw_1d(1:npti), hfx_opw )
         !
      ENDIF ! npti > 0
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'icethd_do', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'icethd_do',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      !
   END SUBROUTINE ice_thd_do


   SUBROUTINE ice_thd_do_init
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE ice_thd_do_init *** 
      !!                 
      !! ** Purpose :   Physical constants and parameters associated with
      !!                ice growth in the leads
      !!
      !! ** Method  :   Read the namthd_do namelist and check the parameters
      !!                called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namthd_do
      !!-------------------------------------------------------------------
      INTEGER  ::   ios   ! Local integer 
      !!
      NAMELIST/namthd_do/ rn_hinew, ln_frazil, rn_maxfraz, rn_vfraz, rn_Cfraz
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namthd_do in reference namelist : Ice thermodynamics
      READ  ( numnam_ice_ref, namthd_do, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namthd_do in reference namelist' )
      REWIND( numnam_ice_cfg )              ! Namelist namthd_do in configuration namelist : Ice thermodynamics
      READ  ( numnam_ice_cfg, namthd_do, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namthd_do in configuration namelist' )
      IF(lwm) WRITE( numoni, namthd_do )
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_thd_do_init: Ice growth in open water'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namthd_do:'
         WRITE(numout,*) '      ice thickness for lateral accretion                       rn_hinew   = ', rn_hinew
         WRITE(numout,*) '      Frazil ice thickness as a function of wind or not         ln_frazil  = ', ln_frazil
         WRITE(numout,*) '      Maximum proportion of frazil ice collecting at bottom     rn_maxfraz = ', rn_maxfraz
         WRITE(numout,*) '      Threshold relative drift speed for collection of frazil   rn_vfraz   = ', rn_vfraz
         WRITE(numout,*) '      Squeezing coefficient for collection of frazil            rn_Cfraz   = ', rn_Cfraz
      ENDIF
      !
      IF ( rn_hinew < rn_himin )   CALL ctl_stop( 'ice_thd_do_init : rn_hinew should be >= rn_himin' )
      !
   END SUBROUTINE ice_thd_do_init
   

   !!======================================================================
END MODULE icethd_do
