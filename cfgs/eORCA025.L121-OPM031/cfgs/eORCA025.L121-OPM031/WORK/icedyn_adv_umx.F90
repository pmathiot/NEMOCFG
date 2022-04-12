MODULE icedyn_adv_umx
   !!==============================================================================
   !!                       ***  MODULE  icedyn_adv_umx  ***
   !! sea-ice : advection using the ULTIMATE-MACHO scheme
   !!==============================================================================
   !! History :  3.6  !  2014-11  (C. Rousset, G. Madec)  Original code
   !!            4.0  !  2018     (many people)           SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn_adv_umx   : update the tracer fields
   !!   ultimate_x(_y)    : compute a tracer value at velocity points using ULTIMATE scheme at various orders
   !!   macho             : compute the fluxes
   !!   nonosc_ice        : limit the fluxes using a non-oscillatory algorithm 
   !!----------------------------------------------------------------------
   USE phycst         ! physical constant
   USE dom_oce        ! ocean domain
   USE sbc_oce , ONLY : nn_fsbc   ! update frequency of surface boundary condition
   USE ice            ! sea-ice variables
   USE icevar         ! sea-ice: operations
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE lbclnk         ! lateral boundary conditions (or mpp links)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn_adv_umx   ! called by icedyn_adv.F90
   !
   INTEGER, PARAMETER ::   np_advS = 1         ! advection for S and T:    dVS/dt = -div(      uVS     ) => np_advS = 1
   !                                                                    or dVS/dt = -div( uA * uHS / u ) => np_advS = 2
   !                                                                    or dVS/dt = -div( uV * uS  / u ) => np_advS = 3
   INTEGER, PARAMETER ::   np_limiter = 1      ! limiter: 1 = nonosc
   !                                                      2 = superbee
   !                                                      3 = h3
   LOGICAL            ::   ll_upsxy  = .TRUE.   ! alternate directions for upstream
   LOGICAL            ::   ll_hoxy   = .TRUE.   ! alternate directions for high order
   LOGICAL            ::   ll_neg    = .TRUE.   ! if T interpolated at u/v points is negative or v_i < 1.e-6
   !                                                 then interpolate T at u/v points using the upstream scheme
   LOGICAL            ::   ll_prelim = .FALSE.  ! prelimiter from: Zalesak(1979) eq. 14 => not well defined in 2D
   !
   REAL(wp)           ::   z1_6   = 1._wp /   6._wp   ! =1/6
   REAL(wp)           ::   z1_120 = 1._wp / 120._wp   ! =1/120
   !
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) ::   imsk_small, jmsk_small
   !
   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icedyn_adv_umx.F90 13617 2020-10-16 08:07:20Z clem $
   !! Software governed by the CeCILL licence     (./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_dyn_adv_umx( kn_umx, kt, pu_ice, pv_ice, ph_i, ph_s, ph_ip,  &
      &                        pato_i, pv_i, pv_s, psv_i, poa_i, pa_i, pa_ip, pv_ip, pv_il, pe_s, pe_i )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dyn_adv_umx  ***
      !! 
      !! **  Purpose :   Compute the now trend due to total advection of 
      !!                 tracers and add it to the general trend of tracer equations
      !!                 using an "Ultimate-Macho" scheme
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74. 
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   kn_umx     ! order of the scheme (1-5=UM or 20=CEN2)
      INTEGER                     , INTENT(in   ) ::   kt         ! time step
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pu_ice     ! ice i-velocity
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pv_ice     ! ice j-velocity
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   ph_i       ! ice thickness
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   ph_s       ! snw thickness
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   ph_ip      ! ice pond thickness
      REAL(wp), DIMENSION(:,:)    , INTENT(inout) ::   pato_i     ! open water area
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_i       ! ice volume
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_s       ! snw volume
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   psv_i      ! salt content
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   poa_i      ! age content
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pa_i       ! ice concentration
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pa_ip      ! melt pond concentration
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_ip      ! melt pond volume
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_il      ! melt pond lid volume
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_s       ! snw heat content
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_i       ! ice heat content
      !
      INTEGER  ::   ji, jj, jk, jl, jt      ! dummy loop indices
      INTEGER  ::   icycle                  ! number of sub-timestep for the advection
      REAL(wp) ::   zamsk                   ! 1 if advection of concentration, 0 if advection of other tracers
      REAL(wp) ::   zdt, z1_dt, zvi_cen
      REAL(wp), DIMENSION(1)                  ::   zcflprv, zcflnow   ! for global communication
      REAL(wp), DIMENSION(jpi,jpj)            ::   zudy, zvdx, zcu_box, zcv_box
      REAL(wp), DIMENSION(jpi,jpj)            ::   zati1, zati2
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   zu_cat, zv_cat
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   zua_ho, zva_ho, zua_ups, zva_ups
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   z1_ai , z1_aip, zhvar
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   zhi_max, zhs_max, zhip_max, zs_i, zsi_max
      REAL(wp), DIMENSION(jpi,jpj,nlay_i,jpl) ::   ze_i, zei_max
      REAL(wp), DIMENSION(jpi,jpj,nlay_s,jpl) ::   ze_s, zes_max
      !
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   zuv_ho, zvv_ho, zuv_ups, zvv_ups, z1_vi, z1_vs 
      !! diagnostics
      REAL(wp), DIMENSION(jpi,jpj)            ::   zdiag_adv_mass, zdiag_adv_salt, zdiag_adv_heat      
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_dyn_adv_umx: Ultimate-Macho advection scheme'
      !
      ! --- Record max of the surrounding 9-pts (for call Hbig) --- !
      ! thickness and salinity
      WHERE( pv_i(:,:,:) >= epsi10 ) ; zs_i(:,:,:) = psv_i(:,:,:) / pv_i(:,:,:)
      ELSEWHERE                      ; zs_i(:,:,:) = 0._wp
      END WHERE
      CALL icemax3D( ph_i , zhi_max )
      CALL icemax3D( ph_s , zhs_max )
      CALL icemax3D( ph_ip, zhip_max)
      CALL icemax3D( zs_i , zsi_max )
      CALL lbc_lnk_multi( 'icedyn_adv_umx', zhi_max, 'T', 1., zhs_max, 'T', 1., zhip_max, 'T', 1., zsi_max, 'T', 1. )

      ! enthalpies
      DO jk = 1, nlay_i
         WHERE( pv_i(:,:,:) >= epsi10 ) ; ze_i(:,:,jk,:) = pe_i(:,:,jk,:) / pv_i(:,:,:)
         ELSEWHERE                      ; ze_i(:,:,jk,:) = 0._wp
         END WHERE
      END DO
      DO jk = 1, nlay_s
         WHERE( pv_s(:,:,:) >= epsi10 ) ; ze_s(:,:,jk,:) = pe_s(:,:,jk,:) / pv_s(:,:,:)
         ELSEWHERE                      ; ze_s(:,:,jk,:) = 0._wp
         END WHERE
      END DO   
      CALL icemax4D( ze_i , zei_max )
      CALL icemax4D( ze_s , zes_max )
      CALL lbc_lnk( 'icedyn_adv_umx', zei_max, 'T', 1. )
      CALL lbc_lnk( 'icedyn_adv_umx', zes_max, 'T', 1. )
      !
      ! --- If ice drift is too fast, use  subtime steps for advection (CFL test for stability) --- !
      !        Note: the advection split is applied at the next time-step in order to avoid blocking global comm.
      !              this should not affect too much the stability
      zcflnow(1) =                  MAXVAL( ABS( pu_ice(:,:) ) * rdt_ice * r1_e1u(:,:) )
      zcflnow(1) = MAX( zcflnow(1), MAXVAL( ABS( pv_ice(:,:) ) * rdt_ice * r1_e2v(:,:) ) )
      
      ! non-blocking global communication send zcflnow and receive zcflprv
      CALL mpp_delay_max( 'icedyn_adv_umx', 'cflice', zcflnow(:), zcflprv(:), kt == nitend - nn_fsbc + 1 )

      IF( zcflprv(1) > .5 ) THEN   ;   icycle = 2
      ELSE                         ;   icycle = 1
      ENDIF
      zdt = rdt_ice / REAL(icycle)
      z1_dt = 1._wp / zdt
      
      ! --- transport --- !
      zudy(:,:) = pu_ice(:,:) * e2u(:,:)
      zvdx(:,:) = pv_ice(:,:) * e1v(:,:)
      !
      ! setup transport for each ice cat 
      DO jl = 1, jpl
         zu_cat(:,:,jl) = zudy(:,:)
         zv_cat(:,:,jl) = zvdx(:,:)
      END DO
      !
      ! --- define velocity for advection: u*grad(H) --- !
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1
            IF    ( pu_ice(ji,jj) * pu_ice(ji-1,jj) <= 0._wp ) THEN   ;   zcu_box(ji,jj) = 0._wp
            ELSEIF( pu_ice(ji,jj)                   >  0._wp ) THEN   ;   zcu_box(ji,jj) = pu_ice(ji-1,jj)
            ELSE                                                      ;   zcu_box(ji,jj) = pu_ice(ji  ,jj)
            ENDIF

            IF    ( pv_ice(ji,jj) * pv_ice(ji,jj-1) <= 0._wp ) THEN   ;   zcv_box(ji,jj) = 0._wp
            ELSEIF( pv_ice(ji,jj)                   >  0._wp ) THEN   ;   zcv_box(ji,jj) = pv_ice(ji,jj-1)
            ELSE                                                      ;   zcv_box(ji,jj) = pv_ice(ji,jj  )
            ENDIF
         END DO
      END DO

      !---------------!
      !== advection ==!
      !---------------!
      DO jt = 1, icycle

         ! diagnostics
         zdiag_adv_mass(:,:) =   SUM(  pv_i(:,:,:) , dim=3 ) * rhoi + SUM(  pv_s(:,:,:) , dim=3 ) * rhos
         zdiag_adv_salt(:,:) =   SUM( psv_i(:,:,:) , dim=3 ) * rhoi
         zdiag_adv_heat(:,:) = - SUM(SUM( pe_i(:,:,1:nlay_i,:) , dim=4 ), dim=3 ) &
            &                  - SUM(SUM( pe_s(:,:,1:nlay_s,:) , dim=4 ), dim=3 )

         ! record at_i before advection (for open water)
         zati1(:,:) = SUM( pa_i(:,:,:), dim=3 )
         
         ! inverse of A and Ap
         WHERE( pa_i(:,:,:) >= epsi20 )   ;   z1_ai(:,:,:) = 1._wp / pa_i(:,:,:)
         ELSEWHERE                        ;   z1_ai(:,:,:) = 0.
         END WHERE
         WHERE( pa_ip(:,:,:) >= epsi20 )  ;   z1_aip(:,:,:) = 1._wp / pa_ip(:,:,:)
         ELSEWHERE                        ;   z1_aip(:,:,:) = 0.
         END WHERE
         !
         ! setup a mask where advection will be upstream
         IF( ll_neg ) THEN
            IF( .NOT. ALLOCATED(imsk_small) )   ALLOCATE( imsk_small(jpi,jpj,jpl) ) 
            IF( .NOT. ALLOCATED(jmsk_small) )   ALLOCATE( jmsk_small(jpi,jpj,jpl) ) 
            DO jl = 1, jpl
               DO jj = 1, jpjm1
                  DO ji = 1, jpim1
                     zvi_cen = 0.5_wp * ( pv_i(ji+1,jj,jl) + pv_i(ji,jj,jl) )
                     IF( zvi_cen < epsi06) THEN   ;   imsk_small(ji,jj,jl) = 0
                     ELSE                         ;   imsk_small(ji,jj,jl) = 1   ;   ENDIF
                     zvi_cen = 0.5_wp * ( pv_i(ji,jj+1,jl) + pv_i(ji,jj,jl) )
                     IF( zvi_cen < epsi06) THEN   ;   jmsk_small(ji,jj,jl) = 0
                     ELSE                         ;   jmsk_small(ji,jj,jl) = 1   ;   ENDIF
                  END DO
               END DO
            END DO
         ENDIF
         !
         ! ----------------------- !
         ! ==> start advection <== !
         ! ----------------------- !
         !
         !== Ice area ==!
         zamsk = 1._wp
         CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy, zvdx, zu_cat , zv_cat , zcu_box, zcv_box, &
            &                                      pa_i, pa_i, zua_ups, zva_ups, zua_ho , zva_ho )
         !
         !                             ! --------------------------------- !
         IF( np_advS == 1 ) THEN       ! -- advection form: -div( uVS ) -- !
            !                          ! --------------------------------- !
            zamsk = 0._wp
            !== Ice volume ==!
            zhvar(:,:,:) = pv_i(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_i, zua_ups, zva_ups )
            !== Snw volume ==!         
            zhvar(:,:,:) = pv_s(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_s, zua_ups, zva_ups )
            !
            zamsk = 1._wp
            !== Salt content ==!
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat, zv_cat, zcu_box, zcv_box, &
               &                                      psv_i, psv_i )
            !== Ice heat content ==!
            DO jk = 1, nlay_i
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat, zv_cat, zcu_box, zcv_box, &
                  &                                      pe_i(:,:,jk,:), pe_i(:,:,jk,:) )
            END DO
            !== Snw heat content ==!
            DO jk = 1, nlay_s
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat, zv_cat, zcu_box, zcv_box, &
                  &                                      pe_s(:,:,jk,:), pe_s(:,:,jk,:) )
            END DO
            !
            !                          ! ------------------------------------------ !
         ELSEIF( np_advS == 2 ) THEN   ! -- advection form: -div( uA * uHS / u ) -- !
            !                          ! ------------------------------------------ !
            zamsk = 0._wp
            !== Ice volume ==!
            zhvar(:,:,:) = pv_i(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_i, zua_ups, zva_ups )
            !== Snw volume ==!         
            zhvar(:,:,:) = pv_s(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_s, zua_ups, zva_ups )
            !== Salt content ==!
            zhvar(:,:,:) = psv_i(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, psv_i, zua_ups, zva_ups )
            !== Ice heat content ==!
            DO jk = 1, nlay_i
               zhvar(:,:,:) = pe_i(:,:,jk,:) * z1_ai(:,:,:)
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho, zva_ho, zcu_box, zcv_box, &
                  &                                      zhvar, pe_i(:,:,jk,:), zua_ups, zva_ups )
            END DO
            !== Snw heat content ==!
            DO jk = 1, nlay_s
               zhvar(:,:,:) = pe_s(:,:,jk,:) * z1_ai(:,:,:)
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho, zva_ho, zcu_box, zcv_box, &
                  &                                      zhvar, pe_s(:,:,jk,:), zua_ups, zva_ups )
            END DO
            !
            !                          ! ----------------------------------------- !
         ELSEIF( np_advS == 3 ) THEN   ! -- advection form: -div( uV * uS / u ) -- !
            !                          ! ----------------------------------------- !
            zamsk = 0._wp
            !
            ALLOCATE( zuv_ho (jpi,jpj,jpl), zvv_ho (jpi,jpj,jpl),  &
               &      zuv_ups(jpi,jpj,jpl), zvv_ups(jpi,jpj,jpl), z1_vi(jpi,jpj,jpl), z1_vs(jpi,jpj,jpl) )
            !
            ! inverse of Vi
            WHERE( pv_i(:,:,:) >= epsi20 )   ;   z1_vi(:,:,:) = 1._wp / pv_i(:,:,:)
            ELSEWHERE                        ;   z1_vi(:,:,:) = 0.
            END WHERE
            ! inverse of Vs
            WHERE( pv_s(:,:,:) >= epsi20 )   ;   z1_vs(:,:,:) = 1._wp / pv_s(:,:,:)
            ELSEWHERE                        ;   z1_vs(:,:,:) = 0.
            END WHERE
            !
            ! It is important to first calculate the ice fields and then the snow fields (because we use the same arrays)
            !
            !== Ice volume ==!
            zuv_ups = zua_ups
            zvv_ups = zva_ups
            zhvar(:,:,:) = pv_i(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_i, zuv_ups, zvv_ups, zuv_ho , zvv_ho )
            !== Salt content ==!
            zhvar(:,:,:) = psv_i(:,:,:) * z1_vi(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zuv_ho , zvv_ho , zcu_box, zcv_box, &
               &                                      zhvar, psv_i, zuv_ups, zvv_ups )
            !== Ice heat content ==!
            DO jk = 1, nlay_i
               zhvar(:,:,:) = pe_i(:,:,jk,:) * z1_vi(:,:,:)
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zuv_ho, zvv_ho, zcu_box, zcv_box, &
                  &                                      zhvar, pe_i(:,:,jk,:), zuv_ups, zvv_ups )
            END DO
            !== Snow volume ==!         
            zuv_ups = zua_ups
            zvv_ups = zva_ups
            zhvar(:,:,:) = pv_s(:,:,:) * z1_ai(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_s, zuv_ups, zvv_ups, zuv_ho , zvv_ho )
            !== Snw heat content ==!
            DO jk = 1, nlay_s
               zhvar(:,:,:) = pe_s(:,:,jk,:) * z1_vs(:,:,:)
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx, zuv_ho, zvv_ho, zcu_box, zcv_box, &
                  &                                      zhvar, pe_s(:,:,jk,:), zuv_ups, zvv_ups )
            END DO
            !
            DEALLOCATE( zuv_ho, zvv_ho, zuv_ups, zvv_ups, z1_vi, z1_vs )
            !
         ENDIF
         !
         !== Ice age ==!
         zamsk = 1._wp
         CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat, zv_cat, zcu_box, zcv_box, &
            &                                      poa_i, poa_i )
         !
         !== melt ponds ==!
         IF ( ln_pnd_LEV ) THEN
            ! concentration
            zamsk = 1._wp
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat , zv_cat , zcu_box, zcv_box, &
               &                                      pa_ip, pa_ip, zua_ups, zva_ups, zua_ho , zva_ho )
            ! volume
            zamsk = 0._wp
            zhvar(:,:,:) = pv_ip(:,:,:) * z1_aip(:,:,:)
            CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                      zhvar, pv_ip, zua_ups, zva_ups )
            ! lid
            IF ( ln_pnd_lids ) THEN
               zamsk = 0._wp
               zhvar(:,:,:) = pv_il(:,:,:) * z1_aip(:,:,:)
               CALL adv_umx( zamsk, kn_umx, jt, kt, zdt, zudy , zvdx , zua_ho , zva_ho , zcu_box, zcv_box, &
                  &                                      zhvar, pv_il, zua_ups, zva_ups )
            ENDIF
         ENDIF
         !
         ! --- Lateral boundary conditions --- !
         IF    ( ln_pnd_LEV .AND. ln_pnd_lids ) THEN
            CALL lbc_lnk_multi( 'icedyn_adv_umx', pa_i,'T',1._wp, pv_i,'T',1._wp, pv_s,'T',1._wp, psv_i,'T',1._wp, poa_i,'T',1._wp &
               &                                , pa_ip,'T',1._wp, pv_ip,'T',1._wp, pv_il,'T',1._wp )
         ELSEIF( ln_pnd_LEV .AND. .NOT.ln_pnd_lids ) THEN
            CALL lbc_lnk_multi( 'icedyn_adv_umx', pa_i,'T',1._wp, pv_i,'T',1._wp, pv_s,'T',1._wp, psv_i,'T',1._wp, poa_i,'T',1._wp &
               &                                , pa_ip,'T',1._wp, pv_ip,'T',1._wp )
         ELSE
            CALL lbc_lnk_multi( 'icedyn_adv_umx', pa_i,'T',1._wp, pv_i,'T',1._wp, pv_s,'T',1._wp, psv_i,'T',1._wp, poa_i,'T',1._wp )
         ENDIF
         CALL lbc_lnk( 'icedyn_adv_umx', pe_i, 'T', 1._wp )
         CALL lbc_lnk( 'icedyn_adv_umx', pe_s, 'T', 1._wp )
         !
         !== Open water area ==!
         zati2(:,:) = SUM( pa_i(:,:,:), dim=3 )
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1
               pato_i(ji,jj) = pato_i(ji,jj) - ( zati2(ji,jj) - zati1(ji,jj) ) & 
                  &                          - ( zudy(ji,jj) - zudy(ji-1,jj) + zvdx(ji,jj) - zvdx(ji,jj-1) ) * r1_e1e2t(ji,jj) * zdt
            END DO
         END DO
         CALL lbc_lnk( 'icedyn_adv_umx', pato_i, 'T', 1._wp )
         !
         ! --- diagnostics --- !
         diag_adv_mass(:,:) = diag_adv_mass(:,:) + (   SUM( pv_i(:,:,:) , dim=3 ) * rhoi + SUM( pv_s(:,:,:) , dim=3 ) * rhos &
            &                                        - zdiag_adv_mass(:,:) ) * z1_dt
         diag_adv_salt(:,:) = diag_adv_salt(:,:) + (   SUM( psv_i(:,:,:) , dim=3 ) * rhoi &
            &                                        - zdiag_adv_salt(:,:) ) * z1_dt
         diag_adv_heat(:,:) = diag_adv_heat(:,:) + ( - SUM(SUM( pe_i(:,:,1:nlay_i,:) , dim=4 ), dim=3 ) &
            &                                        - SUM(SUM( pe_s(:,:,1:nlay_s,:) , dim=4 ), dim=3 ) &
            &                                        - zdiag_adv_heat(:,:) ) * z1_dt
         !
         ! --- Ensure non-negative fields and in-bound thicknesses --- !
         ! Remove negative values (conservation is ensured)
         !    (because advected fields are not perfectly bounded and tiny negative values can occur, e.g. -1.e-20)
         CALL ice_var_zapneg( zdt, pato_i, pv_i, pv_s, psv_i, poa_i, pa_i, pa_ip, pv_ip, pv_il, pe_s, pe_i )
         !
         ! --- Make sure ice thickness is not too big --- !
         !     (because ice thickness can be too large where ice concentration is very small)
         CALL Hbig( zdt, zhi_max, zhs_max, zhip_max, zsi_max, zes_max, zei_max, &
            &            pv_i, pv_s, pa_i, pa_ip, pv_ip, psv_i, pe_s, pe_i )
         !
         ! --- Ensure snow load is not too big --- !
         CALL Hsnow( zdt, pv_i, pv_s, pa_i, pa_ip, pe_s )
         !
      END DO
      !
   END SUBROUTINE ice_dyn_adv_umx

   
   SUBROUTINE adv_umx( pamsk, kn_umx, jt, kt, pdt, pu, pv, puc, pvc, pubox, pvbox,  &
      &                                            pt, ptc, pua_ups, pva_ups, pua_ho, pva_ho )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE adv_umx  ***
      !! 
      !! **  Purpose :   Compute the now trend due to total advection of 
      !!                 tracers and add it to the general trend of tracer equations
      !!
      !! **  Method  :   - calculate upstream fluxes and upstream solution for tracers V/A(=H) etc
      !!                 - calculate tracer H at u and v points (Ultimate)
      !!                 - calculate the high order fluxes using alterning directions (Macho)
      !!                 - apply a limiter on the fluxes (nonosc_ice)
      !!                 - convert this tracer flux to a "volume" flux (uH -> uV)
      !!                 - apply a limiter a second time on the volumes fluxes (nonosc_ice)
      !!                 - calculate the high order solution for V
      !!
      !! ** Action : solve 3 equations => a) dA/dt  = -div(uA)
      !!                                  b) dV/dt  = -div(uV)  using dH/dt = -u.grad(H)
      !!                                  c) dVS/dt = -div(uVS) using either dHS/dt = -u.grad(HS) or dS/dt = -u.grad(S)
      !!
      !!             in eq. b), - fluxes uH are evaluated (with UMx) and limited with nonosc_ice. This step is necessary to get a good H.
      !!                        - then we convert this flux to a "volume" flux this way => uH * uA / u
      !!                             where uA is the flux from eq. a)
      !!                             this "volume" flux is also limited with nonosc_ice (otherwise overshoots can occur)
      !!                        - at last we estimate dV/dt = -div(uH * uA / u)
      !!
      !!             in eq. c), one can solve the equation for  S (ln_advS=T), then dVS/dt = -div(uV * uS  / u)
      !!                                                or for HS (ln_advS=F), then dVS/dt = -div(uA * uHS / u) 
      !!
      !! ** Note : - this method can lead to tiny negative V (-1.e-20) => set it to 0 while conserving mass etc.
      !!           - At the ice edge, Ultimate scheme can lead to:
      !!                              1) negative interpolated tracers at u-v points
      !!                              2) non-zero interpolated tracers at u-v points eventhough there is no ice and velocity is outward
      !!                              Solution for 1): apply an upstream scheme when it occurs. A better solution would be to degrade the order of
      !!                                               the scheme automatically by applying a mask of the ice cover inside Ultimate (not done).
      !!                              Solution for 2): we set it to 0 in this case
      !!           - Eventhough 1D tests give very good results (typically the one from Schar & Smolarkiewiecz), the 2D is less good.
      !!             Large values of H can appear for very small ice concentration, and when it does it messes the things up since we
      !!             work on H (and not V). It is partly related to the multi-category approach
      !!             Therefore, after advection we limit the thickness to the largest value of the 9-points around (only if ice
      !!             concentration is small). We also limit S and T.
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   )           ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   )           ::   kn_umx           ! order of the scheme (1-5=UM or 20=CEN2)
      INTEGER                         , INTENT(in   )           ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   )           ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   )           ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   )           ::   pu   , pv        ! 2 ice velocity components => u*e2
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   )           ::   puc  , pvc       ! 2 ice velocity components => u*e2 or u*a*e2u
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   )           ::   pubox, pvbox     ! upstream velocity
      REAL(wp), DIMENSION(:,:,:)      , INTENT(inout)           ::   pt               ! tracer field
      REAL(wp), DIMENSION(:,:,:)      , INTENT(inout)           ::   ptc              ! tracer content field
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(inout), OPTIONAL ::   pua_ups, pva_ups ! upstream u*a fluxes
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out), OPTIONAL ::   pua_ho, pva_ho   ! high order u*a fluxes
      !
      INTEGER  ::   ji, jj, jl       ! dummy loop indices  
      REAL(wp) ::   ztra             ! local scalar
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zfu_ho , zfv_ho , zpt
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zfu_ups, zfv_ups, zt_ups
      !!----------------------------------------------------------------------
      !
      ! Upstream (_ups) fluxes 
      ! -----------------------
      CALL upstream( pamsk, jt, kt, pdt, pt, pu, pv, zt_ups, zfu_ups, zfv_ups )
      
      ! High order (_ho) fluxes 
      ! -----------------------
      SELECT CASE( kn_umx )
         !
      CASE ( 20 )                          !== centered second order ==!
         !
         CALL cen2( pamsk, jt, kt, pdt, pt, pu, pv, zt_ups, zfu_ups, zfv_ups, zfu_ho, zfv_ho )
         !
      CASE ( 1:5 )                         !== 1st to 5th order ULTIMATE-MACHO scheme ==!
         !
         CALL macho( pamsk, kn_umx, jt, kt, pdt, pt, pu, pv, pubox, pvbox, zt_ups, zfu_ups, zfv_ups, zfu_ho, zfv_ho )
         !
      END SELECT
      !
      !              --ho    --ho
      ! new fluxes = u*H  *  u*a / u
      ! ----------------------------
      IF( pamsk == 0._wp ) THEN
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1
                  IF( ABS( pu(ji,jj) ) > epsi10 ) THEN
                     zfu_ho (ji,jj,jl) = zfu_ho (ji,jj,jl) * puc    (ji,jj,jl) / pu(ji,jj)
                     zfu_ups(ji,jj,jl) = zfu_ups(ji,jj,jl) * pua_ups(ji,jj,jl) / pu(ji,jj)
                  ELSE
                     zfu_ho (ji,jj,jl) = 0._wp
                     zfu_ups(ji,jj,jl) = 0._wp
                  ENDIF
                  !
               END DO
            END DO
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  IF( ABS( pv(ji,jj) ) > epsi10 ) THEN
                     zfv_ho (ji,jj,jl) = zfv_ho (ji,jj,jl) * pvc    (ji,jj,jl) / pv(ji,jj)
                     zfv_ups(ji,jj,jl) = zfv_ups(ji,jj,jl) * pva_ups(ji,jj,jl) / pv(ji,jj)
                  ELSE
                     zfv_ho (ji,jj,jl) = 0._wp  
                     zfv_ups(ji,jj,jl) = 0._wp  
                  ENDIF
               END DO
            END DO
         END DO

         ! the new "volume" fluxes must also be "flux corrected"
         ! thus we calculate the upstream solution and apply a limiter again
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1
                  ztra = - ( zfu_ups(ji,jj,jl) - zfu_ups(ji-1,jj,jl) + zfv_ups(ji,jj,jl) - zfv_ups(ji,jj-1,jl) )
                  !
                  zt_ups(ji,jj,jl) = ( ptc(ji,jj,jl) + ztra * r1_e1e2t(ji,jj) * pdt ) * tmask(ji,jj,1)
               END DO
            END DO
         END DO
         CALL lbc_lnk( 'icedyn_adv_umx', zt_ups, 'T',  1. )
         !
         IF    ( np_limiter == 1 ) THEN
            CALL nonosc_ice( 1._wp, pdt, pu, pv, ptc, zt_ups, zfu_ups, zfv_ups, zfu_ho, zfv_ho )
         ELSEIF( np_limiter == 2 .OR. np_limiter == 3 ) THEN
            CALL limiter_x( pdt, pu, ptc, zfu_ups, zfu_ho )
            CALL limiter_y( pdt, pv, ptc, zfv_ups, zfv_ho )
         ENDIF
         !
      ENDIF
      !                                   --ho    --ups
      ! in case of advection of A: output u*a and u*a
      ! -----------------------------------------------
      IF( PRESENT( pua_ho ) ) THEN
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1
                  pua_ho (ji,jj,jl) = zfu_ho (ji,jj,jl)
                  pua_ups(ji,jj,jl) = zfu_ups(ji,jj,jl)
              END DO
            END DO
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  pva_ho (ji,jj,jl) = zfv_ho (ji,jj,jl)
                  pva_ups(ji,jj,jl) = zfv_ups(ji,jj,jl)
              END DO
            END DO
         END DO
      ENDIF
      !
      ! final trend with corrected fluxes
      ! ---------------------------------
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1 
               ztra = - ( zfu_ho(ji,jj,jl) - zfu_ho(ji-1,jj,jl) + zfv_ho(ji,jj,jl) - zfv_ho(ji,jj-1,jl) )  
               !
               ptc(ji,jj,jl) = ( ptc(ji,jj,jl) + ztra * r1_e1e2t(ji,jj) * pdt ) * tmask(ji,jj,1)               
            END DO
         END DO
      END DO
      !
   END SUBROUTINE adv_umx


   SUBROUTINE upstream( pamsk, jt, kt, pdt, pt, pu, pv, pt_ups, pfu_ups, pfv_ups )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE upstream  ***
      !!     
      !! **  Purpose :   compute the upstream fluxes and upstream guess of tracer
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   ) ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt               ! tracer fields
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pu, pv           ! 2 ice velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pt_ups           ! upstream guess of tracer 
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfu_ups, pfv_ups ! upstream fluxes 
      !
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp) ::   ztra          ! local scalar
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zpt
      !!----------------------------------------------------------------------

      IF( .NOT. ll_upsxy ) THEN         !** no alternate directions **!
         !
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = 1, fs_jpim1
                  pfu_ups(ji,jj,jl) = MAX( pu(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pu(ji,jj), 0._wp ) * pt(ji+1,jj,jl)
                  pfv_ups(ji,jj,jl) = MAX( pv(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pv(ji,jj), 0._wp ) * pt(ji,jj+1,jl)
               END DO
            END DO
         END DO
         !
      ELSE                              !** alternate directions **!
         !
         IF( MOD( (kt - 1) / nn_fsbc , 2 ) ==  MOD( (jt - 1) , 2 ) ) THEN   !==  odd ice time step:  adv_x then adv_y  ==!
            !
            DO jl = 1, jpl              !-- flux in x-direction
               DO jj = 1, jpj
                  DO ji = 1, fs_jpim1
                     pfu_ups(ji,jj,jl) = MAX( pu(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pu(ji,jj), 0._wp ) * pt(ji+1,jj,jl)
                  END DO
               END DO
            END DO
            !
            DO jl = 1, jpl              !-- first guess of tracer from u-flux
               DO jj = 1, jpj
                  DO ji = fs_2, fs_jpim1
                     ztra = - ( pfu_ups(ji,jj,jl) - pfu_ups(ji-1,jj,jl) )              &
                        &   + ( pu     (ji,jj   ) - pu     (ji-1,jj   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                     !
                     zpt(ji,jj,jl) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
                  END DO
               END DO
            END DO
            !
            DO jl = 1, jpl              !-- flux in y-direction
               DO jj = 1, jpjm1
                  DO ji = fs_2, fs_jpim1
                     pfv_ups(ji,jj,jl) = MAX( pv(ji,jj), 0._wp ) * zpt(ji,jj,jl) + MIN( pv(ji,jj), 0._wp ) * zpt(ji,jj+1,jl)
                  END DO
               END DO
            END DO
            !
         ELSE                                                               !==  even ice time step:  adv_y then adv_x  ==!
            !
            DO jl = 1, jpl              !-- flux in y-direction
               DO jj = 1, jpjm1
                  DO ji = 1, jpi
                     pfv_ups(ji,jj,jl) = MAX( pv(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pv(ji,jj), 0._wp ) * pt(ji,jj+1,jl)
                  END DO
               END DO
            END DO
            !
            DO jl = 1, jpl              !-- first guess of tracer from v-flux
               DO jj = 2, jpjm1
                  DO ji = 1, jpi
                     ztra = - ( pfv_ups(ji,jj,jl) - pfv_ups(ji,jj-1,jl) )  &
                        &   + ( pv     (ji,jj   ) - pv     (ji,jj-1   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                     !
                     zpt(ji,jj,jl) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
                  END DO
               END DO
            END DO
            !
            DO jl = 1, jpl              !-- flux in x-direction
               DO jj = 2, jpjm1
                  DO ji = 1, fs_jpim1
                     pfu_ups(ji,jj,jl) = MAX( pu(ji,jj), 0._wp ) * zpt(ji,jj,jl) + MIN( pu(ji,jj), 0._wp ) * zpt(ji+1,jj,jl)
                  END DO
               END DO
            END DO
            !
         ENDIF
         
      ENDIF
      !
      DO jl = 1, jpl                    !-- after tracer with upstream scheme
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1
               ztra = - (   pfu_ups(ji,jj,jl) - pfu_ups(ji-1,jj  ,jl)   &
                  &       + pfv_ups(ji,jj,jl) - pfv_ups(ji  ,jj-1,jl) ) &
                  &   + (   pu     (ji,jj   ) - pu     (ji-1,jj     )   &
                  &       + pv     (ji,jj   ) - pv     (ji  ,jj-1   ) ) * pt(ji,jj,jl) * (1.-pamsk)
               !
               pt_ups(ji,jj,jl) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', pt_ups, 'T', 1. )

   END SUBROUTINE upstream

   
   SUBROUTINE cen2( pamsk, jt, kt, pdt, pt, pu, pv, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cen2  ***
      !!     
      !! **  Purpose :   compute the high order fluxes using a centered
      !!                 second order scheme 
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   ) ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt               ! tracer fields
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pu, pv           ! 2 ice velocity components
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt_ups           ! upstream guess of tracer 
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pfu_ups, pfv_ups ! upstream fluxes 
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfu_ho, pfv_ho   ! high order fluxes 
      !
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp) ::   ztra          ! local scalar
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zpt
      !!----------------------------------------------------------------------
      !
      IF( .NOT.ll_hoxy ) THEN           !** no alternate directions **!
         !
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, fs_jpim1
                  pfu_ho(ji,jj,jl) = 0.5_wp * pu(ji,jj) * ( pt(ji,jj,jl) + pt(ji+1,jj  ,jl) )
               END DO
            END DO
            DO jj = 1, jpjm1
               DO ji = 1, jpi
                  pfv_ho(ji,jj,jl) = 0.5_wp * pv(ji,jj) * ( pt(ji,jj,jl) + pt(ji  ,jj+1,jl) )
               END DO
            END DO
         END DO
         !
         IF    ( np_limiter == 1 ) THEN
            CALL nonosc_ice( pamsk, pdt, pu, pv, pt, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
         ELSEIF( np_limiter == 2 .OR. np_limiter == 3 ) THEN
            CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )
            CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
         ENDIF
         !
      ELSE                              !** alternate directions **!
         !
         IF( MOD( (kt - 1) / nn_fsbc , 2 ) ==  MOD( (jt - 1) , 2 ) ) THEN   !==  odd ice time step:  adv_x then adv_y  ==!
            !
            DO jl = 1, jpl              !-- flux in x-direction
               DO jj = 1, jpj
                  DO ji = 1, fs_jpim1
                     pfu_ho(ji,jj,jl) = 0.5_wp * pu(ji,jj) * ( pt(ji,jj,jl) + pt(ji+1,jj,jl) )
                  END DO
               END DO
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )

            DO jl = 1, jpl              !-- first guess of tracer from u-flux
               DO jj = 1, jpj
                  DO ji = fs_2, fs_jpim1
                     ztra = - ( pfu_ho(ji,jj,jl) - pfu_ho(ji-1,jj,jl) )              &
                        &   + ( pu    (ji,jj   ) - pu    (ji-1,jj   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                     !
                     zpt(ji,jj,jl) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
                  END DO
               END DO
            END DO

            DO jl = 1, jpl              !-- flux in y-direction
               DO jj = 1, jpjm1
                  DO ji = fs_2, fs_jpim1
                     pfv_ho(ji,jj,jl) = 0.5_wp * pv(ji,jj) * ( zpt(ji,jj,jl) + zpt(ji,jj+1,jl) )
                  END DO
               END DO
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )

         ELSE                                                               !==  even ice time step:  adv_y then adv_x  ==!
            !
            DO jl = 1, jpl              !-- flux in y-direction
               DO jj = 1, jpjm1
                  DO ji = 1, jpi
                     pfv_ho(ji,jj,jl) = 0.5_wp * pv(ji,jj) * ( pt(ji,jj,jl) + pt(ji,jj+1,jl) )
                  END DO
               END DO
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
            !
            DO jl = 1, jpl              !-- first guess of tracer from v-flux
               DO jj = 2, jpjm1
                  DO ji = 1, jpi
                     ztra = - ( pfv_ho(ji,jj,jl) - pfv_ho(ji,jj-1,jl) )  &
                        &   + ( pv    (ji,jj   ) - pv    (ji,jj-1   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                     !
                     zpt(ji,jj,jl) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
                  END DO
               END DO
            END DO
            !
            DO jl = 1, jpl              !-- flux in x-direction
               DO jj = 2, jpjm1
                  DO ji = 1, fs_jpim1
                     pfu_ho(ji,jj,jl) = 0.5_wp * pu(ji,jj) * ( zpt(ji,jj,jl) + zpt(ji+1,jj,jl) )
                  END DO
               END DO
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )

         ENDIF
         IF( np_limiter == 1 )   CALL nonosc_ice( pamsk, pdt, pu, pv, pt, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
         
      ENDIF
   
   END SUBROUTINE cen2

   
   SUBROUTINE macho( pamsk, kn_umx, jt, kt, pdt, pt, pu, pv, pubox, pvbox, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE macho  ***
      !!     
      !! **  Purpose :   compute the high order fluxes using Ultimate-Macho scheme  
      !!
      !! **  Method  :   ...
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74. 
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   kn_umx           ! order of the scheme (1-5=UM or 20=CEN2)
      INTEGER                         , INTENT(in   ) ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   ) ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt               ! tracer fields
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pu, pv           ! 2 ice velocity components
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pubox, pvbox     ! upstream velocity
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt_ups           ! upstream guess of tracer 
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pfu_ups, pfv_ups ! upstream fluxes 
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfu_ho, pfv_ho   ! high order fluxes 
      !
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zt_u, zt_v, zpt
      !!----------------------------------------------------------------------
      !
      IF( MOD( (kt - 1) / nn_fsbc , 2 ) ==  MOD( (jt - 1) , 2 ) ) THEN   !==  odd ice time step:  adv_x then adv_y  ==!
         !
         !                                                        !--  ultimate interpolation of pt at u-point  --!
         CALL ultimate_x( pamsk, kn_umx, pdt, pt, pu, zt_u, pfu_ho )
         !                                                        !--  limiter in x --!
         IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )
         !                                                        !--  advective form update in zpt  --!
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1
                  zpt(ji,jj,jl) = ( pt(ji,jj,jl) - (  pubox(ji,jj   ) * ( zt_u(ji,jj,jl) - zt_u(ji-1,jj,jl) ) * r1_e1t  (ji,jj) &
                     &                              + pt   (ji,jj,jl) * ( pu  (ji,jj   ) - pu  (ji-1,jj   ) ) * r1_e1e2t(ji,jj) &
                     &                                                                                        * pamsk           &
                     &                             ) * pdt ) * tmask(ji,jj,1)
               END DO
            END DO
         END DO
         CALL lbc_lnk( 'icedyn_adv_umx', zpt, 'T', 1. )
         !
         !                                                        !--  ultimate interpolation of pt at v-point  --!
         IF( ll_hoxy ) THEN
            CALL ultimate_y( pamsk, kn_umx, pdt, zpt, pv, zt_v, pfv_ho )
         ELSE
            CALL ultimate_y( pamsk, kn_umx, pdt, pt , pv, zt_v, pfv_ho )
         ENDIF
         !                                                        !--  limiter in y --!
         IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
         !         
         !
      ELSE                                                               !==  even ice time step:  adv_y then adv_x  ==!
         !
         !                                                        !--  ultimate interpolation of pt at v-point  --!
         CALL ultimate_y( pamsk, kn_umx, pdt, pt, pv, zt_v, pfv_ho )
         !                                                        !--  limiter in y --!
         IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
         !                                                        !--  advective form update in zpt  --!
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1
                  zpt(ji,jj,jl) = ( pt(ji,jj,jl) - (  pvbox(ji,jj   ) * ( zt_v(ji,jj,jl) - zt_v(ji,jj-1,jl) ) * r1_e2t  (ji,jj) &
                     &                              + pt   (ji,jj,jl) * ( pv  (ji,jj   ) - pv  (ji,jj-1   ) ) * r1_e1e2t(ji,jj) &
                     &                                                                                        * pamsk           &
                     &                             ) * pdt ) * tmask(ji,jj,1) 
               END DO
            END DO
         END DO
         CALL lbc_lnk( 'icedyn_adv_umx', zpt, 'T', 1. )
         !
         !                                                        !--  ultimate interpolation of pt at u-point  --!
         IF( ll_hoxy ) THEN
            CALL ultimate_x( pamsk, kn_umx, pdt, zpt, pu, zt_u, pfu_ho )
         ELSE
            CALL ultimate_x( pamsk, kn_umx, pdt, pt , pu, zt_u, pfu_ho )
         ENDIF
         !                                                        !--  limiter in x --!
         IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )
         !
      ENDIF

      IF( np_limiter == 1 )   CALL nonosc_ice( pamsk, pdt, pu, pv, pt, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !
   END SUBROUTINE macho


   SUBROUTINE ultimate_x( pamsk, kn_umx, pdt, pt, pu, pt_u, pfu_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE ultimate_x  ***
      !!     
      !! **  Purpose :   compute tracer at u-points 
      !!
      !! **  Method  :   ...
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74. 
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk     ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   kn_umx    ! order of the scheme (1-5=UM or 20=CEN2)
      REAL(wp)                        , INTENT(in   ) ::   pdt       ! tracer time-step
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pu        ! ice i-velocity component
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt        ! tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pt_u      ! tracer at u-point 
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfu_ho    ! high order flux 
      !
      INTEGER  ::   ji, jj, jl             ! dummy loop indices
      REAL(wp) ::   zcu, zdx2, zdx4        !   -      -
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   ztu1, ztu2, ztu3, ztu4
      !!----------------------------------------------------------------------
      !
      !                                                     !--  Laplacian in i-direction  --!
      DO jl = 1, jpl
         DO jj = 2, jpjm1         ! First derivative (gradient)
            DO ji = 1, fs_jpim1
               ztu1(ji,jj,jl) = ( pt(ji+1,jj,jl) - pt(ji,jj,jl) ) * r1_e1u(ji,jj) * umask(ji,jj,1)
            END DO
            !                     ! Second derivative (Laplacian)
            DO ji = fs_2, fs_jpim1
               ztu2(ji,jj,jl) = ( ztu1(ji,jj,jl) - ztu1(ji-1,jj,jl) ) * r1_e1t(ji,jj)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', ztu2, 'T', 1. )
      !
      !                                                     !--  BiLaplacian in i-direction  --!
      DO jl = 1, jpl
         DO jj = 2, jpjm1         ! Third derivative
            DO ji = 1, fs_jpim1
               ztu3(ji,jj,jl) = ( ztu2(ji+1,jj,jl) - ztu2(ji,jj,jl) ) * r1_e1u(ji,jj) * umask(ji,jj,1)
            END DO
            !                     ! Fourth derivative
            DO ji = fs_2, fs_jpim1
               ztu4(ji,jj,jl) = ( ztu3(ji,jj,jl) - ztu3(ji-1,jj,jl) ) * r1_e1t(ji,jj)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', ztu4, 'T', 1. )
      !
      !
      SELECT CASE (kn_umx )
      !
      CASE( 1 )                                                   !==  1st order central TIM  ==! (Eq. 21)
         !        
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  pt_u(ji,jj,jl) = 0.5_wp * umask(ji,jj,1) * (                                pt(ji+1,jj,jl) + pt(ji,jj,jl)   &
                     &                                         - SIGN( 1._wp, pu(ji,jj) ) * ( pt(ji+1,jj,jl) - pt(ji,jj,jl) ) )
               END DO
            END DO
         END DO
         !
      CASE( 2 )                                                   !==  2nd order central TIM  ==! (Eq. 23)
         !
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  pt_u(ji,jj,jl) = 0.5_wp * umask(ji,jj,1) * (                                pt(ji+1,jj,jl) + pt(ji,jj,jl)   &
                     &                                                            - zcu   * ( pt(ji+1,jj,jl) - pt(ji,jj,jl) ) ) 
               END DO
            END DO
         END DO
         !  
      CASE( 3 )                                                   !==  3rd order central TIM  ==! (Eq. 24)
         !
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  zdx2 = e1u(ji,jj) * e1u(ji,jj)
!!rachid          zdx2 = e1u(ji,jj) * e1t(ji,jj)
                  pt_u(ji,jj,jl) = 0.5_wp * umask(ji,jj,1) * (         (                      pt  (ji+1,jj,jl) + pt  (ji,jj,jl)     &
                     &                                                            - zcu   * ( pt  (ji+1,jj,jl) - pt  (ji,jj,jl) ) ) &
                     &        + z1_6 * zdx2 * ( zcu*zcu - 1._wp ) *    (                      ztu2(ji+1,jj,jl) + ztu2(ji,jj,jl)     &
                     &                                               - SIGN( 1._wp, zcu ) * ( ztu2(ji+1,jj,jl) - ztu2(ji,jj,jl) ) ) )
               END DO
            END DO
         END DO
         !
      CASE( 4 )                                                   !==  4th order central TIM  ==! (Eq. 27)
         !
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  zdx2 = e1u(ji,jj) * e1u(ji,jj)
!!rachid          zdx2 = e1u(ji,jj) * e1t(ji,jj)
                  pt_u(ji,jj,jl) = 0.5_wp * umask(ji,jj,1) * (         (                      pt  (ji+1,jj,jl) + pt  (ji,jj,jl)     &
                     &                                                            - zcu   * ( pt  (ji+1,jj,jl) - pt  (ji,jj,jl) ) ) &
                     &        + z1_6 * zdx2 * ( zcu*zcu - 1._wp ) *    (                      ztu2(ji+1,jj,jl) + ztu2(ji,jj,jl)     &
                     &                                                   - 0.5_wp * zcu   * ( ztu2(ji+1,jj,jl) - ztu2(ji,jj,jl) ) ) )
               END DO
            END DO
         END DO
         !
      CASE( 5 )                                                   !==  5th order central TIM  ==! (Eq. 29)
         !
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  zdx2 = e1u(ji,jj) * e1u(ji,jj)
!!rachid          zdx2 = e1u(ji,jj) * e1t(ji,jj)
                  zdx4 = zdx2 * zdx2
                  pt_u(ji,jj,jl) = 0.5_wp * umask(ji,jj,1) * (        (                       pt  (ji+1,jj,jl) + pt  (ji,jj,jl)     &
                     &                                                            - zcu   * ( pt  (ji+1,jj,jl) - pt  (ji,jj,jl) ) ) &
                     &        + z1_6   * zdx2 * ( zcu*zcu - 1._wp ) * (                       ztu2(ji+1,jj,jl) + ztu2(ji,jj,jl)     &
                     &                                                   - 0.5_wp * zcu   * ( ztu2(ji+1,jj,jl) - ztu2(ji,jj,jl) ) ) &
                     &        + z1_120 * zdx4 * ( zcu*zcu - 1._wp ) * ( zcu*zcu - 4._wp ) * ( ztu4(ji+1,jj,jl) + ztu4(ji,jj,jl)     &
                     &                                               - SIGN( 1._wp, zcu ) * ( ztu4(ji+1,jj,jl) - ztu4(ji,jj,jl) ) ) )
               END DO
            END DO
         END DO
         !
      END SELECT
      !
      ! if pt at u-point is negative then use the upstream value
      !    this should not be necessary if a proper sea-ice mask is set in Ultimate
      !    to degrade the order of the scheme when necessary (for ex. at the ice edge)
      IF( ll_neg ) THEN
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 1, fs_jpim1
                  IF( pt_u(ji,jj,jl) < 0._wp .OR. ( imsk_small(ji,jj,jl) == 0 .AND. pamsk == 0. ) ) THEN
                     pt_u(ji,jj,jl) = 0.5_wp * umask(ji,jj,1) * (                                pt(ji+1,jj,jl) + pt(ji,jj,jl)   &
                        &                                         - SIGN( 1._wp, pu(ji,jj) ) * ( pt(ji+1,jj,jl) - pt(ji,jj,jl) ) )
                  ENDIF
               END DO
            END DO
         END DO
      ENDIF
      !                                                     !-- High order flux in i-direction  --!
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               pfu_ho(ji,jj,jl) = pu(ji,jj) * pt_u(ji,jj,jl)
            END DO
         END DO
      END DO
      !
   END SUBROUTINE ultimate_x
   
 
   SUBROUTINE ultimate_y( pamsk, kn_umx, pdt, pt, pv, pt_v, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE ultimate_y  ***
      !!     
      !! **  Purpose :   compute tracer at v-points 
      !!
      !! **  Method  :   ...
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74. 
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk     ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   kn_umx    ! order of the scheme (1-5=UM or 20=CEN2)
      REAL(wp)                        , INTENT(in   ) ::   pdt       ! tracer time-step
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pv        ! ice j-velocity component
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt        ! tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pt_v      ! tracer at v-point 
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfv_ho    ! high order flux 
      !
      INTEGER  ::   ji, jj, jl         ! dummy loop indices
      REAL(wp) ::   zcv, zdy2, zdy4    !   -      -
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   ztv1, ztv2, ztv3, ztv4
      !!----------------------------------------------------------------------
      !
      !                                                     !--  Laplacian in j-direction  --!
      DO jl = 1, jpl
         DO jj = 1, jpjm1         ! First derivative (gradient)
            DO ji = fs_2, fs_jpim1
               ztv1(ji,jj,jl) = ( pt(ji,jj+1,jl) - pt(ji,jj,jl) ) * r1_e2v(ji,jj) * vmask(ji,jj,1)
            END DO
         END DO
         DO jj = 2, jpjm1         ! Second derivative (Laplacian)
            DO ji = fs_2, fs_jpim1
               ztv2(ji,jj,jl) = ( ztv1(ji,jj,jl) - ztv1(ji,jj-1,jl) ) * r1_e2t(ji,jj)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', ztv2, 'T', 1. )
      !
      !                                                     !--  BiLaplacian in j-direction  --!
      DO jl = 1, jpl
         DO jj = 1, jpjm1         ! First derivative
            DO ji = fs_2, fs_jpim1
               ztv3(ji,jj,jl) = ( ztv2(ji,jj+1,jl) - ztv2(ji,jj,jl) ) * r1_e2v(ji,jj) * vmask(ji,jj,1)
            END DO
         END DO
         DO jj = 2, jpjm1         ! Second derivative
            DO ji = fs_2, fs_jpim1
               ztv4(ji,jj,jl) = ( ztv3(ji,jj,jl) - ztv3(ji,jj-1,jl) ) * r1_e2t(ji,jj)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', ztv4, 'T', 1. )
      !
      !
      SELECT CASE (kn_umx )
         !
      CASE( 1 )                                                !==  1st order central TIM  ==! (Eq. 21)
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  pt_v(ji,jj,jl) = 0.5_wp * vmask(ji,jj,1) * (                                pt(ji,jj+1,jl) + pt(ji,jj,jl)   &
                     &                                         - SIGN( 1._wp, pv(ji,jj) ) * ( pt(ji,jj+1,jl) - pt(ji,jj,jl) ) )
               END DO
            END DO
         END DO
         !
      CASE( 2 )                                                !==  2nd order central TIM  ==! (Eq. 23)
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  pt_v(ji,jj,jl) = 0.5_wp * vmask(ji,jj,1) * (                                pt(ji,jj+1,jl) + pt(ji,jj,jl)   &
                     &                                                            - zcv *   ( pt(ji,jj+1,jl) - pt(ji,jj,jl) ) )
               END DO
            END DO
         END DO
         !
      CASE( 3 )                                                !==  3rd order central TIM  ==! (Eq. 24)
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  zdy2 = e2v(ji,jj) * e2v(ji,jj)
!!rachid          zdy2 = e2v(ji,jj) * e2t(ji,jj)
                  pt_v(ji,jj,jl) = 0.5_wp * vmask(ji,jj,1) * (      (                         pt  (ji,jj+1,jl) + pt  (ji,jj,jl)     &
                     &                                                            - zcv   * ( pt  (ji,jj+1,jl) - pt  (ji,jj,jl) ) ) &
                     &        + z1_6 * zdy2 * ( zcv*zcv - 1._wp ) * (                         ztv2(ji,jj+1,jl) + ztv2(ji,jj,jl)     &
                     &                                               - SIGN( 1._wp, zcv ) * ( ztv2(ji,jj+1,jl) - ztv2(ji,jj,jl) ) ) )
               END DO
            END DO
         END DO
         !
      CASE( 4 )                                                !==  4th order central TIM  ==! (Eq. 27)
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  zdy2 = e2v(ji,jj) * e2v(ji,jj)
!!rachid          zdy2 = e2v(ji,jj) * e2t(ji,jj)
                  pt_v(ji,jj,jl) = 0.5_wp * vmask(ji,jj,1) * (      (                         pt  (ji,jj+1,jl) + pt  (ji,jj,jl)     &
                     &                                                            - zcv   * ( pt  (ji,jj+1,jl) - pt  (ji,jj,jl) ) ) &
                     &        + z1_6 * zdy2 * ( zcv*zcv - 1._wp ) * (                         ztv2(ji,jj+1,jl) + ztv2(ji,jj,jl)     &
                     &                                                   - 0.5_wp * zcv   * ( ztv2(ji,jj+1,jl) - ztv2(ji,jj,jl) ) ) )
               END DO
            END DO
         END DO
         !
      CASE( 5 )                                                !==  5th order central TIM  ==! (Eq. 29)
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  zdy2 = e2v(ji,jj) * e2v(ji,jj)
!!rachid          zdy2 = e2v(ji,jj) * e2t(ji,jj)
                  zdy4 = zdy2 * zdy2
                  pt_v(ji,jj,jl) = 0.5_wp * vmask(ji,jj,1) * (                              ( pt  (ji,jj+1,jl) + pt  (ji,jj,jl)     &
                     &                                                            - zcv   * ( pt  (ji,jj+1,jl) - pt  (ji,jj,jl) ) ) &
                     &        + z1_6   * zdy2 * ( zcv*zcv - 1._wp ) * (                       ztv2(ji,jj+1,jl) + ztv2(ji,jj,jl)     &
                     &                                                   - 0.5_wp * zcv   * ( ztv2(ji,jj+1,jl) - ztv2(ji,jj,jl) ) ) &
                     &        + z1_120 * zdy4 * ( zcv*zcv - 1._wp ) * ( zcv*zcv - 4._wp ) * ( ztv4(ji,jj+1,jl) + ztv4(ji,jj,jl)     &
                     &                                               - SIGN( 1._wp, zcv ) * ( ztv4(ji,jj+1,jl) - ztv4(ji,jj,jl) ) ) )
               END DO
            END DO
         END DO
         !
      END SELECT
      !
      ! if pt at v-point is negative then use the upstream value
      !    this should not be necessary if a proper sea-ice mask is set in Ultimate
      !    to degrade the order of the scheme when necessary (for ex. at the ice edge)
      IF( ll_neg ) THEN
         DO jl = 1, jpl
            DO jj = 1, jpjm1
               DO ji = fs_2, fs_jpim1
                  IF( pt_v(ji,jj,jl) < 0._wp .OR. ( jmsk_small(ji,jj,jl) == 0 .AND. pamsk == 0. ) ) THEN
                     pt_v(ji,jj,jl) = 0.5_wp * vmask(ji,jj,1) * (                              ( pt(ji,jj+1,jl) + pt(ji,jj,jl) )  &
                        &                                         - SIGN( 1._wp, pv(ji,jj) ) * ( pt(ji,jj+1,jl) - pt(ji,jj,jl) ) )
                  ENDIF
               END DO
            END DO
         END DO
      ENDIF
      !                                                     !-- High order flux in j-direction  --!
      DO jl = 1, jpl
         DO jj = 1, jpjm1
            DO ji = fs_2, fs_jpim1
               pfv_ho(ji,jj,jl) = pv(ji,jj) * pt_v(ji,jj,jl)
            END DO
         END DO
      END DO
      !
   END SUBROUTINE ultimate_y
     

   SUBROUTINE nonosc_ice( pamsk, pdt, pu, pv, pt, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE nonosc_ice  ***
      !!     
      !! **  Purpose :   compute monotonic tracer fluxes from the upstream 
      !!       scheme and the before field by a non-oscillatory algorithm 
      !!
      !! **  Method  :   ...
      !!----------------------------------------------------------------------
      REAL(wp)                   , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      REAL(wp)                   , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION (:,:  ), INTENT(in   ) ::   pu               ! ice i-velocity => u*e2
      REAL(wp), DIMENSION (:,:  ), INTENT(in   ) ::   pv               ! ice j-velocity => v*e1
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pt, pt_ups       ! before field & upstream guess of after field
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pfv_ups, pfu_ups ! upstream flux
      REAL(wp), DIMENSION (:,:,:), INTENT(inout) ::   pfv_ho, pfu_ho   ! monotonic flux
      !
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp) ::   zpos, zneg, zbig, zup, zdo, z1_dt              ! local scalars
      REAL(wp) ::   zau, zbu, zcu, zav, zbv, zcv, zcoef, zzt       !   -      -
      REAL(wp), DIMENSION(jpi,jpj    ) :: zbup, zbdo
      REAL(wp), DIMENSION(jpi,jpj,jpl) :: zbetup, zbetdo, zti_ups, ztj_ups
      !!----------------------------------------------------------------------
      zbig = 1.e+40_wp
      
      ! antidiffusive flux : high order minus low order
      ! --------------------------------------------------
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               pfu_ho(ji,jj,jl) = pfu_ho(ji,jj,jl) - pfu_ups(ji,jj,jl)
            END DO
         END DO
         DO jj = 1, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               pfv_ho(ji,jj,jl) = pfv_ho(ji,jj,jl) - pfv_ups(ji,jj,jl)
            END DO
         END DO
      END DO

      ! extreme case where pfu_ho has to be zero
      ! ----------------------------------------
      !                                    pfu_ho
      !                           *         --->
      !                        |      |  *   |        | 
      !                        |      |      |    *   |    
      !                        |      |      |        |    *
      !            t_ups :       i-1     i       i+1       i+2   
      IF( ll_prelim ) THEN
         
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1 
                  zti_ups(ji,jj,jl)= pt_ups(ji+1,jj  ,jl)
                  ztj_ups(ji,jj,jl)= pt_ups(ji  ,jj+1,jl)
               END DO
            END DO
         END DO
         CALL lbc_lnk_multi( 'icedyn_adv_umx', zti_ups, 'T', 1., ztj_ups, 'T', 1. )

         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1
                  IF ( pfu_ho(ji,jj,jl) * ( pt_ups(ji+1,jj  ,jl) - pt_ups(ji,jj,jl) ) <= 0._wp .AND.  &
                     & pfv_ho(ji,jj,jl) * ( pt_ups(ji  ,jj+1,jl) - pt_ups(ji,jj,jl) ) <= 0._wp ) THEN
                     !
                     IF(  pfu_ho(ji,jj,jl) * ( zti_ups(ji+1,jj  ,jl) - zti_ups(ji,jj,jl) ) <= 0._wp .AND.  &
                        & pfv_ho(ji,jj,jl) * ( ztj_ups(ji  ,jj+1,jl) - ztj_ups(ji,jj,jl) ) <= 0._wp ) THEN
                        pfu_ho(ji,jj,jl)=0._wp
                        pfv_ho(ji,jj,jl)=0._wp
                     ENDIF
                     !
                     IF(  pfu_ho(ji,jj,jl) * ( pt_ups(ji,jj,jl) - pt_ups(ji-1,jj  ,jl) ) <= 0._wp .AND.  &
                        & pfv_ho(ji,jj,jl) * ( pt_ups(ji,jj,jl) - pt_ups(ji  ,jj-1,jl) ) <= 0._wp ) THEN
                        pfu_ho(ji,jj,jl)=0._wp
                        pfv_ho(ji,jj,jl)=0._wp
                     ENDIF
                     !
                  ENDIF
               END DO
            END DO
         END DO
         CALL lbc_lnk_multi( 'icedyn_adv_umx', pfu_ho, 'U', -1., pfv_ho, 'V', -1. )   ! lateral boundary cond.

      ENDIF

      ! Search local extrema
      ! --------------------
      ! max/min of pt & pt_ups with large negative/positive value (-/+zbig) outside ice cover
      z1_dt = 1._wp / pdt
      DO jl = 1, jpl
         
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF    ( pt(ji,jj,jl) <= 0._wp .AND. pt_ups(ji,jj,jl) <= 0._wp ) THEN
                  zbup(ji,jj) = -zbig
                  zbdo(ji,jj) =  zbig
               ELSEIF( pt(ji,jj,jl) <= 0._wp .AND. pt_ups(ji,jj,jl) > 0._wp ) THEN
                  zbup(ji,jj) = pt_ups(ji,jj,jl)
                  zbdo(ji,jj) = pt_ups(ji,jj,jl)
               ELSEIF( pt(ji,jj,jl) > 0._wp .AND. pt_ups(ji,jj,jl) <= 0._wp ) THEN
                  zbup(ji,jj) = pt(ji,jj,jl)
                  zbdo(ji,jj) = pt(ji,jj,jl)
               ELSE
                  zbup(ji,jj) = MAX( pt(ji,jj,jl) , pt_ups(ji,jj,jl) )
                  zbdo(ji,jj) = MIN( pt(ji,jj,jl) , pt_ups(ji,jj,jl) )
               ENDIF
            END DO
         END DO

         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               !
               zup  = MAX( zbup(ji,jj), zbup(ji-1,jj), zbup(ji+1,jj), zbup(ji,jj-1), zbup(ji,jj+1) )  ! search max/min in neighbourhood
               zdo  = MIN( zbdo(ji,jj), zbdo(ji-1,jj), zbdo(ji+1,jj), zbdo(ji,jj-1), zbdo(ji,jj+1) )
               !
               zpos = MAX( 0._wp, pfu_ho(ji-1,jj  ,jl) ) - MIN( 0._wp, pfu_ho(ji  ,jj  ,jl) ) &  ! positive/negative part of the flux
                  & + MAX( 0._wp, pfv_ho(ji  ,jj-1,jl) ) - MIN( 0._wp, pfv_ho(ji  ,jj  ,jl) )
               zneg = MAX( 0._wp, pfu_ho(ji  ,jj  ,jl) ) - MIN( 0._wp, pfu_ho(ji-1,jj  ,jl) ) &
                  & + MAX( 0._wp, pfv_ho(ji  ,jj  ,jl) ) - MIN( 0._wp, pfv_ho(ji  ,jj-1,jl) )
               !
               zpos = zpos - (pt(ji,jj,jl) * MIN( 0., pu(ji,jj) - pu(ji-1,jj) ) + pt(ji,jj,jl) * MIN( 0., pv(ji,jj) - pv(ji,jj-1) ) &
                  &          ) * ( 1. - pamsk )
               zneg = zneg + (pt(ji,jj,jl) * MAX( 0., pu(ji,jj) - pu(ji-1,jj) ) + pt(ji,jj,jl) * MAX( 0., pv(ji,jj) - pv(ji,jj-1) ) &
                  &          ) * ( 1. - pamsk )
               !
               !                                  ! up & down beta terms
               ! clem: zbetup and zbetdo must be 0 for zpos>1.e-10 & zneg>1.e-10 (do not put 0 instead of 1.e-10 !!!)
               IF( zpos > epsi10 ) THEN ; zbetup(ji,jj,jl) = MAX( 0._wp, zup - pt_ups(ji,jj,jl) ) / zpos * e1e2t(ji,jj) * z1_dt
               ELSE                     ; zbetup(ji,jj,jl) = 0._wp ! zbig
               ENDIF
               !
               IF( zneg > epsi10 ) THEN ; zbetdo(ji,jj,jl) = MAX( 0._wp, pt_ups(ji,jj,jl) - zdo ) / zneg * e1e2t(ji,jj) * z1_dt
               ELSE                     ; zbetdo(ji,jj,jl) = 0._wp ! zbig
               ENDIF
               !
               ! if all the points are outside ice cover
               IF( zup == -zbig )   zbetup(ji,jj,jl) = 0._wp ! zbig
               IF( zdo ==  zbig )   zbetdo(ji,jj,jl) = 0._wp ! zbig            
               !
            END DO
         END DO
      END DO
      CALL lbc_lnk_multi( 'icedyn_adv_umx', zbetup, 'T', 1., zbetdo, 'T', 1. )   ! lateral boundary cond. (unchanged sign)

      
      ! monotonic flux in the y direction
      ! ---------------------------------
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               zau = MIN( 1._wp , zbetdo(ji,jj,jl) , zbetup(ji+1,jj,jl) )
               zbu = MIN( 1._wp , zbetup(ji,jj,jl) , zbetdo(ji+1,jj,jl) )
               zcu = 0.5_wp + SIGN( 0.5_wp , pfu_ho(ji,jj,jl) )
               !
               zcoef = ( zcu * zau + ( 1._wp - zcu ) * zbu )
               !
               pfu_ho(ji,jj,jl) = pfu_ho(ji,jj,jl) * zcoef + pfu_ups(ji,jj,jl)
               !
            END DO
         END DO

         DO jj = 1, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zav = MIN( 1._wp , zbetdo(ji,jj,jl) , zbetup(ji,jj+1,jl) )
               zbv = MIN( 1._wp , zbetup(ji,jj,jl) , zbetdo(ji,jj+1,jl) )
               zcv = 0.5_wp + SIGN( 0.5_wp , pfv_ho(ji,jj,jl) )
               !
               zcoef = ( zcv * zav + ( 1._wp - zcv ) * zbv )
               !
               pfv_ho(ji,jj,jl) = pfv_ho(ji,jj,jl) * zcoef + pfv_ups(ji,jj,jl)
               !
            END DO
         END DO

      END DO
      !
   END SUBROUTINE nonosc_ice

   
   SUBROUTINE limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE limiter_x  ***
      !!     
      !! **  Purpose :   compute flux limiter 
      !!----------------------------------------------------------------------
      REAL(wp)                  , INTENT(in   ) ::   pdt          ! tracer time-step
      REAL(wp), DIMENSION(:,:  ), INTENT(in   ) ::   pu           ! ice i-velocity => u*e2
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pt           ! ice tracer
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pfu_ups      ! upstream flux
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   pfu_ho       ! high order flux
      !
      REAL(wp) ::   Cr, Rjm, Rj, Rjp, uCFL, zpsi, zh3, zlimiter, Rr
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp), DIMENSION (jpi,jpj,jpl) ::   zslpx       ! tracer slopes 
      !!----------------------------------------------------------------------
      !
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zslpx(ji,jj,jl) = ( pt(ji+1,jj,jl) - pt(ji,jj,jl) ) * umask(ji,jj,1)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', zslpx, 'U', -1.)   ! lateral boundary cond.
      
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               uCFL = pdt * ABS( pu(ji,jj) ) * r1_e1e2t(ji,jj)
               
               Rjm = zslpx(ji-1,jj,jl)
               Rj  = zslpx(ji  ,jj,jl)
               Rjp = zslpx(ji+1,jj,jl)

               IF( np_limiter == 3 ) THEN

                  IF( pu(ji,jj) > 0. ) THEN   ;   Rr = Rjm
                  ELSE                        ;   Rr = Rjp
                  ENDIF

                  zh3 = pfu_ho(ji,jj,jl) - pfu_ups(ji,jj,jl)     
                  IF( Rj > 0. ) THEN
                     zlimiter =  MAX( 0., MIN( zh3, MAX(-Rr * 0.5 * ABS(pu(ji,jj)),  &
                        &        MIN( 2. * Rr * 0.5 * ABS(pu(ji,jj)),  zh3,  1.5 * Rj * 0.5 * ABS(pu(ji,jj)) ) ) ) )
                  ELSE
                     zlimiter = -MAX( 0., MIN(-zh3, MAX( Rr * 0.5 * ABS(pu(ji,jj)),  &
                        &        MIN(-2. * Rr * 0.5 * ABS(pu(ji,jj)), -zh3, -1.5 * Rj * 0.5 * ABS(pu(ji,jj)) ) ) ) )
                  ENDIF
                  pfu_ho(ji,jj,jl) = pfu_ups(ji,jj,jl) + zlimiter

               ELSEIF( np_limiter == 2 ) THEN
                  IF( Rj /= 0. ) THEN
                     IF( pu(ji,jj) > 0. ) THEN   ;   Cr = Rjm / Rj
                     ELSE                        ;   Cr = Rjp / Rj
                     ENDIF
                  ELSE
                     Cr = 0.
                  ENDIF

                  ! -- superbee --
                  zpsi = MAX( 0., MAX( MIN(1.,2.*Cr), MIN(2.,Cr) ) )
                  ! -- van albada 2 --
                  !!zpsi = 2.*Cr / (Cr*Cr+1.)
                  ! -- sweby (with beta=1) --
                  !!zpsi = MAX( 0., MAX( MIN(1.,1.*Cr), MIN(1.,Cr) ) )
                  ! -- van Leer --
                  !!zpsi = ( Cr + ABS(Cr) ) / ( 1. + ABS(Cr) )
                  ! -- ospre --
                  !!zpsi = 1.5 * ( Cr*Cr + Cr ) / ( Cr*Cr + Cr + 1. )
                  ! -- koren --
                  !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( (1.+2*Cr)/3., 2. ) ) )
                  ! -- charm --
                  !IF( Cr > 0. ) THEN   ;   zpsi = Cr * (3.*Cr + 1.) / ( (Cr + 1.) * (Cr + 1.) )
                  !ELSE                 ;   zpsi = 0.
                  !ENDIF
                  ! -- van albada 1 --
                  !!zpsi = (Cr*Cr + Cr) / (Cr*Cr +1)
                  ! -- smart --
                  !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, 4. ) ) )
                  ! -- umist --
                  !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, MIN(0.75+0.25*Cr, 2. ) ) ) )

                  ! high order flux corrected by the limiter
                  pfu_ho(ji,jj,jl) = pfu_ho(ji,jj,jl) - ABS( pu(ji,jj) ) * ( (1.-zpsi) + uCFL*zpsi ) * Rj * 0.5

               ENDIF
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', pfu_ho, 'U', -1.)   ! lateral boundary cond.
      !
   END SUBROUTINE limiter_x

   
   SUBROUTINE limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE limiter_y  ***
      !!     
      !! **  Purpose :   compute flux limiter 
      !!----------------------------------------------------------------------
      REAL(wp)                   , INTENT(in   ) ::   pdt          ! tracer time-step
      REAL(wp), DIMENSION (:,:  ), INTENT(in   ) ::   pv           ! ice i-velocity => u*e2
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pt           ! ice tracer
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pfv_ups      ! upstream flux
      REAL(wp), DIMENSION (:,:,:), INTENT(inout) ::   pfv_ho       ! high order flux
      !
      REAL(wp) ::   Cr, Rjm, Rj, Rjp, vCFL, zpsi, zh3, zlimiter, Rr
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp), DIMENSION (jpi,jpj,jpl) ::   zslpy       ! tracer slopes 
      !!----------------------------------------------------------------------
      !
      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zslpy(ji,jj,jl) = ( pt(ji,jj+1,jl) - pt(ji,jj,jl) ) * vmask(ji,jj,1)
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', zslpy, 'V', -1.)   ! lateral boundary cond.

      DO jl = 1, jpl
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               vCFL = pdt * ABS( pv(ji,jj) ) * r1_e1e2t(ji,jj)

               Rjm = zslpy(ji,jj-1,jl)
               Rj  = zslpy(ji,jj  ,jl)
               Rjp = zslpy(ji,jj+1,jl)

               IF( np_limiter == 3 ) THEN

                  IF( pv(ji,jj) > 0. ) THEN   ;   Rr = Rjm
                  ELSE                        ;   Rr = Rjp
                  ENDIF

                  zh3 = pfv_ho(ji,jj,jl) - pfv_ups(ji,jj,jl)     
                  IF( Rj > 0. ) THEN
                     zlimiter =  MAX( 0., MIN( zh3, MAX(-Rr * 0.5 * ABS(pv(ji,jj)),  &
                        &        MIN( 2. * Rr * 0.5 * ABS(pv(ji,jj)),  zh3,  1.5 * Rj * 0.5 * ABS(pv(ji,jj)) ) ) ) )
                  ELSE
                     zlimiter = -MAX( 0., MIN(-zh3, MAX( Rr * 0.5 * ABS(pv(ji,jj)),  &
                        &        MIN(-2. * Rr * 0.5 * ABS(pv(ji,jj)), -zh3, -1.5 * Rj * 0.5 * ABS(pv(ji,jj)) ) ) ) )
                  ENDIF
                  pfv_ho(ji,jj,jl) = pfv_ups(ji,jj,jl) + zlimiter

               ELSEIF( np_limiter == 2 ) THEN

                  IF( Rj /= 0. ) THEN
                     IF( pv(ji,jj) > 0. ) THEN   ;   Cr = Rjm / Rj
                     ELSE                        ;   Cr = Rjp / Rj
                     ENDIF
                  ELSE
                     Cr = 0.
                  ENDIF

                  ! -- superbee --
                  zpsi = MAX( 0., MAX( MIN(1.,2.*Cr), MIN(2.,Cr) ) )
                  ! -- van albada 2 --
                  !!zpsi = 2.*Cr / (Cr*Cr+1.)
                  ! -- sweby (with beta=1) --
                  !!zpsi = MAX( 0., MAX( MIN(1.,1.*Cr), MIN(1.,Cr) ) )
                  ! -- van Leer --
                  !!zpsi = ( Cr + ABS(Cr) ) / ( 1. + ABS(Cr) )
                  ! -- ospre --
                  !!zpsi = 1.5 * ( Cr*Cr + Cr ) / ( Cr*Cr + Cr + 1. )
                  ! -- koren --
                  !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( (1.+2*Cr)/3., 2. ) ) )
                  ! -- charm --
                  !IF( Cr > 0. ) THEN   ;   zpsi = Cr * (3.*Cr + 1.) / ( (Cr + 1.) * (Cr + 1.) )
                  !ELSE                 ;   zpsi = 0.
                  !ENDIF
                  ! -- van albada 1 --
                  !!zpsi = (Cr*Cr + Cr) / (Cr*Cr +1)
                  ! -- smart --
                  !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, 4. ) ) )
                  ! -- umist --
                  !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, MIN(0.75+0.25*Cr, 2. ) ) ) )

                  ! high order flux corrected by the limiter
                  pfv_ho(ji,jj,jl) = pfv_ho(ji,jj,jl) - ABS( pv(ji,jj) ) * ( (1.-zpsi) + vCFL*zpsi ) * Rj * 0.5

               ENDIF
            END DO
         END DO
      END DO
      CALL lbc_lnk( 'icedyn_adv_umx', pfv_ho, 'V', -1.)   ! lateral boundary cond.
      !
   END SUBROUTINE limiter_y


   SUBROUTINE Hbig( pdt, phi_max, phs_max, phip_max, psi_max, pes_max, pei_max, &
      &                  pv_i, pv_s, pa_i, pa_ip, pv_ip, psv_i, pe_s, pe_i )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE Hbig  ***
      !!
      !! ** Purpose : Thickness correction in case advection scheme creates
      !!              abnormally tick ice or snow
      !!
      !! ** Method  : 1- check whether ice thickness is larger than the surrounding 9-points
      !!                 (before advection) and reduce it by adapting ice concentration
      !!              2- check whether snow thickness is larger than the surrounding 9-points
      !!                 (before advection) and reduce it by sending the excess in the ocean
      !!
      !! ** input   : Max thickness of the surrounding 9-points
      !!-------------------------------------------------------------------
      REAL(wp)                    , INTENT(in   ) ::   pdt                                   ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   phi_max, phs_max, phip_max, psi_max   ! max ice thick from surrounding 9-pts
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pes_max
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pei_max
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_i, pv_s, pa_i, pa_ip, pv_ip, psv_i
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_s
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_i
      !
      INTEGER  ::   ji, jj, jk, jl         ! dummy loop indices
      REAL(wp) ::   z1_dt, zhip, zhi, zhs, zsi, zes, zei, zfra
      !!-------------------------------------------------------------------
      !
      z1_dt = 1._wp / pdt
      !
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( pv_i(ji,jj,jl) > 0._wp ) THEN
                  !
                  !                               ! -- check h_ip -- !
                  ! if h_ip is larger than the surrounding 9 pts => reduce h_ip and increase a_ip
                  IF( ln_pnd_LEV .AND. pv_ip(ji,jj,jl) > 0._wp ) THEN
                     zhip = pv_ip(ji,jj,jl) / MAX( epsi20, pa_ip(ji,jj,jl) )
                     IF( zhip > phip_max(ji,jj,jl) .AND. pa_ip(ji,jj,jl) < 0.15 ) THEN
                        pa_ip(ji,jj,jl) = pv_ip(ji,jj,jl) / phip_max(ji,jj,jl)
                     ENDIF
                  ENDIF
                  !
                  !                               ! -- check h_i -- !
                  ! if h_i is larger than the surrounding 9 pts => reduce h_i and increase a_i
                  zhi = pv_i(ji,jj,jl) / pa_i(ji,jj,jl)
                  IF( zhi > phi_max(ji,jj,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                     pa_i(ji,jj,jl) = pv_i(ji,jj,jl) / MIN( phi_max(ji,jj,jl), hi_max(jpl) )   !-- bound h_i to hi_max (99 m)
                  ENDIF
                  !
                  !                               ! -- check h_s -- !
                  ! if h_s is larger than the surrounding 9 pts => put the snow excess in the ocean
                  zhs = pv_s(ji,jj,jl) / pa_i(ji,jj,jl)
                  IF( pv_s(ji,jj,jl) > 0._wp .AND. zhs > phs_max(ji,jj,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                     zfra = phs_max(ji,jj,jl) / MAX( zhs, epsi20 )
                     !
                     wfx_res(ji,jj) = wfx_res(ji,jj) + ( pv_s(ji,jj,jl) - pa_i(ji,jj,jl) * phs_max(ji,jj,jl) ) * rhos * z1_dt
                     hfx_res(ji,jj) = hfx_res(ji,jj) - SUM( pe_s(ji,jj,1:nlay_s,jl) ) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                     !
                     pe_s(ji,jj,1:nlay_s,jl) = pe_s(ji,jj,1:nlay_s,jl) * zfra
                     pv_s(ji,jj,jl)          = pa_i(ji,jj,jl) * phs_max(ji,jj,jl)
                  ENDIF           
                  !                  
                  !                               ! -- check s_i -- !
                  ! if s_i is larger than the surrounding 9 pts => put salt excess in the ocean
                  zsi = psv_i(ji,jj,jl) / pv_i(ji,jj,jl)
                  IF( zsi > psi_max(ji,jj,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                     zfra = psi_max(ji,jj,jl) / zsi
                     sfx_res(ji,jj) = sfx_res(ji,jj) + psv_i(ji,jj,jl) * ( 1._wp - zfra ) * rhoi * z1_dt
                     psv_i(ji,jj,jl) = psv_i(ji,jj,jl) * zfra
                  ENDIF
                  !
               ENDIF
            END DO
         END DO
      END DO 
      !
      !                                           ! -- check e_i/v_i -- !
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF ( pv_i(ji,jj,jl) > 0._wp ) THEN
                     ! if e_i/v_i is larger than the surrounding 9 pts => put the heat excess in the ocean
                     zei = pe_i(ji,jj,jk,jl) / pv_i(ji,jj,jl)
                     IF( zei > pei_max(ji,jj,jk,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                        zfra = pei_max(ji,jj,jk,jl) / zei
                        hfx_res(ji,jj) = hfx_res(ji,jj) - pe_i(ji,jj,jk,jl) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                        pe_i(ji,jj,jk,jl) = pe_i(ji,jj,jk,jl) * zfra
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
      END DO
      !                                           ! -- check e_s/v_s -- !
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF ( pv_s(ji,jj,jl) > 0._wp ) THEN
                     ! if e_s/v_s is larger than the surrounding 9 pts => put the heat excess in the ocean
                     zes = pe_s(ji,jj,jk,jl) / pv_s(ji,jj,jl)
                     IF( zes > pes_max(ji,jj,jk,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                        zfra = pes_max(ji,jj,jk,jl) / zes
                        hfx_res(ji,jj) = hfx_res(ji,jj) - pe_s(ji,jj,jk,jl) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                        pe_s(ji,jj,jk,jl) = pe_s(ji,jj,jk,jl) * zfra
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
      END DO
      !
   END SUBROUTINE Hbig


   SUBROUTINE Hsnow( pdt, pv_i, pv_s, pa_i, pa_ip, pe_s )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE Hsnow  ***
      !!
      !! ** Purpose : 1- Check snow load after advection
      !!              2- Correct pond concentration to avoid a_ip > a_i
      !!
      !! ** Method :  If snow load makes snow-ice interface to deplet below the ocean surface
      !!              then put the snow excess in the ocean
      !!
      !! ** Notes :   This correction is crucial because of the call to routine icecor afterwards
      !!              which imposes a mini of ice thick. (rn_himin). This imposed mini can artificially
      !!              make the snow very thick (if concentration decreases drastically)
      !!              This behavior has been seen in Ultimate-Macho and supposedly it can also be true for Prather
      !!-------------------------------------------------------------------
      REAL(wp)                    , INTENT(in   ) ::   pdt   ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_i, pv_s, pa_i, pa_ip
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_s
      !
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      REAL(wp) ::   z1_dt, zvs_excess, zfra
      !!-------------------------------------------------------------------
      !
      z1_dt = 1._wp / pdt
      !
      ! -- check snow load -- !
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( pv_i(ji,jj,jl) > 0._wp ) THEN
                  !
                  zvs_excess = MAX( 0._wp, pv_s(ji,jj,jl) - pv_i(ji,jj,jl) * (rau0-rhoi) * r1_rhos )
                  !
                  IF( zvs_excess > 0._wp ) THEN   ! snow-ice interface deplets below the ocean surface
                     ! put snow excess in the ocean
                     zfra = ( pv_s(ji,jj,jl) - zvs_excess ) / MAX( pv_s(ji,jj,jl), epsi20 )
                     wfx_res(ji,jj) = wfx_res(ji,jj) + zvs_excess * rhos * z1_dt
                     hfx_res(ji,jj) = hfx_res(ji,jj) - SUM( pe_s(ji,jj,1:nlay_s,jl) ) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                     ! correct snow volume and heat content
                     pe_s(ji,jj,1:nlay_s,jl) = pe_s(ji,jj,1:nlay_s,jl) * zfra
                     pv_s(ji,jj,jl)          = pv_s(ji,jj,jl) - zvs_excess
                  ENDIF
                  !
               ENDIF
            END DO
         END DO
      END DO
      !
      !-- correct pond concentration to avoid a_ip > a_i -- !
      WHERE( pa_ip(:,:,:) > pa_i(:,:,:) )   pa_ip(:,:,:) = pa_i(:,:,:)
      !
   END SUBROUTINE Hsnow

   SUBROUTINE icemax3D( pice , pmax )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE icemax3D ***                     
      !! ** Purpose :  compute the max of the 9 points around
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in ) ::   pice   ! input
      REAL(wp), DIMENSION(:,:,:)      , INTENT(out) ::   pmax   ! output
      REAL(wp), DIMENSION(2:jpim1,jpj)              ::   zmax   ! temporary array
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      !!----------------------------------------------------------------------
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 2, jpim1
               zmax(ji,jj) = MAX( epsi20, pice(ji,jj,jl), pice(ji-1,jj,jl), pice(ji+1,jj,jl) )
            END DO
         END DO
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               pmax(ji,jj,jl) = MAX( epsi20, zmax(ji,jj), zmax(ji,jj-1), zmax(ji,jj+1) )
            END DO
         END DO
      END DO
   END SUBROUTINE icemax3D

   SUBROUTINE icemax4D( pice , pmax )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE icemax4D ***                     
      !! ** Purpose :  compute the max of the 9 points around
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:,:)    , INTENT(in ) ::   pice   ! input
      REAL(wp), DIMENSION(:,:,:,:)    , INTENT(out) ::   pmax   ! output
      REAL(wp), DIMENSION(2:jpim1,jpj)              ::   zmax   ! temporary array
      INTEGER  ::   jlay, ji, jj, jk, jl   ! dummy loop indices
      !!----------------------------------------------------------------------
      jlay = SIZE( pice , 3 )   ! size of input arrays
      DO jl = 1, jpl
         DO jk = 1, jlay
            DO jj = 1, jpj
               DO ji = 2, jpim1
                  zmax(ji,jj) = MAX( epsi20, pice(ji,jj,jk,jl), pice(ji-1,jj,jk,jl), pice(ji+1,jj,jk,jl) )
               END DO
            END DO
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  pmax(ji,jj,jk,jl) = MAX( epsi20, zmax(ji,jj), zmax(ji,jj-1), zmax(ji,jj+1) )
               END DO
            END DO
         END DO
      END DO
   END SUBROUTINE icemax4D

#else
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icedyn_adv_umx
