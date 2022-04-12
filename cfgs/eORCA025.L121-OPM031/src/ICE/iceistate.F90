MODULE iceistate
   !!======================================================================
   !!                     ***  MODULE  iceistate  ***
   !!   sea-ice : Initialization of ice variables
   !!======================================================================
   !! History :  2.0  !  2004-01  (C. Ethe, G. Madec) Original code
   !!            3.0  !  2007     (M. Vancoppenolle)  Rewrite for ice cats
   !!            4.0  !  2018     (many people)       SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_istate       :  initialization of diagnostics ice variables
   !!   ice_istate_init  :  initialization of ice state and namelist read
   !!----------------------------------------------------------------------
   USE phycst         ! physical constant
   USE oce            ! dynamics and tracers variables
   USE dom_oce        ! ocean domain
   USE sbc_oce , ONLY : sst_m, sss_m, ln_ice_embd 
   USE sbc_ice , ONLY : tn_ice, snwice_mass, snwice_mass_b
   USE eosbn2         ! equation of state
   USE domvvl         ! Variable volume
   USE ice            ! sea-ice: variables
   USE ice1D          ! sea-ice: thermodynamics variables
   USE icetab         ! sea-ice: 1D <==> 2D transformation
   USE icevar         ! sea-ice: operations
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE fldread        ! read input fields

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_istate        ! called by icestp.F90
   PUBLIC   ice_istate_init   ! called by icestp.F90
   !
   !                             !! ** namelist (namini) **
   LOGICAL, PUBLIC  ::   ln_iceini        !: Ice initialization or not
   INTEGER, PUBLIC  ::   nn_iceini_file   !: Ice initialization:
                                  !        0 = Initialise sea ice based on SSTs
                                  !        1 = Initialise sea ice from single category netcdf file
                                  !        2 = Initialise sea ice from multi category restart file
   REAL(wp) ::   rn_thres_sst
   REAL(wp) ::   rn_hti_ini_n, rn_hts_ini_n, rn_ati_ini_n, rn_smi_ini_n, rn_tmi_ini_n, rn_tsu_ini_n, rn_tms_ini_n
   REAL(wp) ::   rn_hti_ini_s, rn_hts_ini_s, rn_ati_ini_s, rn_smi_ini_s, rn_tmi_ini_s, rn_tsu_ini_s, rn_tms_ini_s
   REAL(wp) ::   rn_apd_ini_n, rn_hpd_ini_n, rn_hld_ini_n
   REAL(wp) ::   rn_apd_ini_s, rn_hpd_ini_s, rn_hld_ini_s
   !
   !                              ! if nn_iceini_file = 1
   INTEGER , PARAMETER ::   jpfldi = 10          ! maximum number of files to read
   INTEGER , PARAMETER ::   jp_hti = 1           ! index of ice thickness    (m)
   INTEGER , PARAMETER ::   jp_hts = 2           ! index of snw thickness    (m)
   INTEGER , PARAMETER ::   jp_ati = 3           ! index of ice fraction     (-)
   INTEGER , PARAMETER ::   jp_smi = 4           ! index of ice salinity     (g/kg)
   INTEGER , PARAMETER ::   jp_tmi = 5           ! index of ice temperature  (K)
   INTEGER , PARAMETER ::   jp_tsu = 6           ! index of ice surface temp (K)
   INTEGER , PARAMETER ::   jp_tms = 7           ! index of snw temperature  (K)
   INTEGER , PARAMETER ::   jp_apd = 8           ! index of pnd fraction     (-)
   INTEGER , PARAMETER ::   jp_hpd = 9           ! index of pnd depth        (m)
   INTEGER , PARAMETER ::   jp_hld = 10          ! index of pnd lid depth    (m)
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   si  ! structure of input fields (file informations, fields read)
   !   
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: iceistate.F90 13284 2020-07-09 15:12:23Z smasson $
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_istate( kt )
      !!-------------------------------------------------------------------
      !!                    ***  ROUTINE ice_istate  ***
      !!
      !! ** Purpose :   defined the sea-ice initial state
      !!
      !! ** Method  :   This routine will put some ice where ocean
      !!                is at the freezing point, then fill in ice 
      !!                state variables using prescribed initial 
      !!                values in the namelist            
      !!
      !! ** Steps   :   1) Set initial surface and basal temperatures
      !!                2) Recompute or read sea ice state variables
      !!                3) Fill in space-dependent arrays for state variables
      !!                4) snow-ice mass computation
      !!
      !! ** Notes   : o_i, t_su, t_s, t_i, sz_i must be filled everywhere, even
      !!              where there is no ice
      !!--------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! time step 
      !!
      INTEGER  ::   ji, jj, jk, jl         ! dummy loop indices
      REAL(wp) ::   ztmelts
      INTEGER , DIMENSION(4)           ::   itest
      REAL(wp), DIMENSION(jpi,jpj)     ::   z2d
      REAL(wp), DIMENSION(jpi,jpj)     ::   zswitch    ! ice indicator
      REAL(wp), DIMENSION(jpi,jpj)     ::   zht_i_ini, zat_i_ini, ztm_s_ini            !data from namelist or nc file
      REAL(wp), DIMENSION(jpi,jpj)     ::   zt_su_ini, zht_s_ini, zsm_i_ini, ztm_i_ini !data from namelist or nc file
      REAL(wp), DIMENSION(jpi,jpj)     ::   zapnd_ini, zhpnd_ini, zhlid_ini            !data from namelist or nc file
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zti_3d , zts_3d                            !temporary arrays
      !!
      REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   zhi_2d, zhs_2d, zai_2d, zti_2d, zts_2d, ztsu_2d, zsi_2d, zaip_2d, zhip_2d, zhil_2d
      !--------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'ice_istate: sea-ice initialization '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~'

      !---------------------------
      ! 1) 1st init. of the fields
      !---------------------------
      !
      ! basal temperature (considered at freezing point)   [Kelvin]
      CALL eos_fzp( sss_m(:,:), t_bo(:,:) )
      t_bo(:,:) = ( t_bo(:,:) + rt0 ) * tmask(:,:,1) 
      !
      ! surface temperature and conductivity
      DO jl = 1, jpl
         t_su   (:,:,jl) = rt0 * tmask(:,:,1)  ! temp at the surface
         cnd_ice(:,:,jl) = 0._wp               ! initialisation of the effective conductivity at the top of ice/snow (ln_cndflx=T)
      END DO
      !
      ! ice and snw temperatures
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            t_i(:,:,jk,jl) = rt0 * tmask(:,:,1)
         END DO
         DO jk = 1, nlay_s
            t_s(:,:,jk,jl) = rt0 * tmask(:,:,1)
         END DO
      END DO
      !
      ! specific temperatures for coupled runs
      tn_ice (:,:,:) = t_i (:,:,1,:)
      t1_ice (:,:,:) = t_i (:,:,1,:)

      ! heat contents
      e_i (:,:,:,:) = 0._wp
      e_s (:,:,:,:) = 0._wp
      
      ! general fields
      a_i (:,:,:) = 0._wp
      v_i (:,:,:) = 0._wp
      v_s (:,:,:) = 0._wp
      sv_i(:,:,:) = 0._wp
      oa_i(:,:,:) = 0._wp
      !
      h_i (:,:,:) = 0._wp
      h_s (:,:,:) = 0._wp
      s_i (:,:,:) = 0._wp
      o_i (:,:,:) = 0._wp
      !
      ! melt ponds
      a_ip     (:,:,:) = 0._wp
      v_ip     (:,:,:) = 0._wp
      v_il     (:,:,:) = 0._wp
      a_ip_eff (:,:,:) = 0._wp
      h_ip     (:,:,:) = 0._wp
      h_il     (:,:,:) = 0._wp
      !
      ! ice velocities
      u_ice (:,:) = 0._wp
      v_ice (:,:) = 0._wp
      !
      !------------------------------------------------------------------------
      ! 2) overwrite some of the fields with namelist parameters or netcdf file
      !------------------------------------------------------------------------
      IF( ln_iceini ) THEN
         !                             !---------------!
         IF( nn_iceini_file == 1 )THEN ! Read a file   !
            !                          !---------------!
            WHERE( ff_t(:,:) >= 0._wp )   ;   zswitch(:,:) = 1._wp
            ELSEWHERE                     ;   zswitch(:,:) = 0._wp
            END WHERE
            !
            CALL fld_read( kt, 1, si ) ! input fields provided at the current time-step
            !
            ! -- mandatory fields -- !
            zht_i_ini(:,:) = si(jp_hti)%fnow(:,:,1) * tmask(:,:,1)
            zht_s_ini(:,:) = si(jp_hts)%fnow(:,:,1) * tmask(:,:,1)
            zat_i_ini(:,:) = si(jp_ati)%fnow(:,:,1) * tmask(:,:,1)

            ! -- optional fields -- !
            !    if fields do not exist then set them to the values present in the namelist (except for temperatures)
            !
            ! ice salinity
            IF( TRIM(si(jp_smi)%clrootname) == 'NOT USED' ) &
               &     si(jp_smi)%fnow(:,:,1) = ( rn_smi_ini_n * zswitch + rn_smi_ini_s * (1._wp - zswitch) ) * tmask(:,:,1)
            !
            ! temperatures
            IF    ( TRIM(si(jp_tmi)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tsu)%clrootname) == 'NOT USED' .AND. &
               &    TRIM(si(jp_tms)%clrootname) == 'NOT USED' ) THEN
               si(jp_tmi)%fnow(:,:,1) = ( rn_tmi_ini_n * zswitch + rn_tmi_ini_s * (1._wp - zswitch) ) * tmask(:,:,1)
               si(jp_tsu)%fnow(:,:,1) = ( rn_tsu_ini_n * zswitch + rn_tsu_ini_s * (1._wp - zswitch) ) * tmask(:,:,1)
               si(jp_tms)%fnow(:,:,1) = ( rn_tms_ini_n * zswitch + rn_tms_ini_s * (1._wp - zswitch) ) * tmask(:,:,1)
            ENDIF
            IF( TRIM(si(jp_tmi)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tms)%clrootname) /= 'NOT USED' ) & ! if T_s is read and not T_i, set T_i = (T_s + T_freeze)/2
               &     si(jp_tmi)%fnow(:,:,1) = 0.5_wp * ( si(jp_tms)%fnow(:,:,1) + 271.15 )
            IF( TRIM(si(jp_tmi)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tsu)%clrootname) /= 'NOT USED' ) & ! if T_su is read and not T_i, set T_i = (T_su + T_freeze)/2
               &     si(jp_tmi)%fnow(:,:,1) = 0.5_wp * ( si(jp_tsu)%fnow(:,:,1) + 271.15 )
            IF( TRIM(si(jp_tsu)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tms)%clrootname) /= 'NOT USED' ) & ! if T_s is read and not T_su, set T_su = T_s
               &     si(jp_tsu)%fnow(:,:,1) = si(jp_tms)%fnow(:,:,1)
            IF( TRIM(si(jp_tsu)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tmi)%clrootname) /= 'NOT USED' ) & ! if T_i is read and not T_su, set T_su = T_i
               &     si(jp_tsu)%fnow(:,:,1) = si(jp_tmi)%fnow(:,:,1)
            IF( TRIM(si(jp_tms)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tsu)%clrootname) /= 'NOT USED' ) & ! if T_su is read and not T_s, set T_s = T_su
               &     si(jp_tms)%fnow(:,:,1) = si(jp_tsu)%fnow(:,:,1)
            IF( TRIM(si(jp_tms)%clrootname) == 'NOT USED' .AND. TRIM(si(jp_tmi)%clrootname) /= 'NOT USED' ) & ! if T_i is read and not T_s, set T_s = T_i
               &     si(jp_tms)%fnow(:,:,1) = si(jp_tmi)%fnow(:,:,1)
            !
            ! pond concentration
            IF( TRIM(si(jp_apd)%clrootname) == 'NOT USED' ) &
               &     si(jp_apd)%fnow(:,:,1) = ( rn_apd_ini_n * zswitch + rn_apd_ini_s * (1._wp - zswitch) ) * tmask(:,:,1) & ! rn_apd = pond fraction => rn_apnd * a_i = pond conc.
               &                              * si(jp_ati)%fnow(:,:,1) 
            !
            ! pond depth
            IF( TRIM(si(jp_hpd)%clrootname) == 'NOT USED' ) &
               &     si(jp_hpd)%fnow(:,:,1) = ( rn_hpd_ini_n * zswitch + rn_hpd_ini_s * (1._wp - zswitch) ) * tmask(:,:,1)
            !
            ! pond lid depth
            IF( TRIM(si(jp_hld)%clrootname) == 'NOT USED' ) &
               &     si(jp_hld)%fnow(:,:,1) = ( rn_hld_ini_n * zswitch + rn_hld_ini_s * (1._wp - zswitch) ) * tmask(:,:,1)
            !
            zsm_i_ini(:,:) = si(jp_smi)%fnow(:,:,1) * tmask(:,:,1)
            ztm_i_ini(:,:) = si(jp_tmi)%fnow(:,:,1) * tmask(:,:,1)
            zt_su_ini(:,:) = si(jp_tsu)%fnow(:,:,1) * tmask(:,:,1)
            ztm_s_ini(:,:) = si(jp_tms)%fnow(:,:,1) * tmask(:,:,1)
            zapnd_ini(:,:) = si(jp_apd)%fnow(:,:,1) * tmask(:,:,1)
            zhpnd_ini(:,:) = si(jp_hpd)%fnow(:,:,1) * tmask(:,:,1)
            zhlid_ini(:,:) = si(jp_hld)%fnow(:,:,1) * tmask(:,:,1)
            !
            ! change the switch for the following
            WHERE( zat_i_ini(:,:) > 0._wp )   ;   zswitch(:,:) = tmask(:,:,1) 
            ELSEWHERE                         ;   zswitch(:,:) = 0._wp
            END WHERE
            !                          !---------------!
         ELSE                          ! Read namelist !
            !                          !---------------!
            ! no ice if (sst - Tfreez) >= thresold
            WHERE( ( sst_m(:,:) - (t_bo(:,:) - rt0) ) * tmask(:,:,1) >= rn_thres_sst )   ;   zswitch(:,:) = 0._wp 
            ELSEWHERE                                                                    ;   zswitch(:,:) = tmask(:,:,1)
            END WHERE
            !
            ! assign initial thickness, concentration, snow depth and salinity to an hemisphere-dependent array
            WHERE( ff_t(:,:) >= 0._wp )
               zht_i_ini(:,:) = rn_hti_ini_n * zswitch(:,:)
               zht_s_ini(:,:) = rn_hts_ini_n * zswitch(:,:)
               zat_i_ini(:,:) = rn_ati_ini_n * zswitch(:,:)
               zsm_i_ini(:,:) = rn_smi_ini_n * zswitch(:,:)
               ztm_i_ini(:,:) = rn_tmi_ini_n * zswitch(:,:)
               zt_su_ini(:,:) = rn_tsu_ini_n * zswitch(:,:)
               ztm_s_ini(:,:) = rn_tms_ini_n * zswitch(:,:)
               zapnd_ini(:,:) = rn_apd_ini_n * zswitch(:,:) * zat_i_ini(:,:) ! rn_apd = pond fraction => rn_apd * a_i = pond conc. 
               zhpnd_ini(:,:) = rn_hpd_ini_n * zswitch(:,:)
               zhlid_ini(:,:) = rn_hld_ini_n * zswitch(:,:)
            ELSEWHERE
               zht_i_ini(:,:) = rn_hti_ini_s * zswitch(:,:)
               zht_s_ini(:,:) = rn_hts_ini_s * zswitch(:,:)
               zat_i_ini(:,:) = rn_ati_ini_s * zswitch(:,:)
               zsm_i_ini(:,:) = rn_smi_ini_s * zswitch(:,:)
               ztm_i_ini(:,:) = rn_tmi_ini_s * zswitch(:,:)
               zt_su_ini(:,:) = rn_tsu_ini_s * zswitch(:,:)
               ztm_s_ini(:,:) = rn_tms_ini_s * zswitch(:,:)
               zapnd_ini(:,:) = rn_apd_ini_s * zswitch(:,:) * zat_i_ini(:,:) ! rn_apd = pond fraction => rn_apd * a_i = pond conc.
               zhpnd_ini(:,:) = rn_hpd_ini_s * zswitch(:,:)
               zhlid_ini(:,:) = rn_hld_ini_s * zswitch(:,:)
            END WHERE
            !
         ENDIF

         ! make sure ponds = 0 if no ponds scheme
         IF ( .NOT.ln_pnd ) THEN
            zapnd_ini(:,:) = 0._wp
            zhpnd_ini(:,:) = 0._wp
            zhlid_ini(:,:) = 0._wp
         ENDIF

         IF ( .NOT.ln_pnd_lids ) THEN
            zhlid_ini(:,:) = 0._wp
         ENDIF
         
         !----------------!
         ! 3) fill fields !
         !----------------!
         ! select ice covered grid points
         npti = 0 ; nptidx(:) = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( zht_i_ini(ji,jj) > 0._wp ) THEN
                  npti         = npti  + 1
                  nptidx(npti) = (jj - 1) * jpi + ji
               ENDIF
            END DO
         END DO

         ! move to 1D arrays: (jpi,jpj) -> (jpi*jpj)
         CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d (1:npti)  , zht_i_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), h_s_1d (1:npti)  , zht_s_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), at_i_1d(1:npti)  , zat_i_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), t_i_1d (1:npti,1), ztm_i_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), t_s_1d (1:npti,1), ztm_s_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), t_su_1d(1:npti)  , zt_su_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), s_i_1d (1:npti)  , zsm_i_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), a_ip_1d(1:npti)  , zapnd_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), h_ip_1d(1:npti)  , zhpnd_ini )
         CALL tab_2d_1d( npti, nptidx(1:npti), h_il_1d(1:npti)  , zhlid_ini )

         ! allocate temporary arrays
         ALLOCATE( zhi_2d (npti,jpl), zhs_2d (npti,jpl), zai_2d (npti,jpl), &
            &      zti_2d (npti,jpl), zts_2d (npti,jpl), ztsu_2d(npti,jpl), zsi_2d(npti,jpl), &
            &      zaip_2d(npti,jpl), zhip_2d(npti,jpl), zhil_2d(npti,jpl) )
         
         ! distribute 1-cat into jpl-cat: (jpi*jpj) -> (jpi*jpj,jpl)
         CALL ice_var_itd( h_i_1d(1:npti)  , h_s_1d(1:npti)  , at_i_1d(1:npti),                  &
            &              zhi_2d          , zhs_2d          , zai_2d         ,                  &
            &              t_i_1d(1:npti,1), t_s_1d(1:npti,1), t_su_1d(1:npti),                  &
            &              s_i_1d(1:npti)  , a_ip_1d(1:npti) , h_ip_1d(1:npti), h_il_1d(1:npti), &
            &              zti_2d          , zts_2d          , ztsu_2d        ,                  &
            &              zsi_2d          , zaip_2d         , zhip_2d        , zhil_2d )

         ! move to 3D arrays: (jpi*jpj,jpl) -> (jpi,jpj,jpl)
         DO jl = 1, jpl
            zti_3d(:,:,jl) = rt0 * tmask(:,:,1)
            zts_3d(:,:,jl) = rt0 * tmask(:,:,1)
         END DO
         CALL tab_2d_3d( npti, nptidx(1:npti), zhi_2d   , h_i    )
         CALL tab_2d_3d( npti, nptidx(1:npti), zhs_2d   , h_s    )
         CALL tab_2d_3d( npti, nptidx(1:npti), zai_2d   , a_i    )
         CALL tab_2d_3d( npti, nptidx(1:npti), zti_2d   , zti_3d )
         CALL tab_2d_3d( npti, nptidx(1:npti), zts_2d   , zts_3d )
         CALL tab_2d_3d( npti, nptidx(1:npti), ztsu_2d  , t_su   )
         CALL tab_2d_3d( npti, nptidx(1:npti), zsi_2d   , s_i    )
         CALL tab_2d_3d( npti, nptidx(1:npti), zaip_2d  , a_ip   )
         CALL tab_2d_3d( npti, nptidx(1:npti), zhip_2d  , h_ip   )
         CALL tab_2d_3d( npti, nptidx(1:npti), zhil_2d  , h_il   )

         ! deallocate temporary arrays
         DEALLOCATE( zhi_2d, zhs_2d, zai_2d , &
            &        zti_2d, zts_2d, ztsu_2d, zsi_2d, zaip_2d, zhip_2d, zhil_2d )

         ! calculate extensive and intensive variables
         CALL ice_var_salprof ! for sz_i
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  v_i (ji,jj,jl) = h_i(ji,jj,jl) * a_i(ji,jj,jl)
                  v_s (ji,jj,jl) = h_s(ji,jj,jl) * a_i(ji,jj,jl)
                  sv_i(ji,jj,jl) = MIN( MAX( rn_simin , s_i(ji,jj,jl) ) , rn_simax ) * v_i(ji,jj,jl)
               END DO
            END DO
         END DO
         !
         DO jl = 1, jpl
            DO jk = 1, nlay_s
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     t_s(ji,jj,jk,jl) = zts_3d(ji,jj,jl)
                     e_s(ji,jj,jk,jl) = zswitch(ji,jj) * v_s(ji,jj,jl) * r1_nlay_s * &
                        &               rhos * ( rcpi * ( rt0 - t_s(ji,jj,jk,jl) ) + rLfus )
                  END DO
               END DO
            END DO
         END DO
         !
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     t_i (ji,jj,jk,jl) = zti_3d(ji,jj,jl) 
                     ztmelts          = - rTmlt * sz_i(ji,jj,jk,jl) + rt0 ! melting temperature in K
                     e_i(ji,jj,jk,jl) = zswitch(ji,jj) * v_i(ji,jj,jl) * r1_nlay_i * &
                        &               rhoi * (  rcpi  * ( ztmelts - t_i(ji,jj,jk,jl) ) + &
                        &                         rLfus * ( 1._wp - (ztmelts-rt0) / MIN( (t_i(ji,jj,jk,jl)-rt0), -epsi20 ) ) &
                        &                       - rcp   * ( ztmelts - rt0 ) )
                  END DO
               END DO
            END DO
         END DO

         ! Melt ponds
         WHERE( a_i > epsi10 )   ;   a_ip_eff(:,:,:) = a_ip(:,:,:) / a_i(:,:,:)
         ELSEWHERE               ;   a_ip_eff(:,:,:) = 0._wp
         END WHERE
         v_ip(:,:,:) = h_ip(:,:,:) * a_ip(:,:,:)
         v_il(:,:,:) = h_il(:,:,:) * a_ip(:,:,:)
          
         ! specific temperatures for coupled runs
         tn_ice(:,:,:) = t_su(:,:,:)
         t1_ice(:,:,:) = t_i (:,:,1,:)
         !
         ! ice concentration should not exceed amax
         at_i(:,:) = SUM( a_i, dim=3 )
         DO jl = 1, jpl
            WHERE( at_i(:,:) > rn_amax_2d(:,:) )   a_i(:,:,jl) = a_i(:,:,jl) * rn_amax_2d(:,:) / at_i(:,:)
         END DO
         at_i(:,:) = SUM( a_i, dim=3 )
         !
      ENDIF ! ln_iceini
      !
      !----------------------------------------------
      ! 4) Snow-ice mass (case ice is fully embedded)
      !----------------------------------------------
      snwice_mass  (:,:) = tmask(:,:,1) * SUM( rhos * v_s(:,:,:) + rhoi * v_i(:,:,:), dim=3  )   ! snow+ice mass
      snwice_mass_b(:,:) = snwice_mass(:,:)
      !
      IF( ln_ice_embd ) THEN            ! embedded sea-ice: deplete the initial ssh below sea-ice area
         !
         sshn(:,:) = sshn(:,:) - snwice_mass(:,:) * r1_rau0
         sshb(:,:) = sshb(:,:) - snwice_mass(:,:) * r1_rau0
         !
         IF( .NOT.ln_linssh ) THEN
            !
            WHERE( ht_0(:,:) > 0 )   ;   z2d(:,:) = 1._wp + sshn(:,:)*tmask(:,:,1) / ht_0(:,:)
            ELSEWHERE                ;   z2d(:,:) = 1._wp   ;   END WHERE
            !
            DO jk = 1,jpkm1                     ! adjust initial vertical scale factors                
               e3t_n(:,:,jk) = e3t_0(:,:,jk) * z2d(:,:)
               e3t_b(:,:,jk) = e3t_n(:,:,jk)
               e3t_a(:,:,jk) = e3t_n(:,:,jk)
            END DO
            !
            ! Reconstruction of all vertical scale factors at now and before time-steps
            ! =========================================================================
            ! Horizontal scale factor interpolations
            ! --------------------------------------
            CALL dom_vvl_interpol( e3t_b(:,:,:), e3u_b(:,:,:), 'U' )
            CALL dom_vvl_interpol( e3t_b(:,:,:), e3v_b(:,:,:), 'V' )
            CALL dom_vvl_interpol( e3t_n(:,:,:), e3u_n(:,:,:), 'U' )
            CALL dom_vvl_interpol( e3t_n(:,:,:), e3v_n(:,:,:), 'V' )
            CALL dom_vvl_interpol( e3u_n(:,:,:), e3f_n(:,:,:), 'F' )
            ! Vertical scale factor interpolations
            ! ------------------------------------
            CALL dom_vvl_interpol( e3t_n(:,:,:), e3w_n (:,:,:), 'W'  )
            CALL dom_vvl_interpol( e3u_n(:,:,:), e3uw_n(:,:,:), 'UW' )
            CALL dom_vvl_interpol( e3v_n(:,:,:), e3vw_n(:,:,:), 'VW' )
            CALL dom_vvl_interpol( e3u_b(:,:,:), e3uw_b(:,:,:), 'UW' )
            CALL dom_vvl_interpol( e3v_b(:,:,:), e3vw_b(:,:,:), 'VW' )
            ! t- and w- points depth
            ! ----------------------
            !!gm not sure of that....
            gdept_n(:,:,1) = 0.5_wp * e3w_n(:,:,1)
            gdepw_n(:,:,1) = 0.0_wp
            gde3w_n(:,:,1) = gdept_n(:,:,1) - sshn(:,:)
            DO jk = 2, jpk
               gdept_n(:,:,jk) = gdept_n(:,:,jk-1) + e3w_n(:,:,jk  )
               gdepw_n(:,:,jk) = gdepw_n(:,:,jk-1) + e3t_n(:,:,jk-1)
               gde3w_n(:,:,jk) = gdept_n(:,:,jk  ) - sshn (:,:)
            END DO
         ENDIF
      ENDIF

!!clem: output of initial state should be written here but it is impossible because
!!      the ocean and ice are in the same file
!!      CALL dia_wri_state( 'output.init' )
      !
   END SUBROUTINE ice_istate


   SUBROUTINE ice_istate_init
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE ice_istate_init  ***
      !!        
      !! ** Purpose :   Definition of initial state of the ice 
      !!
      !! ** Method  :   Read the namini namelist and check the parameter 
      !!              values called at the first timestep (nit000)
      !!
      !! ** input   :  Namelist namini
      !!
      !!-----------------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer output status for namelist read
      INTEGER ::   ifpr, ierror
      !
      CHARACTER(len=256) ::  cn_dir          ! Root directory for location of ice files
      TYPE(FLD_N)                    ::   sn_hti, sn_hts, sn_ati, sn_smi, sn_tmi, sn_tsu, sn_tms, sn_apd, sn_hpd, sn_hld
      TYPE(FLD_N), DIMENSION(jpfldi) ::   slf_i                 ! array of namelist informations on the fields to read
      !
      NAMELIST/namini/ ln_iceini, nn_iceini_file, rn_thres_sst, &
         &             rn_hti_ini_n, rn_hti_ini_s, rn_hts_ini_n, rn_hts_ini_s, &
         &             rn_ati_ini_n, rn_ati_ini_s, rn_smi_ini_n, rn_smi_ini_s, &
         &             rn_tmi_ini_n, rn_tmi_ini_s, rn_tsu_ini_n, rn_tsu_ini_s, rn_tms_ini_n, rn_tms_ini_s, &
         &             rn_apd_ini_n, rn_apd_ini_s, rn_hpd_ini_n, rn_hpd_ini_s, rn_hld_ini_n, rn_hld_ini_s, &
         &             sn_hti, sn_hts, sn_ati, sn_tsu, sn_tmi, sn_smi, sn_tms, sn_apd, sn_hpd, sn_hld, cn_dir
      !!-----------------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namini in reference namelist : Ice initial state
      READ  ( numnam_ice_ref, namini, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namini in reference namelist' )
      REWIND( numnam_ice_cfg )              ! Namelist namini in configuration namelist : Ice initial state
      READ  ( numnam_ice_cfg, namini, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namini in configuration namelist' )
      IF(lwm) WRITE ( numoni, namini )
      !
      slf_i(jp_hti) = sn_hti  ;  slf_i(jp_hts) = sn_hts
      slf_i(jp_ati) = sn_ati  ;  slf_i(jp_smi) = sn_smi
      slf_i(jp_tmi) = sn_tmi  ;  slf_i(jp_tsu) = sn_tsu   ;   slf_i(jp_tms) = sn_tms
      slf_i(jp_apd) = sn_apd  ;  slf_i(jp_hpd) = sn_hpd   ;   slf_i(jp_hld) = sn_hld
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_istate_init: ice parameters inititialisation '
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namini:'
         WRITE(numout,*) '      ice initialization (T) or not (F)                ln_iceini      = ', ln_iceini
         WRITE(numout,*) '      ice initialization from a netcdf file            nn_iceini_file = ', nn_iceini_file
         WRITE(numout,*) '      max ocean temp. above Tfreeze with initial ice   rn_thres_sst   = ', rn_thres_sst
         IF( ln_iceini .AND. nn_iceini_file == 0 ) THEN
            WRITE(numout,*) '      initial snw thickness in the north-south         rn_hts_ini     = ', rn_hts_ini_n,rn_hts_ini_s 
            WRITE(numout,*) '      initial ice thickness in the north-south         rn_hti_ini     = ', rn_hti_ini_n,rn_hti_ini_s
            WRITE(numout,*) '      initial ice concentr  in the north-south         rn_ati_ini     = ', rn_ati_ini_n,rn_ati_ini_s
            WRITE(numout,*) '      initial ice salinity  in the north-south         rn_smi_ini     = ', rn_smi_ini_n,rn_smi_ini_s
            WRITE(numout,*) '      initial surf temperat in the north-south         rn_tsu_ini     = ', rn_tsu_ini_n,rn_tsu_ini_s
            WRITE(numout,*) '      initial ice temperat  in the north-south         rn_tmi_ini     = ', rn_tmi_ini_n,rn_tmi_ini_s
            WRITE(numout,*) '      initial snw temperat  in the north-south         rn_tms_ini     = ', rn_tms_ini_n,rn_tms_ini_s
            WRITE(numout,*) '      initial pnd fraction  in the north-south         rn_apd_ini     = ', rn_apd_ini_n,rn_apd_ini_s
            WRITE(numout,*) '      initial pnd depth     in the north-south         rn_hpd_ini     = ', rn_hpd_ini_n,rn_hpd_ini_s
            WRITE(numout,*) '      initial pnd lid depth in the north-south         rn_hld_ini     = ', rn_hld_ini_n,rn_hld_ini_s
         ENDIF
      ENDIF
      !
      IF( nn_iceini_file == 1 ) THEN                      ! Ice initialization using input file
         !
         ! set si structure
         ALLOCATE( si(jpfldi), STAT=ierror )
         IF( ierror > 0 ) THEN
            CALL ctl_stop( 'ice_istate_ini in iceistate: unable to allocate si structure' )   ;   RETURN
         ENDIF
         !
         DO ifpr = 1, jpfldi
            ALLOCATE( si(ifpr)%fnow(jpi,jpj,1) )
            IF( slf_i(ifpr)%ln_tint )  ALLOCATE( si(ifpr)%fdta(jpi,jpj,1,2) )
         END DO
         !
         ! fill si with slf_i and control print
         CALL fld_fill( si, slf_i, cn_dir, 'ice_istate_ini', 'initialization of sea ice fields', 'numnam_ice' )
         !
      ENDIF
      !
      IF( .NOT.ln_pnd ) THEN
         rn_apd_ini_n = 0. ; rn_apd_ini_s = 0.
         rn_hpd_ini_n = 0. ; rn_hpd_ini_s = 0.
         rn_hld_ini_n = 0. ; rn_hld_ini_s = 0.
         CALL ctl_warn( 'rn_apd_ini & rn_hpd_ini = 0 & rn_hld_ini = 0 when no ponds' )
      ENDIF
      !
      IF( .NOT.ln_pnd_lids ) THEN
         rn_hld_ini_n = 0. ; rn_hld_ini_s = 0.
      ENDIF
      !
   END SUBROUTINE ice_istate_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE iceistate
