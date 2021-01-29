MODULE iceitd
   !!======================================================================
   !!                       ***  MODULE iceitd ***
   !!   sea-ice : ice thickness distribution
   !!======================================================================
   !! History :  3.0  !  2005-12  (M. Vancoppenolle) original code (based on CICE)
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_itd_rem   : redistribute ice thicknesses after thermo growth and melt
   !!   itd_glinear   : build g(h) satisfying area and volume constraints
   !!   itd_shiftice  : shift ice across category boundaries, conserving everything
   !!   ice_itd_reb   : rebin ice thicknesses into bounded categories
   !!   ice_itd_init  : read ice thicknesses mean and min from namelist
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean domain
   USE phycst         ! physical constants 
   USE ice1D          ! sea-ice: thermodynamic variables
   USE ice            ! sea-ice: variables
   USE icevar         ! sea-ice: operations
   USE icectl         ! sea-ice: conservation tests
   USE icetab         ! sea-ice: convert 1D<=>2D
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_itd_init  ! called in icestp
   PUBLIC   ice_itd_rem   ! called in icethd
   PUBLIC   ice_itd_reb   ! called in icecor

   INTEGER            ::   nice_catbnd     ! choice of the type of ice category function
   !                                       ! associated indices:
   INTEGER, PARAMETER ::   np_cathfn = 1   ! categories defined by a function
   INTEGER, PARAMETER ::   np_catusr = 2   ! categories defined by the user
   !
   !                                           !! ** namelist (namitd) **
   LOGICAL                    ::   ln_cat_hfn   ! ice categories are defined by function like rn_himean**(-0.05)
   REAL(wp)                   ::   rn_himean    ! mean thickness of the domain
   LOGICAL                    ::   ln_cat_usr   ! ice categories are defined by rn_catbnd
   REAL(wp), DIMENSION(0:100) ::   rn_catbnd    ! ice categories bounds
! tipaccs limit ice growth if constant supply of supercooled water
   REAL(wp), PUBLIC           ::   rn_himax     ! maximum ice thickness allowed
! end tipaccs limit ice growth if constant supply of supercooled water
   !
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: iceitd.F90 13617 2020-10-16 08:07:20Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_itd_rem( kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_itd_rem ***
      !!
      !! ** Purpose :   computes the redistribution of ice thickness
      !!                after thermodynamic growth of ice thickness
      !!
      !! ** Method  :   Linear remapping 
      !!
      !! References :   W.H. Lipscomb, JGR 2001
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   kt      ! Ocean time step 
      !
      INTEGER  ::   ji, jj, jl, jcat     ! dummy loop index
      INTEGER  ::   ipti                 ! local integer
      REAL(wp) ::   zx1, zwk1, zdh0, zetamin, zdamax   ! local scalars
      REAL(wp) ::   zx2, zwk2, zda0, zetamax           !   -      -
      REAL(wp) ::   zx3        
      REAL(wp) ::   zslope          ! used to compute local thermodynamic "speeds"
      !
      INTEGER , DIMENSION(jpij)       ::   iptidx          ! compute remapping or not
      INTEGER , DIMENSION(jpij,jpl-1) ::   jdonor          ! donor category index
      REAL(wp), DIMENSION(jpij,jpl)   ::   zdhice          ! ice thickness increment
      REAL(wp), DIMENSION(jpij,jpl)   ::   g0, g1          ! coefficients for fitting the line of the ITD
      REAL(wp), DIMENSION(jpij,jpl)   ::   hL, hR          ! left and right boundary for the ITD for each thickness
      REAL(wp), DIMENSION(jpij,jpl-1) ::   zdaice, zdvice  ! local increment of ice area and volume
      REAL(wp), DIMENSION(jpij)       ::   zhb0, zhb1      ! category boundaries for thinnes categories
      REAL(wp), DIMENSION(jpij,0:jpl) ::   zhbnew          ! new boundaries of ice categories
      !!------------------------------------------------------------------

      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_itd_rem: remapping ice thickness distribution' 

      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'iceitd_rem', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (0, 'iceitd_rem',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)

      !-----------------------------------------------------------------------------------------------
      !  1) Identify grid cells with ice
      !-----------------------------------------------------------------------------------------------
      at_i(:,:) = SUM( a_i, dim=3 )
      !
      npti = 0   ;   nptidx(:) = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( at_i(ji,jj) > epsi10 ) THEN
               npti = npti + 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
         END DO
      END DO
      
      !-----------------------------------------------------------------------------------------------
      !  2) Compute new category boundaries
      !-----------------------------------------------------------------------------------------------
      IF( npti > 0 ) THEN
         !
         zdhice(:,:) = 0._wp
         zhbnew(:,:) = 0._wp
         !
         CALL tab_3d_2d( npti, nptidx(1:npti), h_i_2d (1:npti,1:jpl), h_i   )
         CALL tab_3d_2d( npti, nptidx(1:npti), h_ib_2d(1:npti,1:jpl), h_i_b )
         CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d (1:npti,1:jpl), a_i   )
         CALL tab_3d_2d( npti, nptidx(1:npti), a_ib_2d(1:npti,1:jpl), a_i_b )
         !
         DO jl = 1, jpl
            ! Compute thickness change in each ice category
            DO ji = 1, npti
               IF( a_i_2d(ji,jl) > epsi10 )   zdhice(ji,jl) = h_i_2d(ji,jl) - h_ib_2d(ji,jl)
            END DO
         END DO
         !
         ! --- New boundaries for category 1:jpl-1 --- !
         DO jl = 1, jpl - 1
            !
            DO ji = 1, npti
               !
               ! --- New boundary: Hn* = Hn + Fn*dt --- !
               !     Fn*dt = ( fn + (fn+1 - fn)/(hn+1 - hn) * (Hn - hn) ) * dt = zdhice + zslope * (Hmax - h_i_b)
               !
               IF    ( a_ib_2d(ji,jl) >  epsi10 .AND. a_ib_2d(ji,jl+1) >  epsi10 ) THEN   ! a(jl+1) & a(jl) /= 0
                  zslope        = ( zdhice(ji,jl+1) - zdhice(ji,jl) ) / ( h_ib_2d(ji,jl+1) - h_ib_2d(ji,jl) )
                  zhbnew(ji,jl) = hi_max(jl) + zdhice(ji,jl) + zslope * ( hi_max(jl) - h_ib_2d(ji,jl) )
               ELSEIF( a_ib_2d(ji,jl) >  epsi10 .AND. a_ib_2d(ji,jl+1) <= epsi10 ) THEN   ! a(jl+1)=0 => Hn* = Hn + fn*dt
                  zhbnew(ji,jl) = hi_max(jl) + zdhice(ji,jl)
               ELSEIF( a_ib_2d(ji,jl) <= epsi10 .AND. a_ib_2d(ji,jl+1) >  epsi10 ) THEN   ! a(jl)=0 => Hn* = Hn + fn+1*dt
                  zhbnew(ji,jl) = hi_max(jl) + zdhice(ji,jl+1)
               ELSE                                                                       ! a(jl+1) & a(jl) = 0 
                  zhbnew(ji,jl) = hi_max(jl)
               ENDIF
               !
               ! --- 2 conditions for remapping --- !
               ! 1) hn(t+1)+espi < Hn* < hn+1(t+1)-epsi               
               !    Note: hn(t+1) must not be too close to either HR or HL otherwise a division by nearly 0 is possible 
               !          in itd_glinear in the case (HR-HL) = 3(Hice - HL) or = 3(HR - Hice)
               IF( a_i_2d(ji,jl  ) > epsi10 .AND. h_i_2d(ji,jl  ) > ( zhbnew(ji,jl) - epsi10 ) )   nptidx(ji) = 0
               IF( a_i_2d(ji,jl+1) > epsi10 .AND. h_i_2d(ji,jl+1) < ( zhbnew(ji,jl) + epsi10 ) )   nptidx(ji) = 0
               !
               ! 2) Hn-1 < Hn* < Hn+1  
               IF( zhbnew(ji,jl) < hi_max(jl-1) )   nptidx(ji) = 0
               IF( zhbnew(ji,jl) > hi_max(jl+1) )   nptidx(ji) = 0
               !
            END DO
         END DO
         !
         ! --- New boundaries for category jpl --- !
         DO ji = 1, npti
            IF( a_i_2d(ji,jpl) > epsi10 ) THEN
               zhbnew(ji,jpl) = MAX( hi_max(jpl-1), 3._wp * h_i_2d(ji,jpl) - 2._wp * zhbnew(ji,jpl-1) )
            ELSE
               zhbnew(ji,jpl) = hi_max(jpl)  
            ENDIF
            !
            ! --- 1 additional condition for remapping (1st category) --- !
            ! H0+epsi < h1(t) < H1-epsi 
            !    h1(t) must not be too close to either HR or HL otherwise a division by nearly 0 is possible 
            !    in itd_glinear in the case (HR-HL) = 3(Hice - HL) or = 3(HR - Hice)
            IF( h_ib_2d(ji,1) < ( hi_max(0) + epsi10 ) )   nptidx(ji) = 0
            IF( h_ib_2d(ji,1) > ( hi_max(1) - epsi10 ) )   nptidx(ji) = 0
         END DO
         !
         !-----------------------------------------------------------------------------------------------
         !  3) Identify cells where remapping
         !-----------------------------------------------------------------------------------------------
         ipti = 0   ;   iptidx(:) = 0
         DO ji = 1, npti
            IF( nptidx(ji) /= 0 ) THEN
               ipti = ipti + 1
               iptidx(ipti)   = nptidx(ji)
               zhbnew(ipti,:) = zhbnew(ji,:)  ! adjust zhbnew to new indices
            ENDIF
         END DO
         nptidx(:) = iptidx(:)
         npti      = ipti
         !
      ENDIF
   
      !-----------------------------------------------------------------------------------------------
      !  4) Compute g(h) 
      !-----------------------------------------------------------------------------------------------
      IF( npti > 0 ) THEN
         !
         zhb0(:) = hi_max(0)   ;   zhb1(:) = hi_max(1)
         g0(:,:) = 0._wp       ;   g1(:,:) = 0._wp 
         hL(:,:) = 0._wp       ;   hR(:,:) = 0._wp 
         !
         DO jl = 1, jpl
            !
            CALL tab_2d_1d( npti, nptidx(1:npti), h_ib_1d(1:npti), h_i_b(:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d (1:npti), h_i  (:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d (1:npti), a_i  (:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), v_i_1d (1:npti), v_i  (:,:,jl) )
            !
            IF( jl == 1 ) THEN
               !  
               ! --- g(h) for category 1 --- !
               CALL itd_glinear( zhb0(1:npti)  , zhb1(1:npti)  , h_ib_1d(1:npti)  , a_i_1d(1:npti)  ,  &   ! in
                  &              g0  (1:npti,1), g1  (1:npti,1), hL     (1:npti,1), hR    (1:npti,1)   )   ! out
               !
               ! Area lost due to melting of thin ice
               DO ji = 1, npti
                  !
                  IF( a_i_1d(ji) > epsi10 ) THEN
                     !
                     zdh0 =  h_i_1d(ji) - h_ib_1d(ji)                
                     IF( zdh0 < 0.0 ) THEN      ! remove area from category 1
                        zdh0 = MIN( -zdh0, hi_max(1) )
                        !Integrate g(1) from 0 to dh0 to estimate area melted
                        zetamax = MIN( zdh0, hR(ji,1) ) - hL(ji,1)
                        !
                        IF( zetamax > 0.0 ) THEN
                           zx1    = zetamax
                           zx2    = 0.5 * zetamax * zetamax 
                           zda0   = g1(ji,1) * zx2 + g0(ji,1) * zx1                ! ice area removed
                           zdamax = a_i_1d(ji) * (1.0 - h_i_1d(ji) / h_ib_1d(ji) ) ! Constrain new thickness <= h_i                
                           zda0   = MIN( zda0, zdamax )                            ! ice area lost due to melting of thin ice (zdamax > 0)
                           ! Remove area, conserving volume
                           h_i_1d(ji) = h_i_1d(ji) * a_i_1d(ji) / ( a_i_1d(ji) - zda0 )
                           a_i_1d(ji) = a_i_1d(ji) - zda0
                           v_i_1d(ji) = a_i_1d(ji) * h_i_1d(ji) ! useless ?
                        ENDIF
                        !
                     ELSE ! if ice accretion zdh0 > 0
                        ! zhbnew was 0, and is shifted to the right to account for thin ice growth in openwater (F0 = f1)
                        zhbnew(ji,0) = MIN( zdh0, hi_max(1) ) 
                     ENDIF
                     !
                  ENDIF
                  !
               END DO
               !
               CALL tab_1d_2d( npti, nptidx(1:npti), h_i_1d(1:npti), h_i(:,:,jl) )
               CALL tab_1d_2d( npti, nptidx(1:npti), a_i_1d(1:npti), a_i(:,:,jl) )
               CALL tab_1d_2d( npti, nptidx(1:npti), v_i_1d(1:npti), v_i(:,:,jl) )
               !
            ENDIF ! jl=1
            !
            ! --- g(h) for each thickness category --- !  
            CALL itd_glinear( zhbnew(1:npti,jl-1), zhbnew(1:npti,jl), h_i_1d(1:npti)   , a_i_1d(1:npti)   ,  &   ! in
               &              g0    (1:npti,jl  ), g1    (1:npti,jl), hL    (1:npti,jl), hR    (1:npti,jl)   )   ! out
            !
         END DO
         
         !-----------------------------------------------------------------------------------------------
         !  5) Compute area and volume to be shifted across each boundary (Eq. 18)
         !-----------------------------------------------------------------------------------------------
         DO jl = 1, jpl - 1
            !
            DO ji = 1, npti
               !
               ! left and right integration limits in eta space
               IF (zhbnew(ji,jl) > hi_max(jl)) THEN ! Hn* > Hn => transfer from jl to jl+1
                  zetamin = MAX( hi_max(jl)   , hL(ji,jl) ) - hL(ji,jl)   ! hi_max(jl) - hL 
                  zetamax = MIN( zhbnew(ji,jl), hR(ji,jl) ) - hL(ji,jl)   ! hR - hL
                  jdonor(ji,jl) = jl
               ELSE                                 ! Hn* <= Hn => transfer from jl+1 to jl
                  zetamin = 0.0
                  zetamax = MIN( hi_max(jl), hR(ji,jl+1) ) - hL(ji,jl+1)  ! hi_max(jl) - hL
                  jdonor(ji,jl) = jl + 1
               ENDIF
               zetamax = MAX( zetamax, zetamin ) ! no transfer if etamax < etamin
               !
               zx1  = zetamax - zetamin
               zwk1 = zetamin * zetamin
               zwk2 = zetamax * zetamax
               zx2  = 0.5 * ( zwk2 - zwk1 )
               zwk1 = zwk1 * zetamin
               zwk2 = zwk2 * zetamax
               zx3  = 1.0 / 3.0 * ( zwk2 - zwk1 )
               jcat = jdonor(ji,jl)
               zdaice(ji,jl) = g1(ji,jcat)*zx2 + g0(ji,jcat)*zx1
               zdvice(ji,jl) = g1(ji,jcat)*zx3 + g0(ji,jcat)*zx2 + zdaice(ji,jl)*hL(ji,jcat)
               !
            END DO
         END DO
         
         !----------------------------------------------------------------------------------------------
         ! 6) Shift ice between categories
         !----------------------------------------------------------------------------------------------
         CALL itd_shiftice ( jdonor(1:npti,:), zdaice(1:npti,:), zdvice(1:npti,:) )
         
         !----------------------------------------------------------------------------------------------
         ! 7) Make sure h_i >= minimum ice thickness hi_min
         !----------------------------------------------------------------------------------------------
         CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d (1:npti), h_i (:,:,1) )
         CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d (1:npti), a_i (:,:,1) )
         CALL tab_2d_1d( npti, nptidx(1:npti), a_ip_1d(1:npti), a_ip(:,:,1) )
         !
         DO ji = 1, npti
            IF ( a_i_1d(ji) > epsi10 .AND. h_i_1d(ji) < rn_himin ) THEN
               a_i_1d(ji) = a_i_1d(ji) * h_i_1d(ji) / rn_himin 
               IF( ln_pnd_LEV )   a_ip_1d(ji) = a_ip_1d(ji) * h_i_1d(ji) / rn_himin
               h_i_1d(ji) = rn_himin
            ENDIF
         END DO
         !
         CALL tab_1d_2d( npti, nptidx(1:npti), h_i_1d (1:npti), h_i (:,:,1) )
         CALL tab_1d_2d( npti, nptidx(1:npti), a_i_1d (1:npti), a_i (:,:,1) )
         CALL tab_1d_2d( npti, nptidx(1:npti), a_ip_1d(1:npti), a_ip(:,:,1) )
         !
      ENDIF
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'iceitd_rem', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'iceitd_rem',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      !
   END SUBROUTINE ice_itd_rem


   SUBROUTINE itd_glinear( HbL, Hbr, phice, paice, pg0, pg1, phL, phR )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE itd_glinear ***
      !!
      !! ** Purpose :   build g(h) satisfying area and volume constraints (Eq. 6 and 9)
      !!
      !! ** Method  :   g(h) is linear and written as: g(eta) = g1(eta) + g0
      !!                with eta = h - HL
      !!------------------------------------------------------------------
      REAL(wp), DIMENSION(:), INTENT(in   ) ::   HbL, HbR      ! left and right category boundaries
      REAL(wp), DIMENSION(:), INTENT(in   ) ::   phice, paice  ! ice thickness and concentration
      REAL(wp), DIMENSION(:), INTENT(inout) ::   pg0, pg1      ! coefficients in linear equation for g(eta)
      REAL(wp), DIMENSION(:), INTENT(inout) ::   phL, phR      ! min and max value of range over which g(h) > 0
      !
      INTEGER  ::   ji           ! horizontal indices
      REAL(wp) ::   z1_3 , z2_3  ! 1/3 , 2/3
      REAL(wp) ::   zh13         ! HbL + 1/3 * (HbR - HbL)
      REAL(wp) ::   zh23         ! HbL + 2/3 * (HbR - HbL)
      REAL(wp) ::   zdhr         ! 1 / (hR - hL)
      REAL(wp) ::   zwk1, zwk2   ! temporary variables
      !!------------------------------------------------------------------
      !
      z1_3 = 1._wp / 3._wp
      z2_3 = 2._wp / 3._wp
      !
      DO ji = 1, npti
         !
         IF( paice(ji) > epsi10  .AND. phice(ji) > epsi10 )  THEN
            !
            ! Initialize hL and hR
            phL(ji) = HbL(ji)
            phR(ji) = HbR(ji)
            !
            ! Change hL or hR if hice falls outside central third of range,
            ! so that hice is in the central third of the range [HL HR]
            zh13 = z1_3 * ( 2._wp * phL(ji) +         phR(ji) )
            zh23 = z1_3 * (         phL(ji) + 2._wp * phR(ji) )
            !
            IF    ( phice(ji) < zh13 ) THEN   ;   phR(ji) = 3._wp * phice(ji) - 2._wp * phL(ji) ! move HR to the left
            ELSEIF( phice(ji) > zh23 ) THEN   ;   phL(ji) = 3._wp * phice(ji) - 2._wp * phR(ji) ! move HL to the right
            ENDIF
            !
            ! Compute coefficients of g(eta) = g0 + g1*eta
            zdhr = 1._wp / (phR(ji) - phL(ji))
            zwk1 = 6._wp * paice(ji) * zdhr
            zwk2 = ( phice(ji) - phL(ji) ) * zdhr
            pg0(ji) = zwk1 * ( z2_3 - zwk2 )                    ! Eq. 14
            pg1(ji) = 2._wp * zdhr * zwk1 * ( zwk2 - 0.5_wp )   ! Eq. 14
            !
         ELSE  ! remap_flag = .false. or a_i < epsi10 
            phL(ji) = 0._wp
            phR(ji) = 0._wp
            pg0(ji) = 0._wp
            pg1(ji) = 0._wp
         ENDIF
         !
      END DO
      !
   END SUBROUTINE itd_glinear


   SUBROUTINE itd_shiftice( kdonor, pdaice, pdvice )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE itd_shiftice ***
      !!
      !! ** Purpose :   shift ice across category boundaries, conserving everything
      !!              ( area, volume, energy, age*vol, and mass of salt )
      !!------------------------------------------------------------------
      INTEGER , DIMENSION(:,:), INTENT(in) ::   kdonor   ! donor category index
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pdaice   ! ice area transferred across boundary
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pdvice   ! ice volume transferred across boundary
      !
      INTEGER  ::   ji, jl, jk         ! dummy loop indices
      INTEGER  ::   jl2, jl1           ! local integers
      REAL(wp) ::   ztrans             ! ice/snow transferred
      REAL(wp), DIMENSION(jpij)            ::   zworka, zworkv   ! workspace
      REAL(wp), DIMENSION(jpij,jpl)        ::   zaTsfn           !  -    -
      REAL(wp), DIMENSION(jpij,nlay_i,jpl) ::   ze_i_2d
      REAL(wp), DIMENSION(jpij,nlay_s,jpl) ::   ze_s_2d
      !!------------------------------------------------------------------
         
      CALL tab_3d_2d( npti, nptidx(1:npti), h_i_2d (1:npti,1:jpl), h_i  )
      CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d (1:npti,1:jpl), a_i  )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_i_2d (1:npti,1:jpl), v_i  )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_s_2d (1:npti,1:jpl), v_s  )
      CALL tab_3d_2d( npti, nptidx(1:npti), oa_i_2d(1:npti,1:jpl), oa_i )
      CALL tab_3d_2d( npti, nptidx(1:npti), sv_i_2d(1:npti,1:jpl), sv_i )
      CALL tab_3d_2d( npti, nptidx(1:npti), a_ip_2d(1:npti,1:jpl), a_ip )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_ip_2d(1:npti,1:jpl), v_ip )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_il_2d(1:npti,1:jpl), v_il )
      CALL tab_3d_2d( npti, nptidx(1:npti), t_su_2d(1:npti,1:jpl), t_su )
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            CALL tab_2d_1d( npti, nptidx(1:npti), ze_s_2d(1:npti,jk,jl), e_s(:,:,jk,jl) )
         END DO
         DO jk = 1, nlay_i
            CALL tab_2d_1d( npti, nptidx(1:npti), ze_i_2d(1:npti,jk,jl), e_i(:,:,jk,jl) )
         END DO
      END DO
      ! to correct roundoff errors on a_i
      CALL tab_2d_1d( npti, nptidx(1:npti), rn_amax_1d(1:npti), rn_amax_2d )

      !----------------------------------------------------------------------------------------------
      ! 1) Define a variable equal to a_i*T_su
      !----------------------------------------------------------------------------------------------
      DO jl = 1, jpl
         DO ji = 1, npti
            zaTsfn(ji,jl) = a_i_2d(ji,jl) * t_su_2d(ji,jl)
         END DO
      END DO
      
      !-------------------------------------------------------------------------------
      ! 2) Transfer volume and energy between categories
      !-------------------------------------------------------------------------------
      DO jl = 1, jpl - 1
         DO ji = 1, npti
            !
            jl1 = kdonor(ji,jl)
            !
            IF( jl1 > 0 ) THEN
               !
               IF ( jl1 == jl  ) THEN   ;   jl2 = jl1+1
               ELSE                     ;   jl2 = jl 
               ENDIF
               !
               IF( v_i_2d(ji,jl1) >= epsi10 ) THEN   ;   zworkv(ji) = pdvice(ji,jl) / v_i_2d(ji,jl1)
               ELSE                                  ;   zworkv(ji) = 0._wp
               ENDIF
               IF( a_i_2d(ji,jl1) >= epsi10 ) THEN   ;   zworka(ji) = pdaice(ji,jl) / a_i_2d(ji,jl1)
               ELSE                                  ;   zworka(ji) = 0._wp
               ENDIF
               !
               a_i_2d(ji,jl1) = a_i_2d(ji,jl1) - pdaice(ji,jl)       ! Ice areas
               a_i_2d(ji,jl2) = a_i_2d(ji,jl2) + pdaice(ji,jl)
               !
               v_i_2d(ji,jl1) = v_i_2d(ji,jl1) - pdvice(ji,jl)       ! Ice volumes
               v_i_2d(ji,jl2) = v_i_2d(ji,jl2) + pdvice(ji,jl)
               !
               ztrans         = v_s_2d(ji,jl1) * zworkv(ji)          ! Snow volumes
               v_s_2d(ji,jl1) = v_s_2d(ji,jl1) - ztrans
               v_s_2d(ji,jl2) = v_s_2d(ji,jl2) + ztrans 
               !
               ztrans          = oa_i_2d(ji,jl1) * zworka(ji)        ! Ice age
               oa_i_2d(ji,jl1) = oa_i_2d(ji,jl1) - ztrans
               oa_i_2d(ji,jl2) = oa_i_2d(ji,jl2) + ztrans
               !
               ztrans          = sv_i_2d(ji,jl1) * zworkv(ji)        ! Ice salinity
               sv_i_2d(ji,jl1) = sv_i_2d(ji,jl1) - ztrans
               sv_i_2d(ji,jl2) = sv_i_2d(ji,jl2) + ztrans
               !
               ztrans          = zaTsfn(ji,jl1) * zworka(ji)         ! Surface temperature
               zaTsfn(ji,jl1)  = zaTsfn(ji,jl1) - ztrans
               zaTsfn(ji,jl2)  = zaTsfn(ji,jl2) + ztrans
               !  
               IF ( ln_pnd_LEV ) THEN
                  ztrans          = a_ip_2d(ji,jl1) * zworka(ji)     ! Pond fraction
                  a_ip_2d(ji,jl1) = a_ip_2d(ji,jl1) - ztrans
                  a_ip_2d(ji,jl2) = a_ip_2d(ji,jl2) + ztrans
                  !                                              
                  ztrans          = v_ip_2d(ji,jl1) * zworka(ji)     ! Pond volume (also proportional to da/a)
                  v_ip_2d(ji,jl1) = v_ip_2d(ji,jl1) - ztrans
                  v_ip_2d(ji,jl2) = v_ip_2d(ji,jl2) + ztrans
                  !
                  IF ( ln_pnd_lids ) THEN                            ! Pond lid volume
                     ztrans          = v_il_2d(ji,jl1) * zworka(ji)
                     v_il_2d(ji,jl1) = v_il_2d(ji,jl1) - ztrans
                     v_il_2d(ji,jl2) = v_il_2d(ji,jl2) + ztrans
                  ENDIF
               ENDIF
               !
            ENDIF   ! jl1 >0
         END DO
         !
         DO jk = 1, nlay_s         !--- Snow heat content
            DO ji = 1, npti
               !
               jl1 = kdonor(ji,jl)
               !
               IF( jl1 > 0 ) THEN
                  IF(jl1 == jl) THEN  ;  jl2 = jl+1
                  ELSE                ;  jl2 = jl
                  ENDIF
                  ztrans             = ze_s_2d(ji,jk,jl1) * zworkv(ji)
                  ze_s_2d(ji,jk,jl1) = ze_s_2d(ji,jk,jl1) - ztrans
                  ze_s_2d(ji,jk,jl2) = ze_s_2d(ji,jk,jl2) + ztrans
               ENDIF
            END DO
         END DO
         !
         DO jk = 1, nlay_i         !--- Ice heat content
            DO ji = 1, npti
               !
               jl1 = kdonor(ji,jl)
               !
               IF( jl1 > 0 ) THEN
                  IF(jl1 == jl) THEN  ;  jl2 = jl+1
                  ELSE                ;  jl2 = jl
                  ENDIF
                  ztrans             = ze_i_2d(ji,jk,jl1) * zworkv(ji)
                  ze_i_2d(ji,jk,jl1) = ze_i_2d(ji,jk,jl1) - ztrans
                  ze_i_2d(ji,jk,jl2) = ze_i_2d(ji,jk,jl2) + ztrans
               ENDIF
            END DO
         END DO
         !
      END DO                   ! boundaries, 1 to jpl-1

      !-------------------
      ! 3) roundoff errors
      !-------------------
      ! clem: The transfer between one category to another can lead to very small negative values (-1.e-20)
      !       because of truncation error ( i.e. 1. - 1. /= 0 )
      CALL ice_var_roundoff( a_i_2d, v_i_2d, v_s_2d, sv_i_2d, oa_i_2d, a_ip_2d, v_ip_2d, v_il_2d, ze_s_2d, ze_i_2d )

      ! at_i must be <= rn_amax
      zworka(1:npti) = SUM( a_i_2d(1:npti,:), dim=2 )
      DO jl  = 1, jpl
         WHERE( zworka(1:npti) > rn_amax_1d(1:npti) )   &
            &   a_i_2d(1:npti,jl) = a_i_2d(1:npti,jl) * rn_amax_1d(1:npti) / zworka(1:npti)
      END DO
      
      !-------------------------------------------------------------------------------
      ! 4) Update ice thickness and temperature
      !-------------------------------------------------------------------------------
      WHERE( a_i_2d(1:npti,:) >= epsi20 )
         h_i_2d (1:npti,:)  =  v_i_2d(1:npti,:) / a_i_2d(1:npti,:) 
         t_su_2d(1:npti,:)  =  zaTsfn(1:npti,:) / a_i_2d(1:npti,:) 
      ELSEWHERE
         h_i_2d (1:npti,:)  = 0._wp
         t_su_2d(1:npti,:)  = rt0
      END WHERE
      !
      CALL tab_2d_3d( npti, nptidx(1:npti), h_i_2d (1:npti,1:jpl), h_i  )
      CALL tab_2d_3d( npti, nptidx(1:npti), a_i_2d (1:npti,1:jpl), a_i  )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_i_2d (1:npti,1:jpl), v_i  )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_s_2d (1:npti,1:jpl), v_s  )
      CALL tab_2d_3d( npti, nptidx(1:npti), oa_i_2d(1:npti,1:jpl), oa_i )
      CALL tab_2d_3d( npti, nptidx(1:npti), sv_i_2d(1:npti,1:jpl), sv_i )
      CALL tab_2d_3d( npti, nptidx(1:npti), a_ip_2d(1:npti,1:jpl), a_ip )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_ip_2d(1:npti,1:jpl), v_ip )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_il_2d(1:npti,1:jpl), v_il )
      CALL tab_2d_3d( npti, nptidx(1:npti), t_su_2d(1:npti,1:jpl), t_su )
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            CALL tab_1d_2d( npti, nptidx(1:npti), ze_s_2d(1:npti,jk,jl), e_s(:,:,jk,jl) )
         END DO
         DO jk = 1, nlay_i
            CALL tab_1d_2d( npti, nptidx(1:npti), ze_i_2d(1:npti,jk,jl), e_i(:,:,jk,jl) )
         END DO
      END DO
      !
   END SUBROUTINE itd_shiftice
   

   SUBROUTINE ice_itd_reb( kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_itd_reb ***
      !!
      !! ** Purpose : rebin - rebins thicknesses into defined categories
      !!
      !! ** Method  : If a category thickness is out of bounds, shift part (for down to top)
      !!              or entire (for top to down) area, volume, and energy
      !!              to the neighboring category
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   kt      ! Ocean time step 
      INTEGER ::   ji, jj, jl   ! dummy loop indices
      !
      INTEGER , DIMENSION(jpij,jpl-1) ::   jdonor           ! donor category index
      REAL(wp), DIMENSION(jpij,jpl-1) ::   zdaice, zdvice   ! ice area and volume transferred
      !!------------------------------------------------------------------
      !
      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_itd_reb: rebining ice thickness distribution' 
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'iceitd_reb', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (0, 'iceitd_reb',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      !
      jdonor(:,:) = 0
      zdaice(:,:) = 0._wp
      zdvice(:,:) = 0._wp
      !
      !                       !---------------------------------------
      DO jl = 1, jpl-1        ! identify thicknesses that are too big
         !                    !---------------------------------------
         npti = 0   ;   nptidx(:) = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( a_i(ji,jj,jl) > 0._wp .AND. v_i(ji,jj,jl) > (a_i(ji,jj,jl) * hi_max(jl)) ) THEN
                  npti = npti + 1
                  nptidx( npti ) = (jj - 1) * jpi + ji                  
               ENDIF
            END DO
         END DO
         !
         IF( npti > 0 ) THEN            
            !!clem   CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d(1:npti), h_i(:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d(1:npti), a_i(:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), v_i_1d(1:npti), v_i(:,:,jl) )
            !
            DO ji = 1, npti
               jdonor(ji,jl)  = jl 
               ! how much of a_i you send in cat sup is somewhat arbitrary
               !!clem: these do not work properly after a restart (I do not know why) => not sure it is still true
               !!          zdaice(ji,jl)  = a_i_1d(ji) * ( h_i_1d(ji) - hi_max(jl) + epsi10 ) / h_i_1d(ji)  
               !!          zdvice(ji,jl)  = v_i_1d(ji) - ( a_i_1d(ji) - zdaice(ji,jl) ) * ( hi_max(jl) - epsi10 )
               !!clem: these do not work properly after a restart (I do not know why) => not sure it is still true
               !!          zdaice(ji,jl)  = a_i_1d(ji)
               !!          zdvice(ji,jl)  = v_i_1d(ji)
               !!clem: these are from UCL and work ok
               zdaice(ji,jl)  = a_i_1d(ji) * 0.5_wp
               zdvice(ji,jl)  = v_i_1d(ji) - zdaice(ji,jl) * ( hi_max(jl) + hi_max(jl-1) ) * 0.5_wp
            END DO
            !
            CALL itd_shiftice( jdonor(1:npti,:), zdaice(1:npti,:), zdvice(1:npti,:) )  ! Shift jl=>jl+1
            ! Reset shift parameters
            jdonor(1:npti,jl) = 0
            zdaice(1:npti,jl) = 0._wp
            zdvice(1:npti,jl) = 0._wp
         ENDIF
         !
      END DO

      !                       !-----------------------------------------
      DO jl = jpl-1, 1, -1    ! Identify thicknesses that are too small
         !                    !-----------------------------------------
         npti = 0 ; nptidx(:) = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( a_i(ji,jj,jl+1) > 0._wp .AND. v_i(ji,jj,jl+1) <= (a_i(ji,jj,jl+1) * hi_max(jl)) ) THEN
                  npti = npti + 1
                  nptidx( npti ) = (jj - 1) * jpi + ji                  
               ENDIF
            END DO
         END DO
         !
         IF( npti > 0 ) THEN
            CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d(1:npti), a_i(:,:,jl+1) ) ! jl+1 is ok
            CALL tab_2d_1d( npti, nptidx(1:npti), v_i_1d(1:npti), v_i(:,:,jl+1) ) ! jl+1 is ok
            !
            DO ji = 1, npti
               jdonor(ji,jl) = jl + 1
               zdaice(ji,jl) = a_i_1d(ji) 
               zdvice(ji,jl) = v_i_1d(ji)
            END DO
            !
            CALL itd_shiftice( jdonor(1:npti,:), zdaice(1:npti,:), zdvice(1:npti,:) )  ! Shift jl+1=>jl
            ! Reset shift parameters
            jdonor(1:npti,jl) = 0
            zdaice(1:npti,jl) = 0._wp
            zdvice(1:npti,jl) = 0._wp
         ENDIF
         !
      END DO
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'iceitd_reb', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'iceitd_reb',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      !
   END SUBROUTINE ice_itd_reb


   SUBROUTINE ice_itd_init
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_itd_init ***
      !!
      !! ** Purpose :   Initializes the ice thickness distribution
      !! ** Method  :   ...
      !! ** input   :   Namelist namitd
      !!-------------------------------------------------------------------
      INTEGER  ::   jl            ! dummy loop index
      INTEGER  ::   ios, ioptio   ! Local integer output status for namelist read
      REAL(wp) ::   zhmax, znum, zden, zalpha   !   -      -
      !
      NAMELIST/namitd/ ln_cat_hfn, rn_himean, ln_cat_usr, rn_catbnd, rn_himin, rn_himax
      !!------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )      ! Namelist namitd in reference namelist : Parameters for ice
      READ  ( numnam_ice_ref, namitd, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namitd in reference namelist' )
      REWIND( numnam_ice_cfg )      ! Namelist namitd in configuration namelist : Parameters for ice
      READ  ( numnam_ice_cfg, namitd, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namitd in configuration namelist' )
      IF(lwm) WRITE( numoni, namitd )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_itd_init: Initialization of ice cat distribution '
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namitd: '
         WRITE(numout,*) '      Ice categories are defined by a function of rn_himean**(-0.05)    ln_cat_hfn = ', ln_cat_hfn
         WRITE(numout,*) '         mean ice thickness in the domain                               rn_himean  = ', rn_himean
         WRITE(numout,*) '      Ice categories are defined by rn_catbnd                           ln_cat_usr = ', ln_cat_usr
         WRITE(numout,*) '      minimum ice thickness allowed                                     rn_himin   = ', rn_himin 
         WRITE(numout,*) '      maximum ice thickness allowed                                     rn_himax   = ', rn_himax 
      ENDIF
      !
      !-----------------------------------!
      !  Thickness categories boundaries  !
      !-----------------------------------!
      !                             !== set the choice of ice categories ==!
      ioptio = 0 
      IF( ln_cat_hfn ) THEN   ;   ioptio = ioptio + 1   ;   nice_catbnd = np_cathfn    ;   ENDIF
      IF( ln_cat_usr ) THEN   ;   ioptio = ioptio + 1   ;   nice_catbnd = np_catusr    ;   ENDIF
      IF( ioptio /= 1 )   CALL ctl_stop( 'ice_itd_init: choose one and only one ice categories boundaries' )
      !
      SELECT CASE( nice_catbnd )
      !                                !------------------------!
      CASE( np_cathfn )                ! h^(-alpha) function
         !                             !------------------------!
         zalpha = 0.05_wp
         zhmax  = 3._wp * rn_himean
         hi_max(0) = 0._wp
         DO jl = 1, jpl
            znum = jpl * ( zhmax+1 )**zalpha
            zden = REAL( jpl-jl , wp ) * ( zhmax + 1._wp )**zalpha + REAL( jl , wp )
            hi_max(jl) = ( znum / zden )**(1./zalpha) - 1
         END DO
         !                             !------------------------!
      CASE( np_catusr )                ! user defined
         !                             !------------------------!
         DO jl = 0, jpl
            hi_max(jl) = rn_catbnd(jl)
         END DO
         !
      END SELECT
      !
      DO jl = 1, jpl                ! mean thickness by category
         hi_mean(jl) = ( hi_max(jl) + hi_max(jl-1) ) * 0.5_wp
      END DO
      !
      hi_max(jpl) = rn_himax        ! set to a big value to ensure that all ice is thinner than hi_max(jpl)
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '   ===>>>   resulting thickness category boundaries :'
      IF(lwp) WRITE(numout,*) '            hi_max(:)= ', hi_max(0:jpl)
      !
      IF( hi_max(1) < rn_himin )   CALL ctl_stop('ice_itd_init: the upper bound of the 1st category must be bigger than rn_himin')
      !
   END SUBROUTINE ice_itd_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE iceitd
