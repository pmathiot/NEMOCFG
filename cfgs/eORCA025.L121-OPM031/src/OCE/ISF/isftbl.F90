MODULE isftbl
   !!======================================================================
   !!                       ***  MODULE  isftbl  ***
   !! isftbl module :  compute properties of top boundary layer
   !!======================================================================
   !! History :  4.1  !  2019-09  (P. Mathiot) original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isftbl       : routine to compute : 
   !!                  - geometry of the ice shelf tbl (isf_tbl_lvl, isftbl_ktop, isftbl_kbot)
   !!                    (top and bottom level, thickness and fraction of deepest level affected)
   !!                  - tbl averaged properties (isf_tbl, isf_tbl_avg)
   !!----------------------------------------------------------------------

   USE isf_oce ! ice shelf variables

   USE dom_oce ! vertical scale factor and depth

   IMPLICIT NONE

   PRIVATE

   PUBLIC isf_tbl, isf_tbl_avg, isf_tbl_lvl, isf_tbl_ktop, isf_tbl_kbot

CONTAINS

   SUBROUTINE isf_tbl( pvarin, pvarout, cd_ptin, ktop, phtbl, kbot, pfrac )
      !!--------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_tbl  ***
      !!
      !! ** Purpose : compute mean T/S/U/V in the boundary layer at T- point
      !!
      !! ** Method : Average properties over a specific thickness
      !!
      !! ** Reference : inspired from : Losch, Modeling ice shelf cavities in a z coordinate ocean general circulation model
      !!                https://doi.org/10.1029/2007JC004368 , 2008
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)          , INTENT(  out) :: pvarout ! 2d average of pvarin
      !!-------------------------- IN  -------------------------------------
      CHARACTER(len=1)                      , INTENT(in   ) :: cd_ptin       ! point of variable in/out
      REAL(wp), DIMENSION(jpi,jpj,jpk)      , INTENT(in   ) :: pvarin        ! 3d variable to average over the tbl
      INTEGER,  DIMENSION(jpi,jpj)          , INTENT(in   ) :: ktop          ! top level
      REAL(wp), DIMENSION(jpi,jpj)          , INTENT(in   ) :: phtbl         ! tbl thickness
      !!-------------------------- IN OPTIONAL -----------------------------
      INTEGER,  DIMENSION(jpi,jpj), OPTIONAL, INTENT(in   ) :: kbot          ! bottom level
      REAL(wp), DIMENSION(jpi,jpj), OPTIONAL, INTENT(in   ) :: pfrac         ! fraction of bottom cell affected by tbl
      !!--------------------------------------------------------------------
      INTEGER ::   ji, jj                     ! loop index
      INTEGER , DIMENSION(jpi,jpj) :: ikbot   ! bottom level of the tbl
      REAL(wp), DIMENSION(jpi,jpj) :: zvarout ! 2d average of pvarin
      REAL(wp), DIMENSION(jpi,jpj) :: zhtbl   ! thickness of the tbl
      REAL(wp), DIMENSION(jpi,jpj) :: zfrac   ! thickness of the tbl
      !!--------------------------------------------------------------------
      ! 
      SELECT CASE ( cd_ptin )
      CASE ( 'U' )
         !
         ! copy phtbl (phtbl is INTENT in as we don't want to change it)
         zhtbl = phtbl
         !
         ! compute tbl lvl and thickness
         CALL isf_tbl_lvl( hu_n, e3u_n, ktop, ikbot, zhtbl, zfrac )
         !
         ! compute tbl property at U point
         CALL isf_tbl_avg( miku, ikbot, zhtbl, zfrac, e3u_n, pvarin, zvarout )
         !
         ! compute tbl property at T point
         pvarout(1,:) = 0._wp
         DO jj = 1, jpj
            DO ji = 2, jpi
               pvarout(ji,jj) = 0.5_wp * (zvarout(ji,jj) + zvarout(ji-1,jj))
            END DO
         END DO
         ! lbclnk not needed as a final communication is done after the computation of fwf
         ! 
      CASE ( 'V' )
         !
         ! copy phtbl (phtbl is INTENT in as we don't want to change it)
         zhtbl = phtbl
         !
         ! compute tbl lvl and thickness
         CALL isf_tbl_lvl( hv_n, e3v_n, ktop, ikbot, zhtbl, zfrac )
         !
         ! compute tbl property at V point
         CALL isf_tbl_avg( mikv, ikbot, zhtbl, zfrac, e3v_n, pvarin, zvarout )
         !
         ! pvarout is an averaging of wet point
         pvarout(:,1) = 0._wp
         DO jj = 2, jpj
            DO ji = 1, jpi
               pvarout(ji,jj) = 0.5_wp * (zvarout(ji,jj) + zvarout(ji,jj-1))
            END DO
         END DO
         ! lbclnk not needed as a final communication is done after the computation of fwf
         !
      CASE ( 'T' )
         !
         ! compute tbl property at T point
         CALL isf_tbl_avg( ktop, kbot, phtbl, pfrac, e3t_n, pvarin, pvarout )
         !
      END SELECT
      !
   END SUBROUTINE isf_tbl

   SUBROUTINE isf_tbl_avg( ktop, kbot, phtbl, pfrac, pe3, pvarin, pvarout )
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_tbl_avg  ***
      !!
      !! ** Purpose : compute mean property in the boundary layer
      !!
      !! ** Method  : Depth average is made between the top level ktop and the bottom level kbot
      !!              over a thickness phtbl. The bottom level is partially counted (pfrac).
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(  out) :: pvarout      ! tbl property averaged over phtbl between level ktop and kbot
      !!-------------------------- IN  -------------------------------------
      INTEGER,  DIMENSION(jpi,jpj)    , INTENT(in   ) :: ktop, kbot   ! top and bottom level of the top boundary layer
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(in   ) :: phtbl, pfrac ! fraction of bottom level to be affected by the tbl
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) :: pe3          ! vertical scale factor
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) :: pvarin       ! tbl property to average between ktop, kbot over phtbl
      !!--------------------------------------------------------------------
      INTEGER  :: ji,jj,jk                    ! loop indices
      INTEGER  :: ikt, ikb                    ! top and bottom levels
      !!--------------------------------------------------------------------
      !
      ! compute tbl top.bottom level and thickness
      DO jj = 1,jpj
         DO ji = 1,jpi
            !
            ! tbl top/bottom indices initialisation
            ikt = ktop(ji,jj) ; ikb = kbot(ji,jj)
            !
            ! level fully include in the ice shelf boundary layer
            pvarout(ji,jj) = SUM( pvarin(ji,jj,ikt:ikb-1) * pe3(ji,jj,ikt:ikb-1) ) / phtbl(ji,jj)
            !
            ! level partially include in ice shelf boundary layer 
            pvarout(ji,jj) = pvarout(ji,jj) + pvarin(ji,jj,ikb) * pe3(ji,jj,ikb) / phtbl(ji,jj) * pfrac(ji,jj)
            !
         END DO
      END DO

   END SUBROUTINE isf_tbl_avg

   SUBROUTINE isf_tbl_lvl( phw, pe3, ktop, kbot, phtbl, pfrac )
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_tbl_lvl  ***
      !!
      !! ** Purpose : - compute bottom level off the top boundary layer
      !!              - thickness of the top boundary layer
      !!              - fraction of the bottom level affected by the tbl
      !!
      !!-------------------------- OUT --------------------------------------
      INTEGER,  DIMENSION(jpi,jpj)    , INTENT(  out) :: kbot   ! bottom level of the top boundary layer
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(  out) :: pfrac  ! fraction of bottom level in the tbl
      !!-------------------------- IN  --------------------------------------
      INTEGER,  DIMENSION(jpi,jpj)    , INTENT(in   ) :: ktop   ! top level of the top boundary layer
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(in   ) :: phw    ! water column thickness
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) :: pe3    ! vertical scale factor
      !!-------------------------- INOUT ------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(inout) :: phtbl  ! top boundary layer thickness
      !!---------------------------------------------------------------------
      INTEGER :: ji,jj,jk
      INTEGER :: ikt, ikb
      !!---------------------------------------------------------------------
      !
      ! get htbl
      DO jj = 1,jpj
         DO ji = 1,jpi
            !
            ! tbl top/bottom indices initialisation
            ikt = ktop(ji,jj)
            !
            ! limit the tbl to water thickness.
            phtbl(ji,jj) = MIN( phtbl(ji,jj), phw(ji,jj) )
            !
            ! thickness of boundary layer must be at least the top level thickness
            phtbl(ji,jj) = MAX( phtbl(ji,jj), pe3(ji,jj,ikt) )
            !
         END DO
      END DO
      !
      ! get ktbl
      CALL isf_tbl_kbot(ktop, phtbl, pe3, kbot)
      !
      ! get pfrac
      DO jj = 1,jpj
         DO ji = 1,jpi
            !
            ! tbl top/bottom indices initialisation
            ikt = ktop(ji,jj) ; ikb = kbot(ji,jj)
            !
            ! proportion of the bottom cell included in ice shelf boundary layer 
            pfrac(ji,jj) = ( phtbl(ji,jj) - SUM( pe3(ji,jj,ikt:ikb-1) ) ) / pe3(ji,jj,ikb)
            !
         END DO
      END DO
      !
   END SUBROUTINE isf_tbl_lvl
   !
   SUBROUTINE isf_tbl_kbot(ktop, phtbl, pe3, kbot)
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_tbl_bot  ***
      !!
      !! ** Purpose : compute bottom level of the isf top boundary layer
      !!
      !!-------------------------- OUT -------------------------------------
      INTEGER,  DIMENSION(jpi,jpj)    , INTENT(  out) :: kbot   ! bottom level of the top boundary layer
      !!-------------------------- IN  -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(in   ) :: phtbl  ! top boundary layer thickness
      INTEGER,  DIMENSION(jpi,jpj)    , INTENT(in   ) :: ktop   ! top level of the top boundary layer
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) :: pe3    ! vertical scale factor
      !!--------------------------------------------------------------------
      INTEGER :: ji, jj
      INTEGER :: ikt, ikb
      !!--------------------------------------------------------------------
      !
      ! phtbl need to be bounded by water column thickness before
      ! test: if htbl = water column thickness, should return mbathy
      ! test: if htbl = 0 should return ktop (phtbl cap to e3t(ji,jj,1))
      !
      ! get ktbl
      DO jj = 1,jpj
         DO ji = 1,jpi
            !
            ! determine the deepest level influenced by the boundary layer
            ikt = ktop(ji,jj)
            ikb = ikt
            DO WHILE ( SUM(pe3(ji,jj,ikt:ikb-1)) < phtbl(ji,jj ) ) ;  ikb = ikb + 1 ;  END DO
            kbot(ji,jj) = ikb - 1
            !
         END DO
      END DO
      !
   END SUBROUTINE isf_tbl_kbot
      !
   SUBROUTINE isf_tbl_ktop(pdep, ktop)
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_tbl_top  ***
      !!
      !! ** Purpose : compute top level of the isf top boundary layer in case of an ice shelf parametrisation
      !!
      !!-------------------------- OUT -------------------------------------
      INTEGER,  DIMENSION(jpi,jpj), INTENT(  out) :: ktop        ! top level affected by the ice shelf parametrisation
      !!-------------------------- IN  -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) :: pdep        ! top depth of the parametrisation influence
      !!--------------------------------------------------------------------
      INTEGER :: ji,jj
      INTEGER :: ikt
      !!--------------------------------------------------------------------
      !
      ! if we need to recompute the top level at every time stepcompute top level (z*, z~) 
      ! in case of weak ht_n variation we can assume the top level of htbl to be constant
      ! => only done using gdepw_0
      ! be sure pdep is already correctly bounded
      ! test: this routine run on isfdraft should return mikt
      ! test: this routine run with pdep = 0 should return 1
      !
      DO ji = 1, jpi
         DO jj = 1, jpj
            ! comput ktop
            ikt = 2
            DO WHILE ( gdepw_0(ji,jj,ikt) <= pdep(ji,jj ) ) ;  ikt = ikt + 1 ;  END DO
            ktop(ji,jj) = ikt - 1
            !
            ! update pdep
            pdep(ji,jj) = gdepw_0(ji,jj,ktop(ji,jj))
         END DO
      END DO
      !
   END SUBROUTINE isf_tbl_ktop

END MODULE isftbl
