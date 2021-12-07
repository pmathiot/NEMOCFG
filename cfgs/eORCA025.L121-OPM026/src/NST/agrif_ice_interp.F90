MODULE agrif_ice_interp
   !!=====================================================================================
   !!                       ***  MODULE agrif_ice_interp ***
   !! Nesting module :  interp surface ice boundary condition from a parent grid
   !!=====================================================================================
   !! History :  2.0   !  04-2008  (F. Dupont)               initial version
   !!            3.4   !  09-2012  (R. Benshila, C. Herbaut) update and EVP
   !!            4.0   !  2018     (C. Rousset)              SI3 compatibility
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_si3 
   !!----------------------------------------------------------------------
   !!   'key_si3'                                         SI3 sea-ice model
   !!   'key_agrif'                                       AGRIF library
   !!----------------------------------------------------------------------
   !!  agrif_interp_ice    : interpolation of ice at "after" sea-ice time step
   !!  agrif_interp_u_ice   : atomic routine to interpolate u_ice 
   !!  agrif_interp_v_ice   : atomic routine to interpolate v_ice 
   !!  agrif_interp_tra_ice : atomic routine to interpolate ice properties 
   !!----------------------------------------------------------------------
   USE par_oce
   USE dom_oce
   USE sbc_oce
   USE ice
   USE agrif_ice
   USE phycst , ONLY: rt0
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   agrif_interp_ice   ! called by agrif_user.F90

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_ice_interp.F90 13479 2020-09-16 16:56:46Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE agrif_interp_ice( cd_type, kiter, kitermax )
      !!-----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_interp_ice  ***
      !!
      !!  ** Method  : simple call to atomic routines using stored values to
      !!  fill the boundaries depending of the position of the point and
      !!  computing factor for time interpolation
      !!-----------------------------------------------------------------------
      CHARACTER(len=1), INTENT(in   )           ::   cd_type
      INTEGER         , INTENT(in   ), OPTIONAL ::   kiter, kitermax
      !!
      REAL(wp) ::   zbeta   ! local scalar
      !!-----------------------------------------------------------------------
      !
      IF( Agrif_Root() .OR. nn_ice==0 )  RETURN   ! do not interpolate if inside Parent Grid or if child domain does not have ice
      !
      SELECT CASE( cd_type )
      CASE('U','V')
         IF( PRESENT( kiter ) ) THEN  ! interpolation at the child ice sub-time step (only for ice rheology)
            zbeta = ( REAL(nbstep_ice) - REAL(kitermax - kiter) / REAL(kitermax) ) /  &
               &    ( Agrif_Rhot() * REAL(Agrif_Parent(nn_fsbc)) / REAL(nn_fsbc) )
         ELSE                         ! interpolation at the child ice time step
            zbeta = REAL(nbstep_ice) / ( Agrif_Rhot() * REAL(Agrif_Parent(nn_fsbc)) / REAL(nn_fsbc) )
         ENDIF
      CASE('T')
            zbeta = REAL(nbstep_ice) / ( Agrif_Rhot() * REAL(Agrif_Parent(nn_fsbc)) / REAL(nn_fsbc) )
      END SELECT
      !
      Agrif_SpecialValue    = -9999.
      Agrif_UseSpecialValue = .TRUE.
      SELECT CASE( cd_type )
      CASE('U')   ;   CALL Agrif_Bc_variable( u_ice_id  , procname=interp_u_ice  , calledweight=zbeta )
      CASE('V')   ;   CALL Agrif_Bc_variable( v_ice_id  , procname=interp_v_ice  , calledweight=zbeta )
      CASE('T')   ;   CALL Agrif_Bc_variable( tra_ice_id, procname=interp_tra_ice, calledweight=zbeta )
      END SELECT
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .FALSE.
      !
   END SUBROUTINE agrif_interp_ice


   SUBROUTINE interp_u_ice( ptab, i1, i2, j1, j2, before )
      !!-----------------------------------------------------------------------
      !!                     *** ROUTINE interp_u_ice ***
      !!
      !! i1 i2 j1 j2 are the index of the boundaries parent(when before) and child (when after)
      !! To solve issues when parent grid is "land" masked but not all the corresponding child 
      !! grid points, put Agrif_SpecialValue WHERE the parent grid is masked. 
      !! The child solution will be found in the 9(?) points around
      !!-----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      REAL(wp) ::   zrhoy   ! local scalar
      !!-----------------------------------------------------------------------
      !
      IF( before ) THEN  ! parent grid
         ptab(:,:) = e2u(i1:i2,j1:j2) * u_ice(i1:i2,j1:j2)
         WHERE( umask(i1:i2,j1:j2,1) == 0. )   ptab(i1:i2,j1:j2) = Agrif_SpecialValue
      ELSE               ! child grid
         zrhoy = Agrif_Rhoy()
         u_ice(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) / ( e2u(i1:i2,j1:j2) * zrhoy ) * umask(i1:i2,j1:j2,1)
      ENDIF
      !
   END SUBROUTINE interp_u_ice


   SUBROUTINE interp_v_ice( ptab, i1, i2, j1, j2, before )
      !!-----------------------------------------------------------------------
      !!                    *** ROUTINE interp_v_ice ***
      !!
      !! i1 i2 j1 j2 are the index of the boundaries parent(when before) and child (when after)
      !! To solve issues when parent grid is "land" masked but not all the corresponding child 
      !! grid points, put Agrif_SpecialValue WHERE the parent grid is masked. 
      !! The child solution will be found in the 9(?) points around
      !!-----------------------------------------------------------------------      
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      REAL(wp) ::   zrhox   ! local scalar
      !!-----------------------------------------------------------------------
      !
      IF( before ) THEN  ! parent grid
         ptab(:,:) = e1v(i1:i2,j1:j2) * v_ice(i1:i2,j1:j2)
         WHERE( vmask(i1:i2,j1:j2,1) == 0. )   ptab(i1:i2,j1:j2) = Agrif_SpecialValue
      ELSE               ! child grid
         zrhox = Agrif_Rhox()
         v_ice(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) / ( e1v(i1:i2,j1:j2) * zrhox ) * vmask(i1:i2,j1:j2,1)
      ENDIF
      !
   END SUBROUTINE interp_v_ice


   SUBROUTINE interp_tra_ice( ptab, i1, i2, j1, j2, k1, k2, before, nb, ndir )
      !!-----------------------------------------------------------------------
      !!                    *** ROUTINE interp_tra_ice ***                           
      !!
      !! i1 i2 j1 j2 are the index of the boundaries parent(when before) and child (when after)
      !! To solve issues when parent grid is "land" masked but not all the corresponding child 
      !! grid points, put Agrif_SpecialValue WHERE the parent grid is masked. 
      !! The child solution will be found in the 9(?) points around
      !!-----------------------------------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb, ndir
      !!
      INTEGER  ::   ji, jj, jk, jl, jm
      INTEGER  ::   imin, imax, jmin, jmax
      LOGICAL  ::   western_side, eastern_side, northern_side, southern_side
      REAL(wp) ::   zrhox, z1, z2, z3, z4, z5, z6, z7
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztab
      !!-----------------------------------------------------------------------
      ! tracers are not multiplied by grid cell here => before: * e1e2t ; after: * r1_e1e2t / rhox / rhoy
      ! and it is ok since we conserve tracers (same as in the ocean).
      ALLOCATE( ztab(SIZE(a_i,1),SIZE(a_i,2),SIZE(ptab,3)) )
     
      IF( before ) THEN  ! parent grid
         jm = 1
         DO jl = 1, jpl
            ptab(i1:i2,j1:j2,jm  ) = a_i (i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+1) = v_i (i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+2) = v_s (i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+3) = sv_i(i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+4) = oa_i(i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+5) = a_ip(i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+6) = v_ip(i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+7) = v_il(i1:i2,j1:j2,jl)
            ptab(i1:i2,j1:j2,jm+8) = t_su(i1:i2,j1:j2,jl)
            jm = jm + 9
            DO jk = 1, nlay_s
               ptab(i1:i2,j1:j2,jm) = e_s(i1:i2,j1:j2,jk,jl)   ;   jm = jm + 1
            END DO
            DO jk = 1, nlay_i
               ptab(i1:i2,j1:j2,jm) = e_i(i1:i2,j1:j2,jk,jl)   ;   jm = jm + 1
            END DO
         END DO
         
         DO jk = k1, k2
            WHERE( tmask(i1:i2,j1:j2,1) == 0._wp )   ptab(i1:i2,j1:j2,jk) = Agrif_SpecialValue
         END DO
         !
      ELSE               ! child grid
         !
!         IF( nbghostcells > 1 ) THEN   ! ==> The easiest interpolation is used
            !
            jm = 1
            DO jl = 1, jpl
               !
               DO jj = j1, j2
                  DO ji = i1, i2
                     a_i (ji,jj,jl) = ptab(ji,jj,jm  ) * tmask(ji,jj,1)
                     v_i (ji,jj,jl) = ptab(ji,jj,jm+1) * tmask(ji,jj,1)
                     v_s (ji,jj,jl) = ptab(ji,jj,jm+2) * tmask(ji,jj,1)
                     sv_i(ji,jj,jl) = ptab(ji,jj,jm+3) * tmask(ji,jj,1)
                     oa_i(ji,jj,jl) = ptab(ji,jj,jm+4) * tmask(ji,jj,1)
                     a_ip(ji,jj,jl) = ptab(ji,jj,jm+5) * tmask(ji,jj,1)
                     v_ip(ji,jj,jl) = ptab(ji,jj,jm+6) * tmask(ji,jj,1)
                     v_il(ji,jj,jl) = ptab(ji,jj,jm+7) * tmask(ji,jj,1)
                     t_su(ji,jj,jl) = ptab(ji,jj,jm+8) * tmask(ji,jj,1)
                  END DO
               END DO
               jm = jm + 9
               !
               DO jk = 1, nlay_s
                  e_s(i1:i2,j1:j2,jk,jl) = ptab(i1:i2,j1:j2,jm) * tmask(i1:i2,j1:j2,1)
                  jm = jm + 1
               END DO
               !
               DO jk = 1, nlay_i
                  e_i(i1:i2,j1:j2,jk,jl) = ptab(i1:i2,j1:j2,jm) * tmask(i1:i2,j1:j2,1)
                  jm = jm + 1
               END DO
               !
            END DO
            !
!!==> clem: this interpolation does not work because it creates negative values, due
!!          to negative coefficients when mixing points (for ex. z7)
!!
!         ELSE                          ! ==> complex interpolation (only one ghost cell available)
!            !! Use a more complex interpolation since we mix solutions over a couple of grid points
!            !! it is advised to use it for fields modified by high order schemes (e.g. advection UM5...)
!            ! record ztab
!            jm = 1
!            DO jl = 1, jpl
!               ztab(:,:,jm  ) = a_i (:,:,jl)
!               ztab(:,:,jm+1) = v_i (:,:,jl)
!               ztab(:,:,jm+2) = v_s (:,:,jl)
!               ztab(:,:,jm+3) = sv_i(:,:,jl)
!               ztab(:,:,jm+4) = oa_i(:,:,jl)
!               ztab(:,:,jm+5) = a_ip(:,:,jl)
!               ztab(:,:,jm+6) = v_ip(:,:,jl)
!               ztab(:,:,jm+7) = v_il(:,:,jl)
!               ztab(:,:,jm+8) = t_su(:,:,jl)
!               jm = jm + 9
!               DO jk = 1, nlay_s
!                  ztab(:,:,jm) = e_s(:,:,jk,jl)
!                  jm = jm + 1
!               END DO
!               DO jk = 1, nlay_i
!                  ztab(:,:,jm) = e_i(:,:,jk,jl)
!                  jm = jm + 1
!               END DO
!               !
!            END DO
!            !
!            ! borders of the domain
!            western_side  = (nb == 1).AND.(ndir == 1)  ;  eastern_side  = (nb == 1).AND.(ndir == 2)
!            southern_side = (nb == 2).AND.(ndir == 1)  ;  northern_side = (nb == 2).AND.(ndir == 2)
!            !
!            ! spatial smoothing
!            zrhox = Agrif_Rhox()
!            z1 =      ( zrhox - 1. ) * 0.5 
!            z3 =      ( zrhox - 1. ) / ( zrhox + 1. )
!            z6 = 2. * ( zrhox - 1. ) / ( zrhox + 1. )
!            z7 =    - ( zrhox - 1. ) / ( zrhox + 3. )
!            z2 = 1. - z1
!            z4 = 1. - z3
!            z5 = 1. - z6 - z7
!            !
!            ! Remove corners
!            imin = i1  ;  imax = i2  ;  jmin = j1  ;  jmax = j2
!            IF( (nbondj == -1) .OR. (nbondj == 2) )   jmin = 3
!            IF( (nbondj == +1) .OR. (nbondj == 2) )   jmax = nlcj-2
!            IF( (nbondi == -1) .OR. (nbondi == 2) )   imin = 3
!            IF( (nbondi == +1) .OR. (nbondi == 2) )   imax = nlci-2
!
!            ! smoothed fields
!            IF( eastern_side ) THEN
!               ztab(nlci,j1:j2,:) = z1 * ptab(nlci,j1:j2,:) + z2 * ptab(nlci-1,j1:j2,:)
!               DO jj = jmin, jmax
!                  rswitch = 0.
!                  IF( u_ice(nlci-2,jj) > 0._wp ) rswitch = 1.
!                  ztab(nlci-1,jj,:) = ( 1. - umask(nlci-2,jj,1) ) * ztab(nlci,jj,:)  &
!                     &                +      umask(nlci-2,jj,1)   *  &
!                     &                ( ( 1. - rswitch ) * ( z4 * ztab(nlci,jj,:)   + z3 * ztab(nlci-2,jj,:) )  &
!                     &                  +      rswitch   * ( z6 * ztab(nlci-2,jj,:) + z5 * ztab(nlci,jj,:) + z7 * ztab(nlci-3,jj,:) ) )
!                  ztab(nlci-1,jj,:) = ztab(nlci-1,jj,:) * tmask(nlci-1,jj,1)
!               END DO
!            ENDIF
!            ! 
!            IF( northern_side ) THEN
!               ztab(i1:i2,nlcj,:) = z1 * ptab(i1:i2,nlcj,:) + z2 * ptab(i1:i2,nlcj-1,:)
!               DO ji = imin, imax
!                  rswitch = 0.
!                  IF( v_ice(ji,nlcj-2) > 0._wp ) rswitch = 1.
!                  ztab(ji,nlcj-1,:) = ( 1. - vmask(ji,nlcj-2,1) ) * ztab(ji,nlcj,:)  &
!                     &                +      vmask(ji,nlcj-2,1)   *  &
!                     &                ( ( 1. - rswitch ) * ( z4 * ztab(ji,nlcj,:)   + z3 * ztab(ji,nlcj-2,:) ) &
!                     &                  +      rswitch   * ( z6 * ztab(ji,nlcj-2,:) + z5 * ztab(ji,nlcj,:) + z7 * ztab(ji,nlcj-3,:) ) )
!                  ztab(ji,nlcj-1,:) = ztab(ji,nlcj-1,:) * tmask(ji,nlcj-1,1)
!               END DO
!            END IF
!            !
!            IF( western_side) THEN
!               ztab(1,j1:j2,:) = z1 * ptab(1,j1:j2,:) + z2 * ptab(2,j1:j2,:)
!               DO jj = jmin, jmax
!                  rswitch = 0.
!                  IF( u_ice(2,jj) < 0._wp ) rswitch = 1.
!                  ztab(2,jj,:) = ( 1. - umask(2,jj,1) ) * ztab(1,jj,:)  &
!                     &           +      umask(2,jj,1)   *   &
!                     &           ( ( 1. - rswitch ) * ( z4 * ztab(1,jj,:) + z3 * ztab(3,jj,:) ) &
!                     &             +      rswitch   * ( z6 * ztab(3,jj,:) + z5 * ztab(1,jj,:) + z7 * ztab(4,jj,:) ) )
!                  ztab(2,jj,:) = ztab(2,jj,:) * tmask(2,jj,1)
!               END DO
!            ENDIF
!            !
!            IF( southern_side ) THEN
!               ztab(i1:i2,1,:) = z1 * ptab(i1:i2,1,:) + z2 * ptab(i1:i2,2,:)
!               DO ji = imin, imax
!                  rswitch = 0.
!                  IF( v_ice(ji,2) < 0._wp ) rswitch = 1.
!                  ztab(ji,2,:) = ( 1. - vmask(ji,2,1) ) * ztab(ji,1,:)  &
!                     &           +      vmask(ji,2,1)   *  &
!                     &           ( ( 1. - rswitch ) * ( z4 * ztab(ji,1,:) + z3 * ztab(ji,3,:) ) &
!                     &             +      rswitch   * ( z6 * ztab(ji,3,:) + z5 * ztab(ji,1,:) + z7 * ztab(ji,4,:) ) )
!                  ztab(ji,2,:) = ztab(ji,2,:) * tmask(ji,2,1)
!               END DO
!            END IF
!            !
!            ! Treatment of corners
!            IF( (eastern_side) .AND. ((nbondj == -1).OR.(nbondj == 2)) )  ztab(nlci-1,2,:)      = ptab(nlci-1,2,:)      ! East south
!            IF( (eastern_side) .AND. ((nbondj ==  1).OR.(nbondj == 2)) )  ztab(nlci-1,nlcj-1,:) = ptab(nlci-1,nlcj-1,:) ! East north
!            IF( (western_side) .AND. ((nbondj == -1).OR.(nbondj == 2)) )  ztab(2,2,:)           = ptab(2,2,:)           ! West south
!            IF( (western_side) .AND. ((nbondj ==  1).OR.(nbondj == 2)) )  ztab(2,nlcj-1,:)      = ptab(2,nlcj-1,:)      ! West north
!            
!            ! retrieve ice tracers
!            jm = 1
!            DO jl = 1, jpl
!               !
!               DO jj = j1, j2
!                  DO ji = i1, i2
!                     a_i (ji,jj,jl) = ztab(ji,jj,jm  ) * tmask(ji,jj,1)
!                     v_i (ji,jj,jl) = ztab(ji,jj,jm+1) * tmask(ji,jj,1)
!                     v_s (ji,jj,jl) = ztab(ji,jj,jm+2) * tmask(ji,jj,1)
!                     sv_i(ji,jj,jl) = ztab(ji,jj,jm+3) * tmask(ji,jj,1)
!                     oa_i(ji,jj,jl) = ztab(ji,jj,jm+4) * tmask(ji,jj,1)
!                     a_ip(ji,jj,jl) = ztab(ji,jj,jm+5) * tmask(ji,jj,1)
!                     v_ip(ji,jj,jl) = ztab(ji,jj,jm+6) * tmask(ji,jj,1)
!                     v_il(ji,jj,jl) = ztab(ji,jj,jm+7) * tmask(ji,jj,1)
!                     t_su(ji,jj,jl) = ztab(ji,jj,jm+8) * tmask(ji,jj,1)
!                  END DO
!               END DO
!               jm = jm + 9
!               !
!               DO jk = 1, nlay_s
!                  e_s(i1:i2,j1:j2,jk,jl) = ztab(i1:i2,j1:j2,jm) * tmask(i1:i2,j1:j2,1)
!                  jm = jm + 1
!               END DO
!               !
!               DO jk = 1, nlay_i
!                  e_i(i1:i2,j1:j2,jk,jl) = ztab(i1:i2,j1:j2,jm) * tmask(i1:i2,j1:j2,1)
!                  jm = jm + 1
!               END DO
!               !
!            END DO
!          
!         ENDIF  ! nbghostcells=1
         
         DO jl = 1, jpl
            WHERE( tmask(i1:i2,j1:j2,1) == 0._wp )   t_su(i1:i2,j1:j2,jl) = rt0   ! to avoid a division by 0 in sbcblk.F90
         END DO
         !
      ENDIF
      
      DEALLOCATE( ztab )
      !
   END SUBROUTINE interp_tra_ice

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                             no sea-ice
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_ice_interp_empty
      WRITE(*,*)  'agrif_ice_interp : You should not have seen this print! error?'
   END SUBROUTINE agrif_ice_interp_empty
#endif

   !!======================================================================
END MODULE agrif_ice_interp
