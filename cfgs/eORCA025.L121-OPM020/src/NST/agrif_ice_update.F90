#define TWO_WAY
!!#undef TWO_WAY
#undef DECAL_FEEDBACK  /* SEPARATION of INTERFACES*/

MODULE agrif_ice_update
   !!=====================================================================================
   !!                       ***  MODULE agrif_ice_update ***
   !! Nesting module :  update surface ocean boundary condition over ice from a child grid
   !!=====================================================================================
   !! History :  2.0   !  04-2008  (F. Dupont)               initial version
   !!            3.4   !  08-2012  (R. Benshila, C. Herbaut) update and EVP
   !!            4.0   !  2018     (C. Rousset)              SI3 compatibility
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'  :                                      SI3 sea-ice model
   !!   'key_agrif' :                                     AGRIF library 
   !!----------------------------------------------------------------------
   !!   agrif_update_ice  : update sea-ice on boundaries or total
   !!                        child domain for velocities and ice properties
   !!   update_tra_ice    : sea-ice properties
   !!   update_u_ice      : zonal      ice velocity
   !!   update_v_ice      : meridional ice velocity
   !!----------------------------------------------------------------------
   USE dom_oce
   USE sbc_oce
   USE agrif_oce
   USE ice
   USE agrif_ice 
   USE phycst , ONLY: rt0

   IMPLICIT NONE
   PRIVATE

   PUBLIC   agrif_update_ice   ! called by agrif_user.F90 and icestp.F90

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_ice_update.F90 13479 2020-09-16 16:56:46Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE agrif_update_ice( )
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE agrif_update_ice ***
      !! ** Method  :   Call the hydrostaticupdate pressure at the boundary or the entire domain 
      !!
      !! ** Action : - Update (u_ice,v_ice) and ice tracers
      !!----------------------------------------------------------------------
      !
      IF( Agrif_Root() .OR. nn_ice == 0 ) RETURN   ! do not update if inside Parent Grid or if child domain does not have ice
      !
!      IF( ( MOD( (kt-nit000)/nn_fsbc + 1, Agrif_irhot() * Agrif_Parent(nn_fsbc)/nn_fsbc ) /=0 ) .AND. (kt /= 0) ) RETURN   ! update only at the parent ice time step
      IF ( MOD(Agrif_parent_nb_step(), Agrif_Parent(nn_fsbc)) /=0 ) RETURN   ! Update only at the parent ice time step
                                                                             ! It is assumed that at such a time, there is a child ice step which is true
                                                                             ! as long as MOD( Agrif_irhot() * Agrif_Parent(nn_fsbc), nn_fsbc )==0. 
                                                                             ! (This condition is checked in agrif_user, Agrif_InitValues_cont_ice subroutine)
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update sea ice from grid Number',Agrif_Fixed(), agrif_nb_step()
      !
      !
      Agrif_SpecialValueFineGrid    = -9999.
      Agrif_UseSpecialValueInUpdate = .TRUE.

# if defined TWO_WAY
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable( tra_ice_id , procname = update_tra_ice  )
#else
      CALL Agrif_Update_Variable( tra_ice_id , locupdate=(/1,0/), procname = update_tra_ice  )
#endif
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable( u_ice_id   , procname = update_u_ice    )
      CALL Agrif_Update_Variable( v_ice_id   , procname = update_v_ice    )
#else
      CALL Agrif_Update_Variable( u_ice_id   , locupdate1=(/0,-1/),locupdate2=(/1,-2/),procname=update_u_ice) 
      CALL Agrif_Update_Variable( v_ice_id   , locupdate1=(/1,-2/),locupdate2=(/0,-1/),procname=update_v_ice)
#endif
!      CALL Agrif_Update_Variable( tra_ice_id , locupdate=(/0,2/), procname = update_tra_ice  )
!      CALL Agrif_Update_Variable( u_ice_id   , locupdate=(/0,1/), procname = update_u_ice    )
!      CALL Agrif_Update_Variable( v_ice_id   , locupdate=(/0,1/), procname = update_v_ice    )
# endif
      Agrif_SpecialValueFineGrid    = 0.
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
   END SUBROUTINE agrif_update_ice


   SUBROUTINE update_tra_ice( ptab, i1, i2, j1, j2, k1, k2, before )
      !!-----------------------------------------------------------------------
      !!                        *** ROUTINE update_tra_ice ***
      !! ** Method  : Compute the mass properties on the fine grid and recover
      !!              the properties per mass on the coarse grid
      !!-----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!
      INTEGER  :: ji, jj, jk, jl, jm
      !!-----------------------------------------------------------------------
      ! it is ok not to multiply by e1*e2 since we conserve tracers here (same as in the ocean).
      IF( before ) THEN
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
         !
         DO jk = k1, k2
            WHERE( tmask(i1:i2,j1:j2,1) == 0. )   ptab(i1:i2,j1:j2,jk) = Agrif_SpecialValueFineGrid 
         END DO
         !
      ELSE
         !
         jm = 1
         DO jl = 1, jpl
            !
            DO jj = j1, j2
               DO ji = i1, i2
                  IF( ptab(ji,jj,jm) /= Agrif_SpecialValueFineGrid ) THEN
                     a_i (ji,jj,jl) = ptab(ji,jj,jm  ) * tmask(ji,jj,1)
                     v_i (ji,jj,jl) = ptab(ji,jj,jm+1) * tmask(ji,jj,1)
                     v_s (ji,jj,jl) = ptab(ji,jj,jm+2) * tmask(ji,jj,1)
                     sv_i(ji,jj,jl) = ptab(ji,jj,jm+3) * tmask(ji,jj,1)
                     oa_i(ji,jj,jl) = ptab(ji,jj,jm+4) * tmask(ji,jj,1)
                     a_ip(ji,jj,jl) = ptab(ji,jj,jm+5) * tmask(ji,jj,1)
                     v_ip(ji,jj,jl) = ptab(ji,jj,jm+6) * tmask(ji,jj,1)
                     v_il(ji,jj,jl) = ptab(ji,jj,jm+7) * tmask(ji,jj,1)
                     t_su(ji,jj,jl) = ptab(ji,jj,jm+8) * tmask(ji,jj,1)
                  ENDIF
               END DO
            END DO
            jm = jm + 9
            !
            DO jk = 1, nlay_s
               WHERE( ptab(i1:i2,j1:j2,jm) /= Agrif_SpecialValueFineGrid )
                  e_s(i1:i2,j1:j2,jk,jl) = ptab(i1:i2,j1:j2,jm) * tmask(i1:i2,j1:j2,1)
               ENDWHERE
               jm = jm + 1
            END DO
            !
            DO jk = 1, nlay_i
               WHERE( ptab(i1:i2,j1:j2,jm) /= Agrif_SpecialValueFineGrid )
                  e_i(i1:i2,j1:j2,jk,jl) = ptab(i1:i2,j1:j2,jm) * tmask(i1:i2,j1:j2,1)
               ENDWHERE
               jm = jm + 1
            END DO
            !
         END DO
         !
         DO jl = 1, jpl
            WHERE( tmask(i1:i2,j1:j2,1) == 0._wp )   t_su(i1:i2,j1:j2,jl) = rt0   ! to avoid a division by 0 in sbcblk.F90
         END DO
         
      ENDIF
      !
   END SUBROUTINE update_tra_ice


   SUBROUTINE update_u_ice( ptab, i1, i2, j1, j2, before )
      !!-----------------------------------------------------------------------
      !!                        *** ROUTINE update_u_ice ***
      !! ** Method  : Update the fluxes and recover the properties (C-grid)
      !!-----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      REAL(wp) ::   zrhoy   ! local scalar
      !!-----------------------------------------------------------------------
      !
      IF( before ) THEN
         zrhoy = Agrif_Rhoy()
         ptab(:,:) = e2u(i1:i2,j1:j2) * u_ice(i1:i2,j1:j2) * zrhoy
         WHERE( umask(i1:i2,j1:j2,1) == 0._wp )   ptab(:,:) = Agrif_SpecialValueFineGrid
      ELSE
         WHERE( ptab(i1:i2,j1:j2) /= Agrif_SpecialValueFineGrid )
            u_ice(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) / e2u(i1:i2,j1:j2) * umask(i1:i2,j1:j2,1)
         ENDWHERE
      ENDIF
      ! 
   END SUBROUTINE update_u_ice


   SUBROUTINE update_v_ice( ptab, i1, i2, j1, j2, before )
      !!-----------------------------------------------------------------------
      !!                    *** ROUTINE update_v_ice ***
      !! ** Method  : Update the fluxes and recover the properties (C-grid)
      !!-----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      REAL(wp) ::   zrhox   ! local scalar
      !!-----------------------------------------------------------------------
      !
      IF( before ) THEN
         zrhox = Agrif_Rhox()
         ptab(:,:) = e1v(i1:i2,j1:j2) * v_ice(i1:i2,j1:j2) * zrhox
         WHERE( vmask(i1:i2,j1:j2,1) == 0._wp )   ptab(:,:) = Agrif_SpecialValueFineGrid
      ELSE
         WHERE( ptab(i1:i2,j1:j2) /= Agrif_SpecialValueFineGrid )
            v_ice(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) / e1v(i1:i2,j1:j2) * vmask(i1:i2,j1:j2,1)
         ENDWHERE
      ENDIF
      !
   END SUBROUTINE update_v_ice

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                             no sea-ice
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_ice_update_empty
      WRITE(*,*)  'agrif_ice_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_ice_update_empty
#endif

   !!======================================================================
END MODULE agrif_ice_update
