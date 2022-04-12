#define TWO_WAY        /* TWO WAY NESTING */
#undef DECAL_FEEDBACK  /* SEPARATION of INTERFACES*/
#undef VOL_REFLUX      /* VOLUME REFLUXING*/
 
MODULE agrif_oce_update
   !!======================================================================
   !!                   ***  MODULE  agrif_oce_interp  ***
   !! AGRIF: update package for the ocean dynamics (OPA)
   !!======================================================================
   !! History :  2.0  !  2002-06  (L. Debreu)  Original code
   !!            3.2  !  2009-04  (R. Benshila) 
   !!            3.6  !  2014-09  (R. Benshila) 
   !!----------------------------------------------------------------------
#if defined key_agrif 
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce
   USE zdf_oce        ! vertical physics: ocean variables 
   USE agrif_oce
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE domvvl         ! Need interpolation routines 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   Agrif_Update_Tra, Agrif_Update_Dyn, Agrif_Update_vvl, Agrif_Update_ssh
   PUBLIC   Update_Scales

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce_update.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Update_Tra( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Update_Tra ***
      !!----------------------------------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN
      !
#if defined TWO_WAY  
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update tracers  from grid Number',Agrif_Fixed()

      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid    = 0._wp
      ! 
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(tsn_id, procname=updateTS)
! near boundary update:
!      CALL Agrif_Update_Variable(tsn_id,locupdate=(/0,2/), procname=updateTS)
# else
      CALL Agrif_Update_Variable(tsn_id, locupdate=(/1,0/),procname=updateTS)
! near boundary update:
!      CALL Agrif_Update_Variable(tsn_id,locupdate=(/1,2/), procname=updateTS)
# endif
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
#endif
      !
   END SUBROUTINE Agrif_Update_Tra

   SUBROUTINE Agrif_Update_Dyn( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Update_Dyn ***
      !!----------------------------------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN
      !
#if defined TWO_WAY
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update momentum from grid Number',Agrif_Fixed()

      Agrif_UseSpecialValueInUpdate = .FALSE.
      Agrif_SpecialValueFineGrid = 0.
      !     
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(un_update_id,procname = updateU)
      CALL Agrif_Update_Variable(vn_update_id,procname = updateV)
! near boundary update:
!      CALL Agrif_Update_Variable(un_update_id,locupdate=(/0,1/),procname = updateU)
!      CALL Agrif_Update_Variable(vn_update_id,locupdate=(/0,1/),procname = updateV)
# else
      CALL Agrif_Update_Variable(un_update_id,locupdate1=(/0,-1/),locupdate2=(/1,-2/),procname = updateU)
      CALL Agrif_Update_Variable(vn_update_id,locupdate1=(/1,-2/),locupdate2=(/0,-1/),procname = updateV)
! near boundary update:
!      CALL Agrif_Update_Variable(un_update_id,locupdate1=(/0,1/),locupdate2=(/1,1/),procname = updateU)
!      CALL Agrif_Update_Variable(vn_update_id,locupdate1=(/1,1/),locupdate2=(/0,1/),procname = updateV)
# endif

# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(e1u_id,procname = updateU2d)
      CALL Agrif_Update_Variable(e2v_id,procname = updateV2d)  
# else
      CALL Agrif_Update_Variable(e1u_id,locupdate1=(/0,-1/),locupdate2=(/1,-2/),procname = updateU2d)
      CALL Agrif_Update_Variable(e2v_id,locupdate1=(/1,-2/),locupdate2=(/0,-1/),procname = updateV2d)  
# endif
      !
# if ! defined DECAL_FEEDBACK
      ! Account for updated thicknesses at boundary edges
      IF (.NOT.ln_linssh) THEN
!         CALL Agrif_Update_Variable(un_update_id,locupdate1=(/0,0/),locupdate2=(/0,0/),procname = correct_u_bdy)
!         CALL Agrif_Update_Variable(vn_update_id,locupdate1=(/0,0/),locupdate2=(/0,0/),procname = correct_v_bdy)
      ENDIF
# endif
      ! 
      IF ( ln_dynspg_ts .AND. ln_bt_fw ) THEN
         ! Update time integrated transports
#  if ! defined DECAL_FEEDBACK
         CALL Agrif_Update_Variable(ub2b_update_id,procname = updateub2b)
         CALL Agrif_Update_Variable(vb2b_update_id,procname = updatevb2b)
#  else
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/0,-1/),locupdate2=(/1,-2/),procname = updateub2b)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/1,-2/),locupdate2=(/0,-1/),procname = updatevb2b)
#  endif
      END IF
#endif
      !
   END SUBROUTINE Agrif_Update_Dyn

   SUBROUTINE Agrif_Update_ssh( )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_ssh ***
      !!---------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN
      !
#if defined TWO_WAY
      !
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(sshn_id,procname = updateSSH)
# else
      CALL Agrif_Update_Variable(sshn_id,locupdate=(/1,0/),procname = updateSSH)
# endif
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
#  if defined VOL_REFLUX
      IF ( ln_dynspg_ts.AND.ln_bt_fw ) THEN
         ! Refluxing on ssh:
#  if defined DECAL_FEEDBACK
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/0, 0/),locupdate2=(/1, 1/),procname = reflux_sshu)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/1, 1/),locupdate2=(/0, 0/),procname = reflux_sshv)
#  else
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/-1,-1/),locupdate2=(/ 0, 0/),procname = reflux_sshu)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/ 0, 0/),locupdate2=(/-1,-1/),procname = reflux_sshv)
#  endif
      END IF
#  endif
      !
#endif
      !
   END SUBROUTINE Agrif_Update_ssh


   SUBROUTINE Agrif_Update_Tke( )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_Tke ***
      !!---------------------------------------------
      !!
      ! 
      IF (Agrif_Root()) RETURN
      !       
#  if defined TWO_WAY

      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.

      CALL Agrif_Update_Variable( en_id, locupdate=(/0,0/), procname=updateEN  )
      CALL Agrif_Update_Variable(avt_id, locupdate=(/0,0/), procname=updateAVT )
      CALL Agrif_Update_Variable(avm_id, locupdate=(/0,0/), procname=updateAVM )

      Agrif_UseSpecialValueInUpdate = .FALSE.

#  endif
      
   END SUBROUTINE Agrif_Update_Tke


   SUBROUTINE Agrif_Update_vvl( )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_vvl ***
      !!---------------------------------------------
      !
      IF (Agrif_Root()) RETURN
      !
#if defined TWO_WAY  
      !
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update e3 from grid Number',Agrif_Fixed(), 'Step', Agrif_Nb_Step()
      !
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.
      ! 
      ! No interface separation here, update vertical grid at T points 
      ! everywhere over the overlapping regions (one account for refluxing in that case):
      CALL Agrif_Update_Variable(e3t_id, procname=updatee3t) 
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
      CALL Agrif_ChildGrid_To_ParentGrid()
      CALL dom_vvl_update_UVF
      CALL Agrif_ParentGrid_To_ChildGrid()
      !
#endif
      !
   END SUBROUTINE Agrif_Update_vvl

   SUBROUTINE dom_vvl_update_UVF
      !!---------------------------------------------
      !!       *** ROUTINE dom_vvl_update_UVF ***
      !!---------------------------------------------
      !!
      INTEGER :: jk
      REAL(wp):: zcoef
      !!---------------------------------------------

      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Finalize e3 on grid Number', &
                  & Agrif_Fixed(), 'Step', Agrif_Nb_Step()

      ! Save "old" scale factor (prior update) for subsequent asselin correction
      ! of prognostic variables
      ! -----------------------
      !
      e3u_a(:,:,:) = e3u_n(:,:,:)
      e3v_a(:,:,:) = e3v_n(:,:,:)
!      ua(:,:,:) = e3u_b(:,:,:)
!      va(:,:,:) = e3v_b(:,:,:)
      hu_a(:,:) = hu_n(:,:)
      hv_a(:,:) = hv_n(:,:)

      ! 1) NOW fields
      !--------------
      
         ! Vertical scale factor interpolations
         ! ------------------------------------
      CALL dom_vvl_interpol( e3t_n(:,:,:), e3u_n(:,:,:) ,  'U' )
      CALL dom_vvl_interpol( e3t_n(:,:,:), e3v_n(:,:,:) ,  'V' )
      CALL dom_vvl_interpol( e3u_n(:,:,:), e3f_n(:,:,:) ,  'F' )

      CALL dom_vvl_interpol( e3u_n(:,:,:), e3uw_n(:,:,:), 'UW' )
      CALL dom_vvl_interpol( e3v_n(:,:,:), e3vw_n(:,:,:), 'VW' )

         ! Update total depths:
         ! --------------------
      hu_n(:,:) = 0._wp                        ! Ocean depth at U-points
      hv_n(:,:) = 0._wp                        ! Ocean depth at V-points
      DO jk = 1, jpkm1
         hu_n(:,:) = hu_n(:,:) + e3u_n(:,:,jk) * umask(:,:,jk)
         hv_n(:,:) = hv_n(:,:) + e3v_n(:,:,jk) * vmask(:,:,jk)
      END DO
      !                                        ! Inverse of the local depth
      r1_hu_n(:,:) = ssumask(:,:) / ( hu_n(:,:) + 1._wp - ssumask(:,:) )
      r1_hv_n(:,:) = ssvmask(:,:) / ( hv_n(:,:) + 1._wp - ssvmask(:,:) )


      ! 2) BEFORE fields:
      !------------------
      IF (.NOT.(lk_agrif_fstep.AND.(neuler==0) )) THEN
         !
         ! Vertical scale factor interpolations
         ! ------------------------------------
         CALL dom_vvl_interpol( e3t_b(:,:,:), e3u_b(:,:,:),  'U'  )
         CALL dom_vvl_interpol( e3t_b(:,:,:), e3v_b(:,:,:),  'V'  )

         CALL dom_vvl_interpol( e3u_b(:,:,:), e3uw_b(:,:,:), 'UW' )
         CALL dom_vvl_interpol( e3v_b(:,:,:), e3vw_b(:,:,:), 'VW' )

         ! Update total depths:
         ! --------------------
         hu_b(:,:) = 0._wp                     ! Ocean depth at U-points
         hv_b(:,:) = 0._wp                     ! Ocean depth at V-points
         DO jk = 1, jpkm1
            hu_b(:,:) = hu_b(:,:) + e3u_b(:,:,jk) * umask(:,:,jk)
            hv_b(:,:) = hv_b(:,:) + e3v_b(:,:,jk) * vmask(:,:,jk)
         END DO
         !                                     ! Inverse of the local depth
         r1_hu_b(:,:) = ssumask(:,:) / ( hu_b(:,:) + 1._wp - ssumask(:,:) )
         r1_hv_b(:,:) = ssvmask(:,:) / ( hv_b(:,:) + 1._wp - ssvmask(:,:) )
      ENDIF
      !
   END SUBROUTINE dom_vvl_update_UVF

#if defined key_vertical

   SUBROUTINE updateTS( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      !!           *** ROUTINE updateT ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,n1,n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji,jj,jk,jn
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk,n1:n2) :: tabres_child
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      INTEGER  :: N_in, N_out
      REAL(wp) :: zrho_xy, h_diff
      REAL(wp) :: tabin(k1:k2,n1:n2)
      !!---------------------------------------------
      !
      IF (before) THEN
         AGRIF_SpecialValue = -999._wp
         zrho_xy = Agrif_rhox() * Agrif_rhoy() 
         DO jn = n1,n2-1
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = (tsn(ji,jj,jk,jn) * e3t_n(ji,jj,jk) ) &
                                           * tmask(ji,jj,jk) + (tmask(ji,jj,jk)-1)*999._wp
                  END DO
               END DO
            END DO
         END DO
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,n2) = tmask(ji,jj,jk) * e3t_n(ji,jj,jk) &
                                           + (tmask(ji,jj,jk)-1)*999._wp
               END DO
            END DO
         END DO
      ELSE
         tabres_child(:,:,:,:) = 0.
         AGRIF_SpecialValue = 0._wp
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2 !k2 = jpk of child grid
                  IF (tabres(ji,jj,jk,n2) == 0  ) EXIT
                  N_in = N_in + 1
                  tabin(jk,:) = tabres(ji,jj,jk,n1:n2-1)/tabres(ji,jj,jk,n2)
                  h_in(N_in) = tabres(ji,jj,jk,n2)
               ENDDO
               N_out = 0
               DO jk=1,jpk ! jpk of parent grid
                  IF (tmask(ji,jj,jk) < -900) EXIT ! TODO: Will not work with ISF
                  N_out = N_out + 1
                  h_out(N_out) = e3t_n(ji,jj,jk) 
               ENDDO
               IF (N_in > 0) THEN !Remove this?
                  h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                  IF (h_diff < -1.e-4) THEN
                     print *,'CHECK YOUR bathy T points ...',ji,jj,h_diff,sum(h_in(1:N_in)),sum(h_out(1:N_out))
                     print *,h_in(1:N_in)
                     print *,h_out(1:N_out)
                     STOP
                  ENDIF
                  DO jn=n1,n2-1
                     CALL reconstructandremap(tabin(1:N_in,jn),h_in(1:N_in),tabres_child(ji,jj,1:N_out,jn),h_out(1:N_out),N_in,N_out)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN
            ! Add asselin part
            DO jn = n1,n2-1
               DO jk=1,jpk
                  DO jj=j1,j2
                     DO ji=i1,i2
                        IF( tabres_child(ji,jj,jk,jn) .NE. 0. ) THEN
                           tsb(ji,jj,jk,jn) = tsb(ji,jj,jk,jn) & 
                                 & + atfp * ( tabres_child(ji,jj,jk,jn) &
                                 &          - tsn(ji,jj,jk,jn) ) * tmask(ji,jj,jk)
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
         DO jn = n1,n2-1
            DO jk=1,jpk
               DO jj=j1,j2
                  DO ji=i1,i2
                     IF( tabres_child(ji,jj,jk,jn) .NE. 0. ) THEN 
                        tsn(ji,jj,jk,jn) = tabres_child(ji,jj,jk,jn) * tmask(ji,jj,jk)
                     END IF
                  END DO
               END DO
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE updateTS

# else

   SUBROUTINE updateTS( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateT ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,n1,n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji,jj,jk,jn
      REAL(wp) :: ztb, ztnu, ztno
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jn = 1,jpts
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
!> jc tmp
                     tabres(ji,jj,jk,jn) = tsn(ji,jj,jk,jn)  * e3t_n(ji,jj,jk) / e3t_0(ji,jj,jk)
!                     tabres(ji,jj,jk,jn) = tsn(ji,jj,jk,jn)  * e3t_n(ji,jj,jk)
!< jc tmp
                  END DO
               END DO
            END DO
         END DO
      ELSE
!> jc tmp
         DO jn = 1,jpts
            tabres(i1:i2,j1:j2,k1:k2,jn) =  tabres(i1:i2,j1:j2,k1:k2,jn) * e3t_0(i1:i2,j1:j2,k1:k2) &
                                         & * tmask(i1:i2,j1:j2,k1:k2)
         ENDDO
!< jc tmp
         IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN
            ! Add asselin part
            DO jn = 1,jpts
               DO jk = k1, k2
                  DO jj = j1, j2
                     DO ji = i1, i2
                        IF( tabres(ji,jj,jk,jn) /= 0._wp ) THEN
                           ztb  = tsb(ji,jj,jk,jn) * e3t_b(ji,jj,jk) ! fse3t_b prior update should be used
                           ztnu = tabres(ji,jj,jk,jn)
                           ztno = tsn(ji,jj,jk,jn) * e3t_a(ji,jj,jk)
                           tsb(ji,jj,jk,jn) = ( ztb + atfp * ( ztnu - ztno) )  & 
                                     &        * tmask(ji,jj,jk) / e3t_b(ji,jj,jk)
                        ENDIF
                     END DO
                  END DO
               END DO
            END DO
         ENDIF
         DO jn = 1,jpts
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     IF( tabres(ji,jj,jk,jn) /= 0._wp ) THEN 
                        tsn(ji,jj,jk,jn) = tabres(ji,jj,jk,jn) / e3t_n(ji,jj,jk)
                     END IF
                  END DO
               END DO
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            tsb(i1:i2,j1:j2,k1:k2,1:jpts)  = tsn(i1:i2,j1:j2,k1:k2,1:jpts)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updateTS

# endif

# if defined key_vertical

   SUBROUTINE updateu( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateu ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      !
      INTEGER ::   ji, jj, jk
      REAL(wp)::   zrhoy
! VERTICAL REFINEMENT BEGIN
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      INTEGER  :: N_in, N_out
      REAL(wp) :: h_diff, excess, thick
      REAL(wp) :: tabin(k1:k2)
! VERTICAL REFINEMENT END
      !!---------------------------------------------
      ! 
      IF( before ) THEN
         zrhoy = Agrif_Rhoy()
         AGRIF_SpecialValue = -999._wp
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,1) = zrhoy * e2u(ji,jj) * e3u_n(ji,jj,jk) * umask(ji,jj,jk) * un(ji,jj,jk)  &
                                       + (umask(ji,jj,jk)-1)*999._wp
                  tabres(ji,jj,jk,2) = zrhoy * umask(ji,jj,jk) * e2u(ji,jj) * e3u_n(ji,jj,jk)  &
                                       + (umask(ji,jj,jk)-1)*999._wp
               END DO
            END DO
         END DO
      ELSE
         tabres_child(:,:,:) = 0.
         AGRIF_SpecialValue = 0._wp
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               h_in(:) = 0._wp
               tabin(:) = 0._wp
               DO jk=k1,k2 !k2=jpk of child grid
                  IF( tabres(ji,jj,jk,2) < -900) EXIT
                  N_in = N_in + 1
                  tabin(jk) = tabres(ji,jj,jk,1)/tabres(ji,jj,jk,2)
                  h_in(N_in) = tabres(ji,jj,jk,2)/e2u(ji,jj)
               ENDDO
               N_out = 0
               DO jk=1,jpk
                  IF (umask(ji,jj,jk) == 0) EXIT
                  N_out = N_out + 1
                  h_out(N_out) = e3u_n(ji,jj,jk)
               ENDDO
               IF (N_in * N_out > 0) THEN
                  h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                  IF (h_diff < -1.e-4) THEN
!Even if bathy at T points match it's possible for the U points to be deeper in the child grid. 
!In this case we need to move transport from the child grid cells below bed of parent grid into the bottom cell.
                     excess = 0._wp
                     DO jk=N_in,1,-1
                        thick = MIN(-1*h_diff, h_in(jk))
                        excess = excess + tabin(jk)*thick*e2u(ji,jj)
                        tabin(jk) = tabin(jk)*(1. - thick/h_in(jk))
                        h_diff = h_diff + thick
                        IF ( h_diff == 0) THEN
                           N_in = jk
                           h_in(jk) = h_in(jk) - thick
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
                  CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out)
                  tabres_child(ji,jj,N_out) = tabres_child(ji,jj,N_out) + excess/(e2u(ji,jj)*h_out(N_out))
               ENDIF
            ENDDO
         ENDDO

         DO jk=1,jpk
            DO jj=j1,j2
               DO ji=i1,i2
                  IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN ! Add asselin part
                     ub(ji,jj,jk) = ub(ji,jj,jk) & 
                           & + atfp * ( tabres_child(ji,jj,jk) - un(ji,jj,jk) ) * umask(ji,jj,jk)
                  ENDIF
                  !
                  un(ji,jj,jk) = tabres_child(ji,jj,jk) * umask(ji,jj,jk)
               END DO
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE updateu

#else

   SUBROUTINE updateu( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateu ***
      !!---------------------------------------------
      INTEGER                               , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      !
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zrhoy, zub, zunu, zuno
      !!---------------------------------------------
      ! 
      IF( before ) THEN
         zrhoy = Agrif_Rhoy()
         DO jk = k1, k2
            tabres(i1:i2,j1:j2,jk,1) = zrhoy * e2u(i1:i2,j1:j2) * e3u_n(i1:i2,j1:j2,jk) * un(i1:i2,j1:j2,jk)
         END DO
      ELSE
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,1) = tabres(ji,jj,jk,1) * r1_e2u(ji,jj) 
                  !
                  IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN ! Add asselin part
                     zub  = ub(ji,jj,jk) * e3u_b(ji,jj,jk)  ! fse3t_b prior update should be used
                     zuno = un(ji,jj,jk) * e3u_a(ji,jj,jk)
                     zunu = tabres(ji,jj,jk,1)
                     ub(ji,jj,jk) = ( zub + atfp * ( zunu - zuno) ) &      
                                    & * umask(ji,jj,jk) / e3u_b(ji,jj,jk)
                  ENDIF
                  !
                  un(ji,jj,jk) = tabres(ji,jj,jk,1) * umask(ji,jj,jk) / e3u_n(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            ub(i1:i2,j1:j2,k1:k2)  = un(i1:i2,j1:j2,k1:k2)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updateu

# endif

   SUBROUTINE correct_u_bdy( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before, nb, ndir )
      !!---------------------------------------------
      !!           *** ROUTINE correct_u_bdy ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      INTEGER                                     , INTENT(in)    :: nb, ndir
      !!
      LOGICAL :: western_side, eastern_side 
      !
      INTEGER  :: jj, jk
      REAL(wp) :: zcor
      !!---------------------------------------------
      ! 
      IF( .NOT.before ) THEN
         !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         !
         IF (western_side) THEN
            DO jj=j1,j2
               zcor = un_b(i1-1,jj) * hu_a(i1-1,jj) * r1_hu_n(i1-1,jj) - un_b(i1-1,jj)
               un_b(i1-1,jj) = un_b(i1-1,jj) + zcor
               DO jk=1,jpkm1
                  un(i1-1,jj,jk) = un(i1-1,jj,jk) + zcor * umask(i1-1,jj,jk)
               END DO 
            END DO
         ENDIF
         !
         IF (eastern_side) THEN
            DO jj=j1,j2
               zcor = un_b(i2+1,jj) * hu_a(i2+1,jj) * r1_hu_n(i2+1,jj) - un_b(i2+1,jj)
               un_b(i2+1,jj) = un_b(i2+1,jj) + zcor
               DO jk=1,jpkm1
                  un(i2+1,jj,jk) = un(i2+1,jj,jk) + zcor * umask(i2+1,jj,jk)
               END DO 
            END DO
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE correct_u_bdy

# if  defined key_vertical

   SUBROUTINE updatev( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updatev ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zrhox
! VERTICAL REFINEMENT BEGIN
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      INTEGER :: N_in, N_out
      REAL(wp) :: h_diff, excess, thick
      REAL(wp) :: tabin(k1:k2)
! VERTICAL REFINEMENT END
      !!---------------------------------------------      
      !
      IF( before ) THEN
         zrhox = Agrif_Rhox()
         AGRIF_SpecialValue = -999._wp
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,1) = zrhox * e1v(ji,jj) * e3v_n(ji,jj,jk) * vmask(ji,jj,jk) * vn(ji,jj,jk) &
                                       + (vmask(ji,jj,jk)-1)*999._wp
                  tabres(ji,jj,jk,2) = vmask(ji,jj,jk) * zrhox * e1v(ji,jj) * e3v_n(ji,jj,jk) &
                                       + (vmask(ji,jj,jk)-1)*999._wp
               END DO
            END DO
         END DO
      ELSE
         tabres_child(:,:,:) = 0.
         AGRIF_SpecialValue = 0._wp
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2
                  IF (tabres(ji,jj,jk,2) < -900) EXIT
                  N_in = N_in + 1
                  tabin(jk) = tabres(ji,jj,jk,1)/tabres(ji,jj,jk,2)
                  h_in(N_in) = tabres(ji,jj,jk,2)/e1v(ji,jj)
               ENDDO
               N_out = 0
               DO jk=1,jpk
                  IF (vmask(ji,jj,jk) == 0) EXIT
                  N_out = N_out + 1
                  h_out(N_out) = e3v_n(ji,jj,jk)
               ENDDO
               IF (N_in * N_out > 0) THEN
                  h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                  IF (h_diff < -1.e-4) then
!Even if bathy at T points match it's possible for the U points to be deeper in the child grid. 
!In this case we need to move transport from the child grid cells below bed of parent grid into the bottom cell.
                     excess = 0._wp
                     DO jk=N_in,1,-1
                        thick = MIN(-1*h_diff, h_in(jk))
                        excess = excess + tabin(jk)*thick*e2u(ji,jj)
                        tabin(jk) = tabin(jk)*(1. - thick/h_in(jk))
                        h_diff = h_diff + thick
                        IF ( h_diff == 0) THEN
                           N_in = jk
                           h_in(jk) = h_in(jk) - thick
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
                  CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out)
                  tabres_child(ji,jj,N_out) = tabres_child(ji,jj,N_out) + excess/(e1v(ji,jj)*h_out(N_out))
               ENDIF
            ENDDO
         ENDDO

         DO jk=1,jpk
            DO jj=j1,j2
               DO ji=i1,i2
                  !
                  IF( .NOT.(lk_agrif_fstep.AND.(neuler==0)) ) THEN ! Add asselin part
                     vb(ji,jj,jk) = vb(ji,jj,jk) & 
                           & + atfp * ( tabres_child(ji,jj,jk) - vn(ji,jj,jk) ) * vmask(ji,jj,jk)
                  ENDIF
                  !
                  vn(ji,jj,jk) = tabres_child(ji,jj,jk) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE updatev

# else

   SUBROUTINE updatev( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before)
      !!---------------------------------------------
      !!           *** ROUTINE updatev ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      !
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zrhox, zvb, zvnu, zvno
      !!---------------------------------------------      
      !
      IF (before) THEN
         zrhox = Agrif_Rhox()
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,1) = zrhox * e1v(ji,jj) * e3v_n(ji,jj,jk) * vn(ji,jj,jk)
               END DO
            END DO
         END DO
      ELSE
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,1) = tabres(ji,jj,jk,1) * r1_e1v(ji,jj)
                  !
                  IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN ! Add asselin part
                     zvb  = vb(ji,jj,jk) * e3v_b(ji,jj,jk) ! fse3t_b prior update should be used
                     zvno = vn(ji,jj,jk) * e3v_a(ji,jj,jk)
                     zvnu = tabres(ji,jj,jk,1)
                     vb(ji,jj,jk) = ( zvb + atfp * ( zvnu - zvno) ) &      
                                    & * vmask(ji,jj,jk) / e3v_b(ji,jj,jk)
                  ENDIF
                  !
                  vn(ji,jj,jk) = tabres(ji,jj,jk,1) * vmask(ji,jj,jk) / e3v_n(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            vb(i1:i2,j1:j2,k1:k2)  = vn(i1:i2,j1:j2,k1:k2)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updatev

# endif

   SUBROUTINE correct_v_bdy( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before, nb, ndir )
      !!---------------------------------------------
      !!           *** ROUTINE correct_u_bdy ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      INTEGER                                     , INTENT(in)    :: nb, ndir
      !!
      LOGICAL :: southern_side, northern_side 
      !
      INTEGER  :: ji, jk
      REAL(wp) :: zcor
      !!---------------------------------------------
      ! 
      IF( .NOT.before ) THEN
         !
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         !
         IF (southern_side) THEN
            DO ji=i1,i2
               zcor = vn_b(ji,j1-1) * hv_a(ji,j1-1) * r1_hv_n(ji,j1-1) - vn_b(ji,j1-1)
               vn_b(ji,j1-1) = vn_b(ji,j1-1) + zcor
               DO jk=1,jpkm1
                  vn(ji,j1-1,jk) = vn(ji,j1-1,jk) + zcor * vmask(ji,j1-1,jk)
               END DO 
            END DO
         ENDIF
         !
         IF (northern_side) THEN
            DO ji=i1,i2
               zcor = vn_b(ji,j2+1) * hv_a(ji,j2+1) * r1_hv_n(ji,j2+1) - vn_b(ji,j2+1)
               vn_b(ji,j2+1) = vn_b(ji,j2+1) + zcor
               DO jk=1,jpkm1
                  vn(ji,j2+1,jk) = vn(ji,j2+1,jk) + zcor * vmask(ji,j2+1,jk)
               END DO 
            END DO
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE correct_v_bdy


   SUBROUTINE updateu2d( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateu2d ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !! 
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zrhoy
      REAL(wp) :: zcorr
      !!---------------------------------------------
      !
      IF( before ) THEN
         zrhoy = Agrif_Rhoy()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = zrhoy * un_b(ji,jj) * hu_n(ji,jj) * e2u(ji,jj)
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) =  tabres(ji,jj) * r1_e2u(ji,jj)  
               !    
               ! Update "now" 3d velocities:
               spgu(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  spgu(ji,jj) = spgu(ji,jj) + e3u_n(ji,jj,jk) * un(ji,jj,jk)
               END DO
               !
               zcorr = (tabres(ji,jj) - spgu(ji,jj)) * r1_hu_n(ji,jj)
               DO jk=1,jpkm1              
                  un(ji,jj,jk) = un(ji,jj,jk) + zcorr * umask(ji,jj,jk)           
               END DO
               !
               ! Update barotropic velocities:
               IF ( .NOT.ln_dynspg_ts .OR. (ln_dynspg_ts.AND.(.NOT.ln_bt_fw)) ) THEN
                  IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN ! Add asselin part
                     zcorr = (tabres(ji,jj) - un_b(ji,jj) * hu_a(ji,jj)) * r1_hu_b(ji,jj)
                     ub_b(ji,jj) = ub_b(ji,jj) + atfp * zcorr * umask(ji,jj,1)
                  END IF
               ENDIF    
               un_b(ji,jj) = tabres(ji,jj) * r1_hu_n(ji,jj) * umask(ji,jj,1)
               !       
               ! Correct "before" velocities to hold correct bt component:
               spgu(ji,jj) = 0.e0
               DO jk=1,jpkm1
                  spgu(ji,jj) = spgu(ji,jj) + e3u_b(ji,jj,jk) * ub(ji,jj,jk)
               END DO
               !
               zcorr = ub_b(ji,jj) - spgu(ji,jj) * r1_hu_b(ji,jj)
               DO jk=1,jpkm1              
                  ub(ji,jj,jk) = ub(ji,jj,jk) + zcorr * umask(ji,jj,jk)           
               END DO
               !
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            ub_b(i1:i2,j1:j2)  = un_b(i1:i2,j1:j2)
         ENDIF
      ENDIF
      !
   END SUBROUTINE updateu2d


   SUBROUTINE updatev2d( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updatev2d ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      ! 
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zrhox, zcorr
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         zrhox = Agrif_Rhox()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = zrhox * vn_b(ji,jj) * hv_n(ji,jj) * e1v(ji,jj) 
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) =  tabres(ji,jj) * r1_e1v(ji,jj)  
               !    
               ! Update "now" 3d velocities:
               spgv(ji,jj) = 0.e0
               DO jk=1,jpkm1
                  spgv(ji,jj) = spgv(ji,jj) + e3v_n(ji,jj,jk) * vn(ji,jj,jk)
               END DO
               !
               zcorr = (tabres(ji,jj) - spgv(ji,jj)) * r1_hv_n(ji,jj)
               DO jk=1,jpkm1              
                  vn(ji,jj,jk) = vn(ji,jj,jk) + zcorr * vmask(ji,jj,jk)           
               END DO
               !
               ! Update barotropic velocities:
               IF ( .NOT.ln_dynspg_ts .OR. (ln_dynspg_ts.AND.(.NOT.ln_bt_fw)) ) THEN
                  IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN ! Add asselin part
                     zcorr = (tabres(ji,jj) - vn_b(ji,jj) * hv_a(ji,jj)) * r1_hv_b(ji,jj)
                     vb_b(ji,jj) = vb_b(ji,jj) + atfp * zcorr * vmask(ji,jj,1)
                  END IF
               ENDIF              
               vn_b(ji,jj) = tabres(ji,jj) * r1_hv_n(ji,jj) * vmask(ji,jj,1)
               !       
               ! Correct "before" velocities to hold correct bt component:
               spgv(ji,jj) = 0.e0
               DO jk=1,jpkm1
                  spgv(ji,jj) = spgv(ji,jj) + e3v_b(ji,jj,jk) * vb(ji,jj,jk)
               END DO
               !
               zcorr = vb_b(ji,jj) - spgv(ji,jj) * r1_hv_b(ji,jj)
               DO jk=1,jpkm1              
                  vb(ji,jj,jk) = vb(ji,jj,jk) + zcorr * vmask(ji,jj,jk)           
               END DO
               !
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            vb_b(i1:i2,j1:j2)  = vn_b(i1:i2,j1:j2)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updatev2d


   SUBROUTINE updateSSH( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updateSSH ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      INTEGER :: ji, jj
      !!----------------------------------------------------------------------
      ! 
      IF( before ) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = sshn(ji,jj)
            END DO
         END DO
      ELSE
         IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN
            DO jj=j1,j2
               DO ji=i1,i2
                  sshb(ji,jj) =   sshb(ji,jj) &
                        & + atfp * ( tabres(ji,jj) - sshn(ji,jj) ) * tmask(ji,jj,1)
               END DO
            END DO
         ENDIF
         !
         DO jj=j1,j2
            DO ji=i1,i2
               sshn(ji,jj) = tabres(ji,jj) * tmask(ji,jj,1)
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            sshb(i1:i2,j1:j2)  = sshn(i1:i2,j1:j2)
         ENDIF
         !

      ENDIF
      !
   END SUBROUTINE updateSSH


   SUBROUTINE updateub2b( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateub2b ***
      !!----------------------------------------------------------------------
      INTEGER                            , INTENT(in) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                            , INTENT(in) ::   before
      !!
      INTEGER :: ji, jj
      REAL(wp) :: zrhoy, za1, zcor
      !!---------------------------------------------
      !
      IF (before) THEN
         zrhoy = Agrif_Rhoy()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = ub2_i_b(ji,jj) * e2u(ji,jj)
            END DO
         END DO
         tabres = zrhoy * tabres
      ELSE
         ! 
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_e2u(i1:i2,j1:j2)
         !
         za1 = 1._wp / REAL(Agrif_rhot(), wp)
         DO jj=j1,j2
            DO ji=i1,i2
               zcor=tabres(ji,jj) - ub2_b(ji,jj)
               ! Update time integrated fluxes also in case of multiply nested grids:
               ub2_i_b(ji,jj) = ub2_i_b(ji,jj) + za1 * zcor 
               ! Update corrective fluxes:
               un_bf(ji,jj)  = un_bf(ji,jj) + zcor
               ! Update half step back fluxes:
               ub2_b(ji,jj) = tabres(ji,jj)
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE updateub2b

   SUBROUTINE reflux_sshu( tabres, i1, i2, j1, j2, before, nb, ndir )
      !!---------------------------------------------
      !!          *** ROUTINE reflux_sshu ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb, ndir
      !!
      LOGICAL :: western_side, eastern_side 
      INTEGER :: ji, jj
      REAL(wp) :: zrhoy, za1, zcor
      !!---------------------------------------------
      !
      IF (before) THEN
         zrhoy = Agrif_Rhoy()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = ub2_i_b(ji,jj) * e2u(ji,jj)
            END DO
         END DO
         tabres = zrhoy * tabres
      ELSE
         ! 
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_e2u(i1:i2,j1:j2)
         !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         !
         IF (western_side) THEN
            DO jj=j1,j2
               zcor = rdt * r1_e1e2t(i1  ,jj) * e2u(i1,jj) * (ub2_b(i1,jj)-tabres(i1,jj)) 
               sshn(i1  ,jj) = sshn(i1  ,jj) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) sshb(i1  ,jj) = sshb(i1  ,jj) + atfp * zcor
            END DO
         ENDIF
         IF (eastern_side) THEN
            DO jj=j1,j2
               zcor = - rdt * r1_e1e2t(i2+1,jj) * e2u(i2,jj) * (ub2_b(i2,jj)-tabres(i2,jj))
               sshn(i2+1,jj) = sshn(i2+1,jj) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) sshb(i2+1,jj) = sshb(i2+1,jj) + atfp * zcor
            END DO
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE reflux_sshu

   SUBROUTINE updatevb2b( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updatevb2b ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      INTEGER :: ji, jj
      REAL(wp) :: zrhox, za1, zcor
      !!---------------------------------------------
      !
      IF( before ) THEN
         zrhox = Agrif_Rhox()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = vb2_i_b(ji,jj) * e1v(ji,jj) 
            END DO
         END DO
         tabres = zrhox * tabres
      ELSE
         ! 
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_e1v(i1:i2,j1:j2)
         !
         za1 = 1._wp / REAL(Agrif_rhot(), wp)
         DO jj=j1,j2
            DO ji=i1,i2
               zcor=tabres(ji,jj) - vb2_b(ji,jj)
               ! Update time integrated fluxes also in case of multiply nested grids:
               vb2_i_b(ji,jj) = vb2_i_b(ji,jj) + za1 * zcor 
               ! Update corrective fluxes:
               vn_bf(ji,jj)  = vn_bf(ji,jj) + zcor
               ! Update half step back fluxes:
               vb2_b(ji,jj) = tabres(ji,jj)
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE updatevb2b

   SUBROUTINE reflux_sshv( tabres, i1, i2, j1, j2, before, nb, ndir )
      !!---------------------------------------------
      !!          *** ROUTINE reflux_sshv ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb, ndir
      !!
      LOGICAL :: southern_side, northern_side 
      INTEGER :: ji, jj
      REAL(wp) :: zrhox, za1, zcor
      !!---------------------------------------------
      !
      IF (before) THEN
         zrhox = Agrif_Rhox()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = vb2_i_b(ji,jj) * e1v(ji,jj) 
            END DO
         END DO
         tabres = zrhox * tabres
      ELSE
         ! 
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_e1v(i1:i2,j1:j2)
         !
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         !
         IF (southern_side) THEN
            DO ji=i1,i2
               zcor = rdt * r1_e1e2t(ji,j1  ) * e1v(ji,j1  ) * (vb2_b(ji,j1)-tabres(ji,j1))
               sshn(ji,j1  ) = sshn(ji,j1  ) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) sshb(ji,j1  ) = sshb(ji,j1) + atfp * zcor
            END DO
         ENDIF
         IF (northern_side) THEN               
            DO ji=i1,i2
               zcor = - rdt * r1_e1e2t(ji,j2+1) * e1v(ji,j2  ) * (vb2_b(ji,j2)-tabres(ji,j2))
               sshn(ji,j2+1) = sshn(ji,j2+1) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) sshb(ji,j2+1) = sshb(ji,j2+1) + atfp * zcor
            END DO
         ENDIF
         ! 
      ENDIF
      !
   END SUBROUTINE reflux_sshv

   SUBROUTINE update_scales( tabres, i1, i2, j1, j2, k1, k2, n1,n2, before )
      !
      ! ====>>>>>>>>>>    currently not used
      !
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateT ***
      !!----------------------------------------------------------------------
      INTEGER                                    , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   tabres
      LOGICAL                                    , INTENT(in   ) ::   before
      !!
      INTEGER :: ji,jj,jk
      REAL(wp) :: ztemp
      !!----------------------------------------------------------------------

      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,1) = e1t(ji,jj)*e2t(ji,jj)*tmask(ji,jj,jk)
                  tabres(ji,jj,jk,2) = e1t(ji,jj)*tmask(ji,jj,jk)
                  tabres(ji,jj,jk,3) = e2t(ji,jj)*tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         tabres(:,:,:,1)=tabres(:,:,:,1)*Agrif_Rhox()*Agrif_Rhoy()
         tabres(:,:,:,2)=tabres(:,:,:,2)*Agrif_Rhox()
         tabres(:,:,:,3)=tabres(:,:,:,3)*Agrif_Rhoy()
      ELSE
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF( tabres(ji,jj,jk,1) .NE. 0. ) THEN 
                     print *,'VAL = ',ji,jj,jk,tabres(ji,jj,jk,1),e1t(ji,jj)*e2t(ji,jj)*tmask(ji,jj,jk)
                     print *,'VAL2 = ',ji,jj,jk,tabres(ji,jj,jk,2),e1t(ji,jj)*tmask(ji,jj,jk)
                     print *,'VAL3 = ',ji,jj,jk,tabres(ji,jj,jk,3),e2t(ji,jj)*tmask(ji,jj,jk)
                     ztemp = sqrt(tabres(ji,jj,jk,1)/(tabres(ji,jj,jk,2)*tabres(ji,jj,jk,3)))
                     print *,'CORR = ',ztemp-1.
                     print *,'NEW VALS = ',tabres(ji,jj,jk,2)*ztemp,tabres(ji,jj,jk,3)*ztemp, &
                           tabres(ji,jj,jk,2)*ztemp*tabres(ji,jj,jk,3)*ztemp
                     e1t(ji,jj) = tabres(ji,jj,jk,2)*ztemp
                     e2t(ji,jj) = tabres(ji,jj,jk,3)*ztemp
                  END IF
               END DO
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE update_scales


   SUBROUTINE updateEN( ptab, i1, i2, j1, j2, k1, k2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateen ***
      !!----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         ptab (i1:i2,j1:j2,k1:k2) = en(i1:i2,j1:j2,k1:k2)
      ELSE
         en(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2) 
      ENDIF
      !
   END SUBROUTINE updateEN


   SUBROUTINE updateAVT( ptab, i1, i2, j1, j2, k1, k2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateavt ***
      !!----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN   ;   ptab (i1:i2,j1:j2,k1:k2) = avt_k(i1:i2,j1:j2,k1:k2)
      ELSE                ;   avt_k(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2) 
      ENDIF
      !
   END SUBROUTINE updateAVT


   SUBROUTINE updateAVM( ptab, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateavm ***
      !!----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN   ;   ptab (i1:i2,j1:j2,k1:k2) = avm_k(i1:i2,j1:j2,k1:k2)
      ELSE                ;   avm_k(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2) 
      ENDIF
      !
   END SUBROUTINE updateAVM

   SUBROUTINE updatee3t(ptab_dum, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updatee3t ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2) :: ptab_dum
      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      LOGICAL, INTENT(in) :: before
      !
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: ptab
      INTEGER :: ji,jj,jk
      REAL(wp) :: zcoef
      !!---------------------------------------------
      !
      IF (.NOT.before) THEN
         !
         ALLOCATE(ptab(i1:i2,j1:j2,1:jpk)) 
         !
         ! Update e3t from ssh (z* case only)
         DO jk = 1, jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,jk) = e3t_0(ji,jj,jk) * (1._wp + sshn(ji,jj) &
                                     & *ssmask(ji,jj)/(ht_0(ji,jj)-1._wp + ssmask(ji,jj)))
               END DO
            END DO
         END DO
         !
         ! 1) Updates at BEFORE time step:
         ! -------------------------------
         !
         ! Save "old" scale factor (prior update) for subsequent asselin correction
         ! of prognostic variables
         e3t_a(i1:i2,j1:j2,1:jpkm1) = e3t_n(i1:i2,j1:j2,1:jpkm1)

         ! One should also save e3t_b, but lacking of workspace...
!         hdivn(i1:i2,j1:j2,1:jpkm1)   = e3t_b(i1:i2,j1:j2,1:jpkm1)

         IF (.NOT.(lk_agrif_fstep.AND.(neuler==0) )) THEN
            DO jk = 1, jpkm1
               DO jj=j1,j2
                  DO ji=i1,i2
                     e3t_b(ji,jj,jk) =  e3t_b(ji,jj,jk) &
                           & + atfp * ( ptab(ji,jj,jk) - e3t_n(ji,jj,jk) )
                  END DO
               END DO
            END DO
            !
            e3w_b  (i1:i2,j1:j2,1) = e3w_0(i1:i2,j1:j2,1) + e3t_b(i1:i2,j1:j2,1) - e3t_0(i1:i2,j1:j2,1)
            gdepw_b(i1:i2,j1:j2,1) = 0.0_wp
            gdept_b(i1:i2,j1:j2,1) = 0.5_wp * e3w_b(i1:i2,j1:j2,1)
            !
            DO jk = 2, jpk
               DO jj = j1,j2
                  DO ji = i1,i2            
                     zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))
                     e3w_b(ji,jj,jk)  = e3w_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * tmask(ji,jj,jk) ) *        & 
                     &                                        ( e3t_b(ji,jj,jk-1) - e3t_0(ji,jj,jk-1) )  &
                     &                                  +            0.5_wp * tmask(ji,jj,jk)   *        &
                     &                                        ( e3t_b(ji,jj,jk  ) - e3t_0(ji,jj,jk  ) )
                     gdepw_b(ji,jj,jk) = gdepw_b(ji,jj,jk-1) + e3t_b(ji,jj,jk-1)
                     gdept_b(ji,jj,jk) =      zcoef  * ( gdepw_b(ji,jj,jk  ) + 0.5 * e3w_b(ji,jj,jk))  &
                         &               + (1-zcoef) * ( gdept_b(ji,jj,jk-1) +       e3w_b(ji,jj,jk)) 
                  END DO
               END DO
            END DO
            !
         ENDIF        
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         !
         ! Update vertical scale factor at T-points:
         e3t_n(i1:i2,j1:j2,1:jpkm1) = ptab(i1:i2,j1:j2,1:jpkm1)
         !
         ! Update total depth:
         ht_n(i1:i2,j1:j2) = 0._wp
         DO jk = 1, jpkm1
            ht_n(i1:i2,j1:j2) = ht_n(i1:i2,j1:j2) + e3t_n(i1:i2,j1:j2,jk) * tmask(i1:i2,j1:j2,jk)
         END DO
         !
         ! Update vertical scale factor at W-points and depths:
         e3w_n (i1:i2,j1:j2,1) = e3w_0(i1:i2,j1:j2,1) + e3t_n(i1:i2,j1:j2,1) - e3t_0(i1:i2,j1:j2,1)
         gdept_n(i1:i2,j1:j2,1) = 0.5_wp * e3w_n(i1:i2,j1:j2,1)
         gdepw_n(i1:i2,j1:j2,1) = 0.0_wp
         gde3w_n(i1:i2,j1:j2,1) = gdept_n(i1:i2,j1:j2,1) - (ht_n(i1:i2,j1:j2)-ht_0(i1:i2,j1:j2)) ! Last term in the rhs is ssh
         !
         DO jk = 2, jpk
            DO jj = j1,j2
               DO ji = i1,i2            
               zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))
               e3w_n(ji,jj,jk)  = e3w_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * tmask(ji,jj,jk) ) * ( e3t_n(ji,jj,jk-1) - e3t_0(ji,jj,jk-1) )   &
               &                                  +            0.5_wp * tmask(ji,jj,jk)   * ( e3t_n(ji,jj,jk  ) - e3t_0(ji,jj,jk  ) )
               gdepw_n(ji,jj,jk) = gdepw_n(ji,jj,jk-1) + e3t_n(ji,jj,jk-1)
               gdept_n(ji,jj,jk) =      zcoef  * ( gdepw_n(ji,jj,jk  ) + 0.5 * e3w_n(ji,jj,jk))  &
                   &               + (1-zcoef) * ( gdept_n(ji,jj,jk-1) +       e3w_n(ji,jj,jk)) 
               gde3w_n(ji,jj,jk) = gdept_n(ji,jj,jk) - (ht_n(ji,jj)-ht_0(ji,jj)) ! Last term in the rhs is ssh
               END DO
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            e3t_b (i1:i2,j1:j2,1:jpk)  = e3t_n (i1:i2,j1:j2,1:jpk)
            e3w_b (i1:i2,j1:j2,1:jpk)  = e3w_n (i1:i2,j1:j2,1:jpk)
            gdepw_b(i1:i2,j1:j2,1:jpk) = gdepw_n(i1:i2,j1:j2,1:jpk)
            gdept_b(i1:i2,j1:j2,1:jpk) = gdept_n(i1:i2,j1:j2,1:jpk)
         ENDIF
         !
         DEALLOCATE(ptab)
      ENDIF
      !
   END SUBROUTINE updatee3t

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_oce_update_empty
      WRITE(*,*)  'agrif_oce_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_oce_update_empty
#endif

   !!======================================================================
END MODULE agrif_oce_update

