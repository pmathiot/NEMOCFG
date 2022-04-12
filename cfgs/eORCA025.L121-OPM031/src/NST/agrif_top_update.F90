#define TWO_WAY
#undef DECAL_FEEDBACK

MODULE agrif_top_update
   !!======================================================================
   !!                ***  MODULE agrif_top_update  ***
   !! AGRIF :   update package for passive tracers (TOP) 
   !!======================================================================
   !! History :  
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_top
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!   'key_TOP'                                           on-line tracers
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce
   USE agrif_oce
   USE par_trc
   USE trc

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Update_Trc

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_top_update.F90 11078 2019-06-05 14:17:09Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Update_Trc( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Update_Trc ***
      !!----------------------------------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN 
      !
#if defined TWO_WAY   
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid    = 0._wp
      ! 
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(trn_id, procname=updateTRC )
!      CALL Agrif_Update_Variable( trn_id, locupdate=(/0,2/), procname=updateTRC )
# else
      CALL Agrif_Update_Variable(trn_id, locupdate=(/1,0/),procname=updateTRC )
!      CALL Agrif_Update_Variable( trn_id, locupdate=(/1,2/), procname=updateTRC )
# endif
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
#endif
      !
   END SUBROUTINE Agrif_Update_Trc

#ifdef key_vertical
   SUBROUTINE updateTRC( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
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
      REAL(wp) :: h_diff
      REAL(wp) :: zrho_xy
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
                     tabres(ji,jj,jk,jn) = (trn(ji,jj,jk,jn) * e3t_n(ji,jj,jk) ) &
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
                  h_out(N_out) = e3t_n(ji,jj,jk) !Parent grid scale factors. Could multiply by e1e2t here instead of division above
               ENDDO
               IF (N_in > 0) THEN !Remove this?
                  h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                  IF (h_diff < -1.e-4) THEN
                     print *,'CHECK YOUR bathy T points ...',ji,jj,h_diff,sum(h_in(1:N_in)),sum(h_out(1:N_out))
                     print *,h_in(1:N_in)
                     print *,h_out(1:N_out)
                     STOP
                  ENDIF
                  DO jn=1,jptra
                     CALL reconstructandremap(tabin(1:N_in,jn),h_in(1:N_in),tabres_child(ji,jj,1:N_out,jn),h_out(1:N_out),N_in,N_out)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN
            ! Add asselin part
            DO jn = 1,jptra
               DO jk=1,jpk
                  DO jj=j1,j2
                     DO ji=i1,i2
                        IF( tabres_child(ji,jj,jk,jn) .NE. 0. ) THEN
                           trb(ji,jj,jk,jn) = trb(ji,jj,jk,jn) & 
                                 & + atfp * ( tabres_child(ji,jj,jk,jn) &
                                 &          - trn(ji,jj,jk,jn) ) * tmask(ji,jj,jk)
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
         DO jn = 1,jptra
            DO jk=1,jpk
               DO jj=j1,j2
                  DO ji=i1,i2
                     IF( tabres_child(ji,jj,jk,jn) .NE. 0. ) THEN 
                        trn(ji,jj,jk,jn) = tabres_child(ji,jj,jk,jn) * tmask(ji,jj,jk)
                     END IF
                  END DO
               END DO
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE updateTRC


#else
   SUBROUTINE updateTRC( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateTRC ***
      !!----------------------------------------------------------------------
      INTEGER                                    , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   tabres
      LOGICAL                                    , INTENT(in   ) ::   before
      !!
      INTEGER :: ji,jj,jk,jn
      REAL(wp) :: ztb, ztnu, ztno
      !!----------------------------------------------------------------------
      !
      !
      IF (before) THEN
         DO jn = n1,n2
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
!> jc tmp
                     tabres(ji,jj,jk,jn) = trn(ji,jj,jk,jn)  * e3t_n(ji,jj,jk) / e3t_0(ji,jj,jk)
!                     tabres(ji,jj,jk,jn) = trn(ji,jj,jk,jn)  * e3t_n(ji,jj,jk)
!< jc tmp
                  END DO
               END DO
            END DO
         END DO
      ELSE
!> jc tmp
         DO jn = n1,n2
            tabres(i1:i2,j1:j2,k1:k2,jn) =  tabres(i1:i2,j1:j2,k1:k2,jn) * e3t_0(i1:i2,j1:j2,k1:k2) &
                                         & * tmask(i1:i2,j1:j2,k1:k2)
         ENDDO
!< jc tmp
         IF (.NOT.(lk_agrif_fstep.AND.(neuler==0))) THEN
            ! Add asselin part
            DO jn = n1,n2
               DO jk=k1,k2
                  DO jj=j1,j2
                     DO ji=i1,i2
                        IF( tabres(ji,jj,jk,jn) .NE. 0. ) THEN
                           ztb  = trb(ji,jj,jk,jn) * e3t_b(ji,jj,jk) ! fse3t_b prior update should be used
                           ztnu = tabres(ji,jj,jk,jn)
                           ztno = trn(ji,jj,jk,jn) * e3t_a(ji,jj,jk)
                           trb(ji,jj,jk,jn) = ( ztb + atfp * ( ztnu - ztno) )  & 
                                     &        * tmask(ji,jj,jk) / e3t_b(ji,jj,jk)
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
         DO jn = n1,n2
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     IF( tabres(ji,jj,jk,jn) .NE. 0. ) THEN 
                        trn(ji,jj,jk,jn) = tabres(ji,jj,jk,jn) / e3t_n(ji,jj,jk)
                     END IF
                  END DO
               END DO
            END DO
         END DO
         !
         IF  ((neuler==0).AND.(Agrif_Nb_Step()==0) ) THEN
            trb(i1:i2,j1:j2,k1:k2,n1:n2)  = trn(i1:i2,j1:j2,k1:k2,n1:n2)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updateTRC
#endif

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                           no TOP AGRIF
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_top_update_empty
      WRITE(*,*)  'agrif_top_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_top_update_empty
#endif

   !!======================================================================
END MODULE agrif_top_update
