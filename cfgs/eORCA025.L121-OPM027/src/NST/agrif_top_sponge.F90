#define SPONGE_TOP

MODULE agrif_top_sponge
   !!======================================================================
   !!                ***  MODULE agrif_top_sponge  ***
   !! AGRIF :   sponge layer pakage for passive tracers (TOP)
   !!======================================================================
   !! History :  2.0  ! 2006-08  (R. Benshila, L. Debreu)  Original code
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_top
   !!----------------------------------------------------------------------
   !!   Agrif_Sponge_trc : 
   !!   interptrn_sponge :  
   !!----------------------------------------------------------------------
   USE par_oce
   USE par_trc
   USE oce
   USE trc
   USE dom_oce
   USE agrif_oce
   USE agrif_oce_sponge
   !
   USE in_out_manager
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Sponge_trc, interptrn_sponge

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_top_sponge.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Sponge_trc
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Sponge_Trc ***
      !!----------------------------------------------------------------------
      REAL(wp) ::   zcoef   ! local scalar
      !!----------------------------------------------------------------------
      !
#if defined SPONGE_TOP
!! Assume persistence 
      zcoef = REAL(Agrif_rhot()-1,wp)/REAL(Agrif_rhot())
      CALL Agrif_sponge
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .TRUE.
      tabspongedone_trn     = .FALSE.
      CALL Agrif_Bc_Variable( trn_sponge_id, calledweight=zcoef, procname=interptrn_sponge )
      Agrif_UseSpecialValue = .FALSE.
#endif
      !
   END SUBROUTINE Agrif_Sponge_Trc


   SUBROUTINE interptrn_sponge( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE interptrn_sponge ***
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   tabres
      LOGICAL                                     , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   zabe1, zabe2
      REAL(wp), DIMENSION(i1:i2,j1:j2)             ::   ztu, ztv
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2) ::   trbdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk,n1:n2) ::tabres_child
      REAL(wp), DIMENSION(k1:k2,n1:n2-1) :: tabin
      REAL(wp), DIMENSION(k1:k2) :: h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER :: N_in, N_out
      REAL(wp) :: h_diff
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         DO jn = 1, jptra
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = trb(ji,jj,jk,jn)
                  END DO
               END DO
            END DO
         END DO

# if defined key_vertical
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,jpts+1) = tmask(ji,jj,jk) * e3t_n(ji,jj,jk) 
               END DO
            END DO
         END DO
# endif
      ELSE      
# if defined key_vertical
         tabres_child(:,:,:,:) = 0.
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2 !k2 = jpk of parent grid
                  IF (tabres(ji,jj,jk,n2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk,:) = tabres(ji,jj,jk,n1:n2-1)
                  h_in(N_in) = tabres(ji,jj,jk,n2)
               END DO
               N_out = 0
               DO jk=1,jpk ! jpk of child grid
                  IF (tmask(ji,jj,jk) == 0) EXIT 
                  N_out = N_out + 1
                  h_out(jk) = e3t_n(ji,jj,jk) !Child grid scale factors. Could multiply by e1e2t here instead of division above
               ENDDO
               IF (N_in > 0) THEN
                  h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                  tabres(ji,jj,k2,:) = tabres(ji,jj,k2-1,:) !what is this line for?????
                  DO jn=1,jptra
                     call reconstructandremap(tabin(1:N_in,jn),h_in,tabres_child(ji,jj,1:N_out,jn),h_out,N_in,N_out)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
# endif

         DO jj=j1,j2
            DO ji=i1,i2
               DO jk=1,jpkm1
# if defined key_vertical
                  trbdiff(ji,jj,jk,1:jptra) = trb(ji,jj,jk,1:jptra) - tabres_child(ji,jj,jk,1:jptra)
# else
                  trbdiff(ji,jj,jk,1:jptra) = trb(ji,jj,jk,1:jptra) - tabres(ji,jj,jk,1:jptra)
# endif
               ENDDO
            ENDDO
         ENDDO

         DO jn = 1, jptra
            DO jk = 1, jpkm1
               DO jj = j1,j2-1
                  DO ji = i1,i2-1
                     zabe1 = fsaht_spu(ji,jj) * e2_e1u(ji,jj) * e3u_n(ji,jj,jk) * umask(ji,jj,jk)
                     zabe2 = fsaht_spv(ji,jj) * e1_e2v(ji,jj) * e3v_n(ji,jj,jk) * vmask(ji,jj,jk)
                     ztu(ji,jj) = zabe1 * ( trbdiff(ji+1,jj  ,jk,jn) - trbdiff(ji,jj,jk,jn) )
                     ztv(ji,jj) = zabe2 * ( trbdiff(ji  ,jj+1,jk,jn) - trbdiff(ji,jj,jk,jn) )
                  END DO
               END DO
               !
               DO jj = j1+1,j2-1
                  DO ji = i1+1,i2-1
                     IF( .NOT. tabspongedone_trn(ji,jj) ) THEN 
                        tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + (  ztu(ji,jj) - ztu(ji-1,jj  )     &
                           &                                   + ztv(ji,jj) - ztv(ji  ,jj-1)  )  &
                           &                                * r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk)
                     ENDIF
                  END DO
               END DO
            END DO
            !
         END DO
         !
         tabspongedone_trn(i1+1:i2-1,j1+1:j2-1) = .TRUE.
      ENDIF
      !                 
   END SUBROUTINE interptrn_sponge

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                           no TOP AGRIF
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_top_sponge_empty
      WRITE(*,*)  'agrif_top_sponge : You should not have seen this print! error?'
   END SUBROUTINE agrif_top_sponge_empty
#endif

   !!======================================================================
END MODULE agrif_top_sponge
