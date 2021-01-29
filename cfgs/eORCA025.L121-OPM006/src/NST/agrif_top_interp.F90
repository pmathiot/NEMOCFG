MODULE agrif_top_interp
   !!======================================================================
   !!                   ***  MODULE  agrif_top_interp  ***
   !! AGRIF: interpolation package for TOP
   !!======================================================================
   !! History :  2.0  !  ??? 
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_top
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!   'key_top'                                           on-line tracers
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce      
   USE agrif_oce
   USE agrif_top_sponge
   USE par_trc
   USE trc
   !
   USE lib_mpp     ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_trc, interptrn

  !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_top_interp.F90 12737 2020-04-10 17:55:11Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_trc
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE Agrif_trc  ***
      !!----------------------------------------------------------------------
      !
      IF( Agrif_Root() )   RETURN
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .TRUE.
      !
      CALL Agrif_Bc_variable( trn_id, procname=interptrn )
      Agrif_UseSpecialValue = .FALSE.
      !
   END SUBROUTINE Agrif_trc

   SUBROUTINE interptrn( ptab, i1, i2, j1, j2, k1, k2, n1, n2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interptrn ***
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   ptab
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      LOGICAL                                     , INTENT(in   ) ::   before
      INTEGER                                     , INTENT(in   ) ::   nb , ndir
      !
      INTEGER  ::   ji, jj, jk, jn, iref, jref, ibdy, jbdy   ! dummy loop indices
      INTEGER  ::   imin, imax, jmin, jmax, N_in, N_out
      REAL(wp) ::   zrho, z1, z2, z3, z4, z5, z6, z7
      LOGICAL :: western_side, eastern_side,northern_side,southern_side
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk,n1:n2) :: ptab_child
      REAL(wp), DIMENSION(k1:k2,n1:n2-1) :: tabin
      REAL(wp), DIMENSION(k1:k2) :: h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      REAL(wp) :: h_diff

      IF( before ) THEN         
         DO jn = 1,jptra
            DO jk=k1,k2
               DO jj=j1,j2
                 DO ji=i1,i2
                       ptab(ji,jj,jk,jn) = trn(ji,jj,jk,jn)
                 END DO
              END DO
           END DO
        END DO

# if defined key_vertical
        DO jk=k1,k2
           DO jj=j1,j2
              DO ji=i1,i2
                 ptab(ji,jj,jk,jptra+1) = tmask(ji,jj,jk) * e3t_n(ji,jj,jk) 
              END DO
           END DO
        END DO
# endif
      ELSE 

         western_side  = (nb == 1).AND.(ndir == 1)   ;   eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)   ;   northern_side = (nb == 2).AND.(ndir == 2)

# if defined key_vertical              
         DO jj=j1,j2
            DO ji=i1,i2
               iref = ji
               jref = jj
               if(western_side) iref=MAX(2,ji)
               if(eastern_side) iref=MIN(nlci-1,ji)
               if(southern_side) jref=MAX(2,jj)
               if(northern_side) jref=MIN(nlcj-1,jj)
               N_in = 0
               DO jk=k1,k2 !k2 = jpk of parent grid
                  IF (ptab(ji,jj,jk,n2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk,:) = ptab(ji,jj,jk,n1:n2-1)
                  h_in(N_in) = ptab(ji,jj,jk,n2)
               END DO
               N_out = 0
               DO jk=1,jpk ! jpk of child grid
                  IF (tmask(iref,jref,jk) == 0) EXIT 
                  N_out = N_out + 1
                  h_out(jk) = e3t_n(iref,jref,jk)
               ENDDO
               IF (N_in > 0) THEN
                  DO jn=1,jptra
                     call reconstructandremap(tabin(1:N_in,jn),h_in,ptab_child(ji,jj,1:N_out,jn),h_out,N_in,N_out)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
# else
         ptab_child(i1:i2,j1:j2,1:jpk,1:jptra) = ptab(i1:i2,j1:j2,1:jpk,1:jptra)
# endif
         !
         DO jn=1, jptra
            tra(i1:i2,j1:j2,1:jpk,jn)=ptab_child(i1:i2,j1:j2,1:jpk,jn)*tmask(i1:i2,j1:j2,1:jpk) 
         END DO

         IF ( .NOT.lk_agrif_clp ) THEN 
            !
            imin = i1 ; imax = i2
            jmin = j1 ; jmax = j2
            ! 
            ! Remove CORNERS
            IF( l_Southedge ) jmin = 2 + nbghostcells
            IF( l_Northedge ) jmax = nlcj - nbghostcells - 1
            IF( l_Westedge )  imin = 2 + nbghostcells
            IF( l_Eastedge )  imax = nlci - nbghostcells - 1      
            !
            IF( eastern_side ) THEN
               zrho = Agrif_Rhox()
               z1 = ( zrho - 1._wp ) * 0.5_wp                    
               z3 = ( zrho - 1._wp ) / ( zrho + 1._wp )         
               z6 = 2._wp * ( zrho - 1._wp ) / ( zrho + 1._wp )
               z7 =       - ( zrho - 1._wp ) / ( zrho + 3._wp )
               z2 = 1._wp - z1 ; z4 = 1._wp - z3 ; z5 = 1._wp - z6 - z7
               !
               ibdy = nlci-nbghostcells
               DO jn = 1, jptra
                  tra(ibdy+1,jmin:jmax,1:jpkm1,jn) = z1 * ptab_child(ibdy+1,jmin:jmax,1:jpkm1,jn) + z2 * ptab_child(ibdy,jmin:jmax,1:jpkm1,jn)
                  DO jk = 1, jpkm1
                     DO jj = jmin,jmax
                        IF( umask(ibdy-1,jj,jk) == 0._wp ) THEN
                           tra(ibdy,jj,jk,jn) = tra(ibdy+1,jj,jk,jn) * tmask(ibdy,jj,jk)
                        ELSE
                           tra(ibdy,jj,jk,jn)=(z4*tra(ibdy+1,jj,jk,jn)+z3*tra(ibdy-1,jj,jk,jn))*tmask(ibdy,jj,jk)
                           IF( un(ibdy-1,jj,jk) > 0._wp ) THEN
                              tra(ibdy,jj,jk,jn)=( z6*tra(ibdy-1,jj,jk,jn)+z5*tra(ibdy+1,jj,jk,jn) & 
                                                 + z7*tra(ibdy-2,jj,jk,jn) ) * tmask(ibdy,jj,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tra(ibdy+1,jmin:jmax,1:jpkm1,jn) = ptab_child(ibdy+1,jmin:jmax,1:jpkm1,jn) * tmask(ibdy+1,jmin:jmax,1:jpkm1)
               END DO
            ENDIF
            ! 
            IF( northern_side ) THEN
               zrho = Agrif_Rhoy()
               z1 = ( zrho - 1._wp ) * 0.5_wp                    
               z3 = ( zrho - 1._wp ) / ( zrho + 1._wp )         
               z6 = 2._wp * ( zrho - 1._wp ) / ( zrho + 1._wp )
               z7 =       - ( zrho - 1._wp ) / ( zrho + 3._wp )
               z2 = 1._wp - z1 ; z4 = 1._wp - z3 ; z5 = 1._wp - z6 - z7
               !
               jbdy = nlcj-nbghostcells         
               DO jn = 1, jptra
                  tra(imin:imax,jbdy+1,1:jpkm1,jn) = z1 * ptab_child(imin:imax,jbdy+1,1:jpkm1,jn) + z2 * ptab_child(imin:imax,jbdy,1:jpkm1,jn)
                  DO jk = 1, jpkm1
                     DO ji = imin,imax
                        IF( vmask(ji,jbdy-1,jk) == 0._wp ) THEN
                           tra(ji,jbdy,jk,jn) = tra(ji,jbdy+1,jk,jn) * tmask(ji,jbdy,jk)
                        ELSE
                           tra(ji,jbdy,jk,jn)=(z4*tra(ji,jbdy+1,jk,jn)+z3*tra(ji,jbdy-1,jk,jn))*tmask(ji,jbdy,jk)        
                           IF (vn(ji,jbdy-1,jk) > 0._wp ) THEN
                              tra(ji,jbdy,jk,jn)=( z6*tra(ji,jbdy-1,jk,jn)+z5*tra(ji,jbdy+1,jk,jn)  &
                                                 + z7*tra(ji,jbdy-2,jk,jn) ) * tmask(ji,jbdy,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tra(imin:imax,jbdy+1,1:jpkm1,jn) = ptab_child(imin:imax,jbdy+1,1:jpkm1,jn) * tmask(imin:imax,jbdy+1,1:jpkm1)
               END DO
            ENDIF
            !
            IF( western_side ) THEN
               zrho = Agrif_Rhox()
               z1 = ( zrho - 1._wp ) * 0.5_wp                    
               z3 = ( zrho - 1._wp ) / ( zrho + 1._wp )         
               z6 = 2._wp * ( zrho - 1._wp ) / ( zrho + 1._wp )
               z7 =       - ( zrho - 1._wp ) / ( zrho + 3._wp )
               z2 = 1._wp - z1 ; z4 = 1._wp - z3 ; z5 = 1._wp - z6 - z7
               !    
               ibdy = 1+nbghostcells       
               DO jn = 1, jptra
                  tra(ibdy-1,jmin:jmax,1:jpkm1,jn) = z1 * ptab_child(ibdy-1,jmin:jmax,1:jpkm1,jn) + z2 * ptab_child(ibdy,jmin:jmax,1:jpkm1,jn)
                  DO jk = 1, jpkm1
                     DO jj = jmin,jmax
                        IF( umask(ibdy,jj,jk) == 0._wp ) THEN
                           tra(ibdy,jj,jk,jn) = tra(ibdy-1,jj,jk,jn) * tmask(ibdy,jj,jk)
                        ELSE
                           tra(ibdy,jj,jk,jn)=(z4*tra(ibdy-1,jj,jk,jn)+z3*tra(ibdy+1,jj,jk,jn))*tmask(ibdy,jj,jk)        
                           IF( un(ibdy,jj,jk) < 0._wp ) THEN
                              tra(ibdy,jj,jk,jn)=( z6*tra(ibdy+1,jj,jk,jn)+z5*tra(ibdy-1,jj,jk,jn) &
                                                 + z7*tra(ibdy+2,jj,jk,jn) ) * tmask(ibdy,jj,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tra(ibdy-1,jmin:jmax,1:jpkm1,jn) = ptab_child(ibdy-1,jmin:jmax,1:jpkm1,jn) * tmask(ibdy-1,jmin:jmax,1:jpkm1)
               END DO
            ENDIF
            !
            IF( southern_side ) THEN
               zrho = Agrif_Rhoy()
               z1 = ( zrho - 1._wp ) * 0.5_wp                    
               z3 = ( zrho - 1._wp ) / ( zrho + 1._wp )         
               z6 = 2._wp * ( zrho - 1._wp ) / ( zrho + 1._wp )
               z7 =       - ( zrho - 1._wp ) / ( zrho + 3._wp )
               z2 = 1._wp - z1 ; z4 = 1._wp - z3 ; z5 = 1._wp - z6 - z7
               !  
               jbdy=1+nbghostcells        
               DO jn = 1, jptra
                  tra(imin:imax,jbdy-1,1:jpkm1,jn) = z1 * ptab_child(imin:imax,jbdy-1,1:jpkm1,jn) + z2 * ptab_child(imin:imax,jbdy,1:jpkm1,jn)
                  DO jk = 1, jpkm1      
                     DO ji = imin,imax
                        IF( vmask(ji,jbdy,jk) == 0._wp ) THEN
                           tra(ji,jbdy,jk,jn)=tra(ji,jbdy-1,jk,jn) * tmask(ji,jbdy,jk)
                        ELSE
                           tra(ji,jbdy,jk,jn)=(z4*tra(ji,jbdy-1,jk,jn)+z3*tra(ji,jbdy+1,jk,jn))*tmask(ji,jbdy,jk)
                           IF( vn(ji,jbdy,jk) < 0._wp ) THEN
                              tra(ji,jbdy,jk,jn)=( z6*tra(ji,jbdy+1,jk,jn)+z5*tra(ji,jbdy-1,jk,jn) & 
                                                 + z7*tra(ji,jbdy+2,jk,jn) ) * tmask(ji,jbdy,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tra(imin:imax,jbdy-1,1:jpkm1,jn) = ptab_child(imin:imax,jbdy-1,1:jpkm1,jn) * tmask(imin:imax,jbdy-1,1:jpkm1)
               END DO
            ENDIF
            !
         ENDIF

      ENDIF
      !
   END SUBROUTINE interptrn

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                           no TOP AGRIF
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_TOP_Interp_empty
      !!---------------------------------------------
      !!   *** ROUTINE agrif_Top_Interp_empty ***
      !!---------------------------------------------
      WRITE(*,*)  'agrif_top_interp : You should not have seen this print! error?'
   END SUBROUTINE Agrif_TOP_Interp_empty
#endif

   !!======================================================================
END MODULE agrif_top_interp
