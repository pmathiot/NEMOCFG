#define SPONGE && define SPONGE_TOP

MODULE agrif_oce_sponge
   !!======================================================================
   !!                   ***  MODULE  agrif_oce_interp  ***
   !! AGRIF: sponge package for the ocean dynamics (OPA)
   !!======================================================================
   !! History :  2.0  !  2002-06  (XXX)  Original cade
   !!             -   !  2005-11  (XXX) 
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
   !
   USE in_out_manager
   USE agrif_oce
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Sponge, Agrif_Sponge_Tra, Agrif_Sponge_Dyn
   PUBLIC interptsn_sponge, interpun_sponge, interpvn_sponge

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce_sponge.F90 12737 2020-04-10 17:55:11Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Sponge_Tra
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Sponge_Tra ***
      !!----------------------------------------------------------------------
      REAL(wp) ::   zcoef   ! local scalar
      
      !!----------------------------------------------------------------------
      !
#if defined SPONGE
      !! Assume persistence:
      zcoef = REAL(Agrif_rhot()-1,wp)/REAL(Agrif_rhot())

      CALL Agrif_Sponge
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .TRUE.
      tabspongedone_tsn     = .FALSE.
      !
      CALL Agrif_Bc_Variable( tsn_sponge_id, calledweight=zcoef, procname=interptsn_sponge )
      !
      Agrif_UseSpecialValue = .FALSE.
#endif
      !
   END SUBROUTINE Agrif_Sponge_Tra


   SUBROUTINE Agrif_Sponge_dyn
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Sponge_dyn ***
      !!----------------------------------------------------------------------
      REAL(wp) ::   zcoef   ! local scalar
      !!----------------------------------------------------------------------
      !
#if defined SPONGE
      zcoef = REAL(Agrif_rhot()-1,wp)/REAL(Agrif_rhot())

      Agrif_SpecialValue=0.
      Agrif_UseSpecialValue = ln_spc_dyn
      !
      tabspongedone_u = .FALSE.
      tabspongedone_v = .FALSE.         
      CALL Agrif_Bc_Variable( un_sponge_id, calledweight=zcoef, procname=interpun_sponge )
      !
      tabspongedone_u = .FALSE.
      tabspongedone_v = .FALSE.
      CALL Agrif_Bc_Variable( vn_sponge_id, calledweight=zcoef, procname=interpvn_sponge )
      !
      Agrif_UseSpecialValue = .FALSE.
#endif
      !
   END SUBROUTINE Agrif_Sponge_dyn


   SUBROUTINE Agrif_Sponge
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE  Agrif_Sponge ***
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, ind1, ind2
      INTEGER  ::   ispongearea
      REAL(wp) ::   z1_spongearea
      REAL(wp), DIMENSION(jpi,jpj) :: ztabramp
      !!----------------------------------------------------------------------
      !
#if defined SPONGE || defined SPONGE_TOP
      IF (( .NOT. spongedoneT ).OR.( .NOT. spongedoneU )) THEN
         ! Define ramp from boundaries towards domain interior at T-points
         ! Store it in ztabramp

         ispongearea  = 1 + nn_sponge_len * Agrif_irhox()
         z1_spongearea = 1._wp / REAL( ispongearea )
         
         ztabramp(:,:) = 0._wp

         ! --- West --- !
         IF( l_Westedge ) THEN
            ind1 = 1+nbghostcells
            ind2 = 1+nbghostcells + ispongearea 
            DO jj = 1, jpj
               DO ji = ind1, ind2                
                  ztabramp(ji,jj) = REAL( ind2 - ji ) * z1_spongearea * umask(ind1,jj,1)
               END DO
            ENDDO
         ENDIF

         ! --- East --- !
         IF( l_Eastedge ) THEN
            ind1 = nlci - nbghostcells - ispongearea
            ind2 = nlci - nbghostcells
            DO jj = 1, jpj
               DO ji = ind1, ind2
                  ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( ji - ind1 ) * z1_spongearea * umask(ind2-1,jj,1) )
               ENDDO
            ENDDO
         ENDIF

         ! --- South --- !
         IF( l_Southedge ) THEN
            ind1 = 1+nbghostcells
            ind2 = 1+nbghostcells + ispongearea
            DO jj = ind1, ind2 
               DO ji = 1, jpi
                  ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( ind2 - jj ) * z1_spongearea * vmask(ji,ind1,1) )
               END DO
            ENDDO
         ENDIF

         ! --- North --- !
         IF( l_Northedge ) THEN
            ind1 = nlcj - nbghostcells - ispongearea
            ind2 = nlcj - nbghostcells
            DO jj = ind1, ind2
               DO ji = 1, jpi
                  ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( jj - ind1 ) * z1_spongearea * vmask(ji,ind2-1,1) )
               END DO
            ENDDO
         ENDIF

      ENDIF

      ! Tracers
      IF( .NOT. spongedoneT ) THEN
         fsaht_spu(:,:) = 0._wp
         fsaht_spv(:,:) = 0._wp
         DO jj = 2, jpjm1
            DO ji = 2, jpim1   ! vector opt.
               fsaht_spu(ji,jj) = 0.5_wp * visc_tra * ( ztabramp(ji,jj) + ztabramp(ji+1,jj  ) )
               fsaht_spv(ji,jj) = 0.5_wp * visc_tra * ( ztabramp(ji,jj) + ztabramp(ji  ,jj+1) )
            END DO
         END DO
         CALL lbc_lnk( 'agrif_oce_sponge', fsaht_spu, 'U', 1. )   ! Lateral boundary conditions
         CALL lbc_lnk( 'agrif_oce_sponge', fsaht_spv, 'V', 1. )
         
         spongedoneT = .TRUE.
      ENDIF

      ! Dynamics
      IF( .NOT. spongedoneU ) THEN
         fsahm_spt(:,:) = 0._wp
         fsahm_spf(:,:) = 0._wp
         DO jj = 2, jpjm1
            DO ji = 2, jpim1   ! vector opt.
               fsahm_spt(ji,jj) = visc_dyn * ztabramp(ji,jj)
               fsahm_spf(ji,jj) = 0.25_wp * visc_dyn * ( ztabramp(ji  ,jj  ) + ztabramp(ji  ,jj+1) &
                                                     &  +ztabramp(ji+1,jj+1) + ztabramp(ji+1,jj  ) )
            END DO
         END DO
         CALL lbc_lnk( 'agrif_oce_sponge', fsahm_spt, 'T', 1. )   ! Lateral boundary conditions
         CALL lbc_lnk( 'agrif_oce_sponge', fsahm_spf, 'F', 1. )
         
         spongedoneU = .TRUE.
      ENDIF
      !
#endif
      !
   END SUBROUTINE Agrif_Sponge

   SUBROUTINE interptsn_sponge( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE interptsn_sponge ***
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   tabres
      LOGICAL                                     , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   iku, ikv
      REAL(wp) :: ztsa, zabe1, zabe2, zbtr
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk) :: ztu, ztv
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk,n1:n2) ::tsbdiff
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
         DO jn = 1, jpts
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = tsb(ji,jj,jk,jn)
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
         !
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
                  DO jn=1,jpts
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
                  tsbdiff(ji,jj,jk,1:jpts) = tsb(ji,jj,jk,1:jpts) - tabres_child(ji,jj,jk,1:jpts)
# else
                  tsbdiff(ji,jj,jk,1:jpts) = tsb(ji,jj,jk,1:jpts) - tabres(ji,jj,jk,1:jpts)
# endif
               ENDDO
            ENDDO
         ENDDO

         DO jn = 1, jpts            
            DO jk = 1, jpkm1
               ztu(i1:i2,j1:j2,jk) = 0._wp
               DO jj = j1,j2
                  DO ji = i1,i2-1
                     zabe1 = fsaht_spu(ji,jj) * umask(ji,jj,jk) * e2_e1u(ji,jj) * e3u_n(ji,jj,jk)
                     ztu(ji,jj,jk) = zabe1 * ( tsbdiff(ji+1,jj  ,jk,jn) - tsbdiff(ji,jj,jk,jn) ) 
                  END DO
               END DO
               ztv(i1:i2,j1:j2,jk) = 0._wp
               DO ji = i1,i2
                  DO jj = j1,j2-1
                     zabe2 = fsaht_spv(ji,jj) * vmask(ji,jj,jk) * e1_e2v(ji,jj) * e3v_n(ji,jj,jk)
                     ztv(ji,jj,jk) = zabe2 * ( tsbdiff(ji  ,jj+1,jk,jn) - tsbdiff(ji,jj,jk,jn) )
                  END DO
               END DO
               !
               IF( ln_zps ) THEN      ! set gradient at partial step level
                  DO jj = j1,j2
                     DO ji = i1,i2
                        ! last level
                        iku = mbku(ji,jj)
                        ikv = mbkv(ji,jj)
                        IF( iku == jk )   ztu(ji,jj,jk) = 0._wp
                        IF( ikv == jk )   ztv(ji,jj,jk) = 0._wp
                     END DO
                  END DO
               ENDIF
            END DO
            !
            DO jk = 1, jpkm1
               DO jj = j1+1,j2-1
                  DO ji = i1+1,i2-1
                     IF (.NOT. tabspongedone_tsn(ji,jj)) THEN 
                        zbtr = r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk)
                        ! horizontal diffusive trends
                        ztsa = zbtr * (  ztu(ji,jj,jk) - ztu(ji-1,jj,jk) + ztv(ji,jj,jk) - ztv(ji,jj-1,jk)  )
                        ! add it to the general tracer trends
                        tsa(ji,jj,jk,jn) = tsa(ji,jj,jk,jn) + ztsa
                     ENDIF
                  END DO
               END DO
            END DO
            !
         END DO
         !
         tabspongedone_tsn(i1+1:i2-1,j1+1:j2-1) = .TRUE.
         !
      ENDIF
      !
   END SUBROUTINE interptsn_sponge

   SUBROUTINE interpun_sponge(tabres,i1,i2,j1,j2,k1,k2,m1,m2, before)
      !!---------------------------------------------
      !!   *** ROUTINE interpun_sponge ***
      !!---------------------------------------------    
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji,jj,jk,jmax

      ! sponge parameters 
      REAL(wp) :: ze2u, ze1v, zua, zva, zbtr, h_diff
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: ubdiff
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: rotdiff, hdivdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER ::N_in,N_out
      !!---------------------------------------------    
      !
      IF( before ) THEN
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,m1) = ub(ji,jj,jk)
# if defined key_vertical
                  tabres(ji,jj,jk,m2) = e3u_n(ji,jj,jk)*umask(ji,jj,jk)
# endif
               END DO
            END DO
         END DO

      ELSE

# if defined key_vertical
         tabres_child(:,:,:) = 0._wp
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2
                  IF (tabres(ji,jj,jk,m2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk) = tabres(ji,jj,jk,m1)
                  h_in(N_in) = tabres(ji,jj,jk,m2)
              ENDDO
              !
              IF (N_in == 0) THEN
                 tabres_child(ji,jj,:) = 0.
                 CYCLE
              ENDIF
         
              N_out = 0
              DO jk=1,jpk
                 if (umask(ji,jj,jk) == 0) EXIT
                 N_out = N_out + 1
                 h_out(N_out) = e3u_n(ji,jj,jk)
              ENDDO
         
              IF (N_out == 0) THEN
                 tabres_child(ji,jj,:) = 0.
                 CYCLE
              ENDIF
         
              IF (N_in * N_out > 0) THEN
                 h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                 if (h_diff < -1.e4) then
                    print *,'CHECK YOUR BATHY ...', h_diff, sum(h_out(1:N_out)), sum(h_in(1:N_in))
                 endif
              ENDIF
              call reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out)
         
            ENDDO
         ENDDO

         ubdiff(i1:i2,j1:j2,:) = (ub(i1:i2,j1:j2,:) - tabres_child(i1:i2,j1:j2,:))*umask(i1:i2,j1:j2,:)
#else
         ubdiff(i1:i2,j1:j2,:) = (ub(i1:i2,j1:j2,:) - tabres(i1:i2,j1:j2,:,1))*umask(i1:i2,j1:j2,:)
#endif
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !                                             ! ===============

            !                                             ! --------
            ! Horizontal divergence                       !   div
            !                                             ! --------
            DO jj = j1,j2
               DO ji = i1+1,i2   ! vector opt.
                  zbtr = r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk) * fsahm_spt(ji,jj)
                  hdivdiff(ji,jj,jk) = (  e2u(ji  ,jj)*e3u_n(ji  ,jj,jk) * ubdiff(ji  ,jj,jk) &
                                     &   -e2u(ji-1,jj)*e3u_n(ji-1,jj,jk) * ubdiff(ji-1,jj,jk) ) * zbtr
               END DO
            END DO

            DO jj = j1,j2-1
               DO ji = i1,i2   ! vector opt.
                  zbtr = r1_e1e2f(ji,jj) * e3f_n(ji,jj,jk) * fsahm_spf(ji,jj)
                  rotdiff(ji,jj,jk) = ( -e1u(ji,jj+1) * ubdiff(ji,jj+1,jk)   &
                                    &   +e1u(ji,jj  ) * ubdiff(ji,jj  ,jk) ) * fmask(ji,jj,jk) * zbtr 
               END DO
            END DO
         END DO
         !
         DO jj = j1+1, j2-1
            DO ji = i1+1, i2-1   ! vector opt.

               IF (.NOT. tabspongedone_u(ji,jj)) THEN
                  DO jk = 1, jpkm1                                 ! Horizontal slab
                     ze2u = rotdiff (ji,jj,jk)
                     ze1v = hdivdiff(ji,jj,jk)
                     ! horizontal diffusive trends
                     zua = - ( ze2u - rotdiff (ji,jj-1,jk) ) / ( e2u(ji,jj) * e3u_n(ji,jj,jk) )   &
                           + ( hdivdiff(ji+1,jj,jk) - ze1v ) * r1_e1u(ji,jj)

                     ! add it to the general momentum trends
                     ua(ji,jj,jk) = ua(ji,jj,jk) + zua

                  END DO
               ENDIF

            END DO
         END DO

         tabspongedone_u(i1+1:i2-1,j1+1:j2-1) = .TRUE.

         jmax = j2-1
         IF ( l_Northedge ) jmax = MIN(jmax,nlcj-nbghostcells-2)   ! North

         DO jj = j1+1, jmax
            DO ji = i1+1, i2   ! vector opt.

               IF (.NOT. tabspongedone_v(ji,jj)) THEN
                  DO jk = 1, jpkm1                                 ! Horizontal slab
                     ze2u = rotdiff (ji,jj,jk)
                     ze1v = hdivdiff(ji,jj,jk)

                     ! horizontal diffusive trends
                     zva = + ( ze2u - rotdiff (ji-1,jj,jk) ) / ( e1v(ji,jj) * e3v_n(ji,jj,jk) )   &
                           + ( hdivdiff(ji,jj+1,jk) - ze1v ) * r1_e2v(ji,jj)

                     ! add it to the general momentum trends
                     va(ji,jj,jk) = va(ji,jj,jk) + zva
                  END DO
               ENDIF
               !
            END DO
         END DO
         !
         tabspongedone_v(i1+1:i2,j1+1:jmax) = .TRUE.
         !
      ENDIF
      !
   END SUBROUTINE interpun_sponge

   SUBROUTINE interpvn_sponge(tabres,i1,i2,j1,j2,k1,k2,m1,m2, before,nb,ndir)
      !!---------------------------------------------
      !!   *** ROUTINE interpvn_sponge ***
      !!--------------------------------------------- 
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb , ndir
      !
      INTEGER  ::   ji, jj, jk, imax
      REAL(wp) ::   ze2u, ze1v, zua, zva, zbtr, h_diff
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: vbdiff
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: rotdiff, hdivdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER :: N_in, N_out
      !!--------------------------------------------- 
      
      IF( before ) THEN 
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,m1) = vb(ji,jj,jk)
# if defined key_vertical
                  tabres(ji,jj,jk,m2) = vmask(ji,jj,jk) * e3v_n(ji,jj,jk)
# endif
               END DO
            END DO
         END DO
      ELSE

# if defined key_vertical
         tabres_child(:,:,:) = 0._wp
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2
                  IF (tabres(ji,jj,jk,m2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk) = tabres(ji,jj,jk,m1)
                  h_in(N_in) = tabres(ji,jj,jk,m2)
              ENDDO
         
              IF (N_in == 0) THEN
                 tabres_child(ji,jj,:) = 0.
                 CYCLE
              ENDIF
         
              N_out = 0
              DO jk=1,jpk
                 if (vmask(ji,jj,jk) == 0) EXIT
                 N_out = N_out + 1
                 h_out(N_out) = e3v_n(ji,jj,jk)
              ENDDO
         
              IF (N_in * N_out > 0) THEN
                 h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
                 if (h_diff < -1.e4) then
                    print *,'CHECK YOUR BATHY ...', h_diff, sum(h_out(1:N_out)), sum(h_in(1:N_in))
                 endif
              ENDIF
              call reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out)
            ENDDO
         ENDDO

         vbdiff(i1:i2,j1:j2,:) = (vb(i1:i2,j1:j2,:) - tabres_child(i1:i2,j1:j2,:))*vmask(i1:i2,j1:j2,:)  
# else
         vbdiff(i1:i2,j1:j2,:) = (vb(i1:i2,j1:j2,:) - tabres(i1:i2,j1:j2,:,1))*vmask(i1:i2,j1:j2,:)
# endif
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !                                             ! ===============

            !                                             ! --------
            ! Horizontal divergence                       !   div
            !                                             ! --------
            DO jj = j1+1,j2
               DO ji = i1,i2   ! vector opt.
                  zbtr = r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk) * fsahm_spt(ji,jj)
                  hdivdiff(ji,jj,jk) = ( e1v(ji,jj  ) * e3v_n(ji,jj  ,jk) * vbdiff(ji,jj  ,jk)  &
                                     &  -e1v(ji,jj-1) * e3v_n(ji,jj-1,jk) * vbdiff(ji,jj-1,jk)  ) * zbtr
               END DO
            END DO
            DO jj = j1,j2
               DO ji = i1,i2-1   ! vector opt.
                  zbtr = r1_e1e2f(ji,jj) * e3f_n(ji,jj,jk) * fsahm_spf(ji,jj)
                  rotdiff(ji,jj,jk) = ( e2v(ji+1,jj) * vbdiff(ji+1,jj,jk) & 
                                    &  -e2v(ji  ,jj) * vbdiff(ji  ,jj,jk)  ) * fmask(ji,jj,jk) * zbtr
               END DO
            END DO
         END DO

         !                                                ! ===============
         !                                                

         imax = i2 - 1
         IF ( l_Eastedge )   imax = MIN(imax,nlci-nbghostcells-2)   ! East

         DO jj = j1+1, j2
            DO ji = i1+1, imax   ! vector opt.
               IF( .NOT. tabspongedone_u(ji,jj) ) THEN
                  DO jk = 1, jpkm1
                     ua(ji,jj,jk) = ua(ji,jj,jk)                                                               &
                        & - ( rotdiff (ji  ,jj,jk) - rotdiff (ji,jj-1,jk)) / ( e2u(ji,jj) * e3u_n(ji,jj,jk) )  &
                        & + ( hdivdiff(ji+1,jj,jk) - hdivdiff(ji,jj  ,jk)) * r1_e1u(ji,jj)
                  END DO
               ENDIF
            END DO
         END DO
         !
         tabspongedone_u(i1+1:imax,j1+1:j2) = .TRUE.
         !
         DO jj = j1+1, j2-1
            DO ji = i1+1, i2-1   ! vector opt.
               IF( .NOT. tabspongedone_v(ji,jj) ) THEN
                  DO jk = 1, jpkm1
                     va(ji,jj,jk) = va(ji,jj,jk)                                                                  &
                        &  + ( rotdiff (ji,jj  ,jk) - rotdiff (ji-1,jj,jk) ) / ( e1v(ji,jj) * e3v_n(ji,jj,jk) )   &
                        &  + ( hdivdiff(ji,jj+1,jk) - hdivdiff(ji  ,jj,jk) ) * r1_e2v(ji,jj)
                  END DO
               ENDIF
            END DO
         END DO
         tabspongedone_v(i1+1:i2-1,j1+1:j2-1) = .TRUE.
      ENDIF
      !
   END SUBROUTINE interpvn_sponge

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_oce_sponge_empty
      WRITE(*,*)  'agrif_oce_sponge : You should not have seen this print! error?'
   END SUBROUTINE agrif_oce_sponge_empty
#endif

   !!======================================================================
END MODULE agrif_oce_sponge
