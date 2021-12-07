MODULE agrif_oce_interp
   !!======================================================================
   !!                   ***  MODULE  agrif_oce_interp  ***
   !! AGRIF: interpolation package for the ocean dynamics (OPA)
   !!======================================================================
   !! History :  2.0  !  2002-06  (L. Debreu)  Original cade
   !!            3.2  !  2009-04  (R. Benshila) 
   !!            3.6  !  2014-09  (R. Benshila) 
   !!----------------------------------------------------------------------
#if defined key_agrif
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   !!   Agrif_tra     :
   !!   Agrif_dyn     : 
   !!   Agrif_ssh     :
   !!   Agrif_dyn_ts  :
   !!   Agrif_dta_ts  :
   !!   Agrif_ssh_ts  :
   !!   Agrif_avm     : 
   !!   interpu       :
   !!   interpv       :
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce      
   USE zdf_oce
   USE agrif_oce
   USE phycst
   USE dynspg_ts, ONLY: un_adv, vn_adv
   !
   USE in_out_manager
   USE agrif_oce_sponge
   USE lib_mpp
 
   IMPLICIT NONE
   PRIVATE

   PUBLIC   Agrif_dyn, Agrif_ssh, Agrif_dyn_ts, Agrif_ssh_ts, Agrif_dta_ts
   PUBLIC   Agrif_tra, Agrif_avm
   PUBLIC   interpun , interpvn
   PUBLIC   interptsn, interpsshn, interpavm
   PUBLIC   interpunb, interpvnb , interpub2b, interpvb2b
   PUBLIC   interpe3t, interpumsk, interpvmsk

   INTEGER ::   bdy_tinterp = 0

#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce_interp.F90 12857 2020-05-02 16:06:55Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_tra
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_tra  ***
      !!----------------------------------------------------------------------
      !
      IF( Agrif_Root() )   RETURN
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .TRUE.
      !
      CALL Agrif_Bc_variable( tsn_id, procname=interptsn )
      !
      Agrif_UseSpecialValue = .FALSE.
      !
   END SUBROUTINE Agrif_tra


   SUBROUTINE Agrif_dyn( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_DYN  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !
      INTEGER ::   ji, jj, jk       ! dummy loop indices
      INTEGER ::   j1, j2, i1, i2
      INTEGER ::   ibdy1, jbdy1, ibdy2, jbdy2
      REAL(wp), DIMENSION(jpi,jpj) ::   zub, zvb
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = ln_spc_dyn
      !
      CALL Agrif_Bc_variable( un_interp_id, procname=interpun )
      CALL Agrif_Bc_variable( vn_interp_id, procname=interpvn )
      !
      Agrif_UseSpecialValue = .FALSE.
      !
      ! prevent smoothing in ghost cells
      i1 =  1   ;   i2 = nlci
      j1 =  1   ;   j2 = nlcj
      IF( l_Southedge )   j1 = 2 + nbghostcells
      IF( l_Northedge )   j2 = nlcj - nbghostcells - 1
      IF( l_Westedge )    i1 = 2 + nbghostcells 
      IF( l_Eastedge )    i2 = nlci - nbghostcells - 1

      ! --- West --- !
      IF( l_Westedge ) THEN
         ibdy1 = 2
         ibdy2 = 1+nbghostcells 
         !
         IF( .NOT.ln_dynspg_ts ) THEN  ! Store transport
            ua_b(ibdy1:ibdy2,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  ua_b(ibdy1:ibdy2,jj) = ua_b(ibdy1:ibdy2,jj) & 
                      & + e3u_a(ibdy1:ibdy2,jj,jk) * ua(ibdy1:ibdy2,jj,jk) * umask(ibdy1:ibdy2,jj,jk)
               END DO
            END DO
            DO jj = 1, jpj
               ua_b(ibdy1:ibdy2,jj) = ua_b(ibdy1:ibdy2,jj) * r1_hu_a(ibdy1:ibdy2,jj)
            END DO
         ENDIF
         !
         IF( .NOT.lk_agrif_clp ) THEN
            DO jk=1,jpkm1              ! Smooth
               DO jj=j1,j2
                  ua(ibdy2,jj,jk) = 0.25_wp*(ua(ibdy2-1,jj,jk)+2._wp*ua(ibdy2,jj,jk)+ua(ibdy2+1,jj,jk))
               END DO
            END DO
         ENDIF
         !
         zub(ibdy1:ibdy2,:) = 0._wp    ! Correct transport
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               zub(ibdy1:ibdy2,jj) = zub(ibdy1:ibdy2,jj) & 
                  & + e3u_a(ibdy1:ibdy2,jj,jk)  * ua(ibdy1:ibdy2,jj,jk)*umask(ibdy1:ibdy2,jj,jk)
            END DO
         END DO
         DO jj=1,jpj
            zub(ibdy1:ibdy2,jj) = zub(ibdy1:ibdy2,jj) * r1_hu_a(ibdy1:ibdy2,jj)
         END DO
            
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               ua(ibdy1:ibdy2,jj,jk) = ( ua(ibdy1:ibdy2,jj,jk) &
                 & + ua_b(ibdy1:ibdy2,jj)-zub(ibdy1:ibdy2,jj)) * umask(ibdy1:ibdy2,jj,jk)
            END DO
         END DO
            
         IF( ln_dynspg_ts ) THEN       ! Set tangential velocities to time splitting estimate
            zvb(ibdy1:ibdy2,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zvb(ibdy1:ibdy2,jj) = zvb(ibdy1:ibdy2,jj) & 
                     & + e3v_a(ibdy1:ibdy2,jj,jk) * va(ibdy1:ibdy2,jj,jk) * vmask(ibdy1:ibdy2,jj,jk)
               END DO
            END DO
            DO jj = 1, jpj
               zvb(ibdy1:ibdy2,jj) = zvb(ibdy1:ibdy2,jj) * r1_hv_a(ibdy1:ibdy2,jj)
            END DO
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  va(ibdy1:ibdy2,jj,jk) = ( va(ibdy1:ibdy2,jj,jk) & 
                    & + va_b(ibdy1:ibdy2,jj)-zvb(ibdy1:ibdy2,jj))*vmask(ibdy1:ibdy2,jj,jk)
               END DO
            END DO
         ENDIF
         !
         DO jk = 1, jpkm1              ! Mask domain edges
            DO jj = 1, jpj
               ua(1,jj,jk) = 0._wp
               va(1,jj,jk) = 0._wp
            END DO
         END DO 
      ENDIF

      ! --- East --- !
      IF( l_Eastedge ) THEN
         ibdy1 = nlci-1-nbghostcells
         ibdy2 = nlci-2 
         !
         IF( .NOT.ln_dynspg_ts ) THEN  ! Store transport
            ua_b(ibdy1:ibdy2,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  ua_b(ibdy1:ibdy2,jj) = ua_b(ibdy1:ibdy2,jj) & 
                      & + e3u_a(ibdy1:ibdy2,jj,jk) * ua(ibdy1:ibdy2,jj,jk) * umask(ibdy1:ibdy2,jj,jk)
               END DO
            END DO
            DO jj = 1, jpj
               ua_b(ibdy1:ibdy2,jj) = ua_b(ibdy1:ibdy2,jj) * r1_hu_a(ibdy1:ibdy2,jj)
            END DO
         ENDIF
         !
         IF( .NOT.lk_agrif_clp ) THEN
            DO jk=1,jpkm1              ! Smooth
               DO jj=j1,j2
                  ua(ibdy1,jj,jk) = 0.25_wp*(ua(ibdy1-1,jj,jk)+2._wp*ua(ibdy1,jj,jk)+ua(ibdy1+1,jj,jk))
               END DO
            END DO
         ENDIF
         !
         zub(ibdy1:ibdy2,:) = 0._wp    ! Correct transport
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               zub(ibdy1:ibdy2,jj) = zub(ibdy1:ibdy2,jj) & 
                  & + e3u_a(ibdy1:ibdy2,jj,jk)  * ua(ibdy1:ibdy2,jj,jk) * umask(ibdy1:ibdy2,jj,jk)
            END DO
         END DO
         DO jj=1,jpj
            zub(ibdy1:ibdy2,jj) = zub(ibdy1:ibdy2,jj) * r1_hu_a(ibdy1:ibdy2,jj)
         END DO
            
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               ua(ibdy1:ibdy2,jj,jk) = ( ua(ibdy1:ibdy2,jj,jk) & 
                 & + ua_b(ibdy1:ibdy2,jj)-zub(ibdy1:ibdy2,jj))*umask(ibdy1:ibdy2,jj,jk)
            END DO
         END DO
            
         IF( ln_dynspg_ts ) THEN       ! Set tangential velocities to time splitting estimate
            ibdy1 = ibdy1 + 1
            ibdy2 = ibdy2 + 1 
            zvb(ibdy1:ibdy2,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zvb(ibdy1:ibdy2,jj) = zvb(ibdy1:ibdy2,jj) &
                     & + e3v_a(ibdy1:ibdy2,jj,jk) * va(ibdy1:ibdy2,jj,jk) * vmask(ibdy1:ibdy2,jj,jk)
               END DO
            END DO
            DO jj = 1, jpj
               zvb(ibdy1:ibdy2,jj) = zvb(ibdy1:ibdy2,jj) * r1_hv_a(ibdy1:ibdy2,jj)
            END DO
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  va(ibdy1:ibdy2,jj,jk) = ( va(ibdy1:ibdy2,jj,jk) & 
                      & + va_b(ibdy1:ibdy2,jj)-zvb(ibdy1:ibdy2,jj)) * vmask(ibdy1:ibdy2,jj,jk)
               END DO
            END DO
         ENDIF
         !
         DO jk = 1, jpkm1              ! Mask domain edges
            DO jj = 1, jpj
               ua(nlci-1,jj,jk) = 0._wp
               va(nlci  ,jj,jk) = 0._wp
            END DO
         END DO 
      ENDIF

      ! --- South --- !
      IF ( l_Southedge ) THEN
         jbdy1 = 2
         jbdy2 = 1+nbghostcells 
         !
         IF( .NOT.ln_dynspg_ts ) THEN  ! Store transport
            va_b(:,jbdy1:jbdy2) = 0._wp
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  va_b(ji,jbdy1:jbdy2) = va_b(ji,jbdy1:jbdy2) & 
                      & + e3v_a(ji,jbdy1:jbdy2,jk) * va(ji,jbdy1:jbdy2,jk) * vmask(ji,jbdy1:jbdy2,jk)
               END DO
            END DO
            DO ji=1,jpi
               va_b(ji,jbdy1:jbdy2) = va_b(ji,jbdy1:jbdy2) * r1_hv_a(ji,jbdy1:jbdy2)
            END DO
         ENDIF
         !
         IF ( .NOT.lk_agrif_clp ) THEN
            DO jk = 1, jpkm1           ! Smooth
               DO ji = i1, i2
                  va(ji,jbdy2,jk) = 0.25_wp*(va(ji,jbdy2-1,jk)+2._wp*va(ji,jbdy2,jk)+va(ji,jbdy2+1,jk))
               END DO
            END DO
         ENDIF
         !
         zvb(:,jbdy1:jbdy2) = 0._wp    ! Correct transport
         DO jk=1,jpkm1
            DO ji=1,jpi
               zvb(ji,jbdy1:jbdy2) = zvb(ji,jbdy1:jbdy2) & 
                  & + e3v_a(ji,jbdy1:jbdy2,jk) * va(ji,jbdy1:jbdy2,jk) * vmask(ji,jbdy1:jbdy2,jk)
            END DO
         END DO
         DO ji = 1, jpi
            zvb(ji,jbdy1:jbdy2) = zvb(ji,jbdy1:jbdy2) * r1_hv_a(ji,jbdy1:jbdy2)
         END DO

         DO jk = 1, jpkm1
            DO ji = 1, jpi
               va(ji,jbdy1:jbdy2,jk) = ( va(ji,jbdy1:jbdy2,jk) & 
                 & + va_b(ji,jbdy1:jbdy2) - zvb(ji,jbdy1:jbdy2) ) * vmask(ji,jbdy1:jbdy2,jk)
            END DO
         END DO
            
         IF( ln_dynspg_ts ) THEN       ! Set tangential velocities to time splitting estimate
            zub(:,jbdy1:jbdy2) = 0._wp
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zub(ji,jbdy1:jbdy2) = zub(ji,jbdy1:jbdy2) & 
                     & + e3u_a(ji,jbdy1:jbdy2,jk) * ua(ji,jbdy1:jbdy2,jk) * umask(ji,jbdy1:jbdy2,jk)
               END DO
            END DO
            DO ji = 1, jpi
               zub(ji,jbdy1:jbdy2) = zub(ji,jbdy1:jbdy2) * r1_hu_a(ji,jbdy1:jbdy2)
            END DO
               
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  ua(ji,jbdy1:jbdy2,jk) = ( ua(ji,jbdy1:jbdy2,jk) & 
                    & + ua_b(ji,jbdy1:jbdy2) - zub(ji,jbdy1:jbdy2) ) * umask(ji,jbdy1:jbdy2,jk)
               END DO
            END DO
         ENDIF
         !
         DO jk = 1, jpkm1              ! Mask domain edges
            DO ji = 1, jpi
               ua(ji,1,jk) = 0._wp
               va(ji,1,jk) = 0._wp
            END DO
         END DO 
      ENDIF

      ! --- North --- !
      IF( l_Northedge ) THEN
         jbdy1 = nlcj-1-nbghostcells
         jbdy2 = nlcj-2 
         !
         IF( .NOT.ln_dynspg_ts ) THEN  ! Store transport
            va_b(:,jbdy1:jbdy2) = 0._wp
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  va_b(ji,jbdy1:jbdy2) = va_b(ji,jbdy1:jbdy2) & 
                      & + e3v_a(ji,jbdy1:jbdy2,jk) * va(ji,jbdy1:jbdy2,jk) * vmask(ji,jbdy1:jbdy2,jk)
               END DO
            END DO
            DO ji=1,jpi
               va_b(ji,jbdy1:jbdy2) = va_b(ji,jbdy1:jbdy2) * r1_hv_a(ji,jbdy1:jbdy2)
            END DO
         ENDIF
         !
         IF ( .NOT.lk_agrif_clp ) THEN
            DO jk = 1, jpkm1           ! Smooth
               DO ji = i1, i2
                  va(ji,jbdy1,jk) = 0.25_wp*(va(ji,jbdy1-1,jk)+2._wp*va(ji,jbdy1,jk)+va(ji,jbdy1+1,jk))
               END DO
            END DO
         ENDIF
         !
         zvb(:,jbdy1:jbdy2) = 0._wp    ! Correct transport
         DO jk=1,jpkm1
            DO ji=1,jpi
               zvb(ji,jbdy1:jbdy2) = zvb(ji,jbdy1:jbdy2) & 
                  & + e3v_a(ji,jbdy1:jbdy2,jk) * va(ji,jbdy1:jbdy2,jk) * vmask(ji,jbdy1:jbdy2,jk)
            END DO
         END DO
         DO ji = 1, jpi
            zvb(ji,jbdy1:jbdy2) = zvb(ji,jbdy1:jbdy2) * r1_hv_a(ji,jbdy1:jbdy2)
         END DO

         DO jk = 1, jpkm1
            DO ji = 1, jpi
               va(ji,jbdy1:jbdy2,jk) = ( va(ji,jbdy1:jbdy2,jk) & 
                 & + va_b(ji,jbdy1:jbdy2) - zvb(ji,jbdy1:jbdy2) ) * vmask(ji,jbdy1:jbdy2,jk)
            END DO
         END DO
            
         IF( ln_dynspg_ts ) THEN       ! Set tangential velocities to time splitting estimate
            jbdy1 = jbdy1 + 1
            jbdy2 = jbdy2 + 1 
            zub(:,jbdy1:jbdy2) = 0._wp
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zub(ji,jbdy1:jbdy2) = zub(ji,jbdy1:jbdy2) & 
                     & + e3u_a(ji,jbdy1:jbdy2,jk) * ua(ji,jbdy1:jbdy2,jk) * umask(ji,jbdy1:jbdy2,jk)
               END DO
            END DO
            DO ji = 1, jpi
               zub(ji,jbdy1:jbdy2) = zub(ji,jbdy1:jbdy2) * r1_hu_a(ji,jbdy1:jbdy2)
            END DO
               
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  ua(ji,jbdy1:jbdy2,jk) = ( ua(ji,jbdy1:jbdy2,jk) & 
                    & + ua_b(ji,jbdy1:jbdy2) - zub(ji,jbdy1:jbdy2) ) * umask(ji,jbdy1:jbdy2,jk)
               END DO
            END DO
         ENDIF
         !
         DO jk = 1, jpkm1              ! Mask domain edges
            DO ji = 1, jpi
               ua(ji,nlcj  ,jk) = 0._wp
               va(ji,nlcj-1,jk) = 0._wp
            END DO
         END DO 
      ENDIF
      !
   END SUBROUTINE Agrif_dyn


   SUBROUTINE Agrif_dyn_ts( jn )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_dyn_ts  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   jn
      !!
      INTEGER :: ji, jj
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      IF( l_Westedge ) THEN
         DO jj=1,jpj
            va_e(2:nbghostcells+1,jj) = vbdy_w(1:nbghostcells,jj) * hvr_e(2:nbghostcells+1,jj)
            ! Specified fluxes:
            ua_e(2:nbghostcells+1,jj) = ubdy_w(1:nbghostcells,jj) * hur_e(2:nbghostcells+1,jj)
            ! Characteristics method (only if ghostcells=1):
            !alt            ua_e(2,jj) = 0.5_wp * ( ubdy_w(jj) * hur_e(2,jj) + ua_e(3,jj) &
            !alt                       &           - sqrt(grav * hur_e(2,jj)) * (sshn_e(3,jj) - hbdy_w(jj)) )
         END DO
      ENDIF
      !
      IF( l_Eastedge ) THEN
         DO jj=1,jpj
            va_e(nlci-nbghostcells:nlci-1,jj)   = vbdy_e(1:nbghostcells,jj) * hvr_e(nlci-nbghostcells:nlci-1,jj)
            ! Specified fluxes:
            ua_e(nlci-nbghostcells-1:nlci-2,jj) = ubdy_e(1:nbghostcells,jj) * hur_e(nlci-nbghostcells-1:nlci-2,jj)
            ! Characteristics method (only if ghostcells=1):
            !alt            ua_e(nlci-2,jj) = 0.5_wp * ( ubdy_e(jj) * hur_e(nlci-2,jj) + ua_e(nlci-3,jj) &
            !alt                            &           + sqrt(grav * hur_e(nlci-2,jj)) * (sshn_e(nlci-2,jj) - hbdy_e(jj)) )
         END DO
      ENDIF
      !
      IF ( l_Southedge ) THEN
         DO ji=1,jpi
            ua_e(ji,2:nbghostcells+1) = ubdy_s(ji,1:nbghostcells) * hur_e(ji,2:nbghostcells+1)
            ! Specified fluxes:
            va_e(ji,2:nbghostcells+1) = vbdy_s(ji,1:nbghostcells) * hvr_e(ji,2:nbghostcells+1)
            ! Characteristics method (only if ghostcells=1):
            !alt            va_e(ji,2) = 0.5_wp * ( vbdy_s(ji) * hvr_e(ji,2) + va_e(ji,3) &
            !alt                       &           - sqrt(grav * hvr_e(ji,2)) * (sshn_e(ji,3) - hbdy_s(ji)) )
         END DO
      ENDIF
      !
      IF ( l_Northedge ) THEN
         DO ji=1,jpi
            ua_e(ji,nlcj-nbghostcells:nlcj-1)   = ubdy_n(ji,1:nbghostcells) * hur_e(ji,nlcj-nbghostcells:nlcj-1)
            ! Specified fluxes:
            va_e(ji,nlcj-nbghostcells-1:nlcj-2) = vbdy_n(ji,1:nbghostcells) * hvr_e(ji,nlcj-nbghostcells-1:nlcj-2)
            ! Characteristics method (only if ghostcells=1):
            !alt            va_e(ji,nlcj-2) = 0.5_wp * ( vbdy_n(ji) * hvr_e(ji,nlcj-2)  + va_e(ji,nlcj-3) &
            !alt                            &           + sqrt(grav * hvr_e(ji,nlcj-2)) * (sshn_e(ji,nlcj-2) - hbdy_n(ji)) )
         END DO
      ENDIF
      !
   END SUBROUTINE Agrif_dyn_ts


   SUBROUTINE Agrif_dta_ts( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_dta_ts  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !!
      INTEGER :: ji, jj
      LOGICAL :: ll_int_cons
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      ll_int_cons = ln_bt_fw ! Assume conservative temporal integration in the forward case only
      !
      ! Enforce volume conservation if no time refinement:  
      IF ( Agrif_rhot()==1 ) ll_int_cons=.TRUE.  
      !
      ! Interpolate barotropic fluxes
      Agrif_SpecialValue=0._wp
      Agrif_UseSpecialValue = ln_spc_dyn
      !
      IF( ll_int_cons ) THEN  ! Conservative interpolation
         ! order matters here !!!!!!
         CALL Agrif_Bc_variable( ub2b_interp_id, calledweight=1._wp, procname=interpub2b ) ! Time integrated
         CALL Agrif_Bc_variable( vb2b_interp_id, calledweight=1._wp, procname=interpvb2b )
         bdy_tinterp = 1
         CALL Agrif_Bc_variable( unb_id        , calledweight=1._wp, procname=interpunb  ) ! After
         CALL Agrif_Bc_variable( vnb_id        , calledweight=1._wp, procname=interpvnb  )
         bdy_tinterp = 2
         CALL Agrif_Bc_variable( unb_id        , calledweight=0._wp, procname=interpunb  ) ! Before
         CALL Agrif_Bc_variable( vnb_id        , calledweight=0._wp, procname=interpvnb  )         
      ELSE ! Linear interpolation
         bdy_tinterp = 0
         ubdy_w(:,:) = 0._wp   ;   vbdy_w(:,:) = 0._wp 
         ubdy_e(:,:) = 0._wp   ;   vbdy_e(:,:) = 0._wp 
         ubdy_n(:,:) = 0._wp   ;   vbdy_n(:,:) = 0._wp 
         ubdy_s(:,:) = 0._wp   ;   vbdy_s(:,:) = 0._wp
         CALL Agrif_Bc_variable( unb_id, procname=interpunb )
         CALL Agrif_Bc_variable( vnb_id, procname=interpvnb )
      ENDIF
      Agrif_UseSpecialValue = .FALSE.
      ! 
   END SUBROUTINE Agrif_dta_ts


   SUBROUTINE Agrif_ssh( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_ssh  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !
      INTEGER  :: ji, jj, indx, indy
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !      
      ! Linear time interpolation of sea level
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .TRUE.
      CALL Agrif_Bc_variable(sshn_id, procname=interpsshn )
      Agrif_UseSpecialValue = .FALSE.
      !
      ! --- West --- !
      IF( l_Westedge ) THEN
         indx = 1+nbghostcells
         DO jj = 1, jpj
            DO ji = 2, indx
               ssha(ji,jj) = hbdy_w(ji-1,jj)
            ENDDO
         ENDDO
      ENDIF
      !
      ! --- East --- !
      IF( l_Eastedge ) THEN
         indx = nlci-nbghostcells
         DO jj = 1, jpj
            DO ji = indx, nlci-1
               ssha(ji,jj) = hbdy_e(ji-indx+1,jj)
            ENDDO
         ENDDO
      ENDIF
      !
      ! --- South --- !
      IF ( l_Southedge ) THEN
         indy = 1+nbghostcells
         DO jj = 2, indy
            DO ji = 1, jpi
               ssha(ji,jj) = hbdy_s(ji,jj-1)
            ENDDO
         ENDDO
      ENDIF
      !
      ! --- North --- !
      IF ( l_Northedge ) THEN
         indy = nlcj-nbghostcells
         DO jj = indy, nlcj-1
            DO ji = 1, jpi
               ssha(ji,jj) = hbdy_n(ji,jj-indy+1)
            ENDDO
         ENDDO
      ENDIF
      !
   END SUBROUTINE Agrif_ssh


   SUBROUTINE Agrif_ssh_ts( jn )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_ssh_ts  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   jn
      !!
      INTEGER :: ji, jj, indx, indy
      !!----------------------------------------------------------------------  
      !! clem ghost (starting at i,j=1 is important I think otherwise you introduce a grad(ssh)/=0 at point 2)
      !
      IF( Agrif_Root() )   RETURN
      !
      ! --- West --- !
      IF( l_Westedge ) THEN
         indx = 1+nbghostcells
         DO jj = 1, jpj
            DO ji = 2, indx
               ssha_e(ji,jj) = hbdy_w(ji-1,jj)
            ENDDO
         ENDDO
      ENDIF
      !
      ! --- East --- !
      IF( l_Eastedge ) THEN
         indx = nlci-nbghostcells
         DO jj = 1, jpj
            DO ji = indx, nlci-1
               ssha_e(ji,jj) = hbdy_e(ji-indx+1,jj)
            ENDDO
         ENDDO
      ENDIF
      !
      ! --- South --- !
      IF( l_Southedge ) THEN
         indy = 1+nbghostcells
         DO jj = 2, indy
            DO ji = 1, jpi
               ssha_e(ji,jj) = hbdy_s(ji,jj-1)
            ENDDO
         ENDDO
      ENDIF
      !
      ! --- North --- !
      IF( l_Northedge ) THEN
         indy = nlcj-nbghostcells
         DO jj = indy, nlcj-1
            DO ji = 1, jpi
               ssha_e(ji,jj) = hbdy_n(ji,jj-indy+1)
            ENDDO
         ENDDO
      ENDIF
      !
   END SUBROUTINE Agrif_ssh_ts

   SUBROUTINE Agrif_avm
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_avm  ***
      !!----------------------------------------------------------------------  
      REAL(wp) ::   zalpha
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      zalpha = 1._wp ! JC: proper time interpolation impossible  
                     ! => use last available value from parent 
      !
      Agrif_SpecialValue    = 0.e0
      Agrif_UseSpecialValue = .TRUE.
      !
      CALL Agrif_Bc_variable( avm_id, calledweight=zalpha, procname=interpavm )       
      !
      Agrif_UseSpecialValue = .FALSE.
      !
   END SUBROUTINE Agrif_avm
   

   SUBROUTINE interptsn( ptab, i1, i2, j1, j2, k1, k2, n1, n2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interptsn ***
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
         DO jn = 1,jpts
            DO jk=k1,k2
               DO jj=j1,j2
                 DO ji=i1,i2
                       ptab(ji,jj,jk,jn) = tsn(ji,jj,jk,jn)
                 END DO
              END DO
           END DO
        END DO

# if defined key_vertical
        DO jk=k1,k2
           DO jj=j1,j2
              DO ji=i1,i2
                 ptab(ji,jj,jk,jpts+1) = tmask(ji,jj,jk) * e3t_n(ji,jj,jk) 
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
                  DO jn=1,jpts
                     call reconstructandremap(tabin(1:N_in,jn),h_in,ptab_child(ji,jj,1:N_out,jn),h_out,N_in,N_out)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
# else
         ptab_child(i1:i2,j1:j2,1:jpk,1:jpts) = ptab(i1:i2,j1:j2,1:jpk,1:jpts)
# endif
         !
         DO jn=1, jpts
            tsa(i1:i2,j1:j2,1:jpk,jn)=ptab_child(i1:i2,j1:j2,1:jpk,jn)*tmask(i1:i2,j1:j2,1:jpk) 
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
               DO jn = 1, jpts
                  tsa(ibdy+1,jmin:jmax,1:jpkm1,jn) = z1 * ptab_child(ibdy+1,jmin:jmax,1:jpkm1,jn) + z2 * ptab_child(ibdy,jmin:jmax,1:jpkm1,jn)
                  DO jk = 1, jpkm1
                     DO jj = jmin,jmax
                        IF( umask(ibdy-1,jj,jk) == 0._wp ) THEN
                           tsa(ibdy,jj,jk,jn) = tsa(ibdy+1,jj,jk,jn) * tmask(ibdy,jj,jk)
                        ELSE
                           tsa(ibdy,jj,jk,jn)=(z4*tsa(ibdy+1,jj,jk,jn)+z3*tsa(ibdy-1,jj,jk,jn))*tmask(ibdy,jj,jk)
                           IF( un(ibdy-1,jj,jk) > 0._wp ) THEN
                              tsa(ibdy,jj,jk,jn)=( z6*tsa(ibdy-1,jj,jk,jn)+z5*tsa(ibdy+1,jj,jk,jn) & 
                                                 + z7*tsa(ibdy-2,jj,jk,jn) ) * tmask(ibdy,jj,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tsa(ibdy+1,jmin:jmax,1:jpkm1,jn) = ptab_child(ibdy+1,jmin:jmax,1:jpkm1,jn) * tmask(ibdy+1,jmin:jmax,1:jpkm1)
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
               DO jn = 1, jpts
                  tsa(imin:imax,jbdy+1,1:jpkm1,jn) = z1 * ptab_child(imin:imax,jbdy+1,1:jpkm1,jn) + z2 * ptab_child(imin:imax,jbdy,1:jpkm1,jn)
                  DO jk = 1, jpkm1
                     DO ji = imin,imax
                        IF( vmask(ji,jbdy-1,jk) == 0._wp ) THEN
                           tsa(ji,jbdy,jk,jn) = tsa(ji,jbdy+1,jk,jn) * tmask(ji,jbdy,jk)
                        ELSE
                           tsa(ji,jbdy,jk,jn)=(z4*tsa(ji,jbdy+1,jk,jn)+z3*tsa(ji,jbdy-1,jk,jn))*tmask(ji,jbdy,jk)        
                           IF (vn(ji,jbdy-1,jk) > 0._wp ) THEN
                              tsa(ji,jbdy,jk,jn)=( z6*tsa(ji,jbdy-1,jk,jn)+z5*tsa(ji,jbdy+1,jk,jn)  &
                                                 + z7*tsa(ji,jbdy-2,jk,jn) ) * tmask(ji,jbdy,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tsa(imin:imax,jbdy+1,1:jpkm1,jn) = ptab_child(imin:imax,jbdy+1,1:jpkm1,jn) * tmask(imin:imax,jbdy+1,1:jpkm1)
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
               DO jn = 1, jpts
                  tsa(ibdy-1,jmin:jmax,1:jpkm1,jn) = z1 * ptab_child(ibdy-1,jmin:jmax,1:jpkm1,jn) + z2 * ptab_child(ibdy,jmin:jmax,1:jpkm1,jn)
                  DO jk = 1, jpkm1
                     DO jj = jmin,jmax
                        IF( umask(ibdy,jj,jk) == 0._wp ) THEN
                           tsa(ibdy,jj,jk,jn) = tsa(ibdy-1,jj,jk,jn) * tmask(ibdy,jj,jk)
                        ELSE
                           tsa(ibdy,jj,jk,jn)=(z4*tsa(ibdy-1,jj,jk,jn)+z3*tsa(ibdy+1,jj,jk,jn))*tmask(ibdy,jj,jk)        
                           IF( un(ibdy,jj,jk) < 0._wp ) THEN
                              tsa(ibdy,jj,jk,jn)=( z6*tsa(ibdy+1,jj,jk,jn)+z5*tsa(ibdy-1,jj,jk,jn) &
                                                 + z7*tsa(ibdy+2,jj,jk,jn) ) * tmask(ibdy,jj,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tsa(ibdy-1,jmin:jmax,1:jpkm1,jn) = ptab_child(ibdy-1,jmin:jmax,1:jpkm1,jn) * tmask(ibdy-1,jmin:jmax,1:jpkm1)
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
               DO jn = 1, jpts
                  tsa(imin:imax,jbdy-1,1:jpkm1,jn) = z1 * ptab_child(imin:imax,jbdy-1,1:jpkm1,jn) + z2 * ptab_child(imin:imax,jbdy,1:jpkm1,jn)
                  DO jk = 1, jpkm1      
                     DO ji = imin,imax
                        IF( vmask(ji,jbdy,jk) == 0._wp ) THEN
                           tsa(ji,jbdy,jk,jn)=tsa(ji,jbdy-1,jk,jn) * tmask(ji,jbdy,jk)
                        ELSE
                           tsa(ji,jbdy,jk,jn)=(z4*tsa(ji,jbdy-1,jk,jn)+z3*tsa(ji,jbdy+1,jk,jn))*tmask(ji,jbdy,jk)
                           IF( vn(ji,jbdy,jk) < 0._wp ) THEN
                              tsa(ji,jbdy,jk,jn)=( z6*tsa(ji,jbdy+1,jk,jn)+z5*tsa(ji,jbdy-1,jk,jn) & 
                                                 + z7*tsa(ji,jbdy+2,jk,jn) ) * tmask(ji,jbdy,jk)
                           ENDIF
                        ENDIF
                     END DO
                  END DO
                  ! Restore ghost points:
                  tsa(imin:imax,jbdy-1,1:jpkm1,jn) = ptab_child(imin:imax,jbdy-1,1:jpkm1,jn) * tmask(imin:imax,jbdy-1,1:jpkm1)
               END DO
            ENDIF
            !
         ENDIF
      ENDIF
      !
   END SUBROUTINE interptsn

   SUBROUTINE interpsshn( ptab, i1, i2, j1, j2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      LOGICAL :: western_side, eastern_side,northern_side,southern_side
      !!----------------------------------------------------------------------  
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = sshn(i1:i2,j1:j2)
      ELSE
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         !! clem ghost
         IF(western_side)  hbdy_w(1:nbghostcells,j1:j2) = ptab(i1:i2,j1:j2) * tmask(i1:i2,j1:j2,1)
         IF(eastern_side)  hbdy_e(1:nbghostcells,j1:j2) = ptab(i1:i2,j1:j2) * tmask(i1:i2,j1:j2,1)
         IF(southern_side) hbdy_s(i1:i2,1:nbghostcells) = ptab(i1:i2,j1:j2) * tmask(i1:i2,j1:j2,1) 
         IF(northern_side) hbdy_n(i1:i2,1:nbghostcells) = ptab(i1:i2,j1:j2) * tmask(i1:i2,j1:j2,1)
      ENDIF
      !
   END SUBROUTINE interpsshn

   SUBROUTINE interpun( ptab, i1, i2, j1, j2, k1, k2, m1, m2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interpun ***
      !!---------------------------------------------    
      !!
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: ptab
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb , ndir
      !!
      INTEGER :: ji,jj,jk
      REAL(wp) :: zrhoy
      ! vertical interpolation:
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER  :: N_in, N_out, iref
      REAL(wp) :: h_diff
      LOGICAL  :: western_side, eastern_side
      !!---------------------------------------------    
      !
      IF (before) THEN 
         DO jk=1,jpk
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,jk,1) = (e2u(ji,jj) * e3u_n(ji,jj,jk) * un(ji,jj,jk)*umask(ji,jj,jk)) 
# if defined key_vertical
                  ptab(ji,jj,jk,2) = (umask(ji,jj,jk) * e2u(ji,jj) * e3u_n(ji,jj,jk))
# endif
               END DO
            END DO
         END DO
      ELSE
         zrhoy = Agrif_rhoy()
# if defined key_vertical
! VERTICAL REFINEMENT BEGIN
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)

         DO ji=i1,i2
            iref = ji
            IF (western_side) iref = MAX(2,ji)
            IF (eastern_side) iref = MIN(nlci-2,ji)
            DO jj=j1,j2
               N_in = 0
               DO jk=k1,k2
                  IF (ptab(ji,jj,jk,2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk) = ptab(ji,jj,jk,1)/ptab(ji,jj,jk,2)
                  h_in(N_in) = ptab(ji,jj,jk,2)/(e2u(ji,jj)*zrhoy) 
              ENDDO
         
              IF (N_in == 0) THEN
                 ua(ji,jj,:) = 0._wp
                 CYCLE
              ENDIF
         
              N_out = 0
              DO jk=1,jpk
                 if (umask(iref,jj,jk) == 0) EXIT
                 N_out = N_out + 1
                 h_out(N_out) = e3u_a(iref,jj,jk)
              ENDDO
         
              IF (N_out == 0) THEN
                 ua(ji,jj,:) = 0._wp
                 CYCLE
              ENDIF
         
              IF (N_in * N_out > 0) THEN
                 h_diff = sum(h_out(1:N_out))-sum(h_in(1:N_in))
! Should be able to remove the next IF/ELSEIF statement once scale factors are dealt with properly
                 if (h_diff < -1.e4) then
                    print *,'CHECK YOUR BATHY ...', h_diff, sum(h_out(1:N_out)), sum(h_in(1:N_in))
!                    stop
                 endif
              ENDIF
              call reconstructandremap(tabin(1:N_in),h_in(1:N_in),ua(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out)
            ENDDO
         ENDDO

# else
         DO jk = 1, jpkm1
            DO jj=j1,j2
               ua(i1:i2,jj,jk) = ptab(i1:i2,jj,jk,1) / ( zrhoy * e2u(i1:i2,jj) * e3u_a(i1:i2,jj,jk) )
            END DO
         END DO
# endif

      ENDIF
      ! 
   END SUBROUTINE interpun

   SUBROUTINE interpvn( ptab, i1, i2, j1, j2, k1, k2, m1, m2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interpvn ***
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: ptab
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb , ndir
      !
      INTEGER :: ji,jj,jk
      REAL(wp) :: zrhox
      ! vertical interpolation:
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER  :: N_in, N_out, jref
      REAL(wp) :: h_diff
      LOGICAL  :: northern_side,southern_side
      !!---------------------------------------------    
      !      
      IF (before) THEN          
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,jk,1) = (e1v(ji,jj) * e3v_n(ji,jj,jk) * vn(ji,jj,jk)*vmask(ji,jj,jk))
# if defined key_vertical
                  ptab(ji,jj,jk,2) = vmask(ji,jj,jk) * e1v(ji,jj) * e3v_n(ji,jj,jk)
# endif
               END DO
            END DO
         END DO
      ELSE       
         zrhox = Agrif_rhox()
# if defined key_vertical

         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)

         DO jj=j1,j2
            jref = jj
            IF (southern_side) jref = MAX(2,jj)
            IF (northern_side) jref = MIN(nlcj-2,jj)
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2
                  if (ptab(ji,jj,jk,2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk) = ptab(ji,jj,jk,1)/ptab(ji,jj,jk,2)
                  h_in(N_in) = ptab(ji,jj,jk,2)/(e1v(ji,jj)*zrhox)
               END DO
               IF (N_in == 0) THEN
                  va(ji,jj,:) = 0._wp
                  CYCLE
               ENDIF
         
               N_out = 0
               DO jk=1,jpk
                  if (vmask(ji,jref,jk) == 0) EXIT
                  N_out = N_out + 1
                  h_out(N_out) = e3v_a(ji,jref,jk)
               END DO
               IF (N_out == 0) THEN
                 va(ji,jj,:) = 0._wp
                 CYCLE
               ENDIF
               call reconstructandremap(tabin(1:N_in),h_in(1:N_in),va(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out)
            END DO
         END DO
# else
         DO jk = 1, jpkm1
            va(i1:i2,j1:j2,jk) = ptab(i1:i2,j1:j2,jk,1) / ( zrhox * e1v(i1:i2,j1:j2) * e3v_a(i1:i2,j1:j2,jk) )
         END DO
# endif
      ENDIF
      !        
   END SUBROUTINE interpvn

   SUBROUTINE interpunb( ptab, i1, i2, j1, j2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpunb  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      INTEGER  ::   ji, jj
      REAL(wp) ::   zrhoy, zrhot, zt0, zt1, ztcoeff
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !!----------------------------------------------------------------------  
      !
      IF( before ) THEN 
         ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * hu_n(i1:i2,j1:j2) * un_b(i1:i2,j1:j2)
      ELSE
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         zrhoy = Agrif_Rhoy()
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot      
         ! Polynomial interpolation coefficients:
         IF( bdy_tinterp == 1 ) THEN
            ztcoeff = zrhot * (  zt1**2._wp * (       zt1 - 1._wp)        &
               &               - zt0**2._wp * (       zt0 - 1._wp)        )
         ELSEIF( bdy_tinterp == 2 ) THEN
            ztcoeff = zrhot * (  zt1        * (       zt1 - 1._wp)**2._wp &
               &               - zt0        * (       zt0 - 1._wp)**2._wp )
         ELSE
            ztcoeff = 1
         ENDIF
         !   
         IF(western_side)   ubdy_w(1:nbghostcells,j1:j2) = ubdy_w(1:nbghostcells,j1:j2) + ztcoeff * ptab(i1:i2,j1:j2)  
         IF(eastern_side)   ubdy_e(1:nbghostcells,j1:j2) = ubdy_e(1:nbghostcells,j1:j2) + ztcoeff * ptab(i1:i2,j1:j2)  
         IF(southern_side)  ubdy_s(i1:i2,1:nbghostcells) = ubdy_s(i1:i2,1:nbghostcells) + ztcoeff * ptab(i1:i2,j1:j2)
         IF(northern_side)  ubdy_n(i1:i2,1:nbghostcells) = ubdy_n(i1:i2,1:nbghostcells) + ztcoeff * ptab(i1:i2,j1:j2) 
         !            
         IF( bdy_tinterp == 0 .OR. bdy_tinterp == 2) THEN
            IF(western_side)   ubdy_w(1:nbghostcells,j1:j2) = ubdy_w(1:nbghostcells,j1:j2) / (zrhoy*e2u(i1:i2,j1:j2)) * umask(i1:i2,j1:j2,1)
            IF(eastern_side)   ubdy_e(1:nbghostcells,j1:j2) = ubdy_e(1:nbghostcells,j1:j2) / (zrhoy*e2u(i1:i2,j1:j2)) * umask(i1:i2,j1:j2,1)
            IF(southern_side)  ubdy_s(i1:i2,1:nbghostcells) = ubdy_s(i1:i2,1:nbghostcells) / (zrhoy*e2u(i1:i2,j1:j2)) * umask(i1:i2,j1:j2,1)
            IF(northern_side)  ubdy_n(i1:i2,1:nbghostcells) = ubdy_n(i1:i2,1:nbghostcells) / (zrhoy*e2u(i1:i2,j1:j2)) * umask(i1:i2,j1:j2,1)
         ENDIF
      ENDIF
      ! 
   END SUBROUTINE interpunb


   SUBROUTINE interpvnb( ptab, i1, i2, j1, j2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpvnb  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      INTEGER  ::   ji,jj
      REAL(wp) ::   zrhox, zrhot, zt0, zt1, ztcoeff   
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !!----------------------------------------------------------------------  
      ! 
      IF( before ) THEN 
         ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * hv_n(i1:i2,j1:j2) * vn_b(i1:i2,j1:j2)
      ELSE
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         zrhox = Agrif_Rhox()
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot      
         IF( bdy_tinterp == 1 ) THEN
            ztcoeff = zrhot * (  zt1**2._wp * (       zt1 - 1._wp)        &
               &               - zt0**2._wp * (       zt0 - 1._wp)        )
         ELSEIF( bdy_tinterp == 2 ) THEN
            ztcoeff = zrhot * (  zt1        * (       zt1 - 1._wp)**2._wp &
               &               - zt0        * (       zt0 - 1._wp)**2._wp ) 
         ELSE
            ztcoeff = 1
         ENDIF
         !! clem ghost
         IF(western_side)   vbdy_w(1:nbghostcells,j1:j2) = vbdy_w(1:nbghostcells,j1:j2) + ztcoeff * ptab(i1:i2,j1:j2)  
         IF(eastern_side)   vbdy_e(1:nbghostcells,j1:j2) = vbdy_e(1:nbghostcells,j1:j2) + ztcoeff * ptab(i1:i2,j1:j2)   
         IF(southern_side)  vbdy_s(i1:i2,1:nbghostcells) = vbdy_s(i1:i2,1:nbghostcells) + ztcoeff * ptab(i1:i2,j1:j2)
         IF(northern_side)  vbdy_n(i1:i2,1:nbghostcells) = vbdy_n(i1:i2,1:nbghostcells) + ztcoeff * ptab(i1:i2,j1:j2) 
         !            
         IF( bdy_tinterp == 0 .OR. bdy_tinterp == 2) THEN
            IF(western_side)   vbdy_w(1:nbghostcells,j1:j2) = vbdy_w(1:nbghostcells,j1:j2) / (zrhox*e1v(i1:i2,j1:j2)) * vmask(i1:i2,j1:j2,1)
            IF(eastern_side)   vbdy_e(1:nbghostcells,j1:j2) = vbdy_e(1:nbghostcells,j1:j2) / (zrhox*e1v(i1:i2,j1:j2)) * vmask(i1:i2,j1:j2,1)
            IF(southern_side)  vbdy_s(i1:i2,1:nbghostcells) = vbdy_s(i1:i2,1:nbghostcells) / (zrhox*e1v(i1:i2,j1:j2)) * vmask(i1:i2,j1:j2,1)
            IF(northern_side)  vbdy_n(i1:i2,1:nbghostcells) = vbdy_n(i1:i2,1:nbghostcells) / (zrhox*e1v(i1:i2,j1:j2)) * vmask(i1:i2,j1:j2,1)
         ENDIF
      ENDIF
      !
   END SUBROUTINE interpvnb


   SUBROUTINE interpub2b( ptab, i1, i2, j1, j2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpub2b  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      INTEGER  ::   ji,jj
      REAL(wp) ::   zrhot, zt0, zt1,zat
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !!----------------------------------------------------------------------  
      IF( before ) THEN
         IF ( ln_bt_fw ) THEN
            ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * ub2_b(i1:i2,j1:j2)
         ELSE
            ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * un_adv(i1:i2,j1:j2)
         ENDIF
      ELSE
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot
         ! Polynomial interpolation coefficients:
         zat = zrhot * (  zt1**2._wp * (-2._wp*zt1 + 3._wp)    &
            &           - zt0**2._wp * (-2._wp*zt0 + 3._wp)    ) 
         !! clem ghost
         IF(western_side ) ubdy_w(1:nbghostcells,j1:j2) = zat * ptab(i1:i2,j1:j2)  
         IF(eastern_side ) ubdy_e(1:nbghostcells,j1:j2) = zat * ptab(i1:i2,j1:j2)  
         IF(southern_side) ubdy_s(i1:i2,1:nbghostcells) = zat * ptab(i1:i2,j1:j2)
         IF(northern_side) ubdy_n(i1:i2,1:nbghostcells) = zat * ptab(i1:i2,j1:j2) 
      ENDIF
      ! 
   END SUBROUTINE interpub2b
   

   SUBROUTINE interpvb2b( ptab, i1, i2, j1, j2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpvb2b  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      INTEGER ::   ji,jj
      REAL(wp) ::   zrhot, zt0, zt1,zat
      LOGICAL ::   western_side, eastern_side,northern_side,southern_side
      !!----------------------------------------------------------------------  
      !
      IF( before ) THEN
         IF ( ln_bt_fw ) THEN
            ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * vb2_b(i1:i2,j1:j2)
         ELSE
            ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * vn_adv(i1:i2,j1:j2)
         ENDIF
      ELSE      
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot
         ! Polynomial interpolation coefficients:
         zat = zrhot * (  zt1**2._wp * (-2._wp*zt1 + 3._wp)    &
            &           - zt0**2._wp * (-2._wp*zt0 + 3._wp)    ) 
         !
         IF(western_side )   vbdy_w(1:nbghostcells,j1:j2) = zat * ptab(i1:i2,j1:j2)  
         IF(eastern_side )   vbdy_e(1:nbghostcells,j1:j2) = zat * ptab(i1:i2,j1:j2)  
         IF(southern_side)   vbdy_s(i1:i2,1:nbghostcells) = zat * ptab(i1:i2,j1:j2)
         IF(northern_side)   vbdy_n(i1:i2,1:nbghostcells) = zat * ptab(i1:i2,j1:j2) 
      ENDIF
      !      
   END SUBROUTINE interpvb2b


   SUBROUTINE interpe3t( ptab, i1, i2, j1, j2, k1, k2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpe3t  ***
      !!----------------------------------------------------------------------  
      INTEGER                              , INTENT(in   ) :: i1, i2, j1, j2, k1, k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: ptab
      LOGICAL                              , INTENT(in   ) :: before
      INTEGER                              , INTENT(in   ) :: nb , ndir
      !
      INTEGER :: ji, jj, jk
      LOGICAL :: western_side, eastern_side, northern_side, southern_side
      !!----------------------------------------------------------------------  
      !    
      IF( before ) THEN
         ptab(i1:i2,j1:j2,k1:k2) = tmask(i1:i2,j1:j2,k1:k2) * e3t_0(i1:i2,j1:j2,k1:k2)
      ELSE
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         !
         DO jk = k1, k2
            DO jj = j1, j2
               DO ji = i1, i2
                  !
                  IF( ABS( ptab(ji,jj,jk) - tmask(ji,jj,jk) * e3t_0(ji,jj,jk) ) > 1.D-2) THEN
                     IF (western_side.AND.(ptab(i1+nbghostcells-1,jj,jk)>0._wp)) THEN
                        WRITE(numout,*) 'ERROR bathymetry merge at the western border ji,jj,jk ', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*)  ptab(ji,jj,jk), e3t_0(ji,jj,jk) 
                        kindic_agr = kindic_agr + 1
                     ELSEIF (eastern_side.AND.(ptab(i2-nbghostcells+1,jj,jk)>0._wp)) THEN
                        WRITE(numout,*) 'ERROR bathymetry merge at the eastern border ji,jj,jk ', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*)  ptab(ji,jj,jk), e3t_0(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ELSEIF (southern_side.AND.(ptab(ji,j1+nbghostcells-1,jk)>0._wp)) THEN
                        WRITE(numout,*) 'ERROR bathymetry merge at the southern border ji,jj,jk', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*)  ptab(ji,jj,jk), e3t_0(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ELSEIF (northern_side.AND.(ptab(ji,j2-nbghostcells+1,jk)>0._wp)) THEN
                        WRITE(numout,*) 'ERROR bathymetry merge at the northen border ji,jj,jk', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*)  ptab(ji,jj,jk), e3t_0(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE interpe3t


   SUBROUTINE interpumsk( ptab, i1, i2, j1, j2, k1, k2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpumsk  ***
      !!----------------------------------------------------------------------  
      INTEGER                              , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                              , INTENT(in   ) ::   before
      INTEGER                              , INTENT(in   ) ::   nb , ndir
      !
      INTEGER ::   ji, jj, jk
      LOGICAL ::   western_side, eastern_side   
      !!----------------------------------------------------------------------  
      !    
      IF( before ) THEN
         ptab(i1:i2,j1:j2,k1:k2) = umask(i1:i2,j1:j2,k1:k2)
      ELSE
         western_side = (nb == 1).AND.(ndir == 1)
         eastern_side = (nb == 1).AND.(ndir == 2)
         DO jk = k1, k2
            DO jj = j1, j2
               DO ji = i1, i2
                   ! Velocity mask at boundary edge points:
                  IF (ABS(ptab(ji,jj,jk) - umask(ji,jj,jk)) > 1.D-2) THEN
                     IF (western_side) THEN
                        WRITE(numout,*) 'ERROR with umask at the western border ji,jj,jk ', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*) '      masks: parent, child ', ptab(ji,jj,jk), umask(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ELSEIF (eastern_side) THEN
                        WRITE(numout,*) 'ERROR with umask at the eastern border ji,jj,jk ', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*) '      masks: parent, child ', ptab(ji,jj,jk), umask(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE interpumsk


   SUBROUTINE interpvmsk( ptab, i1, i2, j1, j2, k1, k2, before, nb, ndir )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpvmsk  ***
      !!----------------------------------------------------------------------  
      INTEGER                              , INTENT(in   ) ::   i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                              , INTENT(in   ) ::   before
      INTEGER                              , INTENT(in   ) :: nb , ndir
      !
      INTEGER ::   ji, jj, jk
      LOGICAL ::   northern_side, southern_side     
      !!----------------------------------------------------------------------  
      !    
      IF( before ) THEN
         ptab(i1:i2,j1:j2,k1:k2) = vmask(i1:i2,j1:j2,k1:k2)
      ELSE
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         DO jk = k1, k2
            DO jj = j1, j2
               DO ji = i1, i2
                   ! Velocity mask at boundary edge points:
                  IF (ABS(ptab(ji,jj,jk) - vmask(ji,jj,jk)) > 1.D-2) THEN
                     IF (southern_side) THEN
                        WRITE(numout,*) 'ERROR with vmask at the southern border ji,jj,jk ', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*) '      masks: parent, child ', ptab(ji,jj,jk), vmask(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ELSEIF (northern_side) THEN
                        WRITE(numout,*) 'ERROR with vmask at the northern border ji,jj,jk ', ji+nimpp-1,jj+njmpp-1,jk
                        WRITE(numout,*) '      masks: parent, child ', ptab(ji,jj,jk), vmask(ji,jj,jk)
                        kindic_agr = kindic_agr + 1
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE interpvmsk


   SUBROUTINE interpavm( ptab, i1, i2, j1, j2, k1, k2, m1, m2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interavm  ***
      !!----------------------------------------------------------------------  
      INTEGER                                    , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, m1, m2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) ::   ptab
      LOGICAL                                    , INTENT(in   ) ::   before
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER  :: N_in, N_out, ji, jj, jk
      !!----------------------------------------------------------------------  
      !      
      IF (before) THEN         
         DO jk=k1,k2
            DO jj=j1,j2
              DO ji=i1,i2
                    ptab(ji,jj,jk,1) = avm_k(ji,jj,jk)
              END DO
           END DO
        END DO
#ifdef key_vertical         
        DO jk=k1,k2
           DO jj=j1,j2
              DO ji=i1,i2
                 ptab(ji,jj,jk,2) = wmask(ji,jj,jk) * e3w_n(ji,jj,jk) 
              END DO
           END DO
        END DO
#endif
      ELSE 
#ifdef key_vertical         
         avm_k(i1:i2,j1:j2,1:jpk) = 0.
         DO jj=j1,j2
            DO ji=i1,i2
               N_in = 0
               DO jk=k1,k2 !k2 = jpk of parent grid
                  IF (ptab(ji,jj,jk,2) == 0) EXIT
                  N_in = N_in + 1
                  tabin(jk) = ptab(ji,jj,jk,1)
                  h_in(N_in) = ptab(ji,jj,jk,2)
               END DO
               N_out = 0
               DO jk=1,jpk ! jpk of child grid
                  IF (wmask(ji,jj,jk) == 0) EXIT 
                  N_out = N_out + 1
                  h_out(jk) = e3t_n(ji,jj,jk)
               ENDDO
               IF (N_in > 0) THEN
                  CALL reconstructandremap(tabin(1:N_in),h_in,avm_k(ji,jj,1:N_out),h_out,N_in,N_out)
               ENDIF
            ENDDO
         ENDDO
#else
         avm_k(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2,1)
#endif
      ENDIF
      !
   END SUBROUTINE interpavm

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_OCE_Interp_empty
      WRITE(*,*)  'agrif_oce_interp : You should not have seen this print! error?'
   END SUBROUTINE Agrif_OCE_Interp_empty
#endif

   !!======================================================================
END MODULE agrif_oce_interp
