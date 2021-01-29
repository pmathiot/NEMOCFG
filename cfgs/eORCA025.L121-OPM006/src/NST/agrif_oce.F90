MODULE agrif_oce
   !!======================================================================
   !!                       ***  MODULE agrif_oce  ***
   !! AGRIF :   define in memory AGRIF variables
   !!----------------------------------------------------------------------
   !! History :  2.0  ! 2007-12  (R. Benshila)  Original code
   !!----------------------------------------------------------------------
#if defined key_agrif
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE par_oce      ! ocean parameters
   USE dom_oce      ! domain parameters

   IMPLICIT NONE
   PRIVATE 

   PUBLIC agrif_oce_alloc ! routine called by nemo_init in nemogcm.F90
#if defined key_vertical
   PUBLIC reconstructandremap ! remapping routine
#endif   
   !                                              !!* Namelist namagrif: AGRIF parameters
   LOGICAL , PUBLIC ::   ln_spc_dyn    = .FALSE.   !:
   INTEGER , PUBLIC, PARAMETER ::   nn_sponge_len = 2  !: Sponge width (in number of parent grid points)
   REAL(wp), PUBLIC ::   rn_sponge_tra = 2800.     !: sponge coeff. for tracers
   REAL(wp), PUBLIC ::   rn_sponge_dyn = 2800.     !: sponge coeff. for dynamics
   LOGICAL , PUBLIC ::   ln_chk_bathy  = .FALSE.   !: check of parent bathymetry 
   LOGICAL , PUBLIC ::   lk_agrif_clp  = .FALSE.   !: Force clamped bcs
   !                                              !!! OLD namelist names
   REAL(wp), PUBLIC ::   visc_tra                  !: sponge coeff. for tracers
   REAL(wp), PUBLIC ::   visc_dyn                  !: sponge coeff. for dynamics

   LOGICAL , PUBLIC :: spongedoneT = .FALSE.       !: tracer   sponge layer indicator
   LOGICAL , PUBLIC :: spongedoneU = .FALSE.       !: dynamics sponge layer indicator
   LOGICAL , PUBLIC :: lk_agrif_fstep = .TRUE.     !: if true: first step
   LOGICAL , PUBLIC :: lk_agrif_debug = .FALSE.    !: if true: print debugging info

   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_tsn
# if defined key_top
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_trn
# endif
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_u
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_v
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: fsaht_spu, fsaht_spv !: sponge diffusivities
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: fsahm_spt, fsahm_spf !: sponge viscosities

   ! Barotropic arrays used to store open boundary data during time-splitting loop:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ubdy_w, vbdy_w, hbdy_w
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ubdy_e, vbdy_e, hbdy_e
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ubdy_n, vbdy_n, hbdy_n
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ubdy_s, vbdy_s, hbdy_s


   INTEGER, PUBLIC :: tsn_id                                                  ! AGRIF profile for tracers interpolation and update
   INTEGER, PUBLIC :: un_interp_id, vn_interp_id                              ! AGRIF profiles for interpolations
   INTEGER, PUBLIC :: un_update_id, vn_update_id                              ! AGRIF profiles for udpates
   INTEGER, PUBLIC :: tsn_sponge_id, un_sponge_id, vn_sponge_id               ! AGRIF profiles for sponge layers
# if defined key_top
   INTEGER, PUBLIC :: trn_id, trn_sponge_id
# endif  
   INTEGER, PUBLIC :: unb_id, vnb_id, ub2b_interp_id, vb2b_interp_id
   INTEGER, PUBLIC :: ub2b_update_id, vb2b_update_id
   INTEGER, PUBLIC :: e3t_id, e1u_id, e2v_id, sshn_id
   INTEGER, PUBLIC :: scales_t_id
   INTEGER, PUBLIC :: avt_id, avm_id, en_id                ! TKE related identificators
   INTEGER, PUBLIC :: umsk_id, vmsk_id
   INTEGER, PUBLIC :: kindic_agr
   
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce.F90 10425 2018-12-19 21:54:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS 

   INTEGER FUNCTION agrif_oce_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION agrif_oce_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(2) :: ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( fsaht_spu(jpi,jpj), fsaht_spv(jpi,jpj),   &
         &      fsahm_spt(jpi,jpj), fsahm_spf(jpi,jpj),   &
         &      tabspongedone_tsn(jpi,jpj),           &
# if defined key_top         
         &      tabspongedone_trn(jpi,jpj),           &
# endif         
         &      tabspongedone_u  (jpi,jpj),           &
         &      tabspongedone_v  (jpi,jpj), STAT = ierr(1) )

      ALLOCATE( ubdy_w(nbghostcells,jpj), vbdy_w(nbghostcells,jpj), hbdy_w(nbghostcells,jpj),   &
         &      ubdy_e(nbghostcells,jpj), vbdy_e(nbghostcells,jpj), hbdy_e(nbghostcells,jpj),   & 
         &      ubdy_n(jpi,nbghostcells), vbdy_n(jpi,nbghostcells), hbdy_n(jpi,nbghostcells),   & 
         &      ubdy_s(jpi,nbghostcells), vbdy_s(jpi,nbghostcells), hbdy_s(jpi,nbghostcells), STAT = ierr(2) )

      agrif_oce_alloc = MAXVAL(ierr)
      !
   END FUNCTION agrif_oce_alloc

#if defined key_vertical
   SUBROUTINE reconstructandremap(tabin,hin,tabout,hout,N,Nout)      
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION reconstructandremap  ***
      !!----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N, Nout
      REAL(wp) tabin(N), tabout(Nout)
      REAL(wp) hin(N), hout(Nout)
      REAL(wp) coeffremap(N,3),zwork(N,3)
      REAL(wp) zwork2(N+1,3)
      INTEGER jk
      DOUBLE PRECISION, PARAMETER :: dsmll=1.0d-8  
      REAL(wp) q,q01,q02,q001,q002,q0
      REAL(wp) z_win(1:N+1), z_wout(1:Nout+1)
      REAL(wp),PARAMETER :: dpthin = 1.D-3
      INTEGER :: k1, kbox, ktop, ka, kbot
      REAL(wp) :: tsum, qbot, rpsum, zbox, ztop, zthk, zbot, offset, qtop

      z_win(1)=0.; z_wout(1)= 0.
      DO jk=1,N
         z_win(jk+1)=z_win(jk)+hin(jk)
      ENDDO 
      
      DO jk=1,Nout
         z_wout(jk+1)=z_wout(jk)+hout(jk)       
      ENDDO       

      DO jk=2,N
         zwork(jk,1)=1./(hin(jk-1)+hin(jk))
      ENDDO
        
      DO jk=2,N-1
         q0 = 1./(hin(jk-1)+hin(jk)+hin(jk+1))
         zwork(jk,2)=hin(jk-1)+2.*hin(jk)+hin(jk+1)
         zwork(jk,3)=q0
      ENDDO       
     
      DO jk= 2,N
         zwork2(jk,1)=zwork(jk,1)*(tabin(jk)-tabin(jk-1))
      ENDDO
        
      coeffremap(:,1) = tabin(:)
 
      DO jk=2,N-1
         q001 = hin(jk)*zwork2(jk+1,1)
         q002 = hin(jk)*zwork2(jk,1)        
         IF (q001*q002 < 0) then
            q001 = 0.
            q002 = 0.
         ENDIF
         q=zwork(jk,2)
         q01=q*zwork2(jk+1,1)
         q02=q*zwork2(jk,1)
         IF (abs(q001) > abs(q02)) q001 = q02
         IF (abs(q002) > abs(q01)) q002 = q01
        
         q=(q001-q002)*zwork(jk,3)
         q001=q001-q*hin(jk+1)
         q002=q002+q*hin(jk-1)
        
         coeffremap(jk,3)=coeffremap(jk,1)+q001
         coeffremap(jk,2)=coeffremap(jk,1)-q002
        
         zwork2(jk,1)=(2.*q001-q002)**2
         zwork2(jk,2)=(2.*q002-q001)**2
      ENDDO
        
      DO jk=1,N
         IF(jk.EQ.1 .OR. jk.EQ.N .OR. hin(jk).LE.dpthin) THEN
            coeffremap(jk,3) = coeffremap(jk,1)
            coeffremap(jk,2) = coeffremap(jk,1)
            zwork2(jk,1) = 0.
            zwork2(jk,2) = 0.
         ENDIF
      ENDDO
        
      DO jk=2,N
         q002=max(zwork2(jk-1,2),dsmll)
         q001=max(zwork2(jk,1),dsmll)
         zwork2(jk,3)=(q001*coeffremap(jk-1,3)+q002*coeffremap(jk,2))/(q001+q002)
      ENDDO
        
      zwork2(1,3) = 2*coeffremap(1,1)-zwork2(2,3)
      zwork2(N+1,3)=2*coeffremap(N,1)-zwork2(N,3)
 
      DO jk=1,N
         q01=zwork2(jk+1,3)-coeffremap(jk,1)
         q02=coeffremap(jk,1)-zwork2(jk,3)
         q001=2.*q01
         q002=2.*q02
         IF (q01*q02<0) then
            q01=0.
            q02=0.
         ELSEIF (abs(q01)>abs(q002)) then
            q01=q002
         ELSEIF (abs(q02)>abs(q001)) then
            q02=q001
         ENDIF
         coeffremap(jk,2)=coeffremap(jk,1)-q02
         coeffremap(jk,3)=coeffremap(jk,1)+q01
      ENDDO

      zbot=0.0
      kbot=1
      DO jk=1,Nout
         ztop=zbot  !top is bottom of previous layer
         ktop=kbot
         IF     (ztop.GE.z_win(ktop+1)) then
            ktop=ktop+1
         ENDIF
        
         zbot=z_wout(jk+1)
         zthk=zbot-ztop

         IF(zthk.GT.dpthin .AND. ztop.LT.z_wout(Nout+1)) THEN

            kbot=ktop
            DO while (z_win(kbot+1).lt.zbot.and.kbot.lt.N)
               kbot=kbot+1
            ENDDO
            zbox=zbot
            DO k1= jk+1,Nout
               IF     (z_wout(k1+1)-z_wout(k1).GT.dpthin) THEN
                  exit !thick layer
               ELSE
                  zbox=z_wout(k1+1)  !include thin adjacent layers
                  IF(zbox.EQ.z_wout(Nout+1)) THEN
                     exit !at bottom
                  ENDIF
               ENDIF
            ENDDO
            zthk=zbox-ztop

            kbox=ktop
            DO while (z_win(kbox+1).lt.zbox.and.kbox.lt.N)
               kbox=kbox+1
            ENDDO
          
            IF(ktop.EQ.kbox) THEN
               IF(z_wout(jk).NE.z_win(kbox).OR.z_wout(jk+1).NE.z_win(kbox+1)) THEN
                  IF(hin(kbox).GT.dpthin) THEN
                     q001 = (zbox-z_win(kbox))/hin(kbox)
                     q002 = (ztop-z_win(kbox))/hin(kbox)
                     q01=q001**2+q002**2+q001*q002+1.-2.*(q001+q002)
                     q02=q01-1.+(q001+q002)
                     q0=1.-q01-q02
                  ELSE
                     q0 = 1.0
                     q01 = 0.
                     q02 = 0.
                  ENDIF
                  tabout(jk)=q0*coeffremap(kbox,1)+q01*coeffremap(kbox,2)+q02*coeffremap(kbox,3)
               ELSE
                  tabout(jk) = tabin(kbox)
               ENDIF 
            ELSE
               IF(ktop.LE.jk .AND. kbox.GE.jk) THEN
                  ka = jk
               ELSEIF (kbox-ktop.GE.3) THEN
                  ka = (kbox+ktop)/2
               ELSEIF (hin(ktop).GE.hin(kbox)) THEN
                  ka = ktop
               ELSE
                  ka = kbox
               ENDIF !choose ka
    
               offset=coeffremap(ka,1)
    
               qtop = z_win(ktop+1)-ztop !partial layer thickness
               IF(hin(ktop).GT.dpthin) THEN
                  q=(ztop-z_win(ktop))/hin(ktop)
                  q01=q*(q-1.)
                  q02=q01+q
                  q0=1-q01-q02            
               ELSE
                  q0 = 1.
                  q01 = 0.
                  q02 = 0.
               ENDIF
               
               tsum =((q0*coeffremap(ktop,1)+q01*coeffremap(ktop,2)+q02*coeffremap(ktop,3))-offset)*qtop
    
               DO k1= ktop+1,kbox-1
                  tsum =tsum +(coeffremap(k1,1)-offset)*hin(k1)
               ENDDO !k1
               
               qbot = zbox-z_win(kbox) !partial layer thickness
               IF(hin(kbox).GT.dpthin) THEN
                  q=qbot/hin(kbox)
                  q01=(q-1.)**2
                  q02=q01-1.+q
                  q0=1-q01-q02                            
               ELSE
                  q0 = 1.0
                  q01 = 0.
                  q02 = 0.
               ENDIF
              
               tsum = tsum +((q0*coeffremap(kbox,1)+q01*coeffremap(kbox,2)+q02*coeffremap(kbox,3))-offset)*qbot
               
               rpsum=1.0d0/zthk
               tabout(jk)=offset+tsum*rpsum
                 
            ENDIF !single or multiple layers
         ELSE
            IF (jk==1) THEN
               write(*,'(a7,i4,i4,3f12.5)')'problem = ',N,Nout,zthk,z_wout(jk+1),hout(1)
            ENDIF
            tabout(jk) = tabout(jk-1)
             
         ENDIF !normal:thin layer
      ENDDO !jk
            
      return
      end subroutine reconstructandremap
#endif

#endif
   !!======================================================================
END MODULE agrif_oce
