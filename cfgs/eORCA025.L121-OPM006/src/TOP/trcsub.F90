MODULE trcsub
   !!======================================================================
   !!                       ***  MODULE trcsubstp  ***
   !!   TOP :   Averages physics variables for TOP substepping. 
   !!======================================================================
   !! History :  1.0  !  2011-10  (K. Edwards)  Original
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   trc_sub       : passive tracer system sub-stepping 
   !!----------------------------------------------------------------------
   USE oce_trc        ! ocean dynamics and active tracers variables
   USE trc
   USE trabbl         ! bottom boundary layer
   USE zdf_oce
   USE domvvl
   USE divhor         ! horizontal divergence
   USE sbcrnf   , ONLY: h_rnf, nk_rnf    ! River runoff
   USE bdy_oce  , ONLY: ln_bdy, bdytmask ! BDY
   !
   USE prtctl_trc     ! Print control for debbuging
   USE in_out_manager ! 
   USE iom
   USE lbclnk
#if defined key_agrif
   USE agrif_oce_update
   USE agrif_oce_interp
#endif

   IMPLICIT NONE

   PUBLIC   trc_sub_stp     ! called by trc_stp
   PUBLIC   trc_sub_ini     ! called by trc_ini to initialize substepping arrays.
   PUBLIC   trc_sub_reset   ! called by trc_stp to reset physics variables
   PUBLIC   trc_sub_ssh     ! called by trc_stp to reset physics variables

   REAL(wp) :: r1_ndttrc     ! = 1 /  nn_dttrc 
   REAL(wp) :: r1_ndttrcp1   ! = 1 / (nn_dttrc+1) 


   !! averaged and temporary saved variables  (needed when a larger passive tracer time-step is used)
   !! ----------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::    un_tm ,   un_temp   !: i-horizontal velocity average     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::    vn_tm ,   vn_temp   !: j-horizontal velocity average     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::              wn_temp   !: hold current values of avt, un, vn, wn
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   tsn_tm ,  tsn_temp   !: t/s average     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   avs_tm ,  avs_temp   !: vertical diffusivity coeff. at  w-point   [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  rhop_tm , rhop_temp   !: 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshn_tm , sshn_temp   !: average ssh for the now step [m]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::    rnf_tm ,    rnf_temp   !: river runoff
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  h_rnf_tm ,  h_rnf_temp   !: depth in metres to the bottom of the relevant grid box
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   hmld_tm ,   hmld_temp   !: mixed layer depth average [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   fr_i_tm ,   fr_i_temp   !: average ice fraction     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::    emp_tm ,    emp_temp   !: freshwater budget: volume flux [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     :: fmmflx_tm , fmmflx_temp   !: freshwater budget: freezing/melting [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     :: emp_b_hold,  emp_b_temp   !: hold emp from the beginning of each sub-stepping[m]  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::    qsr_tm ,    qsr_temp   !: solar radiation average [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   wndm_tm ,   wndm_temp   !: 10m wind average [m]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshb_hold   !:hold sshb from the beginning of each sub-stepping[m]  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   sshb_temp, ssha_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  hdivn_temp, rotn_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  hdivb_temp, rotb_temp
   !
   !                                                    !!- bottom boundary layer param (ln_trabbl=T)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahu_bbl_tm, ahu_bbl_temp  ! BBL diffusive i-coef.
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahv_bbl_tm, ahv_bbl_temp  ! BBL diffusive j-coef.
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  utr_bbl_tm, utr_bbl_temp  ! BBL u-advection
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  vtr_bbl_tm, vtr_bbl_temp  ! BBL v-advection

   !                                                      !!- iso-neutral slopes (if l_ldfslp=T)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   uslp_temp, vslp_temp, wslpi_temp, wslpj_temp   !: hold current values 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   uslp_tm  , vslp_tm  , wslpi_tm  , wslpj_tm     !: time mean 


   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcsub.F90 10425 2018-12-19 21:54:16Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sub_stp( kt )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp  ***
      !!                      
      !! ** Purpose : Average variables needed for sub-stepping passive tracers
      !! 
      !! ** Method  : Called every timestep to increment _tm (time mean) variables
      !!              on TOP steps, calculate averages.
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      REAL(wp)::   z1_ne3t, z1_ne3u, z1_ne3v, z1_ne3w   ! local scalars
      !!-------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sub_stp')
      !
      IF( kt == nit000 ) THEN
           IF(lwp) WRITE(numout,*)
           IF(lwp) WRITE(numout,*) 'trc_sub_stp : substepping of the passive tracers'
           IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
           !
           sshb_hold  (:,:) = sshn  (:,:)
           emp_b_hold (:,:) = emp_b (:,:)
           !
           r1_ndttrc        = 1._wp / REAL( nn_dttrc    , wp ) 
           r1_ndttrcp1      = 1._wp / REAL( nn_dttrc + 1, wp )
      ENDIF  

      IF( MOD( kt , nn_dttrc ) /= 0 ) THEN
         !
         un_tm   (:,:,:)        = un_tm   (:,:,:)        + un   (:,:,:)        * e3u_n(:,:,:) 
         vn_tm   (:,:,:)        = vn_tm   (:,:,:)        + vn   (:,:,:)        * e3v_n(:,:,:) 
         tsn_tm  (:,:,:,jp_tem) = tsn_tm  (:,:,:,jp_tem) + tsn  (:,:,:,jp_tem) * e3t_n(:,:,:)  
         tsn_tm  (:,:,:,jp_sal) = tsn_tm  (:,:,:,jp_sal) + tsn  (:,:,:,jp_sal) * e3t_n(:,:,:)  
         rhop_tm (:,:,:)        = rhop_tm (:,:,:)        + rhop (:,:,:)        * e3t_n(:,:,:)  
         avs_tm  (:,:,:)        = avs_tm  (:,:,:)        + avs  (:,:,:)        * e3w_n(:,:,:)  
         IF( l_ldfslp ) THEN
            uslp_tm (:,:,:)      = uslp_tm (:,:,:)        + uslp (:,:,:)
            vslp_tm (:,:,:)      = vslp_tm (:,:,:)        + vslp (:,:,:)
            wslpi_tm(:,:,:)      = wslpi_tm(:,:,:)        + wslpi(:,:,:)
            wslpj_tm(:,:,:)      = wslpj_tm(:,:,:)        + wslpj(:,:,:)
         ENDIF
         IF( ln_trabbl ) THEN
            IF( nn_bbl_ldf == 1 ) THEN
               ahu_bbl_tm(:,:)     = ahu_bbl_tm(:,:)        + ahu_bbl(:,:) 
               ahv_bbl_tm(:,:)     = ahv_bbl_tm(:,:)        + ahv_bbl(:,:) 
            ENDIF
            IF( nn_bbl_adv == 1 ) THEN
               utr_bbl_tm(:,:)     = utr_bbl_tm(:,:)        + utr_bbl(:,:) 
               vtr_bbl_tm(:,:)     = vtr_bbl_tm(:,:)        + vtr_bbl(:,:) 
            ENDIF
         ENDIF 
         !
         sshn_tm  (:,:)         = sshn_tm  (:,:)         + sshn  (:,:) 
         rnf_tm   (:,:)         = rnf_tm   (:,:)         + rnf   (:,:) 
         h_rnf_tm (:,:)         = h_rnf_tm (:,:)         + h_rnf (:,:) 
         hmld_tm  (:,:)         = hmld_tm  (:,:)         + hmld  (:,:)
         fr_i_tm  (:,:)         = fr_i_tm  (:,:)         + fr_i  (:,:)
         emp_tm   (:,:)         = emp_tm   (:,:)         + emp   (:,:) 
         fmmflx_tm(:,:)         = fmmflx_tm(:,:)         + fmmflx(:,:)
         qsr_tm   (:,:)         = qsr_tm   (:,:)         + qsr   (:,:)
         wndm_tm  (:,:)         = wndm_tm  (:,:)         + wndm  (:,:)
         !
      ELSE                           !  It is time to substep 
         !   1. set temporary arrays to hold physics/dynamical variables
         un_temp    (:,:,:)      = un    (:,:,:)
         vn_temp    (:,:,:)      = vn    (:,:,:)
         wn_temp    (:,:,:)      = wn    (:,:,:)
         tsn_temp   (:,:,:,:)    = tsn   (:,:,:,:)
         rhop_temp  (:,:,:)      = rhop  (:,:,:)    
         avs_temp   (:,:,:)      = avs   (:,:,:)
         IF( l_ldfslp ) THEN
            uslp_temp  (:,:,:)   = uslp  (:,:,:)   ;   wslpi_temp (:,:,:)   = wslpi (:,:,:)
            vslp_temp  (:,:,:)   = vslp  (:,:,:)   ;   wslpj_temp (:,:,:)   = wslpj (:,:,:)
         ENDIF
         IF( ln_trabbl ) THEN
            IF( nn_bbl_ldf == 1 ) THEN
               ahu_bbl_temp(:,:)   = ahu_bbl(:,:)  
               ahv_bbl_temp(:,:)   = ahv_bbl(:,:) 
            ENDIF
            IF( nn_bbl_adv == 1 ) THEN
               utr_bbl_temp(:,:)   = utr_bbl(:,:) 
               vtr_bbl_temp(:,:)   = vtr_bbl(:,:) 
            ENDIF
         ENDIF 
         sshn_temp  (:,:)        = sshn  (:,:)
         sshb_temp  (:,:)        = sshb  (:,:)
         ssha_temp  (:,:)        = ssha  (:,:)
         rnf_temp   (:,:)        = rnf   (:,:)
         h_rnf_temp (:,:)        = h_rnf (:,:)
         hmld_temp  (:,:)        = hmld  (:,:)
         fr_i_temp  (:,:)        = fr_i  (:,:)
         emp_temp   (:,:)        = emp   (:,:)
         emp_b_temp (:,:)        = emp_b (:,:)
         fmmflx_temp(:,:)        = fmmflx(:,:)
         qsr_temp   (:,:)        = qsr   (:,:)
         wndm_temp  (:,:)        = wndm  (:,:)
         !                                    !  Variables reset in trc_sub_ssh
         hdivn_temp (:,:,:)      = hdivn (:,:,:)
         !
         ! 2. Create averages and reassign variables
         un_tm    (:,:,:)        = un_tm   (:,:,:)        + un   (:,:,:)        * e3u_n(:,:,:) 
         vn_tm    (:,:,:)        = vn_tm   (:,:,:)        + vn   (:,:,:)        * e3v_n(:,:,:) 
         tsn_tm   (:,:,:,jp_tem) = tsn_tm  (:,:,:,jp_tem) + tsn  (:,:,:,jp_tem) * e3t_n(:,:,:)  
         tsn_tm   (:,:,:,jp_sal) = tsn_tm  (:,:,:,jp_sal) + tsn  (:,:,:,jp_sal) * e3t_n(:,:,:)  
         rhop_tm (:,:,:)         = rhop_tm (:,:,:)        + rhop (:,:,:)        * e3t_n(:,:,:)  
         avs_tm   (:,:,:)        = avs_tm  (:,:,:)        + avs  (:,:,:)        * e3w_n(:,:,:)  
         IF( l_ldfslp ) THEN
            uslp_tm  (:,:,:)     = uslp_tm (:,:,:)        + uslp (:,:,:) 
            vslp_tm  (:,:,:)     = vslp_tm (:,:,:)        + vslp (:,:,:)
            wslpi_tm (:,:,:)     = wslpi_tm(:,:,:)        + wslpi(:,:,:) 
            wslpj_tm (:,:,:)     = wslpj_tm(:,:,:)        + wslpj(:,:,:) 
         ENDIF
         IF( ln_trabbl ) THEN
            IF( nn_bbl_ldf == 1 ) THEN
               ahu_bbl_tm(:,:)     = ahu_bbl_tm(:,:)        + ahu_bbl(:,:) 
               ahv_bbl_tm(:,:)     = ahv_bbl_tm(:,:)        + ahv_bbl(:,:) 
            ENDIF
            IF( nn_bbl_adv == 1 ) THEN
               utr_bbl_tm(:,:)     = utr_bbl_tm(:,:)        + utr_bbl(:,:) 
               vtr_bbl_tm(:,:)     = vtr_bbl_tm(:,:)        + vtr_bbl(:,:) 
            ENDIF
         ENDIF 
         sshn_tm  (:,:)          = sshn_tm    (:,:)       + sshn  (:,:) 
         rnf_tm   (:,:)          = rnf_tm     (:,:)       + rnf   (:,:) 
         h_rnf_tm (:,:)          = h_rnf_tm   (:,:)       + h_rnf (:,:) 
         hmld_tm  (:,:)          = hmld_tm    (:,:)       + hmld  (:,:)
         fr_i_tm  (:,:)          = fr_i_tm    (:,:)       + fr_i  (:,:)
         emp_tm   (:,:)          = emp_tm     (:,:)       + emp   (:,:) 
         fmmflx_tm(:,:)          = fmmflx_tm  (:,:)       + fmmflx(:,:)
         qsr_tm   (:,:)          = qsr_tm     (:,:)       + qsr   (:,:)
         wndm_tm  (:,:)          = wndm_tm    (:,:)       + wndm  (:,:)
         !
         sshn     (:,:)          = sshn_tm    (:,:) * r1_ndttrcp1 
         sshb     (:,:)          = sshb_hold  (:,:)
         rnf      (:,:)          = rnf_tm     (:,:) * r1_ndttrcp1 
         h_rnf    (:,:)          = h_rnf_tm   (:,:) * r1_ndttrcp1 
         hmld     (:,:)          = hmld_tm    (:,:) * r1_ndttrcp1 
         !  variables that are initialized after averages
         emp_b    (:,:) = emp_b_hold (:,:)
         IF( kt == nittrc000 ) THEN
            wndm  (:,:)          = wndm_tm    (:,:) * r1_ndttrc 
            qsr   (:,:)          = qsr_tm     (:,:) * r1_ndttrc 
            emp   (:,:)          = emp_tm     (:,:) * r1_ndttrc 
            fmmflx(:,:)          = fmmflx_tm  (:,:) * r1_ndttrc 
            fr_i  (:,:)          = fr_i_tm    (:,:) * r1_ndttrc
            IF( ln_trabbl ) THEN
               IF( nn_bbl_ldf == 1 ) THEN
                  ahu_bbl(:,:)      = ahu_bbl_tm (:,:) * r1_ndttrc  
                  ahv_bbl(:,:)      = ahv_bbl_tm (:,:) * r1_ndttrc 
               ENDIF
               IF( nn_bbl_adv == 1 ) THEN
                  utr_bbl(:,:)      = utr_bbl_tm (:,:) * r1_ndttrc  
                  vtr_bbl(:,:)      = vtr_bbl_tm (:,:) * r1_ndttrc 
               ENDIF
            ENDIF
         ELSE
            wndm  (:,:)          = wndm_tm    (:,:) * r1_ndttrcp1 
            qsr   (:,:)          = qsr_tm     (:,:) * r1_ndttrcp1 
            emp   (:,:)          = emp_tm     (:,:) * r1_ndttrcp1 
            fmmflx(:,:)          = fmmflx_tm  (:,:) * r1_ndttrcp1 
            fr_i  (:,:)          = fr_i_tm    (:,:) * r1_ndttrcp1 
            IF( ln_trabbl ) THEN
               IF( nn_bbl_ldf == 1 ) THEN
                  ahu_bbl(:,:)      = ahu_bbl_tm (:,:) * r1_ndttrcp1  
                  ahv_bbl(:,:)      = ahv_bbl_tm (:,:) * r1_ndttrcp1 
               ENDIF
               IF( nn_bbl_adv == 1 ) THEN
                  utr_bbl(:,:)      = utr_bbl_tm (:,:) * r1_ndttrcp1  
                  vtr_bbl(:,:)      = vtr_bbl_tm (:,:) * r1_ndttrcp1 
               ENDIF
            ENDIF
         ENDIF
         !
         DO jk = 1, jpk
            DO jj = 1, jpj
               DO ji = 1, jpi
                  z1_ne3t = r1_ndttrcp1  / e3t_n(ji,jj,jk)
                  z1_ne3u = r1_ndttrcp1  / e3u_n(ji,jj,jk)
                  z1_ne3v = r1_ndttrcp1  / e3v_n(ji,jj,jk)
                  z1_ne3w = r1_ndttrcp1  / e3w_n(ji,jj,jk)
                  !
                  un   (ji,jj,jk)        = un_tm   (ji,jj,jk)        * z1_ne3u
                  vn   (ji,jj,jk)        = vn_tm   (ji,jj,jk)        * z1_ne3v
                  tsn  (ji,jj,jk,jp_tem) = tsn_tm  (ji,jj,jk,jp_tem) * z1_ne3t
                  tsn  (ji,jj,jk,jp_sal) = tsn_tm  (ji,jj,jk,jp_sal) * z1_ne3t
                  rhop (ji,jj,jk)        = rhop_tm (ji,jj,jk)        * z1_ne3t
!!gm : BUG ==>> for avs I don't understand the division by e3w
                  avs  (ji,jj,jk)        = avs_tm  (ji,jj,jk)        * z1_ne3w
               END DO
            END DO
         END DO
         IF( l_ldfslp ) THEN
            wslpi(:,:,:)        = wslpi_tm(:,:,:) 
            wslpj(:,:,:)        = wslpj_tm(:,:,:)
            uslp (:,:,:)        = uslp_tm (:,:,:)
            vslp (:,:,:)        = vslp_tm (:,:,:)
         ENDIF
         !
         CALL trc_sub_ssh( kt )         ! after ssh & vertical velocity
         !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_sub_stp')
      !
   END SUBROUTINE trc_sub_stp


   SUBROUTINE trc_sub_ini
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sub_ini  ***
      !!                      
      !! ** Purpose : Initialize variables needed for sub-stepping passive tracers
      !! 
      !! ** Method  : 
      !!              Compute the averages for sub-stepping
      !!-------------------------------------------------------------------
      INTEGER ::   ierr
      !!-------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'trc_sub_ini : initial set up of the passive tracers substepping'
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      ierr =  trc_sub_alloc    ()
      CALL mpp_sum( 'trcsub', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'top_sub_alloc : unable to allocate standard ocean arrays' )

      un_tm   (:,:,:)        = un   (:,:,:)        * e3u_n(:,:,:) 
      vn_tm   (:,:,:)        = vn   (:,:,:)        * e3v_n(:,:,:) 
      tsn_tm  (:,:,:,jp_tem) = tsn  (:,:,:,jp_tem) * e3t_n(:,:,:)  
      tsn_tm  (:,:,:,jp_sal) = tsn  (:,:,:,jp_sal) * e3t_n(:,:,:)  
      rhop_tm (:,:,:)        = rhop (:,:,:)        * e3t_n(:,:,:)  
!!gm : BUG? ==>> for avt & avs I don't understand the division by e3w
      avs_tm  (:,:,:)        = avs  (:,:,:)        * e3w_n(:,:,:)  
      IF( l_ldfslp ) THEN
         wslpi_tm(:,:,:)     = wslpi(:,:,:)
         wslpj_tm(:,:,:)     = wslpj(:,:,:)
         uslp_tm (:,:,:)     = uslp (:,:,:)
         vslp_tm (:,:,:)     = vslp (:,:,:)
      ENDIF
      sshn_tm  (:,:) = sshn  (:,:) 
      rnf_tm   (:,:) = rnf   (:,:) 
      h_rnf_tm (:,:) = h_rnf (:,:) 
      hmld_tm  (:,:) = hmld  (:,:)

      ! Physics variables that are set after initialization:
      fr_i_tm  (:,:) = 0._wp
      emp_tm   (:,:) = 0._wp
      fmmflx_tm(:,:)  = 0._wp
      qsr_tm   (:,:) = 0._wp
      wndm_tm  (:,:) = 0._wp
      IF( ln_trabbl ) THEN
         IF( nn_bbl_ldf == 1 ) THEN
            ahu_bbl_tm(:,:) = 0._wp
            ahv_bbl_tm(:,:) = 0._wp
         ENDIF
         IF( nn_bbl_adv == 1 ) THEN
            utr_bbl_tm(:,:) = 0._wp
            vtr_bbl_tm(:,:) = 0._wp
         ENDIF
      ENDIF
      !
   END SUBROUTINE trc_sub_ini


   SUBROUTINE trc_sub_reset( kt )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sub_reset  ***
      !!                      
      !! ** Purpose : Reset physics variables averaged for substepping
      !! 
      !! ** Method  : 
      !!              Compute the averages for sub-stepping
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) ::  kt  ! ocean time-step index
      INTEGER :: jk                 ! dummy loop indices
      !!-------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sub_reset')
      !
      !   restore physics variables
      un    (:,:,:)   =  un_temp    (:,:,:)
      vn    (:,:,:)   =  vn_temp    (:,:,:)
      wn    (:,:,:)   =  wn_temp    (:,:,:)
      tsn   (:,:,:,:) =  tsn_temp   (:,:,:,:)
      rhop  (:,:,:)   =  rhop_temp  (:,:,:)
      avs   (:,:,:)   =  avs_temp   (:,:,:)
      IF( l_ldfslp ) THEN
         wslpi (:,:,:)=  wslpi_temp (:,:,:)
         wslpj (:,:,:)=  wslpj_temp (:,:,:)
         uslp  (:,:,:)=  uslp_temp  (:,:,:)
         vslp  (:,:,:)=  vslp_temp  (:,:,:)
      ENDIF
      sshn  (:,:)     =  sshn_temp  (:,:)
      sshb  (:,:)     =  sshb_temp  (:,:)
      ssha  (:,:)     =  ssha_temp  (:,:)
      rnf   (:,:)     =  rnf_temp   (:,:)
      h_rnf (:,:)     =  h_rnf_temp (:,:)
      !
      hmld  (:,:)     =  hmld_temp  (:,:)
      fr_i  (:,:)     =  fr_i_temp  (:,:)
      emp   (:,:)     =  emp_temp   (:,:)
      fmmflx(:,:)     =  fmmflx_temp(:,:)
      emp_b (:,:)     =  emp_b_temp (:,:)
      qsr   (:,:)     =  qsr_temp   (:,:)
      wndm  (:,:)     =  wndm_temp  (:,:)
      IF( ln_trabbl ) THEN
         IF( nn_bbl_ldf == 1 ) THEN
            ahu_bbl(:,:) = ahu_bbl_temp(:,:) 
            ahv_bbl(:,:) = ahv_bbl_temp(:,:) 
         ENDIF
         IF( nn_bbl_adv == 1 ) THEN
            utr_bbl(:,:) = utr_bbl_temp(:,:) 
            vtr_bbl(:,:) = vtr_bbl_temp(:,:) 
         ENDIF
      ENDIF
      !
      hdivn (:,:,:)   =  hdivn_temp (:,:,:)
      !                                      
      ! Start new averages
         un_tm   (:,:,:)        = un   (:,:,:)        * e3u_n(:,:,:) 
         vn_tm   (:,:,:)        = vn   (:,:,:)        * e3v_n(:,:,:) 
         tsn_tm  (:,:,:,jp_tem) = tsn  (:,:,:,jp_tem) * e3t_n(:,:,:)  
         tsn_tm  (:,:,:,jp_sal) = tsn  (:,:,:,jp_sal) * e3t_n(:,:,:)  
         rhop_tm (:,:,:)        = rhop (:,:,:)        * e3t_n(:,:,:)  
         avs_tm  (:,:,:)        = avs  (:,:,:)        * e3w_n(:,:,:)  
      IF( l_ldfslp ) THEN
         uslp_tm (:,:,:)        = uslp (:,:,:)
         vslp_tm (:,:,:)        = vslp (:,:,:)
         wslpi_tm(:,:,:)        = wslpi(:,:,:) 
         wslpj_tm(:,:,:)        = wslpj(:,:,:)
      ENDIF
      !
      sshb_hold  (:,:) = sshn  (:,:)
      emp_b_hold (:,:) = emp   (:,:)
      sshn_tm    (:,:) = sshn  (:,:) 
      rnf_tm     (:,:) = rnf   (:,:) 
      h_rnf_tm   (:,:) = h_rnf (:,:) 
      hmld_tm    (:,:) = hmld  (:,:)
      fr_i_tm    (:,:) = fr_i  (:,:)
      emp_tm     (:,:) = emp   (:,:)
      fmmflx_tm  (:,:) = fmmflx(:,:)
      qsr_tm     (:,:) = qsr   (:,:)
      wndm_tm    (:,:) = wndm  (:,:)
      IF( ln_trabbl ) THEN
         IF( nn_bbl_ldf == 1 ) THEN
            ahu_bbl_tm(:,:) = ahu_bbl(:,:) 
            ahv_bbl_tm(:,:) = ahv_bbl(:,:) 
         ENDIF
         IF( nn_bbl_adv == 1 ) THEN
            utr_bbl_tm(:,:) = utr_bbl(:,:) 
            vtr_bbl_tm(:,:) = vtr_bbl(:,:) 
         ENDIF
      ENDIF
      !
      !
      IF( ln_timing )   CALL timing_stop('trc_sub_reset')
      !
   END SUBROUTINE trc_sub_reset


   SUBROUTINE trc_sub_ssh( kt ) 
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE trc_sub_ssh  ***
      !!                   
      !! ** Purpose :   compute the after ssh (ssha), the now vertical velocity
      !!              and update the now vertical coordinate (ln_linssh=F).
      !!
      !! ** Method  : - Using the incompressibility hypothesis, the vertical 
      !!      velocity is computed by integrating the horizontal divergence  
      !!      from the bottom to the surface minus the scale factor evolution.
      !!        The boundary conditions are w=0 at the bottom (no flux) and.
      !!
      !! ** action  :   ssha    : after sea surface height
      !!                wn      : now vertical velocity
      !!                sshu_a, sshv_a, sshf_a  : after sea surface height (ln_linssh=F)
      !!
      !! Reference  : Leclair, M., and G. Madec, 2009, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! time step
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zcoefu, zcoefv, zcoeff, z2dt, z1_2dt, z1_rau0   ! local scalars
      REAL(wp), DIMENSION(jpi,jpj) :: zhdiv
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sub_ssh')
      !

      IF( kt == nittrc000 ) THEN
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_sub_ssh : after sea surface height and now vertical velocity '
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~ '
         !
         wn(:,:,jpk) = 0._wp                  ! bottom boundary condition: w=0 (set once for all)
         !
      ENDIF
      !
!!gm BUG here !   hdivn will include the runoff divergence at the wrong timestep !!!!
      CALL div_hor( kt )                              ! Horizontal divergence & Relative vorticity
      !
      z2dt = 2._wp * rdt                              ! set time step size (Euler/Leapfrog)
      IF( neuler == 0 .AND. kt == nittrc000 )   z2dt = rdt

      !                                           !------------------------------!
      !                                           !   After Sea Surface Height   !
      !                                           !------------------------------!
      zhdiv(:,:) = 0._wp
      DO jk = 1, jpkm1                                 ! Horizontal divergence of barotropic transports
        zhdiv(:,:) = zhdiv(:,:) + e3t_n(:,:,jk) * hdivn(:,:,jk)
      END DO
      !                                                ! Sea surface elevation time stepping
      ! In forward Euler time stepping case, the same formulation as in the leap-frog case can be used
      ! because emp_b field is initialized with the vlaues of emp field. Hence, 0.5 * ( emp + emp_b ) = emp
      z1_rau0 = 0.5 / rau0
      ssha(:,:) = (  sshb(:,:) - z2dt * ( z1_rau0 * ( emp_b(:,:) + emp(:,:) ) + zhdiv(:,:) )  ) * tmask(:,:,1)

      IF( .NOT.ln_dynspg_ts ) THEN
      ! These lines are not necessary with time splitting since
      ! boundary condition on sea level is set during ts loop
#if defined key_agrif
      CALL agrif_ssh( kt )
#endif
         IF( ln_bdy ) THEN
            ssha(:,:) = ssha(:,:) * bdytmask(:,:)
            CALL lbc_lnk( 'trcsub', ssha, 'T', 1. ) 
         ENDIF
      ENDIF
      !
      !                                           !------------------------------!
      !                                           !     Now Vertical Velocity    !
      !                                           !------------------------------!
      z1_2dt = 1.e0 / z2dt
      DO jk = jpkm1, 1, -1                             ! integrate from the bottom the hor. divergence
         ! - ML - need 3 lines here because replacement of e3t by its expression yields too long lines otherwise
         wn(:,:,jk) = wn(:,:,jk+1) -   e3t_n(:,:,jk) * hdivn(:,:,jk)        &
            &                      - ( e3t_a(:,:,jk) - e3t_b(:,:,jk) )    &
            &                         * tmask(:,:,jk) * z1_2dt
         IF( ln_bdy ) wn(:,:,jk) = wn(:,:,jk) * bdytmask(:,:)
      END DO
      !
      IF( ln_timing )   CALL timing_stop('trc_sub_ssh')
      !
   END SUBROUTINE trc_sub_ssh


   INTEGER FUNCTION trc_sub_alloc()
      !!-------------------------------------------------------------------
      !!                    *** ROUTINE trc_sub_alloc ***
      !!-------------------------------------------------------------------
      USE lib_mpp, ONLY: ctl_stop
      INTEGER ::  ierr(3)
      !!-------------------------------------------------------------------
      !
      ierr(:) = 0
      !
      ALLOCATE( un_temp(jpi,jpj,jpk)      ,  vn_temp(jpi,jpj,jpk)   ,     &
         &      wn_temp(jpi,jpj,jpk)      ,                               &
         &      rhop_temp(jpi,jpj,jpk)    ,  rhop_tm(jpi,jpj,jpk)   ,     &
         &      sshn_temp(jpi,jpj)        ,  sshb_temp(jpi,jpj)     ,     &
         &      ssha_temp(jpi,jpj)        ,                               &
         &      rnf_temp(jpi,jpj)         ,  h_rnf_temp(jpi,jpj)    ,     &
         &      tsn_temp(jpi,jpj,jpk,2)   ,  emp_b_temp(jpi,jpj)    ,     &
         &      emp_temp(jpi,jpj)         ,  fmmflx_temp(jpi,jpj)   ,     &
         &      hmld_temp(jpi,jpj)        ,  qsr_temp(jpi,jpj)      ,     &
         &      fr_i_temp(jpi,jpj)        ,  fr_i_tm(jpi,jpj)       ,     &
         &      wndm_temp(jpi,jpj)        ,  wndm_tm(jpi,jpj)       ,     &
         &      avs_tm(jpi,jpj,jpk)       ,  avs_temp(jpi,jpj,jpk)  ,     &
         &      hdivn_temp(jpi,jpj,jpk)   ,  hdivb_temp(jpi,jpj,jpk),     &
         &      un_tm(jpi,jpj,jpk)        ,  vn_tm(jpi,jpj,jpk)     ,     &
         &      sshn_tm(jpi,jpj)          ,  sshb_hold(jpi,jpj)     ,     &
         &      tsn_tm(jpi,jpj,jpk,2)     ,                               &
         &      emp_tm(jpi,jpj)           ,  fmmflx_tm(jpi,jpj)     ,     &
         &      emp_b_hold(jpi,jpj)       ,                               &
         &      hmld_tm(jpi,jpj)          ,  qsr_tm(jpi,jpj)        ,     &
         &      rnf_tm(jpi,jpj)           ,  h_rnf_tm(jpi,jpj)      , STAT=ierr(1) )  
      !
      IF( l_ldfslp ) THEN
         ALLOCATE( uslp_temp(jpi,jpj,jpk) ,  wslpi_temp(jpi,jpj,jpk),     &
            &      vslp_temp(jpi,jpj,jpk) ,  wslpj_temp(jpi,jpj,jpk),     &
            &      uslp_tm  (jpi,jpj,jpk) ,  wslpi_tm  (jpi,jpj,jpk),     &
            &      vslp_tm  (jpi,jpj,jpk) ,  wslpj_tm  (jpi,jpj,jpk), STAT=ierr(2) )
      ENDIF
      IF( ln_trabbl ) THEN
         ALLOCATE( ahu_bbl_temp(jpi,jpj)  , utr_bbl_temp(jpi,jpj)   ,     &
            &      ahv_bbl_temp(jpi,jpj)  , vtr_bbl_temp(jpi,jpj)   ,     &
            &      ahu_bbl_tm  (jpi,jpj)  , utr_bbl_tm  (jpi,jpj)   ,     &
            &      ahv_bbl_tm  (jpi,jpj)  , vtr_bbl_tm  (jpi,jpj)   , STAT=ierr(3) ) 
      ENDIF
      !
      trc_sub_alloc = MAXVAL( ierr )
      !
      IF( trc_sub_alloc /= 0 )   CALL ctl_stop( 'STOP', 'trc_sub_alloc: failed to allocate arrays' )
      !
   END FUNCTION trc_sub_alloc

#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO passive tracers
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_sub_stp( kt )        ! Empty routine
      WRITE(*,*) 'trc_sub_stp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sub_stp
   SUBROUTINE trc_sub_ini        ! Empty routine
      WRITE(*,*) 'trc_sub_ini: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sub_ini
#endif

   !!======================================================================
END MODULE trcsub
