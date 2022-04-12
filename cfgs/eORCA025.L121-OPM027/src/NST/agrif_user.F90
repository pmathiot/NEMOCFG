#undef UPD_HIGH   /* MIX HIGH UPDATE */
#if defined key_agrif
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_user.F90 13479 2020-09-16 16:56:46Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
SUBROUTINE agrif_user
END SUBROUTINE agrif_user

SUBROUTINE agrif_before_regridding
END SUBROUTINE agrif_before_regridding

SUBROUTINE Agrif_InitWorkspace
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitWorkspace ***
      !!----------------------------------------------------------------------
   USE par_oce
   USE dom_oce
   USE nemogcm
   USE mppini
      !!
   IMPLICIT NONE
      !!----------------------------------------------------------------------
   !
   IF( .NOT. Agrif_Root() ) THEN
      ! no more static variables
!!$! JC: change to allow for different vertical levels
!!$!     jpk is already set
!!$!     keep it jpk possibly different from jpkglo which 
!!$!     hold parent grid vertical levels number (set earlier)
!!$!      jpk     = jpkglo 
   ENDIF
   !
END SUBROUTINE Agrif_InitWorkspace


SUBROUTINE Agrif_InitValues
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE Agrif_Util
   USE oce 
   USE dom_oce
   USE nemogcm
   USE tradmp
   USE bdy_oce   , ONLY: ln_bdy
   !!
   IMPLICIT NONE
      !!----------------------------------------------------------------------
   !
   CALL nemo_init       !* Initializations of each fine grid

   !                    !* Agrif initialization
   CALL agrif_nemo_init
   CALL Agrif_InitValues_cont_dom
   CALL Agrif_InitValues_cont
# if defined key_top
   CALL Agrif_InitValues_cont_top
# endif
# if defined key_si3
   CALL Agrif_InitValues_cont_ice
# endif
   !    
END SUBROUTINE Agrif_initvalues


SUBROUTINE Agrif_InitValues_cont_dom
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont ***
      !!
      !! ** Purpose ::   Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE Agrif_Util
   USE oce 
   USE dom_oce
   USE nemogcm
   USE in_out_manager
   USE agrif_oce_update
   USE agrif_oce_interp
   USE agrif_oce_sponge
   !
   IMPLICIT NONE
      !!----------------------------------------------------------------------
   !
   ! Declaration of the type of variable which have to be interpolated
   !
   CALL agrif_declare_var_dom
   !
END SUBROUTINE Agrif_InitValues_cont_dom


SUBROUTINE agrif_declare_var_dom
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE agrif_util
   USE par_oce       
   USE oce
   !
   IMPLICIT NONE
   !
   INTEGER :: ind1, ind2, ind3
      !!----------------------------------------------------------------------

      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
   ind1 =     nbghostcells
   ind2 = 1 + nbghostcells
   ind3 = 2 + nbghostcells
   CALL agrif_declare_variable((/1,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),e1u_id)
   CALL agrif_declare_variable((/2,1/),(/ind3,ind2/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),e2v_id)

      ! 2. Type of interpolation
      !-------------------------
   CALL Agrif_Set_bcinterp( e1u_id, interp1=Agrif_linear, interp2=AGRIF_ppm    )
   CALL Agrif_Set_bcinterp( e2v_id, interp1=AGRIF_ppm   , interp2=Agrif_linear )

      ! 3. Location of interpolation
      !-----------------------------
   CALL Agrif_Set_bc(e1u_id,(/0,ind1-1/))
   CALL Agrif_Set_bc(e2v_id,(/0,ind1-1/))

      ! 4. Update type
      !--------------- 
# if defined UPD_HIGH
   CALL Agrif_Set_Updatetype(e1u_id,update1 = Agrif_Update_Average, update2=Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(e2v_id,update1 = Agrif_Update_Full_Weighting, update2=Agrif_Update_Average)
#else
   CALL Agrif_Set_Updatetype(e1u_id,update1 = Agrif_Update_Copy, update2=Agrif_Update_Average)
   CALL Agrif_Set_Updatetype(e2v_id,update1 = Agrif_Update_Average, update2=Agrif_Update_Copy)
#endif

END SUBROUTINE agrif_declare_var_dom


SUBROUTINE Agrif_InitValues_cont
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont ***
      !!
      !! ** Purpose ::   Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE agrif_oce_update
   USE agrif_oce_interp
   USE agrif_oce_sponge
   USE Agrif_Util
   USE oce 
   USE dom_oce
   USE zdf_oce
   USE nemogcm
   !
   USE lib_mpp
   USE in_out_manager
   !
   IMPLICIT NONE
   !
   LOGICAL :: check_namelist
   CHARACTER(len=15) :: cl_check1, cl_check2, cl_check3, cl_check4 
      !!----------------------------------------------------------------------

   ! 1. Declaration of the type of variable which have to be interpolated
   !---------------------------------------------------------------------
   CALL agrif_declare_var

   ! 2. First interpolations of potentially non zero fields
   !-------------------------------------------------------
   Agrif_SpecialValue    = 0._wp
   Agrif_UseSpecialValue = .TRUE.
   CALL Agrif_Bc_variable(tsn_id,calledweight=1.,procname=interptsn)
   CALL Agrif_Sponge
   tabspongedone_tsn = .FALSE.
   CALL Agrif_Bc_variable(tsn_sponge_id,calledweight=1.,procname=interptsn_sponge)
   ! reset tsa to zero
   tsa(:,:,:,:) = 0.

   Agrif_UseSpecialValue = ln_spc_dyn
   CALL Agrif_Bc_variable(un_interp_id,calledweight=1.,procname=interpun)
   CALL Agrif_Bc_variable(vn_interp_id,calledweight=1.,procname=interpvn)
   tabspongedone_u = .FALSE.
   tabspongedone_v = .FALSE.
   CALL Agrif_Bc_variable(un_sponge_id,calledweight=1.,procname=interpun_sponge)
   tabspongedone_u = .FALSE.
   tabspongedone_v = .FALSE.
   CALL Agrif_Bc_variable(vn_sponge_id,calledweight=1.,procname=interpvn_sponge)

   Agrif_UseSpecialValue = .TRUE.
   CALL Agrif_Bc_variable(sshn_id,calledweight=1., procname=interpsshn )
   hbdy_w(:,:) = 0.e0 ; hbdy_e(:,:) = 0.e0 ; hbdy_n(:,:) = 0.e0 ; hbdy_s(:,:) = 0.e0
   ssha(:,:) = 0.e0

   IF ( ln_dynspg_ts ) THEN
      Agrif_UseSpecialValue = ln_spc_dyn
      CALL Agrif_Bc_variable(unb_id,calledweight=1.,procname=interpunb)
      CALL Agrif_Bc_variable(vnb_id,calledweight=1.,procname=interpvnb)
      CALL Agrif_Bc_variable(ub2b_interp_id,calledweight=1.,procname=interpub2b)
      CALL Agrif_Bc_variable(vb2b_interp_id,calledweight=1.,procname=interpvb2b)
      ubdy_w(:,:) = 0.e0 ; vbdy_w(:,:) = 0.e0
      ubdy_e(:,:) = 0.e0 ; vbdy_e(:,:) = 0.e0
      ubdy_n(:,:) = 0.e0 ; vbdy_n(:,:) = 0.e0
      ubdy_s(:,:) = 0.e0 ; vbdy_s(:,:) = 0.e0
   ENDIF

   Agrif_UseSpecialValue = .FALSE. 
   ! reset velocities to zero
   ua(:,:,:) = 0.
   va(:,:,:) = 0.

   ! 3. Some controls
   !-----------------
   check_namelist = .TRUE.

   IF( check_namelist ) THEN 

      ! Check time steps           
      IF( NINT(Agrif_Rhot()) * NINT(rdt) .NE. Agrif_Parent(rdt) ) THEN
         WRITE(cl_check1,*)  NINT(Agrif_Parent(rdt))
         WRITE(cl_check2,*)  NINT(rdt)
         WRITE(cl_check3,*)  NINT(Agrif_Parent(rdt)/Agrif_Rhot())
         CALL ctl_stop( 'Incompatible time step between ocean grids',   &
               &               'parent grid value : '//cl_check1    ,   & 
               &               'child  grid value : '//cl_check2    ,   & 
               &               'value on child grid should be changed to : '//cl_check3 )
      ENDIF

      ! Check run length
      IF( Agrif_IRhot() * (Agrif_Parent(nitend)- &
            Agrif_Parent(nit000)+1) .NE. (nitend-nit000+1) ) THEN
         WRITE(cl_check1,*)  (Agrif_Parent(nit000)-1)*Agrif_IRhot() + 1
         WRITE(cl_check2,*)   Agrif_Parent(nitend)   *Agrif_IRhot()
         CALL ctl_warn( 'Incompatible run length between grids'                      ,   &
               &               'nit000 on fine grid will be changed to : '//cl_check1,   &
               &               'nitend on fine grid will be changed to : '//cl_check2    )
         nit000 = (Agrif_Parent(nit000)-1)*Agrif_IRhot() + 1
         nitend =  Agrif_Parent(nitend)   *Agrif_IRhot()
      ENDIF

      ! Check free surface scheme
      IF ( ( Agrif_Parent(ln_dynspg_ts ).AND.ln_dynspg_exp ).OR.&
         & ( Agrif_Parent(ln_dynspg_exp).AND.ln_dynspg_ts ) ) THEN
         WRITE(cl_check1,*)  Agrif_Parent( ln_dynspg_ts )
         WRITE(cl_check2,*)  ln_dynspg_ts
         WRITE(cl_check3,*)  Agrif_Parent( ln_dynspg_exp )
         WRITE(cl_check4,*)  ln_dynspg_exp
         CALL ctl_stop( 'Incompatible free surface scheme between grids' ,  &
               &               'parent grid ln_dynspg_ts  :'//cl_check1  ,  & 
               &               'child  grid ln_dynspg_ts  :'//cl_check2  ,  &
               &               'parent grid ln_dynspg_exp :'//cl_check3  ,  &
               &               'child  grid ln_dynspg_exp :'//cl_check4  ,  &
               &               'those logicals should be identical' )                 
         STOP
      ENDIF

      ! Check if identical linear free surface option
      IF ( ( Agrif_Parent(ln_linssh ).AND.(.NOT.ln_linssh )).OR.&
         & ( (.NOT.Agrif_Parent(ln_linssh)).AND.ln_linssh ) ) THEN
         WRITE(cl_check1,*)  Agrif_Parent(ln_linssh )
         WRITE(cl_check2,*)  ln_linssh
         CALL ctl_stop( 'Incompatible linearized fs option between grids',  &
               &               'parent grid ln_linssh  :'//cl_check1     ,  &
               &               'child  grid ln_linssh  :'//cl_check2     ,  &
               &               'those logicals should be identical' )                  
         STOP
      ENDIF

      ! check if masks and bathymetries match
      IF(ln_chk_bathy) THEN
         !
         IF(lwp) WRITE(numout,*) 'AGRIF: Check Bathymetry and masks near bdys. Level: ', Agrif_Level()
         !
         kindic_agr = 0
         ! check if umask agree with parent along western and eastern boundaries:
         CALL Agrif_Bc_variable(umsk_id,calledweight=1.,procname=interpumsk)
         ! check if vmask agree with parent along northern and southern boundaries:
         CALL Agrif_Bc_variable(vmsk_id,calledweight=1.,procname=interpvmsk)
         ! check if tmask and vertical scale factors agree with parent over first two coarse grid points:
         CALL Agrif_Bc_variable(e3t_id,calledweight=1.,procname=interpe3t)
         !
         CALL mpp_sum( 'agrif_user', kindic_agr )
         IF( kindic_agr /= 0 ) THEN
            CALL ctl_stop('Child Bathymetry is not correct near boundaries.')
         ELSE
            IF(lwp) WRITE(numout,*) 'Child Bathymetry is ok near boundaries.'
         END IF
      ENDIF
      !
   ENDIF
   ! 
END SUBROUTINE Agrif_InitValues_cont

SUBROUTINE agrif_declare_var
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declarE_var ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE agrif_util
   USE agrif_oce
   USE par_oce       ! ocean parameters
   USE zdf_oce       ! vertical physics
   USE oce
   !
   IMPLICIT NONE
   !
   INTEGER :: ind1, ind2, ind3
      !!----------------------------------------------------------------------

   ! 1. Declaration of the type of variable which have to be interpolated
   !---------------------------------------------------------------------
   ind1 =     nbghostcells
   ind2 = 1 + nbghostcells
   ind3 = 2 + nbghostcells
# if defined key_vertical
   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jpts+1/),tsn_id)
   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jpts+1/),tsn_sponge_id)

   CALL agrif_declare_variable((/1,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),un_interp_id)
   CALL agrif_declare_variable((/2,1,0,0/),(/ind3,ind2,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),vn_interp_id)
   CALL agrif_declare_variable((/1,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),un_update_id)
   CALL agrif_declare_variable((/2,1,0,0/),(/ind3,ind2,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),vn_update_id)
   CALL agrif_declare_variable((/1,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),un_sponge_id)
   CALL agrif_declare_variable((/2,1,0,0/),(/ind3,ind2,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),vn_sponge_id)
# else
   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jpts/),tsn_id)
   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jpts/),tsn_sponge_id)

   CALL agrif_declare_variable((/1,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),un_interp_id)
   CALL agrif_declare_variable((/2,1,0,0/),(/ind3,ind2,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),vn_interp_id)
   CALL agrif_declare_variable((/1,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),un_update_id)
   CALL agrif_declare_variable((/2,1,0,0/),(/ind3,ind2,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),vn_update_id)
   CALL agrif_declare_variable((/1,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),un_sponge_id)
   CALL agrif_declare_variable((/2,1,0,0/),(/ind3,ind2,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),vn_sponge_id)
# endif

   CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nlci,nlcj,jpk/),e3t_id)
   CALL agrif_declare_variable((/1,2,0/),(/ind2,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nlci,nlcj,jpk/),umsk_id)
   CALL agrif_declare_variable((/2,1,0/),(/ind3,ind2,0/),(/'x','y','N'/),(/1,1,1/),(/nlci,nlcj,jpk/),vmsk_id)

   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,3/),scales_t_id)

   CALL agrif_declare_variable((/1,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),unb_id)
   CALL agrif_declare_variable((/2,1/),(/ind3,ind2/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),vnb_id)
   CALL agrif_declare_variable((/1,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),ub2b_interp_id)
   CALL agrif_declare_variable((/2,1/),(/ind3,ind2/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),vb2b_interp_id)
   CALL agrif_declare_variable((/1,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),ub2b_update_id)
   CALL agrif_declare_variable((/2,1/),(/ind3,ind2/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),vb2b_update_id)

   CALL agrif_declare_variable((/2,2/),(/ind3,ind3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),sshn_id)

   IF( ln_zdftke.OR.ln_zdfgls ) THEN
!      CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nlci,nlcj,jpk/), en_id)
!      CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nlci,nlcj,jpk/),avt_id)
# if defined key_vertical
      CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,2/),avm_id)
# else
      CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,1/),avm_id)
# endif
   ENDIF

   ! 2. Type of interpolation
   !-------------------------
   CALL Agrif_Set_bcinterp(tsn_id,interp=AGRIF_linear)

   CALL Agrif_Set_bcinterp(un_interp_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
   CALL Agrif_Set_bcinterp(vn_interp_id,interp1=AGRIF_ppm,interp2=Agrif_linear)

   CALL Agrif_Set_bcinterp(tsn_sponge_id,interp=AGRIF_linear)

   CALL Agrif_Set_bcinterp(sshn_id,interp=AGRIF_linear)
   CALL Agrif_Set_bcinterp(unb_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
   CALL Agrif_Set_bcinterp(vnb_id,interp1=AGRIF_ppm,interp2=Agrif_linear)
   CALL Agrif_Set_bcinterp(ub2b_interp_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
   CALL Agrif_Set_bcinterp(vb2b_interp_id,interp1=AGRIF_ppm,interp2=Agrif_linear)


   CALL Agrif_Set_bcinterp(un_sponge_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
   CALL Agrif_Set_bcinterp(vn_sponge_id,interp1=AGRIF_ppm,interp2=Agrif_linear)

   CALL Agrif_Set_bcinterp(e3t_id,interp=AGRIF_constant)
   CALL Agrif_Set_bcinterp(umsk_id,interp=AGRIF_constant)
   CALL Agrif_Set_bcinterp(vmsk_id,interp=AGRIF_constant)

   IF( ln_zdftke.OR.ln_zdfgls )   CALL Agrif_Set_bcinterp( avm_id, interp=AGRIF_linear )

   ! 3. Location of interpolation
   !-----------------------------
   CALL Agrif_Set_bc(       tsn_id, (/0,ind1/) )
   CALL Agrif_Set_bc( un_interp_id, (/0,ind1/) )
   CALL Agrif_Set_bc( vn_interp_id, (/0,ind1/) )

   CALL Agrif_Set_bc( tsn_sponge_id, (/-nn_sponge_len*Agrif_irhox()-1,0/) )  ! if west and rhox=3 and sponge=2 and ghost=1: columns 2 to 9 
   CALL Agrif_Set_bc(  un_sponge_id, (/-nn_sponge_len*Agrif_irhox()-1,0/) )
   CALL Agrif_Set_bc(  vn_sponge_id, (/-nn_sponge_len*Agrif_irhox()-1,0/) )

   CALL Agrif_Set_bc(        sshn_id, (/0,ind1-1/) )
   CALL Agrif_Set_bc(         unb_id, (/0,ind1-1/) )
   CALL Agrif_Set_bc(         vnb_id, (/0,ind1-1/) )
   CALL Agrif_Set_bc( ub2b_interp_id, (/0,ind1-1/) )
   CALL Agrif_Set_bc( vb2b_interp_id, (/0,ind1-1/) )

   CALL Agrif_Set_bc(  e3t_id, (/-nn_sponge_len*Agrif_irhox(),ind1-1/) )   ! if west and rhox=3 and ghost=1: column 2 to 6 
   CALL Agrif_Set_bc( umsk_id, (/0,0/) )
   CALL Agrif_Set_bc( vmsk_id, (/0,0/) )


   IF( ln_zdftke.OR.ln_zdfgls )   CALL Agrif_Set_bc( avm_id, (/0,ind1/) )

   ! 4. Update type
   !--------------- 
   CALL Agrif_Set_Updatetype(scales_t_id, update = AGRIF_Update_Average)

# if defined UPD_HIGH
   CALL Agrif_Set_Updatetype(tsn_id, update = Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(un_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(vn_update_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average)

   CALL Agrif_Set_Updatetype(ub2b_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(vb2b_update_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average)
   CALL Agrif_Set_Updatetype(sshn_id, update = Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(e3t_id, update = Agrif_Update_Full_Weighting)

   IF( ln_zdftke.OR.ln_zdfgls ) THEN
!      CALL Agrif_Set_Updatetype( en_id, update = AGRIF_Update_Full_Weighting)
!      CALL Agrif_Set_Updatetype(avt_id, update = AGRIF_Update_Full_Weighting)
!      CALL Agrif_Set_Updatetype(avm_id, update = AGRIF_Update_Full_Weighting)
   ENDIF

#else
   CALL Agrif_Set_Updatetype(tsn_id, update = AGRIF_Update_Average)
   CALL Agrif_Set_Updatetype(un_update_id,update1 = Agrif_Update_Copy, update2 = Agrif_Update_Average)
   CALL Agrif_Set_Updatetype(vn_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy)

   CALL Agrif_Set_Updatetype(ub2b_update_id,update1 = Agrif_Update_Copy, update2 = Agrif_Update_Average)
   CALL Agrif_Set_Updatetype(vb2b_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy)
   CALL Agrif_Set_Updatetype(sshn_id, update = AGRIF_Update_Average)
   CALL Agrif_Set_Updatetype(e3t_id, update = AGRIF_Update_Average)

   IF( ln_zdftke.OR.ln_zdfgls ) THEN
!      CALL Agrif_Set_Updatetype( en_id, update = AGRIF_Update_Average)
!      CALL Agrif_Set_Updatetype(avt_id, update = AGRIF_Update_Average)
!      CALL Agrif_Set_Updatetype(avm_id, update = AGRIF_Update_Average)
   ENDIF

#endif
   !
END SUBROUTINE agrif_declare_var

#if defined key_si3
SUBROUTINE Agrif_InitValues_cont_ice
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont_ice ***
      !!
      !! ** Purpose :: Initialisation of variables to be interpolated for ice
      !!----------------------------------------------------------------------
   USE Agrif_Util
   USE sbc_oce, ONLY : nn_fsbc  ! clem: necessary otherwise Agrif_Parent(nn_fsbc) = nn_fsbc
   USE ice
   USE agrif_ice
   USE in_out_manager
   USE agrif_ice_interp
   USE lib_mpp
   !
   IMPLICIT NONE
      !!----------------------------------------------------------------------
   !
   ! Declaration of the type of variable which have to be interpolated (parent=>child)
   !----------------------------------------------------------------------------------
   CALL agrif_declare_var_ice

   ! Controls

   ! clem: For some reason, nn_fsbc(child)/=1 does not work properly (signal can be largely degraded by the agrif zoom)
   !          the run must satisfy CFL=Uice/(dx/dt) < 0.6/nn_fsbc(child)
   !          therefore, if nn_fsbc(child)>1 one must reduce the time-step in proportion to nn_fsbc(child), which is not acceptable
   !       If a solution is found, the following stop could be removed because the rest of the code take nn_fsbc(child) into account
   IF( nn_fsbc > 1 )  CALL ctl_stop('nn_fsbc(child) must be set to 1 otherwise agrif and sea-ice may not work properly')

   ! stop if rhot * nn_fsbc(parent) /= N * nn_fsbc(child) with N being integer
   IF( MOD( Agrif_irhot() * Agrif_Parent(nn_fsbc), nn_fsbc ) /= 0 )  THEN
      CALL ctl_stop('rhot * nn_fsbc(parent) /= N * nn_fsbc(child), therefore nn_fsbc(child) should be set to 1 or nn_fsbc(parent)')
   ENDIF
   ! First Interpolations (using "after" ice subtime step => nbstep_ice=1)
   !----------------------------------------------------------------------
   nbstep_ice = ( Agrif_irhot() * Agrif_Parent(nn_fsbc) / nn_fsbc ) ! clem: to have calledweight=1 in interp (otherwise the western border of the zoom is wrong)
   CALL agrif_interp_ice('U') ! interpolation of ice velocities
   CALL agrif_interp_ice('V') ! interpolation of ice velocities
   CALL agrif_interp_ice('T') ! interpolation of ice tracers 
   nbstep_ice = 0
   
   !
END SUBROUTINE Agrif_InitValues_cont_ice

SUBROUTINE agrif_declare_var_ice
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var_ice ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated for ice
      !!----------------------------------------------------------------------
   USE Agrif_Util
   USE ice
   USE par_oce, ONLY : nbghostcells
   !
   IMPLICIT NONE
   !
   INTEGER :: ind1, ind2, ind3
      !!----------------------------------------------------------------------
   !
   ! 1. Declaration of the type of variable which have to be interpolated (parent=>child)
   !       agrif_declare_variable(position,1st point index,--,--,dimensions,name)
   !           ex.:  position=> 1,1 = not-centered (in i and j)
   !                            2,2 =     centered (    -     )
   !                 index   => 1,1 = one ghost line
   !                            2,2 = two ghost lines
   !-------------------------------------------------------------------------------------
   ind1 =     nbghostcells
   ind2 = 1 + nbghostcells
   ind3 = 2 + nbghostcells
   CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nlci,nlcj,jpl*(9+nlay_s+nlay_i)/),tra_ice_id)
   CALL agrif_declare_variable((/1,2/)  ,(/ind2,ind3/)  ,(/'x','y'/)    ,(/1,1/)  ,(/nlci,nlcj/)                      ,u_ice_id  )
   CALL agrif_declare_variable((/2,1/)  ,(/ind3,ind2/)  ,(/'x','y'/)    ,(/1,1/)  ,(/nlci,nlcj/)                      ,v_ice_id  )

   ! 2. Set interpolations (normal & tangent to the grid cell for velocities)
   !-----------------------------------
   CALL Agrif_Set_bcinterp(tra_ice_id, interp  = AGRIF_linear)
   CALL Agrif_Set_bcinterp(u_ice_id  , interp1 = Agrif_linear,interp2 = AGRIF_ppm   )
   CALL Agrif_Set_bcinterp(v_ice_id  , interp1 = AGRIF_ppm   ,interp2 = Agrif_linear)

   ! 3. Set location of interpolations
   !----------------------------------
   CALL Agrif_Set_bc(tra_ice_id,(/0,ind1/))
   CALL Agrif_Set_bc(u_ice_id  ,(/0,ind1/))
   CALL Agrif_Set_bc(v_ice_id  ,(/0,ind1/))

   ! 4. Set update type in case 2 ways (child=>parent) (normal & tangent to the grid cell for velocities)
   !--------------------------------------------------
# if defined UPD_HIGH
   CALL Agrif_Set_Updatetype(tra_ice_id, update  = Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(u_ice_id  , update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting)
   CALL Agrif_Set_Updatetype(v_ice_id  , update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )
#else
   CALL Agrif_Set_Updatetype(tra_ice_id, update  = AGRIF_Update_Average)
   CALL Agrif_Set_Updatetype(u_ice_id  , update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
   CALL Agrif_Set_Updatetype(v_ice_id  , update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )
#endif

END SUBROUTINE agrif_declare_var_ice
#endif


# if defined key_top
SUBROUTINE Agrif_InitValues_cont_top
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont_top ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE Agrif_Util
   USE oce 
   USE dom_oce
   USE nemogcm
   USE par_trc
   USE lib_mpp
   USE trc
   USE in_out_manager
   USE agrif_oce_sponge
   USE agrif_top_update
   USE agrif_top_interp
   USE agrif_top_sponge
   !!
   IMPLICIT NONE
   !
   CHARACTER(len=10) :: cl_check1, cl_check2, cl_check3
   LOGICAL :: check_namelist
      !!----------------------------------------------------------------------


   ! 1. Declaration of the type of variable which have to be interpolated
   !---------------------------------------------------------------------
   CALL agrif_declare_var_top

   ! 2. First interpolations of potentially non zero fields
   !-------------------------------------------------------
   Agrif_SpecialValue=0.
   Agrif_UseSpecialValue = .TRUE.
   CALL Agrif_Bc_variable(trn_id,calledweight=1.,procname=interptrn)
   Agrif_UseSpecialValue = .FALSE.
   CALL Agrif_Sponge
   tabspongedone_trn = .FALSE.
   CALL Agrif_Bc_variable(trn_sponge_id,calledweight=1.,procname=interptrn_sponge)
   ! reset tsa to zero
   tra(:,:,:,:) = 0.


   ! 3. Some controls
   !-----------------
   check_namelist = .TRUE.

   IF( check_namelist ) THEN
      ! Check time steps
      IF( NINT(Agrif_Rhot()) * NINT(rdt) .NE. Agrif_Parent(rdt) ) THEN
         WRITE(cl_check1,*)  Agrif_Parent(rdt)
         WRITE(cl_check2,*)  rdt
         WRITE(cl_check3,*)  rdt*Agrif_Rhot()
         CALL ctl_stop( 'incompatible time step between grids',   &
               &               'parent grid value : '//cl_check1    ,   & 
               &               'child  grid value : '//cl_check2    ,   & 
               &               'value on child grid should be changed to  &
               &               :'//cl_check3  )
      ENDIF

      ! Check run length
      IF( Agrif_IRhot() * (Agrif_Parent(nitend)- &
            Agrif_Parent(nit000)+1) .NE. (nitend-nit000+1) ) THEN
         WRITE(cl_check1,*)  (Agrif_Parent(nit000)-1)*Agrif_IRhot() + 1
         WRITE(cl_check2,*)   Agrif_Parent(nitend)   *Agrif_IRhot()
         CALL ctl_warn( 'incompatible run length between grids'               ,   &
               &              ' nit000 on fine grid will be change to : '//cl_check1,   &
               &              ' nitend on fine grid will be change to : '//cl_check2    )
         nit000 = (Agrif_Parent(nit000)-1)*Agrif_IRhot() + 1
         nitend =  Agrif_Parent(nitend)   *Agrif_IRhot()
      ENDIF

      ! Check passive tracer cell
      IF( nn_dttrc .NE. 1 ) THEN
         WRITE(*,*) 'nn_dttrc should be equal to 1'
      ENDIF
   ENDIF
   !
END SUBROUTINE Agrif_InitValues_cont_top


SUBROUTINE agrif_declare_var_top
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var_top ***
      !!
      !! ** Purpose :: Declaration of TOP variables to be interpolated
      !!----------------------------------------------------------------------
   USE agrif_util
   USE agrif_oce
   USE dom_oce
   USE trc
   !!
   IMPLICIT NONE
   !
   INTEGER :: ind1, ind2, ind3
      !!----------------------------------------------------------------------

   ! 1. Declaration of the type of variable which have to be interpolated
   !---------------------------------------------------------------------
   ind1 =     nbghostcells
   ind2 = 1 + nbghostcells
   ind3 = 2 + nbghostcells
# if defined key_vertical
   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jptra+1/),trn_id)
   CALL agrif_declare_variable((/2,2,0,0/),(/ind3,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jptra+1/),trn_sponge_id)
# else
   CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jptra/),trn_id)
   CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/nlci,nlcj,jpk,jptra/),trn_sponge_id)
# endif

   ! 2. Type of interpolation
   !-------------------------
   CALL Agrif_Set_bcinterp(trn_id,interp=AGRIF_linear)
   CALL Agrif_Set_bcinterp(trn_sponge_id,interp=AGRIF_linear)

   ! 3. Location of interpolation
   !-----------------------------
   CALL Agrif_Set_bc(trn_id,(/0,ind1/))
   CALL Agrif_Set_bc(trn_sponge_id,(/-nn_sponge_len*Agrif_irhox()-1,0/))

   ! 4. Update type
   !--------------- 
# if defined UPD_HIGH
   CALL Agrif_Set_Updatetype(trn_id, update = Agrif_Update_Full_Weighting)
#else
   CALL Agrif_Set_Updatetype(trn_id, update = AGRIF_Update_Average)
#endif
   !
END SUBROUTINE agrif_declare_var_top
# endif

SUBROUTINE Agrif_detect( kg, ksizex )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
   INTEGER, DIMENSION(2) :: ksizex
   INTEGER, DIMENSION(ksizex(1),ksizex(2)) :: kg 
      !!----------------------------------------------------------------------
   !
   RETURN
   !
END SUBROUTINE Agrif_detect


SUBROUTINE agrif_nemo_init
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE agrif_init ***
      !!----------------------------------------------------------------------
   USE agrif_oce 
   USE agrif_ice
   USE in_out_manager
   USE lib_mpp
   !!
   IMPLICIT NONE
   !
   INTEGER  ::   ios                 ! Local integer output status for namelist read
   INTEGER  ::   iminspon
   NAMELIST/namagrif/ rn_sponge_tra, rn_sponge_dyn, ln_spc_dyn, ln_chk_bathy
      !!--------------------------------------------------------------------------------------
   !
   REWIND( numnam_ref )              ! Namelist namagrif in reference namelist : AGRIF zoom
   READ  ( numnam_ref, namagrif, IOSTAT = ios, ERR = 901)
901 IF( ios /= 0 )   CALL ctl_nam ( ios , 'namagrif in reference namelist' )
   REWIND( numnam_cfg )              ! Namelist namagrif in configuration namelist : AGRIF zoom
   READ  ( numnam_cfg, namagrif, IOSTAT = ios, ERR = 902 )
902 IF( ios >  0 )   CALL ctl_nam ( ios , 'namagrif in configuration namelist' )
   IF(lwm) WRITE ( numond, namagrif )
   !
   IF(lwp) THEN                    ! control print
      WRITE(numout,*)
      WRITE(numout,*) 'agrif_nemo_init : AGRIF parameters'
      WRITE(numout,*) '~~~~~~~~~~~~~~~'
      WRITE(numout,*) '   Namelist namagrif : set AGRIF parameters'
      WRITE(numout,*) '      sponge coefficient for tracers    rn_sponge_tra = ', rn_sponge_tra, ' s'
      WRITE(numout,*) '      sponge coefficient for dynamics   rn_sponge_tra = ', rn_sponge_dyn, ' s'
      WRITE(numout,*) '      use special values for dynamics   ln_spc_dyn    = ', ln_spc_dyn
      WRITE(numout,*) '      check bathymetry                  ln_chk_bathy  = ', ln_chk_bathy
   ENDIF
   !
   ! convert DOCTOR namelist name into OLD names
   visc_tra      = rn_sponge_tra
   visc_dyn      = rn_sponge_dyn
   !
   ! Check sponge length:
   IF(     MIN(jpi   ,jpj   ) <=     1 + nbghostcells + (nn_sponge_len * Agrif_irhox() + 1)     &
      .OR. MIN(jpiglo,jpjglo) <= 2* (1 + nbghostcells + (nn_sponge_len * Agrif_irhox() + 1) ) ) &
      &     CALL ctl_stop('STOP','agrif sponge length is too large')
   !
   IF( agrif_oce_alloc()  > 0 )   CALL ctl_warn('agrif agrif_oce_alloc: allocation of arrays failed')
   !
END SUBROUTINE agrif_nemo_init

# if defined key_mpp_mpi

SUBROUTINE Agrif_InvLoc( indloc, nprocloc, i, indglob )
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE Agrif_InvLoc ***
      !!----------------------------------------------------------------------
   USE dom_oce
   !!
   IMPLICIT NONE
   !
   INTEGER :: indglob, indloc, nprocloc, i
      !!----------------------------------------------------------------------
   !
   SELECT CASE( i )
   CASE(1)   ;   indglob = indloc + nimppt(nprocloc+1) - 1
   CASE(2)   ;   indglob = indloc + njmppt(nprocloc+1) - 1
   CASE DEFAULT
      indglob = indloc
   END SELECT
   !
END SUBROUTINE Agrif_InvLoc


SUBROUTINE Agrif_get_proc_info( imin, imax, jmin, jmax )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_get_proc_info ***
      !!----------------------------------------------------------------------
   USE par_oce
   !!
   IMPLICIT NONE
   !
   INTEGER, INTENT(out) :: imin, imax
   INTEGER, INTENT(out) :: jmin, jmax
      !!----------------------------------------------------------------------
   !
   imin = nimppt(Agrif_Procrank+1)  ! ?????
   jmin = njmppt(Agrif_Procrank+1)  ! ?????
   imax = imin + jpi - 1
   jmax = jmin + jpj - 1
   ! 
END SUBROUTINE Agrif_get_proc_info


SUBROUTINE Agrif_estimate_parallel_cost(imin, imax,jmin, jmax, nbprocs, grid_cost)
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_estimate_parallel_cost ***
      !!----------------------------------------------------------------------
   USE par_oce
   !!
   IMPLICIT NONE
   !
   INTEGER,  INTENT(in)  :: imin, imax
   INTEGER,  INTENT(in)  :: jmin, jmax
   INTEGER,  INTENT(in)  :: nbprocs
   REAL(wp), INTENT(out) :: grid_cost
      !!----------------------------------------------------------------------
   !
   grid_cost = REAL(imax-imin+1,wp)*REAL(jmax-jmin+1,wp) / REAL(nbprocs,wp)
   !
END SUBROUTINE Agrif_estimate_parallel_cost

# endif

#else
SUBROUTINE Subcalledbyagrif
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Subcalledbyagrif ***
      !!----------------------------------------------------------------------
   WRITE(*,*) 'Impossible to be here'
END SUBROUTINE Subcalledbyagrif
#endif
