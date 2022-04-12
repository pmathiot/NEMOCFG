MODULE oce_trc
   !!======================================================================
   !!                      ***  MODULE  oce_trc  ***
   !! TOP :   variables shared between ocean and passive tracers
   !!======================================================================
   !! History :   1.0  !  2004-03  (C. Ethe)  original code
   !!             2.0  !  2007-12 (C. Ethe, G. Madec)  rewritting
   !!----------------------------------------------------------------------
   !                                            !* Domain size *
   USE par_oce , ONLY :   jpi      =>   jpi        !: first  dimension of grid --> i 
   USE par_oce , ONLY :   jpj      =>   jpj        !: second dimension of grid --> j  
   USE par_oce , ONLY :   jpk      =>   jpk        !: number of levels  
   USE par_oce , ONLY :   jpim1    =>   jpim1      !: jpi - 1
   USE par_oce , ONLY :   jpjm1    =>   jpjm1      !: jpj - 1 
   USE par_oce , ONLY :   jpkm1    =>   jpkm1      !: jpk - 1  
   USE par_oce , ONLY :   jpij     =>   jpij       !: jpi x jpj
   USE par_oce , ONLY :   jp_tem   =>   jp_tem     !: indice for temperature
   USE par_oce , ONLY :   jp_sal   =>   jp_sal     !: indice for salinity

   USE in_out_manager                           !* IO manager *
   USE timing                                   !* Timing * 
   USE lib_mpp                                  !* MPP library                         
   USE lib_fortran                              !* Fortran utilities                         
   USE lbclnk                                   !* Lateral boundary conditions                         
   USE phycst                                   !* physical constants *
   USE c1d                                      !* 1D configuration

   USE dom_oce                                  !* model domain *

   USE domvvl, ONLY : un_td, vn_td          !: thickness diffusion transport
   USE domvvl, ONLY : ln_vvl_ztilde         !: ztilde vertical coordinate
   USE domvvl, ONLY : ln_vvl_layer          !: level  vertical coordinate

   !* ocean fields: here now and after fields *
   USE oce , ONLY :   un      =>    un      !: i-horizontal velocity (m s-1) 
   USE oce , ONLY :   vn      =>    vn      !: j-horizontal velocity (m s-1)
   USE oce , ONLY :   wn      =>    wn      !: vertical velocity (m s-1)  
   USE oce , ONLY :   tsn     =>    tsn     !: 4D array contaning ( tn, sn )
   USE oce , ONLY :   tsb     =>    tsb     !: 4D array contaning ( tb, sb )
   USE oce , ONLY :   tsa     =>    tsa     !: 4D array contaning ( ta, sa )
   USE oce , ONLY :   rhop    =>    rhop    !: potential volumic mass (kg m-3) 
   USE oce , ONLY :   rhd     =>    rhd     !: in situ density anomalie rhd=(rho-rau0)/rau0 (no units)
   USE oce , ONLY :   hdivn   =>    hdivn   !: horizontal divergence (1/s)
   USE oce , ONLY :   sshn    =>    sshn    !: sea surface height at t-point [m]   
   USE oce , ONLY :   sshb    =>    sshb    !: sea surface height at t-point [m]   
   USE oce , ONLY :   ssha    =>    ssha    !: sea surface height at t-point [m]   
   USE oce , ONLY :   rab_n   =>    rab_n   !: local thermal/haline expension ratio at T-points

   !* surface fluxes *
   USE sbc_oce , ONLY :   utau       =>    utau       !: i-surface stress component
   USE sbc_oce , ONLY :   vtau       =>    vtau       !: j-surface stress component
   USE sbc_oce , ONLY :   wndm       =>    wndm       !: 10m wind speed 
   USE sbc_oce , ONLY :   qsr        =>    qsr        !: penetrative solar radiation (w m-2)
   USE sbc_oce , ONLY :   emp        =>    emp        !: freshwater budget: volume flux               [Kg/m2/s]
   USE sbc_oce , ONLY :   emp_b      =>    emp_b      !: freshwater budget: volume flux               [Kg/m2/s]
   USE sbc_oce , ONLY :   fmmflx     =>    fmmflx     !: freshwater budget: volume flux               [Kg/m2/s]
   USE sbc_oce , ONLY :   rnf        =>    rnf        !: river runoff   [Kg/m2/s]
   USE sbc_oce , ONLY :   rnf_b      =>    rnf_b      !: river runoff at previus step   [Kg/m2/s]
   USE sbc_oce , ONLY :   ln_dm2dc   =>    ln_dm2dc   !: Diurnal Cycle 
   USE sbc_oce , ONLY :   ln_cpl     =>    ln_cpl     !: ocean-atmosphere coupled formulation
   USE sbc_oce , ONLY :   ncpl_qsr_freq   =>   ncpl_qsr_freq   !: qsr coupling frequency per days from atmospher
   USE sbc_oce , ONLY :   ln_rnf     =>    ln_rnf     !: runoffs / runoff mouths
   USE sbc_oce , ONLY :   fr_i       =>    fr_i       !: ice fraction (between 0 to 1)
   USE sbc_oce , ONLY :   atm_co2    =>    atm_co2    !  atmospheric pCO2
   USE traqsr  , ONLY :   rn_abs     =>    rn_abs     !: fraction absorbed in the very near surface
   USE traqsr  , ONLY :   rn_si0     =>    rn_si0     !: very near surface depth of extinction
   USE traqsr  , ONLY :   nksr       =>    nksr       !: levels below which the light cannot penetrate (depth larger than 391 m)
   USE traqsr  , ONLY :   rkrgb      =>    rkrgb      !: tabulated attenuation coefficients for RGB absorption
   USE traqsr  , ONLY :   ln_qsr_bio =>    ln_qsr_bio !: flag to use or not the biological fluxes for light
   USE sbcrnf  , ONLY :   rnfmsk     =>    rnfmsk     !: mixed adv scheme in runoffs vicinity (hori.) 
   USE sbcrnf  , ONLY :   rnfmsk_z   =>    rnfmsk_z   !: mixed adv scheme in runoffs vicinity (vert.)
   USE sbcrnf  , ONLY :   h_rnf      =>    h_rnf      !: river runoff   [Kg/m2/s]
   USE sbcrnf  , ONLY :   nk_rnf     =>    nk_rnf     !: depth of runoff in model level
   USE sbcrnf  , ONLY :   rn_rfact   =>    rn_rfact   !: multiplicative factor for runoff

   USE trc_oce
     
!!gm  this can be removed if :
!!gm    in trcadv.F90 and trcsub.F90  we add a USE ldfslp
   !* direction of lateral diffusion *
   USE ldfslp , ONLY :   l_ldfslp  =>  l_ldfslp       !: slopes flag
   USE ldfslp , ONLY :   uslp       =>   uslp         !: i-slope at u-point
   USE ldfslp , ONLY :   vslp       =>   vslp         !: j-slope at v-point
   USE ldfslp , ONLY :   wslpi      =>   wslpi        !: i-slope at w-point
   USE ldfslp , ONLY :   wslpj      =>   wslpj        !: j-slope at w-point
   USE ldfslp , ONLY :   ln_traldf_triad => ln_traldf_triad   !: use of triad scheme
   USE ldfslp , ONLY :   ln_traldf_iso => ln_traldf_iso   !: use of isopycnal scheme
!!gm end

   !* vertical diffusion *
   USE zdf_oce , ONLY :   avs        =>   avs         !: vert. diffusivity coef. for salinity    (w-point)
   USE zdf_oce , ONLY :   avt        =>   avt         !: vert. diffusivity coef. for temperature (w-point)

   !* mixing & mixed layer depth *
   USE zdfmxl , ONLY :   nmln        =>   nmln        !: number of level in the mixed layer
   USE zdfmxl , ONLY :   hmld        =>   hmld        !: mixing layer depth (turbocline)
   USE zdfmxl , ONLY :   hmlp        =>   hmlp        !: mixed layer depth  (rho=rho0+zdcrit) (m)
   USE zdfmxl , ONLY :   hmlpt       =>   hmlpt       !: mixed layer depth at t-points (m)
   USE zdfmxl , ONLY :   avt_c       =>   avt_c       !: Kz criterion for the turbocline depth

END MODULE oce_trc
