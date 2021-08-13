MODULE seddta
   !!======================================================================
   !!                     ***  MODULE  seddta  ***
   !! Sediment data  :  read sediment input data from a file
   !!=====================================================================

   !! * Modules used
   USE sed
   USE sedarr
   USE phycst, ONLY : rday
   USE iom
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_dta   ! 

   !! *  Module variables
   REAL(wp) ::  rsecday  ! number of second per a day
   REAL(wp) ::  conv2    ! [kg/m2/month]-->[g/cm2/s] ( 1 month has 30 days )

   !! $Id: seddta.F90 10362 2018-11-30 15:38:17Z aumont $
CONTAINS

   !!---------------------------------------------------------------------------
   !!   sed_dta  : read the NetCDF data file in online version using module iom
   !!---------------------------------------------------------------------------

   SUBROUTINE sed_dta( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dta  ***
      !!                    
      !! ** Purpose :   Reads data from a netcdf file and 
      !!                initialization of rain and pore water (k=1) components
      !! 
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-04  (C. Ethe)  Re-organization ; Use of iom
      !!----------------------------------------------------------------------

      !! Arguments
      INTEGER, INTENT(in) ::  kt    ! time-step

      !! * Local declarations
      INTEGER  ::  ji, jj, js, jw, ikt

      REAL(wp), DIMENSION(jpoce) :: zdtap, zdtag
      REAL(wp), DIMENSION(jpi,jpj) :: zwsbio4, zwsbio3
      REAL(wp) :: zf0, zf1, zf2, zkapp, zratio, zdep

      !----------------------------------------------------------------------

      ! Initialization of sediment variable 
      ! Spatial dimension is merged, and unity converted if needed
      !-------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dta')

      IF (lwp) THEN
         WRITE(numsed,*)
         WRITE(numsed,*) ' sed_dta : Bottom layer fields'
         WRITE(numsed,*) ' ~~~~~~'
         WRITE(numsed,*) ' Data from SMS model'
         WRITE(numsed,*)
      ENDIF


      ! open file
      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_dta : Sediment fields'
         dtsed = r2dttrc
         rsecday = 60.* 60. * 24.
!         conv2   = 1.0e+3 / ( 1.0e+4 * rsecday * 30. )
         conv2 = 1.0e+3 /  1.0e+4 
         rdtsed(2:jpksed) = dtsed / ( denssol * por1(2:jpksed) )
      ENDIF

      ! Initialization of temporaries arrays  
      zdtap(:)    = 0. 
      zdtag(:)    = 0.  

      ! reading variables
      IF (lwp) WRITE(numsed,*)
      IF (lwp) WRITE(numsed,*) ' sed_dta : Bottom layer fields at time  kt = ', kt
      ! reading variables
      !
      !    Sinking speeds of detritus is increased with depth as shown
      !    by data and from the coagulation theory
      !    -----------------------------------------------------------
      IF (ln_sediment_offline) THEN
         DO jj = 1, jpj
            DO ji = 1, jpi
               ikt = mbkt(ji,jj)
               zwsbio4(ji,jj) = wsbio2 / rday
               zwsbio3(ji,jj) = wsbio  / rday
            END DO
         END DO
      ELSE
         DO jj = 1, jpj
            DO ji = 1, jpi
               ikt = mbkt(ji,jj)
               zdep = e3t_n(ji,jj,ikt) / r2dttrc
               zwsbio4(ji,jj) = MIN( 0.99 * zdep, wsbio4(ji,jj,ikt) / rday )
               zwsbio3(ji,jj) = MIN( 0.99 * zdep, wsbio3(ji,jj,ikt) / rday )
            END DO
         END DO
      ENDIF

      trc_data(:,:,:) = 0.
      DO jj = 1,jpj
         DO ji = 1, jpi
            ikt = mbkt(ji,jj)
            IF ( tmask(ji,jj,ikt) == 1 ) THEN
               trc_data(ji,jj,1)   = trb(ji,jj,ikt,jpsil)
               trc_data(ji,jj,2)   = trb(ji,jj,ikt,jpoxy)
               trc_data(ji,jj,3)   = trb(ji,jj,ikt,jpdic)
               trc_data(ji,jj,4)   = trb(ji,jj,ikt,jpno3) / 7.625
               trc_data(ji,jj,5)   = trb(ji,jj,ikt,jppo4) / 122.
               trc_data(ji,jj,6)   = trb(ji,jj,ikt,jptal)
               trc_data(ji,jj,7)   = trb(ji,jj,ikt,jpnh4) / 7.625
               trc_data(ji,jj,8)   = 0.0
               trc_data(ji,jj,9)   = 28.0E-3
               trc_data(ji,jj,10)  = trb(ji,jj,ikt,jpfer)
               trc_data(ji,jj,11 ) = MIN(trb(ji,jj,ikt,jpgsi), 1E-4) * zwsbio4(ji,jj) * 1E3
               trc_data(ji,jj,12 ) = MIN(trb(ji,jj,ikt,jppoc), 1E-4) * zwsbio3(ji,jj) * 1E3
               trc_data(ji,jj,13 ) = MIN(trb(ji,jj,ikt,jpgoc), 1E-4) * zwsbio4(ji,jj) * 1E3
               trc_data(ji,jj,14)  = MIN(trb(ji,jj,ikt,jpcal), 1E-4) * zwsbio4(ji,jj) * 1E3
               trc_data(ji,jj,15)  = tsn(ji,jj,ikt,jp_tem)
               trc_data(ji,jj,16)  = tsn(ji,jj,ikt,jp_sal)
               trc_data(ji,jj,17 ) = ( trb(ji,jj,ikt,jpsfe) * zwsbio3(ji,jj) + trb(ji,jj,ikt,jpbfe)  &
               &                     * zwsbio4(ji,jj)  ) * 1E3 / ( trc_data(ji,jj,12 ) + trc_data(ji,jj,13 ) + rtrn )
               trc_data(ji,jj,17 ) = MIN(1E-3, trc_data(ji,jj,17 ) )
            ENDIF
         ENDDO
      ENDDO

      ! Pore water initial concentration [mol/l] in  k=1
      !-------------------------------------------------
      DO jw = 1, jpwat
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jw), trc_data(1:jpi,1:jpj,jw), iarroce(1:jpoce) )
      END DO
      !  Solid components : 
      !-----------------------
      !  Sinking fluxes for OPAL in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      CALL pack_arr ( jpoce, rainrm_dta(1:jpoce,jsopal), trc_data(1:jpi,1:jpj,11), iarroce(1:jpoce) ) 
      rainrm_dta(1:jpoce,jsopal) = rainrm_dta(1:jpoce,jsopal) * 1e-4
      !  Sinking fluxes for POC in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      CALL pack_arr ( jpoce, zdtap(1:jpoce), trc_data(1:jpi,1:jpj,12) , iarroce(1:jpoce) )      
      CALL pack_arr ( jpoce, zdtag(1:jpoce), trc_data(1:jpi,1:jpj,13) , iarroce(1:jpoce) )
      DO ji = 1, jpoce
!        zkapp  = MIN( (1.0 - 0.02 ) * reac_poc, 3731.0 * max(100.0, zkbot(ji) )**(-1.011) / ( 365.0 * 24.0 * 3600.0 ) )
!        zkapp   = MIN( 0.98 * reac_poc, 100.0 * max(100.0, zkbot(ji) )**(-0.6) / ( 365.0 * 24.0 * 3600.0 ) )
!        zratio = ( ( 1.0 - 0.02 ) * reac_poc + 0.02 * reac_poc * 0. - zkapp) / ( ( 0.02 - 1.0 ) * reac_poc / 100. - 0.02 * reac_poc * 0. + zkapp )
!        zf1    = ( 0.02 * (reac_poc - reac_poc * 0.) + zkapp - reac_poc ) / ( reac_poc / 100. - reac_poc )
!        zf1    = MIN(0.98, MAX(0., zf1 ) )
         zf1    = 0.48
         zf0    = 1.0 - 0.02 - zf1
         zf2    = 0.02
         rainrm_dta(ji,jspoc) =   ( zdtap(ji) +  zdtag(ji) ) * 1e-4 * zf0
         rainrm_dta(ji,jspos) =   ( zdtap(ji) +  zdtag(ji) ) * 1e-4 * zf1
         rainrm_dta(ji,jspor) =   ( zdtap(ji) +  zdtag(ji) ) * 1e-4 * zf2
      END DO
      !  Sinking fluxes for Calcite in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      CALL pack_arr ( jpoce,  rainrm_dta(1:jpoce,jscal), trc_data(1:jpi,1:jpj,14), iarroce(1:jpoce) )
      rainrm_dta(1:jpoce,jscal) = rainrm_dta(1:jpoce,jscal) * 1e-4
      ! vector temperature [°C] and salinity 
      CALL pack_arr ( jpoce,  temp(1:jpoce), trc_data(1:jpi,1:jpj,15), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce,  salt(1:jpoce), trc_data(1:jpi,1:jpj,16), iarroce(1:jpoce) )
      
      ! Clay rain rate in [mol/(cm**2.s)] 
      ! inputs data in [kg.m-2.sec-1] ---> 1e+3/(1e+4) [g.cm-2.s-1]   
      ! divided after by molecular weight g.mol-1      
      CALL pack_arr ( jpoce,  rainrm_dta(1:jpoce,jsclay), dust(1:jpi,1:jpj), iarroce(1:jpoce) )
      rainrm_dta(1:jpoce,jsclay) = rainrm_dta(1:jpoce,jsclay) * conv2 / mol_wgt(jsclay)   &
      &                            + wacc(1:jpoce) * por1(2) * denssol / mol_wgt(jsclay) / ( rsecday * 365.0 )
      rainrm_dta(1:jpoce,jsclay) = rainrm_dta(1:jpoce,jsclay) * 0.965
      rainrm_dta(1:jpoce,jsfeo)  = rainrm_dta(1:jpoce,jsclay) * mol_wgt(jsclay) / mol_wgt(jsfeo) * 0.035 / 0.965
!    rainrm_dta(1:jpoce,jsclay) = 1.0E-4 * conv2 / mol_wgt(jsclay)

      ! Iron monosulphide rain rates. Set to 0
      rainrm_dta(1:jpoce,jsfes)  = 0. 

      ! Fe/C ratio in sinking particles that fall to the sediments
      CALL pack_arr ( jpoce,  fecratio(1:jpoce), trc_data(1:jpi,1:jpj,17), iarroce(1:jpoce) )

      sedligand(:,1) = 1.E-9

      ! sediment pore water at 1st layer (k=1)
      DO jw = 1, jpwat
         pwcp(1:jpoce,1,jw) = pwcp_dta(1:jpoce,jw)
      ENDDO

      !  rain
      DO js = 1, jpsol
         rainrm(1:jpoce,js) = rainrm_dta(1:jpoce,js)
      ENDDO

      ! Calculation of raintg of each sol. comp.: rainrm in [g/(cm**2.s)]
      DO js = 1, jpsol
         rainrg(1:jpoce,js) = rainrm(1:jpoce,js) *  mol_wgt(js)
      ENDDO

      ! Calculation of raintg = total massic flux rained in each cell (sum of sol. comp.)
      raintg(:) = 0.
      DO js = 1, jpsol
         raintg(1:jpoce) = raintg(1:jpoce) + rainrg(1:jpoce,js)
      ENDDO

      ! computation of dzdep = total thickness of solid material rained [cm] in each cell
      dzdep(1:jpoce) = raintg(1:jpoce) * rdtsed(2) 

      IF( lk_iomput ) THEN
          IF( iom_use("sflxclay" ) ) CALL iom_put( "sflxclay", dust(:,:) * conv2 * 1E4 )
          IF( iom_use("sflxcal" ) )  CALL iom_put( "sflxcal", trc_data(:,:,13) )
          IF( iom_use("sflxbsi" ) )  CALL iom_put( "sflxbsi", trc_data(:,:,10) )
          IF( iom_use("sflxpoc" ) )  CALL iom_put( "sflxpoc", trc_data(:,:,11) + trc_data(:,:,12) )
      ENDIF

      IF( ln_timing )  CALL timing_stop('sed_dta')
      
   END SUBROUTINE sed_dta

END MODULE seddta
