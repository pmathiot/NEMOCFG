MODULE isfdiags
   !!======================================================================
   !!                       ***  MODULE  isfdiags  ***
   !! ice shelf diagnostics module :  manage the 2d and 3d flux outputs from the ice shelf module
   !!======================================================================
   !! History :  3.2  !  2011-02  (C.Harris  ) Original code isf cav
   !!            X.X  !  2006-02  (C. Wang   ) Original code bg03
   !!            3.4  !  2013-03  (P. Mathiot) Merging + parametrization
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_isf       : update sbc under ice shelf
   !!----------------------------------------------------------------------

   USE in_out_manager ! I/O manager
   USE dom_oce
   USE isf_oce        ! ice shelf variable
   USE iom            ! 

   IMPLICIT NONE

   PRIVATE

   PUBLIC   isf_diags_flx

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE isf_diags_flx(ktop, kbot, phtbl, pfrac, cdisf, pqfwf, pqoce, pqlat, pqhc)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_diags_flx ***
      !!
      !! ** Purpose : manage the 2d and 3d flux outputs of the ice shelf module
      !!              fwf, latent heat flux, heat content flux, oce->ice heat flux
      !!
      !!----------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      !!-------------------------- IN  -------------------------------------
      INTEGER , DIMENSION(jpi,jpj), INTENT(in) :: ktop , kbot               ! top and bottom level of the tbl
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: phtbl, pfrac              ! thickness of the tbl and fraction of last cell affected by the tbl
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pqfwf, pqoce, pqlat, pqhc ! 2d var to map in 3d
      CHARACTER(LEN=3), INTENT(in) :: cdisf                                 ! parametrisation or interactive melt
      !!---------------------------------------------------------------------
      CHARACTER(LEN=256) :: cvarqfwf  , cvarqoce  , cvarqlat  , cvarqhc
      CHARACTER(LEN=256) :: cvarqfwf3d, cvarqoce3d, cvarqlat3d, cvarqhc3d
      !!---------------------------------------------------------------------
      !
      ! output melt
      cvarqfwf = 'fwfisf_'//cdisf  ; cvarqfwf3d = 'fwfisf3d_'//cdisf
      cvarqoce = 'qoceisf_'//cdisf ; cvarqoce3d = 'qoceisf3d_'//cdisf
      cvarqlat = 'qlatisf_'//cdisf ; cvarqlat3d = 'qlatisf3d_'//cdisf 
      cvarqhc  = 'qhcisf_'//cdisf  ; cvarqhc3d  = 'qhcisf3d_'//cdisf
      !
      ! output 2d melt rate, latent heat and heat content flux from the injected water
      CALL iom_put( TRIM(cvarqfwf), pqfwf(:,:) )   ! mass         flux ( >0 out )
      CALL iom_put( TRIM(cvarqoce), pqoce(:,:) )   ! oce to ice   flux ( >0 out )
      CALL iom_put( TRIM(cvarqlat), pqlat(:,:) )   ! latent heat  flux ( >0 out )
      CALL iom_put( TRIM(cvarqhc) , pqhc (:,:) )   ! heat content flux ( >0 out )
      !
      ! output 3d Diagnostics
      IF ( iom_use( TRIM(cvarqfwf3d) ) ) CALL isf_diags_2dto3d( ktop, kbot, phtbl, pfrac, TRIM(cvarqfwf3d) , pqfwf(:,:))
      IF ( iom_use( TRIM(cvarqoce3d) ) ) CALL isf_diags_2dto3d( ktop, kbot, phtbl, pfrac, TRIM(cvarqoce3d) , pqoce(:,:))
      IF ( iom_use( TRIM(cvarqlat3d) ) ) CALL isf_diags_2dto3d( ktop, kbot, phtbl, pfrac, TRIM(cvarqlat3d) , pqoce(:,:))
      IF ( iom_use( TRIM(cvarqhc3d)  ) ) CALL isf_diags_2dto3d( ktop, kbot, phtbl, pfrac, TRIM(cvarqhc3d)  , pqhc (:,:))
      !
   END SUBROUTINE

   SUBROUTINE isf_diags_2dto3d(ktop, kbot, phtbl, pfrac, cdvar, pvar2d)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_diags_2dto3d ***
      !!
      !! ** Purpose : compute the 3d flux outputs as they are injected into NEMO 
      !!              (ie uniformaly spread into the top boundary layer or parametrisation layer)
      !!
      !!----------------------------------------------------------------------
      !!-------------------------- OUT -------------------------------------
      !!-------------------------- IN  -------------------------------------
      INTEGER , DIMENSION(jpi,jpj), INTENT(in) :: ktop , kbot   ! top and bottom level of the tbl
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: phtbl, pfrac  ! thickness of the tbl and fraction of last cell affected by the tbl
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pvar2d        ! 2d var to map in 3d
      CHARACTER(LEN=*), INTENT(in) :: cdvar
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jk                       ! loop indices
      INTEGER  :: ikt, ikb                         ! top and bottom level of the tbl
      REAL(wp), DIMENSION(jpi,jpj)     :: zvar2d   !
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zvar3d   ! 3d var to output
      !!---------------------------------------------------------------------
      !
      ! compute 3d output
      zvar2d(:,:) = pvar2d(:,:) / phtbl(:,:)
      zvar3d(:,:,:) = 0._wp
      !
      DO jj = 1,jpj
         DO ji = 1,jpi
            ikt = ktop(ji,jj)
            ikb = kbot(ji,jj)
            DO jk = ikt, ikb - 1
               zvar3d(ji,jj,jk) = zvar2d(ji,jj) * e3t_n(ji,jj,jk)
            END DO
            zvar3d(ji,jj,ikb) = zvar2d(ji,jj) * e3t_n(ji,jj,ikb) * pfrac(ji,jj)
         END DO
      END DO
      !
      CALL iom_put( TRIM(cdvar) , zvar3d(:,:,:))
      !
   END SUBROUTINE isf_diags_2dto3d

END MODULE isfdiags
