#define TWO_WAY

 MODULE agrif_all_update
   !!======================================================================
   !!                   ***  MODULE  agrif_all_update  ***
   !! AGRIF: Main update driver for ocean, ice and passive tracers
   !!======================================================================
   !! History :  4.0  !  2018-06  (J. Chanut)  Original code 
   !!----------------------------------------------------------------------
#if defined key_agrif 
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE dom_oce
   USE agrif_oce
   USE agrif_oce_update
#if defined key_top
   USE agrif_top_update
#endif
#if defined key_si3
   USE agrif_ice_update
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   Agrif_Update_All

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_all_update.F90 10069 2018-08-28 14:12:24Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Update_All( )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Update_All ***
      !!
      !! ** Purpose :: Update nested grids for all components (Ocean, Sea Ice, TOP)
      !!               Order of update matters here !
      !!----------------------------------------------------------------------
# if defined TWO_WAY
      IF (Agrif_Root()) RETURN
      !
      IF (lwp.AND.lk_agrif_debug) Write(*,*) ' --> START AGRIF UPDATE from grid Number',Agrif_Fixed()
      !
      CALL Agrif_Update_ssh()                      ! Update sea level
      !
      IF (.NOT.ln_linssh) CALL Agrif_Update_vvl()  ! Update scale factors
      !
      CALL Agrif_Update_tra()                      ! Update temperature/salinity
      !
#if defined key_top
      CALL Agrif_Update_Trc()                      ! Update passive tracers
#endif
      !
      CALL Agrif_Update_dyn()                      ! Update dynamics
      !
! JC remove update because this precludes from perfect restartability
!!      CALL Agrif_Update_tke()                  ! Update tke 

#if defined key_si3
      CALL agrif_update_ice()                      ! Update sea ice
#endif
      IF (lwp.AND.lk_agrif_debug) Write(*,*) ' <-- END AGRIF UPDATE from grid Number',Agrif_Fixed()
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
#endif
    END SUBROUTINE agrif_Update_All

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_Update_all( )
      WRITE(*,*)  'Agrif_Update_All : You should not have seen this print! error?'
   END SUBROUTINE Agrif_Update_all
#endif

   !!======================================================================
END MODULE agrif_all_update

