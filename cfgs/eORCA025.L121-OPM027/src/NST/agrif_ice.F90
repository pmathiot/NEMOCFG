MODULE agrif_ice
   !!======================================================================
   !!                       ***  MODULE agrif_ice  ***
   !! AGRIF :   define in memory AGRIF variables for sea-ice
   !!----------------------------------------------------------------------
   !! History :  3.4   !  2012-08  (R. Benshila)          Original code
   !!            4.0   !  2018     (C. Rousset)           SI3 compatibility
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!   'key_si3'                                         SI3 sea-ice model
   !!----------------------------------------------------------------------   
   IMPLICIT NONE
   PRIVATE 

   INTEGER, PUBLIC ::  u_ice_id, v_ice_id, tra_ice_id
   INTEGER, PUBLIC ::  nbstep_ice = 0    ! child time position in sea-ice model

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_ice.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

#endif
   !!======================================================================
END MODULE agrif_ice
