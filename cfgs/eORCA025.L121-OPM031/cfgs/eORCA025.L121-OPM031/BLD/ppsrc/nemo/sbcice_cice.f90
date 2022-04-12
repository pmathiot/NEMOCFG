










MODULE sbcice_cice
   !!======================================================================
   !!                       ***  MODULE  sbcice_cice  ***
   !! To couple with sea ice model CICE (LANL)
   !!=====================================================================
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module         NO CICE sea-ice model
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_ice_cice ( kt, ksbc )     ! Dummy routine
      IMPLICIT NONE
      INTEGER, INTENT( in ) :: kt, ksbc
      WRITE(*,*) 'sbc_ice_cice: You should not have seen this print! error?', kt
   END SUBROUTINE sbc_ice_cice

   SUBROUTINE cice_sbc_init (ksbc)    ! Dummy routine
      IMPLICIT NONE
      INTEGER, INTENT( in ) :: ksbc
      WRITE(*,*) 'cice_sbc_init: You should not have seen this print! error?', ksbc
   END SUBROUTINE cice_sbc_init

   SUBROUTINE cice_sbc_final     ! Dummy routine
      IMPLICIT NONE
      WRITE(*,*) 'cice_sbc_final: You should not have seen this print! error?'
   END SUBROUTINE cice_sbc_final


   !!======================================================================
END MODULE sbcice_cice
