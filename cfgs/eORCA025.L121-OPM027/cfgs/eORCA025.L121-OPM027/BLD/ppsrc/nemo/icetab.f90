










MODULE icetab
   !!======================================================================
   !!                       ***  MODULE icetab   ***
   !!   sea-ice : transform 1D (2D) array to a 2D (1D) table
   !!======================================================================
   !! History :  4.0  !  2018     (C. Rousset)       Original code SI3
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   tab_3d_2d  : 3-D <==> 2-D
   !!   tab_2d_3d  : 2-D <==> 3-D
   !!   tab_2d_1d  : 2-D <==> 1-D
   !!   tab_1d_2d  : 1-D <==> 2-D
   !!----------------------------------------------------------------------
   USE par_oce
   USE ice, ONLY : jpl
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   tab_3d_2d
   PUBLIC   tab_2d_1d
   PUBLIC   tab_2d_3d
   PUBLIC   tab_1d_2d

   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icetab.F90 10069 2018-08-28 14:12:24Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tab_3d_2d( ndim1d, tab_ind, tab1d, tab2d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_2d_1d  ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   ndim1d   ! 1d size
      INTEGER , DIMENSION(ndim1d)     , INTENT(in   ) ::   tab_ind  ! input index
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(in   ) ::   tab2d    ! input 2D field
      REAL(wp), DIMENSION(ndim1d,jpl) , INTENT(  out) ::   tab1d    ! output 1D field
      !
      INTEGER ::   jl, jn, jid, jjd
      !!----------------------------------------------------------------------
      DO jl = 1, jpl
         DO jn = 1, ndim1d
            jid          = MOD( tab_ind(jn) - 1 , jpi ) + 1
            jjd          =    ( tab_ind(jn) - 1 ) / jpi + 1
            tab1d(jn,jl) = tab2d(jid,jjd,jl)
         END DO
      END DO
   END SUBROUTINE tab_3d_2d


   SUBROUTINE tab_2d_1d( ndim1d, tab_ind, tab1d, tab2d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_2d_1d  ***
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   ndim1d   ! 1d size
      INTEGER , DIMENSION(ndim1d) , INTENT(in   ) ::   tab_ind  ! input index
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   tab2d    ! input 2D field
      REAL(wp), DIMENSION(ndim1d) , INTENT(  out) ::   tab1d    ! output 1D field
      !
      INTEGER ::   jn , jid, jjd
      !!----------------------------------------------------------------------
      DO jn = 1, ndim1d
         jid        = MOD( tab_ind(jn) - 1 , jpi ) + 1
         jjd        =    ( tab_ind(jn) - 1 ) / jpi + 1
         tab1d( jn) = tab2d( jid, jjd)
      END DO
   END SUBROUTINE tab_2d_1d


   SUBROUTINE tab_2d_3d( ndim1d, tab_ind, tab1d, tab2d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_2d_1d  ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   ndim1d    ! 1D size
      INTEGER , DIMENSION(ndim1d)     , INTENT(in   ) ::   tab_ind   ! input index
      REAL(wp), DIMENSION(ndim1d,jpl) , INTENT(in   ) ::   tab1d     ! input 1D field
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   tab2d     ! output 2D field
      !
      INTEGER ::   jl, jn, jid, jjd
      !!----------------------------------------------------------------------
      DO jl = 1, jpl
         DO jn = 1, ndim1d
            jid               = MOD( tab_ind(jn) - 1 ,  jpi ) + 1
            jjd               =    ( tab_ind(jn) - 1 ) / jpi  + 1
            tab2d(jid,jjd,jl) = tab1d(jn,jl)
         END DO
      END DO
   END SUBROUTINE tab_2d_3d


   SUBROUTINE tab_1d_2d( ndim1d, tab_ind, tab1d, tab2d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_2d_1d  ***
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   ndim1d    ! 1D size
      INTEGER , DIMENSION(ndim1d) , INTENT(in   ) ::   tab_ind   ! input index
      REAL(wp), DIMENSION(ndim1d) , INTENT(in   ) ::   tab1d     ! input 1D field
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   tab2d     ! output 2D field
      !
      INTEGER ::   jn , jid, jjd
      !!----------------------------------------------------------------------
      DO jn = 1, ndim1d
         jid             = MOD( tab_ind(jn) - 1 ,  jpi ) + 1
         jjd             =    ( tab_ind(jn) - 1 ) / jpi  + 1
         tab2d(jid, jjd) = tab1d( jn)
      END DO
   END SUBROUTINE tab_1d_2d


   !!======================================================================
END MODULE icetab
