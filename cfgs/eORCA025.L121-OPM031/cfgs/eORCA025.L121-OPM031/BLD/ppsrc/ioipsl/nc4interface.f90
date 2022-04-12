










MODULE nc4interface
!-
!-
! This software is governed by the CeCILL license
! See IOIPSL/IOIPSL_License_CeCILL.txt
!---------------------------------------------------------------------
      !!--------------------------------------------------------------------
      !! 'key_netcdf4' Dummy module (usually defines dummy routines for netcdf4
      !!               calls when compiling without netcdf4 libraries
      !!--------------------------------------------------------------------

   USE netcdf

   !- netcdf4 chunking control structure
   !- (optional on histbeg and histend calls)
!$AGRIF_DO_NOT_TREAT
   TYPE, PUBLIC :: snc4_ctl
      SEQUENCE
      INTEGER :: ni
      INTEGER :: nj
      INTEGER :: nk
      LOGICAL :: luse
   END TYPE snc4_ctl
!$AGRIF_END_DO_NOT_TREAT

CONTAINS
   INTEGER FUNCTION SET_NF90_DEF_VAR_CHUNKING(nfid, nvid, ichunkalg, ichunksz)
      !!--------------------------------------------------------------------
      !!                   ***  SUBROUTINE NF90_DEF_VAR_CHUNKING  ***
      !!
      !! ** Purpose :   Interface NetCDF4 routine to enable compiling with NetCDF4 libraries
      !!                but no 1
      !!--------------------------------------------------------------------
      INTEGER,               INTENT(in) :: nfid
      INTEGER,               INTENT(in) :: nvid
      INTEGER,               INTENT(in) :: ichunkalg
      INTEGER, DIMENSION(:), INTENT(in) :: ichunksz
      !!
      INTEGER                           :: iret
      !!
      iret = NF90_DEF_VAR_CHUNKING(nfid, nvid, ichunkalg, ichunksz)
      SET_NF90_DEF_VAR_CHUNKING = iret
   END FUNCTION SET_NF90_DEF_VAR_CHUNKING

   INTEGER FUNCTION SET_NF90_DEF_VAR_DEFLATE(nfid, nvid, ishuffle, ideflate, ideflate_level)
      !!--------------------------------------------------------------------
      !!                   ***  SUBROUTINE NF90_DEF_VAR_DEFLATE  ***
      !!
      !! ** Purpose :   Interface NetCDF4 routine to enable compiling with NetCDF4 libraries
      !!                but no 1
      !!--------------------------------------------------------------------
      INTEGER,               INTENT(in) :: nfid
      INTEGER,               INTENT(in) :: nvid
      INTEGER,               INTENT(in) :: ishuffle
      INTEGER,               INTENT(in) :: ideflate
      INTEGER,               INTENT(in) :: ideflate_level
      !!
      INTEGER                           :: iret
      !!
      iret = NF90_DEF_VAR_DEFLATE(nfid, nvid, ishuffle, ideflate, ideflate_level)
      SET_NF90_DEF_VAR_DEFLATE = iret
   END FUNCTION SET_NF90_DEF_VAR_DEFLATE

   SUBROUTINE GET_NF90_SYMBOL(sym_name, ivalue)
      CHARACTER(len=*),      INTENT(in)  :: sym_name
      INTEGER,               INTENT(out) :: ivalue
      SELECT CASE (sym_name)
         CASE ("NF90_HDF5")
            ivalue = NF90_HDF5
         CASE DEFAULT
            WRITE(*,*) "Warning: unknown case in GET_NF90_SYMBOL"
      END SELECT
   END SUBROUTINE GET_NF90_SYMBOL

!------------------
END MODULE nc4interface
