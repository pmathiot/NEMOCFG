MODULE isfutils
   !!======================================================================
   !!                       ***  MODULE  isfutils  ***
   !! istutils module : miscelenious useful routines
   !!======================================================================
   !! History :  4.1  !  2019-09  (P. Mathiot) original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfutils       : - read_2dcstdta to read a constant input file with iom_get
   !!                    - debug to print array sum, min, max in ocean.output
   !!----------------------------------------------------------------------

   USE iom           , ONLY: iom_open, iom_get, iom_close, jpdom_data ! read input file
   USE lib_fortran   , ONLY: glob_sum, glob_min, glob_max             ! compute global value
   USE par_oce       , ONLY: jpi,jpj,jpk                              ! domain size
   USE dom_oce       , ONLY: nldi, nlei, nldj, nlej                   ! local domain
   USE in_out_manager, ONLY: i8, wp, lwp, numout                          ! miscelenious
   USE lib_mpp

   IMPLICIT NONE

   PRIVATE

   INTERFACE debug
      MODULE PROCEDURE debug2d, debug3d
   END INTERFACE debug

   PUBLIC read_2dcstdta, debug

CONTAINS

   SUBROUTINE read_2dcstdta(cdfile, cdvar, pvar)
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE read_2dcstdta  ***
      !!
      !! ** Purpose : read input file
      !!
      !!-------------------------- OUT -------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) :: pvar          ! output variable
      !!-------------------------- IN  -------------------------------------
      CHARACTER(len=*)           , INTENT(in   ) :: cdfile   ! input file name
      CHARACTER(len=*)           , INTENT(in   ) :: cdvar    ! variable name
      !!--------------------------------------------------------------------
      INTEGER :: inum
      !!--------------------------------------------------------------------

      CALL iom_open( TRIM(cdfile), inum )
      CALL iom_get( inum, jpdom_data, TRIM(cdvar), pvar)
      CALL iom_close(inum)

   END SUBROUTINE read_2dcstdta

   SUBROUTINE debug2d(cdtxt,pvar)
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_debug2d  ***
      !!
      !! ** Purpose : add debug print for 2d variables
      !!
      !!-------------------------- IN  -------------------------------------
      CHARACTER(LEN=*)          , INTENT(in   ) :: cdtxt
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) :: pvar
      !!--------------------------------------------------------------------
      REAL(wp)    :: zmin, zmax, zsum
      INTEGER(i8) :: imodd, ip
      INTEGER     :: itmps,imods, ji, jj, jk
      !!--------------------------------------------------------------------
      !
      ! global min/max/sum to check data range and NaN
      zsum = glob_sum( 'debug', pvar(:,:) )
      zmin = glob_min( 'debug', pvar(:,:) )
      zmax = glob_max( 'debug', pvar(:,:) )
      !
      ! basic check sum to check reproducibility
      ! TRANSFER function find out the integer corresponding to pvar(i,j) bit pattern
      ! MOD allow us to keep only the latest digits during the sum
      ! imod is not choosen to be very large as at the end there is a classic mpp_sum
      imodd=65521 ! highest prime number < 2**16 with i8 type
      imods=65521 ! highest prime number < 2**16 with default integer for mpp_sum subroutine
      itmps=0
      DO jj=nldj,nlej
         DO ji=nldi,nlei
            itmps = MOD(itmps + MOD(TRANSFER(pvar(ji,jj), ip),imodd), imods)
         END DO
      END DO
      CALL mpp_sum('debug',itmps)
      !
      ! print out
      IF (lwp) THEN
         WRITE(numout,*) TRIM(cdtxt),' (min, max, sum, tag) : ',zmin, zmax, zsum, itmps
         CALL FLUSH(numout)
      END IF
      !
   END SUBROUTINE debug2d

   SUBROUTINE debug3d(cdtxt,pvar)
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_debug3d  ***
      !!
      !! ** Purpose : add debug print for 3d variables
      !!
      !!-------------------------- IN  -------------------------------------
      CHARACTER(LEN=*)              , INTENT(in   ) :: cdtxt
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) :: pvar
      !!--------------------------------------------------------------------
      REAL(wp) :: zmin, zmax, zsum
      INTEGER(i8) :: imodd, ip
      INTEGER     :: itmps,imods, ji, jj, jk
      !!--------------------------------------------------------------------
      !
      ! global min/max/sum to check data range and NaN
      zsum = glob_sum( 'debug', pvar(:,:,:) )
      zmin = glob_min( 'debug', pvar(:,:,:) )
      zmax = glob_max( 'debug', pvar(:,:,:) )
      !
      ! basic check sum to check reproducibility
      ! TRANSFER function find out the integer corresponding to pvar(i,j) bit pattern
      ! MOD allow us to keep only the latest digits during the sum
      ! imod is not choosen to be very large as at the end there is a classic mpp_sum
      imodd=65521 ! highest prime number < 2**16 with i8 type
      imods=65521 ! highest prime number < 2**16 with default integer for mpp_sum subroutine
      itmps=0
      DO jk=1,jpk
         DO jj=nldj,nlej
            DO ji=nldi,nlei
               itmps = MOD(itmps + MOD(TRANSFER(pvar(ji,jj,jk), ip),imodd), imods)
            END DO
         END DO
      END DO
      CALL mpp_sum('debug',itmps)
      !
      ! print out
      IF (lwp) THEN
         WRITE(numout,*) TRIM(cdtxt),' (min, max, sum, tag) : ',zmin, zmax, zsum, itmps
         CALL FLUSH(numout)
      END IF
      !
   END SUBROUTINE debug3d

END MODULE isfutils
