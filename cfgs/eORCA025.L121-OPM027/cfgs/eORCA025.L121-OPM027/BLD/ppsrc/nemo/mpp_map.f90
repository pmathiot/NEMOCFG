










MODULE mpp_map
   !!======================================================================
   !!                       ***  MODULE mpp_mpa  ***
   !! NEMOVAR: MPP global grid point mapping to processors
   !!======================================================================
   !! History :  2.0  ! 2007-08  (K. Mogensen)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  mppmap_init : Initialize mppmap.
   !!----------------------------------------------------------------------
   USE par_kind, ONLY :   wp            ! Precision variables
   USE par_oce , ONLY :   jpi, jpj      ! Ocean parameters
   USE dom_oce , ONLY :   mig, mjg, nldi, nlei, nldj, nlej, nlci, nlcj, narea   ! Ocean space and time domain variables
   USE lib_mpp, ONLY :   mpi_comm_oce   ! MPP library
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC ::   mppmap_init, mppmap   !: ???

   INTEGER, DIMENSION(:,:), ALLOCATABLE ::   mppmap   ! ???

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: mpp_map.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE mppmap_init
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mppmap_init ***
      !!          
      !! ** Purpose : Setup a global map of processor rank for all gridpoints
      !!
      !! ** Method  : MPI all reduce.
      !!
      !! ** Action  : This does only work for MPI. 
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::   imppmap   !
      INTEGER :: ierr

INCLUDE 'mpif.h'
      !!----------------------------------------------------------------------

      IF (.NOT. ALLOCATED(mppmap)) THEN
         ALLOCATE( &
            & mppmap(jpiglo,jpjglo) &
            & )
      ENDIF
      ! Initialize local imppmap

      ALLOCATE( &
         & imppmap(jpiglo,jpjglo) &
         & )
      imppmap(:,:) = 0

!      ! Setup local grid points
      imppmap(mig(1):mig(nlci),mjg(1):mjg(nlcj)) = narea
      
      ! Get global data


      ! Call the MPI library to find the max across processors
      CALL mpi_allreduce( imppmap, mppmap, jpiglo*jpjglo, mpi_integer,   &
         &                mpi_max, mpi_comm_oce, ierr )
      !
   END SUBROUTINE mppmap_init

   !!======================================================================
END MODULE mpp_map
