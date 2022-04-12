










MODULE lbcnfd
   !!======================================================================
   !!                       ***  MODULE  lbcnfd  ***
   !! Ocean        : north fold  boundary conditions
   !!======================================================================
   !! History :  3.2  ! 2009-03  (R. Benshila)  Original code 
   !!            3.5  ! 2013-07  (I. Epicoco, S. Mocavero - CMCC) MPP optimization
   !!            4.0  ! 2017-04  (G. Madec) automatique allocation of array argument (use any 3rd dimension)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   lbc_nfd       : generic interface for lbc_nfd_3d and lbc_nfd_2d routines
   !!   lbc_nfd_3d    : lateral boundary condition: North fold treatment for a 3D arrays   (lbc_nfd)
   !!   lbc_nfd_2d    : lateral boundary condition: North fold treatment for a 2D arrays   (lbc_nfd)
   !!   lbc_nfd_nogather       : generic interface for lbc_nfd_nogather_3d and 
   !!                            lbc_nfd_nogather_2d routines (designed for use
   !!                            with ln_nnogather to avoid global width arrays
   !!                            mpi all gather operations)
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain 
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_nfd
      MODULE PROCEDURE   lbc_nfd_2d    , lbc_nfd_3d    , lbc_nfd_4d
      MODULE PROCEDURE   lbc_nfd_2d_ptr, lbc_nfd_3d_ptr, lbc_nfd_4d_ptr
      MODULE PROCEDURE   lbc_nfd_2d_ext
   END INTERFACE
   !
   INTERFACE lbc_nfd_nogather
!                        ! Currently only 4d array version is needed
     MODULE PROCEDURE   lbc_nfd_nogather_2d    , lbc_nfd_nogather_3d
     MODULE PROCEDURE   lbc_nfd_nogather_4d
     MODULE PROCEDURE   lbc_nfd_nogather_2d_ptr, lbc_nfd_nogather_3d_ptr
!     MODULE PROCEDURE   lbc_nfd_nogather_4d_ptr
   END INTERFACE

   TYPE, PUBLIC ::   PTR_2D   !: array of 2D pointers (also used in lib_mpp)
      REAL(wp), DIMENSION (:,:)    , POINTER ::   pt2d
   END TYPE PTR_2D
   TYPE, PUBLIC ::   PTR_3D   !: array of 3D pointers (also used in lib_mpp)
      REAL(wp), DIMENSION (:,:,:)  , POINTER ::   pt3d
   END TYPE PTR_3D
   TYPE, PUBLIC ::   PTR_4D   !: array of 4D pointers (also used in lib_mpp)
      REAL(wp), DIMENSION (:,:,:,:), POINTER ::   pt4d
   END TYPE PTR_4D

   PUBLIC   lbc_nfd            ! north fold conditions
   PUBLIC   lbc_nfd_nogather   ! north fold conditions (no allgather case)

   INTEGER, PUBLIC, PARAMETER            ::   jpmaxngh = 3               !:
   INTEGER, PUBLIC                       ::   nsndto, nfsloop, nfeloop   !:
   INTEGER, PUBLIC, DIMENSION (jpmaxngh) ::   isendto                    !: processes to which communicate

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lbcnfd.F90 11536 2019-09-11 13:54:18Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***  routine lbc_nfd_(2,3,4)d  ***
   !!----------------------------------------------------------------------
   !!
   !! ** Purpose :   lateral boundary condition 
   !!                North fold treatment without processor exchanges. 
   !!
   !! ** Method  :   
   !!
   !! ** Action  :   ptab with updated values along the north fold
   !!----------------------------------------------------------------------
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_2d( ptab, cd_nat, psgn       )
      REAL(wp),INTENT(inout)::ptab(:,:)                             ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj) = psgn  * ptab(ijt,ipj-2)
               END DO
               ptab(1,ipj) = psgn  * ptab(3,ipj-2)
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipjm1) = psgn  * ptab(ijt,ipjm1)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj) = psgn  * ptab(iju,ipj-2)
               END DO
               ptab(   1  ,ipj) = psgn  * ptab(    2   ,ipj-2)
               ptab(jpiglo,ipj) = psgn  * ptab(jpiglo-1,ipj-2) 
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipjm1) = psgn  * ptab(iju,ipjm1)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj-1) = psgn  * ptab(ijt,ipj-2)
                  ptab(ji,ipj  ) = psgn  * ptab(ijt,ipj-3)
               END DO
               ptab(1,ipj) = psgn  * ptab(3,ipj-3) 
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj-1) = psgn  * ptab(iju,ipj-2)
                  ptab(ji,ipj  ) = psgn  * ptab(iju,ipj-3)
               END DO
               ptab(   1  ,ipj) = psgn  * ptab(    2   ,ipj-3)
               ptab(jpiglo,ipj) = psgn  * ptab(jpiglo-1,ipj-3) 
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj) = psgn  * ptab(ijt,ipj-1)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj) = psgn  * ptab(iju,ipj-1)
               END DO
               ptab(jpiglo,ipj) = psgn * ptab(jpiglo-2,ipj-1)
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj) = psgn  * ptab(ijt,ipj-2)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipjm1) = psgn  * ptab(ijt,ipjm1)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj  ) = psgn  * ptab(iju,ipj-2)
               END DO
               ptab(jpiglo,ipj) = psgn   * ptab(jpiglo-2,ipj-2)
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipjm1) = psgn  * ptab(iju,ipjm1)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(:, 1 ) = 0._wp
               ptab(:,ipj) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(:,ipj) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_2d


   SUBROUTINE lbc_nfd_2d_ptr( ptab, cd_nat, psgn, kfld )
      INTEGER          , INTENT(in   ) ::   kfld        ! number of pt3d arrays
      TYPE(PTR_2D),INTENT(inout)::ptab(:)                             ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat(jf)  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt2d(ji,ipj) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipj-2)
               END DO
               ptab(jf)%pt2d(1,ipj) = psgn(jf)  * ptab(jf)%pt2d(3,ipj-2)
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt2d(ji,ipjm1) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipjm1)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt2d(ji,ipj) = psgn(jf)  * ptab(jf)%pt2d(iju,ipj-2)
               END DO
               ptab(jf)%pt2d(   1  ,ipj) = psgn(jf)  * ptab(jf)%pt2d(    2   ,ipj-2)
               ptab(jf)%pt2d(jpiglo,ipj) = psgn(jf)  * ptab(jf)%pt2d(jpiglo-1,ipj-2) 
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt2d(ji,ipjm1) = psgn(jf)  * ptab(jf)%pt2d(iju,ipjm1)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt2d(ji,ipj-1) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipj-2)
                  ptab(jf)%pt2d(ji,ipj  ) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipj-3)
               END DO
               ptab(jf)%pt2d(1,ipj) = psgn(jf)  * ptab(jf)%pt2d(3,ipj-3) 
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt2d(ji,ipj-1) = psgn(jf)  * ptab(jf)%pt2d(iju,ipj-2)
                  ptab(jf)%pt2d(ji,ipj  ) = psgn(jf)  * ptab(jf)%pt2d(iju,ipj-3)
               END DO
               ptab(jf)%pt2d(   1  ,ipj) = psgn(jf)  * ptab(jf)%pt2d(    2   ,ipj-3)
               ptab(jf)%pt2d(jpiglo,ipj) = psgn(jf)  * ptab(jf)%pt2d(jpiglo-1,ipj-3) 
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat(jf)  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt2d(ji,ipj) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipj-1)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt2d(ji,ipj) = psgn(jf)  * ptab(jf)%pt2d(iju,ipj-1)
               END DO
               ptab(jf)%pt2d(jpiglo,ipj) = psgn(jf) * ptab(jf)%pt2d(jpiglo-2,ipj-1)
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt2d(ji,ipj) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipj-2)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt2d(ji,ipjm1) = psgn(jf)  * ptab(jf)%pt2d(ijt,ipjm1)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt2d(ji,ipj  ) = psgn(jf)  * ptab(jf)%pt2d(iju,ipj-2)
               END DO
               ptab(jf)%pt2d(jpiglo,ipj) = psgn(jf)   * ptab(jf)%pt2d(jpiglo-2,ipj-2)
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt2d(ji,ipjm1) = psgn(jf)  * ptab(jf)%pt2d(iju,ipjm1)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat(jf) )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(jf)%pt2d(:, 1 ) = 0._wp
               ptab(jf)%pt2d(:,ipj) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(jf)%pt2d(:,ipj) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_2d_ptr

   !
   !                       !==  2D array with extra haloes  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_2d_ext( ptab, cd_nat, psgn, kextj )
      !!----------------------------------------------------------------------
      INTEGER          , INTENT(in   ) ::   kextj       ! extra halo width at north fold, declared before its use in ARRAY_TYPE
      REAL(wp),INTENT(inout)::ptab(:,1-kextj:)                      ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      !
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO jh = 0, kextj
                  DO ji = 2, jpiglo
                     ijt = jpiglo-ji+2
                     ptab(ji,ipj+jh) = psgn  * ptab(ijt,ipj-2-jh)
                  END DO
                  ptab(1,ipj+jh) = psgn  * ptab(3,ipj-2-jh)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipjm1) = psgn  * ptab(ijt,ipjm1)
               END DO
            CASE ( 'U' )                               ! U-point
               DO jh = 0, kextj
                  DO ji = 2, jpiglo-1
                     iju = jpiglo-ji+1
                     ptab(ji,ipj+jh) = psgn  * ptab(iju,ipj-2-jh)
                  END DO
                 ptab(   1  ,ipj+jh) = psgn  * ptab(    2   ,ipj-2-jh)
                 ptab(jpiglo,ipj+jh) = psgn  * ptab(jpiglo-1,ipj-2-jh) 
               END DO
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipjm1) = psgn  * ptab(iju,ipjm1)
               END DO
            CASE ( 'V' )                               ! V-point
               DO jh = 0, kextj
                  DO ji = 2, jpiglo
                     ijt = jpiglo-ji+2
                     ptab(ji,ipj-1+jh) = psgn  * ptab(ijt,ipj-2-jh)
                     ptab(ji,ipj+jh  ) = psgn  * ptab(ijt,ipj-3-jh)
                  END DO
                  ptab(1,ipj+jh) = psgn  * ptab(3,ipj-3-jh) 
               END DO
            CASE ( 'F' )                               ! F-point
               DO jh = 0, kextj
                  DO ji = 1, jpiglo-1
                     iju = jpiglo-ji+1
                     ptab(ji,ipj-1+jh) = psgn  * ptab(iju,ipj-2-jh)
                     ptab(ji,ipj+jh  ) = psgn  * ptab(iju,ipj-3-jh)
                  END DO
               END DO
               DO jh = 0, kextj
                  ptab(   1  ,ipj+jh) = psgn  * ptab(    2   ,ipj-3-jh)
                  ptab(jpiglo,ipj+jh) = psgn  * ptab(jpiglo-1,ipj-3-jh)
               END DO
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO jh = 0, kextj
                  DO ji = 1, jpiglo
                     ijt = jpiglo-ji+1
                     ptab(ji,ipj+jh) = psgn  * ptab(ijt,ipj-1-jh)
                  END DO
               END DO
            CASE ( 'U' )                               ! U-point
               DO jh = 0, kextj
                  DO ji = 1, jpiglo-1
                     iju = jpiglo-ji
                     ptab(ji,ipj+jh) = psgn  * ptab(iju,ipj-1-jh)
                  END DO
                  ptab(jpiglo,ipj+jh) = psgn  * ptab(jpiglo-2,ipj-1-jh)
               END DO
            CASE ( 'V' )                               ! V-point
               DO jh = 0, kextj
                  DO ji = 1, jpiglo
                     ijt = jpiglo-ji+1
                     ptab(ji,ipj+jh) = psgn  * ptab(ijt,ipj-2-jh)
                  END DO
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipjm1) = psgn  * ptab(ijt,ipjm1)
               END DO
            CASE ( 'F' )                               ! F-point
               DO jh = 0, kextj
                  DO ji = 1, jpiglo-1
                     iju = jpiglo-ji
                     ptab(ji,ipj+jh  ) = psgn  * ptab(iju,ipj-2-jh)
                  END DO
                  ptab(jpiglo,ipj+jh) = psgn  * ptab(jpiglo-2,ipj-2-jh)
               END DO
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipjm1) = psgn  * ptab(iju,ipjm1)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(:,  1:1-kextj  ) = 0._wp
               ptab(:,ipj:ipj+kextj) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(:,ipj:ipj+kextj) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_2d_ext

   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_3d( ptab, cd_nat, psgn       )
      REAL(wp),INTENT(inout)::ptab(:,:,:)                             ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj,:) = psgn  * ptab(ijt,ipj-2,:)
               END DO
               ptab(1,ipj,:) = psgn  * ptab(3,ipj-2,:)
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipjm1,:) = psgn  * ptab(ijt,ipjm1,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj,:) = psgn  * ptab(iju,ipj-2,:)
               END DO
               ptab(   1  ,ipj,:) = psgn  * ptab(    2   ,ipj-2,:)
               ptab(jpiglo,ipj,:) = psgn  * ptab(jpiglo-1,ipj-2,:) 
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipjm1,:) = psgn  * ptab(iju,ipjm1,:)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj-1,:) = psgn  * ptab(ijt,ipj-2,:)
                  ptab(ji,ipj  ,:) = psgn  * ptab(ijt,ipj-3,:)
               END DO
               ptab(1,ipj,:) = psgn  * ptab(3,ipj-3,:) 
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj-1,:) = psgn  * ptab(iju,ipj-2,:)
                  ptab(ji,ipj  ,:) = psgn  * ptab(iju,ipj-3,:)
               END DO
               ptab(   1  ,ipj,:) = psgn  * ptab(    2   ,ipj-3,:)
               ptab(jpiglo,ipj,:) = psgn  * ptab(jpiglo-1,ipj-3,:) 
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj,:) = psgn  * ptab(ijt,ipj-1,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj,:) = psgn  * ptab(iju,ipj-1,:)
               END DO
               ptab(jpiglo,ipj,:) = psgn * ptab(jpiglo-2,ipj-1,:)
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj,:) = psgn  * ptab(ijt,ipj-2,:)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipjm1,:) = psgn  * ptab(ijt,ipjm1,:)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj  ,:) = psgn  * ptab(iju,ipj-2,:)
               END DO
               ptab(jpiglo,ipj,:) = psgn   * ptab(jpiglo-2,ipj-2,:)
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipjm1,:) = psgn  * ptab(iju,ipjm1,:)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(:, 1 ,:) = 0._wp
               ptab(:,ipj,:) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(:,ipj,:) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_3d


   SUBROUTINE lbc_nfd_3d_ptr( ptab, cd_nat, psgn, kfld )
      INTEGER          , INTENT(in   ) ::   kfld        ! number of pt3d arrays
      TYPE(PTR_3D),INTENT(inout)::ptab(:)                             ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab(1)%pt3d,3)   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat(jf)  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt3d(ji,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipj-2,:)
               END DO
               ptab(jf)%pt3d(1,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(3,ipj-2,:)
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt3d(ji,ipjm1,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipjm1,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt3d(ji,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipj-2,:)
               END DO
               ptab(jf)%pt3d(   1  ,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(    2   ,ipj-2,:)
               ptab(jf)%pt3d(jpiglo,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(jpiglo-1,ipj-2,:) 
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt3d(ji,ipjm1,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipjm1,:)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt3d(ji,ipj-1,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipj-2,:)
                  ptab(jf)%pt3d(ji,ipj  ,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipj-3,:)
               END DO
               ptab(jf)%pt3d(1,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(3,ipj-3,:) 
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt3d(ji,ipj-1,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipj-2,:)
                  ptab(jf)%pt3d(ji,ipj  ,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipj-3,:)
               END DO
               ptab(jf)%pt3d(   1  ,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(    2   ,ipj-3,:)
               ptab(jf)%pt3d(jpiglo,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(jpiglo-1,ipj-3,:) 
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat(jf)  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt3d(ji,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipj-1,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt3d(ji,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipj-1,:)
               END DO
               ptab(jf)%pt3d(jpiglo,ipj,:) = psgn(jf) * ptab(jf)%pt3d(jpiglo-2,ipj-1,:)
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt3d(ji,ipj,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipj-2,:)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt3d(ji,ipjm1,:) = psgn(jf)  * ptab(jf)%pt3d(ijt,ipjm1,:)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt3d(ji,ipj  ,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipj-2,:)
               END DO
               ptab(jf)%pt3d(jpiglo,ipj,:) = psgn(jf)   * ptab(jf)%pt3d(jpiglo-2,ipj-2,:)
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt3d(ji,ipjm1,:) = psgn(jf)  * ptab(jf)%pt3d(iju,ipjm1,:)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat(jf) )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(jf)%pt3d(:, 1 ,:) = 0._wp
               ptab(jf)%pt3d(:,ipj,:) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(jf)%pt3d(:,ipj,:) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_3d_ptr

   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_4d( ptab, cd_nat, psgn       )
      REAL(wp),INTENT(inout)::ptab(:,:,:,:)                             ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = SIZE(ptab,4)   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj,:,:) = psgn  * ptab(ijt,ipj-2,:,:)
               END DO
               ptab(1,ipj,:,:) = psgn  * ptab(3,ipj-2,:,:)
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipjm1,:,:) = psgn  * ptab(ijt,ipjm1,:,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj,:,:) = psgn  * ptab(iju,ipj-2,:,:)
               END DO
               ptab(   1  ,ipj,:,:) = psgn  * ptab(    2   ,ipj-2,:,:)
               ptab(jpiglo,ipj,:,:) = psgn  * ptab(jpiglo-1,ipj-2,:,:) 
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipjm1,:,:) = psgn  * ptab(iju,ipjm1,:,:)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj-1,:,:) = psgn  * ptab(ijt,ipj-2,:,:)
                  ptab(ji,ipj  ,:,:) = psgn  * ptab(ijt,ipj-3,:,:)
               END DO
               ptab(1,ipj,:,:) = psgn  * ptab(3,ipj-3,:,:) 
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj-1,:,:) = psgn  * ptab(iju,ipj-2,:,:)
                  ptab(ji,ipj  ,:,:) = psgn  * ptab(iju,ipj-3,:,:)
               END DO
               ptab(   1  ,ipj,:,:) = psgn  * ptab(    2   ,ipj-3,:,:)
               ptab(jpiglo,ipj,:,:) = psgn  * ptab(jpiglo-1,ipj-3,:,:) 
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj,:,:) = psgn  * ptab(ijt,ipj-1,:,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj,:,:) = psgn  * ptab(iju,ipj-1,:,:)
               END DO
               ptab(jpiglo,ipj,:,:) = psgn * ptab(jpiglo-2,ipj-1,:,:)
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj,:,:) = psgn  * ptab(ijt,ipj-2,:,:)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipjm1,:,:) = psgn  * ptab(ijt,ipjm1,:,:)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj  ,:,:) = psgn  * ptab(iju,ipj-2,:,:)
               END DO
               ptab(jpiglo,ipj,:,:) = psgn   * ptab(jpiglo-2,ipj-2,:,:)
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipjm1,:,:) = psgn  * ptab(iju,ipjm1,:,:)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(:, 1 ,:,:) = 0._wp
               ptab(:,ipj,:,:) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(:,ipj,:,:) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_4d


   SUBROUTINE lbc_nfd_4d_ptr( ptab, cd_nat, psgn, kfld )
      INTEGER          , INTENT(in   ) ::   kfld        ! number of pt3d arrays
      TYPE(PTR_4D),INTENT(inout)::ptab(:)                             ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      !
      INTEGER  ::    ji,  jj,  jk,  jl, jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl,     ipf   ! dimension of the input array
      INTEGER  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab(1)%pt4d,3)   ! 3rd dimension
      ipl = SIZE(ptab(1)%pt4d,4)   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      !
      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ipj = nlcj       ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      END SELECT
      ipjm1 = ipj-1

      !
      DO jf = 1, ipf                      ! Loop on the number of arrays to be treated
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat(jf)  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt4d(ji,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipj-2,:,:)
               END DO
               ptab(jf)%pt4d(1,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(3,ipj-2,:,:)
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt4d(ji,ipjm1,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipjm1,:,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt4d(ji,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipj-2,:,:)
               END DO
               ptab(jf)%pt4d(   1  ,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(    2   ,ipj-2,:,:)
               ptab(jf)%pt4d(jpiglo,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(jpiglo-1,ipj-2,:,:) 
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt4d(ji,ipjm1,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipjm1,:,:)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(jf)%pt4d(ji,ipj-1,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipj-2,:,:)
                  ptab(jf)%pt4d(ji,ipj  ,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipj-3,:,:)
               END DO
               ptab(jf)%pt4d(1,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(3,ipj-3,:,:) 
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(jf)%pt4d(ji,ipj-1,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipj-2,:,:)
                  ptab(jf)%pt4d(ji,ipj  ,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipj-3,:,:)
               END DO
               ptab(jf)%pt4d(   1  ,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(    2   ,ipj-3,:,:)
               ptab(jf)%pt4d(jpiglo,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(jpiglo-1,ipj-3,:,:) 
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat(jf)  )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt4d(ji,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipj-1,:,:)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt4d(ji,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipj-1,:,:)
               END DO
               ptab(jf)%pt4d(jpiglo,ipj,:,:) = psgn(jf) * ptab(jf)%pt4d(jpiglo-2,ipj-1,:,:)
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt4d(ji,ipj,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipj-2,:,:)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(jf)%pt4d(ji,ipjm1,:,:) = psgn(jf)  * ptab(jf)%pt4d(ijt,ipjm1,:,:)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt4d(ji,ipj  ,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipj-2,:,:)
               END DO
               ptab(jf)%pt4d(jpiglo,ipj,:,:) = psgn(jf)   * ptab(jf)%pt4d(jpiglo-2,ipj-2,:,:)
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(jf)%pt4d(ji,ipjm1,:,:) = psgn(jf)  * ptab(jf)%pt4d(iju,ipjm1,:,:)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_nat(jf) )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               ptab(jf)%pt4d(:, 1 ,:,:) = 0._wp
               ptab(jf)%pt4d(:,ipj,:,:) = 0._wp
            CASE ( 'F' )                               ! F-point
               ptab(jf)%pt4d(:,ipj,:,:) = 0._wp
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_4d_ptr

   !
   !  lbc_nfd_nogather routines
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_nogather_2d( ptab, ptab2, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   lateral boundary condition : North fold treatment
      !!                without allgather exchanges. 
      !!
      !!----------------------------------------------------------------------
      REAL(wp),INTENT(inout)::ptab(:,:)                             ! array or pointer of arrays on which the boundary condition is applied
      REAL(wp),INTENT(inout)::ptab2(:,:,:,:)                            ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::    ji,  jj,   jk,     jl,   jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj,  ipk,    ipl,  ipf        ! dimension of the input array
      INTEGER  ::   ijt, iju, ijpj, ijpjp1, ijta, ijua, jia, startloop, endloop
      LOGICAL  ::   l_fast_exchanges
      !!----------------------------------------------------------------------
      ipj = SIZE(ptab2,2)  ! 2nd dimension of input array
      ipk = 1   ! 3rd dimension of output array
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      ! Security check for further developments
      IF ( ipf > 1 ) CALL ctl_stop( 'STOP', 'lbc_nfd_nogather: multiple fields not allowed. Revise implementation...' )
      !
      ijpj   = 1    ! index of first modified line 
      ijpjp1 = 2    ! index + 1
      
      ! 2nd dimension determines exchange speed
      IF (ipj == 1 ) THEN
        l_fast_exchanges = .TRUE.
      ELSE
        l_fast_exchanges = .FALSE.
      ENDIF
      !
      DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3, 4 )                       ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat )
            !
            CASE ( 'T' , 'W' )                         ! T-, W-point
               IF ( nimpp /= 1 ) THEN   ;   startloop = 1
               ELSE                     ;   startloop = 2
               ENDIF
               !
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(ji,nlcj) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF( nimpp == 1 ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(1,nlcj) = psgn * ptab(3,nlcj-2)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, nlci
                           ijt  = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                           jia  = ji + nimpp - 1
                           ijta = jpiglo - jia + 2
                           IF( ijta >= startloop+nimpp-1 .AND. ijta < jia ) THEN
                              ptab(ji,nlcj-1) = psgn * ptab(ijta-nimpp+1,nlcj-1)
                           ELSE
                              ptab(ji,nlcj-1) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                           ENDIF
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF

            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj) = psgn * ptab(2,nlcj-2)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(nlci,nlcj) = psgn * ptab(nlci-1,nlcj-2)
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2 ) THEN
                     startloop = 1
                     ELSEIF( ( nimpp+nlci-1 >= jpiglo/2 ) .AND. ( nimpp < jpiglo/2 ) ) THEN
                     startloop = jpiglo/2 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        jia = ji + nimpp - 1
                        ijua = jpiglo - jia + 1
                        IF( ijua >= startloop+nimpp-1 .AND. ijua < jia ) THEN
                           ptab(ji,nlcj-1) = psgn * ptab(ijua-nimpp+1,nlcj-1)
                        ELSE
                           ptab(ji,nlcj-1) = psgn * ptab2(iju,ijpjp1,jk,jl)
                        ENDIF
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               IF( nimpp /= 1 ) THEN
                 startloop = 1
               ELSE
                 startloop = 2
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                        ptab(ji,nlcj-1) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(ji,nlcj) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj) = psgn * ptab(3,nlcj-3)
               ENDIF
            CASE ( 'F' )                                     ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = 1, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(ji,nlcj-1) = psgn * ptab2(iju,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj) = psgn * ptab(2,nlcj-3)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(1,nlcj-1) = psgn * ptab(2,nlcj-2)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(nlci,nlcj) = psgn * ptab(nlci-1,nlcj-3)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(nlci,nlcj-1) = psgn * ptab(nlci-1,nlcj-2)
               ENDIF
               !
            END SELECT
            !
         CASE ( 5, 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'W' )                               ! T-, W-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               !
            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(ji,nlcj) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(nlci,nlcj) = psgn * ptab(nlci-2,nlcj-1)
                  END DO; END DO
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO

               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(ji,nlcj-1) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'F' )                               ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(ji,nlcj ) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(nlci,nlcj) = psgn * ptab(nlci-2,nlcj-2)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, endloop
                           iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                           ptab(ji,nlcj-1) = psgn * ptab2(iju,ijpjp1,jk,jl)
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF
               !
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            WRITE(*,*) 'lbc_nfd_nogather_generic: You should not have seen this print! error?', npolj
            !
         END SELECT     !  npolj
         !
      END DO            ! End jf loop
   END SUBROUTINE lbc_nfd_nogather_2d

   SUBROUTINE lbc_nfd_nogather_2d_ptr( ptab, ptab2, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   lateral boundary condition : North fold treatment
      !!                without allgather exchanges. 
      !!
      !!----------------------------------------------------------------------
      TYPE(PTR_2D),INTENT(inout)::ptab(:)                             ! array or pointer of arrays on which the boundary condition is applied
      TYPE(PTR_4D),INTENT(inout)::ptab2(:)                            ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::    ji,  jj,   jk,     jl,   jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj,  ipk,    ipl,  ipf        ! dimension of the input array
      INTEGER  ::   ijt, iju, ijpj, ijpjp1, ijta, ijua, jia, startloop, endloop
      LOGICAL  ::   l_fast_exchanges
      !!----------------------------------------------------------------------
      ipj = SIZE(ptab2(1)%pt4d,2)  ! 2nd dimension of input array
      ipk = 1   ! 3rd dimension of output array
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      ! Security check for further developments
      IF ( ipf > 1 ) CALL ctl_stop( 'STOP', 'lbc_nfd_nogather: multiple fields not allowed. Revise implementation...' )
      !
      ijpj   = 1    ! index of first modified line 
      ijpjp1 = 2    ! index + 1
      
      ! 2nd dimension determines exchange speed
      IF (ipj == 1 ) THEN
        l_fast_exchanges = .TRUE.
      ELSE
        l_fast_exchanges = .FALSE.
      ENDIF
      !
      DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3, 4 )                       ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat(jf) )
            !
            CASE ( 'T' , 'W' )                         ! T-, W-point
               IF ( nimpp /= 1 ) THEN   ;   startloop = 1
               ELSE                     ;   startloop = 2
               ENDIF
               !
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF( nimpp == 1 ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(jf)%pt2d(1,nlcj) = psgn(jf) * ptab(jf)%pt2d(3,nlcj-2)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, nlci
                           ijt  = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                           jia  = ji + nimpp - 1
                           ijta = jpiglo - jia + 2
                           IF( ijta >= startloop+nimpp-1 .AND. ijta < jia ) THEN
                              ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab(jf)%pt2d(ijta-nimpp+1,nlcj-1)
                           ELSE
                              ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpjp1,jk,jl)
                           ENDIF
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF

            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(jf)%pt2d(1,nlcj) = psgn(jf) * ptab(jf)%pt2d(2,nlcj-2)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(jf)%pt2d(nlci,nlcj) = psgn(jf) * ptab(jf)%pt2d(nlci-1,nlcj-2)
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2 ) THEN
                     startloop = 1
                     ELSEIF( ( nimpp+nlci-1 >= jpiglo/2 ) .AND. ( nimpp < jpiglo/2 ) ) THEN
                     startloop = jpiglo/2 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        jia = ji + nimpp - 1
                        ijua = jpiglo - jia + 1
                        IF( ijua >= startloop+nimpp-1 .AND. ijua < jia ) THEN
                           ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab(jf)%pt2d(ijua-nimpp+1,nlcj-1)
                        ELSE
                           ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpjp1,jk,jl)
                        ENDIF
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               IF( nimpp /= 1 ) THEN
                 startloop = 1
               ELSE
                 startloop = 2
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                        ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(jf)%pt2d(1,nlcj) = psgn(jf) * ptab(jf)%pt2d(3,nlcj-3)
               ENDIF
            CASE ( 'F' )                                     ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = 1, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(jf)%pt2d(1,nlcj) = psgn(jf) * ptab(jf)%pt2d(2,nlcj-3)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(jf)%pt2d(1,nlcj-1) = psgn(jf) * ptab(jf)%pt2d(2,nlcj-2)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(jf)%pt2d(nlci,nlcj) = psgn(jf) * ptab(jf)%pt2d(nlci-1,nlcj-3)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(jf)%pt2d(nlci,nlcj-1) = psgn(jf) * ptab(jf)%pt2d(nlci-1,nlcj-2)
               ENDIF
               !
            END SELECT
            !
         CASE ( 5, 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat(jf) )
            CASE ( 'T' , 'W' )                               ! T-, W-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               !
            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(jf)%pt2d(nlci,nlcj) = psgn(jf) * ptab(jf)%pt2d(nlci-2,nlcj-1)
                  END DO; END DO
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt2d(ji,nlcj) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO

               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'F' )                               ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(jf)%pt2d(ji,nlcj ) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(jf)%pt2d(nlci,nlcj) = psgn(jf) * ptab(jf)%pt2d(nlci-2,nlcj-2)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, endloop
                           iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                           ptab(jf)%pt2d(ji,nlcj-1) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpjp1,jk,jl)
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF
               !
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            WRITE(*,*) 'lbc_nfd_nogather_generic: You should not have seen this print! error?', npolj
            !
         END SELECT     !  npolj
         !
      END DO            ! End jf loop
   END SUBROUTINE lbc_nfd_nogather_2d_ptr
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_nogather_3d( ptab, ptab2, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   lateral boundary condition : North fold treatment
      !!                without allgather exchanges. 
      !!
      !!----------------------------------------------------------------------
      REAL(wp),INTENT(inout)::ptab(:,:,:)                             ! array or pointer of arrays on which the boundary condition is applied
      REAL(wp),INTENT(inout)::ptab2(:,:,:,:)                            ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::    ji,  jj,   jk,     jl,   jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj,  ipk,    ipl,  ipf        ! dimension of the input array
      INTEGER  ::   ijt, iju, ijpj, ijpjp1, ijta, ijua, jia, startloop, endloop
      LOGICAL  ::   l_fast_exchanges
      !!----------------------------------------------------------------------
      ipj = SIZE(ptab2,2)  ! 2nd dimension of input array
      ipk = SIZE(ptab,3)   ! 3rd dimension of output array
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      ! Security check for further developments
      IF ( ipf > 1 ) CALL ctl_stop( 'STOP', 'lbc_nfd_nogather: multiple fields not allowed. Revise implementation...' )
      !
      ijpj   = 1    ! index of first modified line 
      ijpjp1 = 2    ! index + 1
      
      ! 2nd dimension determines exchange speed
      IF (ipj == 1 ) THEN
        l_fast_exchanges = .TRUE.
      ELSE
        l_fast_exchanges = .FALSE.
      ENDIF
      !
      DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3, 4 )                       ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat )
            !
            CASE ( 'T' , 'W' )                         ! T-, W-point
               IF ( nimpp /= 1 ) THEN   ;   startloop = 1
               ELSE                     ;   startloop = 2
               ENDIF
               !
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(ji,nlcj,jk) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF( nimpp == 1 ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(1,nlcj,jk) = psgn * ptab(3,nlcj-2,jk)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, nlci
                           ijt  = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                           jia  = ji + nimpp - 1
                           ijta = jpiglo - jia + 2
                           IF( ijta >= startloop+nimpp-1 .AND. ijta < jia ) THEN
                              ptab(ji,nlcj-1,jk) = psgn * ptab(ijta-nimpp+1,nlcj-1,jk)
                           ELSE
                              ptab(ji,nlcj-1,jk) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                           ENDIF
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF

            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj,:) = psgn * ptab(2,nlcj-2,:)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(nlci,nlcj,:) = psgn * ptab(nlci-1,nlcj-2,:)
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2 ) THEN
                     startloop = 1
                     ELSEIF( ( nimpp+nlci-1 >= jpiglo/2 ) .AND. ( nimpp < jpiglo/2 ) ) THEN
                     startloop = jpiglo/2 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        jia = ji + nimpp - 1
                        ijua = jpiglo - jia + 1
                        IF( ijua >= startloop+nimpp-1 .AND. ijua < jia ) THEN
                           ptab(ji,nlcj-1,jk) = psgn * ptab(ijua-nimpp+1,nlcj-1,jk)
                        ELSE
                           ptab(ji,nlcj-1,jk) = psgn * ptab2(iju,ijpjp1,jk,jl)
                        ENDIF
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               IF( nimpp /= 1 ) THEN
                 startloop = 1
               ELSE
                 startloop = 2
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                        ptab(ji,nlcj-1,jk) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(ji,nlcj,jk) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj,:) = psgn * ptab(3,nlcj-3,:)
               ENDIF
            CASE ( 'F' )                                     ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = 1, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(ji,nlcj-1,jk) = psgn * ptab2(iju,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj,:) = psgn * ptab(2,nlcj-3,:)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(1,nlcj-1,:) = psgn * ptab(2,nlcj-2,:)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(nlci,nlcj,:) = psgn * ptab(nlci-1,nlcj-3,:)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(nlci,nlcj-1,:) = psgn * ptab(nlci-1,nlcj-2,:)
               ENDIF
               !
            END SELECT
            !
         CASE ( 5, 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'W' )                               ! T-, W-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               !
            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(ji,nlcj,jk) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(nlci,nlcj,jk) = psgn * ptab(nlci-2,nlcj-1,jk)
                  END DO; END DO
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO

               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(ji,nlcj-1,jk) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'F' )                               ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(ji,nlcj ,jk) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(nlci,nlcj,jk) = psgn * ptab(nlci-2,nlcj-2,jk)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, endloop
                           iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                           ptab(ji,nlcj-1,jk) = psgn * ptab2(iju,ijpjp1,jk,jl)
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF
               !
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            WRITE(*,*) 'lbc_nfd_nogather_generic: You should not have seen this print! error?', npolj
            !
         END SELECT     !  npolj
         !
      END DO            ! End jf loop
   END SUBROUTINE lbc_nfd_nogather_3d

   SUBROUTINE lbc_nfd_nogather_3d_ptr( ptab, ptab2, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   lateral boundary condition : North fold treatment
      !!                without allgather exchanges. 
      !!
      !!----------------------------------------------------------------------
      TYPE(PTR_3D),INTENT(inout)::ptab(:)                             ! array or pointer of arrays on which the boundary condition is applied
      TYPE(PTR_4D),INTENT(inout)::ptab2(:)                            ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::    ji,  jj,   jk,     jl,   jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj,  ipk,    ipl,  ipf        ! dimension of the input array
      INTEGER  ::   ijt, iju, ijpj, ijpjp1, ijta, ijua, jia, startloop, endloop
      LOGICAL  ::   l_fast_exchanges
      !!----------------------------------------------------------------------
      ipj = SIZE(ptab2(1)%pt4d,2)  ! 2nd dimension of input array
      ipk = SIZE(ptab(1)%pt3d,3)   ! 3rd dimension of output array
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      ! Security check for further developments
      IF ( ipf > 1 ) CALL ctl_stop( 'STOP', 'lbc_nfd_nogather: multiple fields not allowed. Revise implementation...' )
      !
      ijpj   = 1    ! index of first modified line 
      ijpjp1 = 2    ! index + 1
      
      ! 2nd dimension determines exchange speed
      IF (ipj == 1 ) THEN
        l_fast_exchanges = .TRUE.
      ELSE
        l_fast_exchanges = .FALSE.
      ENDIF
      !
      DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3, 4 )                       ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat(jf) )
            !
            CASE ( 'T' , 'W' )                         ! T-, W-point
               IF ( nimpp /= 1 ) THEN   ;   startloop = 1
               ELSE                     ;   startloop = 2
               ENDIF
               !
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF( nimpp == 1 ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(jf)%pt3d(1,nlcj,jk) = psgn(jf) * ptab(jf)%pt3d(3,nlcj-2,jk)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, nlci
                           ijt  = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                           jia  = ji + nimpp - 1
                           ijta = jpiglo - jia + 2
                           IF( ijta >= startloop+nimpp-1 .AND. ijta < jia ) THEN
                              ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab(jf)%pt3d(ijta-nimpp+1,nlcj-1,jk)
                           ELSE
                              ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpjp1,jk,jl)
                           ENDIF
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF

            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(jf)%pt3d(1,nlcj,:) = psgn(jf) * ptab(jf)%pt3d(2,nlcj-2,:)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(jf)%pt3d(nlci,nlcj,:) = psgn(jf) * ptab(jf)%pt3d(nlci-1,nlcj-2,:)
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2 ) THEN
                     startloop = 1
                     ELSEIF( ( nimpp+nlci-1 >= jpiglo/2 ) .AND. ( nimpp < jpiglo/2 ) ) THEN
                     startloop = jpiglo/2 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        jia = ji + nimpp - 1
                        ijua = jpiglo - jia + 1
                        IF( ijua >= startloop+nimpp-1 .AND. ijua < jia ) THEN
                           ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab(jf)%pt3d(ijua-nimpp+1,nlcj-1,jk)
                        ELSE
                           ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpjp1,jk,jl)
                        ENDIF
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               IF( nimpp /= 1 ) THEN
                 startloop = 1
               ELSE
                 startloop = 2
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                        ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(jf)%pt3d(1,nlcj,:) = psgn(jf) * ptab(jf)%pt3d(3,nlcj-3,:)
               ENDIF
            CASE ( 'F' )                                     ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = 1, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(jf)%pt3d(1,nlcj,:) = psgn(jf) * ptab(jf)%pt3d(2,nlcj-3,:)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(jf)%pt3d(1,nlcj-1,:) = psgn(jf) * ptab(jf)%pt3d(2,nlcj-2,:)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(jf)%pt3d(nlci,nlcj,:) = psgn(jf) * ptab(jf)%pt3d(nlci-1,nlcj-3,:)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(jf)%pt3d(nlci,nlcj-1,:) = psgn(jf) * ptab(jf)%pt3d(nlci-1,nlcj-2,:)
               ENDIF
               !
            END SELECT
            !
         CASE ( 5, 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat(jf) )
            CASE ( 'T' , 'W' )                               ! T-, W-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               !
            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(jf)%pt3d(nlci,nlcj,jk) = psgn(jf) * ptab(jf)%pt3d(nlci-2,nlcj-1,jk)
                  END DO; END DO
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(jf)%pt3d(ji,nlcj,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO

               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab2(jf)%pt4d(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'F' )                               ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(jf)%pt3d(ji,nlcj ,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(jf)%pt3d(nlci,nlcj,jk) = psgn(jf) * ptab(jf)%pt3d(nlci-2,nlcj-2,jk)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, endloop
                           iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                           ptab(jf)%pt3d(ji,nlcj-1,jk) = psgn(jf) * ptab2(jf)%pt4d(iju,ijpjp1,jk,jl)
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF
               !
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            WRITE(*,*) 'lbc_nfd_nogather_generic: You should not have seen this print! error?', npolj
            !
         END SELECT     !  npolj
         !
      END DO            ! End jf loop
   END SUBROUTINE lbc_nfd_nogather_3d_ptr
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE lbc_nfd_nogather_4d( ptab, ptab2, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   lateral boundary condition : North fold treatment
      !!                without allgather exchanges. 
      !!
      !!----------------------------------------------------------------------
      REAL(wp),INTENT(inout)::ptab(:,:,:,:)                             ! array or pointer of arrays on which the boundary condition is applied
      REAL(wp),INTENT(inout)::ptab2(:,:,:,:)                            ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::    ji,  jj,   jk,     jl,   jh,  jf   ! dummy loop indices
      INTEGER  ::   ipi, ipj,  ipk,    ipl,  ipf        ! dimension of the input array
      INTEGER  ::   ijt, iju, ijpj, ijpjp1, ijta, ijua, jia, startloop, endloop
      LOGICAL  ::   l_fast_exchanges
      !!----------------------------------------------------------------------
      ipj = SIZE(ptab2,2)  ! 2nd dimension of input array
      ipk = SIZE(ptab,3)   ! 3rd dimension of output array
      ipl = SIZE(ptab,4)   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      ! Security check for further developments
      IF ( ipf > 1 ) CALL ctl_stop( 'STOP', 'lbc_nfd_nogather: multiple fields not allowed. Revise implementation...' )
      !
      ijpj   = 1    ! index of first modified line 
      ijpjp1 = 2    ! index + 1
      
      ! 2nd dimension determines exchange speed
      IF (ipj == 1 ) THEN
        l_fast_exchanges = .TRUE.
      ELSE
        l_fast_exchanges = .FALSE.
      ENDIF
      !
      DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3, 4 )                       ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_nat )
            !
            CASE ( 'T' , 'W' )                         ! T-, W-point
               IF ( nimpp /= 1 ) THEN   ;   startloop = 1
               ELSE                     ;   startloop = 2
               ENDIF
               !
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF( nimpp == 1 ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(1,nlcj,jk,jl) = psgn * ptab(3,nlcj-2,jk,jl)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, nlci
                           ijt  = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                           jia  = ji + nimpp - 1
                           ijta = jpiglo - jia + 2
                           IF( ijta >= startloop+nimpp-1 .AND. ijta < jia ) THEN
                              ptab(ji,nlcj-1,jk,jl) = psgn * ptab(ijta-nimpp+1,nlcj-1,jk,jl)
                           ELSE
                              ptab(ji,nlcj-1,jk,jl) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                           ENDIF
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF

            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj,:,:) = psgn * ptab(2,nlcj-2,:,:)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(nlci,nlcj,:,:) = psgn * ptab(nlci-1,nlcj-2,:,:)
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2 ) THEN
                     startloop = 1
                     ELSEIF( ( nimpp+nlci-1 >= jpiglo/2 ) .AND. ( nimpp < jpiglo/2 ) ) THEN
                     startloop = jpiglo/2 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        jia = ji + nimpp - 1
                        ijua = jpiglo - jia + 1
                        IF( ijua >= startloop+nimpp-1 .AND. ijua < jia ) THEN
                           ptab(ji,nlcj-1,jk,jl) = psgn * ptab(ijua-nimpp+1,nlcj-1,jk,jl)
                        ELSE
                           ptab(ji,nlcj-1,jk,jl) = psgn * ptab2(iju,ijpjp1,jk,jl)
                        ENDIF
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               IF( nimpp /= 1 ) THEN
                 startloop = 1
               ELSE
                 startloop = 2
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                        ptab(ji,nlcj-1,jk,jl) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = startloop, nlci
                     ijt=jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 4
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj,:,:) = psgn * ptab(3,nlcj-3,:,:)
               ENDIF
            CASE ( 'F' )                                     ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               IF ( .NOT. l_fast_exchanges ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = 1, endloop
                        iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(ji,nlcj-1,jk,jl) = psgn * ptab2(iju,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF (nimpp .eq. 1) THEN
                  ptab(1,nlcj,:,:) = psgn * ptab(2,nlcj-3,:,:)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(1,nlcj-1,:,:) = psgn * ptab(2,nlcj-2,:,:)
               ENDIF
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  ptab(nlci,nlcj,:,:) = psgn * ptab(nlci-1,nlcj-3,:,:)
                  IF ( .NOT. l_fast_exchanges ) &
                     ptab(nlci,nlcj-1,:,:) = psgn * ptab(nlci-1,nlcj-2,:,:)
               ENDIF
               !
            END SELECT
            !
         CASE ( 5, 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_nat )
            CASE ( 'T' , 'W' )                               ! T-, W-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO
               !
            CASE ( 'U' )                                     ! U-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(nlci,nlcj,jk,jl) = psgn * ptab(nlci-2,nlcj-1,jk,jl)
                  END DO; END DO
               ENDIF
               !
            CASE ( 'V' )                                     ! V-point
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, nlci
                     ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                     ptab(ji,nlcj,jk,jl) = psgn * ptab2(ijt,ijpj,jk,jl)
                  END DO
               END DO; END DO

               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = nlci + 1
                  ENDIF
                  IF( startloop <= nlci ) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     DO ji = startloop, nlci
                        ijt = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 3
                        ptab(ji,nlcj-1,jk,jl) = psgn * ptab2(ijt,ijpjp1,jk,jl)
                     END DO
                  END DO; END DO
                  ENDIF
               ENDIF
               !
            CASE ( 'F' )                               ! F-point
               IF( nimpp + nlci - 1 /= jpiglo ) THEN
                  endloop = nlci
               ELSE
                  endloop = nlci - 1
               ENDIF
               DO jl = 1, ipl; DO jk = 1, ipk
                  DO ji = 1, endloop
                     iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                     ptab(ji,nlcj ,jk,jl) = psgn * ptab2(iju,ijpj,jk,jl)
                  END DO
               END DO; END DO
               IF((nimpp + nlci - 1) .eq. jpiglo) THEN
                  DO jl = 1, ipl; DO jk = 1, ipk
                     ptab(nlci,nlcj,jk,jl) = psgn * ptab(nlci-2,nlcj-2,jk,jl)
                  END DO; END DO
               ENDIF
               !
               IF ( .NOT. l_fast_exchanges ) THEN
                  IF( nimpp + nlci - 1 /= jpiglo ) THEN
                     endloop = nlci
                  ELSE
                     endloop = nlci - 1
                  ENDIF
                  IF( nimpp >= jpiglo/2+1 ) THEN
                     startloop = 1
                  ELSEIF( nimpp+nlci-1 >= jpiglo/2+1 .AND. nimpp < jpiglo/2+1 ) THEN
                     startloop = jpiglo/2+1 - nimpp + 1
                  ELSE
                     startloop = endloop + 1
                  ENDIF
                  IF( startloop <= endloop ) THEN
                     DO jl = 1, ipl; DO jk = 1, ipk
                        DO ji = startloop, endloop
                           iju = jpiglo - ji - nimpp - nfiimpp(isendto(1),jpnj) + 2
                           ptab(ji,nlcj-1,jk,jl) = psgn * ptab2(iju,ijpjp1,jk,jl)
                        END DO
                     END DO; END DO
                  ENDIF
               ENDIF
               !
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            WRITE(*,*) 'lbc_nfd_nogather_generic: You should not have seen this print! error?', npolj
            !
         END SELECT     !  npolj
         !
      END DO            ! End jf loop
   END SUBROUTINE lbc_nfd_nogather_4d
!#     define MULTI
!#     define ROUTINE_NFD           lbc_nfd_nogather_4d_ptr
!#     include "lbc_nfd_nogather_generic.h90"
!#     undef ROUTINE_NFD
!#     undef MULTI

   !!----------------------------------------------------------------------


   !!======================================================================
END MODULE lbcnfd
