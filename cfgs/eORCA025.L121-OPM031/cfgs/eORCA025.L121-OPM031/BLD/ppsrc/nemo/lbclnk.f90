










MODULE lbclnk
   !!======================================================================
   !!                       ***  MODULE  lbclnk  ***
   !! NEMO        : lateral boundary conditions
   !!=====================================================================
   !! History :  OPA  ! 1997-06  (G. Madec)  Original code
   !!   NEMO     1.0  ! 2002-09  (G. Madec)  F90: Free form and module
   !!            3.2  ! 2009-03  (R. Benshila)  External north fold treatment  
   !!            3.5  ! 2012     (S.Mocavero, I. Epicoco)  optimization of BDY comm. via lbc_bdy_lnk and lbc_obc_lnk
   !!            3.4  ! 2012-12  (R. Bourdalle-Badie, G. Reffray)  add a C1D case  
   !!            3.6  ! 2015-06  (O. Tintó and M. Castrillo)  add lbc_lnk_multi  
   !!            4.0  ! 2017-03  (G. Madec) automatique allocation of array size (use with any 3rd dim size)
   !!             -   ! 2017-04  (G. Madec) remove duplicated routines (lbc_lnk_2d_9, lbc_lnk_2d_multiple, lbc_lnk_3d_gather)
   !!             -   ! 2017-05  (G. Madec) create generic.h90 files to generate all lbc and north fold routines
   !!----------------------------------------------------------------------
   !!           define the generic interfaces of lib_mpp routines
   !!----------------------------------------------------------------------
   !!   lbc_lnk       : generic interface for mpp_lnk_3d and mpp_lnk_2d routines defined in lib_mpp
   !!   lbc_bdy_lnk   : generic interface for mpp_lnk_bdy_2d and mpp_lnk_bdy_3d routines defined in lib_mpp
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE lib_mpp        ! distributed memory computing library
   USE lbcnfd         ! north fold
   USE in_out_manager ! I/O manager

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_lnk
      MODULE PROCEDURE   mpp_lnk_2d      , mpp_lnk_3d      , mpp_lnk_4d
   END INTERFACE
   INTERFACE lbc_lnk_ptr
      MODULE PROCEDURE   mpp_lnk_2d_ptr  , mpp_lnk_3d_ptr  , mpp_lnk_4d_ptr
   END INTERFACE
   INTERFACE lbc_lnk_multi
      MODULE PROCEDURE   lbc_lnk_2d_multi, lbc_lnk_3d_multi, lbc_lnk_4d_multi
   END INTERFACE
   !
   INTERFACE lbc_lnk_icb
      MODULE PROCEDURE mpp_lnk_2d_icb
   END INTERFACE

   INTERFACE mpp_nfd
      MODULE PROCEDURE   mpp_nfd_2d    , mpp_nfd_3d    , mpp_nfd_4d
      MODULE PROCEDURE   mpp_nfd_2d_ptr, mpp_nfd_3d_ptr, mpp_nfd_4d_ptr
   END INTERFACE

   PUBLIC   lbc_lnk       ! ocean/ice lateral boundary conditions
   PUBLIC   lbc_lnk_multi ! modified ocean/ice lateral boundary conditions
   PUBLIC   lbc_lnk_icb   ! iceberg lateral boundary conditions

!$AGRIF_DO_NOT_TREAT
   INCLUDE 'mpif.h'
!$AGRIF_END_DO_NOT_TREAT

   INTEGER, PUBLIC, PARAMETER ::   jpfillnothing = 1
   INTEGER, PUBLIC, PARAMETER ::   jpfillcst     = 2
   INTEGER, PUBLIC, PARAMETER ::   jpfillcopy    = 3
   INTEGER, PUBLIC, PARAMETER ::   jpfillperio   = 4
   INTEGER, PUBLIC, PARAMETER ::   jpfillmpi     = 5

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lbclnk.F90 13350 2020-07-28 12:28:29Z smueller $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***   load_ptr_(2,3,4)d   ***
   !!
   !!   * Dummy Argument :
   !!       in    ==>   ptab       ! array to be loaded (2D, 3D or 4D)
   !!                   cd_nat     ! nature of pt2d array grid-points
   !!                   psgn       ! sign used across the north fold boundary
   !!       inout <=>   ptab_ptr   ! array of 2D, 3D or 4D pointers
   !!                   cdna_ptr   ! nature of ptab array grid-points
   !!                   psgn_ptr   ! sign used across the north fold boundary
   !!                   kfld       ! number of elements that has been attributed
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!                  ***   lbc_lnk_(2,3,4)d_multi   ***
   !!                     ***   load_ptr_(2,3,4)d   ***
   !!
   !!   * Argument : dummy argument use in lbc_lnk_multi_... routines
   !!
   !!----------------------------------------------------------------------


   SUBROUTINE lbc_lnk_2d_multi( cdname                                                                               &
      &                    , pt1 , cdna1 , psgn1 , pt2 , cdna2 , psgn2 , pt3 , cdna3 , psgn3 , pt4 , cdna4 , psgn4   &
      &                    , pt5 , cdna5 , psgn5 , pt6 , cdna6 , psgn6 , pt7 , cdna7 , psgn7 , pt8 , cdna8 , psgn8   &
      &                    , pt9 , cdna9 , psgn9 , pt10, cdna10, psgn10, pt11, cdna11, psgn11, pt12, cdna12, psgn12  &
      &                    , pt13, cdna13, psgn13, pt14, cdna14, psgn14, pt15, cdna15, psgn15, pt16, cdna16, psgn16  &
      &                    , kfillmode, pfillval, lsend, lrecv, ihlcom )
      !!---------------------------------------------------------------------
      CHARACTER(len=*)     ,                   INTENT(in   ) ::   cdname  ! name of the calling subroutine
      REAL(wp), DIMENSION(:,:)            , TARGET, INTENT(inout) ::   pt1     ! arrays on which the lbc is applied
      REAL(wp), DIMENSION(:,:)  , OPTIONAL, TARGET, INTENT(inout) ::   pt2   , pt3   , pt4   , pt5   , pt6   , pt7   , pt8   , pt9  , &
         &                                                        pt10  , pt11  , pt12  , pt13  , pt14  , pt15  , pt16
      CHARACTER(len=1)                       , INTENT(in   ) ::   cdna1   ! nature of pt2D. array grid-points
      CHARACTER(len=1)     , OPTIONAL        , INTENT(in   ) ::   cdna2 , cdna3 , cdna4 , cdna5 , cdna6 , cdna7 , cdna8 , cdna9, &
         &                                                        cdna10, cdna11, cdna12, cdna13, cdna14, cdna15, cdna16
      REAL(wp)                               , INTENT(in   ) ::   psgn1   ! sign used across the north fold
      REAL(wp)             , OPTIONAL        , INTENT(in   ) ::   psgn2 , psgn3 , psgn4 , psgn5 , psgn6 , psgn7 , psgn8 , psgn9, &
         &                                                        psgn10, psgn11, psgn12, psgn13, psgn14, psgn15, psgn16
      INTEGER              , OPTIONAL        , INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp)             , OPTIONAL        , INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4), OPTIONAL        , INTENT(in   ) ::   lsend, lrecv   ! indicate how communications are to be carried out
      INTEGER              , OPTIONAL        , INTENT(in   ) ::   ihlcom         ! number of ranks and rows to be communicated
      !!
      INTEGER                          ::   kfld        ! number of elements that will be attributed
      TYPE(PTR_2D)         , DIMENSION(16) ::   ptab_ptr    ! pointer array
      CHARACTER(len=1) , DIMENSION(16) ::   cdna_ptr    ! nature of ptab_ptr grid-points
      REAL(wp)         , DIMENSION(16) ::   psgn_ptr    ! sign used across the north fold boundary
      !!---------------------------------------------------------------------
      !
      kfld = 0          ! initial array of pointer size
      !
      !                 ! Load the first array
      CALL load_ptr_2d( pt1, cdna1, psgn1, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !
      !                 ! Look if more arrays are added
      IF( PRESENT(psgn2 ) )   CALL load_ptr_2d( pt2 , cdna2 , psgn2 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn3 ) )   CALL load_ptr_2d( pt3 , cdna3 , psgn3 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn4 ) )   CALL load_ptr_2d( pt4 , cdna4 , psgn4 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn5 ) )   CALL load_ptr_2d( pt5 , cdna5 , psgn5 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn6 ) )   CALL load_ptr_2d( pt6 , cdna6 , psgn6 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn7 ) )   CALL load_ptr_2d( pt7 , cdna7 , psgn7 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn8 ) )   CALL load_ptr_2d( pt8 , cdna8 , psgn8 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn9 ) )   CALL load_ptr_2d( pt9 , cdna9 , psgn9 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn10) )   CALL load_ptr_2d( pt10, cdna10, psgn10, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn11) )   CALL load_ptr_2d( pt11, cdna11, psgn11, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn12) )   CALL load_ptr_2d( pt12, cdna12, psgn12, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn13) )   CALL load_ptr_2d( pt13, cdna13, psgn13, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn14) )   CALL load_ptr_2d( pt14, cdna14, psgn14, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn15) )   CALL load_ptr_2d( pt15, cdna15, psgn15, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn16) )   CALL load_ptr_2d( pt16, cdna16, psgn16, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !
      CALL lbc_lnk_ptr( cdname, ptab_ptr, cdna_ptr, psgn_ptr, kfld, kfillmode, pfillval, lsend, lrecv, ihlcom )
      !
   END SUBROUTINE lbc_lnk_2d_multi


   SUBROUTINE load_ptr_2d( ptab, cdna, psgn, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:)   , TARGET, INTENT(inout) ::   ptab       ! arrays on which the lbc is applied
      CHARACTER(len=1)              , INTENT(in   ) ::   cdna       ! nature of pt2d array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn       ! sign used across the north fold boundary
      TYPE(PTR_2D)        , DIMENSION(:), INTENT(inout) ::   ptab_ptr   ! array of pointers
      CHARACTER(len=1), DIMENSION(:), INTENT(inout) ::   cdna_ptr   ! nature of pt2d_array array grid-points
      REAL(wp)        , DIMENSION(:), INTENT(inout) ::   psgn_ptr   ! sign used across the north fold boundary
      INTEGER                       , INTENT(inout) ::   kfld       ! number of elements that has been attributed
      !!---------------------------------------------------------------------
      !
      kfld                    =  kfld + 1
      ptab_ptr(kfld)%pt2d => ptab
      cdna_ptr(kfld)          =  cdna
      psgn_ptr(kfld)          =  psgn
      !
   END SUBROUTINE load_ptr_2d



   SUBROUTINE lbc_lnk_3d_multi( cdname                                                                               &
      &                    , pt1 , cdna1 , psgn1 , pt2 , cdna2 , psgn2 , pt3 , cdna3 , psgn3 , pt4 , cdna4 , psgn4   &
      &                    , pt5 , cdna5 , psgn5 , pt6 , cdna6 , psgn6 , pt7 , cdna7 , psgn7 , pt8 , cdna8 , psgn8   &
      &                    , pt9 , cdna9 , psgn9 , pt10, cdna10, psgn10, pt11, cdna11, psgn11, pt12, cdna12, psgn12  &
      &                    , pt13, cdna13, psgn13, pt14, cdna14, psgn14, pt15, cdna15, psgn15, pt16, cdna16, psgn16  &
      &                    , kfillmode, pfillval, lsend, lrecv, ihlcom )
      !!---------------------------------------------------------------------
      CHARACTER(len=*)     ,                   INTENT(in   ) ::   cdname  ! name of the calling subroutine
      REAL(wp), DIMENSION(:,:,:)            , TARGET, INTENT(inout) ::   pt1     ! arrays on which the lbc is applied
      REAL(wp), DIMENSION(:,:,:)  , OPTIONAL, TARGET, INTENT(inout) ::   pt2   , pt3   , pt4   , pt5   , pt6   , pt7   , pt8   , pt9  , &
         &                                                        pt10  , pt11  , pt12  , pt13  , pt14  , pt15  , pt16
      CHARACTER(len=1)                       , INTENT(in   ) ::   cdna1   ! nature of pt2D. array grid-points
      CHARACTER(len=1)     , OPTIONAL        , INTENT(in   ) ::   cdna2 , cdna3 , cdna4 , cdna5 , cdna6 , cdna7 , cdna8 , cdna9, &
         &                                                        cdna10, cdna11, cdna12, cdna13, cdna14, cdna15, cdna16
      REAL(wp)                               , INTENT(in   ) ::   psgn1   ! sign used across the north fold
      REAL(wp)             , OPTIONAL        , INTENT(in   ) ::   psgn2 , psgn3 , psgn4 , psgn5 , psgn6 , psgn7 , psgn8 , psgn9, &
         &                                                        psgn10, psgn11, psgn12, psgn13, psgn14, psgn15, psgn16
      INTEGER              , OPTIONAL        , INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp)             , OPTIONAL        , INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4), OPTIONAL        , INTENT(in   ) ::   lsend, lrecv   ! indicate how communications are to be carried out
      INTEGER              , OPTIONAL        , INTENT(in   ) ::   ihlcom         ! number of ranks and rows to be communicated
      !!
      INTEGER                          ::   kfld        ! number of elements that will be attributed
      TYPE(PTR_3D)         , DIMENSION(16) ::   ptab_ptr    ! pointer array
      CHARACTER(len=1) , DIMENSION(16) ::   cdna_ptr    ! nature of ptab_ptr grid-points
      REAL(wp)         , DIMENSION(16) ::   psgn_ptr    ! sign used across the north fold boundary
      !!---------------------------------------------------------------------
      !
      kfld = 0          ! initial array of pointer size
      !
      !                 ! Load the first array
      CALL load_ptr_3d( pt1, cdna1, psgn1, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !
      !                 ! Look if more arrays are added
      IF( PRESENT(psgn2 ) )   CALL load_ptr_3d( pt2 , cdna2 , psgn2 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn3 ) )   CALL load_ptr_3d( pt3 , cdna3 , psgn3 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn4 ) )   CALL load_ptr_3d( pt4 , cdna4 , psgn4 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn5 ) )   CALL load_ptr_3d( pt5 , cdna5 , psgn5 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn6 ) )   CALL load_ptr_3d( pt6 , cdna6 , psgn6 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn7 ) )   CALL load_ptr_3d( pt7 , cdna7 , psgn7 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn8 ) )   CALL load_ptr_3d( pt8 , cdna8 , psgn8 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn9 ) )   CALL load_ptr_3d( pt9 , cdna9 , psgn9 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn10) )   CALL load_ptr_3d( pt10, cdna10, psgn10, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn11) )   CALL load_ptr_3d( pt11, cdna11, psgn11, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn12) )   CALL load_ptr_3d( pt12, cdna12, psgn12, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn13) )   CALL load_ptr_3d( pt13, cdna13, psgn13, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn14) )   CALL load_ptr_3d( pt14, cdna14, psgn14, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn15) )   CALL load_ptr_3d( pt15, cdna15, psgn15, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn16) )   CALL load_ptr_3d( pt16, cdna16, psgn16, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !
      CALL lbc_lnk_ptr( cdname, ptab_ptr, cdna_ptr, psgn_ptr, kfld, kfillmode, pfillval, lsend, lrecv, ihlcom )
      !
   END SUBROUTINE lbc_lnk_3d_multi


   SUBROUTINE load_ptr_3d( ptab, cdna, psgn, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:)   , TARGET, INTENT(inout) ::   ptab       ! arrays on which the lbc is applied
      CHARACTER(len=1)              , INTENT(in   ) ::   cdna       ! nature of pt2d array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn       ! sign used across the north fold boundary
      TYPE(PTR_3D)        , DIMENSION(:), INTENT(inout) ::   ptab_ptr   ! array of pointers
      CHARACTER(len=1), DIMENSION(:), INTENT(inout) ::   cdna_ptr   ! nature of pt2d_array array grid-points
      REAL(wp)        , DIMENSION(:), INTENT(inout) ::   psgn_ptr   ! sign used across the north fold boundary
      INTEGER                       , INTENT(inout) ::   kfld       ! number of elements that has been attributed
      !!---------------------------------------------------------------------
      !
      kfld                    =  kfld + 1
      ptab_ptr(kfld)%pt3d => ptab
      cdna_ptr(kfld)          =  cdna
      psgn_ptr(kfld)          =  psgn
      !
   END SUBROUTINE load_ptr_3d



   SUBROUTINE lbc_lnk_4d_multi( cdname                                                                               &
      &                    , pt1 , cdna1 , psgn1 , pt2 , cdna2 , psgn2 , pt3 , cdna3 , psgn3 , pt4 , cdna4 , psgn4   &
      &                    , pt5 , cdna5 , psgn5 , pt6 , cdna6 , psgn6 , pt7 , cdna7 , psgn7 , pt8 , cdna8 , psgn8   &
      &                    , pt9 , cdna9 , psgn9 , pt10, cdna10, psgn10, pt11, cdna11, psgn11, pt12, cdna12, psgn12  &
      &                    , pt13, cdna13, psgn13, pt14, cdna14, psgn14, pt15, cdna15, psgn15, pt16, cdna16, psgn16  &
      &                    , kfillmode, pfillval, lsend, lrecv, ihlcom )
      !!---------------------------------------------------------------------
      CHARACTER(len=*)     ,                   INTENT(in   ) ::   cdname  ! name of the calling subroutine
      REAL(wp), DIMENSION(:,:,:,:)            , TARGET, INTENT(inout) ::   pt1     ! arrays on which the lbc is applied
      REAL(wp), DIMENSION(:,:,:,:)  , OPTIONAL, TARGET, INTENT(inout) ::   pt2   , pt3   , pt4   , pt5   , pt6   , pt7   , pt8   , pt9  , &
         &                                                        pt10  , pt11  , pt12  , pt13  , pt14  , pt15  , pt16
      CHARACTER(len=1)                       , INTENT(in   ) ::   cdna1   ! nature of pt2D. array grid-points
      CHARACTER(len=1)     , OPTIONAL        , INTENT(in   ) ::   cdna2 , cdna3 , cdna4 , cdna5 , cdna6 , cdna7 , cdna8 , cdna9, &
         &                                                        cdna10, cdna11, cdna12, cdna13, cdna14, cdna15, cdna16
      REAL(wp)                               , INTENT(in   ) ::   psgn1   ! sign used across the north fold
      REAL(wp)             , OPTIONAL        , INTENT(in   ) ::   psgn2 , psgn3 , psgn4 , psgn5 , psgn6 , psgn7 , psgn8 , psgn9, &
         &                                                        psgn10, psgn11, psgn12, psgn13, psgn14, psgn15, psgn16
      INTEGER              , OPTIONAL        , INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp)             , OPTIONAL        , INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4), OPTIONAL        , INTENT(in   ) ::   lsend, lrecv   ! indicate how communications are to be carried out
      INTEGER              , OPTIONAL        , INTENT(in   ) ::   ihlcom         ! number of ranks and rows to be communicated
      !!
      INTEGER                          ::   kfld        ! number of elements that will be attributed
      TYPE(PTR_4D)         , DIMENSION(16) ::   ptab_ptr    ! pointer array
      CHARACTER(len=1) , DIMENSION(16) ::   cdna_ptr    ! nature of ptab_ptr grid-points
      REAL(wp)         , DIMENSION(16) ::   psgn_ptr    ! sign used across the north fold boundary
      !!---------------------------------------------------------------------
      !
      kfld = 0          ! initial array of pointer size
      !
      !                 ! Load the first array
      CALL load_ptr_4d( pt1, cdna1, psgn1, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !
      !                 ! Look if more arrays are added
      IF( PRESENT(psgn2 ) )   CALL load_ptr_4d( pt2 , cdna2 , psgn2 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn3 ) )   CALL load_ptr_4d( pt3 , cdna3 , psgn3 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn4 ) )   CALL load_ptr_4d( pt4 , cdna4 , psgn4 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn5 ) )   CALL load_ptr_4d( pt5 , cdna5 , psgn5 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn6 ) )   CALL load_ptr_4d( pt6 , cdna6 , psgn6 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn7 ) )   CALL load_ptr_4d( pt7 , cdna7 , psgn7 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn8 ) )   CALL load_ptr_4d( pt8 , cdna8 , psgn8 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn9 ) )   CALL load_ptr_4d( pt9 , cdna9 , psgn9 , ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn10) )   CALL load_ptr_4d( pt10, cdna10, psgn10, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn11) )   CALL load_ptr_4d( pt11, cdna11, psgn11, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn12) )   CALL load_ptr_4d( pt12, cdna12, psgn12, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn13) )   CALL load_ptr_4d( pt13, cdna13, psgn13, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn14) )   CALL load_ptr_4d( pt14, cdna14, psgn14, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn15) )   CALL load_ptr_4d( pt15, cdna15, psgn15, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      IF( PRESENT(psgn16) )   CALL load_ptr_4d( pt16, cdna16, psgn16, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !
      CALL lbc_lnk_ptr( cdname, ptab_ptr, cdna_ptr, psgn_ptr, kfld, kfillmode, pfillval, lsend, lrecv, ihlcom )
      !
   END SUBROUTINE lbc_lnk_4d_multi


   SUBROUTINE load_ptr_4d( ptab, cdna, psgn, ptab_ptr, cdna_ptr, psgn_ptr, kfld )
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:,:)   , TARGET, INTENT(inout) ::   ptab       ! arrays on which the lbc is applied
      CHARACTER(len=1)              , INTENT(in   ) ::   cdna       ! nature of pt2d array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn       ! sign used across the north fold boundary
      TYPE(PTR_4D)        , DIMENSION(:), INTENT(inout) ::   ptab_ptr   ! array of pointers
      CHARACTER(len=1), DIMENSION(:), INTENT(inout) ::   cdna_ptr   ! nature of pt2d_array array grid-points
      REAL(wp)        , DIMENSION(:), INTENT(inout) ::   psgn_ptr   ! sign used across the north fold boundary
      INTEGER                       , INTENT(inout) ::   kfld       ! number of elements that has been attributed
      !!---------------------------------------------------------------------
      !
      kfld                    =  kfld + 1
      ptab_ptr(kfld)%pt4d => ptab
      cdna_ptr(kfld)          =  cdna
      psgn_ptr(kfld)          =  psgn
      !
   END SUBROUTINE load_ptr_4d


   !!----------------------------------------------------------------------
   !!                   ***  routine mpp_lnk_(2,3,4)d  ***
   !!
   !!   * Argument : dummy argument use in mpp_lnk_... routines
   !!                ptab      :   array or pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   optional, number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !

   SUBROUTINE mpp_lnk_2d( cdname, ptab, cd_nat, psgn      , kfillmode, pfillval, lsend, lrecv, ihlcom )
      REAL(wp)                    , INTENT(inout) ::   ptab(:,:)                                        ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=*)              , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      CHARACTER(len=1)              , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER ,             OPTIONAL, INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp),             OPTIONAL, INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4),OPTIONAL, INTENT(in   ) ::   lsend, lrecv  ! communication with other 4 proc
      INTEGER              ,OPTIONAL, INTENT(in   ) ::   ihlcom        ! number of ranks and rows to be communicated
      !
      INTEGER  ::    ji,  jj,  jk,  jl,  jf      ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf      ! dimension of the input array
      INTEGER  ::   isize, ishift, ishift2       ! local integers
      INTEGER  ::   ireq_we, ireq_ea, ireq_so, ireq_no     ! mpi_request id
      INTEGER  ::   ierr
      INTEGER  ::   ifill_we, ifill_ea, ifill_so, ifill_no
      INTEGER  ::   ihl                          ! number of ranks and rows to be communicated 
      REAL(wp) ::   zland
      INTEGER , DIMENSION(MPI_STATUS_SIZE)        ::   istat          ! for mpi_isend
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_we, zrcv_we, zsnd_ea, zrcv_ea   ! east -west  & west - east  halos
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_so, zrcv_so, zsnd_no, zrcv_no   ! north-south & south-north  halos
      LOGICAL  ::   llsend_we, llsend_ea, llsend_no, llsend_so       ! communication send
      LOGICAL  ::   llrecv_we, llrecv_ea, llrecv_no, llrecv_so       ! communication receive 
      LOGICAL  ::   lldo_nfd                                     ! do north pole folding
      !!----------------------------------------------------------------------
      !
      ! ----------------------------------------- !
      !     0. local variables initialization     !
      ! ----------------------------------------- !
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( PRESENT(ihlcom) ) THEN   ;   ihl = ihlcom
      ELSE                         ;   ihl = 1
      END IF
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ipk, ipl, ipf, ld_lbc = .TRUE. )
      !
      IF     ( PRESENT(lsend) .AND. PRESENT(lrecv) ) THEN
         llsend_we = lsend(1)   ;   llsend_ea = lsend(2)   ;   llsend_so = lsend(3)   ;   llsend_no = lsend(4)
         llrecv_we = lrecv(1)   ;   llrecv_ea = lrecv(2)   ;   llrecv_so = lrecv(3)   ;   llrecv_no = lrecv(4)
      ELSE IF( PRESENT(lsend) .OR.  PRESENT(lrecv) ) THEN
         WRITE(ctmp1,*) ' E R R O R : Routine ', cdname, '  is calling lbc_lnk with only one of the two arguments lsend or lrecv'
         WRITE(ctmp2,*) ' ========== '
         CALL ctl_stop( ' ', ctmp1, ctmp2, ' ' )
      ELSE   ! send and receive with every neighbour
         llsend_we = nbondi ==  1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_ea = nbondi == -1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_so = nbondj ==  1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llsend_no = nbondj == -1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llrecv_we = llsend_we   ;   llrecv_ea = llsend_ea   ;   llrecv_so = llsend_so   ;   llrecv_no = llsend_no
      END IF
         
         
      lldo_nfd = npolj /= 0                      ! keep for compatibility, should be defined in mppini

      zland = 0._wp                                     ! land filling value: zero by default
      IF( PRESENT( pfillval ) )   zland = pfillval      ! set land value

      ! define the method we will use to fill the halos in each direction
      IF(              llrecv_we ) THEN   ;   ifill_we = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_we = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_we = kfillmode
      ELSE                                ;   ifill_we = jpfillcst
      END IF
      !
      IF(              llrecv_ea ) THEN   ;   ifill_ea = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_ea = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_ea = kfillmode
      ELSE                                ;   ifill_ea = jpfillcst
      END IF
      !
      IF(              llrecv_so ) THEN   ;   ifill_so = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_so = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_so = kfillmode
      ELSE                                ;   ifill_so = jpfillcst
      END IF
      !
      IF(              llrecv_no ) THEN   ;   ifill_no = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_no = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_no = kfillmode
      ELSE                                ;   ifill_no = jpfillcst
      END IF
      !
      !
      ! -------------------------------------------------- !
      !     1. Do east and west MPI exchange if needed     !
      ! -------------------------------------------------- !
      !
      ! Must exchange the whole column (from 1 to jpj) to get the corners if we have no south/north neighbourg
      isize = ihl * jpj * ipk * ipl * ipf      
      !
      ! Allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_we )   ALLOCATE( zsnd_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llsend_ea )   ALLOCATE( zsnd_ea(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_we )   ALLOCATE( zrcv_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_ea )   ALLOCATE( zrcv_ea(ihl,jpj,ipk,ipl,ipf) )
      !
      IF( llsend_we ) THEN   ! copy western side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_we(ji,jj,jk,jl,jf) = ptab(ishift+ji,jj)   ! ihl + 1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF(llsend_ea  ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_ea(ji,jj,jk,jl,jf) = ptab(ishift+ji,jj)   ! jpi - 2*ihl + 1 -> jpi - ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the western/eastern side using local temporary arrays
      IF( llsend_we )   CALL mppsend( 1, zsnd_we(1,1,1,1,1), isize, nowe, ireq_we )
      IF( llsend_ea )   CALL mppsend( 2, zsnd_ea(1,1,1,1,1), isize, noea, ireq_ea )
      ! blocking receive of the western/eastern halo in local temporary arrays
      IF( llrecv_we )   CALL mpprecv( 2, zrcv_we(1,1,1,1,1), isize, nowe )
      IF( llrecv_ea )   CALL mpprecv( 1, zrcv_ea(1,1,1,1,1), isize, noea )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !
      ! ----------------------------------- !
      !     2. Fill east and west halos     !
      ! ----------------------------------- !
      !
      ! 2.1 fill weastern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from ji = 1 to ihl
      SELECT CASE ( ifill_we )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ji,jj) = zrcv_we(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ji,jj) = ptab(ishift2+ji,jj)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(ji,jj) = ptab(ihl+1,jj)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(ji,jj) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 2.2 fill eastern halo
      ! ---------------------
      ishift = jpi - ihl                ! fill halo from ji = jpi-ihl+1 to jpi 
      SELECT CASE ( ifill_ea )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj) = zrcv_ea(ji,jj,jk,jl,jf)   ! jpi - ihl + 1 -> jpi
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj) = ptab(ishift2+ji,jj)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj) = ptab(ishift,jj)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! ------------------------------- !
      !     3. north fold treatment     !
      ! ------------------------------- !
      !
      ! do it before south directions so concerned processes can do it without waiting for the comm with the sourthern neighbor
      !
      IF( lldo_nfd .AND. ifill_no /= jpfillnothing ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd( ptab, cd_nat, psgn  )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_nfd( ptab, cd_nat, psgn  )   ! for all northern procs.
         END SELECT
         !
         ifill_no = jpfillnothing  ! force to do nothing for the northern halo as we just done the north pole folding
         !
      ENDIF
      !
      ! ---------------------------------------------------- !
      !     4. Do north and south MPI exchange if needed     !
      ! ---------------------------------------------------- !
      !
      IF( llsend_so )   ALLOCATE( zsnd_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llsend_no )   ALLOCATE( zsnd_no(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_so )   ALLOCATE( zrcv_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_no )   ALLOCATE( zrcv_no(jpi,ihl,ipk,ipl,ipf) )
      !
      isize = jpi * ihl * ipk * ipl * ipf      

      ! allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_so ) THEN   ! copy sourhern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_so(ji,jj,jk,jl,jf) = ptab(ji,ishift+jj)   ! ihl+1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( llsend_no ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_no(ji,jj,jk,jl,jf) = ptab(ji,ishift+jj)   ! jpj-2*ihl+1 -> jpj-ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the southern/northern side
      IF( llsend_so )   CALL mppsend( 3, zsnd_so(1,1,1,1,1), isize, noso, ireq_so )
      IF( llsend_no )   CALL mppsend( 4, zsnd_no(1,1,1,1,1), isize, nono, ireq_no )
      ! blocking receive of the southern/northern halo
      IF( llrecv_so )   CALL mpprecv( 4, zrcv_so(1,1,1,1,1), isize, noso )
      IF( llrecv_no )   CALL mpprecv( 3, zrcv_no(1,1,1,1,1), isize, nono )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      ! ------------------------------------- !
      !     5. Fill south and north halos     !
      ! ------------------------------------- !
      !
      ! 5.1 fill southern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from jj = 1 to ihl
      SELECT CASE ( ifill_so )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,jj) = zrcv_so(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,jj) = ptab(ji,ishift2+jj)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
                  ptab(ji,jj) = ptab(ji,ihl+1)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi 
                  ptab(ji,jj) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 5.2 fill northern halo
      ! ----------------------
      ishift = jpj - ihl                ! fill halo from jj = jpj-ihl+1 to jpj 
      SELECT CASE ( ifill_no )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj) = zrcv_no(ji,jj,jk,jl,jf)   ! jpj-ihl+1 -> jpj
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj) = ptab(ji,ishift2+jj)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj) = ptab(ji,ishift)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! -------------------------------------------- !
      !     6. deallocate local temporary arrays     !
      ! -------------------------------------------- !
      !
      IF( llsend_we ) THEN
         CALL mpi_wait(ireq_we, istat, ierr )
         DEALLOCATE( zsnd_we )
      ENDIF
      IF( llsend_ea )  THEN
         CALL mpi_wait(ireq_ea, istat, ierr )
         DEALLOCATE( zsnd_ea )
      ENDIF
      IF( llsend_so ) THEN
         CALL mpi_wait(ireq_so, istat, ierr )
         DEALLOCATE( zsnd_so )
      ENDIF
      IF( llsend_no ) THEN
         CALL mpi_wait(ireq_no, istat, ierr )
         DEALLOCATE( zsnd_no )
      ENDIF
      !
      IF( llrecv_we )   DEALLOCATE( zrcv_we )
      IF( llrecv_ea )   DEALLOCATE( zrcv_ea )
      IF( llrecv_so )   DEALLOCATE( zrcv_so )
      IF( llrecv_no )   DEALLOCATE( zrcv_no )
      !
   END SUBROUTINE mpp_lnk_2d


   SUBROUTINE mpp_lnk_2d_ptr( cdname, ptab, cd_nat, psgn, kfld, kfillmode, pfillval, lsend, lrecv, ihlcom )
      INTEGER             , INTENT(in   ) ::   kfld        ! number of pt3d arrays
      TYPE(PTR_2D)                , INTENT(inout) ::   ptab(:)                                        ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=*)              , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      CHARACTER(len=1)              , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER ,             OPTIONAL, INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp),             OPTIONAL, INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4),OPTIONAL, INTENT(in   ) ::   lsend, lrecv  ! communication with other 4 proc
      INTEGER              ,OPTIONAL, INTENT(in   ) ::   ihlcom        ! number of ranks and rows to be communicated
      !
      INTEGER  ::    ji,  jj,  jk,  jl,  jf      ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf      ! dimension of the input array
      INTEGER  ::   isize, ishift, ishift2       ! local integers
      INTEGER  ::   ireq_we, ireq_ea, ireq_so, ireq_no     ! mpi_request id
      INTEGER  ::   ierr
      INTEGER  ::   ifill_we, ifill_ea, ifill_so, ifill_no
      INTEGER  ::   ihl                          ! number of ranks and rows to be communicated 
      REAL(wp) ::   zland
      INTEGER , DIMENSION(MPI_STATUS_SIZE)        ::   istat          ! for mpi_isend
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_we, zrcv_we, zsnd_ea, zrcv_ea   ! east -west  & west - east  halos
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_so, zrcv_so, zsnd_no, zrcv_no   ! north-south & south-north  halos
      LOGICAL  ::   llsend_we, llsend_ea, llsend_no, llsend_so       ! communication send
      LOGICAL  ::   llrecv_we, llrecv_ea, llrecv_no, llrecv_so       ! communication receive 
      LOGICAL  ::   lldo_nfd                                     ! do north pole folding
      !!----------------------------------------------------------------------
      !
      ! ----------------------------------------- !
      !     0. local variables initialization     !
      ! ----------------------------------------- !
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( PRESENT(ihlcom) ) THEN   ;   ihl = ihlcom
      ELSE                         ;   ihl = 1
      END IF
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ipk, ipl, ipf, ld_lbc = .TRUE. )
      !
      IF     ( PRESENT(lsend) .AND. PRESENT(lrecv) ) THEN
         llsend_we = lsend(1)   ;   llsend_ea = lsend(2)   ;   llsend_so = lsend(3)   ;   llsend_no = lsend(4)
         llrecv_we = lrecv(1)   ;   llrecv_ea = lrecv(2)   ;   llrecv_so = lrecv(3)   ;   llrecv_no = lrecv(4)
      ELSE IF( PRESENT(lsend) .OR.  PRESENT(lrecv) ) THEN
         WRITE(ctmp1,*) ' E R R O R : Routine ', cdname, '  is calling lbc_lnk with only one of the two arguments lsend or lrecv'
         WRITE(ctmp2,*) ' ========== '
         CALL ctl_stop( ' ', ctmp1, ctmp2, ' ' )
      ELSE   ! send and receive with every neighbour
         llsend_we = nbondi ==  1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_ea = nbondi == -1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_so = nbondj ==  1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llsend_no = nbondj == -1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llrecv_we = llsend_we   ;   llrecv_ea = llsend_ea   ;   llrecv_so = llsend_so   ;   llrecv_no = llsend_no
      END IF
         
         
      lldo_nfd = npolj /= 0                      ! keep for compatibility, should be defined in mppini

      zland = 0._wp                                     ! land filling value: zero by default
      IF( PRESENT( pfillval ) )   zland = pfillval      ! set land value

      ! define the method we will use to fill the halos in each direction
      IF(              llrecv_we ) THEN   ;   ifill_we = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_we = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_we = kfillmode
      ELSE                                ;   ifill_we = jpfillcst
      END IF
      !
      IF(              llrecv_ea ) THEN   ;   ifill_ea = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_ea = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_ea = kfillmode
      ELSE                                ;   ifill_ea = jpfillcst
      END IF
      !
      IF(              llrecv_so ) THEN   ;   ifill_so = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_so = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_so = kfillmode
      ELSE                                ;   ifill_so = jpfillcst
      END IF
      !
      IF(              llrecv_no ) THEN   ;   ifill_no = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_no = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_no = kfillmode
      ELSE                                ;   ifill_no = jpfillcst
      END IF
      !
      !
      ! -------------------------------------------------- !
      !     1. Do east and west MPI exchange if needed     !
      ! -------------------------------------------------- !
      !
      ! Must exchange the whole column (from 1 to jpj) to get the corners if we have no south/north neighbourg
      isize = ihl * jpj * ipk * ipl * ipf      
      !
      ! Allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_we )   ALLOCATE( zsnd_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llsend_ea )   ALLOCATE( zsnd_ea(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_we )   ALLOCATE( zrcv_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_ea )   ALLOCATE( zrcv_ea(ihl,jpj,ipk,ipl,ipf) )
      !
      IF( llsend_we ) THEN   ! copy western side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_we(ji,jj,jk,jl,jf) = ptab(jf)%pt2d(ishift+ji,jj)   ! ihl + 1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF(llsend_ea  ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_ea(ji,jj,jk,jl,jf) = ptab(jf)%pt2d(ishift+ji,jj)   ! jpi - 2*ihl + 1 -> jpi - ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the western/eastern side using local temporary arrays
      IF( llsend_we )   CALL mppsend( 1, zsnd_we(1,1,1,1,1), isize, nowe, ireq_we )
      IF( llsend_ea )   CALL mppsend( 2, zsnd_ea(1,1,1,1,1), isize, noea, ireq_ea )
      ! blocking receive of the western/eastern halo in local temporary arrays
      IF( llrecv_we )   CALL mpprecv( 2, zrcv_we(1,1,1,1,1), isize, nowe )
      IF( llrecv_ea )   CALL mpprecv( 1, zrcv_ea(1,1,1,1,1), isize, noea )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !
      ! ----------------------------------- !
      !     2. Fill east and west halos     !
      ! ----------------------------------- !
      !
      ! 2.1 fill weastern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from ji = 1 to ihl
      SELECT CASE ( ifill_we )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt2d(ji,jj) = zrcv_we(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt2d(ji,jj) = ptab(jf)%pt2d(ishift2+ji,jj)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(jf)%pt2d(ji,jj) = ptab(jf)%pt2d(ihl+1,jj)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(jf)%pt2d(ji,jj) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 2.2 fill eastern halo
      ! ---------------------
      ishift = jpi - ihl                ! fill halo from ji = jpi-ihl+1 to jpi 
      SELECT CASE ( ifill_ea )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt2d(ishift+ji,jj) = zrcv_ea(ji,jj,jk,jl,jf)   ! jpi - ihl + 1 -> jpi
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt2d(ishift+ji,jj) = ptab(jf)%pt2d(ishift2+ji,jj)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt2d(ishift+ji,jj) = ptab(jf)%pt2d(ishift,jj)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt2d(ishift+ji,jj) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! ------------------------------- !
      !     3. north fold treatment     !
      ! ------------------------------- !
      !
      ! do it before south directions so concerned processes can do it without waiting for the comm with the sourthern neighbor
      !
      IF( lldo_nfd .AND. ifill_no /= jpfillnothing ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd( ptab, cd_nat(:), psgn(:) ,ipf )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_nfd( ptab, cd_nat(:), psgn(:) ,ipf )   ! for all northern procs.
         END SELECT
         !
         ifill_no = jpfillnothing  ! force to do nothing for the northern halo as we just done the north pole folding
         !
      ENDIF
      !
      ! ---------------------------------------------------- !
      !     4. Do north and south MPI exchange if needed     !
      ! ---------------------------------------------------- !
      !
      IF( llsend_so )   ALLOCATE( zsnd_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llsend_no )   ALLOCATE( zsnd_no(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_so )   ALLOCATE( zrcv_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_no )   ALLOCATE( zrcv_no(jpi,ihl,ipk,ipl,ipf) )
      !
      isize = jpi * ihl * ipk * ipl * ipf      

      ! allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_so ) THEN   ! copy sourhern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_so(ji,jj,jk,jl,jf) = ptab(jf)%pt2d(ji,ishift+jj)   ! ihl+1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( llsend_no ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_no(ji,jj,jk,jl,jf) = ptab(jf)%pt2d(ji,ishift+jj)   ! jpj-2*ihl+1 -> jpj-ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the southern/northern side
      IF( llsend_so )   CALL mppsend( 3, zsnd_so(1,1,1,1,1), isize, noso, ireq_so )
      IF( llsend_no )   CALL mppsend( 4, zsnd_no(1,1,1,1,1), isize, nono, ireq_no )
      ! blocking receive of the southern/northern halo
      IF( llrecv_so )   CALL mpprecv( 4, zrcv_so(1,1,1,1,1), isize, noso )
      IF( llrecv_no )   CALL mpprecv( 3, zrcv_no(1,1,1,1,1), isize, nono )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      ! ------------------------------------- !
      !     5. Fill south and north halos     !
      ! ------------------------------------- !
      !
      ! 5.1 fill southern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from jj = 1 to ihl
      SELECT CASE ( ifill_so )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt2d(ji,jj) = zrcv_so(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt2d(ji,jj) = ptab(jf)%pt2d(ji,ishift2+jj)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
                  ptab(jf)%pt2d(ji,jj) = ptab(jf)%pt2d(ji,ihl+1)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi 
                  ptab(jf)%pt2d(ji,jj) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 5.2 fill northern halo
      ! ----------------------
      ishift = jpj - ihl                ! fill halo from jj = jpj-ihl+1 to jpj 
      SELECT CASE ( ifill_no )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt2d(ji,ishift+jj) = zrcv_no(ji,jj,jk,jl,jf)   ! jpj-ihl+1 -> jpj
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt2d(ji,ishift+jj) = ptab(jf)%pt2d(ji,ishift2+jj)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt2d(ji,ishift+jj) = ptab(jf)%pt2d(ji,ishift)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt2d(ji,ishift+jj) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! -------------------------------------------- !
      !     6. deallocate local temporary arrays     !
      ! -------------------------------------------- !
      !
      IF( llsend_we ) THEN
         CALL mpi_wait(ireq_we, istat, ierr )
         DEALLOCATE( zsnd_we )
      ENDIF
      IF( llsend_ea )  THEN
         CALL mpi_wait(ireq_ea, istat, ierr )
         DEALLOCATE( zsnd_ea )
      ENDIF
      IF( llsend_so ) THEN
         CALL mpi_wait(ireq_so, istat, ierr )
         DEALLOCATE( zsnd_so )
      ENDIF
      IF( llsend_no ) THEN
         CALL mpi_wait(ireq_no, istat, ierr )
         DEALLOCATE( zsnd_no )
      ENDIF
      !
      IF( llrecv_we )   DEALLOCATE( zrcv_we )
      IF( llrecv_ea )   DEALLOCATE( zrcv_ea )
      IF( llrecv_so )   DEALLOCATE( zrcv_so )
      IF( llrecv_no )   DEALLOCATE( zrcv_no )
      !
   END SUBROUTINE mpp_lnk_2d_ptr

   !
   !                       !==  3D array and array of 3D pointer  ==!
   !

   SUBROUTINE mpp_lnk_3d( cdname, ptab, cd_nat, psgn      , kfillmode, pfillval, lsend, lrecv, ihlcom )
      REAL(wp)                    , INTENT(inout) ::   ptab(:,:,:)                                        ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=*)              , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      CHARACTER(len=1)              , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER ,             OPTIONAL, INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp),             OPTIONAL, INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4),OPTIONAL, INTENT(in   ) ::   lsend, lrecv  ! communication with other 4 proc
      INTEGER              ,OPTIONAL, INTENT(in   ) ::   ihlcom        ! number of ranks and rows to be communicated
      !
      INTEGER  ::    ji,  jj,  jk,  jl,  jf      ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf      ! dimension of the input array
      INTEGER  ::   isize, ishift, ishift2       ! local integers
      INTEGER  ::   ireq_we, ireq_ea, ireq_so, ireq_no     ! mpi_request id
      INTEGER  ::   ierr
      INTEGER  ::   ifill_we, ifill_ea, ifill_so, ifill_no
      INTEGER  ::   ihl                          ! number of ranks and rows to be communicated 
      REAL(wp) ::   zland
      INTEGER , DIMENSION(MPI_STATUS_SIZE)        ::   istat          ! for mpi_isend
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_we, zrcv_we, zsnd_ea, zrcv_ea   ! east -west  & west - east  halos
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_so, zrcv_so, zsnd_no, zrcv_no   ! north-south & south-north  halos
      LOGICAL  ::   llsend_we, llsend_ea, llsend_no, llsend_so       ! communication send
      LOGICAL  ::   llrecv_we, llrecv_ea, llrecv_no, llrecv_so       ! communication receive 
      LOGICAL  ::   lldo_nfd                                     ! do north pole folding
      !!----------------------------------------------------------------------
      !
      ! ----------------------------------------- !
      !     0. local variables initialization     !
      ! ----------------------------------------- !
      !
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( PRESENT(ihlcom) ) THEN   ;   ihl = ihlcom
      ELSE                         ;   ihl = 1
      END IF
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ipk, ipl, ipf, ld_lbc = .TRUE. )
      !
      IF     ( PRESENT(lsend) .AND. PRESENT(lrecv) ) THEN
         llsend_we = lsend(1)   ;   llsend_ea = lsend(2)   ;   llsend_so = lsend(3)   ;   llsend_no = lsend(4)
         llrecv_we = lrecv(1)   ;   llrecv_ea = lrecv(2)   ;   llrecv_so = lrecv(3)   ;   llrecv_no = lrecv(4)
      ELSE IF( PRESENT(lsend) .OR.  PRESENT(lrecv) ) THEN
         WRITE(ctmp1,*) ' E R R O R : Routine ', cdname, '  is calling lbc_lnk with only one of the two arguments lsend or lrecv'
         WRITE(ctmp2,*) ' ========== '
         CALL ctl_stop( ' ', ctmp1, ctmp2, ' ' )
      ELSE   ! send and receive with every neighbour
         llsend_we = nbondi ==  1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_ea = nbondi == -1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_so = nbondj ==  1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llsend_no = nbondj == -1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llrecv_we = llsend_we   ;   llrecv_ea = llsend_ea   ;   llrecv_so = llsend_so   ;   llrecv_no = llsend_no
      END IF
         
         
      lldo_nfd = npolj /= 0                      ! keep for compatibility, should be defined in mppini

      zland = 0._wp                                     ! land filling value: zero by default
      IF( PRESENT( pfillval ) )   zland = pfillval      ! set land value

      ! define the method we will use to fill the halos in each direction
      IF(              llrecv_we ) THEN   ;   ifill_we = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_we = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_we = kfillmode
      ELSE                                ;   ifill_we = jpfillcst
      END IF
      !
      IF(              llrecv_ea ) THEN   ;   ifill_ea = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_ea = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_ea = kfillmode
      ELSE                                ;   ifill_ea = jpfillcst
      END IF
      !
      IF(              llrecv_so ) THEN   ;   ifill_so = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_so = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_so = kfillmode
      ELSE                                ;   ifill_so = jpfillcst
      END IF
      !
      IF(              llrecv_no ) THEN   ;   ifill_no = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_no = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_no = kfillmode
      ELSE                                ;   ifill_no = jpfillcst
      END IF
      !
      !
      ! -------------------------------------------------- !
      !     1. Do east and west MPI exchange if needed     !
      ! -------------------------------------------------- !
      !
      ! Must exchange the whole column (from 1 to jpj) to get the corners if we have no south/north neighbourg
      isize = ihl * jpj * ipk * ipl * ipf      
      !
      ! Allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_we )   ALLOCATE( zsnd_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llsend_ea )   ALLOCATE( zsnd_ea(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_we )   ALLOCATE( zrcv_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_ea )   ALLOCATE( zrcv_ea(ihl,jpj,ipk,ipl,ipf) )
      !
      IF( llsend_we ) THEN   ! copy western side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_we(ji,jj,jk,jl,jf) = ptab(ishift+ji,jj,jk)   ! ihl + 1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF(llsend_ea  ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_ea(ji,jj,jk,jl,jf) = ptab(ishift+ji,jj,jk)   ! jpi - 2*ihl + 1 -> jpi - ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the western/eastern side using local temporary arrays
      IF( llsend_we )   CALL mppsend( 1, zsnd_we(1,1,1,1,1), isize, nowe, ireq_we )
      IF( llsend_ea )   CALL mppsend( 2, zsnd_ea(1,1,1,1,1), isize, noea, ireq_ea )
      ! blocking receive of the western/eastern halo in local temporary arrays
      IF( llrecv_we )   CALL mpprecv( 2, zrcv_we(1,1,1,1,1), isize, nowe )
      IF( llrecv_ea )   CALL mpprecv( 1, zrcv_ea(1,1,1,1,1), isize, noea )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !
      ! ----------------------------------- !
      !     2. Fill east and west halos     !
      ! ----------------------------------- !
      !
      ! 2.1 fill weastern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from ji = 1 to ihl
      SELECT CASE ( ifill_we )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ji,jj,jk) = zrcv_we(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ji,jj,jk) = ptab(ishift2+ji,jj,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(ji,jj,jk) = ptab(ihl+1,jj,jk)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(ji,jj,jk) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 2.2 fill eastern halo
      ! ---------------------
      ishift = jpi - ihl                ! fill halo from ji = jpi-ihl+1 to jpi 
      SELECT CASE ( ifill_ea )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk) = zrcv_ea(ji,jj,jk,jl,jf)   ! jpi - ihl + 1 -> jpi
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk) = ptab(ishift2+ji,jj,jk)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk) = ptab(ishift,jj,jk)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! ------------------------------- !
      !     3. north fold treatment     !
      ! ------------------------------- !
      !
      ! do it before south directions so concerned processes can do it without waiting for the comm with the sourthern neighbor
      !
      IF( lldo_nfd .AND. ifill_no /= jpfillnothing ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd( ptab, cd_nat, psgn  )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_nfd( ptab, cd_nat, psgn  )   ! for all northern procs.
         END SELECT
         !
         ifill_no = jpfillnothing  ! force to do nothing for the northern halo as we just done the north pole folding
         !
      ENDIF
      !
      ! ---------------------------------------------------- !
      !     4. Do north and south MPI exchange if needed     !
      ! ---------------------------------------------------- !
      !
      IF( llsend_so )   ALLOCATE( zsnd_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llsend_no )   ALLOCATE( zsnd_no(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_so )   ALLOCATE( zrcv_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_no )   ALLOCATE( zrcv_no(jpi,ihl,ipk,ipl,ipf) )
      !
      isize = jpi * ihl * ipk * ipl * ipf      

      ! allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_so ) THEN   ! copy sourhern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_so(ji,jj,jk,jl,jf) = ptab(ji,ishift+jj,jk)   ! ihl+1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( llsend_no ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_no(ji,jj,jk,jl,jf) = ptab(ji,ishift+jj,jk)   ! jpj-2*ihl+1 -> jpj-ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the southern/northern side
      IF( llsend_so )   CALL mppsend( 3, zsnd_so(1,1,1,1,1), isize, noso, ireq_so )
      IF( llsend_no )   CALL mppsend( 4, zsnd_no(1,1,1,1,1), isize, nono, ireq_no )
      ! blocking receive of the southern/northern halo
      IF( llrecv_so )   CALL mpprecv( 4, zrcv_so(1,1,1,1,1), isize, noso )
      IF( llrecv_no )   CALL mpprecv( 3, zrcv_no(1,1,1,1,1), isize, nono )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      ! ------------------------------------- !
      !     5. Fill south and north halos     !
      ! ------------------------------------- !
      !
      ! 5.1 fill southern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from jj = 1 to ihl
      SELECT CASE ( ifill_so )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,jj,jk) = zrcv_so(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,jj,jk) = ptab(ji,ishift2+jj,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
                  ptab(ji,jj,jk) = ptab(ji,ihl+1,jk)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi 
                  ptab(ji,jj,jk) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 5.2 fill northern halo
      ! ----------------------
      ishift = jpj - ihl                ! fill halo from jj = jpj-ihl+1 to jpj 
      SELECT CASE ( ifill_no )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk) = zrcv_no(ji,jj,jk,jl,jf)   ! jpj-ihl+1 -> jpj
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk) = ptab(ji,ishift2+jj,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk) = ptab(ji,ishift,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! -------------------------------------------- !
      !     6. deallocate local temporary arrays     !
      ! -------------------------------------------- !
      !
      IF( llsend_we ) THEN
         CALL mpi_wait(ireq_we, istat, ierr )
         DEALLOCATE( zsnd_we )
      ENDIF
      IF( llsend_ea )  THEN
         CALL mpi_wait(ireq_ea, istat, ierr )
         DEALLOCATE( zsnd_ea )
      ENDIF
      IF( llsend_so ) THEN
         CALL mpi_wait(ireq_so, istat, ierr )
         DEALLOCATE( zsnd_so )
      ENDIF
      IF( llsend_no ) THEN
         CALL mpi_wait(ireq_no, istat, ierr )
         DEALLOCATE( zsnd_no )
      ENDIF
      !
      IF( llrecv_we )   DEALLOCATE( zrcv_we )
      IF( llrecv_ea )   DEALLOCATE( zrcv_ea )
      IF( llrecv_so )   DEALLOCATE( zrcv_so )
      IF( llrecv_no )   DEALLOCATE( zrcv_no )
      !
   END SUBROUTINE mpp_lnk_3d


   SUBROUTINE mpp_lnk_3d_ptr( cdname, ptab, cd_nat, psgn, kfld, kfillmode, pfillval, lsend, lrecv, ihlcom )
      INTEGER             , INTENT(in   ) ::   kfld        ! number of pt3d arrays
      TYPE(PTR_3D)                , INTENT(inout) ::   ptab(:)                                        ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=*)              , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      CHARACTER(len=1)              , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER ,             OPTIONAL, INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp),             OPTIONAL, INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4),OPTIONAL, INTENT(in   ) ::   lsend, lrecv  ! communication with other 4 proc
      INTEGER              ,OPTIONAL, INTENT(in   ) ::   ihlcom        ! number of ranks and rows to be communicated
      !
      INTEGER  ::    ji,  jj,  jk,  jl,  jf      ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf      ! dimension of the input array
      INTEGER  ::   isize, ishift, ishift2       ! local integers
      INTEGER  ::   ireq_we, ireq_ea, ireq_so, ireq_no     ! mpi_request id
      INTEGER  ::   ierr
      INTEGER  ::   ifill_we, ifill_ea, ifill_so, ifill_no
      INTEGER  ::   ihl                          ! number of ranks and rows to be communicated 
      REAL(wp) ::   zland
      INTEGER , DIMENSION(MPI_STATUS_SIZE)        ::   istat          ! for mpi_isend
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_we, zrcv_we, zsnd_ea, zrcv_ea   ! east -west  & west - east  halos
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_so, zrcv_so, zsnd_no, zrcv_no   ! north-south & south-north  halos
      LOGICAL  ::   llsend_we, llsend_ea, llsend_no, llsend_so       ! communication send
      LOGICAL  ::   llrecv_we, llrecv_ea, llrecv_no, llrecv_so       ! communication receive 
      LOGICAL  ::   lldo_nfd                                     ! do north pole folding
      !!----------------------------------------------------------------------
      !
      ! ----------------------------------------- !
      !     0. local variables initialization     !
      ! ----------------------------------------- !
      !
      ipk = SIZE(ptab(1)%pt3d,3)   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( PRESENT(ihlcom) ) THEN   ;   ihl = ihlcom
      ELSE                         ;   ihl = 1
      END IF
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ipk, ipl, ipf, ld_lbc = .TRUE. )
      !
      IF     ( PRESENT(lsend) .AND. PRESENT(lrecv) ) THEN
         llsend_we = lsend(1)   ;   llsend_ea = lsend(2)   ;   llsend_so = lsend(3)   ;   llsend_no = lsend(4)
         llrecv_we = lrecv(1)   ;   llrecv_ea = lrecv(2)   ;   llrecv_so = lrecv(3)   ;   llrecv_no = lrecv(4)
      ELSE IF( PRESENT(lsend) .OR.  PRESENT(lrecv) ) THEN
         WRITE(ctmp1,*) ' E R R O R : Routine ', cdname, '  is calling lbc_lnk with only one of the two arguments lsend or lrecv'
         WRITE(ctmp2,*) ' ========== '
         CALL ctl_stop( ' ', ctmp1, ctmp2, ' ' )
      ELSE   ! send and receive with every neighbour
         llsend_we = nbondi ==  1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_ea = nbondi == -1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_so = nbondj ==  1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llsend_no = nbondj == -1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llrecv_we = llsend_we   ;   llrecv_ea = llsend_ea   ;   llrecv_so = llsend_so   ;   llrecv_no = llsend_no
      END IF
         
         
      lldo_nfd = npolj /= 0                      ! keep for compatibility, should be defined in mppini

      zland = 0._wp                                     ! land filling value: zero by default
      IF( PRESENT( pfillval ) )   zland = pfillval      ! set land value

      ! define the method we will use to fill the halos in each direction
      IF(              llrecv_we ) THEN   ;   ifill_we = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_we = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_we = kfillmode
      ELSE                                ;   ifill_we = jpfillcst
      END IF
      !
      IF(              llrecv_ea ) THEN   ;   ifill_ea = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_ea = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_ea = kfillmode
      ELSE                                ;   ifill_ea = jpfillcst
      END IF
      !
      IF(              llrecv_so ) THEN   ;   ifill_so = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_so = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_so = kfillmode
      ELSE                                ;   ifill_so = jpfillcst
      END IF
      !
      IF(              llrecv_no ) THEN   ;   ifill_no = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_no = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_no = kfillmode
      ELSE                                ;   ifill_no = jpfillcst
      END IF
      !
      !
      ! -------------------------------------------------- !
      !     1. Do east and west MPI exchange if needed     !
      ! -------------------------------------------------- !
      !
      ! Must exchange the whole column (from 1 to jpj) to get the corners if we have no south/north neighbourg
      isize = ihl * jpj * ipk * ipl * ipf      
      !
      ! Allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_we )   ALLOCATE( zsnd_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llsend_ea )   ALLOCATE( zsnd_ea(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_we )   ALLOCATE( zrcv_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_ea )   ALLOCATE( zrcv_ea(ihl,jpj,ipk,ipl,ipf) )
      !
      IF( llsend_we ) THEN   ! copy western side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_we(ji,jj,jk,jl,jf) = ptab(jf)%pt3d(ishift+ji,jj,jk)   ! ihl + 1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF(llsend_ea  ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_ea(ji,jj,jk,jl,jf) = ptab(jf)%pt3d(ishift+ji,jj,jk)   ! jpi - 2*ihl + 1 -> jpi - ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the western/eastern side using local temporary arrays
      IF( llsend_we )   CALL mppsend( 1, zsnd_we(1,1,1,1,1), isize, nowe, ireq_we )
      IF( llsend_ea )   CALL mppsend( 2, zsnd_ea(1,1,1,1,1), isize, noea, ireq_ea )
      ! blocking receive of the western/eastern halo in local temporary arrays
      IF( llrecv_we )   CALL mpprecv( 2, zrcv_we(1,1,1,1,1), isize, nowe )
      IF( llrecv_ea )   CALL mpprecv( 1, zrcv_ea(1,1,1,1,1), isize, noea )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !
      ! ----------------------------------- !
      !     2. Fill east and west halos     !
      ! ----------------------------------- !
      !
      ! 2.1 fill weastern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from ji = 1 to ihl
      SELECT CASE ( ifill_we )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt3d(ji,jj,jk) = zrcv_we(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt3d(ji,jj,jk) = ptab(jf)%pt3d(ishift2+ji,jj,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(jf)%pt3d(ji,jj,jk) = ptab(jf)%pt3d(ihl+1,jj,jk)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(jf)%pt3d(ji,jj,jk) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 2.2 fill eastern halo
      ! ---------------------
      ishift = jpi - ihl                ! fill halo from ji = jpi-ihl+1 to jpi 
      SELECT CASE ( ifill_ea )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt3d(ishift+ji,jj,jk) = zrcv_ea(ji,jj,jk,jl,jf)   ! jpi - ihl + 1 -> jpi
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt3d(ishift+ji,jj,jk) = ptab(jf)%pt3d(ishift2+ji,jj,jk)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt3d(ishift+ji,jj,jk) = ptab(jf)%pt3d(ishift,jj,jk)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt3d(ishift+ji,jj,jk) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! ------------------------------- !
      !     3. north fold treatment     !
      ! ------------------------------- !
      !
      ! do it before south directions so concerned processes can do it without waiting for the comm with the sourthern neighbor
      !
      IF( lldo_nfd .AND. ifill_no /= jpfillnothing ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd( ptab, cd_nat(:), psgn(:) ,ipf )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_nfd( ptab, cd_nat(:), psgn(:) ,ipf )   ! for all northern procs.
         END SELECT
         !
         ifill_no = jpfillnothing  ! force to do nothing for the northern halo as we just done the north pole folding
         !
      ENDIF
      !
      ! ---------------------------------------------------- !
      !     4. Do north and south MPI exchange if needed     !
      ! ---------------------------------------------------- !
      !
      IF( llsend_so )   ALLOCATE( zsnd_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llsend_no )   ALLOCATE( zsnd_no(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_so )   ALLOCATE( zrcv_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_no )   ALLOCATE( zrcv_no(jpi,ihl,ipk,ipl,ipf) )
      !
      isize = jpi * ihl * ipk * ipl * ipf      

      ! allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_so ) THEN   ! copy sourhern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_so(ji,jj,jk,jl,jf) = ptab(jf)%pt3d(ji,ishift+jj,jk)   ! ihl+1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( llsend_no ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_no(ji,jj,jk,jl,jf) = ptab(jf)%pt3d(ji,ishift+jj,jk)   ! jpj-2*ihl+1 -> jpj-ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the southern/northern side
      IF( llsend_so )   CALL mppsend( 3, zsnd_so(1,1,1,1,1), isize, noso, ireq_so )
      IF( llsend_no )   CALL mppsend( 4, zsnd_no(1,1,1,1,1), isize, nono, ireq_no )
      ! blocking receive of the southern/northern halo
      IF( llrecv_so )   CALL mpprecv( 4, zrcv_so(1,1,1,1,1), isize, noso )
      IF( llrecv_no )   CALL mpprecv( 3, zrcv_no(1,1,1,1,1), isize, nono )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      ! ------------------------------------- !
      !     5. Fill south and north halos     !
      ! ------------------------------------- !
      !
      ! 5.1 fill southern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from jj = 1 to ihl
      SELECT CASE ( ifill_so )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt3d(ji,jj,jk) = zrcv_so(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt3d(ji,jj,jk) = ptab(jf)%pt3d(ji,ishift2+jj,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
                  ptab(jf)%pt3d(ji,jj,jk) = ptab(jf)%pt3d(ji,ihl+1,jk)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi 
                  ptab(jf)%pt3d(ji,jj,jk) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 5.2 fill northern halo
      ! ----------------------
      ishift = jpj - ihl                ! fill halo from jj = jpj-ihl+1 to jpj 
      SELECT CASE ( ifill_no )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt3d(ji,ishift+jj,jk) = zrcv_no(ji,jj,jk,jl,jf)   ! jpj-ihl+1 -> jpj
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt3d(ji,ishift+jj,jk) = ptab(jf)%pt3d(ji,ishift2+jj,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt3d(ji,ishift+jj,jk) = ptab(jf)%pt3d(ji,ishift,jk)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt3d(ji,ishift+jj,jk) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! -------------------------------------------- !
      !     6. deallocate local temporary arrays     !
      ! -------------------------------------------- !
      !
      IF( llsend_we ) THEN
         CALL mpi_wait(ireq_we, istat, ierr )
         DEALLOCATE( zsnd_we )
      ENDIF
      IF( llsend_ea )  THEN
         CALL mpi_wait(ireq_ea, istat, ierr )
         DEALLOCATE( zsnd_ea )
      ENDIF
      IF( llsend_so ) THEN
         CALL mpi_wait(ireq_so, istat, ierr )
         DEALLOCATE( zsnd_so )
      ENDIF
      IF( llsend_no ) THEN
         CALL mpi_wait(ireq_no, istat, ierr )
         DEALLOCATE( zsnd_no )
      ENDIF
      !
      IF( llrecv_we )   DEALLOCATE( zrcv_we )
      IF( llrecv_ea )   DEALLOCATE( zrcv_ea )
      IF( llrecv_so )   DEALLOCATE( zrcv_so )
      IF( llrecv_no )   DEALLOCATE( zrcv_no )
      !
   END SUBROUTINE mpp_lnk_3d_ptr

   !
   !                       !==  4D array and array of 4D pointer  ==!
   !

   SUBROUTINE mpp_lnk_4d( cdname, ptab, cd_nat, psgn      , kfillmode, pfillval, lsend, lrecv, ihlcom )
      REAL(wp)                    , INTENT(inout) ::   ptab(:,:,:,:)                                        ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=*)              , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      CHARACTER(len=1)              , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER ,             OPTIONAL, INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp),             OPTIONAL, INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4),OPTIONAL, INTENT(in   ) ::   lsend, lrecv  ! communication with other 4 proc
      INTEGER              ,OPTIONAL, INTENT(in   ) ::   ihlcom        ! number of ranks and rows to be communicated
      !
      INTEGER  ::    ji,  jj,  jk,  jl,  jf      ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf      ! dimension of the input array
      INTEGER  ::   isize, ishift, ishift2       ! local integers
      INTEGER  ::   ireq_we, ireq_ea, ireq_so, ireq_no     ! mpi_request id
      INTEGER  ::   ierr
      INTEGER  ::   ifill_we, ifill_ea, ifill_so, ifill_no
      INTEGER  ::   ihl                          ! number of ranks and rows to be communicated 
      REAL(wp) ::   zland
      INTEGER , DIMENSION(MPI_STATUS_SIZE)        ::   istat          ! for mpi_isend
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_we, zrcv_we, zsnd_ea, zrcv_ea   ! east -west  & west - east  halos
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_so, zrcv_so, zsnd_no, zrcv_no   ! north-south & south-north  halos
      LOGICAL  ::   llsend_we, llsend_ea, llsend_no, llsend_so       ! communication send
      LOGICAL  ::   llrecv_we, llrecv_ea, llrecv_no, llrecv_so       ! communication receive 
      LOGICAL  ::   lldo_nfd                                     ! do north pole folding
      !!----------------------------------------------------------------------
      !
      ! ----------------------------------------- !
      !     0. local variables initialization     !
      ! ----------------------------------------- !
      !
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = SIZE(ptab,4)   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( PRESENT(ihlcom) ) THEN   ;   ihl = ihlcom
      ELSE                         ;   ihl = 1
      END IF
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ipk, ipl, ipf, ld_lbc = .TRUE. )
      !
      IF     ( PRESENT(lsend) .AND. PRESENT(lrecv) ) THEN
         llsend_we = lsend(1)   ;   llsend_ea = lsend(2)   ;   llsend_so = lsend(3)   ;   llsend_no = lsend(4)
         llrecv_we = lrecv(1)   ;   llrecv_ea = lrecv(2)   ;   llrecv_so = lrecv(3)   ;   llrecv_no = lrecv(4)
      ELSE IF( PRESENT(lsend) .OR.  PRESENT(lrecv) ) THEN
         WRITE(ctmp1,*) ' E R R O R : Routine ', cdname, '  is calling lbc_lnk with only one of the two arguments lsend or lrecv'
         WRITE(ctmp2,*) ' ========== '
         CALL ctl_stop( ' ', ctmp1, ctmp2, ' ' )
      ELSE   ! send and receive with every neighbour
         llsend_we = nbondi ==  1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_ea = nbondi == -1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_so = nbondj ==  1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llsend_no = nbondj == -1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llrecv_we = llsend_we   ;   llrecv_ea = llsend_ea   ;   llrecv_so = llsend_so   ;   llrecv_no = llsend_no
      END IF
         
         
      lldo_nfd = npolj /= 0                      ! keep for compatibility, should be defined in mppini

      zland = 0._wp                                     ! land filling value: zero by default
      IF( PRESENT( pfillval ) )   zland = pfillval      ! set land value

      ! define the method we will use to fill the halos in each direction
      IF(              llrecv_we ) THEN   ;   ifill_we = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_we = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_we = kfillmode
      ELSE                                ;   ifill_we = jpfillcst
      END IF
      !
      IF(              llrecv_ea ) THEN   ;   ifill_ea = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_ea = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_ea = kfillmode
      ELSE                                ;   ifill_ea = jpfillcst
      END IF
      !
      IF(              llrecv_so ) THEN   ;   ifill_so = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_so = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_so = kfillmode
      ELSE                                ;   ifill_so = jpfillcst
      END IF
      !
      IF(              llrecv_no ) THEN   ;   ifill_no = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_no = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_no = kfillmode
      ELSE                                ;   ifill_no = jpfillcst
      END IF
      !
      !
      ! -------------------------------------------------- !
      !     1. Do east and west MPI exchange if needed     !
      ! -------------------------------------------------- !
      !
      ! Must exchange the whole column (from 1 to jpj) to get the corners if we have no south/north neighbourg
      isize = ihl * jpj * ipk * ipl * ipf      
      !
      ! Allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_we )   ALLOCATE( zsnd_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llsend_ea )   ALLOCATE( zsnd_ea(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_we )   ALLOCATE( zrcv_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_ea )   ALLOCATE( zrcv_ea(ihl,jpj,ipk,ipl,ipf) )
      !
      IF( llsend_we ) THEN   ! copy western side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_we(ji,jj,jk,jl,jf) = ptab(ishift+ji,jj,jk,jl)   ! ihl + 1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF(llsend_ea  ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_ea(ji,jj,jk,jl,jf) = ptab(ishift+ji,jj,jk,jl)   ! jpi - 2*ihl + 1 -> jpi - ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the western/eastern side using local temporary arrays
      IF( llsend_we )   CALL mppsend( 1, zsnd_we(1,1,1,1,1), isize, nowe, ireq_we )
      IF( llsend_ea )   CALL mppsend( 2, zsnd_ea(1,1,1,1,1), isize, noea, ireq_ea )
      ! blocking receive of the western/eastern halo in local temporary arrays
      IF( llrecv_we )   CALL mpprecv( 2, zrcv_we(1,1,1,1,1), isize, nowe )
      IF( llrecv_ea )   CALL mpprecv( 1, zrcv_ea(1,1,1,1,1), isize, noea )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !
      ! ----------------------------------- !
      !     2. Fill east and west halos     !
      ! ----------------------------------- !
      !
      ! 2.1 fill weastern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from ji = 1 to ihl
      SELECT CASE ( ifill_we )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ji,jj,jk,jl) = zrcv_we(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ji,jj,jk,jl) = ptab(ishift2+ji,jj,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(ji,jj,jk,jl) = ptab(ihl+1,jj,jk,jl)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(ji,jj,jk,jl) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 2.2 fill eastern halo
      ! ---------------------
      ishift = jpi - ihl                ! fill halo from ji = jpi-ihl+1 to jpi 
      SELECT CASE ( ifill_ea )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk,jl) = zrcv_ea(ji,jj,jk,jl,jf)   ! jpi - ihl + 1 -> jpi
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk,jl) = ptab(ishift2+ji,jj,jk,jl)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk,jl) = ptab(ishift,jj,jk,jl)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(ishift+ji,jj,jk,jl) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! ------------------------------- !
      !     3. north fold treatment     !
      ! ------------------------------- !
      !
      ! do it before south directions so concerned processes can do it without waiting for the comm with the sourthern neighbor
      !
      IF( lldo_nfd .AND. ifill_no /= jpfillnothing ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd( ptab, cd_nat, psgn  )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_nfd( ptab, cd_nat, psgn  )   ! for all northern procs.
         END SELECT
         !
         ifill_no = jpfillnothing  ! force to do nothing for the northern halo as we just done the north pole folding
         !
      ENDIF
      !
      ! ---------------------------------------------------- !
      !     4. Do north and south MPI exchange if needed     !
      ! ---------------------------------------------------- !
      !
      IF( llsend_so )   ALLOCATE( zsnd_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llsend_no )   ALLOCATE( zsnd_no(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_so )   ALLOCATE( zrcv_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_no )   ALLOCATE( zrcv_no(jpi,ihl,ipk,ipl,ipf) )
      !
      isize = jpi * ihl * ipk * ipl * ipf      

      ! allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_so ) THEN   ! copy sourhern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_so(ji,jj,jk,jl,jf) = ptab(ji,ishift+jj,jk,jl)   ! ihl+1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( llsend_no ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_no(ji,jj,jk,jl,jf) = ptab(ji,ishift+jj,jk,jl)   ! jpj-2*ihl+1 -> jpj-ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the southern/northern side
      IF( llsend_so )   CALL mppsend( 3, zsnd_so(1,1,1,1,1), isize, noso, ireq_so )
      IF( llsend_no )   CALL mppsend( 4, zsnd_no(1,1,1,1,1), isize, nono, ireq_no )
      ! blocking receive of the southern/northern halo
      IF( llrecv_so )   CALL mpprecv( 4, zrcv_so(1,1,1,1,1), isize, noso )
      IF( llrecv_no )   CALL mpprecv( 3, zrcv_no(1,1,1,1,1), isize, nono )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      ! ------------------------------------- !
      !     5. Fill south and north halos     !
      ! ------------------------------------- !
      !
      ! 5.1 fill southern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from jj = 1 to ihl
      SELECT CASE ( ifill_so )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,jj,jk,jl) = zrcv_so(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,jj,jk,jl) = ptab(ji,ishift2+jj,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
                  ptab(ji,jj,jk,jl) = ptab(ji,ihl+1,jk,jl)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi 
                  ptab(ji,jj,jk,jl) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 5.2 fill northern halo
      ! ----------------------
      ishift = jpj - ihl                ! fill halo from jj = jpj-ihl+1 to jpj 
      SELECT CASE ( ifill_no )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk,jl) = zrcv_no(ji,jj,jk,jl,jf)   ! jpj-ihl+1 -> jpj
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk,jl) = ptab(ji,ishift2+jj,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk,jl) = ptab(ji,ishift,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(ji,ishift+jj,jk,jl) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! -------------------------------------------- !
      !     6. deallocate local temporary arrays     !
      ! -------------------------------------------- !
      !
      IF( llsend_we ) THEN
         CALL mpi_wait(ireq_we, istat, ierr )
         DEALLOCATE( zsnd_we )
      ENDIF
      IF( llsend_ea )  THEN
         CALL mpi_wait(ireq_ea, istat, ierr )
         DEALLOCATE( zsnd_ea )
      ENDIF
      IF( llsend_so ) THEN
         CALL mpi_wait(ireq_so, istat, ierr )
         DEALLOCATE( zsnd_so )
      ENDIF
      IF( llsend_no ) THEN
         CALL mpi_wait(ireq_no, istat, ierr )
         DEALLOCATE( zsnd_no )
      ENDIF
      !
      IF( llrecv_we )   DEALLOCATE( zrcv_we )
      IF( llrecv_ea )   DEALLOCATE( zrcv_ea )
      IF( llrecv_so )   DEALLOCATE( zrcv_so )
      IF( llrecv_no )   DEALLOCATE( zrcv_no )
      !
   END SUBROUTINE mpp_lnk_4d


   SUBROUTINE mpp_lnk_4d_ptr( cdname, ptab, cd_nat, psgn, kfld, kfillmode, pfillval, lsend, lrecv, ihlcom )
      INTEGER             , INTENT(in   ) ::   kfld        ! number of pt3d arrays
      TYPE(PTR_4D)                , INTENT(inout) ::   ptab(:)                                        ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=*)              , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      CHARACTER(len=1)              , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)                      , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER ,             OPTIONAL, INTENT(in   ) ::   kfillmode   ! filling method for halo over land (default = constant)
      REAL(wp),             OPTIONAL, INTENT(in   ) ::   pfillval    ! background value (used at closed boundaries)
      LOGICAL, DIMENSION(4),OPTIONAL, INTENT(in   ) ::   lsend, lrecv  ! communication with other 4 proc
      INTEGER              ,OPTIONAL, INTENT(in   ) ::   ihlcom        ! number of ranks and rows to be communicated
      !
      INTEGER  ::    ji,  jj,  jk,  jl,  jf      ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf      ! dimension of the input array
      INTEGER  ::   isize, ishift, ishift2       ! local integers
      INTEGER  ::   ireq_we, ireq_ea, ireq_so, ireq_no     ! mpi_request id
      INTEGER  ::   ierr
      INTEGER  ::   ifill_we, ifill_ea, ifill_so, ifill_no
      INTEGER  ::   ihl                          ! number of ranks and rows to be communicated 
      REAL(wp) ::   zland
      INTEGER , DIMENSION(MPI_STATUS_SIZE)        ::   istat          ! for mpi_isend
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_we, zrcv_we, zsnd_ea, zrcv_ea   ! east -west  & west - east  halos
      REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE ::   zsnd_so, zrcv_so, zsnd_no, zrcv_no   ! north-south & south-north  halos
      LOGICAL  ::   llsend_we, llsend_ea, llsend_no, llsend_so       ! communication send
      LOGICAL  ::   llrecv_we, llrecv_ea, llrecv_no, llrecv_so       ! communication receive 
      LOGICAL  ::   lldo_nfd                                     ! do north pole folding
      !!----------------------------------------------------------------------
      !
      ! ----------------------------------------- !
      !     0. local variables initialization     !
      ! ----------------------------------------- !
      !
      ipk = SIZE(ptab(1)%pt4d,3)   ! 3rd dimension
      ipl = SIZE(ptab(1)%pt4d,4)   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( PRESENT(ihlcom) ) THEN   ;   ihl = ihlcom
      ELSE                         ;   ihl = 1
      END IF
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ipk, ipl, ipf, ld_lbc = .TRUE. )
      !
      IF     ( PRESENT(lsend) .AND. PRESENT(lrecv) ) THEN
         llsend_we = lsend(1)   ;   llsend_ea = lsend(2)   ;   llsend_so = lsend(3)   ;   llsend_no = lsend(4)
         llrecv_we = lrecv(1)   ;   llrecv_ea = lrecv(2)   ;   llrecv_so = lrecv(3)   ;   llrecv_no = lrecv(4)
      ELSE IF( PRESENT(lsend) .OR.  PRESENT(lrecv) ) THEN
         WRITE(ctmp1,*) ' E R R O R : Routine ', cdname, '  is calling lbc_lnk with only one of the two arguments lsend or lrecv'
         WRITE(ctmp2,*) ' ========== '
         CALL ctl_stop( ' ', ctmp1, ctmp2, ' ' )
      ELSE   ! send and receive with every neighbour
         llsend_we = nbondi ==  1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_ea = nbondi == -1 .OR. nbondi == 0   ! keep for compatibility, should be defined in mppini
         llsend_so = nbondj ==  1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llsend_no = nbondj == -1 .OR. nbondj == 0   ! keep for compatibility, should be defined in mppini
         llrecv_we = llsend_we   ;   llrecv_ea = llsend_ea   ;   llrecv_so = llsend_so   ;   llrecv_no = llsend_no
      END IF
         
         
      lldo_nfd = npolj /= 0                      ! keep for compatibility, should be defined in mppini

      zland = 0._wp                                     ! land filling value: zero by default
      IF( PRESENT( pfillval ) )   zland = pfillval      ! set land value

      ! define the method we will use to fill the halos in each direction
      IF(              llrecv_we ) THEN   ;   ifill_we = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_we = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_we = kfillmode
      ELSE                                ;   ifill_we = jpfillcst
      END IF
      !
      IF(              llrecv_ea ) THEN   ;   ifill_ea = jpfillmpi
      ELSEIF(           l_Iperio ) THEN   ;   ifill_ea = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_ea = kfillmode
      ELSE                                ;   ifill_ea = jpfillcst
      END IF
      !
      IF(              llrecv_so ) THEN   ;   ifill_so = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_so = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_so = kfillmode
      ELSE                                ;   ifill_so = jpfillcst
      END IF
      !
      IF(              llrecv_no ) THEN   ;   ifill_no = jpfillmpi
      ELSEIF(           l_Jperio ) THEN   ;   ifill_no = jpfillperio
      ELSEIF( PRESENT(kfillmode) ) THEN   ;   ifill_no = kfillmode
      ELSE                                ;   ifill_no = jpfillcst
      END IF
      !
      !
      ! -------------------------------------------------- !
      !     1. Do east and west MPI exchange if needed     !
      ! -------------------------------------------------- !
      !
      ! Must exchange the whole column (from 1 to jpj) to get the corners if we have no south/north neighbourg
      isize = ihl * jpj * ipk * ipl * ipf      
      !
      ! Allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_we )   ALLOCATE( zsnd_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llsend_ea )   ALLOCATE( zsnd_ea(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_we )   ALLOCATE( zrcv_we(ihl,jpj,ipk,ipl,ipf) )
      IF( llrecv_ea )   ALLOCATE( zrcv_ea(ihl,jpj,ipk,ipl,ipf) )
      !
      IF( llsend_we ) THEN   ! copy western side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_we(ji,jj,jk,jl,jf) = ptab(jf)%pt4d(ishift+ji,jj,jk,jl)   ! ihl + 1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF(llsend_ea  ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            zsnd_ea(ji,jj,jk,jl,jf) = ptab(jf)%pt4d(ishift+ji,jj,jk,jl)   ! jpi - 2*ihl + 1 -> jpi - ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the western/eastern side using local temporary arrays
      IF( llsend_we )   CALL mppsend( 1, zsnd_we(1,1,1,1,1), isize, nowe, ireq_we )
      IF( llsend_ea )   CALL mppsend( 2, zsnd_ea(1,1,1,1,1), isize, noea, ireq_ea )
      ! blocking receive of the western/eastern halo in local temporary arrays
      IF( llrecv_we )   CALL mpprecv( 2, zrcv_we(1,1,1,1,1), isize, nowe )
      IF( llrecv_ea )   CALL mpprecv( 1, zrcv_ea(1,1,1,1,1), isize, noea )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !
      ! ----------------------------------- !
      !     2. Fill east and west halos     !
      ! ----------------------------------- !
      !
      ! 2.1 fill weastern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from ji = 1 to ihl
      SELECT CASE ( ifill_we )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt4d(ji,jj,jk,jl) = zrcv_we(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = jpi - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt4d(ji,jj,jk,jl) = ptab(jf)%pt4d(ishift2+ji,jj,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(jf)%pt4d(ji,jj,jk,jl) = ptab(jf)%pt4d(ihl+1,jj,jk,jl)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
                  ptab(jf)%pt4d(ji,jj,jk,jl) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 2.2 fill eastern halo
      ! ---------------------
      ishift = jpi - ihl                ! fill halo from ji = jpi-ihl+1 to jpi 
      SELECT CASE ( ifill_ea )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt4d(ishift+ji,jj,jk,jl) = zrcv_ea(ji,jj,jk,jl,jf)   ! jpi - ihl + 1 -> jpi
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use east-weast periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt4d(ishift+ji,jj,jk,jl) = ptab(jf)%pt4d(ishift2+ji,jj,jk,jl)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt4d(ishift+ji,jj,jk,jl) = ptab(jf)%pt4d(ishift,jj,jk,jl)
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, jpj   ;   DO ji = 1, ihl
            ptab(jf)%pt4d(ishift+ji,jj,jk,jl) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! ------------------------------- !
      !     3. north fold treatment     !
      ! ------------------------------- !
      !
      ! do it before south directions so concerned processes can do it without waiting for the comm with the sourthern neighbor
      !
      IF( lldo_nfd .AND. ifill_no /= jpfillnothing ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd( ptab, cd_nat(:), psgn(:) ,ipf )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_nfd( ptab, cd_nat(:), psgn(:) ,ipf )   ! for all northern procs.
         END SELECT
         !
         ifill_no = jpfillnothing  ! force to do nothing for the northern halo as we just done the north pole folding
         !
      ENDIF
      !
      ! ---------------------------------------------------- !
      !     4. Do north and south MPI exchange if needed     !
      ! ---------------------------------------------------- !
      !
      IF( llsend_so )   ALLOCATE( zsnd_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llsend_no )   ALLOCATE( zsnd_no(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_so )   ALLOCATE( zrcv_so(jpi,ihl,ipk,ipl,ipf) )
      IF( llrecv_no )   ALLOCATE( zrcv_no(jpi,ihl,ipk,ipl,ipf) )
      !
      isize = jpi * ihl * ipk * ipl * ipf      

      ! allocate local temporary arrays to be sent/received. Fill arrays to be sent
      IF( llsend_so ) THEN   ! copy sourhern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_so(ji,jj,jk,jl,jf) = ptab(jf)%pt4d(ji,ishift+jj,jk,jl)   ! ihl+1 -> 2*ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( llsend_no ) THEN   ! copy eastern side of the inner mpi domain in local temporary array to be sent by MPI
         ishift = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            zsnd_no(ji,jj,jk,jl,jf) = ptab(jf)%pt4d(ji,ishift+jj,jk,jl)   ! jpj-2*ihl+1 -> jpj-ihl
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      ENDIF
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      ! non-blocking send of the southern/northern side
      IF( llsend_so )   CALL mppsend( 3, zsnd_so(1,1,1,1,1), isize, noso, ireq_so )
      IF( llsend_no )   CALL mppsend( 4, zsnd_no(1,1,1,1,1), isize, nono, ireq_no )
      ! blocking receive of the southern/northern halo
      IF( llrecv_so )   CALL mpprecv( 4, zrcv_so(1,1,1,1,1), isize, noso )
      IF( llrecv_no )   CALL mpprecv( 3, zrcv_no(1,1,1,1,1), isize, nono )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      ! ------------------------------------- !
      !     5. Fill south and north halos     !
      ! ------------------------------------- !
      !
      ! 5.1 fill southern halo
      ! ----------------------
      ! ishift = 0                         ! fill halo from jj = 1 to ihl
      SELECT CASE ( ifill_so )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt4d(ji,jj,jk,jl) = zrcv_so(ji,jj,jk,jl,jf)   ! 1 -> ihl
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = jpj - 2 * ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt4d(ji,jj,jk,jl) = ptab(jf)%pt4d(ji,ishift2+jj,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
                  ptab(jf)%pt4d(ji,jj,jk,jl) = ptab(jf)%pt4d(ji,ihl+1,jk,jl)
               END DO   ;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf                               ! number of arrays to be treated
            IF( .NOT. cd_nat(jf) == 'F' ) THEN        ! do nothing for F point
               DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi 
                  ptab(jf)%pt4d(ji,jj,jk,jl) = zland
               END DO;   END DO   ;   END DO   ;   END DO
            ENDIF
         END DO
      END SELECT
      !
      ! 5.2 fill northern halo
      ! ----------------------
      ishift = jpj - ihl                ! fill halo from jj = jpj-ihl+1 to jpj 
      SELECT CASE ( ifill_no )
      CASE ( jpfillnothing )               ! no filling 
      CASE ( jpfillmpi   )                 ! use data received by MPI 
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt4d(ji,ishift+jj,jk,jl) = zrcv_no(ji,jj,jk,jl,jf)   ! jpj-ihl+1 -> jpj
         END DO   ;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillperio )                 ! use north-south periodicity
         ishift2 = ihl
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt4d(ji,ishift+jj,jk,jl) = ptab(jf)%pt4d(ji,ishift2+jj,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcopy  )                 ! filling with inner domain values
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt4d(ji,ishift+jj,jk,jl) = ptab(jf)%pt4d(ji,ishift,jk,jl)
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      CASE ( jpfillcst   )                 ! filling with constant value
         DO jf = 1, ipf   ;   DO jl = 1, ipl   ;   DO jk = 1, ipk   ;   DO jj = 1, ihl   ;   DO ji = 1, jpi
            ptab(jf)%pt4d(ji,ishift+jj,jk,jl) = zland
         END DO;   END DO   ;   END DO   ;   END DO   ;   END DO
      END SELECT
      !
      ! -------------------------------------------- !
      !     6. deallocate local temporary arrays     !
      ! -------------------------------------------- !
      !
      IF( llsend_we ) THEN
         CALL mpi_wait(ireq_we, istat, ierr )
         DEALLOCATE( zsnd_we )
      ENDIF
      IF( llsend_ea )  THEN
         CALL mpi_wait(ireq_ea, istat, ierr )
         DEALLOCATE( zsnd_ea )
      ENDIF
      IF( llsend_so ) THEN
         CALL mpi_wait(ireq_so, istat, ierr )
         DEALLOCATE( zsnd_so )
      ENDIF
      IF( llsend_no ) THEN
         CALL mpi_wait(ireq_no, istat, ierr )
         DEALLOCATE( zsnd_no )
      ENDIF
      !
      IF( llrecv_we )   DEALLOCATE( zrcv_we )
      IF( llrecv_ea )   DEALLOCATE( zrcv_ea )
      IF( llrecv_so )   DEALLOCATE( zrcv_so )
      IF( llrecv_no )   DEALLOCATE( zrcv_no )
      !
   END SUBROUTINE mpp_lnk_4d_ptr


   !!----------------------------------------------------------------------
   !!                   ***  routine mpp_nfd_(2,3,4)d  ***
   !!
   !!   * Argument : dummy argument use in mpp_nfd_... routines
   !!                ptab      :   array or pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   optional, number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE mpp_nfd_2d( ptab, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      REAL(wp)         , INTENT(inout) ::   ptab(:,:)   ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::   ji,  jj,  jk,  jl, jh, jf, jr   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf         ! dimension of the input array
      INTEGER  ::   imigr, iihom, ijhom             ! local integers
      INTEGER  ::   ierr, ibuffsize, ilci, ildi, ilei, iilb
      INTEGER  ::   ij, iproc
      INTEGER, DIMENSION (jpmaxngh)       ::   ml_req_nf   ! for mpi_isend when avoiding mpi_allgather
      INTEGER                             ::   ml_err      ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat     ! for mpi_isend when avoiding mpi_allgather
      !                                                    ! Workspace for message transfers avoiding mpi_allgather
      INTEGER                             ::   ipf_j       ! sum of lines for all multi fields
      INTEGER                             ::   js          ! counter
      INTEGER, DIMENSION(:,:),          ALLOCATABLE ::   jj_s  ! position of sent lines
      INTEGER, DIMENSION(:),            ALLOCATABLE ::   ipj_s ! number of sent lines
      REAL(wp), DIMENSION(:,:,:)      , ALLOCATABLE ::   ztabl
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   ztab, ztabr
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   znorthloc, zfoldwk      
      REAL(wp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( l_north_nogather ) THEN      !==  no allgather exchanges  ==!

         ALLOCATE(ipj_s(ipf))

         ipj      = 2            ! Max 2nd dimension of message transfers (last two j-line only)
         ipj_s(:) = 1            ! Real 2nd dimension of message transfers (depending on perf requirement)
                                 ! by default, only one line is exchanged

         ALLOCATE( jj_s(ipf,2) )

         ! re-define number of exchanged lines :
         !  must be two during the first two time steps
         !  to correct possible incoherent values on North fold lines from restart 

         !!!!!!!!!           temporary switch off this optimisation ==> force TRUE           !!!!!!!!
         !!!!!!!!!  needed to get the same results without agrif and with agrif and no zoom  !!!!!!!!
         !!!!!!!!!                    I don't know why we must do that...                    !!!!!!!!
         l_full_nf_update = .TRUE.

         ! Two lines update (slower but necessary to avoid different values ion identical grid points
         IF ( l_full_nf_update .OR.                          &    ! if coupling fields
              ( ncom_stp == nit000 .AND. .NOT. ln_rstart ) ) &    ! at first time step, if not restart
            ipj_s(:) = 2

         ! Index of modifying lines in input
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            !
            SELECT CASE ( npolj )
            !
            CASE ( 3, 4 )                       ! *  North fold  T-point pivot
               !
               SELECT CASE ( cd_nat )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 3 ;  jj_s(jf,2) = nlcj - 2
               END SELECT
            !
            CASE ( 5, 6 )                        ! *  North fold  F-point pivot
               SELECT CASE ( cd_nat )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 1      
                  ipj_s(jf) = 1                  ! need only one line anyway
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               END SELECT
            !
            END SELECT
            !
         ENDDO
         ! 
         ipf_j = sum (ipj_s(:))      ! Total number of lines to be exchanged
         !
         ALLOCATE( znorthloc(jpimax,ipf_j,ipk,ipl,1) )
         !
         js = 0
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            DO jj = 1, ipj_s(jf)
               js = js + 1
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     znorthloc(1:jpi,js,jk,jl,1) = ptab(1:jpi,jj_s(jf,jj))
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipf_j * ipk * ipl
         !
         ALLOCATE( zfoldwk(jpimax,ipf_j,ipk,ipl,1) )
         ALLOCATE( ztabr(jpimax*jpmaxngh,ipj,ipk,ipl,ipf) ) 
         ! when some processors of the north fold are suppressed, 
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined 
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztabr(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         !
         DO jr = 1, nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mppsend( 5, znorthloc, ibuffsize, nfipproc(isendto(jr),jpnj), ml_req_nf(jr) )
            ENDIF
         END DO
         !
         DO jr = 1,nsndto
            iproc = nfipproc(isendto(jr),jpnj)
            IF(iproc /= -1) THEN
               iilb = nimppt(iproc+1)
               ilci = nlcit (iproc+1)
               ildi = nldit (iproc+1)
               ilei = nleit (iproc+1)
               IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
               IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
               iilb = nfiimpp(isendto(jr),jpnj) - nfiimpp(isendto(1),jpnj)
            ENDIF
            IF( iproc /= narea-1 .AND. iproc /= -1 ) THEN
               CALL mpprecv(5, zfoldwk, ibuffsize, iproc)
               js = 0
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  js = js + 1
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = zfoldwk(ji,js,jk,jl,1)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ELSE IF( iproc == narea-1 ) THEN
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = ptab(ji,jj_s(jf,jj))
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ENDIF
         END DO
         DO jr = 1,nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mpi_wait( ml_req_nf(jr), ml_stat, ml_err )
            ENDIF
         END DO
         !
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         ! North fold boundary condition
         !
         DO jf = 1, ipf
            CALL lbc_nfd_nogather( ptab(:,:), ztabr(:,1:ipj_s(jf),:,:,jf), cd_nat , psgn  )
         END DO
         !
         DEALLOCATE( zfoldwk, ztabr, jj_s, ipj_s )
         !
      ELSE                             !==  allgather exchanges  ==!
         !
         ipj   = 4            ! 2nd dimension of message transfers (last j-lines)
         !
         ALLOCATE( znorthloc(jpimax,ipj,ipk,ipl,ipf) )
         !
         DO jf = 1, ipf                ! put in znorthloc the last ipj j-lines of ptab
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj - ipj +1, nlcj
                     ij = jj - nlcj + ipj
                     znorthloc(1:jpi,ij,jk,jl,jf) = ptab(1:jpi,jj)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipj * ipk * ipl * ipf
         !
         ALLOCATE( ztab       (jpiglo,ipj,ipk,ipl,ipf     ) )
         ALLOCATE( znorthgloio(jpimax,ipj,ipk,ipl,ipf,jpni) )
         !
         ! when some processors of the north fold are suppressed,
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztab(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         CALL MPI_ALLGATHER( znorthloc  , ibuffsize, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, ibuffsize, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         ! stop waiting time measurement
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            iilb  = nimppt(iproc)
            ilci  = nlcit (iproc)
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
            IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
            DO jf = 1, ipf
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     DO jj = 1, ipj
                        DO ji = ildi, ilei
                           ztab(ji+iilb-1,jj,jk,jl,jf) = znorthgloio(ji,jj,jk,jl,jf,jr)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
         DO jf = 1, ipf
            CALL lbc_nfd( ztab(:,:,:,:,jf), cd_nat , psgn  )   ! North fold boundary condition
         END DO
         !
         DO jf = 1, ipf
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj-ipj+1, nlcj             ! Scatter back to ARRAY_IN
                     ij = jj - nlcj + ipj
                     DO ji= 1, nlci
                        ptab(ji,jj) = ztab(ji+nimpp-1,ij,jk,jl,jf)
                     END DO
                  END DO
               END DO
            END DO
         END DO
         !
      !
         DEALLOCATE( ztab )
         DEALLOCATE( znorthgloio )
      ENDIF
      !
      DEALLOCATE( znorthloc )
      !
   END SUBROUTINE mpp_nfd_2d


   SUBROUTINE mpp_nfd_2d_ptr( ptab, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      TYPE(PTR_2D)     , INTENT(inout) ::   ptab(:)   ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::   ji,  jj,  jk,  jl, jh, jf, jr   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf         ! dimension of the input array
      INTEGER  ::   imigr, iihom, ijhom             ! local integers
      INTEGER  ::   ierr, ibuffsize, ilci, ildi, ilei, iilb
      INTEGER  ::   ij, iproc
      INTEGER, DIMENSION (jpmaxngh)       ::   ml_req_nf   ! for mpi_isend when avoiding mpi_allgather
      INTEGER                             ::   ml_err      ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat     ! for mpi_isend when avoiding mpi_allgather
      !                                                    ! Workspace for message transfers avoiding mpi_allgather
      INTEGER                             ::   ipf_j       ! sum of lines for all multi fields
      INTEGER                             ::   js          ! counter
      INTEGER, DIMENSION(:,:),          ALLOCATABLE ::   jj_s  ! position of sent lines
      INTEGER, DIMENSION(:),            ALLOCATABLE ::   ipj_s ! number of sent lines
      REAL(wp), DIMENSION(:,:,:)      , ALLOCATABLE ::   ztabl
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   ztab, ztabr
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   znorthloc, zfoldwk      
      REAL(wp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ipk = 1   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( l_north_nogather ) THEN      !==  no allgather exchanges  ==!

         ALLOCATE(ipj_s(ipf))

         ipj      = 2            ! Max 2nd dimension of message transfers (last two j-line only)
         ipj_s(:) = 1            ! Real 2nd dimension of message transfers (depending on perf requirement)
                                 ! by default, only one line is exchanged

         ALLOCATE( jj_s(ipf,2) )

         ! re-define number of exchanged lines :
         !  must be two during the first two time steps
         !  to correct possible incoherent values on North fold lines from restart 

         !!!!!!!!!           temporary switch off this optimisation ==> force TRUE           !!!!!!!!
         !!!!!!!!!  needed to get the same results without agrif and with agrif and no zoom  !!!!!!!!
         !!!!!!!!!                    I don't know why we must do that...                    !!!!!!!!
         l_full_nf_update = .TRUE.

         ! Two lines update (slower but necessary to avoid different values ion identical grid points
         IF ( l_full_nf_update .OR.                          &    ! if coupling fields
              ( ncom_stp == nit000 .AND. .NOT. ln_rstart ) ) &    ! at first time step, if not restart
            ipj_s(:) = 2

         ! Index of modifying lines in input
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            !
            SELECT CASE ( npolj )
            !
            CASE ( 3, 4 )                       ! *  North fold  T-point pivot
               !
               SELECT CASE ( cd_nat(jf) )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 3 ;  jj_s(jf,2) = nlcj - 2
               END SELECT
            !
            CASE ( 5, 6 )                        ! *  North fold  F-point pivot
               SELECT CASE ( cd_nat(jf) )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 1      
                  ipj_s(jf) = 1                  ! need only one line anyway
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               END SELECT
            !
            END SELECT
            !
         ENDDO
         ! 
         ipf_j = sum (ipj_s(:))      ! Total number of lines to be exchanged
         !
         ALLOCATE( znorthloc(jpimax,ipf_j,ipk,ipl,1) )
         !
         js = 0
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            DO jj = 1, ipj_s(jf)
               js = js + 1
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     znorthloc(1:jpi,js,jk,jl,1) = ptab(jf)%pt2d(1:jpi,jj_s(jf,jj))
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipf_j * ipk * ipl
         !
         ALLOCATE( zfoldwk(jpimax,ipf_j,ipk,ipl,1) )
         ALLOCATE( ztabr(jpimax*jpmaxngh,ipj,ipk,ipl,ipf) ) 
         ! when some processors of the north fold are suppressed, 
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined 
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztabr(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         !
         DO jr = 1, nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mppsend( 5, znorthloc, ibuffsize, nfipproc(isendto(jr),jpnj), ml_req_nf(jr) )
            ENDIF
         END DO
         !
         DO jr = 1,nsndto
            iproc = nfipproc(isendto(jr),jpnj)
            IF(iproc /= -1) THEN
               iilb = nimppt(iproc+1)
               ilci = nlcit (iproc+1)
               ildi = nldit (iproc+1)
               ilei = nleit (iproc+1)
               IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
               IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
               iilb = nfiimpp(isendto(jr),jpnj) - nfiimpp(isendto(1),jpnj)
            ENDIF
            IF( iproc /= narea-1 .AND. iproc /= -1 ) THEN
               CALL mpprecv(5, zfoldwk, ibuffsize, iproc)
               js = 0
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  js = js + 1
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = zfoldwk(ji,js,jk,jl,1)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ELSE IF( iproc == narea-1 ) THEN
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = ptab(jf)%pt2d(ji,jj_s(jf,jj))
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ENDIF
         END DO
         DO jr = 1,nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mpi_wait( ml_req_nf(jr), ml_stat, ml_err )
            ENDIF
         END DO
         !
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         ! North fold boundary condition
         !
         DO jf = 1, ipf
            CALL lbc_nfd_nogather( ptab(jf)%pt2d(:,:), ztabr(:,1:ipj_s(jf),:,:,jf), cd_nat (jf), psgn (jf) )
         END DO
         !
         DEALLOCATE( zfoldwk, ztabr, jj_s, ipj_s )
         !
      ELSE                             !==  allgather exchanges  ==!
         !
         ipj   = 4            ! 2nd dimension of message transfers (last j-lines)
         !
         ALLOCATE( znorthloc(jpimax,ipj,ipk,ipl,ipf) )
         !
         DO jf = 1, ipf                ! put in znorthloc the last ipj j-lines of ptab
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj - ipj +1, nlcj
                     ij = jj - nlcj + ipj
                     znorthloc(1:jpi,ij,jk,jl,jf) = ptab(jf)%pt2d(1:jpi,jj)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipj * ipk * ipl * ipf
         !
         ALLOCATE( ztab       (jpiglo,ipj,ipk,ipl,ipf     ) )
         ALLOCATE( znorthgloio(jpimax,ipj,ipk,ipl,ipf,jpni) )
         !
         ! when some processors of the north fold are suppressed,
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztab(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         CALL MPI_ALLGATHER( znorthloc  , ibuffsize, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, ibuffsize, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         ! stop waiting time measurement
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            iilb  = nimppt(iproc)
            ilci  = nlcit (iproc)
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
            IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
            DO jf = 1, ipf
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     DO jj = 1, ipj
                        DO ji = ildi, ilei
                           ztab(ji+iilb-1,jj,jk,jl,jf) = znorthgloio(ji,jj,jk,jl,jf,jr)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
         DO jf = 1, ipf
            CALL lbc_nfd( ztab(:,:,:,:,jf), cd_nat (jf), psgn (jf) )   ! North fold boundary condition
         END DO
         !
         DO jf = 1, ipf
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj-ipj+1, nlcj             ! Scatter back to ARRAY_IN
                     ij = jj - nlcj + ipj
                     DO ji= 1, nlci
                        ptab(jf)%pt2d(ji,jj) = ztab(ji+nimpp-1,ij,jk,jl,jf)
                     END DO
                  END DO
               END DO
            END DO
         END DO
         !
      !
         DEALLOCATE( ztab )
         DEALLOCATE( znorthgloio )
      ENDIF
      !
      DEALLOCATE( znorthloc )
      !
   END SUBROUTINE mpp_nfd_2d_ptr

   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE mpp_nfd_3d( ptab, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      REAL(wp)         , INTENT(inout) ::   ptab(:,:,:)   ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::   ji,  jj,  jk,  jl, jh, jf, jr   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf         ! dimension of the input array
      INTEGER  ::   imigr, iihom, ijhom             ! local integers
      INTEGER  ::   ierr, ibuffsize, ilci, ildi, ilei, iilb
      INTEGER  ::   ij, iproc
      INTEGER, DIMENSION (jpmaxngh)       ::   ml_req_nf   ! for mpi_isend when avoiding mpi_allgather
      INTEGER                             ::   ml_err      ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat     ! for mpi_isend when avoiding mpi_allgather
      !                                                    ! Workspace for message transfers avoiding mpi_allgather
      INTEGER                             ::   ipf_j       ! sum of lines for all multi fields
      INTEGER                             ::   js          ! counter
      INTEGER, DIMENSION(:,:),          ALLOCATABLE ::   jj_s  ! position of sent lines
      INTEGER, DIMENSION(:),            ALLOCATABLE ::   ipj_s ! number of sent lines
      REAL(wp), DIMENSION(:,:,:)      , ALLOCATABLE ::   ztabl
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   ztab, ztabr
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   znorthloc, zfoldwk      
      REAL(wp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( l_north_nogather ) THEN      !==  no allgather exchanges  ==!

         ALLOCATE(ipj_s(ipf))

         ipj      = 2            ! Max 2nd dimension of message transfers (last two j-line only)
         ipj_s(:) = 1            ! Real 2nd dimension of message transfers (depending on perf requirement)
                                 ! by default, only one line is exchanged

         ALLOCATE( jj_s(ipf,2) )

         ! re-define number of exchanged lines :
         !  must be two during the first two time steps
         !  to correct possible incoherent values on North fold lines from restart 

         !!!!!!!!!           temporary switch off this optimisation ==> force TRUE           !!!!!!!!
         !!!!!!!!!  needed to get the same results without agrif and with agrif and no zoom  !!!!!!!!
         !!!!!!!!!                    I don't know why we must do that...                    !!!!!!!!
         l_full_nf_update = .TRUE.

         ! Two lines update (slower but necessary to avoid different values ion identical grid points
         IF ( l_full_nf_update .OR.                          &    ! if coupling fields
              ( ncom_stp == nit000 .AND. .NOT. ln_rstart ) ) &    ! at first time step, if not restart
            ipj_s(:) = 2

         ! Index of modifying lines in input
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            !
            SELECT CASE ( npolj )
            !
            CASE ( 3, 4 )                       ! *  North fold  T-point pivot
               !
               SELECT CASE ( cd_nat )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 3 ;  jj_s(jf,2) = nlcj - 2
               END SELECT
            !
            CASE ( 5, 6 )                        ! *  North fold  F-point pivot
               SELECT CASE ( cd_nat )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 1      
                  ipj_s(jf) = 1                  ! need only one line anyway
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               END SELECT
            !
            END SELECT
            !
         ENDDO
         ! 
         ipf_j = sum (ipj_s(:))      ! Total number of lines to be exchanged
         !
         ALLOCATE( znorthloc(jpimax,ipf_j,ipk,ipl,1) )
         !
         js = 0
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            DO jj = 1, ipj_s(jf)
               js = js + 1
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     znorthloc(1:jpi,js,jk,jl,1) = ptab(1:jpi,jj_s(jf,jj),jk)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipf_j * ipk * ipl
         !
         ALLOCATE( zfoldwk(jpimax,ipf_j,ipk,ipl,1) )
         ALLOCATE( ztabr(jpimax*jpmaxngh,ipj,ipk,ipl,ipf) ) 
         ! when some processors of the north fold are suppressed, 
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined 
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztabr(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         !
         DO jr = 1, nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mppsend( 5, znorthloc, ibuffsize, nfipproc(isendto(jr),jpnj), ml_req_nf(jr) )
            ENDIF
         END DO
         !
         DO jr = 1,nsndto
            iproc = nfipproc(isendto(jr),jpnj)
            IF(iproc /= -1) THEN
               iilb = nimppt(iproc+1)
               ilci = nlcit (iproc+1)
               ildi = nldit (iproc+1)
               ilei = nleit (iproc+1)
               IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
               IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
               iilb = nfiimpp(isendto(jr),jpnj) - nfiimpp(isendto(1),jpnj)
            ENDIF
            IF( iproc /= narea-1 .AND. iproc /= -1 ) THEN
               CALL mpprecv(5, zfoldwk, ibuffsize, iproc)
               js = 0
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  js = js + 1
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = zfoldwk(ji,js,jk,jl,1)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ELSE IF( iproc == narea-1 ) THEN
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = ptab(ji,jj_s(jf,jj),jk)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ENDIF
         END DO
         DO jr = 1,nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mpi_wait( ml_req_nf(jr), ml_stat, ml_err )
            ENDIF
         END DO
         !
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         ! North fold boundary condition
         !
         DO jf = 1, ipf
            CALL lbc_nfd_nogather( ptab(:,:,:), ztabr(:,1:ipj_s(jf),:,:,jf), cd_nat , psgn  )
         END DO
         !
         DEALLOCATE( zfoldwk, ztabr, jj_s, ipj_s )
         !
      ELSE                             !==  allgather exchanges  ==!
         !
         ipj   = 4            ! 2nd dimension of message transfers (last j-lines)
         !
         ALLOCATE( znorthloc(jpimax,ipj,ipk,ipl,ipf) )
         !
         DO jf = 1, ipf                ! put in znorthloc the last ipj j-lines of ptab
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj - ipj +1, nlcj
                     ij = jj - nlcj + ipj
                     znorthloc(1:jpi,ij,jk,jl,jf) = ptab(1:jpi,jj,jk)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipj * ipk * ipl * ipf
         !
         ALLOCATE( ztab       (jpiglo,ipj,ipk,ipl,ipf     ) )
         ALLOCATE( znorthgloio(jpimax,ipj,ipk,ipl,ipf,jpni) )
         !
         ! when some processors of the north fold are suppressed,
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztab(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         CALL MPI_ALLGATHER( znorthloc  , ibuffsize, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, ibuffsize, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         ! stop waiting time measurement
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            iilb  = nimppt(iproc)
            ilci  = nlcit (iproc)
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
            IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
            DO jf = 1, ipf
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     DO jj = 1, ipj
                        DO ji = ildi, ilei
                           ztab(ji+iilb-1,jj,jk,jl,jf) = znorthgloio(ji,jj,jk,jl,jf,jr)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
         DO jf = 1, ipf
            CALL lbc_nfd( ztab(:,:,:,:,jf), cd_nat , psgn  )   ! North fold boundary condition
         END DO
         !
         DO jf = 1, ipf
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj-ipj+1, nlcj             ! Scatter back to ARRAY_IN
                     ij = jj - nlcj + ipj
                     DO ji= 1, nlci
                        ptab(ji,jj,jk) = ztab(ji+nimpp-1,ij,jk,jl,jf)
                     END DO
                  END DO
               END DO
            END DO
         END DO
         !
      !
         DEALLOCATE( ztab )
         DEALLOCATE( znorthgloio )
      ENDIF
      !
      DEALLOCATE( znorthloc )
      !
   END SUBROUTINE mpp_nfd_3d


   SUBROUTINE mpp_nfd_3d_ptr( ptab, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      TYPE(PTR_3D)     , INTENT(inout) ::   ptab(:)   ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::   ji,  jj,  jk,  jl, jh, jf, jr   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf         ! dimension of the input array
      INTEGER  ::   imigr, iihom, ijhom             ! local integers
      INTEGER  ::   ierr, ibuffsize, ilci, ildi, ilei, iilb
      INTEGER  ::   ij, iproc
      INTEGER, DIMENSION (jpmaxngh)       ::   ml_req_nf   ! for mpi_isend when avoiding mpi_allgather
      INTEGER                             ::   ml_err      ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat     ! for mpi_isend when avoiding mpi_allgather
      !                                                    ! Workspace for message transfers avoiding mpi_allgather
      INTEGER                             ::   ipf_j       ! sum of lines for all multi fields
      INTEGER                             ::   js          ! counter
      INTEGER, DIMENSION(:,:),          ALLOCATABLE ::   jj_s  ! position of sent lines
      INTEGER, DIMENSION(:),            ALLOCATABLE ::   ipj_s ! number of sent lines
      REAL(wp), DIMENSION(:,:,:)      , ALLOCATABLE ::   ztabl
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   ztab, ztabr
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   znorthloc, zfoldwk      
      REAL(wp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab(1)%pt3d,3)   ! 3rd dimension
      ipl = 1   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( l_north_nogather ) THEN      !==  no allgather exchanges  ==!

         ALLOCATE(ipj_s(ipf))

         ipj      = 2            ! Max 2nd dimension of message transfers (last two j-line only)
         ipj_s(:) = 1            ! Real 2nd dimension of message transfers (depending on perf requirement)
                                 ! by default, only one line is exchanged

         ALLOCATE( jj_s(ipf,2) )

         ! re-define number of exchanged lines :
         !  must be two during the first two time steps
         !  to correct possible incoherent values on North fold lines from restart 

         !!!!!!!!!           temporary switch off this optimisation ==> force TRUE           !!!!!!!!
         !!!!!!!!!  needed to get the same results without agrif and with agrif and no zoom  !!!!!!!!
         !!!!!!!!!                    I don't know why we must do that...                    !!!!!!!!
         l_full_nf_update = .TRUE.

         ! Two lines update (slower but necessary to avoid different values ion identical grid points
         IF ( l_full_nf_update .OR.                          &    ! if coupling fields
              ( ncom_stp == nit000 .AND. .NOT. ln_rstart ) ) &    ! at first time step, if not restart
            ipj_s(:) = 2

         ! Index of modifying lines in input
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            !
            SELECT CASE ( npolj )
            !
            CASE ( 3, 4 )                       ! *  North fold  T-point pivot
               !
               SELECT CASE ( cd_nat(jf) )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 3 ;  jj_s(jf,2) = nlcj - 2
               END SELECT
            !
            CASE ( 5, 6 )                        ! *  North fold  F-point pivot
               SELECT CASE ( cd_nat(jf) )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 1      
                  ipj_s(jf) = 1                  ! need only one line anyway
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               END SELECT
            !
            END SELECT
            !
         ENDDO
         ! 
         ipf_j = sum (ipj_s(:))      ! Total number of lines to be exchanged
         !
         ALLOCATE( znorthloc(jpimax,ipf_j,ipk,ipl,1) )
         !
         js = 0
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            DO jj = 1, ipj_s(jf)
               js = js + 1
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     znorthloc(1:jpi,js,jk,jl,1) = ptab(jf)%pt3d(1:jpi,jj_s(jf,jj),jk)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipf_j * ipk * ipl
         !
         ALLOCATE( zfoldwk(jpimax,ipf_j,ipk,ipl,1) )
         ALLOCATE( ztabr(jpimax*jpmaxngh,ipj,ipk,ipl,ipf) ) 
         ! when some processors of the north fold are suppressed, 
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined 
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztabr(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         !
         DO jr = 1, nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mppsend( 5, znorthloc, ibuffsize, nfipproc(isendto(jr),jpnj), ml_req_nf(jr) )
            ENDIF
         END DO
         !
         DO jr = 1,nsndto
            iproc = nfipproc(isendto(jr),jpnj)
            IF(iproc /= -1) THEN
               iilb = nimppt(iproc+1)
               ilci = nlcit (iproc+1)
               ildi = nldit (iproc+1)
               ilei = nleit (iproc+1)
               IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
               IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
               iilb = nfiimpp(isendto(jr),jpnj) - nfiimpp(isendto(1),jpnj)
            ENDIF
            IF( iproc /= narea-1 .AND. iproc /= -1 ) THEN
               CALL mpprecv(5, zfoldwk, ibuffsize, iproc)
               js = 0
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  js = js + 1
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = zfoldwk(ji,js,jk,jl,1)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ELSE IF( iproc == narea-1 ) THEN
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = ptab(jf)%pt3d(ji,jj_s(jf,jj),jk)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ENDIF
         END DO
         DO jr = 1,nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mpi_wait( ml_req_nf(jr), ml_stat, ml_err )
            ENDIF
         END DO
         !
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         ! North fold boundary condition
         !
         DO jf = 1, ipf
            CALL lbc_nfd_nogather( ptab(jf)%pt3d(:,:,:), ztabr(:,1:ipj_s(jf),:,:,jf), cd_nat (jf), psgn (jf) )
         END DO
         !
         DEALLOCATE( zfoldwk, ztabr, jj_s, ipj_s )
         !
      ELSE                             !==  allgather exchanges  ==!
         !
         ipj   = 4            ! 2nd dimension of message transfers (last j-lines)
         !
         ALLOCATE( znorthloc(jpimax,ipj,ipk,ipl,ipf) )
         !
         DO jf = 1, ipf                ! put in znorthloc the last ipj j-lines of ptab
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj - ipj +1, nlcj
                     ij = jj - nlcj + ipj
                     znorthloc(1:jpi,ij,jk,jl,jf) = ptab(jf)%pt3d(1:jpi,jj,jk)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipj * ipk * ipl * ipf
         !
         ALLOCATE( ztab       (jpiglo,ipj,ipk,ipl,ipf     ) )
         ALLOCATE( znorthgloio(jpimax,ipj,ipk,ipl,ipf,jpni) )
         !
         ! when some processors of the north fold are suppressed,
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztab(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         CALL MPI_ALLGATHER( znorthloc  , ibuffsize, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, ibuffsize, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         ! stop waiting time measurement
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            iilb  = nimppt(iproc)
            ilci  = nlcit (iproc)
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
            IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
            DO jf = 1, ipf
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     DO jj = 1, ipj
                        DO ji = ildi, ilei
                           ztab(ji+iilb-1,jj,jk,jl,jf) = znorthgloio(ji,jj,jk,jl,jf,jr)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
         DO jf = 1, ipf
            CALL lbc_nfd( ztab(:,:,:,:,jf), cd_nat (jf), psgn (jf) )   ! North fold boundary condition
         END DO
         !
         DO jf = 1, ipf
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj-ipj+1, nlcj             ! Scatter back to ARRAY_IN
                     ij = jj - nlcj + ipj
                     DO ji= 1, nlci
                        ptab(jf)%pt3d(ji,jj,jk) = ztab(ji+nimpp-1,ij,jk,jl,jf)
                     END DO
                  END DO
               END DO
            END DO
         END DO
         !
      !
         DEALLOCATE( ztab )
         DEALLOCATE( znorthgloio )
      ENDIF
      !
      DEALLOCATE( znorthloc )
      !
   END SUBROUTINE mpp_nfd_3d_ptr

   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
!                          !==  IN: ptab is an array  ==!

   SUBROUTINE mpp_nfd_4d( ptab, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      REAL(wp)         , INTENT(inout) ::   ptab(:,:,:,:)   ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::   ji,  jj,  jk,  jl, jh, jf, jr   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf         ! dimension of the input array
      INTEGER  ::   imigr, iihom, ijhom             ! local integers
      INTEGER  ::   ierr, ibuffsize, ilci, ildi, ilei, iilb
      INTEGER  ::   ij, iproc
      INTEGER, DIMENSION (jpmaxngh)       ::   ml_req_nf   ! for mpi_isend when avoiding mpi_allgather
      INTEGER                             ::   ml_err      ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat     ! for mpi_isend when avoiding mpi_allgather
      !                                                    ! Workspace for message transfers avoiding mpi_allgather
      INTEGER                             ::   ipf_j       ! sum of lines for all multi fields
      INTEGER                             ::   js          ! counter
      INTEGER, DIMENSION(:,:),          ALLOCATABLE ::   jj_s  ! position of sent lines
      INTEGER, DIMENSION(:),            ALLOCATABLE ::   ipj_s ! number of sent lines
      REAL(wp), DIMENSION(:,:,:)      , ALLOCATABLE ::   ztabl
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   ztab, ztabr
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   znorthloc, zfoldwk      
      REAL(wp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = SIZE(ptab,4)   ! 4th    -
      ipf = 1   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( l_north_nogather ) THEN      !==  no allgather exchanges  ==!

         ALLOCATE(ipj_s(ipf))

         ipj      = 2            ! Max 2nd dimension of message transfers (last two j-line only)
         ipj_s(:) = 1            ! Real 2nd dimension of message transfers (depending on perf requirement)
                                 ! by default, only one line is exchanged

         ALLOCATE( jj_s(ipf,2) )

         ! re-define number of exchanged lines :
         !  must be two during the first two time steps
         !  to correct possible incoherent values on North fold lines from restart 

         !!!!!!!!!           temporary switch off this optimisation ==> force TRUE           !!!!!!!!
         !!!!!!!!!  needed to get the same results without agrif and with agrif and no zoom  !!!!!!!!
         !!!!!!!!!                    I don't know why we must do that...                    !!!!!!!!
         l_full_nf_update = .TRUE.

         ! Two lines update (slower but necessary to avoid different values ion identical grid points
         IF ( l_full_nf_update .OR.                          &    ! if coupling fields
              ( ncom_stp == nit000 .AND. .NOT. ln_rstart ) ) &    ! at first time step, if not restart
            ipj_s(:) = 2

         ! Index of modifying lines in input
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            !
            SELECT CASE ( npolj )
            !
            CASE ( 3, 4 )                       ! *  North fold  T-point pivot
               !
               SELECT CASE ( cd_nat )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 3 ;  jj_s(jf,2) = nlcj - 2
               END SELECT
            !
            CASE ( 5, 6 )                        ! *  North fold  F-point pivot
               SELECT CASE ( cd_nat )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 1      
                  ipj_s(jf) = 1                  ! need only one line anyway
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               END SELECT
            !
            END SELECT
            !
         ENDDO
         ! 
         ipf_j = sum (ipj_s(:))      ! Total number of lines to be exchanged
         !
         ALLOCATE( znorthloc(jpimax,ipf_j,ipk,ipl,1) )
         !
         js = 0
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            DO jj = 1, ipj_s(jf)
               js = js + 1
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     znorthloc(1:jpi,js,jk,jl,1) = ptab(1:jpi,jj_s(jf,jj),jk,jl)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipf_j * ipk * ipl
         !
         ALLOCATE( zfoldwk(jpimax,ipf_j,ipk,ipl,1) )
         ALLOCATE( ztabr(jpimax*jpmaxngh,ipj,ipk,ipl,ipf) ) 
         ! when some processors of the north fold are suppressed, 
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined 
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztabr(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         !
         DO jr = 1, nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mppsend( 5, znorthloc, ibuffsize, nfipproc(isendto(jr),jpnj), ml_req_nf(jr) )
            ENDIF
         END DO
         !
         DO jr = 1,nsndto
            iproc = nfipproc(isendto(jr),jpnj)
            IF(iproc /= -1) THEN
               iilb = nimppt(iproc+1)
               ilci = nlcit (iproc+1)
               ildi = nldit (iproc+1)
               ilei = nleit (iproc+1)
               IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
               IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
               iilb = nfiimpp(isendto(jr),jpnj) - nfiimpp(isendto(1),jpnj)
            ENDIF
            IF( iproc /= narea-1 .AND. iproc /= -1 ) THEN
               CALL mpprecv(5, zfoldwk, ibuffsize, iproc)
               js = 0
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  js = js + 1
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = zfoldwk(ji,js,jk,jl,1)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ELSE IF( iproc == narea-1 ) THEN
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = ptab(ji,jj_s(jf,jj),jk,jl)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ENDIF
         END DO
         DO jr = 1,nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mpi_wait( ml_req_nf(jr), ml_stat, ml_err )
            ENDIF
         END DO
         !
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         ! North fold boundary condition
         !
         DO jf = 1, ipf
            CALL lbc_nfd_nogather( ptab(:,:,:,:), ztabr(:,1:ipj_s(jf),:,:,jf), cd_nat , psgn  )
         END DO
         !
         DEALLOCATE( zfoldwk, ztabr, jj_s, ipj_s )
         !
      ELSE                             !==  allgather exchanges  ==!
         !
         ipj   = 4            ! 2nd dimension of message transfers (last j-lines)
         !
         ALLOCATE( znorthloc(jpimax,ipj,ipk,ipl,ipf) )
         !
         DO jf = 1, ipf                ! put in znorthloc the last ipj j-lines of ptab
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj - ipj +1, nlcj
                     ij = jj - nlcj + ipj
                     znorthloc(1:jpi,ij,jk,jl,jf) = ptab(1:jpi,jj,jk,jl)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipj * ipk * ipl * ipf
         !
         ALLOCATE( ztab       (jpiglo,ipj,ipk,ipl,ipf     ) )
         ALLOCATE( znorthgloio(jpimax,ipj,ipk,ipl,ipf,jpni) )
         !
         ! when some processors of the north fold are suppressed,
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztab(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         CALL MPI_ALLGATHER( znorthloc  , ibuffsize, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, ibuffsize, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         ! stop waiting time measurement
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            iilb  = nimppt(iproc)
            ilci  = nlcit (iproc)
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
            IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
            DO jf = 1, ipf
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     DO jj = 1, ipj
                        DO ji = ildi, ilei
                           ztab(ji+iilb-1,jj,jk,jl,jf) = znorthgloio(ji,jj,jk,jl,jf,jr)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
         DO jf = 1, ipf
            CALL lbc_nfd( ztab(:,:,:,:,jf), cd_nat , psgn  )   ! North fold boundary condition
         END DO
         !
         DO jf = 1, ipf
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj-ipj+1, nlcj             ! Scatter back to ARRAY_IN
                     ij = jj - nlcj + ipj
                     DO ji= 1, nlci
                        ptab(ji,jj,jk,jl) = ztab(ji+nimpp-1,ij,jk,jl,jf)
                     END DO
                  END DO
               END DO
            END DO
         END DO
         !
      !
         DEALLOCATE( ztab )
         DEALLOCATE( znorthgloio )
      ENDIF
      !
      DEALLOCATE( znorthloc )
      !
   END SUBROUTINE mpp_nfd_4d


   SUBROUTINE mpp_nfd_4d_ptr( ptab, cd_nat, psgn, kfld )
      !!----------------------------------------------------------------------
      TYPE(PTR_4D)     , INTENT(inout) ::   ptab(:)   ! array or pointer of arrays on which the boundary condition is applied
      CHARACTER(len=1) , INTENT(in   ) ::   cd_nat(:)   ! nature of array grid-points
      REAL(wp)         , INTENT(in   ) ::   psgn(:)   ! sign used across the north fold boundary
      INTEGER, OPTIONAL, INTENT(in   ) ::   kfld        ! number of pt3d arrays
      !
      INTEGER  ::   ji,  jj,  jk,  jl, jh, jf, jr   ! dummy loop indices
      INTEGER  ::   ipi, ipj, ipk, ipl, ipf         ! dimension of the input array
      INTEGER  ::   imigr, iihom, ijhom             ! local integers
      INTEGER  ::   ierr, ibuffsize, ilci, ildi, ilei, iilb
      INTEGER  ::   ij, iproc
      INTEGER, DIMENSION (jpmaxngh)       ::   ml_req_nf   ! for mpi_isend when avoiding mpi_allgather
      INTEGER                             ::   ml_err      ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat     ! for mpi_isend when avoiding mpi_allgather
      !                                                    ! Workspace for message transfers avoiding mpi_allgather
      INTEGER                             ::   ipf_j       ! sum of lines for all multi fields
      INTEGER                             ::   js          ! counter
      INTEGER, DIMENSION(:,:),          ALLOCATABLE ::   jj_s  ! position of sent lines
      INTEGER, DIMENSION(:),            ALLOCATABLE ::   ipj_s ! number of sent lines
      REAL(wp), DIMENSION(:,:,:)      , ALLOCATABLE ::   ztabl
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   ztab, ztabr
      REAL(wp), DIMENSION(:,:,:,:,:)  , ALLOCATABLE ::   znorthloc, zfoldwk      
      REAL(wp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ipk = SIZE(ptab(1)%pt4d,3)   ! 3rd dimension
      ipl = SIZE(ptab(1)%pt4d,4)   ! 4th    -
      ipf = kfld   ! 5th    -      use in "multi" case (array of pointers)
      !
      IF( l_north_nogather ) THEN      !==  no allgather exchanges  ==!

         ALLOCATE(ipj_s(ipf))

         ipj      = 2            ! Max 2nd dimension of message transfers (last two j-line only)
         ipj_s(:) = 1            ! Real 2nd dimension of message transfers (depending on perf requirement)
                                 ! by default, only one line is exchanged

         ALLOCATE( jj_s(ipf,2) )

         ! re-define number of exchanged lines :
         !  must be two during the first two time steps
         !  to correct possible incoherent values on North fold lines from restart 

         !!!!!!!!!           temporary switch off this optimisation ==> force TRUE           !!!!!!!!
         !!!!!!!!!  needed to get the same results without agrif and with agrif and no zoom  !!!!!!!!
         !!!!!!!!!                    I don't know why we must do that...                    !!!!!!!!
         l_full_nf_update = .TRUE.

         ! Two lines update (slower but necessary to avoid different values ion identical grid points
         IF ( l_full_nf_update .OR.                          &    ! if coupling fields
              ( ncom_stp == nit000 .AND. .NOT. ln_rstart ) ) &    ! at first time step, if not restart
            ipj_s(:) = 2

         ! Index of modifying lines in input
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            !
            SELECT CASE ( npolj )
            !
            CASE ( 3, 4 )                       ! *  North fold  T-point pivot
               !
               SELECT CASE ( cd_nat(jf) )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 3 ;  jj_s(jf,2) = nlcj - 2
               END SELECT
            !
            CASE ( 5, 6 )                        ! *  North fold  F-point pivot
               SELECT CASE ( cd_nat(jf) )
               !
               CASE ( 'T' , 'W' ,'U' )                            ! T-, U-, W-point
                  jj_s(jf,1) = nlcj - 1      
                  ipj_s(jf) = 1                  ! need only one line anyway
               CASE ( 'V' , 'F' )                                 ! V-, F-point
                  jj_s(jf,1) = nlcj - 2 ;  jj_s(jf,2) = nlcj - 1
               END SELECT
            !
            END SELECT
            !
         ENDDO
         ! 
         ipf_j = sum (ipj_s(:))      ! Total number of lines to be exchanged
         !
         ALLOCATE( znorthloc(jpimax,ipf_j,ipk,ipl,1) )
         !
         js = 0
         DO jf = 1, ipf                      ! Loop over the number of arrays to be processed
            DO jj = 1, ipj_s(jf)
               js = js + 1
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     znorthloc(1:jpi,js,jk,jl,1) = ptab(jf)%pt4d(1:jpi,jj_s(jf,jj),jk,jl)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipf_j * ipk * ipl
         !
         ALLOCATE( zfoldwk(jpimax,ipf_j,ipk,ipl,1) )
         ALLOCATE( ztabr(jpimax*jpmaxngh,ipj,ipk,ipl,ipf) ) 
         ! when some processors of the north fold are suppressed, 
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined 
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztabr(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         !
         DO jr = 1, nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mppsend( 5, znorthloc, ibuffsize, nfipproc(isendto(jr),jpnj), ml_req_nf(jr) )
            ENDIF
         END DO
         !
         DO jr = 1,nsndto
            iproc = nfipproc(isendto(jr),jpnj)
            IF(iproc /= -1) THEN
               iilb = nimppt(iproc+1)
               ilci = nlcit (iproc+1)
               ildi = nldit (iproc+1)
               ilei = nleit (iproc+1)
               IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
               IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
               iilb = nfiimpp(isendto(jr),jpnj) - nfiimpp(isendto(1),jpnj)
            ENDIF
            IF( iproc /= narea-1 .AND. iproc /= -1 ) THEN
               CALL mpprecv(5, zfoldwk, ibuffsize, iproc)
               js = 0
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  js = js + 1
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = zfoldwk(ji,js,jk,jl,1)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ELSE IF( iproc == narea-1 ) THEN
               DO jf = 1, ipf ; DO jj = 1, ipj_s(jf)
                  DO jl = 1, ipl
                     DO jk = 1, ipk
                        DO ji = ildi, ilei
                           ztabr(iilb+ji,jj,jk,jl,jf) = ptab(jf)%pt4d(ji,jj_s(jf,jj),jk,jl)
                        END DO
                     END DO
                  END DO
               END DO; END DO
            ENDIF
         END DO
         DO jr = 1,nsndto
            IF( nfipproc(isendto(jr),jpnj) /= narea-1 .AND. nfipproc(isendto(jr),jpnj) /= -1 ) THEN
               CALL mpi_wait( ml_req_nf(jr), ml_stat, ml_err )
            ENDIF
         END DO
         !
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         ! North fold boundary condition
         !
         DO jf = 1, ipf
            CALL lbc_nfd_nogather( ptab(jf)%pt4d(:,:,:,:), ztabr(:,1:ipj_s(jf),:,:,jf), cd_nat (jf), psgn (jf) )
         END DO
         !
         DEALLOCATE( zfoldwk, ztabr, jj_s, ipj_s )
         !
      ELSE                             !==  allgather exchanges  ==!
         !
         ipj   = 4            ! 2nd dimension of message transfers (last j-lines)
         !
         ALLOCATE( znorthloc(jpimax,ipj,ipk,ipl,ipf) )
         !
         DO jf = 1, ipf                ! put in znorthloc the last ipj j-lines of ptab
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj - ipj +1, nlcj
                     ij = jj - nlcj + ipj
                     znorthloc(1:jpi,ij,jk,jl,jf) = ptab(jf)%pt4d(1:jpi,jj,jk,jl)
                  END DO
               END DO
            END DO
         END DO
         !
         ibuffsize = jpimax * ipj * ipk * ipl * ipf
         !
         ALLOCATE( ztab       (jpiglo,ipj,ipk,ipl,ipf     ) )
         ALLOCATE( znorthgloio(jpimax,ipj,ipk,ipl,ipf,jpni) )
         !
         ! when some processors of the north fold are suppressed,
         ! values of ztab* arrays corresponding to these suppressed domain won't be defined
         ! and we need a default definition to 0.
         ! a better test should be: a testing if "suppressed land-processors" belongs to the north-pole folding
         IF ( jpni*jpnj /= jpnij ) ztab(:,:,:,:,:) = 0._wp
         !
         ! start waiting time measurement
         IF( ln_timing ) CALL tic_tac(.TRUE.)
         CALL MPI_ALLGATHER( znorthloc  , ibuffsize, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, ibuffsize, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         ! stop waiting time measurement
         IF( ln_timing ) CALL tic_tac(.FALSE.)
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            iilb  = nimppt(iproc)
            ilci  = nlcit (iproc)
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            IF( iilb            ==      1 )   ildi = 1      ! e-w boundary already done -> force to take 1st column
            IF( iilb + ilci - 1 == jpiglo )   ilei = ilci   ! e-w boundary already done -> force to take last column
            DO jf = 1, ipf
               DO jl = 1, ipl
                  DO jk = 1, ipk
                     DO jj = 1, ipj
                        DO ji = ildi, ilei
                           ztab(ji+iilb-1,jj,jk,jl,jf) = znorthgloio(ji,jj,jk,jl,jf,jr)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
         DO jf = 1, ipf
            CALL lbc_nfd( ztab(:,:,:,:,jf), cd_nat (jf), psgn (jf) )   ! North fold boundary condition
         END DO
         !
         DO jf = 1, ipf
            DO jl = 1, ipl
               DO jk = 1, ipk
                  DO jj = nlcj-ipj+1, nlcj             ! Scatter back to ARRAY_IN
                     ij = jj - nlcj + ipj
                     DO ji= 1, nlci
                        ptab(jf)%pt4d(ji,jj,jk,jl) = ztab(ji+nimpp-1,ij,jk,jl,jf)
                     END DO
                  END DO
               END DO
            END DO
         END DO
         !
      !
         DEALLOCATE( ztab )
         DEALLOCATE( znorthgloio )
      ENDIF
      !
      DEALLOCATE( znorthloc )
      !
   END SUBROUTINE mpp_nfd_4d_ptr



   !!======================================================================



   SUBROUTINE mpp_lbc_north_icb( pt2d, cd_type, psgn, kextj)
      !!---------------------------------------------------------------------
      !!                   ***  routine mpp_lbc_north_icb  ***
      !!
      !! ** Purpose :   Ensure proper north fold horizontal bondary condition
      !!              in mpp configuration in case of jpn1 > 1 and for 2d
      !!              array with outer extra halo
      !!
      !! ** Method  :   North fold condition and mpp with more than one proc
      !!              in i-direction require a specific treatment. We gather
      !!              the 4+kextj northern lines of the global domain on 1
      !!              processor and apply lbc north-fold on this sub array.
      !!              Then we scatter the north fold array back to the processors.
      !!              This routine accounts for an extra halo with icebergs
      !!              and assumes ghost rows and columns have been suppressed.
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   pt2d     ! 2D array with extra halo
      CHARACTER(len=1)        , INTENT(in   ) ::   cd_type  ! nature of pt3d grid-points
      !                                                     !   = T ,  U , V , F or W -points
      REAL(wp)                , INTENT(in   ) ::   psgn     ! = -1. the sign change across the
      !!                                                    ! north fold, =  1. otherwise
      INTEGER                 , INTENT(in   ) ::   kextj    ! Extra halo width at north fold
      !
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ipj, ij, iproc
      !
      REAL(wp), DIMENSION(:,:)  , ALLOCATABLE  ::  ztab_e, znorthloc_e
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE  ::  znorthgloio_e
      !!----------------------------------------------------------------------
      !
      ipj=4
      ALLOCATE(        ztab_e(jpiglo, 1-kextj:ipj+kextj)       ,       &
     &            znorthloc_e(jpimax, 1-kextj:ipj+kextj)       ,       &
     &          znorthgloio_e(jpimax, 1-kextj:ipj+kextj,jpni)    )
      !
      ztab_e(:,:)      = 0._wp
      znorthloc_e(:,:) = 0._wp
      !
      ij = 1 - kextj
      ! put the last ipj+2*kextj lines of pt2d into znorthloc_e 
      DO jj = jpj - ipj + 1 - kextj , jpj + kextj
         znorthloc_e(1:jpi,ij)=pt2d(1:jpi,jj)
         ij = ij + 1
      END DO
      !
      itaille = jpimax * ( ipj + 2*kextj )
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      CALL MPI_ALLGATHER( znorthloc_e(1,1-kextj)    , itaille, MPI_DOUBLE_PRECISION,    &
         &                znorthgloio_e(1,1-kextj,1), itaille, MPI_DOUBLE_PRECISION,    &
         &                ncomm_north, ierr )
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      DO jr = 1, ndim_rank_north            ! recover the global north array
         iproc = nrank_north(jr) + 1
         ildi = nldit (iproc)
         ilei = nleit (iproc)
         iilb = nimppt(iproc)
         DO jj = 1-kextj, ipj+kextj
            DO ji = ildi, ilei
               ztab_e(ji+iilb-1,jj) = znorthgloio_e(ji,jj,jr)
            END DO
         END DO
      END DO

      ! 2. North-Fold boundary conditions
      ! ----------------------------------
      CALL lbc_nfd( ztab_e(:,1-kextj:ipj+kextj), cd_type, psgn, kextj )

      ij = 1 - kextj
      !! Scatter back to pt2d
      DO jj = jpj - ipj + 1 - kextj , jpj + kextj
         DO ji= 1, jpi
            pt2d(ji,jj) = ztab_e(ji+nimpp-1,ij)
         END DO
         ij  = ij +1
      END DO
      !
      DEALLOCATE( ztab_e, znorthloc_e, znorthgloio_e )
      !
   END SUBROUTINE mpp_lbc_north_icb


   SUBROUTINE mpp_lnk_2d_icb( cdname, pt2d, cd_type, psgn, kexti, kextj )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpp_lnk_2d_icb  ***
      !!
      !! ** Purpose :   Message passing management for 2d array (with extra halo for icebergs)
      !!                This routine receives a (1-kexti:jpi+kexti,1-kexti:jpj+kextj)
      !!                array (usually (0:jpi+1, 0:jpj+1)) from lbc_lnk_icb calls.
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask
      !!      between processors following neighboring subdomains.
      !!            domain parameters
      !!                    jpi    : first dimension of the local subdomain
      !!                    jpj    : second dimension of the local subdomain
      !!                    kexti  : number of columns for extra outer halo
      !!                    kextj  : number of rows for extra outer halo
      !!                    nbondi : mark for "east-west local boundary"
      !!                    nbondj : mark for "north-south local boundary"
      !!                    noea   : number for local neighboring processors
      !!                    nowe   : number for local neighboring processors
      !!                    noso   : number for local neighboring processors
      !!                    nono   : number for local neighboring processors
      !!----------------------------------------------------------------------
      CHARACTER(len=*)                                        , INTENT(in   ) ::   cdname      ! name of the calling subroutine
      REAL(wp), DIMENSION(1-kexti:jpi+kexti,1-kextj:jpj+kextj), INTENT(inout) ::   pt2d     ! 2D array with extra halo
      CHARACTER(len=1)                                        , INTENT(in   ) ::   cd_type  ! nature of ptab array grid-points
      REAL(wp)                                                , INTENT(in   ) ::   psgn     ! sign used across the north fold
      INTEGER                                                 , INTENT(in   ) ::   kexti    ! extra i-halo width
      INTEGER                                                 , INTENT(in   ) ::   kextj    ! extra j-halo width
      !
      INTEGER  ::   jl   ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! local integers
      INTEGER  ::   ipreci, iprecj             !   -       -
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat   ! for key_mpi_isend
      !!
      REAL(wp), DIMENSION(1-kexti:jpi+kexti,nn_hls+kextj,2) ::   r2dns, r2dsn
      REAL(wp), DIMENSION(1-kextj:jpj+kextj,nn_hls+kexti,2) ::   r2dwe, r2dew
      !!----------------------------------------------------------------------

      ipreci = nn_hls + kexti      ! take into account outer extra 2D overlap area
      iprecj = nn_hls + kextj

      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, 1, 1, 1, ld_lbc = .TRUE. )

      ! 1. standard boundary treatment
      ! ------------------------------
      ! Order matters Here !!!!
      !
      !                                      ! East-West boundaries
      !                                           !* Cyclic east-west
      IF( l_Iperio ) THEN
         pt2d(1-kexti:     1   ,:) = pt2d(jpim1-kexti: jpim1 ,:)       ! east
         pt2d(  jpi  :jpi+kexti,:) = pt2d(     2     :2+kexti,:)       ! west
         !
      ELSE                                        !* closed
         IF( .NOT. cd_type == 'F' )   pt2d(  1-kexti   :nn_hls   ,:) = 0._wp    ! east except at F-point
                                      pt2d(jpi-nn_hls+1:jpi+kexti,:) = 0._wp    ! west
      ENDIF
      !                                      ! North-South boundaries
      IF( l_Jperio ) THEN                         !* cyclic (only with no mpp j-split)
         pt2d(:,1-kextj:     1   ) = pt2d(:,jpjm1-kextj:  jpjm1)       ! north
         pt2d(:,  jpj  :jpj+kextj) = pt2d(:,     2     :2+kextj)       ! south
      ELSE                                        !* closed
         IF( .NOT. cd_type == 'F' )   pt2d(:,  1-kextj   :nn_hls   ) = 0._wp    ! north except at F-point
                                      pt2d(:,jpj-nn_hls+1:jpj+kextj) = 0._wp    ! south
      ENDIF
      !

      ! north fold treatment
      ! -----------------------
      IF( npolj /= 0 ) THEN
         !
         SELECT CASE ( jpni )
                   CASE ( 1 )     ;   CALL lbc_nfd          ( pt2d(1:jpi,1-kextj:jpj+kextj), cd_type, psgn, kextj )
                   CASE DEFAULT   ;   CALL mpp_lbc_north_icb( pt2d(1:jpi,1:jpj+kextj), cd_type, psgn, kextj )
         END SELECT
         !
      ENDIF

      ! 2. East and west directions exchange
      ! ------------------------------------
      ! we play with the neigbours AND the row number because of the periodicity
      !
      SELECT CASE ( nbondi )      ! Read Dirichlet lateral conditions
      CASE ( -1, 0, 1 )                ! all exept 2 (i.e. close case)
         iihom = jpi-nreci-kexti
         DO jl = 1, ipreci
            r2dew(:,jl,1) = pt2d(nn_hls+jl,:)
            r2dwe(:,jl,1) = pt2d(iihom +jl,:)
         END DO
      END SELECT
      !
      !                           ! Migrations
      imigr = ipreci * ( jpj + 2*kextj )
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         CALL mppsend( 2, r2dwe(1-kextj,1,1), imigr, noea, ml_req1 )
         CALL mpprecv( 1, r2dew(1-kextj,1,2), imigr, noea )
         CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 1, r2dew(1-kextj,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, r2dwe(1-kextj,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, r2dew(1-kextj,1,2), imigr, noea )
         CALL mpprecv( 2, r2dwe(1-kextj,1,2), imigr, nowe )
         CALL mpi_wait(ml_req1,ml_stat,ml_err)
         CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 1, r2dew(1-kextj,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, r2dwe(1-kextj,1,2), imigr, nowe )
         CALL mpi_wait(ml_req1,ml_stat,ml_err)
      END SELECT
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !                           ! Write Dirichlet lateral conditions
      iihom = jpi - nn_hls
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         DO jl = 1, ipreci
            pt2d(iihom+jl,:) = r2dew(:,jl,2)
         END DO
      CASE ( 0 )
         DO jl = 1, ipreci
            pt2d(jl-kexti,:) = r2dwe(:,jl,2)
            pt2d(iihom+jl,:) = r2dew(:,jl,2)
         END DO
      CASE ( 1 )
         DO jl = 1, ipreci
            pt2d(jl-kexti,:) = r2dwe(:,jl,2)
         END DO
      END SELECT


      ! 3. North and south directions
      ! -----------------------------
      ! always closed : we play only with the neigbours
      !
      IF( nbondj /= 2 ) THEN      ! Read Dirichlet lateral conditions
         ijhom = jpj-nrecj-kextj
         DO jl = 1, iprecj
            r2dsn(:,jl,1) = pt2d(:,ijhom +jl)
            r2dns(:,jl,1) = pt2d(:,nn_hls+jl)
         END DO
      ENDIF
      !
      !                           ! Migrations
      imigr = iprecj * ( jpi + 2*kexti )
      !
      IF( ln_timing ) CALL tic_tac(.TRUE.)
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         CALL mppsend( 4, r2dsn(1-kexti,1,1), imigr, nono, ml_req1 )
         CALL mpprecv( 3, r2dns(1-kexti,1,2), imigr, nono )
         CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 3, r2dns(1-kexti,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, r2dsn(1-kexti,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, r2dns(1-kexti,1,2), imigr, nono )
         CALL mpprecv( 4, r2dsn(1-kexti,1,2), imigr, noso )
         CALL mpi_wait(ml_req1,ml_stat,ml_err)
         CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 3, r2dns(1-kexti,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, r2dsn(1-kexti,1,2), imigr, noso )
         CALL mpi_wait(ml_req1,ml_stat,ml_err)
      END SELECT
      !
      IF( ln_timing ) CALL tic_tac(.FALSE.)
      !
      !                           ! Write Dirichlet lateral conditions
      ijhom = jpj - nn_hls
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         DO jl = 1, iprecj
            pt2d(:,ijhom+jl) = r2dns(:,jl,2)
         END DO
      CASE ( 0 )
         DO jl = 1, iprecj
            pt2d(:,jl-kextj) = r2dsn(:,jl,2)
            pt2d(:,ijhom+jl) = r2dns(:,jl,2)
         END DO
      CASE ( 1 )
         DO jl = 1, iprecj
            pt2d(:,jl-kextj) = r2dsn(:,jl,2)
         END DO
      END SELECT
      !
   END SUBROUTINE mpp_lnk_2d_icb
   
END MODULE lbclnk

