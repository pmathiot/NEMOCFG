MODULE prtctl_trc
   !!======================================================================
   !!                         ***  MODULE prtctl_trc  ***
   !! TOP :   print all SUM trends for each processor domain
   !!======================================================================
   !! History :    -   !  2005-07  (C. Talandier) original code for OPA
   !!             1.0  !  2005-10  (C. Ethe     ) adapted to passive tracer
   !!----------------------------------------------------------------------
   !!   prt_ctl_trc      :   control print in mpp for passive tracers
   !!   prt_ctl_trc_info :   ???
   !!   prt_ctl_trc_init :   ???
   !!----------------------------------------------------------------------
   USE par_trc          ! TOP parameters
   USE oce_trc          ! ocean space and time domain variables
   USE prtctl           ! print control for OPA

   IMPLICIT NONE
   PRIVATE

   INTEGER , DIMENSION(:), ALLOCATABLE ::   numid_trc          !: logical unit 
   INTEGER , DIMENSION(:), ALLOCATABLE ::   nlditl , nldjtl    !: first, last indoor index for each i-domain
   INTEGER , DIMENSION(:), ALLOCATABLE ::   nleitl , nlejtl    !: first, last indoor index for each j-domain
   INTEGER , DIMENSION(:), ALLOCATABLE ::   nimpptl, njmpptl   !: i-, j-indexes for each processor
   INTEGER , DIMENSION(:), ALLOCATABLE ::   nlcitl , nlcjtl    !: dimensions of every subdomain
   INTEGER , DIMENSION(:), ALLOCATABLE ::   ibonitl, ibonjtl

   REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   tra_ctl         !: previous trend values

   PUBLIC prt_ctl_trc         ! called by all subroutines
   PUBLIC prt_ctl_trc_info    !
   PUBLIC prt_ctl_trc_init    ! called by opa.F90

CONTAINS

   SUBROUTINE prt_ctl_trc( tab4d, mask, clinfo, ovlap, kdim, clinfo2 )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl  ***
      !!
      !! ** Purpose : - print sum control 3D arrays over the same area 
      !!                in mono and mpp case. This way can be usefull when
      !!                debugging a new parametrization in mono or mpp. 
      !!
      !! ** Method  : 2 possibilities exist when setting the ln_ctl parameter to
      !!                .true. in the ocean namelist:
      !!              - to debug a MPI run .vs. a mono-processor one; 
      !!                the control print will be done over each sub-domain.
      !!                The nictl[se] and njctl[se] parameters in the namelist must 
      !!                be set to zero and [ij]splt to the corresponding splitted
      !!                domain in MPI along respectively i-, j- directions.
      !!              - to debug a mono-processor run over the whole domain/a specific area; 
      !!                in the first case the nictl[se] and njctl[se] parameters must be set
      !!                to zero else to the indices of the area to be controled. In both cases
      !!                isplt and jsplt must be set to 1.
      !!              - All arguments of the above calling sequence are optional so their
      !!                name must be explicitly typed if used. For instance if the mask
      !!                array tmask(:,:,:) must be passed through the prt_ctl subroutine, 
      !!                it must looks like: CALL prt_ctl( mask=tmask ).
      !!----------------------------------------------------------------------
      REAL(wp)         , DIMENSION(:,:,:,:), INTENT(in), OPTIONAL ::   tab4d     ! 4D array
      REAL(wp)         , DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask      ! 3D mask to apply to the tab4d array
      CHARACTER (len=*), DIMENSION(:)      , INTENT(in), OPTIONAL ::   clinfo    ! information about the tab3d array
      CHARACTER (len=*)                    , INTENT(in), OPTIONAL ::   clinfo2   ! ???
      INTEGER                              , INTENT(in), OPTIONAL ::   ovlap     ! overlap value
      INTEGER                              , INTENT(in), OPTIONAL ::   kdim      ! k- direction for 4D arrays
      !!
      INTEGER  ::   overlap, jn, js, sind, eind, kdir, j_id
      REAL(wp) ::   zsum, zvctl
      CHARACTER (len=20), ALLOCATABLE, DIMENSION(:) ::   cl
      CHARACTER (len=10) ::   cl2
      REAL(wp), DIMENSION(jpi,jpj,jpk)  :: zmask, ztab3d 
      !!----------------------------------------------------------------------

      ALLOCATE( cl(jptra) )
      !                                      ! Arrays, scalars initialization 
      overlap       = 0
      kdir          = jpkm1
      zsum          = 0.e0
      zvctl         = 0.e0
      cl(:)         = ''
      cl2           = ''
      ztab3d(:,:,:) = 0.e0
      zmask (:,:,:) = 1.e0

      !                                      ! Control of optional arguments
      IF( PRESENT(ovlap)   )   overlap       = ovlap
      IF( PRESENT(kdim)    )   kdir          = kdim
      IF( PRESENT(clinfo ) )   cl(:)         = clinfo(:)
      IF( PRESENT(clinfo2) )   cl2           = clinfo2
      IF( PRESENT(mask)    )   zmask (:,:,:) = mask(:,:,:)

      IF( lk_mpp )   THEN      ! processor number
         sind = narea
         eind = narea
      ELSE                     ! processors total number
         sind = 1
         eind = ijsplt
      ENDIF

      ! Loop over each sub-domain, i.e. the total number of processors ijsplt
      DO js = sind, eind
         !
         ! Set logical unit
         j_id = numid_trc( js - narea + 1 )
         ! Set indices for the SUM control
         IF( .NOT. lsp_area ) THEN
            IF (lk_mpp )   THEN
               nictls = MAX( 1, nlditl(js) - overlap )
               nictle = nleitl(js) + overlap * MIN( 1, nlcitl(js) - nleitl(js)) 
               njctls = MAX( 1, nldjtl(js) - overlap )
               njctle = nlejtl(js) + overlap * MIN( 1, nlcjtl(js) - nlejtl(js))
               ! Do not take into account the bound of the domain
               IF( ibonitl(js) == -1 .OR. ibonitl(js) == 2 )   nictls = MAX( 2, nictls )
               IF( ibonitl(js) ==  1 .OR. ibonitl(js) == 2 )   nictle = MIN( nictle, nleitl(js) - 1 )
               IF( ibonjtl(js) == -1 .OR. ibonjtl(js) == 2 )   njctls = MAX( 2, njctls )
               IF( ibonjtl(js) ==  1 .OR. ibonjtl(js) == 2 )   njctle = MIN( njctle, nlejtl(js) - 1 )
            ELSE
               nictls = MAX( 1, nimpptl(js) + nlditl(js) - 1 - overlap )
               nictle = nimpptl(js) + nleitl(js) - 1 + overlap * MIN( 1, nlcitl(js) - nleitl(js) ) 
               njctls = MAX( 1, njmpptl(js) + nldjtl(js) - 1 - overlap )
               njctle = njmpptl(js) + nlejtl(js) - 1 + overlap * MIN( 1, nlcjtl(js) - nlejtl(js) ) 
               ! Do not take into account the bound of the domain
               IF( ibonitl(js) == -1 .OR. ibonitl(js) == 2 )   nictls = MAX( 2, nictls )
               IF( ibonjtl(js) == -1 .OR. ibonjtl(js) == 2 )   njctls = MAX( 2, njctls )
               IF( ibonitl(js) ==  1 .OR. ibonitl(js) == 2 )   nictle = MIN( nictle, nimpptl(js) + nleitl(js) - 2 )
               IF( ibonjtl(js) ==  1 .OR. ibonjtl(js) == 2 )   njctle = MIN( njctle, njmpptl(js) + nlejtl(js) - 2 )
            ENDIF
         ENDIF
         !
         IF( PRESENT(clinfo2) ) THEN
            DO jn = 1, jptra
               zvctl  = tra_ctl(jn,js)
               ztab3d(:,:,:) = tab4d(:,:,:,jn)
               zsum          = SUM( ztab3d(nictls:nictle,njctls:njctle,1:kdir)   &
                  &                * zmask(nictls:nictle,njctls:njctle,1:kdir) )
               WRITE(j_id,FMT="(3x,a,' : ',D23.16)") cl(jn), zsum-zvctl
               tra_ctl(jn,js) = zsum
            END DO
         ELSE
            DO jn = 1, jptra
               ztab3d(:,:,:) = tab4d(:,:,:,jn)
               zsum          = SUM( ztab3d(nictls:nictle,njctls:njctle,1:kdir)   &
                  &               *  zmask(nictls:nictle,njctls:njctle,1:kdir) )
               WRITE(j_id,FMT="(3x,a,' : ',D23.16)") cl(jn), zsum
            END DO
         ENDIF
         !
      END DO
      !
      DEALLOCATE( cl )
      !
   END SUBROUTINE prt_ctl_trc


   SUBROUTINE prt_ctl_trc_info( clinfo )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_trc_info  ***
      !!
      !! ** Purpose : - print information without any computation
      !!----------------------------------------------------------------------
      CHARACTER (len=*), INTENT(in) ::   clinfo      ! information to print
      !! 
      INTEGER ::   js, sind, eind, j_id
      !!----------------------------------------------------------------------

      IF( lk_mpp ) THEN      ! processor number
         sind = narea
         eind = narea
      ELSE                   ! total number of processors
         sind = 1
         eind = ijsplt
      ENDIF

      ! Loop over each sub-domain, i.e. number of processors ijsplt
      DO js = sind, eind
         j_id = numid_trc(js - narea + 1)
         WRITE(j_id,*) clinfo
      END DO
      !
   END SUBROUTINE prt_ctl_trc_info


   SUBROUTINE prt_ctl_trc_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_trc_init  ***
      !!
      !! ** Purpose :   open ASCII files & compute indices
      !!----------------------------------------------------------------------
      INTEGER            ::   js, sind, eind, j_id
      CHARACTER (len=31) :: clfile_out
      CHARACTER (len=27) :: clb_name
      CHARACTER (len=19) :: cl_run
      !!----------------------------------------------------------------------

      !                             ! Allocate arrays
      ALLOCATE( nlditl (ijsplt) )
      ALLOCATE( nldjtl (ijsplt) )
      ALLOCATE( nleitl (ijsplt) )
      ALLOCATE( nlejtl (ijsplt) )
      ALLOCATE( nimpptl(ijsplt) )
      ALLOCATE( njmpptl(ijsplt) )
      ALLOCATE( nlcitl (ijsplt) )
      ALLOCATE( nlcjtl (ijsplt) )
      ALLOCATE( tra_ctl(jptra,ijsplt) )
      ALLOCATE( ibonitl(ijsplt) )
      ALLOCATE( ibonjtl(ijsplt) )

      tra_ctl(:,:) = 0.e0           ! Initialization to zero

      IF( lk_mpp ) THEN
         sind = narea
         eind = narea
         clb_name = "('mpp.top.output_',I4.4)"
         cl_run = 'MULTI processor run'
         ! use indices for each area computed by mpp_init subroutine
         nlditl(1:jpnij) = nldit(:) 
         nleitl(1:jpnij) = nleit(:) 
         nldjtl(1:jpnij) = nldjt(:) 
         nlejtl(1:jpnij) = nlejt(:) 
         !
         nimpptl(1:jpnij) = nimppt(:)
         njmpptl(1:jpnij) = njmppt(:)
         !
         nlcitl(1:jpnij) = nlcit(:)
         nlcjtl(1:jpnij) = nlcjt(:)
         !
         ibonitl(1:jpnij) = ibonit(:)
         ibonjtl(1:jpnij) = ibonjt(:)
      ELSE
         sind = 1
         eind = ijsplt
         clb_name = "('mono.top.output_',I4.4)"
         cl_run   = 'MONO processor run '
         ! compute indices for each area as done in mpp_init subroutine
         CALL sub_dom
      ENDIF

      ALLOCATE( numid_trc(eind-sind+1) )

      DO js = sind, eind
         WRITE(clfile_out,FMT=clb_name) js-1
         CALL ctl_opn( numid_trc(js -narea + 1), clfile_out, 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
         j_id = numid_trc(js -narea + 1)
         WRITE(j_id,*)
         WRITE(j_id,*) '                 L O D Y C - I P S L'
         WRITE(j_id,*) '                       N E M 0 '
         WRITE(j_id,*) '            Ocean General Circulation Model'
         WRITE(j_id,*) '               version TOP 1.0  (2005) '
         WRITE(j_id,*)
         WRITE(j_id,*) '                   PROC number: ', js
         WRITE(j_id,*)
         WRITE(j_id,FMT="(19x,a20)") cl_run

         ! Print the SUM control indices
         IF( .NOT. lsp_area )   THEN
            IF ( lk_mpp )   THEN
               nictls = nlditl(js) 
               nictle = nleitl(js)
               njctls = nldjtl(js)
               njctle = nlejtl(js)
            ELSE
               nictls = nimpptl(js) + nlditl(js) - 1
               nictle = nimpptl(js) + nleitl(js) - 1
               njctls = njmpptl(js) + nldjtl(js) - 1
               njctle = njmpptl(js) + nlejtl(js) - 1
            ENDIF
         ENDIF
         WRITE(j_id,*) 
         WRITE(j_id,*) 'prt_tra_ctl :  Sum control indices'
         WRITE(j_id,*) '~~~~~~~'
         WRITE(j_id,*)
         WRITE(j_id,9000)'                                nlej   = ', nlejtl(js), '              '
         WRITE(j_id,9000)'                  ------------- njctle = ', njctle, ' -------------'
         WRITE(j_id,9001)'                  |                                       |'
         WRITE(j_id,9001)'                  |                                       |'
         WRITE(j_id,9001)'                  |                                       |'
         WRITE(j_id,9002)'           nictls = ', nictls,  '                           nictle = ', nictle
         WRITE(j_id,9002)'           nldi   = ', nlditl(js),  '                           nlei   = ', nleitl(js)
         WRITE(j_id,9001)'                  |                                       |'
         WRITE(j_id,9001)'                  |                                       |'
         WRITE(j_id,9001)'                  |                                       |'
         WRITE(j_id,9004)'  njmpp  = ',njmpptl(js),'   ------------- njctls = ', njctls, ' -------------'
         WRITE(j_id,9003)'           nimpp  = ', nimpptl(js), '        nldj   = ', nldjtl(js), '              '
         WRITE(j_id,*)
         WRITE(j_id,*)

9000     FORMAT(a41,i4.4,a14)
9001     FORMAT(a59)
9002     FORMAT(a20,i4.4,a36,i3.3)
9003     FORMAT(a20,i4.4,a17,i4.4)
9004     FORMAT(a11,i4.4,a26,i4.4,a14)
      END DO
      !
   END SUBROUTINE prt_ctl_trc_init

END MODULE prtctl_trc
