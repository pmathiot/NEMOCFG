MODULE trcbdy
   !!======================================================================
   !!                       ***  MODULE  bdytrc  ***
   !! Ocean tracers:   Apply boundary conditions for tracers in TOP component
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!            3.5  !  2012     (S. Mocavero, I. Epicoco) Optimization of BDY communications
   !!            3.6  !  2015     (T. Lovato) Adapt BDY for tracers in TOP component
   !!            4.0  !  2016     (T. Lovato) Generalize OBC structure
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   trc_bdy       : Apply open boundary conditions & damping to tracers
   !!----------------------------------------------------------------------
   USE timing                       ! Timing
   USE oce_trc                      ! ocean dynamics and tracers variables
   USE par_trc
   USE trc                          ! ocean space and time domain variables 
   USE bdylib                       ! for orlanski library routines
   USE lbclnk                       ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager               ! I/O manager
   USE bdy_oce                      ! ocean open boundary conditions

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_bdy      ! routine called in trcnxt.F90 
   PUBLIC trc_bdy_dmp  ! routine called in trcstp.F90 

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcbdy.F90 11821 2019-10-29 10:08:08Z acc $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_bdy( kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE trc_bdy  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for TOP tracers
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt     ! Main time step counter
      !!
      INTEGER                           :: ib_bdy ,ir, jn ,igrd ! Loop indices
      REAL(wp), POINTER, DIMENSION(:,:) ::  ztrc
      REAL(wp), POINTER                 ::  zfac
      LOGICAL                           :: llrim0               ! indicate if rim 0 is treated
      LOGICAL, DIMENSION(4)             :: llsend1, llrecv1     ! indicate how communications are to be carried out
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_bdy')
      !
      igrd = 1 
      llsend1(:) = .false.  ;   llrecv1(:) = .false.
      DO ir = 1, 0, -1   ! treat rim 1 before rim 0
         IF( ir == 0 ) THEN   ;   llrim0 = .TRUE.
         ELSE                 ;   llrim0 = .FALSE.
         END IF
         DO ib_bdy=1, nb_bdy
            DO jn = 1, jptra
               !
               ztrc => trcdta_bdy(jn,ib_bdy)%trc 
               zfac => trcdta_bdy(jn,ib_bdy)%rn_fac
               !
               SELECT CASE( TRIM(trcdta_bdy(jn,ib_bdy)%cn_obc) )
               CASE('none'        )   ;   CYCLE
               CASE('frs'         )   ! treat the whole boundary at once
                  IF( ir == 0 ) CALL bdy_frs( idx_bdy(ib_bdy),                tra(:,:,:,jn), ztrc*zfac )
               CASE('specified'   )   ! treat the whole rim      at once
                  IF( ir == 0 ) CALL bdy_spe( idx_bdy(ib_bdy),                tra(:,:,:,jn), ztrc*zfac )
               CASE('neumann'     )   ;   CALL bdy_nmn( idx_bdy(ib_bdy), igrd         , tra(:,:,:,jn) )   ! tra masked
               CASE('orlanski'    )   ;   CALL bdy_orl( idx_bdy(ib_bdy), trb(:,:,:,jn), tra(:,:,:,jn), ztrc*zfac, ll_npo=.false. )
               CASE('orlanski_npo')   ;   CALL bdy_orl( idx_bdy(ib_bdy), trb(:,:,:,jn), tra(:,:,:,jn), ztrc*zfac, ll_npo=.true. )
               CASE DEFAULT           ;   CALL ctl_stop( 'trc_bdy : unrecognised option for open boundaries for passive tracers' )
               END SELECT
               !
            END DO
         END DO
         !
         IF( nn_hls > 1 .AND. ir == 1 ) CYCLE   ! at least 2 halos will be corrected -> no need to correct rim 1 before rim 0
         IF( nn_hls == 1 ) THEN   ;   llsend1(:) = .false.   ;   llrecv1(:) = .false.   ;   END IF
         DO ib_bdy=1, nb_bdy
            SELECT CASE( TRIM(cn_tra(ib_bdy)) )
            CASE('neumann')
               llsend1(:) = llsend1(:) .OR. lsend_bdyint(ib_bdy,1,:,ir)   ! possibly every direction, T points
               llrecv1(:) = llrecv1(:) .OR. lrecv_bdyint(ib_bdy,1,:,ir)   ! possibly every direction, T points
            CASE('orlanski','orlanski_npo')
               llsend1(:) = llsend1(:) .OR. lsend_bdy(ib_bdy,1,:,ir)   ! possibly every direction, T points
               llrecv1(:) = llrecv1(:) .OR. lrecv_bdy(ib_bdy,1,:,ir)   ! possibly every direction, T points
            END SELECT
         END DO
         IF( ANY(llsend1) .OR. ANY(llrecv1) ) THEN   ! if need to send/recv in at least one direction
            CALL lbc_lnk( 'trcbdy', tra, 'T',  1., kfillmode=jpfillnothing ,lsend=llsend1, lrecv=llrecv1 )
         END IF
         !
      END DO   ! ir
      !
      IF( ln_timing )   CALL timing_stop('trc_bdy')
      !
   END SUBROUTINE trc_bdy


   SUBROUTINE trc_bdy_dmp( kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE trc_bdy_dmp  ***
      !!                    
      !! ** Purpose : Apply damping for tracers at open boundaries.
      !!             It currently applies the damping to all tracers!!!
      !! 
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      !! 
      INTEGER  ::   jn             ! Tracer index
      REAL(wp) ::   zwgt           ! boundary weight
      REAL(wp) ::   zta, zsa, ztime
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      INTEGER  ::   ib_bdy         ! Loop index
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_bdy_dmp')
      !
      DO jn = 1, jptra
         DO ib_bdy=1, nb_bdy
            IF( trcdta_bdy(jn, ib_bdy)%dmp ) THEN
               igrd = 1                       ! Everything is at T-points here
               DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  zwgt = idx_bdy(ib_bdy)%nbd(ib,igrd)
                  DO ik = 1, jpkm1
                     zta = zwgt * ( trcdta_bdy(jn, ib_bdy)%trc(ib,ik) - trb(ii,ij,ik,jn) ) * tmask(ii,ij,ik)
                     tra(ii,ij,ik,jn) = tra(ii,ij,ik,jn) + zta
                  END DO
               END DO
            ENDIF
         END DO
      END DO
      !
      IF( ln_timing )   CALL timing_stop('trc_bdy_dmp')
      !
   END SUBROUTINE trc_bdy_dmp
 
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_bdy(kt)      ! Empty routine
      WRITE(*,*) 'trc_bdy: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bdy

   SUBROUTINE trc_bdy_dmp(kt)      ! Empty routine
      WRITE(*,*) 'trc_bdy_dmp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bdy_dmp

#endif

   !!======================================================================
END MODULE trcbdy
