MODULE isfcpl
   !!======================================================================
   !!                       ***  MODULE  isfcpl  ***
   !!
   !! iceshelf coupling module : module managing the coupling between NEMO and an ice sheet model
   !!
   !!======================================================================
   !! History :  4.1  !  2019-07  (P. Mathiot) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfrst : read/write iceshelf variables in/from restart
   !!----------------------------------------------------------------------
   USE isf_oce                          ! ice shelf variable
   USE isfutils, ONLY : debug
   USE lib_mpp , ONLY: mpp_sum, mpp_max ! mpp routine
   USE domvvl  , ONLY: dom_vvl_zgr      ! vertical scale factor interpolation
   !
   USE oce            ! ocean dynamics and tracers
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   !
   IMPLICIT NONE

   PRIVATE

   PUBLIC isfcpl_rst_write, isfcpl_init                    ! iceshelf restart read and write 
   PUBLIC isfcpl_ssh, isfcpl_tra, isfcpl_vol, isfcpl_cons  ! iceshelf correction for ssh, tra, dyn and conservation 

   TYPE isfcons
      INTEGER :: ii     ! i global
      INTEGER :: jj     ! j global
      INTEGER :: kk     ! k level
      REAL(wp):: dvol   ! volume increment
      REAL(wp):: dsal   ! salt increment
      REAL(wp):: dtem   ! heat increment
      REAL(wp):: lon    ! lon
      REAL(wp):: lat    ! lat
      INTEGER :: ngb    ! 0/1 (valid location or not (ie on halo or no neigbourg))
   END TYPE
   !
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE isfcpl_init()
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE iscpl_init  ***
      !! 
      !! ** Purpose : correct ocean state for new wet cell and horizontal divergence 
      !!              correction for the dynamical adjustement
      !!
      !! ** Action : - compute ssh on new wet cell
      !!             - compute T/S on new wet cell
      !!             - compute horizontal divergence correction as a volume flux
      !!             - compute the T/S/vol correction increment to keep trend to 0
      !!
      !!---------------------------------------------------------------------
      INTEGER :: id
      !!----------------------------------------------------------------------
      !
      ! start on an euler time step
      neuler = 0
      ! 
      ! allocation and initialisation to 0
      CALL isf_alloc_cpl()
      !
      ! check presence of variable needed for coupling
      ! iom_varid return 0 if not found
      id = 1
      id = id * iom_varid(numror, 'ssmask', ldstop = .false.)
      id = id * iom_varid(numror, 'tmask' , ldstop = .false.)
      id = id * iom_varid(numror, 'e3t_n' , ldstop = .false.)
      id = id * iom_varid(numror, 'e3u_n' , ldstop = .false.)
      id = id * iom_varid(numror, 'e3v_n' , ldstop = .false.)
      IF(lwp) WRITE(numout,*) ' isfcpl_init:', id
      IF (id == 0) THEN
         IF(lwp) WRITE(numout,*) ' isfcpl_init: restart variables for ice sheet coupling are missing, skip coupling for this leg ' 
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~'
         IF(lwp) WRITE(numout,*) ''
      ELSE
         ! extrapolation ssh
         CALL isfcpl_ssh()
         !
         ! extrapolation tracer properties
         CALL isfcpl_tra()
         !
         ! correction of the horizontal divergence and associated temp. and salt content flux
         ! Need to : - include in the cpl cons the risfcpl_vol/tsc contribution
         !           - decide how to manage thickness level change in conservation
         CALL isfcpl_vol()
         !
         ! apply the 'conservation' method
         IF ( ln_isfcpl_cons ) CALL isfcpl_cons()
         !
      END IF
      !
      ! mask velocity properly (mask used in restart not compatible with new mask)
      un(:,:,:) = un(:,:,:) * umask(:,:,:)
      vn(:,:,:) = vn(:,:,:) * vmask(:,:,:)
      !
      ! all before fields set to now values
      tsb  (:,:,:,:) = tsn  (:,:,:,:)
      ub   (:,:,:)   = un   (:,:,:)
      vb   (:,:,:)   = vn   (:,:,:)
      sshb (:,:)     = sshn (:,:)
      e3t_b(:,:,:)   = e3t_n(:,:,:)
 
      ! prepare writing restart
      IF( lwxios ) THEN
         CALL iom_set_rstw_var_active('ssmask')
         CALL iom_set_rstw_var_active('tmask')
         CALL iom_set_rstw_var_active('e3t_n')
         CALL iom_set_rstw_var_active('e3u_n')
         CALL iom_set_rstw_var_active('e3v_n')
      END IF
      !
   END SUBROUTINE isfcpl_init
   ! 
   SUBROUTINE isfcpl_rst_write(kt)
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE iscpl_rst_write  ***
      !! 
      !! ** Purpose : write icesheet coupling variables in restart
      !!
      !!-------------------------- IN  --------------------------------------
      INTEGER, INTENT(in) :: kt
      !!----------------------------------------------------------------------
      !
      IF( lwxios ) CALL iom_swap( cwxios_context )
      CALL iom_rstput( kt, nitrst, numrow, 'tmask'  , tmask , ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, 'ssmask' , ssmask, ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, 'e3t_n'  , e3t_n , ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, 'e3u_n'  , e3u_n , ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, 'e3v_n'  , e3v_n , ldxios = lwxios )
      CALL iom_rstput( kt, nitrst, numrow, 'gdepw_n', gdepw_n , ldxios = lwxios )
      IF( lwxios ) CALL iom_swap( cxios_context )
      !
   END SUBROUTINE isfcpl_rst_write

   SUBROUTINE isfcpl_ssh()
      !!---------------------------------------------------------------------- 
      !!                   ***  ROUTINE iscpl_ssh  ***
      !! 
      !! ** Purpose :   basic guess of ssh in new wet cell
      !! 
      !! ** Method  :   basic extrapolation from neigbourg cells
      !!
      !!----------------------------------------------------------------------
      !!
      INTEGER :: ji, jj, jd, jk      !! loop index
      INTEGER :: jip1, jim1, jjp1, jjm1
      !!
      REAL(wp):: zsummsk
      REAL(wp), DIMENSION(jpi,jpj) :: zdssmask, zssmask0, zssmask_b, zssh
      !!----------------------------------------------------------------------
      !
      CALL iom_get( numror, jpdom_autoglo, 'ssmask'  , zssmask_b, ldxios = lrxios   ) ! need to extrapolate T/S

      ! compute new ssh if we open a full water column 
      ! rude average of the closest neigbourgs (e1e2t not taking into account)
      !
      zssh(:,:)     = sshn(:,:)
      zssmask0(:,:) = zssmask_b(:,:)
      !
      DO jd = 1, nn_drown
         !
         zdssmask(:,:) = ssmask(:,:) - zssmask0(:,:)
         DO jj = 2,jpj-1
            DO ji = 2,jpi-1
               jip1=ji+1; jim1=ji-1;
               jjp1=jj+1; jjm1=jj-1;
               !
               zsummsk = zssmask0(jip1,jj) + zssmask0(jim1,jj) + zssmask0(ji,jjp1) + zssmask0(ji,jjm1)
               !
               IF (zdssmask(ji,jj) == 1._wp .AND. zsummsk /= 0._wp) THEN
                  sshn(ji,jj)=( zssh(jip1,jj)*zssmask0(jip1,jj)     &
                  &           + zssh(jim1,jj)*zssmask0(jim1,jj)     &
                  &           + zssh(ji,jjp1)*zssmask0(ji,jjp1)     &
                  &           + zssh(ji,jjm1)*zssmask0(ji,jjm1)) / zsummsk
                  zssmask_b(ji,jj) = 1._wp
               ENDIF
            END DO
         END DO
         !
         zssh(:,:) = sshn(:,:)
         zssmask0(:,:) = zssmask_b(:,:)
         !
         CALL lbc_lnk_multi( 'iscplrst', zssh, 'T', 1., zssmask0, 'T', 1. )
         !
      END DO
      !
      ! update sshn
      sshn(:,:) = zssh(:,:) * ssmask(:,:)
      !
      sshb(:,:) = sshn(:,:)
      !
      IF ( ln_isfdebug ) CALL debug('isfcpl_ssh: sshn',sshn(:,:))
      !
      ! recompute the vertical scale factor, depth and water thickness
      IF(lwp) write(numout,*) 'isfcpl_ssh : recompute scale factor from sshn (new wet cell)'
      IF(lwp) write(numout,*) '~~~~~~~~~~~'
      DO jk = 1, jpk
         e3t_n(:,:,jk) =  e3t_0(:,:,jk) * ( ht_0(:,:) + sshn(:,:) ) &
             &                          / ( ht_0(:,:) + 1._wp - ssmask(:,:) ) * tmask(:,:,jk)   &
             &          + e3t_0(:,:,jk)                               * (1._wp -tmask(:,:,jk))
      END DO
      e3t_b(:,:,:) = e3t_n(:,:,:)
      CALL dom_vvl_zgr()
      !
   END SUBROUTINE isfcpl_ssh

   SUBROUTINE isfcpl_tra()
      !!---------------------------------------------------------------------- 
      !!                   ***  ROUTINE iscpl_tra  ***
      !! 
      !! ** Purpose :   compute new tn, sn in case of evolving geometry of ice shelves 
      !! 
      !! ** Method  :   tn, sn : basic extrapolation from neigbourg cells
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ztmask_b
      !REAL(wp), DIMENSION(:,:,:  ), INTENT(in ) :: pdepw_b                         !! depth w before
      !!
      INTEGER :: ji, jj, jk, jd          !! loop index
      INTEGER :: jip1, jim1, jjp1, jjm1, jkp1, jkm1
      !!
      REAL(wp):: zsummsk
      REAL(wp):: zdz, zdzm1, zdzp1
      !!
      REAL(wp), DIMENSION(jpi,jpj)          :: zdmask 
      REAL(wp), DIMENSION(jpi,jpj,jpk)      :: ztmask0, zwmaskn
      REAL(wp), DIMENSION(jpi,jpj,jpk)      :: ztmask1, zwmaskb, ztmp3d
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts) :: zts0
      !!----------------------------------------------------------------------
      ! 
      CALL iom_get( numror, jpdom_autoglo, 'tmask'  , ztmask_b, ldxios = lrxios   ) ! need to extrapolate T/S
      !CALL iom_get( numror, jpdom_autoglo, 'wmask'  , zwmask_b, ldxios = lrxios   ) ! need to extrapolate T/S
      !CALL iom_get( numror, jpdom_autoglo, 'gdepw_n', zdepw_b(:,:,:), ldxios = lrxios ) ! need to interpol vertical profile (vvl)
      !
      ! 
      ! compute new T/S (interpolation) if vvl only for common wet cell in before and after wmask
      !PM: Is this IF needed since change to VVL by default
      !bugged : to be corrected (PM)
      ! back up original t/s/mask
      !tsb (:,:,:,:) = tsn(:,:,:,:)
      !
     ! compute new T/S (interpolation) if vvl only for common wet cell in before and after wmask

!      IF (.NOT.ln_linssh) THEN
!         DO jk = 2,jpk-1
!            DO jj = 1,jpj
!               DO ji = 1,jpi
!                  IF (wmask(ji,jj,jk) * zwmaskb(ji,jj,jk) == 1._wp .AND. (tmask(ji,jj,1)==0._wp .OR. ztmask_b(ji,jj,1)==0._wp) ) THEN
!
!                     !compute weight
!                     zdzp1 = MAX(0._wp,pdepw_b(ji,jj,jk+1) - gdepw_n(ji,jj,jk+1))
!                     zdzm1 = MAX(0._wp,gdepw_n(ji,jj,jk  ) - pdepw_b(ji,jj,jk  ))
!                     zdz   = e3t_n(ji,jj,jk) - zdzp1 - zdzm1 ! if isf : e3t = gdepw_n(ji,jj,jk+1)- gdepw_n(ji,jj,jk)
!
!                     IF (zdz .LT. 0._wp) THEN
!                        CALL ctl_stop( 'STOP', 'rst_iscpl : unable to compute the interpolation' )
!                     END IF
!
!                     tsn(ji,jj,jk,jp_tem) = ( zdzp1*tsb(ji,jj,jk+1,jp_tem) &
!                        &                   + zdz  *tsb(ji,jj,jk  ,jp_tem) &
!                        &                   + zdzm1*tsb(ji,jj,jk-1,jp_tem) )/e3t_n(ji,jj,jk)
!
!                     tsn(ji,jj,jk,jp_sal) = ( zdzp1*tsb(ji,jj,jk+1,jp_sal) &
!                        &                   + zdz  *tsb(ji,jj,jk  ,jp_sal) &
!                        &                   + zdzm1*tsb(ji,jj,jk-1,jp_sal) )/e3t_n(ji,jj,jk)
!
!                  END IF
!               END DO
!            END DO
!         END DO
!      END IF

      zts0(:,:,:,:)  = tsn(:,:,:,:)
      ztmask0(:,:,:) = ztmask_b(:,:,:)
      ztmask1(:,:,:) = ztmask_b(:,:,:)
      !
      ! iterate the extrapolation processes nn_drown times
      DO jd = 1,nn_drown ! resolution dependent (OK for ISOMIP+ case)
         DO jk = 1,jpk-1
            !
            ! define new wet cell
            zdmask(:,:) = tmask(:,:,jk) - ztmask0(:,:,jk);
            !
            DO jj = 2,jpj-1
               DO ji = 2,jpi-1
                  jip1=ji+1; jim1=ji-1;
                  jjp1=jj+1; jjm1=jj-1;
                  !
                  ! check if a wet neigbourg cell is present
                  zsummsk = ztmask0(jip1,jj  ,jk) + ztmask0(jim1,jj  ,jk) &
                          + ztmask0(ji  ,jjp1,jk) + ztmask0(ji  ,jjm1,jk)
                  !
                  ! if neigbourg wet cell available at the same level
                  IF ( zdmask(ji,jj) == 1._wp  .AND. zsummsk /= 0._wp ) THEN
                     !
                     ! horizontal basic extrapolation
                     tsn(ji,jj,jk,1)=( zts0(jip1,jj  ,jk,1) * ztmask0(jip1,jj  ,jk) &
                     &               + zts0(jim1,jj  ,jk,1) * ztmask0(jim1,jj  ,jk) &
                     &               + zts0(ji  ,jjp1,jk,1) * ztmask0(ji  ,jjp1,jk) &
                     &               + zts0(ji  ,jjm1,jk,1) * ztmask0(ji  ,jjm1,jk) ) / zsummsk
                     tsn(ji,jj,jk,2)=( zts0(jip1,jj  ,jk,2) * ztmask0(jip1,jj  ,jk) &
                     &               + zts0(jim1,jj  ,jk,2) * ztmask0(jim1,jj  ,jk) &
                     &               + zts0(ji  ,jjp1,jk,2) * ztmask0(ji  ,jjp1,jk) &
                     &               + zts0(ji  ,jjm1,jk,2) * ztmask0(ji  ,jjm1,jk) ) / zsummsk
                     !
                     ! update mask for next pass
                     ztmask1(ji,jj,jk)=1
                     !
                  ! in case no neigbourg wet cell available at the same level
                  ! check if a wet cell is available below
                  ELSEIF (zdmask(ji,jj) == 1._wp .AND. zsummsk == 0._wp) THEN
                     !
                     ! vertical extrapolation if horizontal extrapolation failed
                     jkm1=max(1,jk-1) ; jkp1=min(jpk,jk+1)
                     !
                     ! check if a wet neigbourg cell is present
                     zsummsk = ztmask0(ji,jj,jkm1) + ztmask0(ji,jj,jkp1)
                     IF (zdmask(ji,jj) == 1._wp .AND. zsummsk /= 0._wp ) THEN
                        tsn(ji,jj,jk,1)=( zts0(ji,jj,jkp1,1)*ztmask0(ji,jj,jkp1)     &
                        &               + zts0(ji,jj,jkm1,1)*ztmask0(ji,jj,jkm1)) / zsummsk
                        tsn(ji,jj,jk,2)=( zts0(ji,jj,jkp1,2)*ztmask0(ji,jj,jkp1)     &
                        &               + zts0(ji,jj,jkm1,2)*ztmask0(ji,jj,jkm1)) / zsummsk
                        !
                        ! update mask for next pass
                        ztmask1(ji,jj,jk)=1._wp
                     END IF
                  END IF
               END DO
            END DO
         END DO
         !
         ! update temperature and salinity and mask
         zts0(:,:,:,:)  = tsn(:,:,:,:)
         ztmask0(:,:,:) = ztmask1(:,:,:)
         !
         CALL lbc_lnk_multi( 'iscplrst', zts0(:,:,:,jp_tem), 'T', 1., zts0(:,:,:,jp_sal), 'T', 1., ztmask0, 'T', 1.)
         !
      END DO  ! nn_drown
      !
      ! mask new tsn field
      tsn(:,:,:,jp_tem) = zts0(:,:,:,jp_tem) * tmask(:,:,:)
      tsn(:,:,:,jp_sal) = zts0(:,:,:,jp_sal) * tmask(:,:,:)
      !
      ! sanity check
      ! -----------------------------------------------------------------------------------------
      ! case we open a cell but no neigbour cells available to get an estimate of T and S
      DO jk = 1,jpk-1
         DO jj = 1,jpj
            DO ji = 1,jpi
               IF (tmask(ji,jj,jk) == 1._wp .AND. tsn(ji,jj,jk,2) == 0._wp)              &
                  &   CALL ctl_stop('STOP', 'failing to fill all new weet cell,     &
                  &                          try increase nn_drown or activate XXXX &
                  &                         in your domain cfg computation'         )
            END DO
         END DO
      END DO
      ! 
   END SUBROUTINE isfcpl_tra

   SUBROUTINE isfcpl_vol()
      !!---------------------------------------------------------------------- 
      !!                   ***  ROUTINE iscpl_vol  ***
      !! 
      !! ** Purpose : compute the correction of the local divergence to apply  
      !!              during the first time step after the coupling.
      !!
      !! ** Method  : - compute horizontal vol div. before/after coupling
      !!              - compute vertical input
      !!              - compute correction
      !!                
      !!----------------------------------------------------------------------
      !!
      INTEGER :: ji, jj, jk 
      INTEGER :: ikb, ikt
      !!
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zqvolb, zqvoln  ! vol flux div.         before/after coupling
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ze3u_b, ze3v_b  ! vertical scale factor before/after coupling
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ztmask_b        ! mask                  before       coupling
      !!----------------------------------------------------------------------
      !
      CALL iom_get( numror, jpdom_autoglo, 'tmask'  , ztmask_b, ldxios = lrxios )
      CALL iom_get( numror, jpdom_autoglo, 'e3u_n'  , ze3u_b  , ldxios = lrxios )
      CALL iom_get( numror, jpdom_autoglo, 'e3v_n'  , ze3v_b  , ldxios = lrxios )
      !
      ! 1.0: compute horizontal volume flux divergence difference before-after coupling
      !
      DO jk = 1, jpk                                 ! Horizontal slab
         ! 1.1: get volume flux before coupling (>0 out)
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               zqvolb(ji,jj,jk) =  (   e2u(ji,jj) * ze3u_b(ji,jj,jk) * un(ji,jj,jk) - e2u(ji-1,jj  ) * ze3u_b(ji-1,jj  ,jk) * un(ji-1,jj  ,jk)    &
                  &                  + e1v(ji,jj) * ze3v_b(ji,jj,jk) * vn(ji,jj,jk) - e1v(ji  ,jj-1) * ze3v_b(ji  ,jj-1,jk) * vn(ji  ,jj-1,jk)  ) &
                  &                * ztmask_b(ji,jj,jk)
            END DO
         ENDDO
         !
         ! 1.2: get volume flux after coupling (>0 out)
         ! properly mask velocity 
         ! (velocity are still mask with old mask at this stage)
         un(:,:,jk) = un(:,:,jk) * umask(:,:,jk)
         vn(:,:,jk) = vn(:,:,jk) * vmask(:,:,jk)
         ! compute volume flux divergence after coupling
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               zqvoln(ji,jj,jk) = (   e2u(ji,jj) * e3u_n(ji,jj,jk) * un(ji,jj,jk) - e2u(ji-1,jj  ) * e3u_n(ji-1,jj  ,jk) * un(ji-1,jj  ,jk)    &
                  &                 + e1v(ji,jj) * e3v_n(ji,jj,jk) * vn(ji,jj,jk) - e1v(ji  ,jj-1) * e3v_n(ji  ,jj-1,jk) * vn(ji  ,jj-1,jk)  ) &
                  &               * tmask(ji,jj,jk)
            END DO
         ENDDO
         !
         ! 1.3: get 3d volume flux difference (before - after cpl) (>0 out)
         !      correction to add is _b - _n
         risfcpl_vol(:,:,jk) = zqvolb(:,:,jk) - zqvoln(:,:,jk)
      END DO
      !
      ! 2.0: include the contribution of the vertical velocity in the volume flux correction
      !
      DO jj = 2, jpjm1
         DO ji = 2, jpim1
            !
            ikt = mikt(ji,jj)
            IF ( ikt > 1 .AND. ssmask(ji,jj) == 1 ) THEN
               risfcpl_vol(ji,jj,ikt) = risfcpl_vol(ji,jj,ikt) + SUM(zqvolb(ji,jj,1:ikt-1))  ! test sign
            ENDIF
            !
         END DO
      ENDDO
      !
      CALL lbc_lnk( 'iscpl', risfcpl_vol, 'T', 1. )
      !
      ! 3.0: set total correction (div, tra, ssh)
      !
      ! 3.1: mask volume flux divergence correction
      risfcpl_vol(:,:,:) = risfcpl_vol(:,:,:) * tmask(:,:,:)
      !
      ! 3.2: get 3d tra increment to apply at the first time step
      ! temperature and salt content flux computed using local tsn 
      ! (very simple advection scheme)
      ! (>0 out)
      risfcpl_tsc(:,:,:,jp_tem) = -risfcpl_vol(:,:,:) * tsn(:,:,:,jp_tem)
      risfcpl_tsc(:,:,:,jp_sal) = -risfcpl_vol(:,:,:) * tsn(:,:,:,jp_sal)
      !
      ! 3.3: ssh correction (for dynspg_ts)
      risfcpl_ssh(:,:) = 0.0
      DO jk = 1,jpk
         risfcpl_ssh(:,:) = risfcpl_ssh(:,:) + risfcpl_vol(:,:,jk) * r1_e1e2t(:,:)
      END DO

   END SUBROUTINE isfcpl_vol

   SUBROUTINE isfcpl_cons()
      !!---------------------------------------------------------------------- 
      !!                   ***  ROUTINE iscpl_cons  ***
      !! 
      !! ** Purpose :   compute the corrective increment in volume/salt/heat to put back the vol/heat/salt
      !!                removed or added during the coupling processes (wet or dry new cell)
      !! 
      !! ** Method  :   - compare volume/heat/salt before and after
      !!                - look for the closest wet cells (share amoung neigbourgs if there are)
      !!                - build the correction increment to applied at each time step
      !!                
      !!----------------------------------------------------------------------
      !
      TYPE(isfcons), DIMENSION(:),ALLOCATABLE :: zisfpts ! list of point receiving a correction
      !
      INTEGER  ::   ji   , jj  , jk  , jproc          ! loop index
      INTEGER  ::   jip1 , jim1, jjp1, jjm1           ! dummy indices
      INTEGER  ::   iig  , ijg, ik                    ! dummy indices
      INTEGER  ::   jisf                              ! start, end and current position in the increment array
      INTEGER  ::   ingb, ifind                       ! 0/1 target found or need to be found 
      INTEGER  ::   nisfl_area                        ! global number of cell concerned by the wet->dry case 
      INTEGER, DIMENSION(jpnij) :: nisfl              ! local  number of cell concerned by the wet->dry case
      !
      REAL(wp) ::   z1_sum, z1_rdtiscpl
      REAL(wp) ::   zdtem, zdsal, zdvol, zratio       ! tem, sal, vol increment
      REAL(wp) ::   zlon , zlat                       ! target location  
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ztmask_b    ! mask before
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ze3t_b      ! scale factor before
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zt_b      ! scale factor before
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zs_b      ! scale factor before
      !!----------------------------------------------------------------------

      !==============================================================================
      ! 1.0: initialisation
      !==============================================================================

      ! get restart variable
      CALL iom_get( numror, jpdom_autoglo, 'tmask'  , ztmask_b(:,:,:), ldxios = lrxios   ) ! need to extrapolate T/S
      CALL iom_get( numror, jpdom_autoglo, 'e3t_n'  , ze3t_b(:,:,:)  , ldxios = lrxios )
      CALL iom_get( numror, jpdom_autoglo, 'tn'     , zt_b(:,:,:)    , ldxios = lrxios )
      CALL iom_get( numror, jpdom_autoglo, 'sn'     , zs_b(:,:,:)    , ldxios = lrxios )

      ! compute run length
      nstp_iscpl  = nitend - nit000 + 1
      rdt_iscpl   = nstp_iscpl * rn_rdt
      z1_rdtiscpl = 1._wp / rdt_iscpl 

      IF (lwp) WRITE(numout,*) '            nb of stp for cons  = ', nstp_iscpl
      IF (lwp) WRITE(numout,*) '            coupling time step  = ', rdt_iscpl

      ! initialisation correction
      risfcpl_cons_vol = 0.0
      risfcpl_cons_ssh = 0.0
      risfcpl_cons_tsc = 0.0

      !==============================================================================
      ! 2.0: diagnose the heat, salt and volume input and compute the correction variable
      !      for case where we wet a cell or cell still wet (no change in cell status)
      !==============================================================================

      DO jk = 1,jpk-1
         DO jj = nldj,nlej
            DO ji = nldi,nlei

               ! volume diff
               zdvol = e3t_n(ji,jj,jk) * tmask(ji,jj,jk) - ze3t_b(ji,jj,jk) * ztmask_b(ji,jj,jk)

               ! heat diff
               zdtem = tsn (ji,jj,jk,jp_tem) *  e3t_n(ji,jj,jk) *  tmask  (ji,jj,jk)   &
                     - zt_b(ji,jj,jk)        * ze3t_b(ji,jj,jk) * ztmask_b(ji,jj,jk)

               ! salt diff
               zdsal = tsn(ji,jj,jk,jp_sal) *  e3t_n(ji,jj,jk) *  tmask  (ji,jj,jk)   &
                     - zs_b(ji,jj,jk)       * ze3t_b(ji,jj,jk) * ztmask_b(ji,jj,jk)
            
               ! volume, heat and salt differences in each cell (>0 means correction is an outward flux)
               ! in addition to the geometry change unconservation, need to add the divergence correction as it is flux across the boundary
               risfcpl_cons_vol(ji,jj,jk)        = (   zdvol * e1e2t(ji,jj) + risfcpl_vol(ji,jj,jk)        ) * z1_rdtiscpl
               risfcpl_cons_tsc(ji,jj,jk,jp_sal) = ( - zdsal * e1e2t(ji,jj) + risfcpl_tsc(ji,jj,jk,jp_sal) ) * z1_rdtiscpl
               risfcpl_cons_tsc(ji,jj,jk,jp_tem) = ( - zdtem * e1e2t(ji,jj) + risfcpl_tsc(ji,jj,jk,jp_tem) ) * z1_rdtiscpl

            END DO
         END DO
      END DO
      !
      !==============================================================================
      ! 3.0: diagnose the heat, salt and volume input and compute the correction variable
      !      for case where we close a cell
      !==============================================================================
      !
      ! compute the total number of point receiving a correction increment for each processor
      ! local
      nisfl(:)=0
      DO jk = 1,jpk-1
         DO jj = nldj,nlej
            DO ji = nldi,nlei
               jip1=MIN(ji+1,jpi) ; jim1=MAX(ji-1,1) ; jjp1=MIN(jj+1,jpj) ; jjm1=MAX(jj-1,1) ;
               IF ( tmask(ji,jj,jk) == 0._wp .AND. ztmask_b(ji,jj,jk) == 1._wp ) nisfl(narea) = nisfl(narea) + MAX(SUM(tmask(jim1:jip1,jjm1:jjp1,jk)),1._wp)
            ENDDO
         ENDDO
      ENDDO
      !
      ! global 
      CALL mpp_sum('isfcpl',nisfl  )
      !
      ! allocate list of point receiving correction
      ALLOCATE(zisfpts(nisfl(narea)))
      !
      zisfpts(:) = isfcons(0,0,0,-HUGE(1.0), -HUGE(1.0), -HUGE(1.0), -HUGE(1.0), -HUGE(1.0), 0)
      !
      ! start computing the correction and fill zisfpts
      ! local
      jisf = 0
      DO jk = 1,jpk-1
         DO jj = nldj,nlej
            DO ji = nldi,nlei
               IF ( tmask(ji,jj,jk) == 0._wp .AND. ztmask_b(ji,jj,jk) == 1._wp ) THEN

                  jip1=MIN(ji+1,jpi) ; jim1=MAX(ji-1,1) ; jjp1=MIN(jj+1,jpj) ; jjm1=MAX(jj-1,1) ;

                  zdvol = risfcpl_cons_vol(ji,jj,jk       )
                  zdsal = risfcpl_cons_tsc(ji,jj,jk,jp_sal)
                  zdtem = risfcpl_cons_tsc(ji,jj,jk,jp_tem)

                  IF ( SUM( tmask(jim1:jip1,jjm1:jjp1,jk) ) > 0._wp ) THEN
                     ! spread correction amoung neigbourg wet cells (horizontal direction first)
                     ! as it is a rude correction corner and lateral cell have the same weight
                     !
                     z1_sum =  1._wp / SUM( tmask(jim1:jip1,jjm1:jjp1,jk) )
                     !
                     ! lateral cells
                     IF (tmask(jip1,jj  ,jk) == 1) CALL update_isfpts(zisfpts, jisf, jip1, jj  , jk, zdvol, zdsal, zdtem, z1_sum)
                     IF (tmask(jim1,jj  ,jk) == 1) CALL update_isfpts(zisfpts, jisf, jim1, jj  , jk, zdvol, zdsal, zdtem, z1_sum)
                     IF (tmask(ji  ,jjp1,jk) == 1) CALL update_isfpts(zisfpts, jisf, ji  , jjp1, jk, zdvol, zdsal, zdtem, z1_sum)
                     IF (tmask(ji  ,jjm1,jk) == 1) CALL update_isfpts(zisfpts, jisf, ji  , jjm1, jk, zdvol, zdsal, zdtem, z1_sum)
                     !
                     ! corner  cells
                     IF (tmask(jip1,jjm1,jk) == 1) CALL update_isfpts(zisfpts, jisf, jip1, jjm1, jk, zdvol, zdsal, zdtem, z1_sum)
                     IF (tmask(jim1,jjm1,jk) == 1) CALL update_isfpts(zisfpts, jisf, jim1, jjm1, jk, zdvol, zdsal, zdtem, z1_sum)
                     IF (tmask(jim1,jjp1,jk) == 1) CALL update_isfpts(zisfpts, jisf, jim1, jjp1, jk, zdvol, zdsal, zdtem, z1_sum)
                     IF (tmask(jip1,jjp1,jk) == 1) CALL update_isfpts(zisfpts, jisf, jip1, jjp1, jk, zdvol, zdsal, zdtem, z1_sum)
                     !
                  ELSE IF ( tmask(ji,jj,jk+1) == 1._wp ) THEN
                     ! spread correction amoung neigbourg wet cells (vertical direction)
                     CALL update_isfpts(zisfpts, jisf, ji  , jj  , jk+1, zdvol, zdsal, zdtem, 1., 0)
                  ELSE
                     ! need to find where to put correction in later on
                     CALL update_isfpts(zisfpts, jisf, ji  , jj  , jk  , zdvol, zdsal, zdtem, 1., 1)
                  END IF
               END IF
            END DO
         END DO
      END DO
      !
      ! share data among all processes because for some point we need to find the closest wet point (could be on other process)
      DO jproc=1,jpnij
         ! 
         ! share total number of isf point treated for proc jproc
         IF (jproc==narea) THEN
            nisfl_area=nisfl(jproc)
         ELSE
            nisfl_area=0
         END IF
         CALL mpp_max('isfcpl',nisfl_area)
         !
         DO jisf = 1,nisfl_area
            !
            IF (jproc==narea) THEN
               ! indices (conversion to global indices and sharing)
               iig = zisfpts(jisf)%ii       ; ijg = zisfpts(jisf)%jj       ; ik = zisfpts(jisf)%kk
               !
               ! data
               zdvol = zisfpts(jisf)%dvol   ; zdsal = zisfpts(jisf)%dsal   ; zdtem = zisfpts(jisf)%dtem
               !
               ! location
               zlat = zisfpts(jisf)%lat     ; zlon = zisfpts(jisf)%lon
               !
               ! find flag
               ingb = zisfpts(jisf)%ngb
            ELSE
               iig  =0   ; ijg  =0   ; ik   =0  
               zdvol=-HUGE(1.0) ; zdsal=-HUGE(1.0) ; zdtem=-HUGE(1.0)
               zlat =-HUGE(1.0) ; zlon =-HUGE(1.0)   
               ingb = 0
            END IF
            !
            ! share data (need synchronisation of data as get_correction call a global com)
            CALL mpp_max('isfcpl',iig)   ; CALL mpp_max('isfcpl',ijg)   ; CALL mpp_max('isfcpl',ik)
            CALL mpp_max('isfcpl',zdvol) ; CALL mpp_max('isfcpl',zdsal) ; CALL mpp_max('isfcpl',zdtem)
            CALL mpp_max('isfcpl',zlat)  ; CALL mpp_max('isfcpl',zlon)
            CALL mpp_max('isfcpl',ingb)
            !
            ! fill the 3d correction array
            CALL get_correction(iig, ijg, ik, zlon, zlat, zdvol, zdsal, zdtem, ingb)
         END DO
      END DO
      !
      !==============================================================================
      ! 4.0: finalisation and compute ssh equivalent of the volume correction
      !==============================================================================
      !
      ! mask (>0 out)
      risfcpl_cons_vol(:,:,:       ) = risfcpl_cons_vol(:,:,:       ) * tmask(:,:,:)
      risfcpl_cons_tsc(:,:,:,jp_sal) = risfcpl_cons_tsc(:,:,:,jp_sal) * tmask(:,:,:)
      risfcpl_cons_tsc(:,:,:,jp_tem) = risfcpl_cons_tsc(:,:,:,jp_tem) * tmask(:,:,:)
      !
      ! add lbclnk
      CALL lbc_lnk_multi( 'iscplrst', risfcpl_cons_tsc(:,:,:,jp_tem), 'T', 1., risfcpl_cons_tsc(:,:,:,jp_sal), 'T', 1., &
         &                            risfcpl_cons_vol(:,:,:)       , 'T', 1.)
      !
      ! ssh correction (for dynspg_ts)
      DO jk = 1,jpk
         risfcpl_cons_ssh(:,:) = risfcpl_cons_ssh(:,:) + risfcpl_cons_vol(:,:,jk)
      END DO
      risfcpl_cons_ssh(:,:) = risfcpl_cons_ssh(:,:) * r1_e1e2t(:,:)
      !
   END SUBROUTINE isfcpl_cons
   !
   SUBROUTINE update_isfpts(sisfpts, kpts, ki, kj, kk, pdvol, pdsal, pdtem, pratio, kfind)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE update_isfpts  ***
      !!
      !! ** Purpose : if a cell become dry, we need to put the corrective increment elsewhere
      !!
      !! ** Action  : update the list of point
      !!
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      TYPE(isfcons), DIMENSION(:), INTENT(inout) :: sisfpts
      INTEGER,                     INTENT(inout) :: kpts
      !!----------------------------------------------------------------------
      INTEGER,      INTENT(in   )           :: ki, kj, kk                  !    target location (kfind=0) 
      !                                                                    ! or source location (kfind=1)
      INTEGER,      INTENT(in   ), OPTIONAL :: kfind                       ! 0  target cell already found
      !                                                                    ! 1  target to be determined
      REAL(wp),     INTENT(in   )           :: pdvol, pdsal, pdtem, pratio ! vol/sal/tem increment 
      !                                                                    ! and ratio in case increment span over multiple cells.
      !!----------------------------------------------------------------------
      INTEGER :: ifind
      !!----------------------------------------------------------------------
      ! 
      ! increment position
      kpts = kpts + 1
      !
      ! define if we need to look for closest valid wet cell (no neighbours or neigbourg on halo)
      IF ( PRESENT(kfind) ) THEN
         ifind = kfind
      ELSE
         ifind = ( 1 - tmask_h(ki,kj) ) * tmask(ki,kj,kk)
      END IF
      !
      ! update isfpts structure
      sisfpts(kpts) = isfcons(mig(ki), mjg(kj), kk, pratio * pdvol, pratio * pdsal, pratio * pdtem, glamt(ki,kj), gphit(ki,kj), ifind )
      !
   END SUBROUTINE update_isfpts
   !
   SUBROUTINE get_correction( ki, kj, kk, plon, plat, pvolinc, psalinc, pteminc, kfind)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE get_correction  ***
      !!
      !! ** Action : - Find the closest valid cell if needed (wet and not on the halo)
      !!             - Scale the correction depending of pratio (case where multiple wet neigbourgs)
      !!             - Fill the correction array
      !!
      !!----------------------------------------------------------------------
      INTEGER , INTENT(in) :: ki, kj, kk, kfind        ! target point indices
      REAL(wp), INTENT(in) :: plon, plat               ! target point lon/lat
      REAL(wp), INTENT(in) :: pvolinc, pteminc,psalinc ! correction increment for vol/temp/salt
      !!----------------------------------------------------------------------
      INTEGER :: jj, ji, iig, ijg
      !!----------------------------------------------------------------------
      !
      ! define global indice of correction location
      iig = ki ; ijg = kj
      IF ( kfind == 1 ) CALL dom_ngb( plon, plat, iig, ijg,'T', kk)
      !
      ! fill the correction array
      DO jj = mj0(ijg),mj1(ijg)
         DO ji = mi0(iig),mi1(iig)
            ! correct the vol_flx and corresponding heat/salt flx in the closest cell
            risfcpl_cons_vol(ji,jj,kk)        =  risfcpl_cons_vol(ji,jj,kk       ) + pvolinc
            risfcpl_cons_tsc(ji,jj,kk,jp_sal) =  risfcpl_cons_tsc(ji,jj,kk,jp_sal) + psalinc
            risfcpl_cons_tsc(ji,jj,kk,jp_tem) =  risfcpl_cons_tsc(ji,jj,kk,jp_tem) + pteminc
         END DO
      END DO

   END SUBROUTINE get_correction

END MODULE isfcpl
