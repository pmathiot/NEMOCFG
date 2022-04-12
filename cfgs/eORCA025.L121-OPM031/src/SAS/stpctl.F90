MODULE stpctl
   !!======================================================================
   !!                       ***  MODULE  stpctl  ***
   !! Ocean run control :  gross check of the ocean time stepping
   !!                      version for standalone surface scheme
   !!======================================================================
   !! History :  OPA  ! 1991-03  (G. Madec) Original code
   !!            6.0  ! 1992-06  (M. Imbard)
   !!            8.0  ! 1997-06  (A.M. Treguier)
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            2.0  ! 2009-07  (G. Madec)  Add statistic for time-spliting
   !!            3.5  ! 2012-03  (S. Alderson)
   !!            4.0  ! 2017-04  (G. Madec)  regroup global communications
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp_ctl      : Control the run
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE ice      , ONLY : vt_i, u_ice, tm_i
   !
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! distributed memory computing

   USE netcdf          ! NetCDF library
   IMPLICIT NONE
   PRIVATE

   PUBLIC stp_ctl           ! routine called by step.F90

   INTEGER  ::   idrun, idtime, idssh, idu, ids, istatus
   !!----------------------------------------------------------------------
   !! NEMO/SAS 4.0 , NEMO Consortium (2018)
   !! $Id: stpctl.F90 12859 2020-05-03 09:33:32Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE stp_ctl( kt, kindic )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE stp_ctl  ***
      !!                     
      !! ** Purpose :   Control the run
      !!
      !! ** Method  : - Save the time step in numstp
      !!              - Print it each 50 time steps
      !!
      !! ** Actions :   "time.step" file = last ocean time-step
      !!                "run.stat"  file = run statistics
      !!                
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in    ) ::   kt       ! ocean time-step index
      INTEGER, INTENT( inout ) ::   kindic   ! indicator of solver convergence
      !!
      REAL(wp), DIMENSION(3) ::   zmax
      LOGICAL                ::   ll_wrtstp, ll_colruns, ll_wrtruns
      CHARACTER(len=20) :: clname
      !!----------------------------------------------------------------------
      !
      ll_wrtstp  = ( MOD( kt-nit000, sn_cfctl%ptimincr ) == 0 ) .OR. ( kt == nitend )
      ll_colruns = ll_wrtstp .AND. sn_cfctl%l_runstat .AND. jpnij > 1 
      ll_wrtruns = ( ll_colruns .OR. jpnij == 1 ) .AND. lwm
      !
      IF( kt == nit000 ) THEN
         !
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'stp_ctl : time-stepping control'
            WRITE(numout,*) '~~~~~~~'
         ENDIF
         !                                ! open time.step file
         IF( lwm ) CALL ctl_opn( numstp, 'time.step', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
         !                                ! open run.stat file(s) at start whatever
         !                                ! the value of sn_cfctl%ptimincr
         IF( ll_wrtruns ) THEN
            CALL ctl_opn( numrun, 'run.stat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
            clname = 'run.stat.nc'
            IF( .NOT. Agrif_Root() )   clname = TRIM(Agrif_CFixed())//"_"//TRIM(clname)
            istatus = NF90_CREATE( 'run.stat.nc', NF90_CLOBBER, idrun )
            istatus = NF90_DEF_DIM( idrun, 'time'     , NF90_UNLIMITED, idtime )
            istatus = NF90_DEF_VAR( idrun, 'vt_i_max' , NF90_DOUBLE, (/ idtime /), idssh )
            istatus = NF90_DEF_VAR( idrun, 'abs_u_max', NF90_DOUBLE, (/ idtime /), idu )
            istatus = NF90_DEF_VAR( idrun, 'tm_i_min' , NF90_DOUBLE, (/ idtime /), ids )
            istatus = NF90_ENDDEF(idrun)
         ENDIF
      ENDIF
      !
      IF(lwm .AND. ll_wrtstp) THEN        !==  current time step  ==!   ("time.step" file)
         WRITE ( numstp, '(1x, i8)' )   kt
         REWIND( numstp )
      ENDIF
      !                                   !==  test of extrema  ==!
      IF( ll_colruns .OR. jpnij == 1 ) THEN
         zmax(1) = MAXVAL(      vt_i (:,:) )                                           ! max ice thickness
         zmax(2) = MAXVAL( ABS( u_ice(:,:) ) )                                         ! max ice velocity (zonal only)
         zmax(3) = MAXVAL(     -tm_i (:,:)+273.15_wp , mask = ssmask(:,:) == 1._wp )   ! min ice temperature
         IF( ll_colruns )   CALL mpp_max( "stpctl", zmax )                             ! max over the global domain
      END IF
      !                                            !==  run statistics  ==!   ("run.stat" file)
      IF( ll_wrtruns ) THEN
         WRITE(numrun,9500) kt, zmax(1), zmax(2), - zmax(3)
         istatus = NF90_PUT_VAR( idrun, idssh, (/ zmax(1)/), (/kt/), (/1/) )
         istatus = NF90_PUT_VAR( idrun,   idu, (/ zmax(2)/), (/kt/), (/1/) )
         istatus = NF90_PUT_VAR( idrun,   ids, (/-zmax(3)/), (/kt/), (/1/) )
         IF( MOD( kt , 100 ) == 0 ) istatus = NF90_SYNC(idrun)
         IF( kt == nitend         ) istatus = NF90_CLOSE(idrun)
      END IF
      !
9500  FORMAT(' it :', i8, '    vt_i_max: ', D23.16, ' |u|_max: ', D23.16,' tm_i_min: ', D23.16)
      !
   END SUBROUTINE stp_ctl

   !!======================================================================
END MODULE stpctl
