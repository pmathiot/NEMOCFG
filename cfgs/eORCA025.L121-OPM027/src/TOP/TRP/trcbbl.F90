MODULE trcbbl
  !!======================================================================
   !!                       ***  MODULE  trcbbl  ***
   !! Ocean passive tracers physics :  advective and/or diffusive bottom boundary 
   !!                                  layer scheme
   !!======================================================================
   !! History :  OPA  !  1996-06  (L. Mortier)  Original code
   !!            8.0  !  1997-11  (G. Madec)    Optimization
   !!   NEMO     1.0  !  2002-08  (G. Madec)  free form + modules
   !!             -   !  2004-01  (A. de Miranda, G. Madec, J.M. Molines ) add advective bbl
   !!            3.3  !  2009-11  (G. Madec)  merge trabbl and trabbl_adv + style + optimization 
   !!             -   !  2010-04  (G. Madec)  Campin & Goosse advective bbl 
   !!             -   !  2010-06  (C. Ethe, G. Madec)  merge TRA-TRC
   !!            4.0  !  2017-04  (G. Madec)  ln_trabbl namelist variable instead of a CPP key
   !!----------------------------------------------------------------------
#if  defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!    trc_bbl      : update the tracer trends due to the bottom boundary layer (advective and/or diffusive)
   !!----------------------------------------------------------------------
   USE oce_trc        ! ocean dynamics and active tracers variables
   USE trc            ! ocean passive tracers variables
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! tracer trends
   USE trabbl         ! bottom boundary layer 
   USE prtctl_trc     ! Print control for debbuging

   PUBLIC   trc_bbl   !  routine called by trctrp.F90

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcbbl.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_bbl( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE bbl  ***
      !!                   
      !! ** Purpose :   Compute the before tracer (t & s) trend associated 
      !!     with the bottom boundary layer and add it to the general trend
      !!     of tracer equations.
      !!
      !!----------------------------------------------------------------------  
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step 
      INTEGER :: jn                   ! loop index
      CHARACTER (len=22) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztrtrd
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_bbl')
      !
      IF( .NOT. l_offline .AND. nn_dttrc == 1 ) THEN
         CALL bbl( kt, nittrc000, 'TRC' )      ! Online coupling with dynamics  : Computation of bbl coef and bbl transport
         l_bbl = .FALSE.                       ! Offline coupling with dynamics : Read bbl coef and bbl transport from input files
      ENDIF

      IF( l_trdtrc )  THEN
         ALLOCATE( ztrtrd(jpi,jpj,jpk,jptra) ) ! temporary save of trends
         ztrtrd(:,:,:,:)  = tra(:,:,:,:)
      ENDIF

      !* Diffusive bbl :
      IF( nn_bbl_ldf == 1 ) THEN
         !
         CALL tra_bbl_dif( trb, tra, jptra )  
         IF( ln_ctl )   THEN
            WRITE(charout, FMT="(' bbl_dif')")  ;  CALL prt_ctl_trc_info(charout)
            CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
         ENDIF
         !
      ENDIF

      !* Advective bbl : bbl upstream advective trends added to the tracer trends
      IF( nn_bbl_adv /= 0 ) THEN
         !
         CALL tra_bbl_adv( trb, tra, jptra )  
         IF( ln_ctl )   THEN
            WRITE(charout, FMT="(' bbl_adv')")  ;  CALL prt_ctl_trc_info(charout)
            CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
         ENDIF
         !
      ENDIF

      IF( l_trdtrc )   THEN                      ! save the horizontal diffusive trends for further diagnostics
        DO jn = 1, jptra
           ztrtrd(:,:,:,jn) = tra(:,:,:,jn) - ztrtrd(:,:,:,jn)
           CALL trd_tra( kt, 'TRC', jn, jptra_bbl, ztrtrd(:,:,:,jn) )
        END DO
        DEALLOCATE( ztrtrd ) ! temporary save of trends
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_bbl')
      !
   END SUBROUTINE trc_bbl

#endif

   !!======================================================================
END MODULE trcbbl
