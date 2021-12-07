MODULE step
   !!======================================================================
   !!                       ***  MODULE step  ***
   !! Time-stepping    : manager of the ocean, tracer and ice time stepping
   !!                    version for standalone surface scheme
   !!======================================================================
   !! History :  OPA  !  1991-03  (G. Madec)  Original code
   !!             .   !    .                                                     
   !!             .   !    .                                                     
   !!   NEMO     3.5  !  2012-03  (S. Alderson)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp             : OPA system time-stepping
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and tracers variables
   USE dom_oce          ! ocean space and time domain variables 
   USE daymod           ! calendar                         (day     routine)
   USE sbc_oce          ! surface boundary condition: fields
   USE sbcmod           ! surface boundary condition       (sbc     routine)
   USE sbcrnf           ! surface boundary condition: runoff variables
   USE sbccpl           ! surface boundary condition: coupled interface
   USE eosbn2           ! equation of state                (eos_bn2 routine)
   USE diawri           ! Standard run outputs             (dia_wri routine)
   USE bdy_oce   , ONLY: ln_bdy
   USE bdydta           ! mandatory for sea-ice
   USE stpctl           ! time stepping control            (stp_ctl routine)
   !
   USE in_out_manager   ! I/O manager
   USE prtctl           ! Print control                    (prt_ctl routine)
   USE iom              !
   USE lbclnk           !
   USE timing           ! Timing            
#if defined key_iomput
   USE xios
#endif

#if defined key_agrif
   USE agrif_oce, ONLY: lk_agrif_debug
#if defined key_si3
   USE agrif_ice_update
#endif
#endif
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp   ! called by nemogcm.F90

   !!----------------------------------------------------------------------
   !! NEMO/SAS 4.0 , NEMO Consortium (2018)
   !! $Id: step.F90 12651 2020-04-03 07:51:14Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_agrif
   RECURSIVE SUBROUTINE stp( )
      INTEGER             ::   kstp   ! ocean time-step index
#else
   SUBROUTINE stp( kstp )
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
#endif
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp  ***
      !!                      
      !! ** Purpose : - Time stepping of SBC (surface boundary)
      !! 
      !! ** Method  : -1- Update forcings and data  
      !!              -2- Outputs and diagnostics
      !!----------------------------------------------------------------------
      INTEGER ::   indic    ! error indicator if < 0
      !! ---------------------------------------------------------------------

#if defined key_agrif
      IF( nstop > 0 ) return   ! avoid to go further if an error was detected during previous time step 
      kstp = nit000 + Agrif_Nb_Step()
      IF ( lk_agrif_debug ) THEN
         IF ( Agrif_Root() .and. lwp) Write(*,*) '---'
         IF (lwp) Write(*,*) 'Grid Number',Agrif_Fixed(),' time step ',kstp, 'int tstep',Agrif_NbStepint()
      ENDIF

      IF ( kstp == (nit000 + 1) ) lk_agrif_fstep = .FALSE.

# if defined key_iomput
      IF( Agrif_Nbstepint() == 0 )   CALL iom_swap( cxios_context )
# endif   
#endif   
                             indic = 0                    ! although indic is not changed in stp_ctl
                                                          ! need to keep the same interface 
      IF( kstp == nit000 )   CALL iom_init( cxios_context ) ! iom_put initialization (must be done after nemo_init for AGRIF+XIOS+OASIS)
      IF( kstp /= nit000 )   CALL day( kstp )             ! Calendar (day was already called at nit000 in day_init)
                             CALL iom_setkt( kstp - nit000 + 1, cxios_context )   ! tell iom we are at time step kstp

      ! ==> clem: open boundaries is mandatory for sea-ice because ice BDY is not decoupled from  
      !           the environment of ocean BDY. Therefore bdy is called in both OPA and SAS modules.
      !           From SAS: ocean bdy data are wrong  (but we do not care) and ice bdy data are OK.  
      !           This is not clean and should be changed in the future. 
      IF( ln_bdy     )       CALL bdy_dta ( kstp, kt_offset=+1 )   ! update dynamic & tracer data at open boundaries
      ! ==>
                             CALL sbc    ( kstp )         ! Sea Boundary Condition (including sea-ice)

                             CALL dia_wri( kstp )         ! ocean model: outputs

#if defined key_agrif
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! AGRIF recursive integration
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<      
                             CALL Agrif_Integrate_ChildGrids( stp )  
#endif                 
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Control
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                             CALL stp_ctl( kstp, indic )
      IF( indic < 0  )  THEN
                             CALL ctl_stop( 'step: indic < 0' )
                             CALL dia_wri_state( 'output.abort' )
      ENDIF
#if defined key_agrif
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! AGRIF update
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<      
      IF( Agrif_NbStepint() == 0 .AND. nstop == 0 ) THEN                       ! AGRIF Update from zoom N to zoom 1 then to Parent 
#if defined key_si3
                             CALL Agrif_Update_ice( )   ! update sea-ice
#endif
      ENDIF
#endif
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! File manipulation at the end of the first time step
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<                         
      IF( kstp == nit000   ) CALL iom_close( numror )                          ! close input  ocean restart file
      
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Coupled mode
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( lk_oasis    )  CALL sbc_cpl_snd( kstp )     ! coupled mode : field exchanges if OASIS-coupled ice

#if defined key_iomput
      IF( kstp == nitrst ) THEN
         IF(.NOT.lwxios) THEN
            CALL iom_close( numrow )     
         ELSE
            CALL iom_context_finalize( cwxios_context )
         ENDIF
         lrst_oce = .FALSE.
      ENDIF
      IF( kstp == nitend .OR. indic < 0 ) THEN
                             CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
      ENDIF
#endif
      !
      IF( ln_timing .AND.  kstp == nit000  )   CALL timing_reset
      !
   END SUBROUTINE stp

   !!======================================================================
END MODULE step
