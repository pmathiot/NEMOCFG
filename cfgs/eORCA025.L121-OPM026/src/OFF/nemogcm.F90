MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! Off-line Ocean   : passive tracer evolution, dynamics read in files
   !!======================================================================
   !! History :  3.3  ! 2010-05  (C. Ethe)  Full reorganization of the off-line: phasing with the on-line
   !!            3.4  ! 2011-01  (C. Ethe, A. R. Porter, STFC Daresbury) dynamical allocation
   !!            4.0  ! 2016-10  (C. Ethe, G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm      : off-line: solve ocean tracer only
   !!   nemo_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init     : initialization of the NEMO system
   !!   nemo_ctl      : initialisation of the contol print
   !!   nemo_closefile: close remaining open files
   !!   nemo_alloc    : dynamical allocation
   !!   istate_init   : simple initialization to zero of ocean fields
   !!   stp_ctl       : reduced step control (no dynamics in off-line)
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space domain variables
   USE oce            ! dynamics and tracers variables
   USE trc_oce        ! Shared ocean/passive tracers variables
   USE c1d            ! 1D configuration
   USE domain         ! domain initialization from coordinate & bathymetry (dom_init routine)
   USE closea         ! treatment of closed seas (for ln_closea)
   USE usrdef_nam     ! user defined configuration
   USE eosbn2         ! equation of state            (eos bn2 routine)
   !                  ! ocean physics
   USE bdy_oce,  ONLY : ln_bdy
   USE bdyini         ! open boundary cond. setting       (bdy_init routine)
   USE ldftra         ! lateral diffusivity setting    (ldf_tra_init routine)
   USE ldfslp         ! slopes of neutral surfaces     (ldf_slp_init routine)
   USE traqsr         ! solar radiation penetration    (tra_qsr_init routine)
   USE trabbl         ! bottom boundary layer          (tra_bbl_init routine)
   USE traldf         ! lateral physics                (tra_ldf_init routine)
   USE sbcmod         ! surface boundary condition     (sbc_init     routine)
   USE phycst         ! physical constant                   (par_cst routine)
   USE dtadyn         ! Lecture and Interpolation of the dynamical fields
   USE trcini         ! Initilization of the passive tracers
   USE daymod         ! calendar                            (day     routine)
   USE trcstp         ! passive tracer time-stepping        (trc_stp routine)
   USE dtadyn         ! Lecture and interpolation of the dynamical fields
   !              ! Passive tracers needs
   USE trc            ! passive tracer : variables
   USE trcnam         ! passive tracer : namelist
   USE trcrst         ! passive tracer restart
   USE diaptr         ! Need to initialise this as some variables are used in if statements later
   USE sbc_oce , ONLY : ln_rnf
   USE sbcrnf         ! surface boundary condition : runoffs
   !              ! I/O & MPP
   USE iom            ! I/O library
   USE in_out_manager ! I/O manager
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE lib_mpp        ! distributed memory computing
#if defined key_iomput
   USE xios           ! xIOserver
#endif 
   USE prtctl         ! Print control                    (prt_ctl_init routine)
   USE timing         ! Timing
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
   USE lbcnfd  , ONLY : isendto, nsndto, nfsloop, nfeloop   ! Setup of north fold exchanges

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   nemo_gcm   ! called by nemo.F90

   CHARACTER (len=64) ::   cform_aaa="( /, 'AAAAAAAA', / ) "   ! flag for output listing

   !!----------------------------------------------------------------------
   !! NEMO/OFF 4.0 , NEMO Consortium (2018)
   !! $Id: nemogcm.F90 13013 2020-06-03 08:33:06Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE nemo_gcm
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_gcm  ***
      !!
      !! ** Purpose :   NEMO solves the primitive equations on an orthogonal
      !!              curvilinear mesh on the sphere.
      !!
      !! ** Method  : - model general initialization
      !!              - launch the time-stepping (dta_dyn and trc_stp)
      !!              - finalize the run by closing files and communications
      !!
      !! References : Madec, Delecluse,Imbard, and Levy, 1997:  internal report, IPSL.
      !!              Madec, 2008, internal report, IPSL.
      !!----------------------------------------------------------------------
      INTEGER :: istp, indic       ! time step index
      !!----------------------------------------------------------------------

      CALL nemo_init  ! Initializations

      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
      CALL mpp_max( 'nemogcm', nstop )

      !                            !-----------------------!
      !                            !==   time stepping   ==!
      !                            !-----------------------!
      istp = nit000
      !
      IF( ln_rnf )   CALL sbc_rnf(istp)   ! runoffs initialization 
      ! 
      CALL iom_init( cxios_context )      ! iom_put initialization (must be done after nemo_init for AGRIF+XIOS+OASIS)
      ! 
      DO WHILE ( istp <= nitend .AND. nstop == 0 )    !==  OFF time-stepping  ==!
         !
         IF( istp /= nit000 )   CALL day        ( istp )         ! Calendar (day was already called at nit000 in day_init)
                                CALL iom_setkt  ( istp - nit000 + 1, cxios_context )   ! say to iom that we are at time step kstp
#if defined key_sed_off
                                CALL dta_dyn_sed( istp )         ! Interpolation of the dynamical fields
#else
                                CALL dta_dyn    ( istp )         ! Interpolation of the dynamical fields
#endif
                                CALL trc_stp    ( istp )         ! time-stepping
#if ! defined key_sed_off
         IF( .NOT.ln_linssh )   CALL dta_dyn_swp( istp )         ! swap of sea  surface height and vertical scale factors
#endif
                                CALL stp_ctl    ( istp, indic )  ! Time loop: control and print
         istp = istp + 1
      END DO
      !
#if defined key_iomput
      CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
#endif

      !                            !------------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      IF(lwp) WRITE(numout,cform_aaa)                 ! Flag AAAAAAA

      IF( nstop /= 0 .AND. lwp ) THEN                 ! error print
         WRITE(ctmp1,*) '   ==>>>   nemo_gcm: a total of ', nstop, ' errors have been found'
         WRITE(ctmp2,*) '           Look for "E R R O R" messages in all existing ocean_output* files'
         CALL ctl_stop( ' ', ctmp1, ' ', ctmp2 )
      ENDIF
      !
      IF( ln_timing )   CALL timing_finalize
      !
      CALL nemo_closefile
      !
#if defined key_iomput
                     CALL xios_finalize   ! end mpp communications with xios
#else
      IF( lk_mpp )   CALL mppstop         ! end mpp communications
#endif
      !
      IF(lwm) THEN
         IF( nstop == 0 ) THEN   ;   STOP 0
         ELSE                    ;   STOP 123
         ENDIF
      ENDIF
      !
   END SUBROUTINE nemo_gcm


   SUBROUTINE nemo_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_init  ***
      !!
      !! ** Purpose :   initialization of the nemo model in off-line mode
      !!----------------------------------------------------------------------
      INTEGER ::   ios, ilocal_comm   ! local integers
      !!
      NAMELIST/namctl/ ln_ctl   , sn_cfctl, nn_print, nn_ictls, nn_ictle,   &
         &             nn_isplt , nn_jsplt, nn_jctls, nn_jctle,             &
         &             ln_timing, ln_diacfl
      NAMELIST/namcfg/ ln_read_cfg, cn_domcfg, ln_closea, ln_write_cfg, cn_domcfg_out, ln_use_jattr
      !!----------------------------------------------------------------------
      !
      cxios_context = 'nemo'
      !
      !                             !-------------------------------------------------!
      !                             !     set communicator & select the local rank    !
      !                             !  must be done as soon as possible to get narea  !
      !                             !-------------------------------------------------!
      !
#if defined key_iomput
      CALL xios_initialize( "for_xios_mpi_id", return_comm=ilocal_comm )   ! nemo local communicator given by xios
      CALL mpp_start( ilocal_comm )
#else
      CALL mpp_start( )
#endif
      !
      narea = mpprank + 1               ! mpprank: the rank of proc (0 --> mppsize -1 )
      lwm = (narea == 1)                ! control of output namelists
      !
      !                             !---------------------------------------------------------------!
      !                             ! Open output files, reference and configuration namelist files !
      !                             !---------------------------------------------------------------!
      !
      ! open ocean.output as soon as possible to get all output prints (including errors messages)
      IF( lwm )   CALL ctl_opn(     numout,        'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ! open reference and configuration namelist files
                  CALL ctl_opn( numnam_ref,        'namelist_ref',     'OLD', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
                  CALL ctl_opn( numnam_cfg,        'namelist_cfg',     'OLD', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      IF( lwm )   CALL ctl_opn(     numond, 'output.namelist.dyn', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ! open /dev/null file to be able to supress output write easily
      IF( Agrif_Root() ) THEN
                  CALL ctl_opn(     numnul,           '/dev/null', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
#ifdef key_agrif
      ELSE
                  numnul = Agrif_Parent(numnul)
#endif
      ENDIF
      !
      !                             !--------------------!
      !                             ! Open listing units !  -> need ln_ctl from namctl to define lwp
      !                             !--------------------!
      !
      REWIND( numnam_ref )              ! Namelist namctl in reference namelist
      READ  ( numnam_ref, namctl, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namctl in reference namelist' )
      REWIND( numnam_cfg )              ! Namelist namctl in confguration namelist
      READ  ( numnam_cfg, namctl, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namctl in configuration namelist' )
      !
      lwp = (narea == 1) .OR. ln_ctl    ! control of all listing output print
      !
      IF(lwp) THEN                            ! open listing units
         !
         IF( .NOT. lwm )   &           ! alreay opened for narea == 1
            &     CALL ctl_opn(     numout,        'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE., narea )
         !
         WRITE(numout,*)
         WRITE(numout,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(numout,*) '                       NEMO team'
         WRITE(numout,*) '                   Off-line TOP Model'
         WRITE(numout,*) '                NEMO version 4.0  (2019) '
         WRITE(numout,*)
         WRITE(numout,*) "           ._      ._      ._      ._      ._    "
         WRITE(numout,*) "       _.-._)`\_.-._)`\_.-._)`\_.-._)`\_.-._)`\_ "
         WRITE(numout,*)
         WRITE(numout,*) "           o         _,           _,             "
         WRITE(numout,*) "            o      .' (        .-' /             "
         WRITE(numout,*) "           o     _/..._'.    .'   /              "
         WRITE(numout,*) "      (    o .-'`      ` '-./  _.'               "
         WRITE(numout,*) "       )    ( o)           ;= <_         (       "
         WRITE(numout,*) "      (      '-.,\\__ __.-;`\   '.        )      "
         WRITE(numout,*) "       )  )       \) |`\ \)  '.   \      (   (   "
         WRITE(numout,*) "      (  (           \_/       '-._\      )   )  "
         WRITE(numout,*) "       )  )                        `     (   (   "
         WRITE(numout,*) "     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ "
         WRITE(numout,*)
         !
         WRITE(numout,cform_aaa)                                        ! Flag AAAAAAA
         !
      ENDIF
      !
      ! finalize the definition of namctl variables
      IF( sn_cfctl%l_config ) THEN
         ! Activate finer control of report outputs
         ! optionally switch off output from selected areas (note this only
         ! applies to output which does not involve global communications)
         IF( ( narea < sn_cfctl%procmin .OR. narea > sn_cfctl%procmax  ) .OR. &
           & ( MOD( narea - sn_cfctl%procmin, sn_cfctl%procincr ) /= 0 ) )    &
           &   CALL nemo_set_cfctl( sn_cfctl, .FALSE., .FALSE. )
      ELSE
         ! Use ln_ctl to turn on or off all options.
         CALL nemo_set_cfctl( sn_cfctl, ln_ctl, .TRUE. )
      ENDIF
      !
      IF(lwm) WRITE( numond, namctl )
      !
      !                             !------------------------------------!
      !                             !  Set global domain size parameters !
      !                             !------------------------------------!
      !     
      REWIND( numnam_ref )              ! Namelist namcfg in reference namelist
      READ  ( numnam_ref, namcfg, IOSTAT = ios, ERR = 903 )
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namcfg in reference namelist' )
      REWIND( numnam_cfg )              ! Namelist namcfg in confguration namelist
      READ  ( numnam_cfg, namcfg, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'namcfg in configuration namelist' )   
      !
      IF( ln_read_cfg ) THEN              ! Read sizes in domain configuration file
         CALL domain_cfg ( cn_cfg, nn_cfg, jpiglo, jpjglo, jpkglo, jperio )
      ELSE                                ! user-defined namelist
         CALL usr_def_nam( cn_cfg, nn_cfg, jpiglo, jpjglo, jpkglo, jperio )
      ENDIF
      !
      IF(lwm)   WRITE( numond, namcfg )
      l_offline = .true.                  ! passive tracers are run offline
      !
      !                             !-----------------------------------------!
      !                             ! mpp parameters and domain decomposition !
      !                             !-----------------------------------------!
      !
      CALL mpp_init

      ! Now we know the dimensions of the grid and numout has been set: we can allocate arrays
      CALL nemo_alloc()

      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL nemo_ctl                          ! Control prints
      !
      !                                      ! General initialization
      IF( ln_timing    )   CALL timing_init
      IF( ln_timing    )   CALL timing_start( 'nemo_init')
      !
                           CALL     phy_cst         ! Physical constants
                           CALL     eos_init        ! Equation of state
      IF( lk_c1d       )   CALL     c1d_init        ! 1D column configuration
                           CALL     dom_init("OPA") ! Domain
      IF( ln_ctl       )   CALL prt_ctl_init        ! Print control

                           CALL  istate_init    ! ocean initial state (Dynamics and tracers)

                           CALL     sbc_init    ! Forcings : surface module
                           CALL     bdy_init    ! Open boundaries initialisation                          

      !                                      ! Tracer physics
                           CALL ldf_tra_init    ! Lateral ocean tracer physics
                           CALL ldf_eiv_init    ! Eddy induced velocity param
                           CALL tra_ldf_init    ! lateral mixing
      IF( l_ldfslp     )   CALL ldf_slp_init    ! slope of lateral mixing
      IF( ln_traqsr    )   CALL tra_qsr_init    ! penetrative solar radiation
      IF( ln_trabbl    )   CALL tra_bbl_init    ! advective (and/or diffusive) bottom boundary layer scheme

      !                                      ! Passive tracers
                           CALL trc_nam_run    ! Needed to get restart parameters for passive tracers
                           CALL trc_rst_cal( nit000, 'READ' )   ! calendar
#if defined key_sed_off
                           CALL dta_dyn_sed_init ! Initialization for the dynamics
#else
                           CALL dta_dyn_init   ! Initialization for the dynamics
#endif

                           CALL     trc_init   ! Passive tracers initialization
                           CALL dia_ptr_init   ! Poleward TRansports initialization
                           
      IF(lwp) WRITE(numout,cform_aaa)           ! Flag AAAAAAA
      !
      IF( ln_timing    )   CALL timing_stop( 'nemo_init')
      !
   END SUBROUTINE nemo_init


   SUBROUTINE nemo_ctl
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_ctl  ***
      !!
      !! ** Purpose :   control print setting
      !!
      !! ** Method  : - print namctl information and check some consistencies
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'nemo_ctl: Control prints'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '   Namelist namctl'
         WRITE(numout,*) '      run control (for debugging)     ln_ctl     = ', ln_ctl
         WRITE(numout,*) '       finer control over o/p sn_cfctl%l_config  = ', sn_cfctl%l_config
         WRITE(numout,*) '                              sn_cfctl%l_runstat = ', sn_cfctl%l_runstat
         WRITE(numout,*) '                              sn_cfctl%l_trcstat = ', sn_cfctl%l_trcstat
         WRITE(numout,*) '                              sn_cfctl%l_oceout  = ', sn_cfctl%l_oceout
         WRITE(numout,*) '                              sn_cfctl%l_layout  = ', sn_cfctl%l_layout
         WRITE(numout,*) '                              sn_cfctl%l_mppout  = ', sn_cfctl%l_mppout
         WRITE(numout,*) '                              sn_cfctl%l_mpptop  = ', sn_cfctl%l_mpptop
         WRITE(numout,*) '                              sn_cfctl%procmin   = ', sn_cfctl%procmin  
         WRITE(numout,*) '                              sn_cfctl%procmax   = ', sn_cfctl%procmax  
         WRITE(numout,*) '                              sn_cfctl%procincr  = ', sn_cfctl%procincr 
         WRITE(numout,*) '                              sn_cfctl%ptimincr  = ', sn_cfctl%ptimincr 
         WRITE(numout,*) '      level of print                  nn_print   = ', nn_print
         WRITE(numout,*) '      Start i indice for SUM control  nn_ictls   = ', nn_ictls
         WRITE(numout,*) '      End i indice for SUM control    nn_ictle   = ', nn_ictle
         WRITE(numout,*) '      Start j indice for SUM control  nn_jctls   = ', nn_jctls
         WRITE(numout,*) '      End j indice for SUM control    nn_jctle   = ', nn_jctle
         WRITE(numout,*) '      number of proc. following i     nn_isplt   = ', nn_isplt
         WRITE(numout,*) '      number of proc. following j     nn_jsplt   = ', nn_jsplt
         WRITE(numout,*) '      timing by routine               ln_timing  = ', ln_timing
         WRITE(numout,*) '      CFL diagnostics                 ln_diacfl  = ', ln_diacfl
      ENDIF
      !
      nprint    = nn_print          ! convert DOCTOR namelist names into OLD names
      nictls    = nn_ictls
      nictle    = nn_ictle
      njctls    = nn_jctls
      njctle    = nn_jctle
      isplt     = nn_isplt
      jsplt     = nn_jsplt

      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namcfg'
         WRITE(numout,*) '      read domain configuration file              ln_read_cfg      = ', ln_read_cfg
         WRITE(numout,*) '         filename to be read                         cn_domcfg     = ', TRIM(cn_domcfg)
         WRITE(numout,*) '         keep closed seas in the domain (if exist)   ln_closea     = ', ln_closea
         WRITE(numout,*) '      create a configuration definition file      ln_write_cfg     = ', ln_write_cfg
         WRITE(numout,*) '         filename to be written                      cn_domcfg_out = ', TRIM(cn_domcfg_out)
         WRITE(numout,*) '      use file attribute if exists as i/p j-start ln_use_jattr     = ', ln_use_jattr
      ENDIF
      IF( .NOT.ln_read_cfg )   ln_closea = .false.   ! dealing possible only with a domcfg file
      !
      !                             ! Parameter control
      !
      IF( ln_ctl ) THEN                 ! sub-domain area indices for the control prints
         IF( lk_mpp .AND. jpnij > 1 ) THEN
            isplt = jpni   ;   jsplt = jpnj   ;   ijsplt = jpni*jpnj   ! the domain is forced to the real split domain
         ELSE
            IF( isplt == 1 .AND. jsplt == 1  ) THEN
               CALL ctl_warn( ' - isplt & jsplt are equal to 1',   &
                  &           ' - the print control will be done over the whole domain' )
            ENDIF
            ijsplt = isplt * jsplt            ! total number of processors ijsplt
         ENDIF
         IF(lwp) WRITE(numout,*)'          - The total number of processors over which the'
         IF(lwp) WRITE(numout,*)'            print control will be done is ijsplt : ', ijsplt
         !
         !                              ! indices used for the SUM control
         IF( nictls+nictle+njctls+njctle == 0 )   THEN    ! print control done over the default area
            lsp_area = .FALSE.
         ELSE                                             ! print control done over a specific  area
            lsp_area = .TRUE.
            IF( nictls < 1 .OR. nictls > jpiglo )   THEN
               CALL ctl_warn( '          - nictls must be 1<=nictls>=jpiglo, it is forced to 1' )
               nictls = 1
            ENDIF
            IF( nictle < 1 .OR. nictle > jpiglo )   THEN
               CALL ctl_warn( '          - nictle must be 1<=nictle>=jpiglo, it is forced to jpiglo' )
               nictle = jpiglo
            ENDIF
            IF( njctls < 1 .OR. njctls > jpjglo )   THEN
               CALL ctl_warn( '          - njctls must be 1<=njctls>=jpjglo, it is forced to 1' )
               njctls = 1
            ENDIF
            IF( njctle < 1 .OR. njctle > jpjglo )   THEN
               CALL ctl_warn( '          - njctle must be 1<=njctle>=jpjglo, it is forced to jpjglo' )
               njctle = jpjglo
            ENDIF
         ENDIF
      ENDIF
      !
      IF( 1._wp /= SIGN(1._wp,-0._wp)  )   CALL ctl_stop( 'nemo_ctl: The intrinsec SIGN function follows f2003 standard.',  &
         &                                                'Compile with key_nosignedzero enabled' )
      !
   END SUBROUTINE nemo_ctl


   SUBROUTINE nemo_closefile
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_closefile  ***
      !!
      !! ** Purpose :   Close the files
      !!----------------------------------------------------------------------
      !
      IF( lk_mpp )   CALL mppsync
      !
      CALL iom_close                                 ! close all input/output files managed by iom_*
      !
      IF( numstp     /= -1 )   CLOSE( numstp     )   ! time-step file
      IF( numnam_ref /= -1 )   CLOSE( numnam_ref )   ! oce reference namelist
      IF( numnam_cfg /= -1 )   CLOSE( numnam_cfg )   ! oce configuration namelist
      IF( numout     /=  6 )   CLOSE( numout     )   ! standard model output file
      IF( lwm.AND.numond  /= -1 )   CLOSE( numond          )   ! oce output namelist
      !
      numout = 6                                     ! redefine numout in case it is used after this point...
      !
   END SUBROUTINE nemo_closefile


   SUBROUTINE nemo_alloc
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_alloc  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the OPA modules
      !!
      !! ** Method  :
      !!----------------------------------------------------------------------
      USE diawri ,   ONLY : dia_wri_alloc
      USE dom_oce,   ONLY : dom_oce_alloc
      USE zdf_oce,   ONLY : zdf_oce_alloc
      USE trc_oce,   ONLY : trc_oce_alloc
      USE bdy_oce,   ONLY : bdy_oce_alloc
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        oce_alloc    ()          ! ocean 
      ierr = ierr + dia_wri_alloc()
      ierr = ierr + dom_oce_alloc()          ! ocean domain
      ierr = ierr + zdf_oce_alloc()          ! ocean vertical physics
      ierr = ierr + trc_oce_alloc()          ! shared TRC / TRA arrays
      ierr = ierr + bdy_oce_alloc()    ! bdy masks (incl. initialization)
      
      !
      CALL mpp_sum( 'nemogcm', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'nemo_alloc: unable to allocate standard ocean arrays' )
      !
   END SUBROUTINE nemo_alloc

   SUBROUTINE nemo_set_cfctl(sn_cfctl, setto, for_all )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_set_cfctl  ***
      !!
      !! ** Purpose :   Set elements of the output control structure to setto.
      !!                for_all should be .false. unless all areas are to be
      !!                treated identically.
      !!
      !! ** Method  :   Note this routine can be used to switch on/off some
      !!                types of output for selected areas but any output types
      !!                that involve global communications (e.g. mpp_max, glob_sum)
      !!                should be protected from selective switching by the
      !!                for_all argument
      !!----------------------------------------------------------------------
      LOGICAL :: setto, for_all
      TYPE(sn_ctl) :: sn_cfctl
      !!----------------------------------------------------------------------
      IF( for_all ) THEN
         sn_cfctl%l_runstat = setto
         sn_cfctl%l_trcstat = setto
      ENDIF
      sn_cfctl%l_oceout  = setto
      sn_cfctl%l_layout  = setto
      sn_cfctl%l_mppout  = setto
      sn_cfctl%l_mpptop  = setto
   END SUBROUTINE nemo_set_cfctl

   SUBROUTINE istate_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_init  ***
      !!
      !! ** Purpose :   Initialization to zero of the dynamics and tracers.
      !!----------------------------------------------------------------------
      !
      !     now fields         !     after fields      !
      un   (:,:,:)   = 0._wp   ;   ua(:,:,:) = 0._wp   !
      vn   (:,:,:)   = 0._wp   ;   va(:,:,:) = 0._wp   !
      wn   (:,:,:)   = 0._wp   !                       !
      hdivn(:,:,:)   = 0._wp   !                       !
      tsn  (:,:,:,:) = 0._wp   !                       !
      !
      rhd  (:,:,:) = 0.e0
      rhop (:,:,:) = 0.e0
      rn2  (:,:,:) = 0.e0
      !
   END SUBROUTINE istate_init


   SUBROUTINE stp_ctl( kt, kindic )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE stp_ctl  ***
      !!
      !! ** Purpose :   Control the run
      !!
      !! ** Method  : - Save the time step in numstp
      !!
      !! ** Actions :   'time.step' file containing the last ocean time-step
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt      ! ocean time-step index
      INTEGER, INTENT(inout) ::   kindic  ! indicator of solver convergence
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 .AND. lwm ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'stp_ctl : time-stepping control'
         WRITE(numout,*) '~~~~~~~'
         ! open time.step file
         CALL ctl_opn( numstp, 'time.step', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
      ENDIF
      !
      IF(lwm) WRITE ( numstp, '(1x, i8)' )   kt      !* save the current time step in numstp
      IF(lwm) REWIND( numstp )                       ! --------------------------
      !
   END SUBROUTINE stp_ctl
   !!======================================================================
END MODULE nemogcm
