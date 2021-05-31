********************
eORCA025.L121-OPM017
********************

Summary
=======

Compare to eORCA025.L121-OPM016, we switched off the AABW restoring, decreased the diffusion as well as the GM coefficient (get back to UKMO value)

Namelist
========

Only the change compare to the reference (eORCA025.L121-OPM016) a mentioned here:

namelist_oce
------------

namtsd
~~~~~~

* tradmp input file changed to be equal to initial condition as in OPM006 because AABW restoring turned off.

.. code-block:: console

  !-----------------------------------------------------------------------
  &namtsd_drk    !    Temperature & Salinity Data  (init/dmp)             (default: OFF)
  !              !   if key_drakkar, **only**  namtsd_drk is read
  !-----------------------------------------------------------------------
     ln_tsd_init   = .true.   !  Initialisation of ocean T & S with T &S input data (T) or not (F)
     ln_tsd_dmp = .false.     !  damping of ocean T & S toward T &S input data (T) or not (F)

     cn_dir        = './'     !  root directory for the location of the temperature and salinity file
     !___________!_____________________________________!___________________!___________!_____________!________!___________!_____________!__________!_______________!
     !           !  file name                          ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights     ! rotation ! land/sea mask !
     !           !                                     !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !   filename  ! pairing  !    filename   !
     ! data used for initial condition (istate)
     sn_tem_ini  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.1', -1., 'votemper',  .true.   , .true. , 'yearly'  ,  ''         , ' '      , ' '
     sn_sal_ini  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.1', -1., 'vosaline',  .true.   , .true. , 'yearly'  ,  ''         , ' '      , ' '
     ! data used for damping ( tradmp)
     sn_tem_dmp  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.1', -1., 'votemper',  .true.      , .true., 'yearly'   ,  ''         , ' '      , ' '
     sn_sal_dmp  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.1', -1., 'vosaline',  .true.      , .true., 'yearly'   ,  ''         , ' '      , ' '
     !
  /

namtra_ldf
^^^^^^^^^^

* We get back to UKMO values (150m2/s)

.. code-block:: console

    !-----------------------------------------------------------------------
    &namtra_ldf    !   lateral diffusion scheme for tracers                 (default: NO selection)
    !-----------------------------------------------------------------------
       !                       !  Operator type:
       ln_traldf_OFF   = .false.   !  No explicit diffusion
       ln_traldf_lap   = .true.    !    laplacian operator
       ln_traldf_blp   = .false.   !  bilaplacian operator
       !
       !                       !  Direction of action:
       ln_traldf_lev   = .false.   !  iso-level
       ln_traldf_hor   = .false.   !  horizontal  (geopotential)
       ln_traldf_iso   = .true.    !  iso-neutral (standard operator)
       ln_traldf_triad = .false.   !  iso-neutral (triad    operator)
       !
       !                             !  iso-neutral options:
       ln_traldf_msc   = .false.   !  Method of Stabilizing Correction      (both operators)
       rn_slpmax       =  0.01     !  slope limit                           (both operators)
       ln_triad_iso    = .false.   !  pure horizontal mixing in ML              (triad only)
       rn_sw_triad     = 1         !  =1 switching triad ; =0 all 4 triads used (triad only)
       ln_botmix_triad = .false.   !  lateral mixing on bottom                  (triad only)
       !
       !                       !  Coefficients:
       nn_aht_ijk_t    = 20        !  space/time variation of eddy coefficient:
          !                             !   =-20 (=-30)    read in eddy_diffusivity_2D.nc (..._3D.nc) file
          !                             !   =  0           constant
          !                             !   = 10 F(k)      =ldf_c1d
          !                             !   = 20 F(i,j)    =ldf_c2d
          !                             !   = 21 F(i,j,t)  =Treguier et al. JPO 1997 formulation
          !                             !   = 30 F(i,j,k)  =ldf_c2d * ldf_c1d
          !                             !   = 31 F(i,j,k,t)=F(local velocity and grid-spacing)
          !                        !  time invariant coefficients:  aht0 = 1/2  Ud*Ld   (lap case)
          !                             !                           or   = 1/12 Ud*Ld^3 (blp case)
          rn_Ud        = 0.011          !  lateral diffusive velocity [m/s] (nn_aht_ijk_t= 0, 10, 20, 30)
          rn_Ld        = 200.e+3        !  lateral diffusive length   [m]   (nn_aht_ijk_t= 0, 10)
    /

namtra_eiv
^^^^^^^^^^

* Based on UKMO test and Julie D. discussion we switch one GM (150m2/s) (300m2/s2 in OPM016)

.. code-block:: console

    !-----------------------------------------------------------------------
    &namtra_eiv    !   eddy induced velocity param.                         (default: OFF)
    !-----------------------------------------------------------------------
       ln_ldfeiv   = .true.    ! use eddy induced velocity parameterization
          !
          !                        !  Coefficients:
          nn_aei_ijk_t    = 21          !  space/time variation of eddy coefficient:
          !                             !   =-20 (=-30)    read in eddy_induced_velocity_2D.nc (..._3D.nc) file
          !                             !   =  0           constant
          !                             !   = 10 F(k)      =ldf_c1d
          !                             !   = 20 F(i,j)    =ldf_c2d
          !                             !   = 21 F(i,j,t)  =Treguier et al. JPO 1997 formulation
          !                             !   = 30 F(i,j,k)  =ldf_c2d * ldf_c1d
          !                        !  time invariant coefficients:  aei0 = 1/2  Ue*Le
          rn_Ue        = 0.03           !  lateral diffusive velocity [m/s] (nn_aei_ijk_t= 0, 10, 20, 30)
          rn_Le        = 10.e+3         !  lateral diffusive length   [m]   (nn_aei_ijk_t= 0, 10)
          !
          ln_ldfeiv_dia =.false.   ! diagnose eiv stream function and velocities
          nn_ldfeiv_shape = 2           !  shape of bounding coefficient    (nn_aei_ijk_t= 21 only)
    /

namtra_dmp_drk
--------------

* we switch off AABW restoring.

.. code-block:: console

    !-----------------------------------------------------------------------
    &namtra_dmp_drk !   tracer: T & S newtonian damping                      (default: OFF)
    !-----------------------------------------------------------------------
       nn_hdmp     =   -2      !  -2 = Drakkar customisation ( use dtacof in tradmp.F90 )
                               !  any other value : Nemo standard code
       nn_file     =    1      !  create a damping.coeff NetCDF file (=1) or not (=0)
                               !  The output file can then be used as input resto file (cn_resto)
       ln_dmpmask  = .false.    !  Read dmp_mask.nc file  when T (between 0 and 1 )
          rn_timsk    =  730.     !  Time scale used for dmp_mask
          cn_dir      =  './'     ! directtory wher to find damping mask
       !___________!____________!___________________!___________!_____________!________!___________!___________!__________!_______________!
       !           !  file name ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights e ! rotation ! land/sea mask !
       !           !            !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !  filename ! pairing  !    filename   !
       sn_dmp      = 'eORCA025.L121_dmpmask_b0.2_c0.3_d1.0_v0.0' ,  -12. ,  'wdmp' , .false.  , .true. , 'yearly'  ,   ''      ,   ''     ,   ''
    /

Input files
===========

Only the file changed between the reference (:ref:`bug_input_eO025-OPM016`) and this simulation are described.

damping data
------------
    - filename: :ref:`eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.1`

Monitoring
==========

see :ref:`eORCA025.L121-_monitoring`
