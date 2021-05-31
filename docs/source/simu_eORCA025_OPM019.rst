********************
eORCA025.L121-OPM019
********************

Summary
=======

Compare to eORCA025.L121-OPM017, only the initial condition changed. Instead of filling the cavity by JRA UKMO output we fill it with extrapolation of WOA2018.

Namelist
========

Only the change compare to the reference (eORCA025.L121-OPM017) a mentioned here:

namelist_oce
------------

namtsd
~~~~~~

* tradmp input file changed to be equal to initial condition as in OPM017 because AABW restoring turned off.

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
     sn_tem_ini  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.2', -1. , 't_an',  .true.   , .true. , 'yearly'  ,  ''         , ' '      , ' '
     sn_sal_ini  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.2', -1. , 's_an',  .true.   , .true. , 'yearly'  ,  ''         , ' '      , ' '
     ! data used for damping ( tradmp)
     sn_tem_dmp  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.2', -1. , 't_an',  .true.      , .true., 'yearly'   ,  ''         , ' '      , ' '
     sn_sal_dmp  = 'eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.2', -1. , 's_an',  .true.      , .true., 'yearly'   ,  ''         , ' '      , ' '
     !
  /

Input files
===========

  Only the file changed between the reference (:ref:`bug_input_eO025-OPM017`) and this simulation are described.

initial condition
-----------------
      - filename: :ref:`eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.2.nc <eORCA025.L121_WOA2018_c3.0_d1.0_v19812010.5.2>`
      - variables: t_an, s_an
      - frequency: monthly

Monitoring
==========

see :ref:`eORCA025.L121-_monitoring`
