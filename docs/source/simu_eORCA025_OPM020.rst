********************
eORCA025.L121-OPM020
********************

Summary
=======

Compare to eORCA025.L121-OPM019, only the start date changed. The simulation start in 1979 instead of 1959.

Namelist
========

Only the change compare to the reference (eORCA025.L121-OPM017) a mentioned here:

namelist_oce
------------

namtsd
~~~~~~

* the simulation start in 1979 instead of 1959.

.. code-block:: console

    !-----------------------------------------------------------------------
    &namrun        !   parameters of the run
    !-----------------------------------------------------------------------
      nn_no         =   <NN_NO>         !  job number
      cn_exp        =   eORCA025.L121-OPM020      !  experience name
      nn_it000      =    <NIT000>       !  first time step
      nn_itend      =    <NITEND>       !  last  time step
      nn_date0      =  19790101         !  initial calendar date yymmdd (used if nrstdt=1)
      nn_time0    =       0             !  initial time of day in hhmm
      nn_leapy    =       0             !  Leap year calendar (1) or not (0)
      ln_rstart   = <RESTART>           !  start from rest (F) or from a restart file (T)
         nn_euler    =       1   !  = 0 : start with forward time step if ln_rstart=T
         nn_rstctl   =       2   !  restart control => activated only if ln_rstart = T
                                 !    = 0 nn_date0 read in namelist ; nn_it000 : read in namelist
                                 !    = 1 nn_date0 read in namelist ; nn_it000 : check consistancy between namelist and restart
                                 !    = 2 nn_date0 read in restart  ; nn_it000 : check consistancy between namelist and restart
         cn_ocerst_in  = "restart" !  suffix of ocean restart name (input)
         cn_ocerst_indir = "<CN_DIRRST>"       !  directory from which to read input ocean restarts
         cn_ocerst_out = "restart" !  suffix of ocean restart name (output)
         cn_ocerst_outdir = "<CN_DIRRST>"      !  directory in which to write output ocean restarts
      nn_istate   =       0   !  output the initial state (1) or not (0)
      ln_rst_list = .false.   !  output restarts at list of times using nn_stocklist (T) or at set frequency with nn_stock (F)
      nn_stock    =       0   !  used only if ln_rst_list = F: output restart freqeuncy (modulo referenced to 1)
         !                          !    =  0 force to write restart files only at the end of the run
         !                          !    = -1 do not do any restart
      nn_stocklist = 0,0,0,0,0,0,0,0,0,0 ! List of timesteps when a restart file is to be written
      nn_write    =       0   !  used only if key_iomput is not defined: output frequency (modulo referenced to nn_it000)
         !                          !    =  0 force to write output files only at the end of the run
         !                          !    = -1 do not do any output file
      ln_mskland  = .false.   !  mask land points in NetCDF outputs
      ln_cfmeta   = .false.   !  output additional data to netCDF files required for compliance with the CF metadata standard
      ln_clobber  = .true.    !  clobber (overwrite) an existing file
      nn_chunksz  =       0   !  chunksize (bytes) for NetCDF file (works only with iom_nf90 routines)
      ln_xios_read = .FALSE.  !  use XIOS to read restart file (only for a single file restart)
      nn_wxios = 0      !  use XIOS to write restart file 0 - no, 1 - single file output, 2 - multiple file output
    /

Monitoring
==========

see :ref:`eORCA025.L121-_monitoring`
