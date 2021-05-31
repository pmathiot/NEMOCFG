********************
eORCA025.L121-OPM018
********************

Summary
=======

Compare to eORCA025.L121-OPM017, the only changed is that we switched off GM.

Namelist
========

Only the change compare to the reference (eORCA025.L121-OPM017) a mentioned here:

namelist_oce
------------

namtra_eiv
^^^^^^^^^^

* GM is switch off

.. code-block:: console

    !-----------------------------------------------------------------------
    &namtra_eiv    !   eddy induced velocity param.                         (default: OFF)
    !-----------------------------------------------------------------------
       ln_ldfeiv   = .false.    ! use eddy induced velocity parameterization
          !
          !                        !  Coefficients:
          nn_aei_ijk_t    = 0           !  space/time variation of eddy coefficient:
          !                             !   =-20 (=-30)    read in eddy_induced_velocity_2D.nc (..._3D.nc) file
          !                             !   =  0           constant
          !                             !   = 10 F(k)      =ldf_c1d
          !                             !   = 20 F(i,j)    =ldf_c2d
          !                             !   = 21 F(i,j,t)  =Treguier et al. JPO 1997 formulation
          !                             !   = 30 F(i,j,k)  =ldf_c2d * ldf_c1d
          !                        !  time invariant coefficients:  aei0 = 1/2  Ue*Le
          rn_Ue        = 0.02           !  lateral diffusive velocity [m/s] (nn_aei_ijk_t= 0, 10, 20, 30)
          rn_Le        = 200.e+3        !  lateral diffusive length   [m]   (nn_aei_ijk_t= 0, 10)
          !
          ln_ldfeiv_dia =.false.   ! diagnose eiv stream function and velocities
    /

Monitoring
==========

see :ref:`eORCA025.L121-_monitoring`
