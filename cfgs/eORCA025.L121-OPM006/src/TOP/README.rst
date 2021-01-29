***************
Oceanic tracers
***************

.. todo::



.. contents::
   :local:

TOP (Tracers in the Ocean Paradigm) is the NEMO hardwired interface toward
biogeochemical models and provide the physical constraints/boundaries for oceanic tracers.
It consists of a modular framework to handle multiple ocean tracers,
including also a variety of built-in modules.

This component of the NEMO framework allows one to exploit available modules (see below) and
further develop a range of applications, spanning from the implementation of a dye passive tracer to
evaluate dispersion processes (by means of MY_TRC), track water masses age (AGE module),
assess the ocean interior penetration of persistent chemical compounds
(e.g., gases like CFC or even PCBs), up to the full set of equations involving
marine biogeochemical cycles.

Structure
=========

TOP interface has the following location in the source code :file:`./src/TOP` and
the following modules are available:

:file:`TRP`
   Interface to NEMO physical core for computing tracers transport

:file:`CFC`
   Inert carbon tracers (CFC11,CFC12,SF6)

:file:`C14`
   Radiocarbon passive tracer

:file:`AGE`
   Water age tracking

:file:`MY_TRC`
   Template for creation of new modules and external BGC models coupling

:file:`PISCES`
   Built in BGC model. See :cite:`gmd-8-2465-2015` for a throughout description.

The usage of TOP is activated
*i)* by including in the configuration definition the component ``TOP`` and
*ii)* by adding the macro ``key_top`` in the configuration CPP file
(see for more details :forge:`"Learn more about the model" <wiki/Users>`).

As an example, the user can refer to already available configurations in the code,
``GYRE_PISCES`` being the NEMO biogeochemical demonstrator and
``GYRE_BFM`` to see the required configuration elements to couple with an external biogeochemical model
(see also Section 4) .

Note that, since version 4.0,
TOP interface core functionalities are activated by means of logical keys and
all submodules preprocessing macros from previous versions were removed.

Here below the list of preprocessing keys that applies to the TOP interface (beside ``key_top``):

``key_iomput``
   use XIOS I/O

``key_agrif``
   enable AGRIF coupling

``key_trdtrc`` & ``key_trdmxl_trc``
   trend computation for tracers

Synthetic Workflow
==================

A synthetic description of the TOP interface workflow is given below to
summarize the steps involved in the computation of biogeochemical and physical trends and
their time integration and outputs,
by reporting also the principal Fortran subroutine herein involved.

Model initialization (:file:`./src/OCE/nemogcm.F90`)
----------------------------------------------------

Call to ``trc_init`` subroutine (:file:`./src/TOP/trcini.F90`) to initialize TOP.

.. literalinclude:: ../../../src/TOP/trcini.F90
   :language:        fortran
   :lines:           41-86
   :emphasize-lines: 21,30-32,38-40
   :caption:         ``trc_init`` subroutine

Time marching procedure (:file:`./src/OCE/step.F90`)
----------------------------------------------------

Call to ``trc_stp`` subroutine (:file:`./src/TOP/trcstp.F90`) to compute/update passive tracers.

.. literalinclude:: ../../../src/TOP/trcstp.F90
   :language:        fortran
   :lines:           46-125
   :emphasize-lines: 42,55-57
   :caption:         ``trc_stp`` subroutine

BGC trends computation for each submodule (:file:`./src/TOP/trcsms.F90`)
------------------------------------------------------------------------

.. literalinclude:: ../../../src/TOP/trcsms.F90
   :language:        fortran
   :lines:           21
   :caption:         :file:`trcsms` snippet

Physical trends computation (:file:`./src/TOP/TRP/trctrp.F90`)
--------------------------------------------------------------

.. literalinclude:: ../../../src/TOP/TRP/trctrp.F90
   :language:        fortran
   :lines:           46-95
   :emphasize-lines: 17,21,29,33-35
   :caption:         ``trc_trp`` subroutine

Namelists walkthrough
=====================

:file:`namelist_top`
--------------------

Here below are listed the features/options of the TOP interface accessible through
the :file:`namelist_top_ref` and modifiable by means of :file:`namelist_top_cfg`
(as for NEMO physical ones).

Note that ``##`` is used to refer to a number in an array field.

.. literalinclude:: ../../namelists/namtrc_run
   :language: fortran

.. literalinclude:: ../../namelists/namtrc
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_dta
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_adv
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_ldf
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_rad
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_snk
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_dmp
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_ice
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_trd
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_bc
   :language: fortran

.. literalinclude:: ../../namelists/namtrc_bdy
   :language: fortran

.. literalinclude:: ../../namelists/namage
   :language: fortran

Two main types of data structure are used within TOP interface
to initialize tracer properties (1) and
to provide related initial and boundary conditions (2).

1. TOP tracers initialization: ``sn_tracer`` (``&namtrc``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Beside providing name and metadata for tracers,
here are also defined the use of initial (``sn_tracer%llinit``) and
boundary (``sn_tracer%llsbc, sn_tracer%llcbc, sn_tracer%llobc``) conditions.

In the following, an example of the full structure definition is given for
two idealized tracers both with initial conditions given,
while the first has only surface boundary forcing and
the second both surface and coastal forcings:

.. code-block:: fortran

   !             !    name   !           title of the field            !   units    ! initial data ! sbc   !   cbc  !   obc  !
   sn_tracer(1)  = 'TRC1'    , 'Tracer 1 Concentration                ',   ' - '    ,  .true.      , .true., .false., .true.
   sn_tracer(2)  = 'TRC2 '   , 'Tracer 2 Concentration                ',   ' - '    ,  .true.      , .true., .true. , .false.

As tracers in BGC models are increasingly growing,
the same structure can be written also in a more compact and readable way:

.. code-block:: fortran

   !             !    name   !           title of the field            !   units    ! initial data !
   sn_tracer(1)  = 'TRC1'    , 'Tracer 1 Concentration                ',   ' - '    ,   .true.
   sn_tracer(2)  = 'TRC2 '   , 'Tracer 2 Concentration                ',   ' - '    ,   .true.
   ! sbc
   sn_tracer(1)%llsbc = .true.
   sn_tracer(2)%llsbc = .true.
   ! cbc
   sn_tracer(2)%llcbc = .true.

The data structure is internally initialized by code with dummy names and
all initialization/forcing logical fields set to ``.false.`` .

2. Structures to read input initial and boundary conditions: ``&namtrc_dta`` (``sn_trcdta``), ``&namtrc_bc`` (``sn_trcsbc`` / ``sn_trccbc`` / ``sn_trcobc``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The overall data structure (Fortran type) is based on the general one defined for NEMO core in the SBC component
(see details in ``SBC`` Chapter of :doc:`Reference Manual <cite>` on Input Data specification).

Input fields are prescribed within ``&namtrc_dta`` (with ``sn_trcdta`` structure),
while Boundary Conditions are applied to the model by means of ``&namtrc_bc``,
with dedicated structure fields for surface (``sn_trcsbc``), riverine (``sn_trccbc``), and
lateral open (``sn_trcobc``) boundaries.

The following example illustrates the data structure in the case of initial condition for
a single tracer contained in the file named :file:`tracer_1_data.nc`
(``.nc`` is implicitly assumed in namelist filename),
with a doubled initial value, and located in the :file:`usr/work/model/inputdata` folder:

.. code-block:: fortran

   !               !  file name             ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
   !               !                        !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
     sn_trcdta(1)  = 'tracer_1_data'        ,        -12        ,  'TRC1'   ,    .false.   , .true. , 'yearly'  , ''       , ''       , ''
     rf_trfac(1) = 2.0
     cn_dir = 'usr/work/model/inputdata/'

Note that, the Lateral Open Boundaries conditions are applied on
the segments defined for the physical core of NEMO
(see ``BDY`` description in the :doc:`Reference Manual <cite>`).

:file:`namelist_trc`
--------------------

Here below the description of :file:`namelist_trc_ref` used to handle Carbon tracers modules,
namely CFC and C14.

.. literalinclude:: ../../../cfgs/SHARED/namelist_trc_ref
   :language: fortran
   :lines: 7,17,26,34
   :caption: :file:`namelist_trc_ref` snippet

``MY_TRC`` interface for coupling external BGC models
=====================================================

The generalized interface is pivoted on MY_TRC module that contains template files to
build the coupling between
NEMO and any external BGC model.

The call to MY_TRC is activated by setting ``ln_my_trc = .true.`` (in ``&namtrc``)

The following 6 fortran files are available in MY_TRC with the specific purposes here described.

:file:`par_my_trc.F90`
   This module allows to define additional arrays and public variables to
   be used within the MY_TRC interface

:file:`trcini_my_trc.F90`
   Here are initialized user defined namelists and
   the call to the external BGC model initialization procedures to populate general tracer array
   (``trn`` and ``trb``).
   Here are also likely to be defined support arrays related to system metrics that
   could be needed by the BGC model.

:file:`trcnam_my_trc.F90`
   This routine is called at the beginning of ``trcini_my_trc`` and
   should contain the initialization of additional namelists for the BGC model or user-defined code.

:file:`trcsms_my_trc.F90`
   The routine performs the call to Boundary Conditions and its main purpose is to
   contain the Source-Minus-Sinks terms due to the biogeochemical processes of the external model.
   Be aware that lateral boundary conditions are applied in trcnxt routine.

   .. warning::
      The routines to compute the light penetration along the water column and
      the tracer vertical sinking should be defined/called in here,
      as generalized modules are still missing in the code.

:file:`trcice_my_trc.F90`
   Here it is possible to prescribe the tracers concentrations in the sea-ice that
   will be used as boundary conditions when ice melting occurs (``nn_ice_tr = 1`` in ``&namtrc_ice``).
   See e.g. the correspondent PISCES subroutine.

:file:`trcwri_my_trc.F90`
   This routine performs the output of the model tracers (only those defined in ``&namtrc``) using
   IOM module (see chapter “Output and Diagnostics” in the :doc:`Reference Manual <cite>`).
   It is possible to place here the output of additional variables produced by the model,
   if not done elsewhere in the code, using the call to ``iom_put``.

Coupling an external BGC model using NEMO framework
===================================================

The coupling with an external BGC model through the NEMO compilation framework can be achieved in
different ways according to the degree of coding complexity of the Biogeochemical model, like e.g.,
the whole code is made only by one file or
it has multiple modules and interfaces spread across several subfolders.

Beside the 6 core files of MY_TRC module, let’s assume an external BGC model named *MYBGC* and
constituted by a rather essential coding structure, likely few Fortran files.
The new coupled configuration name is *NEMO_MYBGC*.

The best solution is to have all files (the modified ``MY_TRC`` routines and the BGC model ones)
placed in a unique folder with root ``MYBGCPATH`` and
to use the makenemo external readdressing of ``MY_SRC`` folder.

The coupled configuration listed in :file:`work_cfgs.txt` will look like

::

   NEMO_MYBGC OCE TOP

and the related ``cpp_MYBGC.fcm`` content will be

.. code-block:: perl

   bld::tool::fppkeys key_iomput key_mpp_mpi key_top

the compilation with :file:`makenemo` will be executed through the following syntax

.. code-block:: console

   $ makenemo -n 'NEMO_MYBGC' -m '<arch_my_machine>' -j 8 -e '<MYBGCPATH>'

The makenemo feature ``-e`` was introduced to
readdress at compilation time the standard MY_SRC folder (usually found in NEMO configurations) with
a user defined external one.

The compilation of more articulated BGC model code & infrastructure,
like in the case of BFM (|BFM man|_), requires some additional features.

As before, let’s assume a coupled configuration name *NEMO_MYBGC*,
but in this case MYBGC model root becomes :file:`MYBGC` path that
contains 4 different subfolders for biogeochemistry,
named :file:`initialization`, :file:`pelagic`, and :file:`benthic`,
and a separate one named :file:`nemo_coupling` including the modified `MY_SRC` routines.
The latter folder containing the modified NEMO coupling interface will be still linked using
the makenemo ``-e`` option.

In order to include the BGC model subfolders in the compilation of NEMO code,
it will be necessary to extend the configuration :file:`cpp_NEMO_MYBGC.fcm` file to include the specific paths of :file:`MYBGC` folders, as in the following example

.. code-block:: perl

   bld::tool::fppkeys  key_iomput key_mpp_mpi key_top

   src::MYBGC::initialization         <MYBGCPATH>/initialization
   src::MYBGC::pelagic                <MYBGCPATH>/pelagic
   src::MYBGC::benthic                <MYBGCPATH>/benthic

   bld::pp::MYBGC      1
   bld::tool::fppflags::MYBGC   %FPPFLAGS
   bld::tool::fppkeys           %bld::tool::fppkeys MYBGC_MACROS

where *MYBGC_MACROS* is the space delimited list of macros used in *MYBGC* model for
selecting/excluding specific parts of the code.
The BGC model code will be preprocessed in the configuration :file:`BLD` folder as for NEMO,
but with an independent path, like :file:`NEMO_MYBGC/BLD/MYBGC/<subforlders>`.

The compilation will be performed similarly to in the previous case with the following

.. code-block:: console

   $ makenemo -n 'NEMO_MYBGC' -m '<arch_my_machine>' -j 8 -e '<MYBGCPATH>/nemo_coupling'

.. note::
   The additional lines specific for the BGC model source and build paths can be written into
   a separate file, e.g. named :file:`MYBGC.fcm`,
   and then simply included in the :file:`cpp_NEMO_MYBGC.fcm` as follow

   .. code-block:: perl

      bld::tool::fppkeys  key_zdftke key_dynspg_ts key_iomput key_mpp_mpi key_top
      inc <MYBGCPATH>/MYBGC.fcm

   This will enable a more portable compilation structure for all MYBGC related configurations.

.. warning::
   The coupling interface contained in :file:`nemo_coupling` cannot be added using the FCM syntax,
   as the same files already exists in NEMO and they are overridden only with
   the readdressing of MY_SRC contents to avoid compilation conflicts due to duplicate routines.

All modifications illustrated above, can be easily implemented using shell or python scripting
to edit the NEMO configuration :file:`CPP.fcm` file and
to create the BGC model specific FCM compilation file with code paths.

.. |BFM man| replace:: BFM-NEMO coupling manual
