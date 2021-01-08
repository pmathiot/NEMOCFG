**********
NEMO tools
**********

A complete list of the NEMO tools is available `here <(https://forge.ipsl.jussieu.fr/nemo/chrome/site/doc/NEMO/guide/html/tools.html#list-of-tools)>`_.

The main NEMO tools are the tools used to :
 - build the domain_cfg.nc NEMO start up file (`DOMAINcfg <https://forge.ipsl.jussieu.fr/nemo/chrome/site/doc/NEMO/guide/html/tools.html#domaincfg>`_)
 - build the weight file used for the online interpolation (`WEIGHTS <https://forge.ipsl.jussieu.fr/nemo/chrome/site/doc/NEMO/guide/html/tools.html#weights>`_)
 - rebuild the restart file and other split files (`REBUILD_NEMO <https://forge.ipsl.jussieu.fr/nemo/chrome/site/doc/NEMO/guide/html/tools.html#rebuild-nemo>`_)

The tools are included in the NEMO distribution.
So the best to download these tools, is to check out the NEMO version you plan to use and go in the tools directory

*************
DRAKKAR tools
*************

NEMOBAT
-------

.. _CDFTOOLS:

CDFTOOLS
--------
CDFTOOLS is a diagnostic package written in fortran 90 for the analysis of NEMO model output,
initialized in the frame of the DRAKKAR project (https://www.drakkar-ocean.eu/).

The version described here differs from the original package mostly in how the netcdf variable and dimension named are managed.
With this version, there is no need to recompile the code when you change your output dimensions/variables name
once it has beed added to the list of available names.

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/CDFTOOLS_4.0_ISF.git

* description and README available here: https://github.com/pmathiot/CDFTOOLS_4.0_ISF

SOSIE
-----
SOSIE is an interpolation package for NetCDF files (full description available here: https://brodeau.github.io/sosie/).
It is mainly used to pre-process file in order to start a NEMO simulation or to post-process output files.

* How to download it:

.. code-block:: console

  git clone https://github.com/brodeau/sosie.git

* description and README available here: https://github.com/brodeau/sosie

************
OTHERS tools
************

.. _VALSO:

VALSO
-----
VALSO is the toolbox used to produce the plots in the eORCA025.L121 OPM006 monitoring section (:ref:`here <eORCA025.L121-OPM006_monitoring>`).
It is based on the :ref:`CDFTOOLS <CDFTOOLS>` for the processing of the NEMO output files, python and image magic for plotting.

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/VALSO.git

* description and README available here: https://github.com/pmathiot/VALSO

.. _VALIS:

VALIS
-----
VALIS is a toolbox used to produced the plots in the eORCA025.L121 OPM006 ice shelf monitoring section (:ref:`here <eORCA025.L121-OPM006_isf_monitoring>`).
It is python based and as input file it used the filed processed by :ref:`VALSO <VALSO>`.

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/VALIS.git

* description and README available here: https://github.com/pmathiot/VALIS
