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

For all these tools, see the README available in the git directory. If there is no README ask the main developer.

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

PYCHART
-------

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/PyChart.git

* Example of a command (the one used to produce the figure :numref:`bsf_OPM006`):

.. code-block:: console

  python ~/GIT/PyChart/plot_maps.py \
   --ft 'BSF (OPM006)' \
   --spfid '1979-1988'                            '1989-1998'                           '1999-2008'                            '2009-2018'                         \
   --mapf eORCA025.L121-OPM006_10y_y1979_psi.nc eORCA025.L121-OPM006_10y_y1989_psi.nc eORCA025.L121-OPM006_10y_y1999_psi.nc eORCA025.L121-OPM006_10y_y2009_psi.nc  \
   --cntf eORCA025.L121-OPM006_10y_y1979_psi.nc eORCA025.L121-OPM006_10y_y1989_psi.nc eORCA025.L121-OPM006_10y_y1999_psi.nc eORCA025.L121-OPM006_10y_y2009_psi.nc  \
   --mapv sobarstf \
   --cntv sobarstf \
   --cblvl  0 100 10 \
   --cntlvl 0 100 10 \
   --cbu Sv \
   --cbn magma_r \
   --mapsf 0.000001 \
   --cntsf 0.000001 \
   --cbext both \
   --mesh   mesh.nc     \
   --bathyf mesh.nc     \
   --bathyv bathy_metry \
   --bathylvl 1000 2000 3000 4000 \
   -p south_ocean \
   --sp 2x2 \
   -o bsf_OPM006

--\*v, --\*v, --mesh, --bathy? can allow 1 or nfile arguments
-p is the projection among a predefined list, --sp the disposition you want (can be 1x3 even if you ask only 2 plots)
-o the output

PYTIME
------
PyTimes is a tools to build easily multiple time series from a bench of simulations. It manages list of input files with multiple
 time frame or multiple files of 1 time frame. This is one of the core tools for VALSO.

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/PyTimes.git

PYTOOLS
-------
PyTools is a list of tools used when NEMO is not cooperative:

- filechek.py check if 2 netcdf files are identical. It checks data, attributes (...).
- get_value.py display value of a list of variable value at a specific points.
- get_klevel.py find the closest level of a specific depth.

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/PyTools.git

BUILD_CLIMATO
-------------
BUILD_CLIMATO is tools to build multi year mean from NEMO output.
To have support of the thickness weighting, variable attributes need to specify a correct cell_methods: "time: mean (thickness weighted)".
This can be set up before the start of the run in XIOS by using cell_methods="time: mean (thickness weighted)" in XIOS variable definition.

* How to download it:

.. code-block:: console

  git clone https://github.com/pmathiot/BUILD_CLIMATO.git
