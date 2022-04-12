********************
eORCA025.L121-OPM027
********************

Summary
=======

Compare to eORCA025.L121-OPM026, we perturbed the atmospheric forcing by adding the HADCM3 A1B monthly 2160-2200 anomaly (wrt XXXX) to all the forcing variables.

Input files
===========
All the inputs files needed to run this configuration are here: https://ige-meom-opendap.univ-grenoble-alpes.fr/thredds/catalog/meomopendap/extract/eORCA025.L121/eORCA025.L121-OPM026/catalog.html?dataset=meomscanpublic/eORCA025.L121/eORCA025.L121-OPM026/eORCA025.L121-OPM026_input.tar

A detailed description of each file is given in :ref:`eORCA025_input_files`

Pertubed forcing set on demand.

Namelist
========

Except the sbcblk namelist block, no change compare to eORCA025.L121-OPM026.

Full files are available here: https://github.com/pmathiot/NEMOCFG/tree/main/cfgs/eORCA025.L121-OPM026/cfgs/eORCA025.L121-OPM026/EXPREF

namelist_oce
------------

- change atmospheric forcing file name:

.. code-block:: console


Input files
===========
 - Except the atmospheric forcings, no change in the input files.

Code changes
============
- No changes wrt OPM026.

Full files are available here: https://github.com/pmathiot/NEMOCFG/tree/main/cfgs/eORCA025.L121-OPM026/src/

Monitoring
==========

.. _eORCA025.L121-OPM027_monitoring:

Global indicators
-----------------
On these plot you can find a time series of:

- ACC transport
- AMOC at rapid array
- AMHT at rapid array
- Net global heat fluxes
- mean sst in the southern ocean (see box in the map)
- mean sst in the North West Corner (see box in the map)
- sea ice extent (arctic/ant in summer/winter)

.. image:: _static/VALGLO_PERT.png

Regional indicators
-------------------
On these plot, you can find time series of:

- ACC transport
- Maximum of the Weddell and Ross Gyre (box where the max compute show in the map)
- Mean bottom salinity over the main dense water formation hot spot (West Ross and West FRIS)
- Mean bottom temperature over East Ross and Amudsen sea to monitor CDW intrusion

.. image:: _static/VALSO_PERT.png

Local indicators
----------------
These plots monitor the evolution of ice shelf melting and the corresponding shelf properties (ROSS, FRIS, PINE, GETZ)

.. image:: _static/VALSI_PERT.png

Amundsen/Belingshausen seas
---------------------------
These plot monitoring the evolution of temperature, salinity and ice shelf melt in Amundsen sea.

.. image:: _static/VALAMU_PERT.png

Ice shelves
-----------

Amery
~~~~~
.. image:: _static/AMER_eORCA025.L121-OPM027.png

Ross
~~~~
.. image:: _static/ROSS_eORCA025.L121-OPM027.png

Getz
~~~~
.. image:: _static/GETZ_eORCA025.L121-OPM027.png

Pine island
~~~~~~~~~~~
.. image:: _static/PINE_eORCA025.L121-OPM027.png

George VI
~~~~~~~~~
.. image:: _static/GEVI_eORCA025.L121-OPM027.png

Filschner Ronne
~~~~~~~~~~~~~~~
.. image:: _static/FRIS_eORCA025.L121-OPM027.png

Riiser
~~~~~~
.. image:: _static/RIIS_eORCA025.L121-OPM027.png

Fimbul
~~~~~~
.. image:: _static/FIMB_eORCA025.L121-OPM027.png

Evaluation
==========

Ice shelf melt: the pictures below are the climatological melt for all the ice shelves in NEMO for the OPM027 (XXXX-XXXX) and OPM026 (2009-2018) simulations by sectors.

.. image:: _static/WAIS_OPM027_2009.png
   :scale: 27%

.. image:: _static/EAIS_OPM027_2009.png
   :scale: 27%

.. image:: _static/WEDD_OPM027_2009.png
   :scale: 27%

Map of ice shelf melt for both simulations OPM027 (XXXX-XXXX) and OPM026 (2009-2018) over the last decade of the simulations  with bottom temperature.

.. image:: _static/melt_sector_OPM026_2009-2018.png
   :scale: 20%

.. image:: _static/melt_sector_OPM021_2009-2018.png
   :scale: 20%

Map of barotropic stream function for both simulations OPM027 (XXXX-XXXX) and OPM026 (2009-2018).

.. image:: _static/BSF_PERT_HADCM3.png

Map of bottomT and bottomS for both simulations OPM027 (XXXX-XXXX) and OPM026 (2009-2018).

.. image:: _static/BOTS_PERT_HADCM3_comb.png

.. image:: _static/BOTT_PERT_HADCM3_comb.png
