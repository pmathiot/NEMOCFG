***************************************
Input files for eORCA025 configurations
***************************************

.. contents::
   :local:

Coordinates
===========

.. _eORCA025_coord_c3.0:

eORCA025_coord_c3.0.nc
----------------------

* source:
   - file build at Mercator (Clement Bricaud):
* comments:
   - Bug in the north corrected.
   - add proper extension under masked area
* original name:
   - eORCA_R025_coordinates_v3.0.nc
* reference for the south extension:
   - Mathiot, P., Jenkins, A., Harris, C., and Madec, G.: Explicit representation and parametrised impacts of under ice shelf seas in the z∗ coordinate ocean model NEMO 3.6, Geosci. Model Dev., 10, 2849–2874, https://doi.org/10.5194/gmd-10-2849-2017, 2017.

Bathymetry and ice shelf draft
==============================

.. _eORCA025_bathymetry_b0.0:

eORCA025_bathymetry_b0.0.nc
---------------------------
* original name:
   - bathy_noisf_ORCA025_IBCSO_ETOPO1_mask75L_v2.2_landsuppression.nc
* Source:
   - File used in GO6 (cite GO6 paper)
* references:
   - Storkey, D., Blaker, A. T., Mathiot, P., Megann, A., Aksenov, Y., Blockley, E. W., Calvert, D., Graham, T., Hewitt, H. T., Hyder, P., Kuhlbrodt, T., Rae, J. G. L., and Sinha, B.: UK Global Ocean GO6 and GO7: a traceable hierarchy of model resolutions, Geosci. Model Dev., 11, 3187–3213, https://doi.org/10.5194/gmd-11-3187-2018, 2018.
   - Mathiot, P., Jenkins, A., Harris, C., and Madec, G.: Explicit representation and parametrised im    pacts of under ice shelf seas in the z∗ coordinate ocean model NEMO 3.6, Geosci. Model Dev., 10,     2849–2874, https://doi.org/10.5194/gmd-10-2849-2017, 2017.
* comments:
   - Method for the southern extention described in Mathiot et al., 2017

.. _eORCA025_bathymetry_b0.1:

eORCA025_bathymetry_b0.1.nc
---------------------------

* original name:
   - eORCA025_bathymetry_isf_v2.3.nc
* Source :
   - File used in GO7 (cite GO6 paper)
* references:
   - Storkey, D., Blaker, A. T., Mathiot, P., Megann, A., Aksenov, Y., Blockley, E. W., Calvert, D., Graham, T., Hewitt, H. T., Hyder, P., Kuhlbrodt, T., Rae, J. G. L., and Sinha, B.: UK Global Ocean GO6 and GO7: a traceable hierarchy of model resolutions, Geosci. Model Dev., 11, 3187–3213, https://doi.org/10.5194/gmd-11-3187-2018, 2018.
   - Mathiot, P., Jenkins, A., Harris, C., and Madec, G.: Explicit representation and parametrised im    pacts of under ice shelf seas in the z∗ coordinate ocean model NEMO 3.6, Geosci. Model Dev., 10,     2849–2874, https://doi.org/10.5194/gmd-10-2849-2017, 2017.
* comments:
   - as b0.0 but with under ice shelf data (isf draft and bathy) from bedmap2.
   - Method for the southern extention described in Mathiot et al., 2017

.. _eORCA025_bathymetry_b0.2:

eORCA025_bathymetry_b0.2.nc
---------------------------
* heritage:
   - :ref:`eORCA025_bathymetry_b0.1`
* source:
   - Bathymetry north of Antarctic continental slope: :ref:`eORCA025_bathymetry_b0.1`
   - Bathymetry south of Antarctic continental slope: Bed Machine (BedMachineAntarctica-2020-07-15.nc). Version available on the server is bugged (issue with reference for the IBCSO part).
   - ice shelf draft is from BedMachineAntarctica-2020-07-15.nc
   - path to raw data: TOADD
* reference:
   - Morlighem, M., E. Rignot, T. Binder, D. D. Blankenship, R. Drews, G. Eagles, O. Eisen, F. Ferraccioli, R. Forsberg, P. Fretwell, V. Goel, J. S. Greenbaum, H. Gudmundsson, J. Guo, V. Helm, C. Hofstede, I. Howat, A. Humbert, W. Jokat, N. B. Karlsson, W. Lee, K. Matsuoka, R. Millan, J. Mouginot, J. Paden, F. Pattyn, J. L. Roberts, S. Rosier, A. Ruppel, H. Seroussi, E. C. Smith, D. Steinhage, B. Sun, M. R. van den Broeke, T. van Ommen, M. van Wessem, and D. A. Young. 2020. Deep glacial troughs and stabilizing ridges unveiled beneath the margins of the Antarctic ice sheet, Nature Geoscience. 13. 132-137. https://doi.org/10.1038/s41561-019-0510-8
* method:
   - step 1: create the lat/lon variable associated to the netcdf file (BedMachine2NEMOBAT.py)
   - step 2: run interpolation for bathymetry, isfdraft, mask
   - step 3: smooth as eORCA025 bathymetry
   - step 4: enforce compatibility and build mask before combining data set
   - step 5: combine with reference eORCA025 bathymetry (b0.0) files
* tools:
   - NEMOBAT: https://github.com/pmathiot/NEMOBAT (ce3db4f..78d1867)
* path to building directory:
   - TOADD

Domaincfg
=========

.. _eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0:

eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0.nc
------------------------------------------

* source:
   - bathymetry: :ref:`eORCA025_bathymetry_b0.2`
   - horiz. coordinates: :ref:`eORCA025_coord_c3.0`
   - vert. coordinates: z + 121 levels selected to have rougly constant resolution between 100m and 1000m (ie range of depth spanning by the isf cavities.
* comments:
   - full namelist available in the namelist_cfg variable in the netcdf file.
   - closed seas removed
* tools:
   - r13390 of NEMO DOMAIN_cfg tools
* paths:
   - building directory: TOADD
   - file: TOADD

Runoff
======

First_runoff_file:
------------------

* source:
   - The runoffs data file comes from the Dai and Trenberth study . It is documented in a publication (Dai and Trenberth, 2002, hereafter DT02). The spatial resolution of this file is 1◦ ×1◦ with monthly data. This data are available on the web : http ://www.cgd.ucar.edu/cas/catalog/dai/
* reference:
   - https://www.drakkar-ocean.eu/publications/reports/runoff-mercator-06.pdf

.. _eORCA025_runoff_b0.2_v0.0:

eORCA025_runoff_b0.2_v0.0.nc
----------------------------

* sources:
   - The runoffs data file comes from the Dai and Trenberth study . It is documented in a publication (Dai and Trenberth, 2002, hereafter DT02). The spatial resolution of this file is 1◦ ×1◦ with monthly data. This data are available on the web : http ://www.cgd.ucar.edu/cas/catalog/dai/
* original name:
   - ORCA025_rnficbisf_rignot2013_noberg.nc
* compatibility:
   - bathymetry: :ref:`eORCA025_bathymetry_b0.2`
* Comments:
   - original file from the Met Office. As the coastline north of Antactica did not change, the GO6 runoff is adapted to the bathymetry 0.3.
   - Antarctic runoff has been removed in Mathiot et al. (2017).
   - GO6 runoff variable name is confusing. This is: sornficb (even if no icb are present in it.
   - we rename runoff variable name to sorunoff.
   - we extract only the variable related to runoff (sorunoff,socoefr,nav_lat,nav_lon)

Initial condition
=================

.. _eORCA025.L121_WOA2018_b0.2_c3.0_d1.0_v19812010.4:

eORCA025.L121_WOA2018_b0.2_c3.0_d1.0_v19812010.4.nc
---------------------------------------------------
* source:
   - Open ocean: WOA2018 on period 1981-2010 (see :ref:WOA2018_v19812010.1 for details)
   - Under isf: UKMO eORCA025 JRA simulation bt705 over period (1995-2005)
* methode:
   - WOA are drowned and interpolated to :ref:`eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0.nc`
   - WOA is only provided for each season, so we build the monthly 3D file with a linear interpolation
   - WOA is only provided monthly data between surface and 1450m depth, we used this data for first 1400m
     (ie level 89, last level fully included in the monthly data).
   A linear transition from one to the other data set is done between 1200m and depth of level 90.
   - UKMO data are downloaded monthly and a climatology is built
   - Merge between the 2 data set is done in the open ocean. UKMO data are kept until 10km offshore the Antarctic ice shelves. WOA is kept for cell further away than 70 km offshore the Antarctic ice shelves. A linear transition in the transition area is applied.
   - A non penetrative convection scheme is applied to avoid static instability
   - A shapiro filter is applied to remove strong gradient (3 passes)
* compatibility:
   - eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0.nc as file is masked.
* path:
   - directory: TOADD
   - file: TOADD

.. _eORCA025.L121_WOA2018_b0.2_c3.0_d1.0_v19812010.5:

eORCA025.L121_WOA2018_b0.2_c3.0_d1.0_v19812010.5.nc
---------------------------------------------------
* source:
  - Open ocean: WOA2018 on period 1981-2010 (see :ref:WOA2018_v19812010.1 for details)
  - Under isf: UKMO eORCA025 JRA simulation bt705 over period (1995-2005)
* methode:
  - as :ref:`eORCA025.L121_WOA2018_b0.2_c3.0_d1.0_v19812010.4`
  - convert to TEOS10 using GSW-Fortran-3.05-6 package (see TEOS10 directory in building directory)
  - then check and correct overlap cells with check_lbclnk_v3.py
* compatibility:
  - eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0.nc as file is masked.
* path:
  - directory: TOADD
  - file: TOADD

Restoring
=========

sss_WOA2018r04_v19812010.2.nc
-----------------------------
* source:
    - WOA2018 on period 1981-2010 (see :ref:WOA2018_v19812010.1 for details)
* methode:
    - monthly WOA surface data are drowned.
    - original file is compressed to level 1 and we used a smaller chunk size.
* variable:
    - s_an
* frequency:
    - monthly
* weights:
    - eORCA025_sss_WOA2018r04_v19812010_c3.0_weights_bilin.nc
      (compatible with :ref:`eORCA025_coord_c3.0` or :ref:`eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0`)

Iceberg calving
===============

.. _eORCA025_calving_b0.2_v2.3:

eORCA025_calving_b0.2_v2.3.nc
-----------------------------
the amount of calving per ice shelves comes from Rignot et al. (2013). The distribution along the ice shelf front is random (the idea is as I don't know where are the calving site and the amount of calving per site, I draw a random distribution of calving scale to the total amount provided by the climatology).

* sources:
   - north: as in March et al. (2015).
   - south: Rignot et al. (2013).
* useful tools:
   - git project: https://github.com/pmathiot/CDFTOOLS_4.0_ISF
   - tag: v3.0.2-330-g40595ba
   - path on dahu: /home/mathiotp/TOOLS/CDFTOOLS/20200823_40595ba/src
   - script: cdfisf_fill, cdficb_clv
* compatibility:
   - bathymetry: :ref:`eORCA025_bathymetry_b0.2`
* comments:
   - in this version only 1 time frame is provided, we can easily extend the logic to interannual calving or monthly calving by drawing X different state.
* variable:
   - soicbclv
* frequency:
   - annual
* path to building directory:
   - TOADD
* path to file:
   - TOADD

Geothermal heating
==================

.. _ghflux_v2.0:

ghflux_v2.0.nc
--------------
Goutorbe geothermal heat flux with online interpolation

* old name:
   - Goutorbe_ghflux.nc
* weights availables:
   - eORCA025_ghflux_v2.0_c3.0_weights_bilin.nc: weight compatible with :ref:`eORCA025_coord_c3.0` or :ref:`eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0`
* path:
   - TOADD

Top tidal velocity
==================

As shown in Jourdain et al. (2018): including tidal velocities into the equation of the turbulent heat flux is a good approach to account for tide-induced melting in ocean models that do not explicitly represent tides. It is nonetheless important to keep the horizontal patterns of tidal velocities, and prescribing uniform tidal velocities leads to large errors.

.. _eORCA025_ttv_b0.2_v0.0:

eORCA025_ttv_b0.2_v0.0.nc
-------------------------

* source:
   - CATS2008: CATS2008 is a regional inverse barotropic tide model for the circum-Antarctic ocean on a 4 km grid. The model domain includes ocean cavities under the floating ice shelves. The coastline is based on the MODIS MOA [Scambos et al., 2007; Remote Sensing of Environment] feature identification files, adjusted to match ICESat-derived grounding lines for the Ross and Filchner-Ronne ice shelves and Interferometric Synthetic Aperture Radar (InSAR) grounding lines. The water depth map for open water is based on the 2007 release update to Smith and Sandwell [1997; Science]. Adjustments to this map have been made in various regions, including the open continental shelf in front of the Larsen-C Ice Shelf which has been blended with GEBCO bathymetry.
   - Data downloaded: 12/08/2020 from here: https://www.usap-dc.org/view/dataset/601235
* method:
   - see Jourdain et al. (2018) on how to compute the mean tidal velocity for detailed. Here we used the first 6 component : m2 s2 n2 k1 q1 o1 (the one provided by CATS). The average is done over 190d with a sampling of 15 minutes. The mean velocity is computed from the tidal transport using the CATS water column thickness. The data a drown then interpolated on NEMO grid then drowned again. At the end, we masked it for visualisation and for the simulation. In case other bathymetry/grounding line used, you can simply redo the step 2 to 4 described below as ttv.nc is a drowned file.
* compatibility:
   - :ref:`eORCA025.L121_domain_cfg_b0.2_c3.0_d1.0`
* path to data and building directory
   - TOADD
   - TOADD

Internal wave mixing
====================

.. _eORCA025_iwm_b0.2_v0.0:

eORCA025_iwm_b0.2_v0.0.nc
-------------------------
The mixing variables correspond to the column-integrated power available for mixing. Each goes with a different vertical structure of the dissipation.

   - 'cri': exponential decay from the seafloor, with a spatially variable e-folding length given by 'decay_scale_cri.nc'.
   - 'bot': exponential decay from the seafloor in WKB z-coordinate (a z-coordinate that depends on stratification), with a spatially variable e-folding length given by 'decay_scale_bot.nc'.
   - 'pyc': pycnocline-intensified dissipation, proportional to N.

These variable replace the older input files for the tidal mixing parameterization.

* source:
   - see De Lavergne et al. (2016) and details in :ref:`De_Lavergne_et_al_2016`.
* methode:
   - Interpolation from the regular 0.5 degree resolution dataset to eORCA025 grid.
   - from email discussion with Casimir, I decided to fill land and isf cavities to the backgrou    nd value instead of drowning data.
* reference:
   - de Lavergne, C., G. Madec, J. L. Sommer, A. J. G. Nurser, and A. C. N. Garabato, 2016: The impact of a variable mixing efficiency on the abyssal overturning. Journal of Physical Oceanography, 46, 663?~@~S681
* path:
   - building directory: TOADD
   - file: TOADD

Chlorophyle
===========

.. _chlorophyl_v0.0:

chlorophyl_v0.0.nc
------------------

* source:
   - merge between ESACCI and biomer (CMEMS reanalysis)
* comments:
   - file created by Romain Bourdalie Badie (Mercator)
   - file use as it is with on the fly interpolation
* compatibility of weight files:
   - :ref:`eORCA025_coord_c3.0`
* weights: eORCA025_chlorophyl_v0.0_c3.0_weights_bilin.nc
   - computed using /home/mathiotp/TOOLS/NEMO/WEIGHTS/r13204/ and mkweights in ../TOOLS/WEIGHTS/
   - namelist used: eORCA025_chlorophyl_v0.0_c3.0_namelist_bilin
   - need to increase the stack on dahu (ulimit -s unlimited)
* path:
   - building directory: TOADD
   - files: TOADD

2d lateral slip conditions
==========================

.. _eORCA025_shlat2d_v0.0:

eORCA025_shlat2d_v0.0.nc
------------------------
* Purpose:
    - no slip condition along West greenland (generation of EKE close to cape desolation)
    - no slip condition in Med. sea (Bernard Barnier and Balear university experts)
    - no slip condition in Bering strait to decrease the transport.

* source:
    - DRAKKAR ORCA025 GRD100 simulation (file has simply been extended south)
* comments:
    - from GRD100 report, it seems the fix for greenland has only a marginal impact.
* path:
    - file: TOADD

2d bottom friction
==================

.. _eORCA025_bfr2d_v0.0:

eORCA025_bfr2d_v0.0.nc
----------------------
Increase bottom friction in Torres strait, Denmark strait and Bab el Mandel strait.

* original name:
    - bfr_coef.nc
* history:
    - file used in GO6 (UKMO)
    - full detail unknown (who build it, for what ...)
* path:
    - file: TOADD

Indonesian Through Flow
=======================

.. _eORCA025_mskitf_v1.0:

eORCA025_mskitf_v1.0.nc
-----------------------
* original name:
    - mask_itf_ORCA025ext.nc
* comments:
    - full history unknown
    - this file is simply an extension for the eORCA025 grid from an older file
* path:
    - file: TOADD

Distance to coast
=================

.. _eORCA025_distcoast_b0.2_v0.0:

eORCA025_distcoast_b0.2_v0.0.nc
-------------------------------
* tools:
   - cdfcofdis (./cdfcofdis -H eORCA025.L121_mesh_mask_b0.2_c3.0_d1.0.nc -M eORCA025.L121_mesh_mask_b0.2_c3.0_d1.0.nc -T eORCA025.L121_mesh_mask_b0.2_c3.0_d1.0.nc -jperio 4 -surf -noisland 50)
   - github.com:pmathiot/CDFTOOLS_4.0_ISF.git (master @ 14ab158..67ee63f)
* comments:
   - To switch off restoring along the the restoring near the coastal boundaries, in order to let the dynamics build the coherent water masses. We removed all the islands bigger than 50 cells.
