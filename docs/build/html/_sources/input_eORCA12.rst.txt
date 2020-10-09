**************************************
Input files for eORCA12 configurations
**************************************

.. contents::
   :local:

Coordinates
===========

.. _eORCA12_coord_c0.0:

eORCA12_coord_c0.0.nc
---------------------
File build at UKMO for GO6 (P. Mathiot) (~2015)

* original file name: eORCA12_coordinates.nc
* file name: eORCA12_coord_c0.0.nc
* motivation: The grid is extended beneath the ice shelf up to -86oS. To keep the time step constraint reasonable, we have to distort the grid.
* strategy: Buid 2 sector (Ross and Weddell) based on the transformation did in the north. Basically, the technique is: cut the north part you want from 22oN, rotate it, move to polar stereo domain, scale it to fit the line 64oS, paste it, come back to a lat/lon grid. 
* warning: You cannot simulate a total collapse of the WAIS because you will face a discontinuity line across ocean (on today Antarctic ice sheet state, it is fine).
* path to file: TODO
* path to directory: TODO

Bathymetry and ice shelf draft
==============================

.. _eORCA12_bathymetry_b2.0:

eORCA12_bathymetry_b2.0.nc
--------------------------

* sources:
   - Antarctic sub iceshelf geometry (ice shelf draft + bathymetry): Bedmap 2
   - Antarctic shelf: IBCSO data v1.0 + coastline maualy edited to fit the Antarctic Digital Database v6 (British Antarctic Survey)
   - Rest of the world: GEBCO 2014 revision 20150318 + manual editing point based on the record of the ORCA12 bathymetry + fill Bridlington Bay (UK) for stability in the UK climate model
* references:
   - Arndt, J.E., H. W. Schenke, M. Jakobsson, F. Nitsche, G. Buys, B. Goleby, M. Rebesco, F. Bohoyo, J.K. Hong, J. Black, R. Greku, G. Udintsev, F. Barrios, W. Reynoso-Peralta, T. Morishita, R. Wigley, "The International Bathymetric Chart of the Southern Ocean (IBCSO) Version 1.0 - A new bathymetric compilation covering circum-Antarctic waters", 2013, Geophysical Research Letters, Vol. 40, p. 3111-3117, doi: 10.1002/grl.50413
   - http://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_30_second_grid/
* comments: 
   - Bedmap2 (sub ice shelf bathymetry) is included in IBCSO data set but not in GEBCO 2014 (bathymetry beneath ice shelf is set to 0). So we merge IBCSO and GEBCO between line 1110 and 1130. 
   - Ice shelf draft is done separately with the same tool and we mask bathy and isf_draft based on the interpolation of ocean fraction cell available in Bedmap 2. 
   - Pas de lissage (il y a les pour et les contre. On a fait le choix de pas de lissage).
   - All intermediate file used to build the v2.0 are in the repository eORCA12_bathy/TMP_VERSION. 
   - This file was build in collaboration with Romain BB at MERCATOR-France.
* warning:
   - no great lakes, victoria, caspienne ... 
   - the extension _xios is to deal with XIOS and the land suppression

.. _eORCA12_bathymetry_b2.1:

eORCA12_bathymetry_b2.1.nc
--------------------------
* heritage: :ref:`eORCA12_bathymetry_b2.0`
* old file name: eORCA12_bathymetry_v2.0_lake_xios.nc
* source: 

   * file eORCA12_bathymetry_v2.0_xios.nc + lake data from file bathymetry_ORCA12_V3.3_with_grtlakes_caspian_lakevictoria.nc
   * lake data was from the Great Lakes Environmental Research Laboratory file: 15sglake-8152.xyz (http://www.glerl.noaa.gov/ftp/coastwatch/topo/gltopo.readme), for lake victoria: ETOPO2v2c_f4.nc, caspian sea from an earlier version of the ORCA12 bathy.

* path directory: TOADD
* for extra detail see the README in the building directory

.. _eORCA12_bathymetry_b2.4:

eORCA12_bathymetry_b2.4.nc
--------------------------
* old file name: eORCA12_bathymetry_v2.4.nc
* heritage : :ref:`eORCA12_bathymetry_b2.1`
* changes: 

    * filling single point bay around Antarctica to get rid of the ice pillar
    * filling these isolated points :  (j=2710,i=2677) (j=837,i=1066) (j=711,i=1150) (j=711,i=1151) (j=710,i=1151) (j=710,i=1150)


Domaincfg
=========

.. _eORCA12.L75_domain_cfg_b2.4_c0.0_v1.0:

eORCA12.L75_domain_cfg_b2.4_c0.0_v1.0.nc
----------------------------------------
* original file name: domaincfg_eORCA12_v1.0.nc
* file name: eORCA12.L75_domain_cfg_b2.4_c0.0_v1.0.nc
* coordinates.nc = eORCA12_coordinates.nc (or :ref:`eORCA12_coord_c0.0`)    (file used by the GO6 eORCA12 simulation, Met Office)
* bathy_meter.nc = eORCA12_bathymetry_v2.4.nc (or :ref:`eORCA12_bathymetry_b2.4`) (file used by the GO6 eORCA12 simulation, Met Office)
* z level: default 75 vertical levels
* mppmsk variable: mask to deals with closed cavities and XIOS (NEMO/branches/UKMO/dev_r10037_mppmask)
* path : TODO
* building directory path: TODO

Runoff
======

.. _eORCA12_runoff_b2.0_v1.0:

eORCA12_runoff_b2.0_v1.0.nc
---------------------------
* original file name : eORCA12_runoff_v1.0.nc
* file name : eORCA12_runoff_b2.0_v1.0.nc
* description: runoff file compatible with :ref:`eORCA12_bathymetry_b2.4`

* source: 

  * runoff  data: north of Drake passage comes from the runoff file runoff_obtaz_rhone_1m_ORCA12_20102008.nc. 
  * Iceberg data: comes from the file runoff_iceberg_eORCA025_v1.nc. The data have been interpolated on the eORCA12 grid, a treshold at 1e-9 has been applied, and finally I scale these data to the total calving provided by Rignot 2013 (1265 Gt/y)., this is an idealised iceberg distribution.
  * Ice shelf data: comes from Rignot 2013. The location the fwf input have been set infront of all the ice shelves represented in the eORCA12 bathymetry. The top and bottom depth used by the isf parametrisation has been determined by hand based on the typical bottom depth and ice shelf draft for each ice shelf. Total fw input is 1500 Gt/y without seasonal cycle

* Variables:

  * sorunoff : only runoff north of Drake passage (usefull if icb model and isf on)

    * socoefr : associated "mask" variable

  * sofwficb : iceberg only
  * srnficb    : sorunoff + icb contribution (usefull if isf on)
  * socoefricb : associated "mask" variable
  * sornfisficb   : sorunofficb + ice shelf contribution spread on 100 points in front of each ice shelf to avoid unrealistic small sss.
  * socoefrisficb : associated "mask" variable
  * sofwfisf : iceshelf only (nn_isf = 3)

    * zmin_isf : mean isf draft
    * zmax_isf : mean grounding line depth

* path: TODO

eORCA12_runoff_b2.4_v1.0.nc
---------------------------
* heritage: :ref:`eORCA12_runoff_b2.0_v1.0`
* compatibility: :ref:`eORCA12_bathymetry_b2.4`
* change: version adapted to bathymetry b2.4. ie runoff, icb, isf adapted to new coast line.
* path: TODO

Initial condition
=================

EN4.1.1_75L_monthly_19952014_reg1d_C_0.nc
-----------------------------------------
EN4.1 data set used for GO6 simulation in eORCA12. 

output files: 

* EN4.1.1_75L_monthly_19952014_reg1d_C_0.nc
* EN4.1.1_75L_monthly_19952014_reg1d_C_0_c0.0_weights_bilin.nc (consistent with coordinate file c0.0)

These files are the EN4 initial condition used in GO6 eORCA12. EN4.1.1_75L_monthly_19952014_reg1d_C_0.nc 
is interpolated on a 1x1 regular degree grid and eORCA12_EN4.1.1f_reg_weights_bilin.nc 
are the weight file used by NEMO to interpolate EN4 onto eORCA12 on the fly

* source: EN.4.1.1.f.analysis.g10.1995-2014.L75.nc
* modiled in : EN4.1.1_75L_monthly_19952014_reg1d_C_0.nc (regular 1degree and on Celcuis)
* reference: Good, S. A., M. J. Martin and N. A. Rayner, 2013. EN4: quality controlled ocean temperature and salinity profiles and monthly objective analyses with uncertainty estimates, Journal of Geophysical Research: Oceans, 118, 6704-6716, doi:10.1002/2013JC009067
* comments: 

   * as for EN3, the initial condition was not interpolate onto the eORCA12 grid. As I have issue with the raw file with NEMO, I interpolated on a 1degree regular grid -90:90oN with SOSIE and used this file as input file for NEMO and I compute the weight related to this file. 
   * I drown the land so it should be OK for the lake. However, I am not sure the data are suitable for lakes 
   * EN.4.1.1.f.analysis.g10.1995-2014.L75.nc has been made by Dave Storkey (Met Office)
   * The file is already interpolated on classical L75 levels. If new vertical levels distribution, you need to start from the original EN4 input files (ask Met Office, freely available on the web I think)

eORCA12.L75_EN4.2.1g10_1995-2014*.nc
------------------------------------
* source: EN4.2

Iceberg calving
===============

eORCA12_calving_b2.4_v0.0.nc
----------------------------
File build for GO6 eORCA12 at UKMO

* original name: eORCA12_calving.nc
* file name: eORCA12_calving_b2.4_v0.0.nc (ie compatible with the b2.4 eORCA12 bathymetry)
* Source: Marsh, R., Ivchenko, V. O., Skliris, N., Alderson, S., Bigg, G. R., Madec, G., Blaker, A. T., Aksenov, Y., Sinha, B., Coward, A. C., Le Sommer, J., Merino, N., and Zalesny, V. B.: NEMO?~@~SICB (v1.0): interactive icebergs in the NEMO ocean model globally configured at eddy-permitting resolution, Geosci. Model Dev., 8, 1547-1562, doi:10.5194/gmd-8-1547-2015, 2015.
* comments: I remapped the file eORCA025_calving_v2.2.nc on the eORCA12 grid based on the correspondence of T point between the 2 grids. 2 points are moved by hand because on land. No tuning on the location are done. It seems position in the NH hemisphere are quit approximative (even in the 025 file). Some work could also be done on the calving location along the big ice shelf. Maybe for GO7.
* link to file: TODO
* link to building directory: TODO

eORCA12_calving_b2.4_v2.0.nc
----------------------------
File build for GO8 eORCA12 at UKMO and 2020 eORCA12 simulation at MEOM. The idea behind this file is a random distributioon of calving along the ice shelf front (ie random pattern). The total per ice shelf is scaled to the Rignot et al. (2013) estimates. It is very different to the v0.0. in the v0.0, each ice shelf have only one or two iceberg calving site. 

* original name: eORCA12_calving_v2.4.nc
* file name: eORCA12_calving_b2.4_v2.0.nc
* compatibility: :ref:`eORCA12_bathymetry_b2.4`
* calving rate per ice shelf are from Rignot et al. (2013)
* Details on the construction available here [TODO : set link to the directory and README]
* link to file: TODO
* link to building directory: TODO

Geothermal heating
==================

ghflux_v2.0
-----------
Goutorbe geothermal heat flux with online interpolation

* old name: Goutorbe_ghflux.nc
* file: ghflux_v2.0.nc
* weights availables:

  * eORCA12_ghflux_v2.0_c0.0_weights_bilin.nc (old name eORCA12_Goutorbe_weights_bilin.nc): weight compatible wit :ref:`eORCA12_coord_c0.0` or :ref:`eORCA12.L75_domain_cfg_b2.4_c0.0_v1.0`

* path: TOADD

2d bottom friction
==================

.. _eORCA12_bfr2d_v1.0:

eORCA12_bfr2d_v1.0
------------------
* file name: eORCA12_bfr2d_v1.0.nc
* old name: eORCA12_bfr2d_UKmod.nc
* source file: bfr_coef_ORCA12_new.nc (who build it ????) changed in eORCA12_bfr2d.nc (UKMO)
* comments: I add line of 0 at the bottom part of the ORCA12 file, and add enhance bottom friction. Enhance bottom friction has been added in Bridlington Bay to help with instabilities. It appears the source of the probleme was interpolation of tmx input files. 

2d slip condition
=================

eORCA12_shlat2d_v1.0
--------------------
* file name: eORCA12_shlat2d_v1.0.nc
* old name: eORCA12_shlat2d_UK_Ant_Spain_mod.nc
* source file: shlat2d_ORCA12grid_fev09.nc (who build it ? acc ?) rename in eORCA12_shlat2d.nc
* comments: I add line of 0 at the bottom part of the ORCA12 file. No slip condition has been added along Antarctica to help with instabilities in GC3, in Bridlington Bay and in the south of Spain (see comments on :ref:`eORCA12_bfr2d_v1.0`).

Indonesian Through Flow
=======================

eORCA12_mskitf_v1.0
-------------------
* old name: eORCA12_msk_itf.nc
* file name: eORCA12_mskitf_v1.0.nc
* source file: mask_itf_ORCA025ext.nc (who build it ???)
* comments: mask_itf_ORCA12_new.nc is bugged. I ask if acc, jmm, rbb have an other one without bug, no reply. I build it from eORCA025 based on the corresponding cells between eORCA025 and eORCA12.

Distance to coast
=================

eORCA12_distcoast_v0.0
----------------------
* old name: eORCA12.L75_distcoast.nc
* file name: eORCA12_distcoast_v0.0.nc
* path to directory: TOADD
* path to file: TOADD
