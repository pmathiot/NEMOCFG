*****************
Original data set
*****************

Bathymetry
==========

.. _Bedmachine:

Bedmachine v2.0
---------------
* file:
   - BedMachineAntarctica-2020-07-15.nc
* path:
   - TOADD
* Reference:
   - Morlighem, M., E. Rignot, T. Binder, D. D. Blankenship, R. Drews, G. Eagles, O. Eisen, F. Ferraccioli, R. Forsberg, P. Fretwell, V. Goel, J. S. Greenbaum, H. Gudmundsson, J. Guo, V. Helm, C. Hofstede, I. Howat, A. Humbert, W. Jokat, N. B. Karlsson, W. Lee, K. Matsuoka, R. Millan, J. Mouginot, J. Paden, F. Pattyn, J. L. Roberts, S. Rosier, A. Ruppel, H. Seroussi, E. C. Smith, D. Steinhage, B. Sun, M. R. van den Broeke, T. van Ommen, M. van Wessem, and D. A. Young. 2020. Deep glacial troughs and stabilizing ridges unveiled beneath the margins of the Antarctic ice sheet, Nature Geoscience. 13. 132-137. https://doi.org/10.1038/s41561-019-0510-8
* guide:
   - https://nsidc.org/data/nsidc-0756
* access:
   - file provided kindly by M. Morlighem as the available v1 is bugged (reference used for IBCSO data wrong). The file should also be available on nsidc server.

Tidal velocity
==============

.. _CATS2008:

CATS2008
--------
* files:
   - grid_CATS2008: CATS grid data
   - hf.CATS2008.out: CATS ssh data
   - uv.CATS2008.out: CATS tidal transport
* details:
   - Data downloaded:
      + 12/08/2020 from here: https://www.usap-dc.org/view/dataset/601235
   - abstract:
      + CATS2008 is a regional inverse barotropic tide model for the circum-Antarctic ocean on a 4 km grid. The model domain includes ocean cavities under the floating ice shelves. The coastline is based on the MODIS MOA [Scambos et al., 2007; Remote Sensing of Environment] feature identification files, adjusted to match ICESat-derived grounding lines for the Ross and Filchner-Ronne ice shelves and Interferometric Synthetic Aperture Radar (InSAR) grounding lines. The water depth map for open water is based on the 2007 release update to Smith and Sandwell [1997; Science].  Adjustments to this map have been made in various regions, including the open continental shelf in front of the Larsen-C Ice Shelf which has been blended with GEBCO bathymetry.
   - Model type:
      + Tides only; Inverse (data assimilation); barotropic (no vertical variation of currents)
   - Grid:
      + 4-km uniform polar stereographic (centered at 71 degrees S, 70 degrees W)
   - Constituents:
      + M2, S2, N2, K2, K1, O1, P1, Q1, Mf, Mm
   - Units:
      + z (sea surface height; meters); u,v (currents; cm/s); U,V (transports; m2/s)
   - Acquisition procedures:
      + We first develop an optimized forward model used direct factorization of the linearized shallow water equations, forced at the open boundary by tide heights from the global inverse model TPXO7.1 (https://www.tpxo.net/) and by the astronomical potentials. The model is tuned to data by the use of a linear benthic drag coefficient rather than the more usual quadratic drag formulation. Each of the ten primary tidal harmonics is tuned separately. While the optimized forward model is not as accurate as could be obtained with more sophisticated tidal physics, it creates a relatively accurate “prior” model for use in the next step of data assimilation.
      + The second step is to assimilate available data sets. We assimilate (i) TOPEX/Poseidon radar altimetry from the open ocean when no sea ice is present, (ii) a set of ~50 “high quality” tide records (including bottom pressure recorders, coastal tide gauges, and a few long-duration GPS records on ice shelves), and (iii) ICESat laser altimetry data at crossovers on the Ross and Filchner-Ronne ice shelves (see Padman and Fricker [2005; Geophysical Research Letters] and Padman et al. [2008; Geophysical Research Letters]). The set of assimilated tide records is drawn from the larger set described at our Antarctic Tide Gauge Database web page (https://www.esr.org/data-products/antarctic_tg_database/).
   - Limitations and issues:
      1) This model provides ocean tide only; i.e., sea surface height change relative to the seabed. Some applications will require adjustment for seabed deformation (“ocean tide loading”). Users require a separate ocean load tide model for this calculation.
      2) Grounding lines of ice shelves can migrate with ice loss, and with tidal and non-tidal fluctuations in mean sea level. The model assumes a fixed grounding line position.
      3) Under ice shelves, the model “water depth” is replaced by “water column thickness” (wct), which is the distance from the seabed to the ice-shelf base.
      4) For ice-shelf areas, modeled height fields are valid only where the ice can be assumed to be hydrostatic; close to the grounding line, ice-shelf flexure reduces the tidal height amplitude. See Padman et al. [2018; Reviews of Geophysics].
      5) Bathymetry in some areas is poorly constrained by data. For currents, we recommend that the user calculates depth-integrated volume transport, then divides by the latest depth data to get depth-averaged currents.
* references:
   - Howard, S. L. et al. (2019) "CATS2008: Circum-Antarctic Tidal Simulation version 2008" U.S. Antarctic Program (USAP) Data Center. doi: https://doi.org/10.15784/601235.
   - L., H. A. Fricker, R. Coleman, S. Howard, and S. Erofeeva (2002), A new tidal model for the Antarctic ice shelves and seas, Ann. Glaciol., 34, 247-254. (doi:10.3189/172756402781817752)
   - Padman, L., L. Erofeeva, and H. A. Fricker (2008), Improving Antarctic tide models by assimilation of ICESat laser altimetry over ice shelves, Geophys. Res. Lett., 35, L22504. (doi:10.1029/2008GL035592)
* path:
   - TOADD

Chlorophyl
==========

ESACCI_BIOMER4V1R1
------------------

* file:
   - merged_ESACCI_BIOMER4V1R1_CHL_REG05.nc
* comments:
   - Mercator compile chlorophyl data from ESACCI data set (climatology on 1998-2011 period) and where no data available, the gaps were filled by BIOMER CMEMS reanalysis.
   - dataset provided on a regular 0.5 degree resolution grid.

T/S
===

.. _WOA2018_v19812010.1:

WOA2018_v19812010.1
-------------------
* source:
   - WOA2018 at resolution 1/4 degreee over period 1981-2010
* path to original data:
   - https://www.nodc.noaa.gov/cgi-bin/OC5/woa18/woa18.pl (last access 2020/08/27)
* user guide:
   - https://data.nodc.noaa.gov/woa/WOA18/DOC/woa18documentation.pdf
* reference:
   - for temperature: Locarnini, R. A., A. V. Mishonov, O. K. Baranova, T. P. Boyer, M. M. Zweng, H. E. Garcia, J. R. Reagan, D. Seidov, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 1: Temperature. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 81, 52 pp.
   - for salinity: Zweng, M. M., J. R. Reagan, D. Seidov, T. P. Boyer, R. A. Locarnini, H. E. Garcia, A. V. Mishonov, O. K. Baranova, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 2: Salinity. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 82, 50 pp.
* seasonal 3d file:
   - in RAW to original file from WOA
   - seasonal files has been concatanated, drown (v19812010.1) and compressed and chunk correctly
   - drown is done with SOSIE3 with the gaussian filter (need change in makefile to activate it)
* monthly sss file:
   - for surface data monthly data were used.
   - data drowned with with SOSIE3 with the gaussian filter (need change in makefile to activate it)
* paths:
   - directory: TOADD
   - files: TOADD

Internal Wave Mixing
====================

.. _De_Lavergne_et_al_2016:

De_Lavergne_2016
----------------
* file:
   - Fichiers_inputs_NEWTMX.zip
* Provider:
   - Casimir Delavergne
* Details:
   - read the pdf available
* data file:
   - new_tmx_forcing_fields.nc
* reference:
   - de Lavergne, C., G. Madec, J. Le Sommer, A. J. G. Nurser, and A. C. Naveira Garabato, 2016: The Impact of a Variable Mixing Efficiency on the Abyssal Overturning. J. Phys. Oceanogr., 46, 663–681, https://doi.org/10.1175/JPO-D-14-0259.1.
* paths:
   - directory: TOADD
   - file: TOADD

Geothermal heat flux
====================

Goutorbe_2010
-------------

* resolution :
   - regular 1 degree
* data set:
   - see suplementary material in the reference
   - netcdf provided via shaconemo project
* reference:
   - Goutorbe, B., Poort, J., Lucazeau, F. and Raillard, S. (2011), Global heat flow trends resolved from multiple geological and geophysical proxies. Geophysical Journal International, 187: 1405-1419. doi:10.1111/j.1365-246X.2011.05228.x
* path:
   - directory: TOADD
   - file: TOADD


Atmospheric forcing
===================

.. _JRA55_v1.4:

JRA v1.4.0
----------
* details:
   - JRA55-do (Tsujino et al., 2018) is a surface dataset for driving ocean-sea ice models and used in phase 2 of OMIP (OMIP-2). JRA55-do corrects the atmospheric reanalysis product JRA-55 (Kobayashi et al., 2015) using satellite and other atmospheric reanalysis products. The merits of JRA55-do are the high horizontal resolution (~55 km) and temporal interval (3 h). An assessment by Tsujino et al. (2020) implies that JRA55-do can suitably replace the current CORE/OMIP-1 dataset.
   - Versions 1.3 and 1.4 covers 01Jan1958 to 10Jan2020. Version 1.3 and 1.4 differ only in runoff around Greenland: version 1.3 uses climatological (1961-1990) runoff from Greenland (Bamber et al.2012) and version 1.4 uses inter-annually varying (1958-2016) runoff from Greenland and Canadian Arctic Archipelagos (Bamber et al.2018). See User guide for details.
   - from J-M Molines details: JRA55 files were downloaded from the ESG site hosted at LLNL (Lawrence Livermore National Laboratory, US) for OMIP experiments. Dedicated wget scripts were used for downloading the data (see the TOOLS/FORCING/WGET directory).
   In order to use interpolation on the fly capability of NEMO, the files were 'drowned' using the SOSIE package at commit cf9bdff12...
   Additional pre-processing was performed to produce files for the total precipitation (solid+liquid), needed by NEMO, as JRA55 native files give liquid (rain) and solid (snow) separately. Note that snow-fall is used by the ice model.
   Finally, some additional processing was performed in order to restore some intesting variable attributes (units, long_name, standard_name, comment) in the netcdf files (lost during the drowning procedure).
* user guide:
   - https://climate.mri-jma.go.jp/~htsujino/docs/JRA55-do/v1_4-manual/User_manual_jra55_do_v1_4.pdf
* reference:
   - Bamber, J. L., Tedstone, A. J., King, M. D., Howat, I. M., Enderlin, E. M., van den Broeke, M. R., & Noel, B. (2018). Land ice freshwater budget of the Arctic and North Atlantic Oceans: 1. Data, methods, and results. Journal of Geophysical Research: Oceans, 123, 1827– 1837. https://doi.org/10.1002/2017JC013605
   - Hiroyuki Tsujino, Shogo Urakawa, Hideyuki Nakano, R. Justin Small, Who M. Kim, Stephen G. Yeager, Gokhan Danabasoglu, Tatsuo Suzuki, Jonathan L. Bamber, Mats Bentsen, Claus W. Böning, Alexandra Bozec, Eric P. Chassignet, Enrique Curchitser, Fabio Boeira Dias, Paul J. Durack, Stephen M. Griffies, Yayoi Harada, Mehmet Ilicak, Simon A. Josey, Chiaki Kobayashi, Shinya Kobayashi, Yoshiki Komuro, William G. Large, Julien Le Sommer, Simon J. Marsland, Simona Masina, Markus Scheinert, Hiroyuki Tomita, Maria Valdivieso, Dai Yamazaki, JRA-55 based surface dataset for driving ocean–sea-ice models (JRA55-do), Ocean Modelling, Volume 130, 2018, Pages 79-139, ISSN 1463-5003, https://doi.org/10.1016/j.ocemod.2018.07.002.
   - Tsujino, H., Urakawa, S., Nakano, H., Small, R. J., Kim, W. M., Yeager, S. G., … Yamazaki, D. (2018). JRA-55 based surface dataset for driving ocean–sea-ice models (JRA55-do). Ocean Modelling, 130, 79–139. https://doi.org/10.1016/j.ocemod.2018.07.002
* path:
   - drowned file: TOADD
