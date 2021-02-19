******
NetCDF
******

Chunking
========

The definition of chunking is well described here: https://www.unidata.ucar.edu/software/netcdf/workshops/most-recent/nc4chunking/WhatIsChunking.html.

- how to know the chunk size of a file (ncdump -hs):

.. code-block:: console

  ncdump -hs toce_nico.nc

  netcdf toce_nico {
  dimensions:
  	deptht = 75 ;
  	axis_nbounds = 2 ;
  	y = 567 ;
  	x = 687 ;
  	time_counter = UNLIMITED ; // (32 currently)
  variables:
  ...
  	float toce(time_counter, deptht, y, x) ;
  		toce:standard_name = "sea_water_potential_temperature" ;
      ...
  		toce:_Storage = "chunked" ;
  		toce:_ChunkSizes = 1, 19, 142, 172 ;
  		toce:_Endianness = "little" ;

- In the previous file, it means to access toce, netcdf will read tile of (1, 19, 142, 172).
  So, if you read with an horizontal 2d access (a map), you will read more data than needed because netcdf will first read 19 levels then filter out the map you want.
  For a 'map' access, the best is to have a chunksize of 1 on the vertical. I don't know for AMUXL, but for eORCA12 access of 3d variable this is CRITICAL to have the right chunking size.
  If you plan to extract only vertical profile from your output file, the chunck will probably be something like (1, nz, cx, cy) cx and cy to be defined by testing.

- how to specify a specific chunking: to do so, you can either:
  - copy you file with nccopy and specifying a new chunksize with the -c option.
  - extract and copy with ncks and  --cnk_dmn option

.. code-block:: console

  nccopy -c x/200,y/200,deptht/1,time_counter/1 toce_nico.nc out.nc

or

.. code-block:: console

  ncks --cnk_dmn x,200 --cnk_dmn y,200 --cnk_dmn time_counter,1 --cnk_dmn deptht,1 -d deptht,0,0 -v toce toce_nico.nc out.nc

- If you found on of your processing is very very slow, have a look to your chunksize and see if it is adapted to your reading pattern.

Deflation
=========
Netcdf since the version 4 support deflation (ie compression),
depending on your netcdf it can save a lot of storage (the more 0 you have, the more efficient it is).

Below some test made with AMUXL12 configuration with a file containing a month of daily 3d temperature.

- as expected, the higher is the compression level, the longer it takes to create it (-d option of nccopy or -L option with ncks).

.. code-block:: console

  time nccopy -d 0 -c x/200,y/200,deptht/1,time_counter/1 toce_nico.nc toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d0

  real	0m23.817s
  user	0m2.731s
  sys	0m7.741s

.. code-block:: console

  time nccopy -d 1 -c x/200,y/200,deptht/1,time_counter/1 toce_nico.nc toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d1

  real	1m37.195s
  user	1m32.293s
  sys	0m4.242s

.. code-block:: console

  time nccopy -d 2 -c x/200,y/200,deptht/1,time_counter/1 toce_nico.nc toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d2

  real	1m38.894s
  user	1m34.070s
  sys	0m4.317s

.. code-block:: console

  time nccopy -d 9 -c x/200,y/200,deptht/1,time_counter/1 toce_nico.nc toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d9

  real	2m27.320s
  user	2m19.162s
  sys	0m4.354s

- as expected, the higher the compression level is, the smaller the final file is. However for AMUXL,
  the differences are tiny between d1 and d9 and you will spend more time to create it and read it (2.096 Gb for d1 and 2.068 Gb for d9).
  d1 is thus a good compromise. You spare 1.7 Gb (ie 44 %) of storage while minimizing the draw back of a slower writing/reading
  (probably this depends of the computer as you trade reading by cpu for decompression)

.. code-block:: console

  3804842884 Jan 20 10:23 toce_nico.nc
  2096447815 Jan 20 10:25 toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d1
  2094722329 Jan 20 10:38 toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d2
  2068371276 Jan 20 10:33 toce_nico.nc_4.4.4-intel-19.0.4-intelmpi-2019.4.243_d9

nco
===

NCO is netcdf tools box to manipulate netcdf. It is very useful to extract date, concatenate file, make some operation, average files ...

ncks
----
ncks is a tools to extract data out of a netcdf file. I allow to extract/exclude variable or cut file along dimension.

For exemple, the following command will extract only the first deptht level (-d option) from toce variable (-v option) in out.nc.
out.nc will be written with a deflation level of 1 (-L option) and a chunksize of (x/200, y/200, deptht/1, time_counter/1).

.. code-block:: console

  ncks -L 1 --cnk_dmn x,200 --cnk_dmn y,200 --cnk_dmn time_counter,1 --cnk_dmn deptht,1 -d deptht,0,0 -v toce toce_nico.nc out.nc


****************
reStructuredText
****************

Creation of the html from rst file
==================================

1: installation of sphinx:
  conda is your best friends:

.. code-block:: console

  $ conda install sphinx

2: run a quick installation:
  The fastest way to do it is to use sphinx-quickstart

.. code-block:: console

  $ sphinx-quickstart

  your output should be like this:

.. code-block:: console

  Welcome to the Sphinx 3.2.1 quickstart utility.

  Please enter values for the following settings (just press Enter to
  accept a default value, if one is given in brackets).

  Selected root path: .

  You have two options for placing the build directory for Sphinx output.
  Either, you use a directory "_build" within the root path, or you separate
  "source" and "build" directories within the root path.
  > Separate source and build directories (y/n) [n]: y

  The project name will occur in several places in the built documentation.
  > Project name: test
  > Author name(s): toto
  > Project release []: 0.0

  If the documents are to be written in a language other than English,
  you can select a language here by its language code. Sphinx will then
  translate text that it generates into that language.

  For a list of supported codes, see
  https://www.sphinx-doc.org/en/master/usage/configuration.html#confval-language.
  > Project language [en]:

  Creating file /Users/mathiotp/Test/source/conf.py.
  Creating file /Users/mathiotp/Test/source/index.rst.
  Creating file /Users/mathiotp/Test/Makefile.
  Creating file /Users/mathiotp/Test/make.bat.

  Finished: An initial directory structure has been created.

  You should now populate your master file /Users/mathiotp/Test/source/index.rst and create other documentation
  source files. Use the Makefile to build the docs, like so:
     make builder
  where "builder" is one of the supported builders, e.g. html, latex or linkcheck.

3: build the web site
  the command make html will do the job for you.

.. code-block:: console

  $ make html

  Running Sphinx v3.2.1
  making output directory... done
  building [mo]: targets for 0 po files that are out of date
  building [html]: targets for 1 source files that are out of date
  updating environment: [new config] 1 added, 0 changed, 0 removed
  reading sources... [100%] index
  looking for now-outdated files... none found
  pickling environment... done
  checking consistency... done
  preparing documents... done
  writing output... [100%] index
  generating indices...  genindexdone
  writing additional pages...  searchdone
  copying static files... ... done
  copying extra files... done
  dumping search index in English (code: en)... done
  dumping object inventory... done
  build succeeded.

4: visualise your html
  a index.html is available in build/html and you can visualise it with your favorite browser

5: customize it
  the graphical option are controle by the conf.py file in source.
  For something similar to this, refer to the conf.py in the source directory of the ghpages branch project.

6: publish it on github pages.
  This is probably the most tricky part.

- setp 1: you need to have an account on git, create your project and push all the files (make, build, source ...). If you are not premium, you need to have your repo public.
- step 2: create a branch call it gh-pages
- step 3: check in your repository setting that gh-pages is activated
- step 4: add an empty file .nojekyll in your directory to tell github to not interpret it as md.
- step 5: if not done, you need to add in your conf.py these lines (it works for me):

.. code-block:: console

  import sphinx_rtd_theme

  html_theme = 'sphinx_rtd_theme'

and add sphinx_rtd_theme to the extension:

.. code-block:: console

  extensions = [...,'sphinx_rtd_theme']

These modules can be installed with these commands:

.. code-block:: console

    conda install -c anaconda sphinx_rtd_theme
    conda install -c conda-forge sphinxcontrib-bibtex

- step 6: commit and push the changes to gh-pages branch (you can check in your github interface if the web site is well published).
  The site will be in 'https://username.github.io/REPO/docs/build/html/index.html'. For a quick access, you can add the link to your README.

Syntax of rst file
==================
- a quick and detailed guide is available here: https://docutils.sourceforge.io/docs/user/rst/quickref.html
- All this site is written in rst, so for any option you see in these pages, you can refer to the source code.

**********
CMIP6 data
**********

Wget
====

You can find all the available name, variable ... here: https://esgf-index1.ceda.ac.uk/search/cmip6-ceda/
A listing of all the facet available for the query is described here: https://esgf-node.llnl.gov/esg-search/search?project=CMIP6&facets=*&limit=0

To create the wget script:
1. look for your data
   To do this you have to enter this http: https://esgf-node.llnl.gov/esg-search/search?experiment_id=ssp585&variable=tob&distrib=true.
   In this case, I am looking for the variable tob in the ssp585 experiment id on all the server. More details on the method here:
   https://esgf.github.io/esgf-user-support/user_guide.html#facet-queries.
2. get your wget script
   Replace in the http, 'search?' by 'wget?'
3. run the script with the -s option

For more advanced method, look at the documentation.

***
Git
***

**Create a remote branch:**

- create a local branch (it will create a local from your local branch):

.. code-block:: console

    git checkout -b <branch-name>

- push the branch to the remote directory:

.. code-block:: console

    git push -u origin <branch-name>

**List available branches:**

.. code-block:: console

    git branch

**Checkout remote branch from a team member:**

.. code-block:: console

    git fetch
    git checkout <branch-name>

*****
CONDA
*****

- Create an environment:

.. code-block:: console

    conda create -n <env name> python=X.Y.Z

- add a new package in an environement:

.. code-block:: console

    conda install <package name>

or

.. code-block:: console

    conda install -c conda-forge <package name>

- export an environment:

.. code-block:: console

    conda env export > <env file name>.yml

- create an environment from a yml file:

.. code-block:: console

    conda env create -f <env file name>.yml

- Activate or deactivate an environment:

.. code-block:: console

    conda activate <env name>
    conda deactivate
