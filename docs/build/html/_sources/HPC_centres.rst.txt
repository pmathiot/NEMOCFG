***********
HPC centres
***********

=======
Occigen
=======

Login
=====

How to ask a login
------------------
- WARNING: do not use safari as the display of a page to print do not work
- link to the form to fill: https://www.cines.fr/services/formulaires-et-textes/

How to log in
-------------
The easiest to connect is to use a proxy jump. You will still have to enter your password for Occigen but not for the gateway.
To do so you need to:

- set up your ssh key to be able to connect to your gateway without password.

  * If you use a non-empty passphrase, you may consider using a ssh-agent to manage your key during your session

- set up you proxy jump:

.. code-block:: console

    Host occigen
      ProxyJump gatewayname
      User username
      HostName occigen.cines.fr
      ForwardX11 yes
      ForwardX11Timeout 24h
      ForwardX11Trusted yes

Now you can connect with only the occigen password to enter:

.. code-block:: console

    ssh occigen

Python
======
To use python on Occigen, the easiest is to use conda.

- Full documentation on the package management is available here:
  https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#create-env-from-file
- The conda module can be load:

.. code-block:: console

   module load /opt/software/alfred/spack-dev/modules/tools/linux-rhel7-x86_64/miniconda3/4.7.12.1-gcc-4.8.5

- Installing packages on the home will put a lot of file and possibly reach the inode limit on your home.
  The work around is to create a .conda directory on your scratchdir and link it to your home. The drawback is that it is not backed up.
  The work around it to generate your environment file and save it to your home directory or elsewhere.

.. code-block:: console

  (base) xxxxxx@machine:~$ ls -la
  ...
  lrwxrwxrwx.  1 xxxxx yyyyyy    .....  .conda -> /scratch/zzzzz/yyyy/xxxx/.conda
  ...


- You can generate an environment file like this:

.. code-block:: console

  conda env export --from-history

- You can create an env from an env file like this:

.. code-block:: console

  conda env create -f environment.yml

======
GRICAD
======

For full details on how to have a login, log in and how to use the computer, see the documentation `here <https://gricad-doc.univ-grenoble-alpes.fr/en/hpc/connexion/>`_

***************
Parallelisation
***************

===
MPI
===

=======
Open MP
=======

===
GPU
===
