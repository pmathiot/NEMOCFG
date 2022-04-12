MODULE trcnam_pisces
   !!======================================================================
   !!                      ***  MODULE trcnam_pisces  ***
   !! TOP :   initialisation of some run parameters for PISCES bio-model
   !!======================================================================
   !! History :    -   !  1999-10 (M.A. Foujols, M. Levy) original code
   !!              -   !  2000-01 (L. Bopp) hamocc3, p3zd
   !!             1.0  !  2003-08 (C. Ethe)  module F90
   !!             2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.pisces.h90
   !!----------------------------------------------------------------------
   !! trc_nam_pisces   : PISCES model namelist read
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE sms_pisces      ! sms trends
   USE trdtrc_oce
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_pisces   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcnam_pisces.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_nam_pisces
      !!----------------------------------------------------------------------
      !!                     ***  trc_nam_pisces  ***  
      !!
      !! ** Purpose :   read PISCES namelist
      !!
      !! ** input   :   file 'namelist.trc.sms' containing the following
      !!             namelist: natext, natbio, natsms
      !!----------------------------------------------------------------------
      INTEGER :: jl, jn
      INTEGER :: ios, ioptio         ! Local integer
      CHARACTER(LEN=20)::   clname
      !!
      NAMELIST/nampismod/ln_p2z, ln_p4z, ln_p5z, ln_ligand, ln_sediment
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      clname = 'namelist_pisces'

      IF(lwp) WRITE(numout,*) 'trc_nam_pisces : read PISCES namelist'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~'
      CALL ctl_opn( numnatp_ref, TRIM( clname )//'_ref', 'OLD'    , 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      CALL ctl_opn( numnatp_cfg, TRIM( clname )//'_cfg', 'OLD'    , 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      IF(lwm) CALL ctl_opn( numonp     , 'output.namelist.pis' , 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      !
      REWIND( numnatp_ref )              ! Namelist nampisbio in reference namelist : Pisces variables
      READ  ( numnatp_ref, nampismod, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampismod in reference namelist' )
      REWIND( numnatp_cfg )              ! Namelist nampisbio in configuration namelist : Pisces variables
      READ  ( numnatp_cfg, nampismod, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampismod in configuration namelist' )
      IF(lwm) WRITE( numonp, nampismod )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*) '   Namelist : nampismod '
         WRITE(numout,*) '      Flag to use LOBSTER model            ln_p2z      = ', ln_p2z
         WRITE(numout,*) '      Flag to use PISCES standard model    ln_p4z      = ', ln_p4z
         WRITE(numout,*) '      Flag to use PISCES quota    model    ln_p5z      = ', ln_p5z
         WRITE(numout,*) '      Flag to ligand                       ln_ligand   = ', ln_ligand
         WRITE(numout,*) '      Flag to use sediment                 ln_sediment = ', ln_sediment
      ENDIF
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*)
         IF( ln_p5z      )  WRITE(numout,*) '   ==>>>   PISCES QUOTA model is used'
         IF( ln_p4z      )  WRITE(numout,*) '   ==>>>   PISCES STANDARD model is used'
         IF( ln_p2z      )  WRITE(numout,*) '   ==>>>   LOBSTER model is used'
         IF( ln_ligand )  WRITE(numout,*) '   ==>>>   Compute remineralization/dissolution of organic ligands'
         IF( ln_sediment )  WRITE(numout,*) '   ==>>>   Sediment module is used'
      ENDIF
    
      ioptio = 0
      IF( ln_p2z )    ioptio = ioptio + 1
      IF( ln_p4z )    ioptio = ioptio + 1
      IF( ln_p5z )    ioptio = ioptio + 1
      !
      IF( ioptio /= 1 )   CALL ctl_stop( 'Choose ONE PISCES model namelist nampismod' )
       !
   END SUBROUTINE trc_nam_pisces

   !!======================================================================
END MODULE trcnam_pisces
