MODULE trcnam_cfc
   !!======================================================================
   !!                         ***  MODULE trcnam_cfc  ***
   !! TOP :   initialisation of some run parameters for CFC chemical model
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.cfc.h90
   !!----------------------------------------------------------------------
   !! trc_nam_cfc      : CFC model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE trc             ! TOP variables
   USE trcsms_cfc      ! CFC specific variable

   IMPLICIT NONE
   PRIVATE

   CHARACTER(len=34), PUBLIC ::   clname ! Input filename of CFCs atm. concentrations

   PUBLIC   trc_nam_cfc   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcnam_cfc.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_nam_cfc
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_cfc  ***
      !!                 
      !! ** Purpose :   Definition some run parameter for CFC model
      !!
      !! ** Method  :   Read the namcfc namelist and check the parameter 
      !!       values called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namcfc
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      INTEGER ::   jl, jn
      !!
      NAMELIST/namcfc/ ndate_beg, nyear_res, clname
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) ' CFCs'
         WRITE(numout,*) ' '
         WRITE(numout,*) ' trc_nam_cfc : Read namcfc namelist for CFC chemical model'
         WRITE(numout,*) ' ~~~~~~~~~~~'
      ENDIF
      !
      REWIND( numtrc_ref )              ! Namelist namcfcdate in reference namelist : CFC parameters
      READ  ( numtrc_ref, namcfc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namcfc in reference namelist' )
      REWIND( numtrc_cfg )              ! Namelist namcfcdate in configuration namelist : CFC parameters
      READ  ( numtrc_cfg, namcfc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namcfc in configuration namelist' )
      IF(lwm) WRITE( numonr, namcfc )
      IF(lwm) CALL FLUSH ( numonr )     ! flush output namelist CFC

      IF(lwp) THEN                  ! control print
         WRITE(numout,*) '   Namelist : namcfc'
         WRITE(numout,*) '      initial calendar date (aammjj) for CFC     ndate_beg = ', ndate_beg, '[yymmdd]'
         WRITE(numout,*) '      restoring time constant (year)             nyear_res = ', nyear_res
      ENDIF
      nyear_beg = ndate_beg / 10000
      IF(lwp) WRITE(numout,*) '      associated initial year (aa)               nyear_beg = ', nyear_beg, '[yy]'
      !
      jn = jp_cfc0 - 1
      ! Variables setting
      IF( ln_cfc11 ) THEN
         jn = jn + 1
         ctrcnm    (jn) = 'CFC11'
         ctrcln    (jn) = 'Chlorofluoro carbon 11 Concentration'
         ctrcun    (jn) = 'umolC/L'
         ln_trc_ini(jn) = .false.
         ln_trc_sbc(jn) = .false.
         ln_trc_cbc(jn) = .false.
         ln_trc_obc(jn) = .false.
      ENDIF
      !
      IF( ln_cfc12 ) THEN
         jn = jn + 1
         ctrcnm    (jn) = 'CFC12'
         ctrcln    (jn) = 'Chlorofluoro carbon 12 Concentration'
         ctrcun    (jn) = 'umolC/L'
         ln_trc_ini(jn) = .false.
         ln_trc_sbc(jn) = .false.
         ln_trc_cbc(jn) = .false.
         ln_trc_obc(jn) = .false.
      ENDIF
      !
      IF( ln_sf6 ) THEN
         jn = jn + 1
         ctrcnm    (jn) = 'SF6'
         ctrcln    (jn) = 'Sulfur hexafluoride Concentration'
         ctrcun    (jn) = 'umol/L'
         ln_trc_ini(jn) = .false.
         ln_trc_sbc(jn) = .false.
         ln_trc_cbc(jn) = .false.
         ln_trc_obc(jn) = .false.
      ENDIF
      !
   END SUBROUTINE trc_nam_cfc
   
   !!======================================================================
END MODULE trcnam_cfc
