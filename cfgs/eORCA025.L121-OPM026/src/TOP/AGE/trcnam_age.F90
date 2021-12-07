MODULE trcnam_age
   !!======================================================================
   !!                         ***  MODULE trcnam_age  ***
   !! TOP :   initialisation of some run parameters for Age tracer
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) 
   !!----------------------------------------------------------------------
   !! trc_nam_age      : AGE tracer initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE trc             ! Ocean variables
   USE trcsms_age      ! AGE specific variable

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_age   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcnam_age.F90 11536 2019-09-11 13:54:18Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_nam_age
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_age  ***
      !!                 
      !! ** Purpose :   Definition some run parameter for AGE model
      !!
      !! ** input   :   Namelist namage
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !!
      NAMELIST/namage/ rn_age_depth, rn_age_kill_rate 
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' Sea Age Tracer'
         WRITE(numout,*)
         WRITE(numout,*) 'trc_nam_age : Read namage namelist for Age passive tracer'
         WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF

      ! Variable setting
      ctrcnm    (jp_age) = 'Age'
      ctrcln    (jp_age) = 'Sea water age since surface contact'
      ctrcun    (jp_age) = 'year'
      ln_trc_ini(jp_age) = .false.
      ln_trc_sbc(jp_age) = .false.
      ln_trc_cbc(jp_age) = .false.
      ln_trc_obc(jp_age) = .false.
      !
      REWIND( numnat_ref )              ! Namelist namagedate in reference namelist : AGE parameters
      READ  ( numnat_ref, namage, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namage in reference namelist' )
      REWIND( numnat_cfg )              ! Namelist namagedate in configuration namelist : AGE parameters
      READ  ( numnat_cfg, namage, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namage in configuration namelist' )
      IF(lwm) WRITE ( numont, namage )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*) '   Namelist : namage'
         WRITE(numout,*) '      depth over which age tracer reset to zero     rn_age_depth      = ', rn_age_depth 
         WRITE(numout,*) '      recip of relaxation timescale                 rn_age_kill_rate  = ', rn_age_kill_rate, '[s]'
         WRITE(numout,*) '      (for age tracer shallower than age_depth) '
      ENDIF
      !
   END SUBROUTINE trc_nam_age
   
   !!======================================================================
END MODULE trcnam_age
