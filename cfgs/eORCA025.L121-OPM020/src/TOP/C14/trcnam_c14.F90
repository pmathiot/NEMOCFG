MODULE trcnam_c14
   !!======================================================================
   !!                         ***  MODULE trcnam_c14  ***
   !! TOP :   initialisation of some run parameters for C14 chemical model
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.cfc.h90
   !! History :        !  2015 (A.Mouchet) equilibrium + transient C14
   !!----------------------------------------------------------------------
   !! trc_nam_c14      : C14 model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE trc             ! TOP variables
   USE sms_c14

   IMPLICIT NONE
   PRIVATE
   !!
   PUBLIC   trc_nam_c14   ! called by trcnam.F90 module
   !!
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcnam_c14.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_c14
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_c14  ***
      !!                 
      !! ** Purpose :   Definition some run parameter for C14 model
      !!
      !! ** Method  :   Read the namc14 namelist and check the parameter 
      !!       values called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namelist_c14
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !! 
      NAMELIST/namc14_typ/ kc14typ,rc14at, pco2at, rc14init   ! type of C14 tracer, default values of C14/C, pco2, & ocean r14
      NAMELIST/namc14_sbc/ ln_chemh, xkwind, xdicsur          ! chem enh, wind coeff, ref DIC 
      NAMELIST/namc14_fcg/ cfileco2, cfilec14, tyrc14_beg     ! for transient exps; atm forcing
      !!-------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Radiocarbon C14'
         WRITE(numout,*) ' '
         WRITE(numout,*) ' trc_nam_c14 : Read C14 namelists'
         WRITE(numout,*) ' ~~~~~~~~~~~'
      ENDIF
      !
      ! Variable setting
      ctrcnm    (jp_c14) = 'RC14'
      ctrcln    (jp_c14) = 'Radiocarbon ratio'
      ctrcun    (jp_c14) = '-'
      ln_trc_ini(jp_c14) = .false.
      ln_trc_sbc(jp_c14) = .false.
      ln_trc_cbc(jp_c14) = .false.
      ln_trc_obc(jp_c14) = .false.
      !
      REWIND( numtrc_ref )              ! Namelist namc14_typ in reference namelist :
      READ  ( numtrc_ref, namc14_typ, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namc14_typ in reference namelist' )
      REWIND( numtrc_cfg )              ! Namelist namcfcdate in configuration namelist 
      READ  ( numtrc_cfg, namc14_typ, IOSTAT = ios, ERR = 902)
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namc14_typ in configuration namelist' )
      IF(lwm) WRITE ( numonr, namc14_typ )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*) '   Namelist : namc14_typ'
         WRITE(numout,*) '      Type of C14 tracer (0=equilibrium; 1=bomb transient; 2=past transient) kc14typ = ', kc14typ
         WRITE(numout,*) '      Default value for atmospheric C14/C (used for equil run)               rc14at  = ', rc14at
         WRITE(numout,*) '      Default value for atmospheric pcO2 [atm] (used for equil run)          pco2at  = ', pco2at
         WRITE(numout,*) '      Default value for initial C14/C in the ocean (used for equil run)      rc14init= ', rc14init
         WRITE(numout,*)
      ENDIF

      REWIND( numtrc_ref )              ! Namelist namc14_typ in reference namelist :
      READ  ( numtrc_ref, namc14_sbc, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namc14_sbc in reference namelist' )
      REWIND( numtrc_cfg )              ! Namelist namcfcdate in configuration namelist 
      READ  ( numtrc_cfg, namc14_sbc, IOSTAT = ios, ERR = 904)
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'namc14_sbc in configuration namelist' )
      IF(lwm) WRITE( numonr, namc14_sbc )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*) '   Namelist namc14_sbc'
         WRITE(numout,*) '      Chemical enhancement in piston velocity   ln_chemh = ', ln_chemh
         WRITE(numout,*) '      Coefficient for gas exchange velocity     xkwind   = ', xkwind
         WRITE(numout,*) '      Reference DIC concentration (mol/m3)      xdicsur  = ', xdicsur
         WRITE(numout,*)
      ENDIF

      REWIND( numtrc_ref )              ! Namelist namc14_typ in reference namelist :
      READ  ( numtrc_ref, namc14_fcg, IOSTAT = ios, ERR = 905)
905   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namc14_fcg in reference namelist' )
      REWIND( numtrc_cfg )              ! Namelist namcfcdate in configuration namelist 
      READ  ( numtrc_cfg, namc14_fcg, IOSTAT = ios, ERR = 906)
906   IF( ios >  0 )   CALL ctl_nam ( ios , 'namc14_fcg in configuration namelist' )
      IF(lwm) WRITE ( numonr, namc14_fcg )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*) '   Namelist namc14_fcg'
         WRITE(numout,*) '      Atmospheric co2 file ( bomb )           cfileco2   = ', TRIM( cfileco2 )
         WRITE(numout,*) '      Atmospheric c14 file ( bomb )           cfilec14   = ', TRIM( cfilec14 )
         WRITE(numout,*) '      Starting year of experiment             tyrc14_beg = ', tyrc14_beg
      ENDIF

      !
      IF( kc14typ == 2 )    tyrc14_beg = 1950._wp - tyrc14_beg   ! BP to AD dates
      ! set units 
      rlam14 = LOG(2._wp) / 5730._wp / rsiyea    ! C14 decay  rate: yr^-1 --> s^-1
      !                                          ! radiocarbon half-life is 5730 yr
   END SUBROUTINE trc_nam_c14
   
   !!======================================================================
END MODULE trcnam_c14
