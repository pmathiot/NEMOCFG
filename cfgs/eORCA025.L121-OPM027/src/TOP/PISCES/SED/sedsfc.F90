MODULE sedsfc
   !!======================================================================
   !!              ***  MODULE  sedsfc  ***
   !!    Sediment : Data at sediment surface
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedarr
   USE seddta

   PUBLIC sed_sfc

   !! $Id: sedsfc.F90 10222 2018-10-25 09:42:23Z aumont $
CONTAINS

   SUBROUTINE sed_sfc( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_sfc ***
      !!
      !! ** Purpose :  Give data from sediment model to tracer model
      !!
      !!
      !!   History :
      !!        !  06-04 (C. Ethe)  Orginal code
      !!----------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  kt              ! time step

      ! * local variables
      INTEGER :: ji, jj, ikt     ! dummy loop indices

      !------------------------------------------------------------------------
      ! reading variables

      IF( ln_timing )  CALL timing_start('sed_sfc')

      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,1), iarroce(1:jpoce), pwcp(1:jpoce,1,jwalk) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,2), iarroce(1:jpoce), pwcp(1:jpoce,1,jwdic) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,3), iarroce(1:jpoce), pwcp(1:jpoce,1,jwno3) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,4), iarroce(1:jpoce), pwcp(1:jpoce,1,jwpo4) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,5), iarroce(1:jpoce), pwcp(1:jpoce,1,jwoxy) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,6), iarroce(1:jpoce), pwcp(1:jpoce,1,jwsil) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,7), iarroce(1:jpoce), pwcp(1:jpoce,1,jwnh4) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,8), iarroce(1:jpoce), pwcp(1:jpoce,1,jwfe2) )


      DO jj = 1,jpj
         DO ji = 1, jpi
            ikt = mbkt(ji,jj)
            IF ( tmask(ji,jj,ikt) == 1 ) THEN
               trb(ji,jj,ikt,jptal) = trc_data(ji,jj,1)
               trb(ji,jj,ikt,jpdic) = trc_data(ji,jj,2)
               trb(ji,jj,ikt,jpno3) = trc_data(ji,jj,3) * 7.625
               trb(ji,jj,ikt,jppo4) = trc_data(ji,jj,4) * 122.
               trb(ji,jj,ikt,jpoxy) = trc_data(ji,jj,5)
               trb(ji,jj,ikt,jpsil) = trc_data(ji,jj,6)
               trb(ji,jj,ikt,jpnh4) = trc_data(ji,jj,7) * 7.625
               trb(ji,jj,ikt,jpfer) = trc_data(ji,jj,8)
            ENDIF
         ENDDO
      ENDDO

      IF( ln_timing )  CALL timing_stop('sed_sfc')

   END SUBROUTINE sed_sfc

END MODULE sedsfc
