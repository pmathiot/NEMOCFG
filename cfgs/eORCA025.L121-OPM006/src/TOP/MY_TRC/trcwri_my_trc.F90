MODULE trcwri_my_trc
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!     trc_wri_my_trc   :  outputs of concentration fields
   !!======================================================================
#if defined key_top && defined key_iomput
   !!----------------------------------------------------------------------
   !! History :      !  2007  (C. Ethe, G. Madec)  Original code
   !!                !  2016  (C. Ethe, T. Lovato) Revised architecture
   !!----------------------------------------------------------------------
   USE par_trc         ! passive tracers common variables
   USE trc         ! passive tracers common variables 
   USE iom         ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_my_trc 

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcwri_my_trc.F90 10069 2018-08-28 14:12:24Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_wri_my_trc
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_trc  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      CHARACTER (len=20)   :: cltra
      INTEGER              :: jn
      !!---------------------------------------------------------------------
 
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      DO jn = jp_myt0, jp_myt1
         cltra = TRIM( ctrcnm(jn) )                  ! short title for tracer
         CALL iom_put( cltra, trn(:,:,:,jn) )
      END DO
      !
   END SUBROUTINE trc_wri_my_trc

#else

CONTAINS

   SUBROUTINE trc_wri_my_trc
      !
   END SUBROUTINE trc_wri_my_trc

#endif

END MODULE trcwri_my_trc
