MODULE sedbtb
   !!======================================================================
   !!              ***  MODULE  sedbtb  ***
   !!    Sediment : bioturbation of the solid components
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedmat  ! linear system of equations
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_btb


   !! $Id: sedbtb.F90 10222 2018-10-25 09:42:23Z aumont $
CONTAINS
   
   SUBROUTINE sed_btb( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_btb  ***
      !!
      !! ** Purpose :  performs bioturbation of the solid sediment components
      !!
      !! ** Method  :  ``diffusion'' of solid sediment components. 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) F90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  kt              ! time step

      ! * local variables
      INTEGER :: ji, jk, js
      REAL(wp), DIMENSION(jpoce,jpksedm1,jpsol) ::  zsol  !   solution
      !------------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_btb')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_btb : Bioturbation  '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF

      ! Initializations
      !----------------
      zsol(:,:,:) = 0.

      ! right hand side of coefficient matrix
      !--------------------------------------
      DO js = 1, jpsol
         DO jk = 1, jpksedm1
            DO ji = 1, jpoce
               zsol(ji,jk,js) = solcp(ji,jk+1,js)
            ENDDO
         ENDDO
      ENDDO

      CALL sed_mat( jpsol, jpoce, jpksedm1, zsol, dtsed / 2.0 )


      ! store solution of the tridiagonal system
      !------------------------
      DO js = 1, jpsol
         DO jk = 1, jpksedm1
            DO ji = 1, jpoce
               solcp(ji,jk+1,js) = zsol(ji,jk,js)
            ENDDO
         ENDDO
      ENDDO

      IF( ln_timing )  CALL timing_stop('sed_btb')

   END SUBROUTINE sed_btb

END MODULE sedbtb
