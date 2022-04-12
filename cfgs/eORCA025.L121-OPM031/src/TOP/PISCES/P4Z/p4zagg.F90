MODULE p4zagg
   !!======================================================================
   !!                         ***  MODULE p4zagg  ***
   !! TOP :  PISCES  aggregation of particles
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Change aggregation formula
   !!             3.5  !  2012-07  (O. Aumont) Introduce potential time-splitting
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   p4z_agg       :  Compute aggregation of particles
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE prtctl_trc      !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_agg         ! called in p4zbio.F90

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zagg.F90 10069 2018-08-28 14:12:24Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_agg ( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_agg  ***
      !!
      !! ** Purpose :   Compute aggregation of particles
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   !
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zagg, zagg1, zagg2, zagg3, zagg4
      REAL(wp) ::   zaggpoc1, zaggpoc2, zaggpoc3, zaggpoc4
      REAL(wp) ::   zaggpoc , zaggfe, zaggdoc, zaggdoc2, zaggdoc3
      REAL(wp) ::   zaggpon , zaggdon, zaggdon2, zaggdon3
      REAL(wp) ::   zaggpop, zaggdop, zaggdop2, zaggdop3
      REAL(wp) ::   zaggtmp, zfact, zmax
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_agg')
      !
      !  Exchange between organic matter compartments due to coagulation/disaggregation
      !  ---------------------------------------------------
      IF( ln_p4z ) THEN
         !
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !
                  zfact = xstep * xdiss(ji,jj,jk)
                  !  Part I : Coagulation dependent on turbulence
                  zagg1 = 25.9  * zfact * trb(ji,jj,jk,jppoc) * trb(ji,jj,jk,jppoc)
                  zagg2 = 4452. * zfact * trb(ji,jj,jk,jppoc) * trb(ji,jj,jk,jpgoc)

                  ! Part II : Differential settling

                  !  Aggregation of small into large particles
                  zagg3 =  47.1 * xstep * trb(ji,jj,jk,jppoc) * trb(ji,jj,jk,jpgoc)
                  zagg4 =  3.3  * xstep * trb(ji,jj,jk,jppoc) * trb(ji,jj,jk,jppoc)

                  zagg   = zagg1 + zagg2 + zagg3 + zagg4
                  zaggfe = zagg * trb(ji,jj,jk,jpsfe) / ( trb(ji,jj,jk,jppoc) + rtrn )

                  ! Aggregation of DOC to POC : 
                  ! 1st term is shear aggregation of DOC-DOC
                  ! 2nd term is shear aggregation of DOC-POC
                  ! 3rd term is differential settling of DOC-POC
                  zaggdoc  = ( ( 0.369 * 0.3 * trb(ji,jj,jk,jpdoc) + 102.4 * trb(ji,jj,jk,jppoc) ) * zfact       &
                  &            + 2.4 * xstep * trb(ji,jj,jk,jppoc) ) * 0.3 * trb(ji,jj,jk,jpdoc)
                  ! transfer of DOC to GOC : 
                  ! 1st term is shear aggregation
                  ! 2nd term is differential settling 
                  zaggdoc2 = ( 3.53E3 * zfact + 0.1 * xstep ) * trb(ji,jj,jk,jpgoc) * 0.3 * trb(ji,jj,jk,jpdoc)
                  ! tranfer of DOC to POC due to brownian motion
                  zaggdoc3 =  114. * 0.3 * trb(ji,jj,jk,jpdoc) *xstep * 0.3 * trb(ji,jj,jk,jpdoc)

                  !  Update the trends
                  tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) - zagg + zaggdoc + zaggdoc3
                  tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zagg + zaggdoc2
                  tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) - zaggfe
                  tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zaggfe
                  tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) - zaggdoc - zaggdoc2 - zaggdoc3
                  !
                  conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zagg + zaggdoc + zaggdoc3
                  prodgoc(ji,jj,jk) = prodgoc(ji,jj,jk) + zagg + zaggdoc2
                  !
               END DO
            END DO
         END DO
      ELSE    ! ln_p5z
        !
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !
                  zfact = xstep * xdiss(ji,jj,jk)
                  !  Part I : Coagulation dependent on turbulence
                  zaggtmp = 25.9  * zfact * trb(ji,jj,jk,jppoc)
                  zaggpoc1 = zaggtmp * trb(ji,jj,jk,jppoc)
                  zaggtmp = 4452. * zfact * trb(ji,jj,jk,jpgoc)
                  zaggpoc2 = zaggtmp * trb(ji,jj,jk,jppoc)

                  ! Part II : Differential settling
   
                  !  Aggregation of small into large particles
                  zaggtmp =  47.1 * xstep * trb(ji,jj,jk,jpgoc)
                  zaggpoc3 = zaggtmp * trb(ji,jj,jk,jppoc)
                  zaggtmp =  3.3  * xstep * trb(ji,jj,jk,jppoc)
                  zaggpoc4 = zaggtmp * trb(ji,jj,jk,jppoc)

                  zaggpoc   = zaggpoc1 + zaggpoc2 + zaggpoc3 + zaggpoc4
                  zaggpon = zaggpoc * trb(ji,jj,jk,jppon) / ( trb(ji,jj,jk,jppoc) + rtrn)
                  zaggpop = zaggpoc * trb(ji,jj,jk,jppop) / ( trb(ji,jj,jk,jppoc) + rtrn)
                  zaggfe = zaggpoc * trb(ji,jj,jk,jpsfe) / ( trb(ji,jj,jk,jppoc)  + rtrn )

                  ! Aggregation of DOC to POC : 
                  ! 1st term is shear aggregation of DOC-DOC
                  ! 2nd term is shear aggregation of DOC-POC
                  ! 3rd term is differential settling of DOC-POC
                  zaggtmp = ( ( 0.369 * 0.3 * trb(ji,jj,jk,jpdoc) + 102.4 * trb(ji,jj,jk,jppoc) ) * zfact       &
                  &            + 2.4 * xstep * trb(ji,jj,jk,jppoc) )
                  zaggdoc  = zaggtmp * 0.3 * trb(ji,jj,jk,jpdoc)
                  zaggdon  = zaggtmp * 0.3 * trb(ji,jj,jk,jpdon)
                  zaggdop  = zaggtmp * 0.3 * trb(ji,jj,jk,jpdop)

                  ! transfer of DOC to GOC : 
                  ! 1st term is shear aggregation
                  ! 2nd term is differential settling 
                  zaggtmp = ( 3.53E3 * zfact + 0.1 * xstep ) * trb(ji,jj,jk,jpgoc)
                  zaggdoc2 = zaggtmp * 0.3 * trb(ji,jj,jk,jpdoc)
                  zaggdon2 = zaggtmp * 0.3 * trb(ji,jj,jk,jpdon)
                  zaggdop2 = zaggtmp * 0.3 * trb(ji,jj,jk,jpdop)

                  ! tranfer of DOC to POC due to brownian motion
                  zaggtmp = ( 114. * 0.3 * trb(ji,jj,jk,jpdoc) ) * xstep
                  zaggdoc3 =  zaggtmp * 0.3 * trb(ji,jj,jk,jpdoc)
                  zaggdon3 =  zaggtmp * 0.3 * trb(ji,jj,jk,jpdon)
                  zaggdop3 =  zaggtmp * 0.3 * trb(ji,jj,jk,jpdop)

                  !  Update the trends
                  tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) - zaggpoc + zaggdoc + zaggdoc3
                  tra(ji,jj,jk,jppon) = tra(ji,jj,jk,jppon) - zaggpon + zaggdon + zaggdon3
                  tra(ji,jj,jk,jppop) = tra(ji,jj,jk,jppop) - zaggpop + zaggdop + zaggdop3
                  tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zaggpoc + zaggdoc2
                  tra(ji,jj,jk,jpgon) = tra(ji,jj,jk,jpgon) + zaggpon + zaggdon2
                  tra(ji,jj,jk,jpgop) = tra(ji,jj,jk,jpgop) + zaggpop + zaggdop2
                  tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) - zaggfe
                  tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zaggfe
                  tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) - zaggdoc - zaggdoc2 - zaggdoc3
                  tra(ji,jj,jk,jpdon) = tra(ji,jj,jk,jpdon) - zaggdon - zaggdon2 - zaggdon3
                  tra(ji,jj,jk,jpdop) = tra(ji,jj,jk,jpdop) - zaggdop - zaggdop2 - zaggdop3
                  !
                  conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zaggpoc + zaggdoc + zaggdoc3
                  prodgoc(ji,jj,jk) = prodgoc(ji,jj,jk) + zaggpoc + zaggdoc2
                  !
               END DO
            END DO
         END DO
         !
      ENDIF
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('agg')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_agg')
      !
   END SUBROUTINE p4z_agg

   !!======================================================================
END MODULE p4zagg
