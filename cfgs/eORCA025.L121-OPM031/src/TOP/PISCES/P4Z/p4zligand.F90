MODULE p4zligand
   !!======================================================================
   !!                         ***  MODULE p4zligand  ***
   !! TOP :   PISCES Compute remineralization/dissolution of organic ligands
   !!=========================================================================
   !! History :   3.6  !  2016-03  (O. Aumont, A. Tagliabue) Quota model and reorganization
   !!----------------------------------------------------------------------
   !!   p4z_ligand     :  Compute remineralization/dissolution of organic ligands
   !!   p4z_ligand_init:  Initialisation of parameters for remineralisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE prtctl_trc      ! print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_ligand         ! called in p4zbio.F90
   PUBLIC   p4z_ligand_init    ! called in trcsms_pisces.F90

   REAL(wp), PUBLIC ::  rlgw     !: lifetime (years) of weak ligands
   REAL(wp), PUBLIC ::  rlgs     !: lifetime (years) of strong ligands
   REAL(wp), PUBLIC ::  rlig     !: Remin ligand production
   REAL(wp), PUBLIC ::  prlgw    !: Photochemical of weak ligand

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zligand.F90 12276 2019-12-20 11:14:26Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_ligand( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_ligand  ***
      !!
      !! ** Purpose :   Compute remineralization/scavenging of organic ligands
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt ! ocean time step
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zlgwp, zlgwpr, zlgwr, zlablgw
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zligrem, zligpr, zligprod
      CHARACTER (len=25) ::   charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_ligand')
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               !
               ! ------------------------------------------------------------------
               ! Remineralization of iron ligands
               ! ------------------------------------------------------------------
               ! production from remineralisation of organic matter
               zlgwp = orem(ji,jj,jk) * rlig
               ! decay of weak ligand
               ! This is based on the idea that as LGW is lower
               ! there is a larger fraction of refractory OM
               zlgwr = max( rlgs , rlgw * exp( -2 * (trb(ji,jj,jk,jplgw)*1e9) ) ) ! years
               zlgwr = 1. / zlgwr * tgfunc(ji,jj,jk) * ( xstep / nyear_len(1) ) * blim(ji,jj,jk) * trb(ji,jj,jk,jplgw)
               ! photochem loss of weak ligand
               zlgwpr = prlgw * xstep * etot(ji,jj,jk) * trb(ji,jj,jk,jplgw) * (1. - fr_i(ji,jj))
               tra(ji,jj,jk,jplgw) = tra(ji,jj,jk,jplgw) + zlgwp - zlgwr - zlgwpr
               zligrem(ji,jj,jk)   = zlgwr
               zligpr(ji,jj,jk)    = zlgwpr
               zligprod(ji,jj,jk)  = zlgwp
               !
            END DO
         END DO
      END DO
      !
      !  Output of some diagnostics variables
      !     ---------------------------------
      IF( lk_iomput .AND. knt == nrdttrc ) THEN
         IF( iom_use( "LIGREM" ) ) THEN
           zligrem(:,:,jpk) = 0.  ; CALL iom_put( "LIGREM", zligrem(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:) )
         ENDIF
         IF( iom_use( "LIGPR" ) ) THEN
           zligpr(:,:,jpk) = 0.   ; CALL iom_put( "LIGPR" , zligpr(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:) )
         ENDIF
         IF( iom_use( "LPRODR" ) ) THEN
           zligprod(:,:,jpk) = 0. ; CALL iom_put( "LPRODR", zligprod(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:) )
         ENDIF
      ENDIF
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('ligand1')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_ligand')
      !
   END SUBROUTINE p4z_ligand


   SUBROUTINE p4z_ligand_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_ligand_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampislig namelist and check the parameters
      !!
      !! ** input   :   Namelist nampislig
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer 
      !
      NAMELIST/nampislig/ rlgw, prlgw, rlgs, rlig
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_ligand_init : remineralization/scavenging of organic ligands'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
      ENDIF

      REWIND( numnatp_ref )
      READ  ( numnatp_ref, nampislig, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampislig in reference namelist' )

      REWIND( numnatp_cfg )
      READ  ( numnatp_cfg, nampislig, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampislig in configuration namelist' )
      IF(lwm) WRITE ( numonp, nampislig )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampislig'
         WRITE(numout,*) '      Lifetime (years) of weak ligands             rlgw  =', rlgw
         WRITE(numout,*) '      Remin ligand production per unit C           rlig  =', rlig
         WRITE(numout,*) '      Photolysis of weak ligand                    prlgw =', prlgw
         WRITE(numout,*) '      Lifetime (years) of strong ligands           rlgs  =', rlgs
      ENDIF
      !
   END SUBROUTINE p4z_ligand_init

   !!======================================================================
END MODULE p4zligand
