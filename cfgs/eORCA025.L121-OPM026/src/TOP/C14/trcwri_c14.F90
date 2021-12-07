MODULE trcwri_c14
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    MY_SRC :   Additional outputs for C14 tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !! History :   2.0  !  2015 (A. Mouchet)  adapted code for C14
   !!----------------------------------------------------------------------
#if defined key_top && defined key_iomput
   !!----------------------------------------------------------------------
   !! trc_wri_c14   :  outputs of ventilation fields
   !!----------------------------------------------------------------------
   USE oce_trc       ! Ocean variables
   USE trc         ! passive tracers common variables 
   USE iom         ! I/O manager 
   USE sms_c14

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_c14
   !
   !   Standard ratio: 1.176E-12 ; Avogadro's nbr = 6.022E+23 at/mol ; bomb C14 traditionally reported as 1.E+26 atoms
   REAL(wp), PARAMETER  :: atomc14 = 1.176 * 6.022E-15   ! conversion factor 


CONTAINS

   SUBROUTINE trc_wri_c14
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_c14  ***
      !!
      !! ** Purpose :   output additional C14 tracers fields 
      !!---------------------------------------------------------------------
      CHARACTER (len=20)   :: cltra         ! short title for tracer
      INTEGER              :: ji,jj,jk,jn   ! dummy loop indexes
      REAL(wp)             :: zage,zarea,ztemp   ! temporary
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   :: zres, z2d ! temporary storage 2D
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: z3d , zz3d ! temporary storage 3D
      !!---------------------------------------------------------------------
 
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      cltra = TRIM( ctrcnm(jp_c14) )                  ! short title for tracer
      CALL iom_put( cltra, trn(:,:,:,jp_c14) )

      ! compute and write the tracer diagnostic in the file
      ! ---------------------------------------
      
      IF( iom_use("DeltaC14") .OR. iom_use("C14Age") .OR. iom_use("RAge")   ) THEN
         !
         ALLOCATE( z2d(jpi,jpj), zres(jpi,jpj) )
         ALLOCATE( z3d(jpi,jpj,jpk), zz3d(jpi,jpj,jpk) )
         !
         zage = -1._wp / rlam14 / rsiyea  ! factor for radioages in year
         z3d(:,:,:)  = 1._wp
         zz3d(:,:,:) = 0._wp
         !
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF( tmask(ji,jj,jk) > 0._wp) THEN
                     z3d (ji,jj,jk) = trn(ji,jj,jk,jp_c14)
                     zz3d(ji,jj,jk) = LOG( z3d(ji,jj,jk) )
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         zres(:,:) = z3d(:,:,1)

         ! Reservoir age [yr]
         z2d(:,:) =0._wp
         jk = 1
         DO jj = 1, jpj
            DO ji = 1, jpi
               ztemp = zres(ji,jj) / c14sbc(ji,jj)
               IF( ztemp > 0._wp .AND. tmask(ji,jj,jk) > 0._wp ) z2d(ji,jj) = LOG( ztemp )
            ENDDO
         ENDDO
         !
         z3d(:,:,:) = 1.d03 * ( z3d(:,:,:) - 1._wp )
         CALL iom_put( "DeltaC14" , z3d(:,:,:)  )  ! Delta C14 [permil]
         CALL iom_put( "C14Age"   , zage * zz3d(:,:,:) )            !  Radiocarbon age [yr]

         CALL iom_put( "qtr_c14", rsiyea * qtr_c14(:,:)  )            !  Radiocarbon surf flux [./m2/yr]
         CALL iom_put( "qint_c14" , qint_c14  )                       ! cumulative flux [./m2]
         CALL iom_put( "RAge" , zage * z2d(:,:) )                     ! Reservoir age [yr]
         !
         DEALLOCATE( z2d, zres, z3d, zz3d )
         !
      ENDIF
      !
      !  0-D fields
      !
      CALL iom_put( "AtmCO2", co2sbc )  !     global atmospheric CO2 [ppm]
    
      IF( iom_use("AtmC14") ) THEN
         zarea = glob_sum( 'trcwri_c14', e1e2t(:,:) )           ! global ocean surface
         ztemp = glob_sum( 'trcwri_c14', c14sbc(:,:) * e1e2t(:,:) )
         ztemp = ( ztemp / zarea - 1._wp ) * 1000._wp
         CALL iom_put( "AtmC14" , ztemp )   ! Global atmospheric DeltaC14 [permil]
      ENDIF
      IF( iom_use("K_C14") ) THEN
         ztemp = glob_sum ( 'trcwri_c14', exch_c14(:,:) * e1e2t(:,:) )
         ztemp = rsiyea * ztemp / zarea
         CALL iom_put( "K_C14" , ztemp )   ! global mean exchange velocity for C14/C ratio [m/yr]
      ENDIF
      IF( iom_use("K_CO2") ) THEN
         zarea = glob_sum( 'trcwri_c14', e1e2t(:,:) )           ! global ocean surface
         ztemp = glob_sum ( 'trcwri_c14', exch_co2(:,:) * e1e2t(:,:) )
         ztemp = 360000._wp * ztemp / zarea       ! cm/h units: directly comparable with literature
         CALL iom_put( "K_CO2", ztemp )  !  global mean CO2 piston velocity [cm/hr]
      ENDIF
      IF( iom_use("C14Inv") ) THEN
         ztemp = glob_sum( 'trcwri_c14', trn(:,:,:,jp_c14) * cvol(:,:,:) )
         ztemp = atomc14 * xdicsur * ztemp
         CALL iom_put( "C14Inv", ztemp )  !  Radiocarbon ocean inventory [10^26 atoms]
      END IF
      !
   END SUBROUTINE trc_wri_c14

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No C14 tracer
   !!----------------------------------------------------------------------
   PUBLIC trc_wri_c14
CONTAINS
   SUBROUTINE trc_wri_c14                     ! Empty routine  
   END SUBROUTINE trc_wri_c14
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcwri_c14.F90 10425 2018-12-19 21:54:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri_c14
