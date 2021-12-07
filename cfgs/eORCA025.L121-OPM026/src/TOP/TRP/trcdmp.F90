MODULE trcdmp
   !!======================================================================
   !!                       ***  MODULE  trcdmp  ***
   !! Ocean physics: internal restoring trend on passive tracers
   !!======================================================================
   !! History :  OPA  !  1991-03  (O. Marti, G. Madec)  Original code
   !!                 !  1996-01  (G. Madec) statement function for e3
   !!                 !  1997-05  (H. Loukos)  adapted for passive tracers
   !!    NEMO    9.0  !  2004-03  (C. Ethe)    free form + modules
   !!            3.2  !  2007-02  (C. Deltel)  Diagnose ML trends for passive tracers
   !!            3.3  !  2010-06  (C. Ethe, G. Madec) merge TRA-TRC 
   !!----------------------------------------------------------------------
#if  defined key_top 
   !!----------------------------------------------------------------------
   !!   trc_dmp      : update the tracer trend with the internal damping
   !!   trc_dmp_init : initialization, namlist read, parameters control
   !!----------------------------------------------------------------------
   USE oce_trc         ! ocean dynamics and tracers variables
   USE trc             ! ocean passive tracers variables
   USE trcdta
   USE tradmp
   USE trdtra
   USE trd_oce
   !
   USE iom
   USE prtctl_trc      ! Print control for debbuging

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_dmp      
   PUBLIC trc_dmp_clo   
   PUBLIC trc_dmp_alloc  
   PUBLIC trc_dmp_ini    

   INTEGER            , PUBLIC ::   nn_zdmp_tr    !: = 0/1/2 flag for damping in the mixed layer
   CHARACTER(LEN=200) , PUBLIC ::   cn_resto_tr   !: File containing restoration coefficient

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   restotr   ! restoring coeff. on tracers (s-1)

   INTEGER, PARAMETER         ::   npncts = 8       ! number of closed sea
   INTEGER, DIMENSION(npncts) ::   nctsi1, nctsj1   ! south-west closed sea limits (i,j)
   INTEGER, DIMENSION(npncts) ::   nctsi2, nctsj2   ! north-east closed sea limits (i,j)

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcdmp.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_dmp_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( restotr(jpi,jpj,jpk) , STAT=trc_dmp_alloc )
      !
      IF( trc_dmp_alloc /= 0 )   CALL ctl_warn('trc_dmp_alloc: failed to allocate array')
      !
   END FUNCTION trc_dmp_alloc


   SUBROUTINE trc_dmp( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp  ***
      !!                  
      !! ** Purpose :   Compute the passive tracer trend due to a newtonian damping
      !!      of the tracer field towards given data field and add it to the
      !!      general tracer trends.
      !!
      !! ** Method  :   Newtonian damping towards trdta computed 
      !!      and add to the general tracer trends:
      !!                     trn = tra + restotr * (trdta - trb)
      !!         The trend is computed either throughout the water column
      !!      (nlmdmptr=0) or in area of weak vertical mixing (nlmdmptr=1) or
      !!      below the well mixed layer (nlmdmptr=2)
      !!
      !! ** Action  : - update the tracer trends tra with the newtonian 
      !!                damping trends.
      !!              - save the trends ('key_trdmxl_trc')
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER ::   ji, jj, jk, jn, jl   ! dummy loop indices
      CHARACTER (len=22) ::   charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrtrd
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrcdta   ! 3D  workspace
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_dmp')
      !
      IF( l_trdtrc )   ALLOCATE( ztrtrd(jpi,jpj,jpk) )   ! temporary save of trends
      !
      IF( nb_trcdta > 0 ) THEN  ! Initialisation of tracer from a file that may also be used for damping
         !
         ALLOCATE( ztrcdta(jpi,jpj,jpk) )    ! Memory allocation
         !                                                          ! ===========
         DO jn = 1, jptra                                           ! tracer loop
            !                                                       ! ===========
            IF( l_trdtrc ) ztrtrd(:,:,:) = tra(:,:,:,jn)    ! save trends 
            !
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
               !
               jl = n_trc_index(jn) 
               CALL trc_dta( kt, sf_trcdta(jl), rf_trfac(jl), ztrcdta )   ! read tracer data at nit000
               !
               SELECT CASE ( nn_zdmp_tr )
               !
               CASE( 0 )                !==  newtonian damping throughout the water column  ==!
                  DO jk = 1, jpkm1
                     DO jj = 2, jpjm1
                        DO ji = fs_2, fs_jpim1   ! vector opt.
                           tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + restotr(ji,jj,jk) * ( ztrcdta(ji,jj,jk) - trb(ji,jj,jk,jn) )
                        END DO
                     END DO
                  END DO
                  !
               CASE ( 1 )                !==  no damping in the turbocline (avt > 5 cm2/s)  ==!
                  DO jk = 1, jpkm1
                     DO jj = 2, jpjm1
                        DO ji = fs_2, fs_jpim1   ! vector opt.
                           IF( avt(ji,jj,jk) <= avt_c )  THEN 
                              tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + restotr(ji,jj,jk) * ( ztrcdta(ji,jj,jk) - trb(ji,jj,jk,jn) )
                           ENDIF
                        END DO
                     END DO
                  END DO
                  !
               CASE ( 2 )               !==  no damping in the mixed layer   ==! 
                  DO jk = 1, jpkm1
                     DO jj = 2, jpjm1
                        DO ji = fs_2, fs_jpim1   ! vector opt.
                           IF( gdept_n(ji,jj,jk) >= hmlp (ji,jj) ) THEN
                              tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + restotr(ji,jj,jk) * ( ztrcdta(ji,jj,jk) - trb(ji,jj,jk,jn) )
                           END IF
                        END DO
                     END DO
                  END DO
                  !  
               END SELECT
               ! 
            ENDIF
            !
            IF( l_trdtrc ) THEN
               ztrtrd(:,:,:) = tra(:,:,:,jn) -  ztrtrd(:,:,:)
               CALL trd_tra( kt, 'TRC', jn, jptra_dmp, ztrtrd )
            END IF
            !                                                       ! ===========
         END DO                                                     ! tracer loop
         !                                                          ! ===========
         DEALLOCATE( ztrcdta )
      ENDIF
      !
      IF( l_trdtrc )  DEALLOCATE( ztrtrd )
      !                                          ! print mean trends (used for debugging)
      IF( ln_ctl ) THEN
         WRITE(charout, FMT="('dmp ')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_dmp')
      !
   END SUBROUTINE trc_dmp


   SUBROUTINE trc_dmp_ini
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_dmp_ini  ***
      !! 
      !! ** Purpose :   Initialization for the newtonian damping 
      !!
      !! ** Method  :   read the nammbf namelist and check the parameters
      !!              called by trc_dmp at the first timestep (nittrc000)
      !!----------------------------------------------------------------------
      INTEGER ::   ios, imask  ! local integers
      !!
      NAMELIST/namtrc_dmp/ nn_zdmp_tr , cn_resto_tr
      !!----------------------------------------------------------------------
      !
      REWIND( numnat_ref )              ! Namelist namtrc_dmp in reference namelist : Passive tracers newtonian damping
      READ  ( numnat_ref, namtrc_dmp, IOSTAT = ios, ERR = 909)
909   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namtrc_dmp in reference namelist' )
      REWIND( numnat_cfg )              ! Namelist namtrc_dmp in configuration namelist : Passive tracers newtonian damping
      READ  ( numnat_cfg, namtrc_dmp, IOSTAT = ios, ERR = 910)
910   IF( ios >  0 )   CALL ctl_nam ( ios , 'namtrc_dmp in configuration namelist' )
      IF(lwm) WRITE ( numont, namtrc_dmp )

      IF(lwp) THEN                       ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_dmp : Passive tracers newtonian damping'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namtrc_dmp : set damping parameter'
         WRITE(numout,*) '      mixed layer damping option     nn_zdmp_tr  = ', nn_zdmp_tr, '(zoom: forced to 0)'
         WRITE(numout,*) '      Restoration coeff file         cn_resto_tr = ', cn_resto_tr
      ENDIF
      !                          ! Allocate arrays
      IF( trc_dmp_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_dmp_ini: unable to allocate arrays' )
      !
      SELECT CASE ( nn_zdmp_tr )
      CASE ( 0 )   ;   IF(lwp) WRITE(numout,*) '      ===>>   tracer damping throughout the water column'
      CASE ( 1 )   ;   IF(lwp) WRITE(numout,*) '      ===>>   no tracer damping in the turbocline (avt > 5 cm2/s)'
      CASE ( 2 )   ;   IF(lwp) WRITE(numout,*) '      ===>>   no tracer damping in the mixed layer'
      CASE DEFAULT
         WRITE(ctmp1,*) 'bad flag value for nn_zdmp_tr = ', nn_zdmp_tr
         CALL ctl_stop(ctmp1)
      END SELECT

      IF( .NOT.lk_c1d ) THEN
         IF( .NOT.ln_tradmp )   &
            &   CALL ctl_stop( 'passive tracer damping need ln_tradmp to compute damping coef.' )
         !
         !                          ! Read damping coefficients from file
         !Read in mask from file
         CALL iom_open ( cn_resto_tr, imask)
         CALL iom_get  ( imask, jpdom_autoglo, 'resto', restotr)
         CALL iom_close( imask )
         !
      ENDIF
      !
   END SUBROUTINE trc_dmp_ini


   SUBROUTINE trc_dmp_clo( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trc_dmp_clo  ***
      !!
      !! ** Purpose :   Closed sea domain initialization
      !!
      !! ** Method  :   if a closed sea is located only in a model grid point
      !!                we restore to initial data
      !!
      !! ** Action  :   nctsi1(), nctsj1() : south-west closed sea limits (i,j)
      !!                nctsi2(), nctsj2() : north-east Closed sea limits (i,j)
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      !
      INTEGER :: ji , jj, jk, jn, jl, jc                    ! dummy loop indicesa
      INTEGER :: isrow                                      ! local index
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ztrcdta       ! 3D  workspace
      !!----------------------------------------------------------------------

      IF( kt == nit000 ) THEN
         ! initial values
         nctsi1(:) = 1  ;  nctsi2(:) = 1
         nctsj1(:) = 1  ;  nctsj2(:) = 1

         ! set the closed seas (in data domain indices)
         ! -------------------

         IF( cn_cfg == "orca" .OR. cn_cfg == "ORCA") THEN
            !
            SELECT CASE ( nn_cfg )
            !                                           ! =======================
            CASE ( 1 )                                  ! eORCA_R1 configuration
            !                                           ! =======================
            isrow = 332 - jpjglo
            !
            nctsi1(1)   = 333  ; nctsj1(1)   = 243 - isrow   ! Caspian Sea
            nctsi2(1)   = 342  ; nctsj2(1)   = 274 - isrow
            !                                        
            nctsi1(2)   = 198  ; nctsj1(2)   = 258 - isrow   ! Lake Superior
            nctsi2(2)   = 204  ; nctsj2(2)   = 262 - isrow
            !                                         
            nctsi1(3)   = 201  ; nctsj1(3)   = 250 - isrow   ! Lake Michigan
            nctsi2(3)   = 203  ; nctsj2(3)   = 256 - isrow
            !                                        
            nctsi1(4)   = 204  ; nctsj1(4)   = 252 - isrow   ! Lake Huron
            nctsi2(4)   = 209  ; nctsj2(4)   = 256 - isrow
            !                                        
            nctsi1(5)   = 206  ; nctsj1(5)   = 249 - isrow   ! Lake Erie
            nctsi2(5)   = 209  ; nctsj2(5)   = 251 - isrow
            !                                        
            nctsi1(6)   = 210  ; nctsj1(6)   = 252 - isrow   ! Lake Ontario
            nctsi2(6)   = 212  ; nctsj2(6)   = 252 - isrow
            !                                        
            nctsi1(7)   = 321  ; nctsj1(7)   = 180 - isrow   ! Victoria Lake
            nctsi2(7)   = 322  ; nctsj2(7)   = 189 - isrow
            !                                        
            nctsi1(8)   = 297  ; nctsj1(8)   = 270 - isrow   ! Baltic Sea
            nctsi2(8)   = 308  ; nctsj2(8)   = 293 - isrow
            !                                        
            !                                           ! =======================
            CASE ( 2 )                                  !  ORCA_R2 configuration
               !                                        ! =======================
               !                                      
               nctsi1(1)   =  11  ;  nctsj1(1)   = 103       ! Caspian Sea
               nctsi2(1)   =  17  ;  nctsj2(1)   = 112
               !                                     
               nctsi1(2)   =  97  ;  nctsj1(2)   = 107       ! Great North American Lakes
               nctsi2(2)   = 103  ;  nctsj2(2)   = 111
               !                                     
               nctsi1(3)   = 174  ;  nctsj1(3)   = 107       ! Black Sea 1 : west part of the Black Sea
               nctsi2(3)   = 181  ;  nctsj2(3)   = 112
              !                                      
               nctsi1(4)   =   2  ;  nctsj1(4)   = 107      ! Black Sea 2 : est part of the Black Sea
               nctsi2(4)   =   6  ;  nctsj2(4)   = 112
               !                                     
               nctsi1(5)   =  145 ;  nctsj1(5)   = 116       ! Baltic Sea
               nctsi2(5)   =  150 ;  nctsj2(5)   = 126
               !                                        ! =======================
            CASE ( 4 )                                  !  ORCA_R4 configuration
               !                                        ! =======================
               !                                   
               nctsi1(1)   =  4  ;  nctsj1(1)   = 53         ! Caspian Sea
               nctsi2(1)   =  4  ;  nctsj2(1)   = 56
               !                                   
               nctsi1(2)   = 49  ;  nctsj1(2)   = 55         ! Great North American Lakes
               nctsi2(2)   = 51  ;  nctsj2(2)   = 56
               !                                   
               nctsi1(3)   = 88  ;  nctsj1(3)   = 55         ! Black Sea
               nctsi2(3)   = 91  ;  nctsj2(3)   = 56
               !                                   
               nctsi1(4)   = 75  ;  nctsj1(4)   = 59         ! Baltic Sea
               nctsi2(4)   = 76  ;  nctsj2(4)   = 61
               !                                        ! =======================
            CASE ( 025 )                                ! ORCA_R025 configuration
               !                                        ! =======================
               !                                   
               nctsi1(1)   = 1330 ; nctsj1(1)   = 645        ! Caspian + Aral sea
               nctsi2(1)   = 1400 ; nctsj2(1)   = 795
               !                                    
               nctsi1(2)   = 1284 ; nctsj1(2)   = 722        ! Azov Sea
               nctsi2(2)   = 1304 ; nctsj2(2)   = 747
               !
            END SELECT
            !
         ENDIF
         !
         ! convert the position in local domain indices
         ! --------------------------------------------
         DO jc = 1, npncts
            nctsi1(jc)   = mi0( nctsi1(jc) )
            nctsj1(jc)   = mj0( nctsj1(jc) )
            !
            nctsi2(jc)   = mi1( nctsi2(jc) )
            nctsj2(jc)   = mj1( nctsj2(jc) )
         END DO
         !
      ENDIF

      ! Restore close seas values to initial data
      IF( ln_trcdta .AND. nb_trcdta > 0 )  THEN   ! Initialisation of tracer from a file that may also be used for damping
         !
         IF(lwp)  WRITE(numout,*)
         IF(lwp)  WRITE(numout,*) ' trc_dmp_clo : Restoring of nutrients on close seas at time-step kt = ', kt
         IF(lwp)  WRITE(numout,*)
         !
         ALLOCATE( ztrcdta(jpi,jpj,jpk) )   ! Memory allocation
         !
         DO jn = 1, jptra
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
                jl = n_trc_index(jn)
                CALL trc_dta( kt, sf_trcdta(jl), rf_trfac(jl), ztrcdta )   ! read tracer data at nit000
                DO jc = 1, npncts
                   DO jk = 1, jpkm1
                      DO jj = nctsj1(jc), nctsj2(jc)
                         DO ji = nctsi1(jc), nctsi2(jc)
                            trn(ji,jj,jk,jn) = ztrcdta(ji,jj,jk)
                            trb(ji,jj,jk,jn) = trn(ji,jj,jk,jn)
                         END DO
                      END DO
                   END DO
                END DO
             ENDIF
          END DO
          DEALLOCATE( ztrcdta )
      ENDIF
      !
   END SUBROUTINE trc_dmp_clo
 
#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_dmp( kt )        ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'trc_dmp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_dmp
#endif

   !!======================================================================
END MODULE trcdmp
