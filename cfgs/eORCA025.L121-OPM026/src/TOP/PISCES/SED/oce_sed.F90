MODULE oce_sed
   !!======================================================================
   !!                        ***  sed  ***
   !! Sediment :   set sediment global variables
   !!======================================================================
   !! History :
   !!        !  06-12  (C. Ethe)  Orignal
   !!----------------------------------------------------------------------
   USE par_sed
   USE timing
   USE par_trc

   USE dom_oce , ONLY :   glamt     =>   glamt          !: longitude of t-point (degre)
   USE dom_oce , ONLY :   gphit     =>   gphit          !: latitude  of t-point (degre)
   USE dom_oce , ONLY :   e3t_n     =>   e3t_n          !: latitude  of t-point (degre)
   USE dom_oce , ONLY :   e3t_1d    =>   e3t_1d         !: reference depth of t-points (m)
   USE dom_oce , ONLY :   gdepw_0   =>   gdepw_0        !: reference depth of t-points (m)
   USE dom_oce , ONLY :   mbkt      =>   mbkt           !: vertical index of the bottom last T- ocean level
   USE dom_oce , ONLY :   tmask     =>   tmask          !: land/ocean mask at t-points
   USE dom_oce , ONLY :   rdt       =>   rdt            !: time step for the dynamics
   USE dom_oce , ONLY :   nyear     =>   nyear          !: Current year
   USE dom_oce , ONLY :   ndastp    =>   ndastp         !: time step date in year/month/day aammjj
   USE dom_oce , ONLY :   adatrj    =>   adatrj         !: number of elapsed days since the begining of the run
   USE trc     , ONLY :  nittrc000  =>   nittrc000
   !                                !: it is the accumulated duration of previous runs
   !                                !: that may have been run with different time steps.

   USE oce     , ONLY :  tsn        =>   tsn             !: pot. temperature (celsius) and salinity (psu)
   USE trc     , ONLY :  trb        =>   trb             !: pot. temperature (celsius) and salinity (psu)

   USE sms_pisces, ONLY : wsbio4    =>   wsbio4          !: sinking flux for POC
   USE sms_pisces, ONLY : wsbio3    =>   wsbio3          !: sinking flux for GOC
   USE sms_pisces, ONLY : wsbio2    =>   wsbio2           !: sinking flux for calcite
   USE sms_pisces, ONLY : wsbio     =>   wsbio           !: sinking flux for calcite
   USE sms_pisces, ONLY : ln_p5z    =>   ln_p5z          !: PISCES-QUOTA flag
   USE p4zche, ONLY     : akb3      =>   akb3            !: Chemical constants  
   USE sms_pisces, ONLY : ak13      =>   ak13            !: Chemical constants  
   USE sms_pisces, ONLY : ak23      =>   ak23            !: Chemical constants  
   USE p4zche, ONLY     : akw3      =>   akw3            !: Chemical constants  
   USE sms_pisces, ONLY : aksp      =>   aksp            !: Chemical constants  
   USE p4zche, ONLY     : borat     =>   borat           !: Chemical constants ( borat ) 
   USE p4zche, ONLY     : ak1p3     =>   ak1p3           !: Chemical constants  
   USE p4zche, ONLY     : ak2p3     =>   ak2p3           !: Chemical constants  
   USE p4zche, ONLY     : ak3p3     =>   ak3p3           !: Chemical constants  
   USE p4zche, ONLY     : aksi3     =>   aksi3           !: Chemical constants  
   USE p4zche, ONLY     : aks3      =>   aks3            !: Chemical constants  
   USE p4zche, ONLY     : akf3      =>   akf3            !: Chemical constants  
   USE p4zche, ONLY     : fluorid   =>   fluorid         !: Chemical constants  
   USE p4zche, ONLY     : sulfat    =>   sulfat          !: Chemical constants  
   USE p4zche, ONLY     : sio3eq    =>   sio3eq          !: Chemical constants  
   USE p4zsbc, ONLY     : dust      =>   dust
   USE trc       , ONLY : r2dttrc   =>   r2dttrc

END MODULE oce_sed


