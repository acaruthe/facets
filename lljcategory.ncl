;calculates wind related from 3D variables and writes them to a file
;for entire domain and 3 hrly
;calculates 5 LLJ variables
;calculates a sfc->1-km, 3km, 5km, and 6km shear too
;uses  U, V, UAS, VAS, Z (must have this calculated already)
;set up for 28 vertical levels!
;DOING THIS MONTHLY AND OUTPUTTING MONTHLY FILES
;STITCH TOGETHER LATER WITH NCO


 load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_csm.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

;;;;;;;;;;;;;;;;;; NOTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This NCL file calls a fortran routine 
; NCL is not great with "loops" which is why fortran is used here
;
; Replace this with the location of your llj.so"
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 external UTIL17 "/glade/work/acaruth/facets/lljonlyseptember.so" ;change path here !
;----------------------------------------------------------------------

;read in regcm variables 

;;;;;;;;;;NOTE;;;;;;;;;;;
;
; This is where you put the path to the file
;
;;;;;;;;;;;;;


f1 = addfile("infile.nc", "r") ; this is where you put the path to the data file were using  


;;;;;;;;;;;;;;; NOTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Here we read in all the variables we need 
;
; Time, U/V winds, z (height, which in this model is "sigma")
; Latitude, longitude are each 2 dimensions -> this is confusing!  
;    Your data may by just 1 dimension
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   time = f1->time ; time 
   nt = dimsizes(time)   
 
   uvar = f1->ua ; u winds
   vvar = f1->va ; v wind  
   zvar = f1->sigma ; height coordinate 

   lat2d = f1->xlat(:,:)  ;latitude 
   lon2d = f1->xlon(:,:)  ; longitude 

;;; here we define each index of the variable uvar(time,lev,lat,lon)
   uvar!0      = "time"
   uvar!1        = "lev"
   uvar!2      = "lat"
   uvar!3      = "lon"


   uvar@lat2d = lat2d
   uvar@lon2d = lon2d


   vvar!0      = "time"
   vvar!1      = "level"
   vvar!2      = "lat"
   vvar!3      = "lon"

   vvar@lat2d = lat2d
   vvar@lon2d = lon2d

   nlev = dimsizes(zvar) ; number of levels 

   xlon = f1->xlon ;another longitude variable 
   xlat = f1->xlat ; another latitude variable 

   xc = f1->jx ; this is the x,y of the latitude/longitude 
   yc = f1->iy ; so lat(jx,iy), lon(jx,iy)

   nx = dimsizes(xc) ;size of xc
   ny = dimsizes(yc) ; size of yc


;;; here we define file values for missing data 
   uvar@_FillValue = -999
   vvar@_FillValue = -999
   zvar@_FillValue = -999


;;; this takes the coordinates from the .nc file 
   pname = uvar@grid_mapping
       proj = f1->$pname$
       print(proj)

;;;;;;;;;;;;; NOTE ;;;;;;;;;;;;;;;; IMPORTANT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; I use regCM data, which has all the surface data in 1 file
;  and all the upper atmosphere data in another
;
; you can probably delete this part, if your data files have surface/upper air in 1!
;
;;;;;;;;;;;;;;;;;;

f2 = addfile("sfcinfile_september.nc", "r") ;regcm has separate sfc files


;;;;;;;; NOTE ;;;;;;;;
;
; this is for my work, you can ignore :) 
;
; ERA-I SRF has hourly output,
; MPI SRF has hourly output 
; GFDL SRF has 3 hourly output
; hadgem SRF has 3 hourly
; all ATM is 6 hourly  
;
;atm files begin at month 01 06Z
;sfc begin at month 01 00z, so bring in every 6th time starting at 6z
;
;;;;;;;;;;;;;;;;


   usvar = f2->uas(1::6,0,:,:) ; read in only the data ever 6 hours
   vsvar = f2->vas(1::6,0,:,:) ; 1::6 for hourly, 1::2 for 3 hours, everything for 6 hours 

   lat2d = f2->xlat(:,:) ; latitude 
   lon2d = f2->xlon(:,:) l longitude

   usvar@lat2d = lat2d ;make 2d 
   usvar@lon2d = lon2d 
  
   vsvar@lat2d = lat2d ; make 2d 
   vsvar@lon2d = lon2d


; set index as usvar(time, lat, lon) 
   usvar!0      = "time"
   usvar!1      = "lat"
   usvar!2      = "lon"

   vsvar!0      = "time"
   vsvar!1      = "lat"
   vsvar!2      = "lon"


;set fill value 
   usvar@_FillValue = -999
   vsvar@_FillValue = -999


;;;;;;;;;;;;;; NOTE ;;;;;;;;; IMPORTANT
;
; This is the end of the surface file
;  if you deleted lines for the surface file, you should stop here!
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; CHECK IF VARIABLES WERE SUCCESSFULLY READ IN
print("brought in variables")

;;;;;;;;;;;;;;;;;;;; NOTE ;;;;;;;;;;;;;;
;
; this next section is some programed calendar information 
; for more information, search "NCL cd_calendar" 
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;


   date = cd_calendar(time,-3)
     ;option 3 YYYYMMDDHH
   yyyymm_all = cd_calendar(time,-1)
      ; YYYYMM

   datestart = cd_calendar(time(0),0)
      ;array of years, months, days, hours, minutes, seconds
   yyyystart = datestart(0,0)
      ; 0 is years 
   mmstart   = datestart(0,1)
      ; 1 is months 
   dateend = cd_calendar(time(nt-1),0)

print(datestart)
print(dateend)

   yyyymmstart = cd_calendar(time(0),-1)
     ; option 1 is YYYYMM
   yyyymmend   = cd_calendar(time(nt-1),-1)
     ; option 1 is YYYYMM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Now we get into the funs stuff!! 

;COMPUTE SFC-XKM SHEAR & LLJ STUFF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; LLJ
;
; Create a new variable that is the LLJ
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     msp  = usvar ; llj wind speed
     mwd  = usvar ; llj wind direction 
     mshr   = usvar ; llj wind shear
     mhgt   = usvar ; llj height 
     lljcat = floattoint(usvar) ;which LLJ category is it? 1, 2,3 
     lljcat1 = usvar ; 1 for LLJ, 0 for no 
     lljcat2 = usvar ; 1 for LLJ, 0 for no
     lljcat3 = usvar ; 1 for LLJ, 0 for no

;printVarSummary(lljcat1) ; test


;;;;;;;;;
;
; set fill values as above 
;
;;;;;;;;;;;;;;;

     msp  = msp@_FillValue
     mwd  = mwd@_FillValue
     mshr   = mshr@_FillValue
     mhgt   = mhgt@_FillValue
     lljcat = lljcat@_FillValue
     lljcat1 = lljcat1@_FillValue
     lljcat2 = lljcat2@_FillValue
     lljcat3 = lljcat3@_FillValue

;;;;;;;;;
;
; define the characteristics 
;
;;;;;;;;;;;;;;;;;;;;;

     msp@long_name = "Maximum LLJ Speed"
     mwd@long_name = "Wind Direction of Max LLJ Speed"
     mshr@long_name = "Maximum Shear above the LLJ Speed Max"
     mhgt@long_name = "Height of LLJ Speed Max"
     lljcat@long_name = "LLJ Category"
     lljcat1@long_name = "Frequency LLJ Category 1"
     lljcat2@long_name = "Frequency LLJ Category 2"
     lljcat3@long_name = "Frequency LLJ Category 3"

     msp@standard_name = "Maximum LLJ Speed"
     mwd@standard_name = "Wind Direction of Max LLJ Speed"
     mshr@standard_name = "Maximum Shear above the LLJ Speed Max"
     mhgt@standard_name = "Height of LLJ Speed Max"
     lljcat@standard_name = "LLJ Category"
     lljcat1@standard_name = "Frequency LLJ Category 1"
     lljcat2@standard_name = "Frequency LLJ Category 2"
     lljcat3@standard_name = "Frequency LLJ Category 3"

     msp@units = "m s-1"
     mwd@units = "deg"
     mshr@units = "m s-1"
     mhgt@units = "m"
     lljcat@units = "none"
     lljcat1@units = "none"
     lljcat2@units = "none" 
     lljcat3@units = "none"



;;;;;;; 
;
; this step here is to copy the mapping coordinates to the new variable, LLJ 
;
; floattoint doesn't copy anything, and copy_VarMeta won't work because fillval is float,
; so do stuff here
;
;;;;;;;;;;;;;;;;;;;;

     copy_VarCoords(usvar,lljcat)
     lljcat@grid_mapping = usvar@grid_mapping
     lljcat@coordinates = usvar@coordinates

     copy_VarCoords(usvar,lljcat1)
     lljcat1@grid_mapping = usvar@grid_mapping
     lljcat1@coordinates = usvar@coordinates
    
     copy_VarCoords(usvar,lljcat2)
     lljcat2@grid_mapping = usvar@grid_mapping
     lljcat2@coordinates = usvar@coordinates

     copy_VarCoords(usvar,lljcat3)
     lljcat3@grid_mapping = usvar@grid_mapping
     lljcat3@coordinates = usvar@coordinates

;;;;;;;;;;;;;
;
;
;calculate wind speed and dir for whole atmos
;
;
; this converts radians to degrees, if your output already uses degrees, skip!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;


 r2d = 45.0/atan(1.0)           ;conversion factor (radians to degrees) 1 radian is 57.3 degrees


     windspeed = (uvar*uvar + vvar*vvar)^0.5
     winddir   = atan2(uvar,vvar) * r2d + 180. ;give winddir in degrees (arctan finds inverse tangent of the u,v winds) 

;;;;; set the variable indexes here msp(time,yc,xc)

     msp!0 = "time"
     msp!1 = "yc"
     msp!2 = "xc"

     mwd!0 = "time"
     mwd!1 = "yc"
     mwd!2 = "xc"
 
     mshr!0 = "time"
     mshr!1 = "yc"
     mshr!2 = "xc"

     mhgt!0 = "time"
     mhgt!1 = "yc"
     mhgt!2 = "xc"

     lljcat!0 = "time"
     lljcat!1 = "yc"
     lljcat!2 = "xc"

     lljcat1!0 = "time"
     lljcat1!1 = "yc"
     lljcat1!2 = "xc"
 
     lljcat2!0 = "time"
     lljcat2!1 = "yc"
     lljcat2!2 = "xc"

     lljcat3!0 = "time"
     lljcat3!1 = "yc"
     lljcat3!2 = "xc" 

     windspeed!0 = "time"
     windspeed!1 = "lev"
     windspeed!2 = "yc"
     windspeed!3 = "xc"
 
     winddir!0 = "time"
     winddir!1 = "lev"
     winddir!2 = "yc"
     winddir!3 = "xc"

     xlat!0 = "yc"
     xlat!1 = "xc"

     xlon!0 = "yc"
     xlon!1 = "xc"

; ;;;;;;;;;;;;;;;;;;
;
; this step is because we have 2d lat/lon
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


     msp@lat2d = xlat
     msp@lon2d = xlon


     mwd@lat2d = xlat
     mwd@lon2d = xlon

     mshr@lat2d = xlat
     mshr@lon2d = xlon

     lljcat@lat2d = xlat
     lljcat@lon2d = xlon

     lljcat1@lat2d = xlat
     lljcat1@lon2d = xlon

     lljcat2@lat2d = xlat
     lljcat2@lon2d = xlon

     lljcat3@lat2d = xlat
     lljcat3@lon2d = xlon

     windspeed@lat2d = xlat
     windspeed@lon2d = xlon
 
     winddir@lat2d = xlat
     winddir@lon2d = xlon

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; congrats, if you've made it this far, you have read in all data and created all your new variables
;
; now its time to use fortran to calculate a LLJ !
;

;;;;;; LLJ time!;;;;;;;


print("attempting subroutine now!") ; ths gives you a heads up that fortran is being used here! 



;;;;; ;;;;;
;
;
;for whatever reason, the dimensions of the vars need to be reversed to be read into fortran
;coding is weird. 
;instead of time, lev, yc, xc,
;xc, yc, lev, time
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;
; this is telling NCL to go to the fortran code named "llj.so" and use windspeed, winddir, 
;  zvar, mwd, mshr, mght, lljcat, lljcat1, lljcat2, lljcat3, nt, nlev, ny, nx in the 
; calulations 
;
;
     UTIL17::lljonly(windspeed(xc|:,yc|:,lev|:,time|:),winddir(xc|:,yc|:,lev|:,time|:), \
                    zvar(kz|:),msp(xc|:,yc|:,time|:),                \
                    mwd(xc|:,yc|:,time|:),mshr(xc|:,yc|:,time|:),                      \
                    mhgt(xc|:,yc|:,time|:),lljcat(xc|:,yc|:,time|:), lljcat1(xc|:,yc|:,time|:),\
                     lljcat2(xc|:,yc|:,time|:), lljcat3(xc|:,yc|:,time|:),         \
                    nt,nlev,ny,nx,zvar@_FillValue)               
     



;----------------------------------------------------------------------
;NOW OUTPUT THIS ARRAY 
;----------------------------------------------------------------------
;OUTPUT NEW FILES
; 
; these files will contain information about LLJs calculated above 


     foutname1 = "llj.nc" ; NAME OF FILE 
      dirout = "/glade/scratch/acaruth/facets/" ;directory location where new file will be stored
     fout1 = dirout+foutname1 ; directory and filename

     system("rm "+fout1) ; this removes a file of the same name if it exists 

     ncdf1 = addfile(fout1,"c") ;creating new netcdf file 

;ASSIGN FILE ATTRIBUTES
; give it all the attributes !

     fAtt = True
 
      fAtt@source_file = "infile.nc" ;name of original data file ! 
      fAtt@creation_date = systemfunc("date")  ; date that your creating file 
      fAtt@Conventions = "None" 
     
     fileattdef(ncdf1,fAtt) ;assign these attributes to the new netcdf file were making 

;PREDEFINE COORDINATE INFO

     dimNames = (/"time","yc","xc"/) ;dimensions used
     dimSizes = (/nt,ny,nx/) ;dimension sizes 
     dimUnlim = (/True,False,False/) 
    
     filedimdef(ncdf1,dimNames,dimSizes,dimUnlim) ;assign dimension information to netcdf file 
    
     filevardef    (ncdf1, "time", typeof(time), "time") ;want our "time" variable to be similar as the original "time"
     filevarattdef (ncdf1, "time", time) ; assign time to file 
    
     filevardef    (ncdf1, "xc", typeof(xc), "xc")
     filevarattdef (ncdf1, "xc", xc) ;assign xc to file 
  
     filevardef    (ncdf1, "yc", typeof(yc), "yc")
     filevarattdef (ncdf1, "yc", yc) ;assign yc to file 

     filevardef    (ncdf1, "xlat", typeof(xlat), (/"yc","xc"/))
     filevarattdef (ncdf1, "xlat", xlat) ;assign xlat 
    
     filevardef    (ncdf1, "xlon", typeof(xlon), (/"yc","xc"/))
     filevarattdef (ncdf1, "xlon", xlon) ;assign xlon 

     filevardef    (ncdf1, "lat2d", typeof(lat2d), (/"yc","xc"/) )
     filevarattdef (ncdf1, "lat2d", lat2d) ;assign lat2d
    
     filevardef    (ncdf1, "lon2d", typeof(lon2d), (/"yc","xc"/) )
     filevarattdef (ncdf1, "lon2d", lon2d) ; assign lon2d
    
     filevardef    (ncdf1, "msp", typeof(msp), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "msp", msp ) ;assign msp 
  
     filevardef    (ncdf1, "mwd", typeof(mwd), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "mwd", mwd ) ; assign mwd
  
     filevardef    (ncdf1, "mshr", typeof(mshr), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "mshr", mshr ) ;assign mshr
  
     filevardef    (ncdf1, "mhgt", typeof(mhgt), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "mhgt", mhgt ) ;assign mhgt
  
     filevardef    (ncdf1, "lljcat", typeof(lljcat), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "lljcat", lljcat ) ;lljcat
  
     filevardef    (ncdf1, "lljcat1", typeof(lljcat1), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "lljcat1", lljcat1 ) ;llcat1
  
     filevardef    (ncdf1, "lljcat2", typeof(lljcat2), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "lljcat2", lljcat2 ) ;lljcat2
  
     filevardef    (ncdf1, "lljcat3", typeof(lljcat3), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "lljcat3", lljcat3 )  ;lljcat3
  
  
;WRITE VARS TO PREDEFINED LOCATIONS, ALREADY COPIED ATTS, SO
;TRANSFER VALUES ONLY (/.../)

     ncdf1->time	= (/time/)
     ncdf1->xc		= (/xc/)
     ncdf1->yc		= (/yc/)
     ncdf1->lat2d	= (/lat2d/)
     ncdf1->lon2d	= (/lon2d/)
     ncdf1->xlat        = (/xlat/)
     ncdf1->xlon        = (/xlon/)
     ncdf1->$pname$	= proj
     ncdf1->msp		= (/msp/)
     ncdf1->mwd		= (/mwd/)
     ncdf1->mshr	= (/mshr/)
     ncdf1->mhgt	= (/mhgt/)
     ncdf1->lljcat      = (/lljcat/)
     ncdf1->lljcat1      = (/lljcat1/)
     ncdf1->lljcat2      = (/lljcat2/)
     ncdf1->lljcat3      = (/lljcat3/)

;;;;;;
;
; delete all the variables you created
; this is usually important if you loop through files with different dimension sizes 
;
;;;;;;;


 delete(fAtt)
 delete(msp)
 delete(mwd)
 delete(mshr)
 delete(mhgt)
 delete(lljcat) 
 delete(time)
 delete(date)

print("DONE"); YAY you made it ! 
end
