;;;;; this script plots llj frequency;;;;;;;;;;;
; hourly or over whole month; 
; panel plots
;18 may 2018
;acar

 load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_csm.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

;----------------------------------------------------------------------
;read in regcm variables 




;;;;;set month;;;;;;;
;               0       1       2      3     4           5
month_all = (/"april","may","june","july","august","september"/)
months_all = (/"04"  ,"05",    "06", "07",   "08","     09"/)

do mon = 0,4

;mon = 4
month=month_all(mon)
months = months_all(mon)


;;;;;;set model;;;;;;;;;;;
;                   0            1             2            3
model_mb_all = (/"ERA-INT","GFDL-ESM2M","HadGEM2-ES","MPI-ESM-LR"/)
model_all =     (/"era",      "gfdl",      "hadgem",     "mpi"/)

mods= 3

model_mb = model_mb_all(mods)
model = model_all(mods)

;;;;;set gridres;;;;;;;;;;;;;;;
;                     0             1          2
gridres_mb_all = (/"FACETS-11","NAM-22","NAM-44"/)
gridres_all =     (/"12km",     "25km",   "50km"/)

xres = 0

gridres_mb = gridres_mb_all(xres)
gridres = gridres_all(xres)

;;;;;;;;;set climate type;;;;;;;;
;		    0       1      2	
modeltype_all = (/"eval","hist","rcp85"/)

mt = 1

modeltype = modeltype_all(mt)

;;;; set hours;;;;;;;;
;		   0    1    2      3
hours_all = (/0,1,2,3/) ; 0=0z 1=6z 2=12z 3=18z
hoursstring_all = (/"00Z","06Z","12Z","18Z"/)

hr = 1

hours = hours_all(hr)
hoursstring = hoursstring_all(hr)

;; number of days in the month;;;;; NOT for HadGEM! 
monthdays_all = (/30,31,30,31,31,30/)
monthdays = monthdays_all(mon)

;monthdays = 31  ;30 days for every month in HadGEM
;                ;April,June 30
;                ;May, July, August 31


if(mt.eq.0)then
year20 = 9110  ;8100 for HadGEM, GFDL
	        ;9110 for ERA-I
end if

if(mt.eq.1)then
year20 = 8100
end if

if(mt.eq.2)then
year20=7695
end if 

print(year20)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; dir = "/tera12/acaruthe/facets/"+model+"/"+month+"/"
  dir = "/glade/scratch/acaruth/facets/regcm/"+model+"/"+month+"/"

; f1 = addfile(dir+"llj_"+gridres+"_"+year20+".nc","r")

 f1 = addfile(dir+"llj_"+gridres+"_"+month+"_"+year20+".nc","r")
 
   time = f1->time
   nt = dimsizes(time)   
    
print(dimsizes(time))


   lljcat1 = f1->lljcat1
   lljcat2 = f1->lljcat2
   lljcat3 = f1->lljcat3
   lljcat_all = f1->lljcat1


print(lljcat1(:,60,60))


   xc = f1->xc
   yc = f1->yc
   nx = dimsizes(xc)
   ny = dimsizes(yc)

   xlat = f1->xlat
   xlon = f1->xlon
 
   lat2d = f1->xlat(:,:)
   lon2d = f1->xlon(:,:)

   
     lljcat1!0 = "time"
     lljcat1!1 = "yc"
     lljcat1!2 = "xc"
  
     lljcat2!0 = "time"
     lljcat2!1 = "yc"
     lljcat2!2 = "xc"

     lljcat3!0 = "time"
     lljcat3!1 = "yc"
     lljcat3!2 = "xc"
    

     lljcat_all!0 = "time"
     lljcat_all!1 = "yc"
     lljcat_all!2 = "xc"

    pname = lljcat_all@grid_mapping
       proj = f1->$pname$
;       print(proj)




;;;idk why, but this needs to be deleted bc its annoying;;;;;

   delete(lljcat1@lat2d)
   delete(lljcat1@lon2d)

   delete(lljcat2@lat2d)
   delete(lljcat2@lon2d)

   delete(lljcat3@lat2d)
   delete(lljcat3@lon2d)

   delete(lljcat_all@lat2d)
   delete(lljcat_all@lon2d)


   lljcat1@lat2d = lat2d
   lljcat1@lon2d = lon2d
 
   lljcat2@lat2d = lat2d
   lljcat2@lon2d = lon2d
  
   lljcat3@lat2d = lat2d
   lljcat3@lon2d = lon2d

   lljcat_all@lat2d = lat2d
   lljcat_all@lon2d = lon2d

print("brought in variables")


   date = cd_calendar(time,-3)
     ;option 3 YYYYMMDDHH
   yyyymm_all = cd_calendar(time,-1)
      ; YYYYMM

;DOING THIS PER FILE CHUNK AND THEN A DO LOOP OVER ALL TIMES IN IT HAS PROVEN
;TO BE TOO MUCH... SO WE ARE GOING TO DO THIS PER MONTH NOW INSTEAD

   datestart = cd_calendar(time(0),0)
      ;array of years, months, days, hours, minutes, seconds
   yyyystart = datestart(0,0)
      ; 0 is years 
   mmstart   = datestart(0,1)
      ; 1 is months 
   dateend = cd_calendar(time(nt-1),0)

   yyyymmstart = cd_calendar(time(0),-1)
     ; option 1 is YYYYMM
   yyyymmend   = cd_calendar(time(nt-1),-1)
     ; option 1 is YYYYMM

 ;;;;;;;;;;;;;;;;;finding frequency;;;;;;;;;;;;;;;;;;;;
   numdays = 20*monthdays ;30 days in april, x 20 years in analysis 

 ; numdays = monthdays; 30 days in april, 22 years in analysis

;;;;;;;;;;;;;;;;for hourly frequency;;;;;;;;;;;;;;;;;;;;;;

lljcat1_freq = lljcat1(hours,:,:)
lljcat2_freq = lljcat2(hours,:,:)
lljcat3_freq = lljcat3(hours,:,:)
lljcat_freq = lljcat_all(hours,:,:)

; printVarSummary(lljcat1_freq)
; print(lljcat1_freq)


     lljcat_freq = lljcat_freq@_FillValue
     lljcat1_freq = lljcat1_freq@_FillValue
     lljcat2_freq = lljcat2_freq@_FillValue
     lljcat3_freq = lljcat3_freq@_FillValue
 
     lljcat1_freq@long_name = "Frequency LLJ Category 1"
     lljcat2_freq@long_name = "Frequency LLJ Category 2"
     lljcat3_freq@long_name = "Frequency LLJ Category 3"
     lljcat_freq@long_name = "Frequency LLJ All Category"

     lljcat1_freq@standard_name = "Frequency LLJ Category 1"
     lljcat2_freq@standard_name = "Frequency LLJ Category 2"
     lljcat3_freq@standard_name = "Frequency LLJ Category 3"
     lljcat_freq@standard_name = "Frequency LLJ All Category"


     lljcat_freq@units = "none"
     lljcat1_freq@units = "none"
     lljcat2_freq@units = "none"
     lljcat3_freq@units = "none"

     lljcat1_freq@grid_mapping = lljcat1@grid_mapping
     copy_VarAtts(lljcat1,lljcat1_freq)   

     lljcat2_freq@grid_mapping = lljcat2@grid_mapping
      copy_VarAtts(lljcat2,lljcat2_freq)     

     lljcat3_freq@grid_mapping = lljcat3@grid_mapping
     copy_VarAtts(lljcat3,lljcat3_freq)     

     lljcat_freq@grid_mapping = lljcat1@grid_mapping
      copy_VarAtts(lljcat1,lljcat_freq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	; do i = 0,3
 lljcat1_sum = dim_sum_n(lljcat1(hours::4,:,:), 0) ;over whole month
 lljcat1_total = lljcat1_sum/numdays ;30 for 30 days in month of april

  lljcat1_freq = lljcat1_total*100 ;into unit percent


  lljcat2_sum = dim_sum_n(lljcat2(hours::4,:,:), 0)
  lljcat2_total = lljcat2_sum/numdays
  
  lljcat2_freq = lljcat2_total*100

  lljcat3_sum = dim_sum_n(lljcat3(hours::4,:,:), 0)
  lljcat3_total = lljcat3_sum/numdays

  lljcat3_freq = lljcat3_total*100

  lljcat_sum = lljcat1_sum + lljcat2_sum + lljcat3_sum
  lljcat_total = lljcat_sum/numdays

;   print(dimsizes(lljcat_total))
 ;  print(dimsizes(lljcat_freq))
  lljcat_freq = lljcat_total*100

;print(lljcat_freq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


     

     lljcat1_freq@lat2d = lat2d
     lljcat1_freq@lon2d = lon2d
    
     lljcat2_freq@lat2d = lat2d
     lljcat2_freq@lon2d = lon2d

     lljcat3_freq@lat2d = lat2d
     lljcat3_freq@lon2d = lon2d

     lljcat_freq@lat2d = lat2d
     lljcat_freq@lon2d = lon2d


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; here we are going to create a new output file
; cat1
 
   foutname1 = "llj_"+gridres+"_"+month+"_"+year20+"_frequency_"+hoursstring+"_cat1.nc"
    dirout = "/glade/scratch/acaruth/facets/regcm/"+model+"/"+month+"/"
  ;  dirout = "/tera12/acaruthe/facets/"+model+"/"+month+"/"
   fout1 = dirout+foutname1

   system("rm "+fout1)
   ncdf1 = addfile(fout1,"c")

   fAtt = True

;    fAtt@source_file = dir+"llj_"+gridres+"_"+month+"_"+year20+".nc"
    fAtt@source_file = dir+"llj_"+gridres+"_"+month+"_"+year20+".nc"

    fAtt@creation_date = systemfunc("date")
    fAtt@Conventions = "None"

    fileattdef(ncdf1,fAtt)
  
    dimNames = (/"yc","xc"/)
    dimSizes = (/ny,nx/)
    dimUnlim = (/False,False/)

    filedimdef(ncdf1,dimNames,dimSizes,dimUnlim)

    filevardef    (ncdf1, "xc", typeof(xc), "xc")
    filevarattdef (ncdf1, "xc", xc)

    filevardef    (ncdf1, "yc", typeof(yc), "yc")
    filevarattdef (ncdf1, "yc", yc)

    filevardef    (ncdf1, "xlat", typeof(xlat), (/"yc","xc"/))
    filevarattdef (ncdf1, "xlat", xlat)

    filevardef    (ncdf1, "xlon", typeof(xlon), (/"yc","xc"/))
    filevarattdef (ncdf1, "xlon", xlon)

    filevardef    (ncdf1, "lat2d", typeof(lat2d), (/"yc","xc"/) )
    filevarattdef (ncdf1, "lat2d", lat2d)

    filevardef    (ncdf1, "lon2d", typeof(lon2d), (/"yc","xc"/) )
    filevarattdef (ncdf1, "lon2d", lon2d)

    filevardef    (ncdf1, "lljcat1_freq", typeof(lljcat1_freq), (/"yc","xc"/) )
    filevarattdef (ncdf1, "lljcat1_freq", lljcat1_freq)


     ncdf1->xc          = (/xc/)
     ncdf1->yc          = (/yc/)
     ncdf1->lat2d       = (/lat2d/)
     ncdf1->lon2d       = (/lon2d/)
     ncdf1->xlat        = (/xlat/)
     ncdf1->xlon        = (/xlon/)
     ncdf1->$pname$     = proj
     ncdf1->lljcat1_freq     =(/lljcat1_freq/)


   foutname2 = "llj_"+gridres+"_"+month+"_"+year20+"_frequency_"+hoursstring+"_cat2.nc"
    fout2 = dirout+foutname2

   system("rm "+fout2)
   ncdf2= addfile(fout2,"c")

    fileattdef(ncdf2,fAtt)

    dimNames = (/"yc","xc"/)
    dimSizes = (/ny,nx/)
    dimUnlim = (/False,False/)

    filedimdef(ncdf2,dimNames,dimSizes,dimUnlim)

    filevardef    (ncdf2, "xc", typeof(xc), "xc")
    filevarattdef (ncdf2, "xc", xc)

    filevardef    (ncdf2, "yc", typeof(yc), "yc")
    filevarattdef (ncdf2, "yc", yc)

    filevardef    (ncdf2, "xlat", typeof(xlat), (/"yc","xc"/))
    filevarattdef (ncdf2, "xlat", xlat)

    filevardef    (ncdf2, "xlon", typeof(xlon), (/"yc","xc"/))
    filevarattdef (ncdf2, "xlon", xlon)

    filevardef    (ncdf2, "lat2d", typeof(lat2d), (/"yc","xc"/) )
    filevarattdef (ncdf2, "lat2d", lat2d)

    filevardef    (ncdf2, "lon2d", typeof(lon2d), (/"yc","xc"/) )
    filevarattdef (ncdf2, "lon2d", lon2d)

    filevardef    (ncdf2, "lljcat2_freq", typeof(lljcat2_freq), (/"yc","xc"/) )
    filevarattdef (ncdf2, "lljcat2_freq", lljcat2_freq)


     ncdf2->xc          = (/xc/)
     ncdf2->yc          = (/yc/)
     ncdf2->lat2d       = (/lat2d/)
     ncdf2->lon2d       = (/lon2d/)
     ncdf2->xlat        = (/xlat/)
     ncdf2->xlon        = (/xlon/)
     ncdf2->$pname$     = proj
     ncdf2->lljcat2_freq     =(/lljcat2_freq/)



   foutname3 = "llj_"+gridres+"_"+month+"_"+year20+"_frequency_"+hoursstring+"_cat3.nc"
    fout3 = dir+foutname3
  
   system("rm "+fout3)
   ncdf3 = addfile(fout3,"c")

    fileattdef(ncdf3,fAtt)

    dimNames = (/"yc","xc"/)
    dimSizes = (/ny,nx/)
    dimUnlim = (/False,False/)

    filedimdef(ncdf3,dimNames,dimSizes,dimUnlim)

    filevardef    (ncdf3, "xc", typeof(xc), "xc")
    filevarattdef (ncdf3, "xc", xc)

    filevardef    (ncdf3, "yc", typeof(yc), "yc")
    filevarattdef (ncdf3, "yc", yc)

    filevardef    (ncdf3, "xlat", typeof(xlat), (/"yc","xc"/))
    filevarattdef (ncdf3, "xlat", xlat)

    filevardef    (ncdf3, "xlon", typeof(xlon), (/"yc","xc"/))
    filevarattdef (ncdf3, "xlon", xlon)

    filevardef    (ncdf3, "lat2d", typeof(lat2d), (/"yc","xc"/) )
    filevarattdef (ncdf3, "lat2d", lat2d)

    filevardef    (ncdf3, "lon2d", typeof(lon2d), (/"yc","xc"/) )
    filevarattdef (ncdf3, "lon2d", lon2d)

    filevardef    (ncdf3, "lljcat3_freq", typeof(lljcat3_freq), (/"yc","xc"/) )
    filevarattdef (ncdf3, "lljcat3_freq", lljcat3_freq)


 
     ncdf3->xc          = (/xc/)
     ncdf3->yc          = (/yc/)
     ncdf3->lat2d       = (/lat2d/)
     ncdf3->lon2d       = (/lon2d/)
     ncdf3->xlat        = (/xlat/)
     ncdf3->xlon        = (/xlon/)
     ncdf3->$pname$     = proj
     ncdf3->lljcat3_freq     =(/lljcat3_freq/)



   foutname4 = "llj_"+gridres+"_"+month+"_"+year20+"_frequency_"+hoursstring+"_all.nc"
    fout4 = dirout+foutname4
   
   system("rm "+fout4)
   ncdf4 = addfile(fout4,"c")

   fAtt = True

    fileattdef(ncdf4,fAtt)

    dimNames = (/"yc","xc"/)
    dimSizes = (/ny,nx/)
    dimUnlim = (/False,False/)

    filedimdef(ncdf4,dimNames,dimSizes,dimUnlim)

    filevardef    (ncdf4, "xc", typeof(xc), "xc")
    filevarattdef (ncdf4, "xc", xc)

    filevardef    (ncdf4, "yc", typeof(yc), "yc")
    filevarattdef (ncdf4, "yc", yc)

    filevardef    (ncdf4, "xlat", typeof(xlat), (/"yc","xc"/))
    filevarattdef (ncdf4, "xlat", xlat)

    filevardef    (ncdf4, "xlon", typeof(xlon), (/"yc","xc"/))
    filevarattdef (ncdf4, "xlon", xlon)

    filevardef    (ncdf4, "lat2d", typeof(lat2d), (/"yc","xc"/) )
    filevarattdef (ncdf4, "lat2d", lat2d)

    filevardef    (ncdf4, "lon2d", typeof(lon2d), (/"yc","xc"/) )
    filevarattdef (ncdf4, "lon2d", lon2d)

    filevardef    (ncdf4, "lljcat_freq", typeof(lljcat_freq), (/"yc","xc"/) )
    filevarattdef (ncdf4, "lljcat_freq", lljcat_freq)

        
     ncdf4->xc          = (/xc/)
     ncdf4->yc          = (/yc/)
     ncdf4->lat2d       = (/lat2d/)
     ncdf4->lon2d       = (/lon2d/)
     ncdf4->xlat        = (/xlat/)
     ncdf4->xlon        = (/xlon/)
     ncdf4->$pname$     = proj
     ncdf4->lljcat_freq     =(/lljcat_freq/)


delete(time)
delete(date)
delete(yyyymm_all)
delete(lljcat1)
delete(lljcat2)
delete(lljcat3)
delete(lljcat_all)


end do 

print("DONE")
end
