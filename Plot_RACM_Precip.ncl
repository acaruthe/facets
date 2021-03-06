;***********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
;load "/u1/uaf/higgins/ncl/fixes/WRFUserARW.ncl"
;************************************************
begin

  a = addfile("infile.nc","r")
  wks = gsn_open_wks("pdf","test")

; Next, read in WRF data
   res = True

   precip     = a->pr
   lat2d = a->xlat(:,:)
   lon2d = a->xlon(:,:)

   precip!0      = "time"
   precip!1      = "lat"
   precip!2      = "lon"

   precip@lat2d = lat2d
   precip@lon2d = lon2d

;*************************************************
;Lat/lon attributes
;*************************************************

    gsn_define_colormap(wks,"gui_default")      ; choose colormap

    res                     = True              ; Plot mods desired.
    res@gsnDraw             = True             ; We will panel later.
    res@gsnFrame            = True

;    res@mpProjection      = "LambertConformal"
    res@gsnMaximize         = True              ; Maximize plot
    res@gsnSpreadColors     = True              ; use full range of colormap

;    res@mpMaxLatF           =  60              ; Set accordingly
;    res@mpMinLatF           =  24
;    res@mpMinLonF           = -30
;    res@mpMaxLonF           =  60

    res@cnFillOn            = True              ; color plot desired
    res@cnLinesOn           = False             ; turn off contour lines
    res@cnLineLabelsOn      = False             ; turn off contour lines

;    res@cnLevelSelectionMode =  "ManualLevels"   ; manual levels
;    res@cnMinLevelValF       = 0                 ; min level
;    res@cnMaxLevelValF       = 6000                ; max level
;    res@cnLevelSpacingF      =  250               ; interval

    res@lbLabelBarOn        = True              ; Labelbar will be in panel


;*************************************************
;**CONFIGURE BELOW********************************
;*************************************************
; Setup plots
 
    plot =  gsn_csm_contour_map(wks,precip(100,:,:),res)

end

