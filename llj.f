cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c This subroutine searches through a wind profile to determine
c whether the wind profile contains a LLJ, a SLLJ or SWE.  This
c distinction is based upon Bonner's classification (1968):  
c      Category 1: 12 m/s <= V < 16 m/s & dV >= 6 m/s
c      Category 2: 16 m/s <= V < 20 m/s & dV >= 8 m/s
c      Category 3: 20 m/s <= V          & dV >= 10 m/s
c The wind maximum (V) must occur in the lowest 1.5 km, and dV is
c the minimum difference between the level of max. wind and 3 km.
c
c Input variables:
c ! input variables must be bottom-up in array indexing
c      WS - 4D array containing the Wind Speed at each level (m/s)
c      WD - 4D array containing the Wind Direction at each level
c      HGT - 4D array containing the height AGL of the each level (m)
c Output variable: 3D (time,ny,nx)
c      LLJCAT - exclusive LLJ categories
c!not  LLJOVR - overlapping LLJ categories
c!not  WE - wind event
c      MAXU - max LLJ speed
c      MAXD - max LLJ speed direction
c      MAXSHR - max shear above LLJ
c      MAXHGT - llj height
c
c this only returns what is a llj (i.e., category .ge 0)
c everything else is set to fillvalue -999.
c 
C NCLFORTSTART
       subroutine lljonly(ws,wd,sigma,maxu,maxd,maxshr,maxhght,lljcat,
     &               lljcat1,lljcat2,lljcat3,nt,nl,ny,nx,fillval)

       integer nt,nl,ny,nx,maxl
       integer lljcat(nt,ny,nx)
       real maxhght(nt,ny,nx)
       real lljcat1(nt,ny,nx)
       real lljcat2(nt,ny,nx)
       real lljcat3(nt,ny,nx)
       real fillval
       real ws(nt,nl,ny,nx)
       real wd(nt,nl,ny,nx)
       real sigma(nl)
       real maxu(nt,ny,nx)
       real maxd(nt,ny,nx)
       real maxshr(nt,ny,nx)
       real dif


      print *, "Begining Fortran"
c WE ARE GOING TO ASSUME THAT THE FILL VALUE OF THE INPUT IS -999.
c MAKE SURE IT IS

       if(fillval.ne.-999.)then
         WRITE(*,*) 'FILL VALUE MUST EQ -999 IN LLJ FUNCTION =',fillval
         stop
       endif

c LOOP OVER GRID AND NUMBER OF TIMES

       do m = 1,nt
       do j = 1,ny
       do i = 1,nx

c       This part of the code searches for the wind maximum in the
c       lowest 5 sigma levels of a regcm vertical grid 
c       It then records the wind direction and height of that 
c       value

      maxu(m,j,i) = 0.0
c       LOOP OVER NUMBER OF LEVELS 
c       NOTE! 13 corresponds to the regcm sigma level ~1.5km AGL 
c       change accordingly!! 
        do l = 13,nl 
         if (ws(m,l,j,i) .gt. maxu(m,j,i)) then 
                maxu(m,j,i)=ws(m,l,j,i)
                maxd(m,j,i)=wd(m,l,j,i)
                maxhght(m,j,i)=sigma(l)
                maxl = l 
         endif 
         enddo

c       This part of the code calculates the shear from 
c       the maximum wind found above and the lowest ~3km 
c       which is 11 in regcm sigma coords  
         
         maxshr(m,j,i) = 0.0
          do l = 11,maxl
c       LOOP OVER NUMBER OF LEVELS 
c       NOTE! 11 corresponds to the regcm sigma level ~3km AGL 
c       change accordingly!! 
            if (ws(m,l,j,i) .ne. fillval) then
                dif = maxu(m,j,i) - ws(m,l,j,i)
                  if (dif.gt.maxshr(m,j,i)) then 
                        maxshr(m,j,i) = dif
                  endif
            endif 
           enddo 

c        Here we define our LLJcategories all as 0. We will 
c        flag that time,lat,lon with a 1 if it meets the criteria


         lljcat(m,j,i) = 0
         lljcat1(m,j,i) = 0
         lljcat2(m,j,i) = 0
         lljcat3(m,j,i) = 0

c--------Check for category 1
         if(maxu(m,j,i).ge.12.)then
           if(maxshr(m,j,i).ge.6.)then
             if(maxu(m,j,i).lt.16.)then
               lljcat(m,j,i) = 1
               lljcat1(m,j,i) = lljcat1(m,j,i) + 1
             endif
           endif
         endif
 
          
c--------Check for category 2
         if(maxu(m,j,i).ge.16.)then
           if(maxshr(m,j,i).ge.8.)then
             if(maxu(m,j,i).lt.20.)then
               lljcat(m,j,i) = 2
               lljcat2(m,j,i) = lljcat2(m,j,i) + 1
             endif
           endif
         endif
  

c--------Check for category 3
         if(maxu(m,j,i).ge.20.)then
           if(maxshr(m,j,i).ge.10.)then
             lljcat(m,j,i) = 3
             lljcat3(m,j,i) = lljcat3(m,j,i) + 1
           endif
         endif

c        fill any missing values 

       if(lljcat(m,j,i).lt.1.)then 
                maxu(m,j,i)=-999.
                maxshr(m,j,i)=-999.
                maxhght(m,j,i)=-999.
                maxd(m,j,i)=-999.
       endif 


       enddo
       enddo
       enddo


      print *, "end :)" 
        return
c        DONE!

        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

