# Changelog
## 220420
******

Renaming of variables in contaminant code. See Docs/Rename_Overview.md.

Update compiler, minor changes needed.

Minor bug in writing of syd file (Dano example)

Heading for writing of every timestep discharge


## 240915
******

I have changed NLYREE from 10 to 20

    ! --- MAXIMUM NUMBER OF SOIL LAYERS + 1
          INTEGER, PARAMETER :: NLYREE=20


## 270515
******
in etmod.f90

    ! sb 270515 bare soil evap should be less than short grass evap    
        IF (II.EQ.top_cell_no) ESOIL = 0.5 * AE * (1 - CPLAI)  



## 200515
******
Put needed dll's in program file


## 220415
******
Optional Extra discharge points. read in rundata number 47 and output the data in regular discharge output
change to al_D.f90 and frmod.f90 mostly froutput.f90


## 210415
******
for big catchments change formating from 200 to 500 in the following lines
frmod.f90: L4313, L1388
ocmod.f90: L1020,L1042
utilsmod.f90: L646,L648,L661,L739


## 060415
******
reduce value of invalid timestep


## 260215
******
Tidy up output. Need new shegrpah2.3.dll


## 240215
******

- Change write sta to remove ***** in pri file.
- Change cobres example so it only needs 50 cells
- improve output to the screen


## 130215
******

time varying output step (in fr52)
In FRmod.f90(25):

    SF, SMD, SD, TIMEUZ, TS, TIM, TMAX, TTH, UZVAL, VHT, VED, VSE,TOUTPUT                  

FRmod.f90(1797):    
   
    uznowt=uznow*(1/TOUTPUT)
    next_hour = INT(uznowt) + 1.0

In FRmod.f90(1803):    

    uznowt=uznow*(1/TOUTPUT)

    hour_now  = INT(uznowt)
    
    IF(hour_now<next_hour) THEN  ! not new hour
        qoctot = qoctot + qocav*(uznowt-uzold)  

        qoctot    = qocav * (uznowt-next_hour)

In FRmod.f90(4359):

    READ (FRD, *) TOUTPUT




## 130215
******
Add spatially distributed temperature

In frmod.f90:

    1673 ista=.true.

    1690:
    !***Sb 161213
          if (I.eq.45.or.i.eq.46) then 
             ista=.false.
          endif

    4022:
          if (ISTA) then 
             READ (TAH,*,err=570,end=570)
             READ (TAL,*,err=571,end=571)
          endif
    4036:
     570  CALL ERROR(FFFATAL,1066,PPPRI,0,0,   'no data in air temp - high file')
     571  CALL ERROR(FFFATAL,1067,PPPRI,0,0,   'no data in air temp - low file')

In rest.f90
    14: 
                         tah, tal, ista
    385 +:
     284    if (ista) then
             READ (TAH, *, END = 383) (tahigh (I), I = 1, NM)  
             goto 384
             endif

      383    do i=1,nm
               tahigh(i) = 10.0
             enddo

      384    if (ista) then
             READ (TAL, *, END = 483) (talow (I), I = 1, NM)  
             goto 484
             endif
    
      483     do i=1,nm
               talow(i) = 10.0
             enddo

    
      484 do i = 1, nm 

    L488:
          ta (I) = (tahigh (I) +  talow (I) )/2.0
 
In AL.D:
    
    ISTA


## 051214
*******
In utilsmod.f90 change the following to be compatible with prepare-xml.exe

    !          READ (INF, 30) (IA (J, K), J = 1, NX)  
               READ (INF, *) (IA (J, K), J = 1, NX)  
               30 FORMAT(20I4)  



## 120914
******
Compiled with vs2012 and cvf2013. Changed location of "incl_mod_shegraph" directory in the following two places
        project | properties | fortran | general |additional include direcories
        project | properties | linker  | general |additional library direcories



## 120514
******
Outlet link and face now calculated correctly. Check it is type 7 (weir)

Lateral flow input not working. 

    CALL OCEXT  was commented out, now included

and

    READ (OCD, * ) NOCHB,NOCFB,NOCPB  

was put back in.


make xstab dynamically alloctable***************

ocmod.f90 line14, 156
ocmod2.f90 line15 lines61-64 and line18

As 4.4.1 except the following bug fixes

4.4.1 As 4.4.0 except uses start.exe not sv4.4.0.exe


## 211112
******

    nxscee=20000



## Error in smmod.f90

need the following change:

    SUBROUTINE initialise_smmod
       LOGICAL         :: first=.TRUE.
       if (FIRST) then
          ALLOCATE (TMELT(max_no_snowmelt_slugs,total_no_elements))
          ALLOCATE (SMELT(max_no_snowmelt_slugs,total_no_elements))
          FIRST = .FALSE.
       endif
    END SUBROUTINE initialise_smmod


## Error in utilsmod.f90 *

    r= r+ 0.0000028  !add 1/100 of a second to sort out round error with mins

## Error in Frmod.90 and subroutine frmb. leap years wrong!!**************************************

    CALL FRRESP (AIOSTO, UZNOW, .TRUE.)  
    ! Calculate the next output time
    IF (MBFLAG.EQ.1) THEN  
    !         * next day
       LYEAR = 0  
       IF (MOD (MBYEAR, 4) .EQ.0.AND.MBMON.EQ.2) LYEAR = 1  
       MBDAY = MOD (MBDAY, MONEND (MBMON) + LYEAR) + 1  
    ELSE  
    !         * next month
       MBDAY = 1  
    ENDIF  

based on ulilsmod.f90

    IF (MBFLAG.EQ.1) THEN  
    !         * next day
       LYEAR = 0  
    
    
       IF(MOD(mbyear,4)==0) THEN
        IF(MOD(mbyear,100)==0) THEN
            r = MOD(mbyear,400)==0
        ELSE
            r = .TRUE.
        ENDIF
       ELSE
        r = .FALSE.
       ENDIF
    
    
       IF ((r=.true.).AND.MBMON.EQ.2) LYEAR = 1  
       MBDAY = MOD (MBDAY, MONEND (MBMON) + LYEAR) + 1  
    ELSE  
    !         * next month
       MBDAY = 1  
    ENDIF  






