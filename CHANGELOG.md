# Changelog

## 071025

Added nitrate component - new version called 4.5.

### mnmod.f90

New file.

### cmmod 

```fortran
add USE MNMOD, only : MNCONT
USE COLM_CG  
USE PLANT_CC
USE SGLOBAL, ONLY       : uznow
USE AL_D, ONLY       : TA
```

Add nitrate call:

```fortran
IF (ismn) then 
    CALL MNCONT(MND,MNFC,MNFN,MNPL,MNPR,MNOUT1,MNOUT2,MNOUTPL,NCETOP,NCON,NEL,NLF, &....
```

Change source/sink terms for nitrate:

```fortran
   if (ISMN) then
       DO NCE = NCEBOT,NCETOP
           EDCAP(NCE) = SSS1(NCL,NCE,NCONT).....
```

### frmod

Add extra files to be read in:

```fortran
USE AL_C, ONLY :..... MND,MNFC,MNFN,MNPL,MNPR,MNOUT1,MNOUT2,MNOUTPL
USE IS_CC,    ONLY : ISPLT,ISMN
```

File unit numbers: log = unit 61, nitrate 53-60

Stop crash for 1d simulation with no outlet

```fortran
    if ((mblink.eq.0).and.(mbface.eq.0)) then
        qocav=0
    else
           qocav     = qoc (mblink, mbface)
    endif
```

For reading spatially variable contaminant array just read the active bit.

```fortran
DOUBLEPRECISION DUMMYCONC(total_no_elements,top_cell_no)
      CALL ALINTP (LLEE, NCETOP, ..... DUMMYCONC )
              CCCC (NCL, NCE, NCONT)= DUMMYCONC (NCL,NCE)
```

### AL_C.f90

Added file unit numbers for nitrate

```fortran
CONT_CC add
      DOUBLEPRECISION SSS1(NELEE,LLEE,NCONEE), SSS2(NELEE,LLEE,NCONEE)
!                           SOURCE/SINK TERMS FOR PLANT UPTAKE AND NITRATE
```

### IS_CC.f90 

Added ISMN.

### mod_load_filedata

Change error message in ALALLI. Correct error in ALREAD and ALRED2 for number of characters in ALINTP chage so the cacultion only uses dynamic cells.

## 230922

Reduced maximum allowed number of grid sizes in x and y directions and allow more raingauges.
Allows negative Strickler for additional storage.

## 070920

Add all the visulaisation.f90 files and up to date include and library files. Remove shegraph dll.

## 090720

Add Reservoir ZQ component. 
Version is now 4.5.0. 
See [ZQ module.md](<docs/module_docs/ZQ module.md>) for details.


## 080720

Sort out problem with 1024,1030 and 1060 errors.
There is now a reduction in timestep length if one of the above errors occurs.

## 220420

Renaming of variables in contaminant code. See [Rename_Overview.md](docs/Rename_Overview.md).
Update compiler, minor changes needed.
Minor bug in writing of syd file (Dano example).
Heading for writing of every timestep discharge.


## 240915
Changed NLYREE from 10 to 20

```fortran
    ! --- MAXIMUM NUMBER OF SOIL LAYERS + 1
          INTEGER, PARAMETER :: NLYREE=20
```

## 270515

### etmod.f90

```fortran
    ! sb 270515 bare soil evap should be less than short grass evap    
        IF (II.EQ.top_cell_no) ESOIL = 0.5 * AE * (1 - CPLAI)  
```

## 200515

Put needed dll's in program file.

## 220415

Optional Extra discharge points. 
Read in rundata number 47 and output the data in regular discharge output.
Changes to al_D.f90 and frmod.f90 mostly froutput.f90.


## 210415

For big catchments change formating from 200 to 500 in the following lines:

- frmod.f90: L4313, L1388
- ocmod.f90: L1020,L1042
- utilsmod.f90: L646,L648,L661,L739


## 060415

Reduced value of invalid timestep.


## 260215

Tidy up output. Needs new shegrpah2.3.dll.


## 240215

- Change write sta to remove ```*****``` in pri file.
- Change cobres example so it only needs 50 cells.
- improve output to the screen.


## 130215

Time varying output step (in fr52)

### FRmod.f90(25)

```fortran
    SF, SMD, SD, TIMEUZ, TS, TIM, TMAX, TTH, UZVAL, VHT, VED, VSE,TOUTPUT                  
```

### FRmod.f90(1797)

```fortran
    uznowt=uznow*(1/TOUTPUT)
    next_hour = INT(uznowt) + 1.0
```

### FRmod.f90(1803)

```fortran
    uznowt=uznow*(1/TOUTPUT)
    
    hour_now  = INT(uznowt)

    IF(hour_now<next_hour) THEN  ! not new hour
        qoctot = qoctot + qocav*(uznowt-uzold)  

        qoctot    = qocav * (uznowt-next_hour)
```

### FRmod.f90(4359)

```fortran
    READ (FRD, *) TOUTPUT
```

## 130215

Add spatially distributed temperature.

### frmod.f90:

```fortran
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
```

### rest.f90

L14

```fortran
                         tah, tal, ista
```

L385

```fortran
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
```

L488

```fortran
          ta (I) = (tahigh (I) +  talow (I) )/2.0
```

### AL.D:

```fortran
    ISTA
```

## 051214

In utilsmod.f90 change the following to be compatible with prepare-xml.exe

```fortran
    !          READ (INF, 30) (IA (J, K), J = 1, NX)  
               READ (INF, *) (IA (J, K), J = 1, NX)  
               30 FORMAT(20I4)  
```


## 120914

Compiled with vs2012 and cvf2013. Changed location of "incl_mod_shegraph" directory in the following two places

```
        project | properties | fortran | general |additional include direcories
        project | properties | linker  | general |additional library direcories
```


## 120514

Outlet link and face now calculated correctly. Check it is type 7 (weir).

Lateral flow input not working: 

```
    CALL OCEXT  was commented out, now included
```

and

```fortran
    READ (OCD, * ) NOCHB,NOCFB,NOCPB  
```

was put back in.

Made xstab dynamically alloctable ```***************```

ocmod.f90 line14, 156
ocmod2.f90 line15 lines61-64 and line18

As 4.4.1 except the following bug fixes

4.4.1 As 4.4.0 except uses start.exe not sv4.4.0.exe


## 211112

```fortran
    nxscee=20000
```


## Error in smmod.f90

Needed the following change:

```fortran
    SUBROUTINE initialise_smmod
       LOGICAL         :: first=.TRUE.
       if (FIRST) then
          ALLOCATE (TMELT(max_no_snowmelt_slugs,total_no_elements))
          ALLOCATE (SMELT(max_no_snowmelt_slugs,total_no_elements))
          FIRST = .FALSE.
       endif
    END SUBROUTINE initialise_smmod
```

## Error in utilsmod.f90 *

```fortran
    r= r+ 0.0000028  !add 1/100 of a second to sort out round error with mins
```

## Error in Frmod.90 and subroutine frmb

Leap years were wrongly calculated wrong!!

```fortran
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
```

### utilsmod.f90

```fortran
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
```
