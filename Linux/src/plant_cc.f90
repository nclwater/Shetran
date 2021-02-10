MODULE PLANT_CC
!------------------------------ Start of PLANT.CC ---------------------*
!
!                       INCLUDE FILE FOR PLANT CONTAMINANT DATA
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/PLANT.CC/4.1
! Modifications:
!                           JE      APR 91   3.0     WRITTEN
!                           JE     13/6/91   3.1     CHECKED, NO CHANGES
!                           JE     16/3/93   3.4     FULL IMPLEMENTATION
! RAH  970224  4.1  Explicit typing.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Imported constants
!                     NCONEE,NELEE,NPELEE
!
! Commons
USE SGLOBAL, ONLY : NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
IMPLICIT NONE

INTEGER, PARAMETER :: NTEMP1=2*NELEE*NPELEE*NCONEE, NTEMP2=NPLTEE*NCONEE !FROM pldat BLOCKDATA


DOUBLEPRECISION GENAA (NPELEE), GENBB (NPELEE)  
DOUBLEPRECISION :: GCPL, GMCPAA, GMCPBB, GMCBBD, QCPAA, QCPBB, RHOPL=500.0d0  !DATA FROM pldat BLOCKDATA

!COMMON / ALOCAL / GENAA, GENBB  

!COMMON / VLOCAL / GCPL, GMCPAA, GMCPBB, GMCBBD, QCPAA, QCPBB, &
 !RHOPL
!                       Arrays and variables used only in plant routines
DOUBLEPRECISION :: BCPAA (NELEE, NPELEE, NCONEE)=0.0d0  !DATA FROM pldat BLOCKDATA 
DOUBLEPRECISION :: BCPBB (NELEE, NPELEE, NCONEE)=0.0d0

!COMMON / BCON / BCPAA, BCPBB  
DOUBLEPRECISION :: DELONE (NPLTEE)=0.5, DELTWO (NPLTEE)=0.9 !DATA FROM pldat BLOCKDATA 
DOUBLEPRECISION :: DELTHR (NPLTEE)=1.0, DELFOU (NPLTEE)=1.0, FLEFT (NPLTEE) !DATA FROM pldat BLOCKDATA
     
!COMMON / DELTA / DELONE, DELTWO, DELTHR, DELFOU, FLEFT  
!                 Plant and cropping property data
DOUBLEPRECISION GMCBBO (NELEE, NPELEE)  

!COMMON / GMOLD / GMCBBO  
!                 Old values for masses in compartment b
INTEGER :: NPL (NELEE), NPLTYP (NELEE, NPELEE)=1, NPLT  !DATA FROM pldat BLOCKDATA

!COMMON / NUMPL / NPL, NPLTYP, NPLT  
!                 Total number of plants, and their type numbers, on
!                 each soil column
DOUBLEPRECISION PKMAX (NPLTEE, NCONEE), PMASS (NPLTEE)  

!COMMON / MASSP / PKMAX, PMASS  
!                 Contaminant uptake coefficient, and maximum mass of
!                 plant material per unit area
DOUBLEPRECISION PFONE (NELEE, NPELEE), PFTWO (NPLTEE)  
DOUBLEPRECISION PF2MAX (NPLTEE), PDZF3 (NELEE, NPELEE, LLEE)  

!COMMON / PF123 / PFONE, PFTWO, PF2MAX, PDZF3  
!                 nb  PFTWO and PF2MAX are specified for each plant type
!                 PLAI, CLAI, and RDF for use in contaminant plant
!                 uptake routines
DOUBLEPRECISION XXI
!PRIVATE :: NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
end MODULE PLANT_CC

!      BLOCK DATA PLDAT
!*           Plant data
!
!      USE SGLOBAL
!      USE AL_C
!      USE COLM_CC
!      
!      USE PLANT_CC
!      PARAMETER(NTEMP1=2*NELEE*NPELEE*NCONEE, NTEMP2=NPLTEE*NCONEE)
!
!
!      DATA BCPAA,BCPBB / NTEMP1*0.0D0 /
!*                 Initialise plant relative concentrations
!
!      DATA DELONE / NPLTEE*half / DELTWO / NPLTEE*0.9D0 /
!     &     DELTHR / NPLTEE*one / FLEFT  / NPLTEE*one /
!*                 Plant and cropping property data
!
!*     DATA ESSCAP,ESSCPC,ESSCPT / 3*0.0D0 /
!*                 Ensures there is no uptake from surface water
!*                 and sediments
!
!      DATA RHOPL / 500.0D0 /
!*                 Density of plant material (used in scaling only)
!
!      DATA (NPLTYP(I,2),I=1,NELEE) / NELEE*1 /
!*                 Second plant type on each soil column
!
!      END
