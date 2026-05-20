!> summary: Sediment and contaminant interface state.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `SED_CS` replaces the legacy `SED.CS` common blocks. It stores the shared
!> sediment/contaminant interface variables: deposited sediment area and depth,
!> infiltration and erosion rates, sediment fractions and porosities, stream-bed
!> parent-material soil type, sediment count, link flows, and particle flow
!> rates.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-16 | JE | 3.1 | Completed. |
!> | 1993-02-08 | GP | 3.4 | Brought `QLINK`/`QDEFF` from `LINK.CW`, added `SEDDIA`, and renamed `SDPOR` as `SDEPOR`. |
!> | 1994-10-02 | RAH | 3.4.1 | Applied standard header, declared variables, added `QSED`, and removed redundant `SEDDIA`. |
!> | 1997-02-20 | RAH | 4.1 | Separated `SDEPOI` from mixed-type `SDEPOR`. |
!> | 1998-03-08 | RAH | 4.2 | Removed `PSD`. |
!> | 1999-01-27 | SB | 4.27 | Added `DCBED` and `DCBSED`. |
!> | 2004-11 | JE | - | Converted to Fortran 95. |
!> @endhistory
MODULE sed_cs
!*------------------- Start of SED.CS ----------------------------------*
!*
!*                       INCLUDE FILE FOR SEDIMENT/CONTAMINANT INTERFACE
!*
!*----------------------------------------------------------------------*
!* Version:  SHETRAN/INCLUDE/SED.CS/4.2
!* Modifications:
!*                           JE     26/4/91   3.0     WRITTEN
!*                           JE     16/6/91   3.1     COMPLETED
!*  GP  930208  3.4  Bring QLINK,QDEFF from LINK.CW.  Add SEDDIA.
!*                   Rename SDPOR as SDEPOR.
!* RAH  02.10.94  Version 3.4.1 from version 3.4: standard header;
!*                 declare everything; add QSED to /SDFLO/;
!*                 remove redundant SEDDIA from /NUMSED/.
!* RAH  970220  4.1  Separate /SDEPOI/ from mixed-type /SDEPOR/.
!* RAH  980308  4.2  Remove PSD.
!*  SB  990127  4.27 Add DCBED and DCBSED as in 3.4.2
!  JE  NOV 04 ---- Convert to FORTRAN 95
!*----------------------------------------------------------------------*
!* Imported constants
!*                        NELEE,NLFEE,NSEDEE,NSEE
!*
!* Commons
   USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE, NSEE
   IMPLICIT NONE
   DOUBLEPRECISION   ARBDEP(NLFEE),DLS(NELEE),GINFD(NLFEE,NSEDEE), &
      GINFS(NLFEE,NSEDEE),GNU(NELEE),GNUBK(NLFEE), &
      DCBED(NLFEE),DCBSED(NLFEE,NSEDEE)
!!!      COMMON/   SDDEP   /ARBDEP,DLS,GINFD,GINFS,GNU,GNUBK,DCBED,DCBSED
!*                             ACCUMULATED X-SECTIONAL AREA OF DEPOSITED
!*                             SEDIMENTS; DEPTH OF LOOSE SEDIMENT; RATES
!*                             OF INFILTRATION OF SEDIMENTS; AND RATES OF
!*                             EROSION
!*
   DOUBLEPRECISION   FDEL(NELEE,NSEDEE)
   !!!     COMMON/   SDDEN   /FDEL
!*                             RELATIVE DENSITY OF SUSPENDED SEDIMENTS
!*
   DOUBLEPRECISION   FBETA(NELEE,NSEDEE),FBTSD(NLFEE,NSEDEE)
!!!      COMMON/   SDFRA   /FBETA,FBTSD
!*                             FRACTIONS OF LOOSE AND NEWLY DEPOSITED
!*                             SEDIMENTS
!*
   DOUBLEPRECISION   PBSED(NLFEE),PLS(NELEE), &
      SOSDFN(NSEE,NSEDEE),SOFN(NSEE,NSEDEE)
!!!!      COMMON/   SDEPOR  /PBSED,PLS,SOSDFN,SOFN
!*                             POROSITIES; THE FRACTION OF A SOIL WHICH
!*                             IS IN A GIVEN PARTICLE SIZE GROUP (SOFN IS
!*                             USED TO SET SOSDFN ONLY IF THE SEDIMENT
!*                             CODE IS NOT ACTIVE)
!*
   INTEGER            NSOBED(NLFEE)
!!!!      COMMON/   SDEPOI  /NSOBED
!*                             SOIL TYPE FOR PARENT MATERIAL @ STREAM BED
!*
   INTEGER            NSED
!!!!      COMMON/   NUMSED  /NSED
!*                             NUMBER OF SEDIMENTS
!*
   DOUBLEPRECISION   QLINK(NLFEE,2),QDEFF(NLFEE,2), &
      QSED(NELEE,NSEDEE,4)
!!!!      COMMON/   SDFLO   /QLINK,QDEFF,QSED
!*                             LINK FLOWS; PARTICLE FLOW RATES
!*--------------------- End of SED.CS ----------------------------------*
!PRIVATE :: NELEE, NLFEE, NSEDEE, NSEE
END MODULE sed_cs
