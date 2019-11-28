MODULE CMmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the CM COLM and LINK .F files
!
USE SGLOBAL
USE SGLOBAL, ONLY : nlf=>total_no_links, area=>cellarea, NEL=>total_no_elements
USE OCMOD2,  ONLY : hrf=>hrfzz
!!!!USE AL_P
USE AL_C  
USE AL_G
USE IS_CC
USE UTILSMOD, ONLY : TRIDAG
USE IS_CC
USE ALmod, ONLY: ALALLI, ALINIT, ALREDC, ALREDF, ALREDI, ALREDL, ALRED2
!!!USE ALmod, ONLY:ERROR, ERRC, ERRNEE, ERRTOT !AD NEEDS THIS  , HELPPATH
USE UTILSMOD, ONLY : DCOPY
IMPLICIT NONE

INTEGER :: JBK, JFLINK, JSOL(LLEE), NWORK(4), NLINKA, NCWELL  
DOUBLEPRECISION :: VELDUM (LLEE), QQQWEL, QQQWL1, QQRV(LLEE), ROH(LLEE)
LOGICAL :: ISBDY (4)  
!COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL  
!COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH  
!COMMON / WTOCL / ISBDY
INTEGER:: count = 0
INTEGER :: LWORK(6), NBK(2), nwell 
LOGICAL :: islk(2)
DOUBLEPRECISION ::  qqqdum, QQQSL1
!COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1  
!common / temp / nwell  
!!##### nwell and qqqdum used in temporary irrigation code###########


PRIVATE
PUBLIC :: CMSIM, CMFIN, CMRD
CONTAINS




!SSSSSS SUBROUTINE CMFIN  
SUBROUTINE CMFIN  
!                             CALLED FROM WATER FLOW COMPONENTS.
!                             TIDIES UP AT END OF SIMULATION.
RETURN  
END SUBROUTINE CMFIN



!SSSSSS SUBROUTINE CMRD (CMD, CPR, NCATEE, NCONEE, NELEE, NEL, NLF, NLFEE, &
SUBROUTINE CMRD (CMD, CPR, NCATEE, NCONEE, NELEE, NEL, NLF, NLFEE, &
 NSEE, NS, NSEDEE, NSED, NTABEE, NX, NXEE, NYEE, NY, NLYRBE, ICMXY, &
 ICMBK, ICMREF, BEXBK, LINKNS, NCAT, NCATTY, NCON, NCOLMB, NTAB, &
 DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB, CTAB, DTAB, IIICF, SOFN, &
 GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, ALPHA, FADS, &
 ISCNSV, IDUM, DUMMY)
!
!----------------------------------------------------------------------*
!
!  Read CM data input file
!
!----------------------------------------------------------------------*
!       Version: 4.2  Context: SHETRAN/CM
! Modifications:
! RAH  22.03.95  3.4.2  File created 01.02.95.
! SB 1.05.97 See below
!----------------------------------------------------------------------*
!
!
USE CONT_CC, ONLY:CCAPIN  
!
! Input arguments
INTEGER :: CMD, CPR, NCATEE, NCONEE, NELEE, NEL, NLF, NLFEE, NSEE, &
 NS
INTEGER :: NSEDEE, NSED, NTABEE, NX, NXEE, NYEE, NY  
INTEGER :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2), ICMREF (NELEE, 4, &
 2:2)
INTEGER :: NLYRBE (NLF + 1:NEL)  
LOGICAL :: BEXBK, LINKNS (NLFEE)  
!
! Output arguments
INTEGER :: NCON  
INTEGER :: NCAT (NCONEE), NCATTY (NELEE, NCONEE)  
INTEGER :: NCOLMB (NLF + 1:NEL), NTAB (NCATEE, NCONEE)  
DOUBLEPRECISION DBS, DBDI  
DOUBLEPRECISION CCAPI (NCONEE), CCAPE (NELEE, NCONEE)  
DOUBLEPRECISION CCAPR (NELEE, NCONEE), CCAPB (NELEE, NCONEE)  
DOUBLEPRECISION CTAB (NCATEE, NTABEE, NCONEE)  
DOUBLEPRECISION DTAB (NCATEE, NTABEE, NCONEE)  
DOUBLEPRECISION IIICF (NCONEE), SOFN (NSEE, 3)  
DOUBLEPRECISION GNN (NCONEE), GGLMSO (NCONEE)  
DOUBLEPRECISION ALPHBD (NCONEE), ALPHBS (NCONEE)  
DOUBLEPRECISION KDDLS (NSEDEE, NCONEE)  
DOUBLEPRECISION ALPHA (NSEE, NCONEE), FADS (NSEE, NCONEE)  
DOUBLEPRECISION PHIDAT (NSEE), DIFDAT (NCONEE), DISPDT (NSEE, &
 NCONEE)
!!!!LOGICAL :: ISFLXB, ISADNL  
LOGICAL :: ISCNSV (NCONEE)  
!
! Workspace arguments
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM  
DOUBLEPRECISION DUMMY (NELEE)  
!
! Locals, etc
INTEGER :: FATAL, rubbish (1, 1), j
PARAMETER (FATAL = 1)  
!
INTEGER :: I, IEL, INDX, NC, NCBC, NCED, NCLBND, NCONCM, NCONT  
INTEGER :: NDATA, NFEX, NMAX (3), NREQ, NSCM, NSEDCM, NTB, NTBL, &
 SOIL
LOGICAL :: LDUM (1)  
CHARACTER (80) :: CDUM(1)
CHARACTER(132) :: MSG  
!
!----------------------------------------------------------------------*
!
!
! Preliminaries
! -------------
!
!     * Check status of data file
CALL ALRED2 (0, CMD, CPR, 'CMD')  
!
!     * Print title for contaminant simulation
CALL ALREDC (0, CMD, CPR, ':CM1', 1, 1, CDUM)  
WRITE (CPR, '(/1X,A/)') CDUM  
!
!
! Some Static Data
! ----------------
!
!     * Number of contaminants
CALL ALREDI (0, CMD, CPR, ':CM3', 1, 1, IDUM)  
NCON = IDUM (1)  
!
!     * Flux boundary condition at base of column?
CALL ALREDL (0, CMD, CPR, ':CM5', 1, 1, LDUM)  
ISFLXB = LDUM (1)  
!
!
! Bottom Cell Data
! ----------------
!
!     * Default cell number at base of columns (-1 special: see below)
CALL ALREDI (0, CMD, CPR, ':CM7', 1, 1, IDUM)  
NCED = IDUM (1)  
!
!     * Number of columns where bottom cell number is not default value
CALL ALREDI (0, CMD, CPR, ':CM9', 1, 1, IDUM)  
NCLBND = IDUM (1)  
!
IF (NCLBND.GT.0) THEN  
!        * Column numbers & bottom cell numbers for those columns
   NREQ = 2 * NCLBND  
   IF (NREQ.GT.NELEE) GOTO 8090  
   CALL ALREDI (0, CMD, CPR, ':CM11', 2, NCLBND, IDUM)  
ENDIF  
!
!     * Assemble the above information: set the default ...
DO 110 IEL = NLF + 1, NEL  
   IF (NCED.EQ. - 1) THEN  
!           * special case
      NCOLMB (IEL) = NLYRBE (IEL)  
   ELSE  
      NCOLMB (IEL) = NCED  
   ENDIF  
  110 END DO  
!       ... then overwrite any non-default columns
INDX = 1  
DO 114 I = 1, NCLBND  
   IEL = IDUM (INDX)  
   IF (IEL.LE.NLF.OR.IEL.GT.NEL) GOTO 8110  
   NCOLMB (IEL) = IDUM (INDX + 1)  
   INDX = INDX + 2  
  114 END DO  
!
!
! More Static & Initialization Data
! ---------------------------------
!
!     * Non-linear adsorption?
CALL ALREDL (0, CMD, CPR, ':CM13', 1, 1, LDUM)  
ISADNL = LDUM (1)  
!
!     * Depth of bed surface layer
CALL ALREDF (0, CMD, CPR, ':CM15', 1, 1, DUMMY)  
DBS = DUMMY (1)  
!
!     * Initial depth of bed deep layer
CALL ALREDF (0, CMD, CPR, ':CM17', 1, 1, DUMMY)  
DBDI = DUMMY (1)  
!
!
! Local Data
! ----------
!
!     * Number of contaminants for which there are property data
CALL ALREDI (0, CMD, CPR, ':CM19', 1, 1, IDUM)  
NCONCM = IDUM (1)  
!
!     * Number of soil types for which there are contaminant data
CALL ALREDI (0, CMD, CPR, ':CM21', 1, 1, IDUM)  
NSCM = IDUM (1)  
!
!     * Number of sediment sizes for which there are contaminant data
CALL ALREDI (0, CMD, CPR, ':CM23', 1, 1, IDUM)  
NSEDCM = IDUM (1)  
!
!     * Set maximum admissible values for the above
NMAX (1) = MIN (NCONEE, NELEE)  
!      NMAX(2) = MIN( NSEE, NELEE/4, (NELEE/NCONCM))
nmax (2) = nsee  
!      NMAX(3) = MIN( NSEDEE, (NELEE/NCONCM) - 1 )
nmax (3) = nsedee  
!
! Initial Conditions
! ------------------
!






IF (NCONCM.LT.1.OR.NCONCM.GT.NMAX (1) ) GOTO 8190  
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! New code by SB for spatially distributed initial conditions
! -----------------------------------------------------------
! New variables
! External  ALALLI
! Input Variables
!     INTEGER NCATEE,NLFEE,NTABEE,NX,NXEE,NY,
!     INTEGER ICMXY(NXEE,NY),ICMBK(NLFEE,2),ICMREF(NELEE,4,2:2)
!     LOGICAL BEXBK,LINKNS(NLFEE)
! Output Variables
!      INTEGER NCAT(NCONEE),NTAB(NCATEE,NCONEE)
!      INTEGER NCATTY(NELEE,NCONEE)
!      DOUBLEPRECISION CTAB(NCATEE,NTABEE,NCONEE)
!      DOUBLEPRECISION DTAB(NCATEE,NTABEE,NCONEE)
!      LOGICAL ISCNSV(NCONEE)
! Local Variables
!     INTEGER NC,NDATA,NTBL,NTB
! Workspace
!     DUMMY wil work corrctly if NELEE >= 2 * NTABEE
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!
DO 260 I = 1, NCONCM  
!
!        Is the initial contaminant concentration spatially
!        variable ?
   CALL ALREDL (0, CMD, CPR, ':CM25', 1, 1, LDUM)  
   ISCNSV (I) = LDUM (1)  
!
   IF (.NOT.ISCNSV (I) ) THEN  
!
!           * Initial concentration throughout catchment
      CALL ALREDF (0, CMD, CPR, ':CM26', 1, 1, CCAPIN (I) )  
!
   ELSE  
!
!           * Initial concentration for link elements
      CALL ALREDF (0, CMD, CPR, ':CM26a', 1, 1, CCAPIN (I) )  
      DO J = 1, NLF  
      !"" NCATTY (J, I) = CCAPIN (I)  !AD
      ENDDO  
!
!          * Find out how many typical element catagories
      CALL ALREDI (0, CMD, CPR, ':CM26b', 1, 1, IDUM)  
      NCAT (I) = IDUM (1)  
      IF ( (NCAT (I) .GT.NCATEE) .OR. (NCAT (I) .LE.0) ) THEN  
CALL ERROR (FATAL, 2101, CPR, 0, 0, 'Error in NCAT in :CM26 in CM data file')
      ENDIF  
!
!           * Read the catagory type for each element into the element
!           * number
      CALL ALALLI (NCAT (I) , CMD, CPR, ':CM26c', NEL, NLF, NX, &
       NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
       NCATTY (NLF + 1, I) , IDUM)
!
!           * Table of values for each typical element
      DO 930 NC = 1, NCAT (I)  
!               CALL ALREDI(0,CMD,CPR,':CM26d',1,1,NTBL)
         CALL ALREDI (0, CMD, CPR, ':CM26d', 1, 1, rubbish)  
         ntbl = rubbish (1, 1)  
!
         NTAB (NC, I) = NTBL  
         IF ( (NTBL.GT.NTABEE) .OR. (NTBL.LE.0) ) THEN  
CALL ERROR (FATAL, 2102, CPR, 0, 0, 'Error in NTBL in :CM26a in CM data file')
         ENDIF  
!
         NDATA = NTBL * 2  
         CALL ALREDF (0, CMD, CPR, ':CM26e', NDATA, 1, DUMMY)  
         DO 940 NTB = 1, NTBL  
            DTAB (NC, NTB, I) = DUMMY (2 * NTB - 1)  
            CTAB (NC, NTB, I) = DUMMY (2 * NTB)  
  940          END DO  
  930       END DO  
   ENDIF  
!


  260 END DO  
!
!^^^^^^^^^^^^^^^^^^^^^^
! End of new code by SB
! ---------------------
!^^^^^^^^^^^^^^^^^^^^^^
!
! Boundary Conditions
! -------------------
!
!     * Concentrations in rainfall
CALL ALREDF (0, CMD, CPR, ':CM27', NCONCM, 1, CCAPI)  
!
!     * Number of columns which receive flow from outside catchment
CALL ALREDI (0, CMD, CPR, ':CM29', 1, 1, IDUM)  
NFEX = IDUM (1)  
!
IF (NFEX.GT.0) THEN  
!        * Numbers of those columns, and concentrations in the flows
!        * (read list index as extra column of floating-point data)
   NREQ = (1 + NCONCM) * NFEX  
   IF (NREQ.GT.NELEE) GOTO 8290  
   CALL ALREDF (0, CMD, CPR, ':CM31', 1 + NCONCM, NFEX, DUMMY)  
ENDIF  
!
!     * Assemble the above info
DO 310 NCONT = 1, NCONCM  
   CALL ALINIT (ZERO, NEL - NLF, CCAPE (NLF + 1, NCONT) )  
  310 END DO  
INDX = 1  
DO 312 I = 1, NFEX  
   IEL = NINT (DUMMY (INDX) )  
   IF (IEL.LE.NLF.OR.IEL.GT.NEL) GOTO 8310  
   CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPE (IEL, 1), &
    NELEE)
   INDX = INDX + 1 + NCONCM  
  312 END DO  
!
!     * Default concentration at or convected into bases of columns
CALL ALREDF (0, CMD, CPR, ':CM33', NCONCM, 1, DUMMY)  
DO 330 NCONT = 1, NCONCM  
   IF (ISFLXB) THEN  
      CALL ALINIT (DUMMY (NCONT), NEL - NLF, CCAPR (NLF + 1, &
       NCONT) )
   ELSE  
      CALL ALINIT (DUMMY (NCONT), NEL - NLF, CCAPB (NLF + 1, &
       NCONT) )
   ENDIF  
  330 END DO  
!
!     * Number of columns where base concentration is not default value
CALL ALREDI (0, CMD, CPR, ':CM35', 1, 1, IDUM)  
NCBC = IDUM (1)  
!
IF (NCBC.GT.0) THEN  
!        * Numbers and concentrations for those columns
!        * (read list index as extra column of floating-point data)
   NREQ = (1 + NCONCM) * NCBC  
   IF (NREQ.GT.NELEE) GOTO 8350  
   CALL ALREDF (0, CMD, CPR, ':CM37', 1 + NCONCM, NCBC, DUMMY)  
   INDX = 1  
   DO 370 I = 1, NCBC  
      IEL = NINT (DUMMY (INDX) )  
      IF (IEL.LE.NLF.OR.IEL.GT.NEL) GOTO 8370  
      IF (ISFLXB) THEN  
         CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPR (IEL, 1), &
          NELEE)
      ELSE  
         CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPB (IEL, 1), &
          NELEE)
      ENDIF  
      INDX = INDX + 1 + NCONCM  
  370    END DO  
ENDIF  
!
!     * Rate of dry deposition, for each contaminant
CALL ALREDF (0, CMD, CPR, ':CM39', NCONCM, 1, IIICF)  
!
!
! Some Soil Properties
! --------------------
!
IF (NSCM.LT.1.OR.NSCM.GT.NMAX (2) ) GOTO 8210  
!
!     * 3 size fractions (used only if SY module inactive)
!     * (read soil index as extra column of floating-point data)
CALL ALREDF (0, CMD, CPR, ':CM41', 4, NSCM, DUMMY)  
INDX = 1  
DO 410 I = 1, NSCM  
   SOIL = NINT (DUMMY (INDX) )  
   IF (SOIL.LT.1.OR.SOIL.GT.NSCM) GOTO 8410  
   CALL DCOPY (3, DUMMY (INDX + 1), 1, SOFN (SOIL, 1), NSEE)  
   INDX = INDX + 4  
  410 END DO  
!
!
! Some Contaminant Properties
! ---------------------------
!
!     * Freundlich isotherm power constant
CALL ALREDF (0, CMD, CPR, ':CM43', NCONCM, 1, GNN)  
!
!     * Chemical decay constant
CALL ALREDF (0, CMD, CPR, ':CM45', NCONCM, 1, GGLMSO)  
!
!     * Coefficients for exchange between bed layers
CALL ALREDF (0, CMD, CPR, ':CM47', NCONCM, 1, ALPHBD)  
!
!     * Coefficients for exchange between water and bed
CALL ALREDF (0, CMD, CPR, ':CM49', NCONCM, 1, ALPHBS)  
!
!
! More Contaminant/Sediment/Soil Properties
! -----------------------------------------
!
IF (NSEDCM.LT.1.OR.NSEDCM.GT.NMAX (3) ) GOTO 8230  
!
!     * Reference Kd for each particle size
!     * (read contaminant index as extra column of floating-point data)
CALL ALREDF (0, CMD, CPR, ':CM51', 1 + NSEDCM, NCONCM, DUMMY)  
INDX = 1  
DO 510 I = 1, NCONCM  
   NCONT = NINT (DUMMY (INDX) )  
   IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8510  
   CALL DCOPY (NSEDCM, DUMMY (INDX + 1), 1, KDDLS (1, NCONT), &
    1)
   INDX = INDX + 1 + NSEDCM  
  510 END DO  
!
!     * Coefficients for exchange between soil regions
CALL ALREDF (0, CMD, CPR, ':CM53', 1 + NSCM, NCONCM, DUMMY)  
INDX = 1  
DO 530 I = 1, NCONCM  
   NCONT = NINT (DUMMY (INDX) )  
   IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8530  
   CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, ALPHA (1, NCONT), &
    1)
   INDX = INDX + 1 + NSCM  
  530 END DO  
!
!     * Fraction of adsorption sites in dynamic region
CALL ALREDF (0, CMD, CPR, ':CM55', 1 + NSCM, NCONCM, DUMMY)  
INDX = 1  
DO 550 I = 1, NCONCM  
   NCONT = NINT (DUMMY (INDX) )  
   IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8550  
   CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, FADS (1, NCONT), &
    1)
   INDX = INDX + 1 + NSCM  
  550 END DO  
!
!     * Fraction of pore water in dynamic region
CALL ALREDF (0, CMD, CPR, ':CM57', NSCM, 1, PHIDAT)  
!
!     * Diffusion coefficient
CALL ALREDF (0, CMD, CPR, ':CM59', NCONCM, 1, DIFDAT)  
!
!     * Dispersivity
CALL ALREDF (0, CMD, CPR, ':CM61', 1 + NSCM, NCONCM, DUMMY)  
INDX = 1  
DO 610 I = 1, NCONCM  
   NCONT = NINT (DUMMY (INDX) )  
   IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8610  
   CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, DISPDT (1, NCONT), &
    1)
   INDX = INDX + 1 + NSCM  
  610 END DO  
!
!
! Epilogue
! -----------
!
!     * Close the data file
CALL ALRED2 (1, CMD, CPR, 'CMD')  
!
!     * Is everything defined?
IF (NCONCM.LT.NCON.OR.NSCM.LT.NS.OR.NSEDCM.LT.NSED) GOTO 8000  
!
RETURN  
!
!
! Error Branches
! --------------
!
!     * Not enough data
 8000 WRITE (MSG, 9800) NCONCM, NSCM, NSEDCM, NCON, NS, NSED  
CALL ERROR (FATAL, 3008, CPR, 0, 0, MSG)  
!
!     * Insufficient workspace
 8090 WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM9: NCLBND ', NCLBND
CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)  
!
!     * Invalid column number
 8110 WRITE (MSG, 9811) IEL, 'CM11', 'column element'  
CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)  
!
!     * Too many contaminants
 8190 WRITE (MSG, 9819) 'contaminants', 'CM19: NCONCM', NCONCM, NMAX (1)  
CALL ERROR (FATAL, 3003, CPR, 0, 0, MSG)  
!
!     * Too many soil types
 8210 WRITE (MSG, 9819) 'soil types', 'CM21: NSCM', NSCM, NMAX (2)  
CALL ERROR (FATAL, 3004, CPR, 0, 0, MSG)  
!
!     * Too many sediment sizes
 8230 WRITE (MSG, 9819) 'sediment sizes', 'CM23: NSEDCM', NSEDCM, NMAX ( &
 3)
CALL ERROR (FATAL, 3005, CPR, 0, 0, MSG)  
!
!     * Insufficient workspace
 8290 WRITE (MSG, 9809) NELEE, NREQ, 'flow-receiving columns', 'CM29: NFEX', NFEX
CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)  
!
!     * Invalid column number
 8310 WRITE (MSG, 9811) IEL, 'CM31', 'column element'  
CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)  
!
!     * Insufficient workspace
 8350 WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM35: NCBC' &
&, NCBC
CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)  
!
!     * Invalid column number
 8370 WRITE (MSG, 9811) IEL, 'CM37', 'column element'  
CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)  
!
!     * Invalid soil type
 8410 WRITE (MSG, 9811) SOIL, 'CM41', 'soil type'  
CALL ERROR (FATAL, 3006, CPR, 0, 0, MSG)  
!
!     * Invalid contaminant number
 8510 WRITE (MSG, 9811) NCONT, 'CM51', 'contaminant number'  
CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)  
!
!     * Invalid contaminant number
 8530 WRITE (MSG, 9811) NCONT, 'CM53', 'contaminant number'  
CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)  
!
!     * Invalid contaminant number
 8550 WRITE (MSG, 9811) NCONT, 'CM55', 'contaminant number'  
CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)  
!
!     * Invalid contaminant number
 8610 WRITE (MSG, 9811) NCONT, 'CM61', 'contaminant number'  
CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)  
!
!
! Formats
! -------
!
 9800 FORMAT ('No. of contaminants/soils/sediments with data' &
&       ,' (CM19-23: NCONCM/NSCM/NSEDCM = ',2(I3,'/'),I3,')' &
&       ,' must be at least ',2(I3,'/'),I3)
!
!     * length 91+2A
 9809 FORMAT ('Insufficient workspace (have NELEE =',I6,', need',I6,')' &
&       ,' for the number of ',A,' given (',A,' =',I6,')')
!
!     * length 57+2A
 9811 FORMAT ('Index',I6,' (given as part of data item ',A,')' &
&       ,' is not a valid ',A)
!
!     * length 73+2A
 9819 FORMAT ('Number of ',A,' with data (',A,' =',I6,')' &
&       ,' must be positive & not greater than',I6)
!
!
END SUBROUTINE CMRD



!SSSSSS SUBROUTINE CMSIM (ISSDON)  
SUBROUTINE CMSIM (ISSDON)  
!----------------------------------------------------------------------*
!                             ENTRY POINT TO THE CONTAMINANT COMPONENTS
!                             WHEN UPDATING THE CONTAMINANT
!                             CONCENTRATIONS FOR THE WHOLE CATCHMENT
!                             FOR ONE TIME STEP
!----------------------------------------------------------------------*
! Version:  /SHETRAN/MUZ/CMSIM/4.1
! Modifications:
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
! RAH  950322  4.0  New VSS: replace RSZWEL with QVSWEL.
! RAH  970313  4.1  Explicit typing.
!----------------------------------------------------------------------*
! Commons and constants
USE SED_CS  
USE CONT_CC  
USE COLM_C1  
USE COLM_CO  
USE LINK_CW  
!                             INCLUDE THE PARAMETER STATEMENTS
!                             AND THE WATER/CONTAMINANT INTERFACE
!                             COMMON BLOCKS
!
! Input arguments
LOGICAL :: ISSDON  
!                             ANSWER TO: IS SEDIMENT CODE ACTIVE?
!
! Locals, etc

INTEGER :: NLINK, NDUM, NELM, NCONT, NCE  
!
!----------------------------------------------------------------------*
!
IF (.NOT.ISSDON) THEN  
   DO 100 NLINK = 1, NLF  
      IF (LINKNS (NLINK) ) THEN  
         QLINK (NLINK, 1) = - QOC (NLINK, 2)  
         QLINK (NLINK, 2) = QOC (NLINK, 4)  
      ELSE  
         QLINK (NLINK, 1) = - QOC (NLINK, 1)  
         QLINK (NLINK, 2) = QOC (NLINK, 3)  
      ENDIF  
  100    END DO  


ENDIF  
!                             IF THE SEDIMENT CODE IS NOT RUNNING, SET
!                             UP FLOWS INTO LINKS

TSE = D0 * DTUZ / Z2SQ  
!                            SET NON-DIMENSIONED TIME STEP

IF (ISPLT) CALL PLPREP  
!                            Prepare for plant uptake calculations
DO 1 NDUM = 1, NEL  
   NELM = ISORT (NDUM)  
   IF (NELM.GT.NLF) THEN  
      CALL COLMW (NELM)  
      CALL COLMSM (NELM)  
   ELSE  
      CALL LINKW (NELM)  
      CALL LINKSM (NELM)  
   ENDIF  


    1 END DO  
!                             STEP THROUGH COLUMNS AND LINKS
!                             UPDATING THE CONCENTRATIONS IN THE
!                             CATCHMENT ARRAYS CCCC AND SSSS
DO 10 NCONT = 1, NCON  
   DO 11 NELM = 1, NLF  
      DO 12 NCE = NCETOP - 2, NCETOP  
         CCCCO (NELM, NCE, NCONT) = CCCC (NELM, NCE, NCONT)  
   12       END DO  

   11    END DO  
   DO 13 NELM = NLF + 1, NEL  
!#######################################################################
      RSZWLO (NELM) = QVSWEL (NELM)  
!                               put here temporarily after introduction
!                                of irrigation
!#######################################################################
      DO 14 NCE = NLYRBT (NELM, 1), NCETOP  
         CCCCO (NELM, NCE, NCONT) = CCCC (NELM, NCE, NCONT)  
         SSSSO (NELM, NCE, NCONT) = SSSS (NELM, NCE, NCONT)  
   14       END DO  
   13    END DO  

   10 END DO  
!                             SAVE THE NEW CONCENTRATIONS, FOR THE
!                             ENTIRE CATCHMENT, FOR USE AT THE NEXT
!                             TIME LEVEL
END SUBROUTINE CMSIM



!SSSSSS SUBROUTINE COLM  
SUBROUTINE COLM  
!----------------------------------------------------------------------*
!                            UPDATES CONCENTRATION FOR ONE
!                            CONTAMINANT IN ONE COLUMN (BETWEEN
!                            CELLS NCEBOT AND NCETOP): RETURNS
!                            CCAP AND SCAP VECTORS
!----------------------------------------------------------------------*
! Version:  /SHETRAN/MUZ/COLM/4.0
! Modifications:
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
! RAH  950509  4.0  Incorporate KSP into expressions for OCAPP & OCAPP1.
! RAH  970313  4.1  Explicit typing.  Generic intrinsics.
!----------------------------------------------------------------------*
! Commons and constants
USE COLM_C1  
USE COLM_C2  
USE COLM_CC  
USE COLM_CC1  
!
! Locals, etc
!INTRINSIC ABS, SIGN  
INTEGER :: NC, J, NCADJ, NDUM  
DOUBLEPRECISION TTHT, TTHT1, PPHITH, PPHIT1, PPHTHP, PPHTP1  
DOUBLEPRECISION KKD, MCAP, MCAPC, MCAPT, WORKA (LLEE), WORKB ( &
 LLEE)
DOUBLEPRECISION FFKD, GGNMON, AALPH  
DOUBLEPRECISION SUM1, SUM2, SUM3, CBCAPC, OMCBCC, CBCAP, CBCAPT  
DOUBLEPRECISION ANCAP, ANCAPT, ANCAPS, BCAP, BCAP1, BCAPSG  
DOUBLEPRECISION FCAP, FCAPT, FCAPC  
DOUBLEPRECISION GCAP, GCAPT, GCAPS, GMCAP, GMCAP1, GMCPSG  
DOUBLEPRECISION OCAPM, OCAPP, OCAPM1, OCAPP1  
DOUBLEPRECISION PCAPM, PCAPP, PCAPM1, PCAPP1  
DOUBLEPRECISION BPGSG, BMGSG, DUMMY  
DOUBLEPRECISION ALT, ALT1, HLT, HLT1, BLT, BLT1, ALTSG, HLTSG, &
 BLTSG
DOUBLEPRECISION VCAP, VCAP1  
DOUBLEPRECISION CBSWC, OMCBSC, CBSW, CBSWT, RRRB, RRRBT, RRRBC  
DOUBLEPRECISION CCPRFC, OMCRFC, CBRF, CBRFT  
!
!----------------------------------------------------------------------*
!
OCAPP = zero  
OCAPP1 = zero  
PCAPP = zero  
PCAPP1 = zero  
DO 1 NC = NCEBOT, NCETOP  
!     ^^^^^^^^^^^^^^^^^^^^^^ MAIN LOOP - SETS ELEMENTS, FOR ALL
!                                        CELLS, FOR VECTORS FOR
!                                        DIFFERENCE EQUATIONS
   TTHT = TTHET (NC)  
   TTHT1 = TTHET1 (NC)  
   PPHITH = PPHI (NC) * TTHT  
   PPHIT1 = PPHI1 (NC) * TTHT1  
   PPHTHP = PPHI (NC + 1) * TTHET (NC + 1)  
   PPHTP1 = PPHI1 (NC + 1) * TTHET1 (NC + 1)  
   KKD = KKDSO (NC)  
   FFKD = FFSO (NC) * KKD  
   GGNMON = GGNNSO (NC) - one  


   AALPH = AALPSO (NC)  
!                            SET DEPTH AND SOIL DEPENDENT
!                            VARIABLES
   SUM1 = zero  
   SUM2 = zero  
   SUM3 = zero  
   IF (NC.LE. (NCEPSF + 1) ) THEN  
      DO 2 J = 1, 4  
         CBCAPC = half - SIGN (half, QQ (NC, J) )  
         OMCBCC = one - CBCAPC  
         CBCAP = OMCBCC * CCAPA (NC, J) + CBCAPC * COLCAP (NC)  
         CBCAPT = OMCBCC * CCAPAT (NC, J)  
         SUM1 = SUM1 + QQ (NC, J) * CBCAP  
         SUM2 = SUM2 + (QQ1 (NC, J) / ZONE1 - QQ (NC, J) / ZONE) &
          * CBCAP + TSE * QQ (NC, J) * CBCAPT / ZONE
         SUM3 = SUM3 + QQ (NC, J) * CBCAPC  
    2       END DO  
   ENDIF  
!                            SUM CONVECTION TERMS OVER FOUR FACES
   MCAP = GNERD (NC) - EDCAP (NC) + CST1 * SUM1  
   MCAPT = GND2 (NC) - EDCAPT (NC) + CST1 * SUM2 * ZONE / TSE  
   MCAPC = - EDCAPC (NC) + CST1 * SUM3  
   ANCAP = GNDSE (NC) - ESCAP (NC)  
   ANCAPT = GNDSE2 (NC) - ESCAPT (NC)  
   ANCAPS = - ESCAPS (NC)  
   BCAP = Z2SQOD * (AALPH + half * ABS (GGAMM (NC) ) )  
   BCAP1 = Z2SQOD * (AALPH + half * ABS (GGAMM1 (NC) ) )  
   BCAPSG = OMSGMA * BCAP + SGMA * BCAP1  
   FCAP = PPHITH + FFKD * COLCAP (NC) **GGNMON  
   FCAPT = (PPHIT1 - PPHITH) / TSE  
   FCAPC = GGNMON * (FCAP - PPHITH) / COLCAP (NC)  
   GND2 (NC) = GCAPLA * FCAPT * COLCAP (NC)  
   GNERD (NC) = GCAPLA * COLCAP (NC) * FCAP + SGTSE * GND2 (NC)  
   WORKA (NC) = GCAPLA * FCAP * SGTSE  
!                            SET GENERATION TERMS FOR DYNAMIC REGION
!                            A FURTHER TERM WILL BE ADDED TO GENRD LATER
   GCAP = TTHT - PPHITH + (KKD-FFKD) * SOLCAP (NC) **GGNMON  
   GCAPT = (TTHT1 - PPHIT1 - TTHT + PPHITH) / TSE  
   GCAPS = GGNMON * (GCAP - TTHT + PPHITH) / SOLCAP (NC)  
   GNDSE2 (NC) = GCAPLA * GCAPT * SOLCAP (NC)  
   GNDSE (NC) = GCAPLA * SOLCAP (NC) * GCAP + SGTSE * GNDSE2 (NC)  
   WORKB (NC) = GCAPLA * GCAP * SGTSE  
!                            SET GENERATION TERMS FOR DEAD SPACE
!                            A FURTHER TERM WILL BE ADDED TO GNDSE LATER
   GMCAP = Z2SQOD * GGAMM (NC) / two  
   GMCAP1 = Z2SQOD * GGAMM1 (NC) / two  
   GMCPSG = OMSGMA * GMCAP + SGMA * GMCAP1  
   OCAPM = OCAPP  
   OCAPP = two * PPHITH * DDOD (NC) * PPHTHP * DDOD (NC + 1) &
    * KSP (NC) * KSP (NC + 1) / (PPHITH * DDOD (NC) * KSP (NC + 1) &
    + PPHTHP * DDOD (NC + 1) * KSP (NC) )
!                            WEIGHTED HARMONIC MEAN
   OCAPM1 = OCAPP1  
   OCAPP1 = two * PPHIT1 * DDOD1 (NC) * PPHTP1 * DDOD1 (NC + 1) &
    * KSP (NC) * KSP (NC + 1) / (PPHIT1 * DDOD1 (NC) * KSP (NC + 1) &
    + PPHTP1 * DDOD1 (NC + 1) * KSP (NC) )
   PCAPM = PCAPP  
   PCAPP = Z2OD * UUAJP (NC)  
   PCAPM1 = PCAPP1  


   PCAPP1 = Z2OD * UUAJP1 (NC)  
!                            SET VALUES FOR NON-DIMENSIONED
!                            VARIABLES
   BPGSG = BCAPSG + GMCPSG  
   BMGSG = BCAPSG - GMCPSG  
   DUMMY = one / KSP (NC)  
   ALT = DUMMY * MAX (zero, OCAPP / KSPP (NC) - half * PCAPP, &
    - PCAPP)
   ALT1 = DUMMY * MAX (zero, OCAPP1 / KSPP (NC) - half * &
    PCAPP1, - PCAPP1)
   HLT = DUMMY * MAX (zero, OCAPM / KSPP (NC - 1) + half * &
    PCAPM, PCAPM)
   HLT1 = DUMMY * MAX (zero, OCAPM1 / KSPP (NC - 1) + half * &
    PCAPM1, PCAPM1)
   BLT = - ALT - HLT - DUMMY * (PCAPP - PCAPM)  
   BLT1 = - ALT1 - HLT1 - DUMMY * (PCAPP1 - PCAPM1)  
   ALTSG = OMSGMA * ALT + SGMA * ALT1  
   HLTSG = OMSGMA * HLT + SGMA * HLT1  


   BLTSG = OMSGMA * BLT + SGMA * BLT1  
!                            SET WORKING VALUES, AND
!                            COEFFICIENTS (A, B, AND H) FOR COMBINED
!                            CONVECTION AND DDERSION TERM


   NCADJ = NC - NCEBOT + 1  
!                            ADJUST CELL NUMBERS SO THE COEFFICIENTS
!                            BELOW ARE SET FOR NCADJ=1,2,3 ETC
   DLT (NCADJ) = - SGTSE * ALTSG  
   ELT (NCADJ) = SGTSE * ( - BLTSG + BPGSG) + OPSGL * (FCAP + &
    FCAPC * COLCAP (NC) ) + OPSGSL * TSE * FCAPT - SGTSE * MCAPC
   ELTSTR (NCADJ) = OPSGSL * TSE * FCAPC  
   FLT (NCADJ) = - SGTSE * HLTSG  
   GLT (NCADJ) = SGTSE * BMGSG  
   PLT (NCADJ) = SGTSE * BMGSG + OPSGL * (GCAP + GCAPS * SOLCAP ( &
    NC) ) + OPSGSL * TSE * GCAPT - SGTSE * ANCAPS
   PLTSTR (NCADJ) = OPSGSL * TSE * GCAPS  
   QLT (NCADJ) = - (GCAPLA * GCAP + BMGSG + OPSGL * GCAPT) &
    * SOLCAP (NC) + BPGSG * COLCAP (NC) + ANCAP + SGTSE * ANCAPT
   SLT (NCADJ) = ALTSG * COLCAP (NC + 1) + (BLTSG - BPGSG - &
    GCAPLA * FCAP - OPSGL * FCAPT) * COLCAP (NC) + HLTSG * COLCAP ( &
    NC - 1) + BMGSG * SOLCAP (NC) + MCAP + SGTSE * MCAPT


   TLT (NCADJ) = SGTSE * BPGSG  
!                            SET ELEMENTS, FOR INTERNAL CELLS,
!                            OF THE VECTORS FOR THE DIFFERENCE
!                            EQUATIONS



    1 END DO  
!     ^^^^^^^^^^^^^^^^^^^^^^ END OF MAIN LOOP
NC = NCETOP  
VCAP = GGGNU * Z2OD  
VCAP1 = GGGNU1 * Z2OD  
SUM1 = zero  
SUM2 = zero  
SUM3 = zero  
DO 3 J = 1, 4  
   CBSWC = half - SIGN (half, QQQSW (J) )  
   OMCBSC = one - CBSWC  
   CBSW = OMCBSC * CSWA (J) + CBSWC * COLCAP (NCETOP)  
   CBSWT = OMCBSC * CSWAT (J)  
   RRRB = OMCBSC * RRRSWA (J) + CBSWC * RRRSW  
   RRRBT = OMCBSC * RRRSAT (J) + CBSWC * RRRSWT  
   RRRBC = CBSWC * RRRSWC  
   SUM1 = SUM1 + QQQSW (J) * RRRB * CBSW  
   SUM2 = SUM2 + (QQQSW1 (J) - QQQSW (J) ) * RRRB * CBSW + QQQSW ( &
    J) * TSE * (RRRB * CBSWT + RRRBT * CBSW)
   SUM3 = SUM3 + QQQSW (J) * (RRRB * CBSWC + RRRBC * CBSW)  
    3 END DO  
!                            SUM CONVECTION TERMS OVER FOUR FACES
MCAP = MCAP + (VCAP * (FCAP * COLCAP (NC) + GCAP * SOLCAP (NC) ) &
 - ESSCAP - ICAP - QCAP + CST2 * SUM1) / KSP (NC)
!                            THE GENERATION TERM FOR SOIL,
!                            SURFACE WATER, AND SEDIMENTS IS INCLUDED
!                            IN MCAP AS SET IN THE MAIN LOOP
MCAPT = MCAPT + ( (VCAP1 - VCAP) * (FCAP * COLCAP (NC) + GCAP * &
 SOLCAP (NC) ) / TSE+VCAP * (FCAPT * COLCAP (NC) + GCAPT * SOLCAP ( &
 NC) ) - ESSCPT - ICAPT - QCAPT + CST2 * SUM2 / TSE) / KSP (NC)
MCAPC = MCAPC + (VCAP * (FCAPC * COLCAP (NC) + FCAP) - ESSCPC - &
 ICAPC - QCAPC + CST2 * SUM3) / KSP (NC)
!                            THE FOLLOWING CODE MUST COME AFTER
!                            MCAP IS OVERWRITTEN
FCAP = FCAP + (DDDSW * RRRSW + DDDLS * TTTLSE * RRRLS) / (Z2 * &
 KSP (NC) )
FCAPT = FCAPT + (RRRSW * (DDDSW1 - DDDSW) + TTTLSE * RRRLS * &
 (DDDLS1 - DDDLS) + TSE * (DDDSW * RRRSWT + DDDLS * TTTLSE * &
 RRRLST) ) / (TSE * KSP (NC) * Z2)
FCAPC = FCAPC + (DDDSW * RRRSWC + DDDLS * TTTLSE * RRRLSC) &
 / (KSP (NC) * Z2)
!                            ADD EFFECT OF SURFACE WATER AND SED. TO F
GND2 (NC) = GCAPLA * FCAPT * COLCAP (NC)  
GNERD (NC) = GCAPLA * COLCAP (NC) * FCAP + SGTSE * GND2 (NC)  
WORKA (NC) = GCAPLA * FCAP * SGTSE  
BLT = - HLT + DUMMY * PCAPM  
BLT1 = - HLT1 + DUMMY * PCAPM1  
BLTSG = OMSGMA * BLT + SGMA * BLT1  
NCADJ = NC - NCEBOT + 1  
DLT (NCADJ) = zero  
ELT (NCADJ) = SGTSE * ( - BLTSG + BPGSG) + OPSGL * (FCAP + FCAPC * &
 COLCAP (NC) ) + OPSGSL * TSE * FCAPT - SGTSE * MCAPC
ELTSTR (NCADJ) = OPSGSL * TSE * FCAPC  


SLT (NCADJ) = (BLTSG - BPGSG - GCAPLA * FCAP - OPSGL * FCAPT) &
 * COLCAP (NC) + HLTSG * COLCAP (NC - 1) + BMGSG * SOLCAP (NC) &
 + MCAP + SGTSE * MCAPT
!                            OVERWRITE VECTOR ELEMENTS
!                            FOR THE TOP CELL
NC = NCEBOT  
IF (ISFLXB) THEN  
   CCPRFC = half - SIGN (half, QQRF)  
   OMCRFC = one - CCPRFC  
   CBRF = OMCRFC * CCPRF + CCPRFC * COLCAP (NC)  
   CBRFT = OMCRFC * CCPRFT  
   ELT (1) = ELT (1) - CST3 * SGTSE * QQRF * CCPRFC  
   SLT (1) = SLT (1) + CST3 * QQRF * CBRF  
   SLT (1) = SLT (1) + CST3 * SGTSE * ( (QQRF1 - QQRF) * CBRF + &
    TSE * QQRF * CBRFT) / TSE
ELSE  
   DLT (1) = zero  
   ELT (1) = one  
   ELTSTR (1) = zero  
   FLT (1) = zero  
   GLT (1) = zero  
   SLT (1) = (CCAP (NCEBOT) - COLCAP (NCEBOT) ) / TSE  


ENDIF  
!                            OVERWRITE VECTOR ELEMENTS
!                            FOR THE BOTTOM CELL
NDUM = NCETOP - NCEBOT + 1  



CALL SLVCLM (NDUM)  
!                            SOLVE THE DIFFERENCE EQUATIONS
!                            FOR THE EPSILON AND OMEGA VECTORS
DO 4 NC = NCEBOT, NCETOP  
   NCADJ = NC - NCEBOT + 1  
   CCAP (NC) = COLCAP (NC) + OME (NCADJ) * TSE  
   SCAP (NC) = SOLCAP (NC) + EPS (NCADJ) * TSE  
   GNERD (NC) = GNERD (NC) + WORKA (NC) * OME (NCADJ)  
   GNDSE (NC) = GNDSE (NC) + WORKB (NC) * EPS (NCADJ)  

    4 END DO  
!                            SET ELEMENTS OF CONCENTRATION VECTORS
!                            AND GENERATION VECTORS
END SUBROUTINE COLM



!SSSSSS SUBROUTINE COLMSM (NCL)  
SUBROUTINE COLMSM (NCL)  
!----------------------------------------------------------------------*
!                            UPDATES THE CONCENTRATIONS OF EACH
!                            CONTAMINANT IN COLUMN NCL
!----------------------------------------------------------------------*
! Version:  /SHETRAN/MUZ/COLMSM/4.1
! Modifications:
! GP   930930  3.4  Initialize EDCAP* & ESCAP* (case .not.ISPLT).
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
! GP   960717  4.0  Add to /WTOC/: JFACEA,QQQWEL,QQQWL1,NCWELL  (COLMW).
!                   Loop 14: replace JSUMQ,JKZCOL with SUMQ,QQ1 (COLMW);
!                   and test NOLBT>0.  Also set CCAPA=0 if SUMQ=0.
!                   Add loop 19 (to calculate CCAPA beneath channel).
!                   Incorporate QQQW* into calculation of QCAP & QCAPT.
! RAH  970314  4.1  Explicit typing.  Split mixed-type /WTOC/ (COLMW).
!                   Generic intrinsics.
!      970521       Remove JFACEA,NAQU,TRAN (redundant in /WTOC/).
!----------------------------------------------------------------------*
! Commons and constants
USE SED_CS  
USE CONT_CC  
USE COLM_C1  
USE COLM_C2  
USE COLM_CC  
USE COLM_CG  
USE BK_CW  
USE SED_CO  

USE PLANT_CC  
!! Input common
!INTEGER :: JBK, JFLINK, JSOL (LLEE), NWORK (4), NLINKA, NCWELL  
!DOUBLEPRECISION VELDUM (LLEE), QQQWEL, QQQWL1, QQRV (LLEE), &
! ROH (LLEE)
!LOGICAL :: ISBDY (4) 
!COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL  
!COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH  

!COMMON / WTOCL / ISBDY  
!                             VARIABLES USED ONLY IN COLMW AND COLMSM
! Input arguments

INTEGER :: NCL  
! Locals, etc
!INTRINSIC ABS, MAX, MIN  
INTEGER :: NCONT, NCE, JA, NDUM, NOLDUM, NOLP, JCEA, JSED  
DOUBLEPRECISION CCBT, SUM, SUMQ, SUMQC, SUMW  
DOUBLEPRECISION :: DUM, DUM0, DUM1, DUM2, DUM3, DUMBED, CDUM=0.0d0 
DOUBLEPRECISION GNDUM, QDUM, QCDUM, QCDUM1, UDUMP, UDUMM, UCDUMP, &
 UCDUMM

DOUBLEPRECISION FBO (NSEDEE), FB (NSEDEE), FDLO (NSEDEE), FDL ( &
 NSEDEE), KDDUM (NSEDEE)
!----------------------------------------------------------------------*
!
DO 1 NCE = 1, LLEE  
   GNERD (NCE) = zero  
   GNDSE (NCE) = zero  
   GND2 (NCE) = zero  
   GNDSE2 (NCE) = zero  



    1 END DO  
!                             SET GENERATION VARIABLES TO ZERO IN
!                             PREPARATION FOR THE 1ST PASS OF DO LOOP 5

DO 5 NCONT = 1, NCON  
!                             +++++ MAIN LOOP FOR UPDATING CONCS ++++++
   DO 6 NCE = NCEBOT - 1, NCETOP + 1  
      COLCAP (NCE) = CCCCO (NCL, NCE, NCONT)  
      SOLCAP (NCE) = SSSSO (NCL, NCE, NCONT)  

    6    END DO  
!                             SET OLD CONCENTRATION VECTORS

   GCAPLA = GCPLA (NCONT)  
   DO 11 NCE = NCEBOT, NCETOP  
      DDOD (NCE) = OODO * DISP (NCONT, JSOL (NCE), TTHET (NCE), &
       UUAJP (NCE-1), UUAJP (NCE) )
      DDOD1 (NCE) = OODO * DISP (NCONT, JSOL (NCE), TTHET1 (NCE), &
       UUAJP1 (NCE-1), UUAJP1 (NCE) )
      AALPSO (NCE) = ALPHA (JSOL (NCE), NCONT)  
      FFSO (NCE) = FADS (JSOL (NCE), NCONT)  
      GGNNSO (NCE) = GNN (NCONT)  
      KKDSO (NCE) = KDDSOL (JSOL (NCE), NCONT)  
   11    END DO  
   DDOD (NCETOP + 1) = zero  

   DDOD1 (NCETOP + 1) = zero  
!                            SET THE EFFECTIVE DISPERSION COEFFICIENTS
!                            AND OTHER SOIL PROPERTIES

   DO 12 JA = 1, 4  

      IF (.NOT.ISBDY (JA) .AND. (JA.NE.JFLINK) ) THEN  
!                             IS NOT FACE AT CATCHMENT BOUNDARY OR THE
!                             EXPOSED FACE OF A BANK
         NDUM = NCEPSF + 1  
         DO 13 NCE = NCEBOT, MIN (NDUM, NCETOP)  
            SUMQ = zero  
            SUMQC = zero  
            NOLDUM = MAX (1, NOLBT (NCL, NCE, JA) )  
            DO 14 NOLP = NOLDUM, NOLBT (NCL, NCE+1, JA) - 1  
               JCEA = NOLCEA (NCL, NOLP, JA)  
               QDUM = QQ1 (NCE, JA)  
               SUMQ = SUMQ + QDUM  
               SUMQC = SUMQC + QDUM * CCCCO (NWORK (JA), JCEA, &
                NCONT)
   14             END DO  
            IF (NOTZERO(SUMQ)) SUMQ = SUMQC / SUMQ  
            CCAPA (NCE, JA) = SUMQ  
            CCAPAT (NCE, JA) = zero  
   13          END DO  
!                             EXPLICIT (IN C) LATERAL COUPLING IN
!                             SUBSURFACE
         CSWA (JA) = CCCCO (NWORK (JA), NCETOP, NCONT)  
         CSWAT (JA) = (CCCC (NWORK (JA), NCETOP, NCONT) - CSWA ( &
          JA) ) / TSE
         RRRSWA (JA) = RSW (NWORK (JA), NCONT)  

         RRRSAT (JA) = RSWT (NWORK (JA), NCONT) + RSWC (NWORK (JA) &
          , NCONT) * CSWAT (JA)
!                             IMPLICIT (IN C) LATERAL COUPLING IN SURF.
!                             NB: TIME DERIVATIVE OF R IN ADJACENT
!                             COLUMN INCLUDES THE EFFECT OF THE CHANGING
!                             CONC. IN THAT COLUMN

      ELSEIF (ISBDY (JA) ) THEN  
!                             IF ADJACENT COLUMN IS OUTSIDE BOUNDARY
         DO 16 NCE = NCEBOT, NCEPSF + 1  
            CCAPA (NCE, JA) = CCAPE (NCL, NCONT)  
            CCAPAT (NCE, JA) = zero  
   16          END DO  
         CSWA (JA) = CCAPE (NCL, NCONT)  
         CSWAT (JA) = zero  
         RRRSWA (JA) = one  
!                             NB: NO SEDIMENT WITH FLOWS OVER BOUNDARY

         RRRSAT (JA) = zero  

      ELSE  
!                             IS THE EXPOSED FACE OF A BANK COLUMN
         DO 19 NCE = NCEBOT, NHBED (NLINKA, JBK)  
            CCAPA (NCE, JA) = CCCCO (NWORK (JA), NCE, NCONT)  
            CCAPAT (NCE, JA) = zero  
   19          END DO  
!                             EXPLICIT (IN C) LATERAL COUPLING IN
!                             SUBSURFACE
         DO 17 NCE = NHBED (NLINKA, JBK) + 1, NCETOP  
            CCAPA (NCE, JA) = CCCCO (NLINKA, NCETOP, NCONT)  
            CCAPAT (NCE, JA) = (CCCC (NLINKA, NCETOP, NCONT) &
             - CCAPA (NCE, JA) ) / TSE
!                             IMPLICIT COUPLING WITH STREAM WATER FOR
!                             SUBSURFACE EXPOSED BANK CELLS
   17          END DO  
         CSWA (JA) = CCAPA (NCETOP, JA)  
         CSWAT (JA) = CCAPAT (NCETOP, JA)  
         RRRSWA (JA) = FSF (NLINKA, NCONT)  
         RRRSAT (JA) = FSFT (NLINKA, NCONT) + FSFC (NLINKA, NCONT) &
          * CSWAT (JA)
!                             NB: TIME DERIVATIVE OF F IN ADJACENT
!                             LINK INCLUDES THE EFFECT OF THE CHANGING
!                             CONC. IN THAT LINK

      ENDIF  


   12    END DO  
!                             SET CONCENTRATIONS AND RETARDATION FACTORS
!                             IN ADJACENT COLUMN
   IF (.NOT.ISFLXB) THEN  
      CCBT = CCAPB (NCL, NCONT)  
      CCAP (NCEBOT) = CCBT  
!                             NB: CCAP(NCEBOT) IS USED AS THE BOUNDARY
!                             CONCENTRATION IN SUBROUTINE COLM
      CCPRF = zero  
      CCPRFT = zero  
   ELSE  
      CCPRF = CCAPR (NCL, NCONT)  
      CCPRFT = zero  
      CCBT = CCPRF  
   ENDIF  
   DO 20 NCE = 1, NCEBOT - 1  
      COLCAP (NCE) = CCBT  
      CCAP (NCE) = CCBT  
      SCAP (NCE) = CCBT  



   20    END DO  
!                            SET BOTTOM CELL VARIABLES
   DO 22 JSED = 1, NSED  
      KDDUM (JSED) = KDDLS (JSED, NCONT)  
      FBO (JSED) = FBETAO (NCL, JSED)  
      FB (JSED) = FBETA (NCL, JSED)  
      FBETAO (NCL, JSED) = FB (JSED)  
      FDLO (JSED) = FDELO (NCL, JSED)  
      FDL (JSED) = FDEL (NCL, JSED)  
      FDELO (NCL, JSED) = FDL (JSED)  

   22    END DO  
!                             SET UP ARRAYS FOR USE IN CALLS TO FUNCTION
!                             RET

   CALL RET (COLCAP (NCETOP), GNN (NCONT), TTTLSE, TTTLSE, FBO, &
    FB, KDDUM, RRRLS, RRRLSC, RRRLST, TSE, NSED, ISADNL)
!                             SET LOOSE SEDIMENT REATRDATION VARIABLES

   CALL RET (COLCAP (NCETOP), GNN (NCONT), one, one, FDLO, &
    FDL, KDDUM, RRRSW, RRRSWC, RRRSWT, TSE, NSED, ISADNL)
!                             SET SURFACE WATER RETARDATION VARIABLES
   RSW (NCL, NCONT) = RRRSW  
   RSWC (NCL, NCONT) = RRRSWC  


   RSWT (NCL, NCONT) = RRRSWT  
!                             SAVE SURFACE WATER REATRDATION VALUES
!                             FOR USE IN CALCULATING LATERAL CONVECTION
!                             RATES
   ICAP = - Z2OD * IIICFO (NCONT)  
   IIICFO (NCONT) = IIICF (NCONT)  
   ICAPT = zero  
   ICAPC = zero  
   DUM = Z2OD / (DDA * DDB)  
   QCDUM = (QI - QQQWEL) * CCAPIO (NCONT)  
   QCDUM1 = (QI1 - QQQWL1) * CCAPI (NCONT)  
   IF (NCWELL.GT.0) THEN  
      QCDUM = QCDUM + QQQWEL * CCCCW (NCWELL, NCONT)  
      QCDUM1 = QCDUM1 + QQQWL1 * CCCCW (NCWELL, NCONT)  
   ENDIF  
   QCAP = DUM * QCDUM  
   QCAPT = (DUM * QCDUM1 - QCAP) / TSE  

   CCAPIO (NCONT) = CCAPI (NCONT)  



   QCAPC = zero  
!                            SET SURFACE INPUT VARIABLES
   DO 30 NCE = NCEBOT, NCETOP  
      DUMMY (NCE) = zero  
   30    END DO  

   IF (ISBK) THEN  
      SUM = zero  
      SUMQ = zero  
      DUM0 = Z2OD / AREA (NCL)  
      DO 32 NCE = NCEAB (NLINKA, JBK), NHBED (NLINKA, JBK) &
       + 1
         SUMQ = SUMQ + QQRV (NCE)  
         DUM1 = ABS (QQRV (NCE) )  
         DUM2 = half * (QQRV (NCE) + DUM1)  
         DUM3 = half * (QQRV (NCE) - DUM1)  
         QCDUM = DUM2 * CCCC (NLINKA, NCETOP - 2, NCONT) + DUM3 * &
          CCCCO (NCL, NCE, NCONT)
!                             IMPLICIT COUPLING TO DEEP BED CONC.
         SUM = SUM + QCDUM  
         DUMMY (NCE) = DUMMY (NCE) + ROH (NCE) * QCDUM * DUM0 / &
          KSP (NCE)

   32       END DO  
!                             SET SOURCE FOR CONVECTION INTO STREAM FROM
!                             BANK
      IF (NOTZERO(SUMQ)) SUMQ = SUM / SUMQ  
      CDUM = SUMQ  
!                             SET EFFECTIVE CONCENTRATION IN WATER FLOW
!                             INTO STREAM FROM BANK
      NCE = NCEBD (NLINKA, JBK) + 1  
      UDUMP = UUAJP1 (NCE)  
      UDUMM = UUAJP1 (NCE-1)  
      UCDUMP = MAX (zero, UDUMP * COLCAP (NCE) ) - MAX (zero, &
       - UDUMP * COLCAP (NCE+1) )
      UCDUMM = MAX (zero, UDUMM * COLCAP (NCE-1) ) - MAX (zero, &
       - UDUMM * COLCAP (NCE) )
      DUMBED = (ROH (NCE) * VELDUM (NCE-1) - one) * UCDUMM - &
       (ROH (NCE) * VELDUM (NCE) - one) * UCDUMP

      DUMBED = Z2OD * DUMBED / KSP (NCE)  

   ELSE  

      DUMBED = zero  

   ENDIF  
   IF (ISPLT) THEN  
      CALL PLCOLM (NCL, NCONT)  
   ELSE  
      DO 33 NCE = NCEBOT, NCETOP  
         EDCAP (NCE) = zero  
         EDCAPC (NCE) = zero  
         EDCAPT (NCE) = zero  
         ESCAP (NCE) = zero  
         ESCAPS (NCE) = zero  
         ESCAPT (NCE) = zero  
   33       END DO  

   ENDIF  
!                 Call contaminant plant uptake routine
!                  Sets EDCAP, ESCAP etc
   SUM = zero  
   SUMW = zero  

   DO 34 NCE = NCEBOT, NCETOP  

      EDCAP (NCE) = EDCAP (NCE) - DUMMY (NCE) + WELDRA (NCE) &
       * Z2OD * COLCAP (NCE) / KSP (NCE)
!                         Add  stream and well uptake to plant uptake
      SUM = SUM + WELDRA (NCE) * COLCAP (NCE)  
      SUMW = SUMW + WELDRA (NCE)  
   34    END DO  
   IF (ISBK) THEN  
      NCE = NCEBD (NLINKA, JBK) + 1  
      EDCAP (NCE) = EDCAP (NCE) - DUMBED  

   ENDIF  
!                       Add  uptake to dry streams to plant and
!                       well uptake
   IF (NOTZERO(SUMW)) THEN  
      CCCCW (NCL, NCONT) = SUM / SUMW  
   ELSE  
      CCCCW (NCL, NCONT) = zero  



   ENDIF  
!                             SET PLANT WELL AND STREAM UPTAKE
!                             VARIABLES; AND SET THE MIXED WELL WATER
!                             CONCENTRATION FOR USE IN PRINTOUTS
!                             NB: WELL UPTAKE AND LOSS TO STREAM VIA BED
!                             INCLUDED IN EDCAP
   OPSGL = one + SGTSE * GCAPLA  

   OPSGSL = one + SGSTSE * GCAPLA  
!                            SET FACTORS AND TERMS DEPENDING ON SIGMA

   CALL COLM  
!                            RETURNS UPDATED CONCENTRATIONS
!                            IN THE VECTORS CCAP AND SCAP
   CCCCO (NCL, 1, NCONT) = CDUM  
   CCCC (NCL, 1, NCONT) = CDUM  
!                             FLOW RATE AVERAGED CONC. IN WATER FLOW
!                             FROM BANK TO STREAM STORED AS ELEMENT 1
!                             IN GLOBAL CONTAMINANT ARRAYS
   DO 40 NCE = 1, NCETOP  
!    ##########################temporary MAX######################
      CCCC (NCL, NCE, NCONT) = MAX (1D-16, CCAP (NCE) )  
      SSSS (NCL, NCE, NCONT) = MAX (1D-16, SCAP (NCE) )  
!    ##############################################################


   40    END DO  
!                             SAVE THE UPDATED CONCENTRATIONS
   IF (ISBK.AND. (.NOT.ISADNL) ) THEN  
      DO 42 NCE = NHBED (NLINKA, JBK) + 1, NCETOP  
         FCPBKO (NLINKA, JBK, NCE, NCONT) = PPHI (NCE) * TTHET ( &
          NCE) + FFSO (NCE) * KKDSO (NCE)
         GCPBKO (NLINKA, JBK, NCE, NCONT) = (one - PPHI (NCE) ) &
          * TTHET (NCE) + (one - FFSO (NCE) ) * KKDSO (NCE)
   42       END DO  
   ELSEIF (ISBK.AND.ISADNL) THEN  
      GNDUM = GNN (NCONT) - one  
      DO 44 NCE = NHBED (NLINKA, JBK) + 1, NCETOP  
         FCPBKO (NLINKA, JBK, NCE, NCONT) = PPHI (NCE) * TTHET ( &
          NCE) + FFSO (NCE) * KKDSO (NCE) * COLCAP (NCE) **GNDUM
         GCPBKO (NLINKA, JBK, NCE, NCONT) = (one - PPHI (NCE) ) &
          * TTHET (NCE) + (one - FFSO (NCE) ) * KKDSO (NCE) &
          * SOLCAP (NCE) **GNDUM
   44       END DO  


   ENDIF  
!                             FCPBK AND GCPBK ARE USED IN THE BANK
!                             EROSION CALCULATIONS IN LINK

    5 END DO  
!                             ++++++++++++ END OF MAIN LOOP +++++++++++
END SUBROUTINE COLMSM





!SSSSSS SUBROUTINE COLMW (NCL)  
SUBROUTINE COLMW (NCL)  
!----------------------------------------------------------------------*
!                             SETS UP THE WATER FLOW DATA FOR USE IN
!                             THE BANK AND GRID COLUMN SOLVER
!----------------------------------------------------------------------*
! Version:  SHETRAN/MUZ/COLMW/4.2
! Modifications:
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.  No INTEGER*2.
!  GP  960717  4.0  Move JFACEA to /WTOC/, with new outputs QQQWEL,
!                   QQQWL1,NCWELL (COLMSM).
!                   Scrap: "overlap" locals; outputs FNCPSF (COLM.C1) &
!                   BFSCL (BK.CW); & inputs NHSAT,FHSAT,QBKF,FHBED
!                   (AL.C), JKZCH (BK.CW), & JKZCOB,NOLCE,JKZCOL,NOLBT,
!                   SCL,JOLFN (COLM.CG).
!                   Replace inputs: KSPE(,NVC) & KSPPE(,NVC)
!                   (COLM.CG,AL.C) with DELTAZ/Z2 & diff(ZVSNOD)/Z2;
!                   TH3O with VSTHEO (COLM.CO); TH3 with VSTHE (AL.C).
!                   Uncomment setting of UUAJP,UUAJP1,UUAJPO (but
!                   still overwritten in "temporary" section at end).
!                   For TTHET1 & UUAJP1 at banks take mean of bank and
!                   link values, and don't multiply UUAJP1 by ROH.
!                   Set KSP(LL+1) & UUAJP*(NAQU-1).
!                   Use ZGRUND,NCETOP & ICMREF in place of HSZ,NHSAT
!                   (AL.C) & NBKA (BK.CW) for ZONE1,NCEPSF & NWORK.
!                   Scrap local Q1.  Irrigation: scrap "new as old"
!                   option, & COLM.CG inputs JKZWEL,JKZWCE; replace AL.C
!                   inputs NWC,RSZWEL with NVSWLI, QVSWLI(*,NVSWLI()); &
!                   note *PSF above.
!                   Simplify setting of QQRV (NCEAB=NHBED - see INCM).
!                   Use EMULT to reduce correction to UUAJP1 at the end.
! RAH  970218  4.1  Swap subscripts: QVSH,DELTAZ,ZVSNOD,QVSV,QVSWLI,
!                   VSTHE (see AL.C).
!      970314       New locals OMROH,THEDUM,QVDUM.  Generic intrinsics.
!      970317       Split mixed-type /WTOC/ (COLMSM).  Explicit typing.
!                   Remove redundant locals NOLMX,FNOLMX (& input NOL
!                   (COLM.CG)).  Remove redundant code.  Move NDIFF.
!                   Re-introduce local Q1.  Use JBK for ITYPE.
!                   Use NAQU for NLYRBT(NCL,1); NCETOP for LL.
!                   Use ISBK & ISBDY in tests.  Labels in order.
!                   New local PHIDUM.  Don't set VSTHEO(NLINKA,NCEA).
!                   Scrap local ADUM.
!      970318       Scrap COLM.CO output WELDRO, & locals NCEB,NCEM.
!                   Condense well code.
!      970325       Remove provisional "bankless" code for QQRV, and
!                   redundant /WTOC/ outputs JFACEA,TRAN (COLMSM), and
!                   make NAQU local (was in /WTOCI/).
!      970521       Use 1 not JBTLYR (COLM.CG) as loop 221 limit.
!                   Use ALINIT for initialization.
! RAH  981103  4.2  Scrap output ERUZO (COLM.CO).
!----------------------------------------------------------------------*
! Commons and constants
USE SED_CS  
USE COLM_C1  
USE COLM_C2  
USE COLM_CO  
USE COLM_CG  
USE BK_CW  
USE SED_CO  
USE PLANT_CC  
! Imported constants
!     COLM.C1:         LLEE,NELEE,NLFEE
! Input common
!     COLM.C1:         NCETOP
!                      SGMA,TSE,SGSQ,Z2,D0,Z2SQOD,Z2OD
!     COLM.CO:         RSZWLO(NVSWLT(NCL):NVSWLT(NCL))
!     COLM.CG:         NCOLMB(NCL:NCL)
!                      ZCOLMB(NCL:NCL)
!        AL.C:         LL
!                      NLYRBT(NELEE,*),NLYR(NCL:NCL),NTSOIL(NELEE,*)
!                      NHBED(NLFEE,2),NVSWLT(NCL:NCL),NVSWLI(NCL:NCL)
!                      NWELBT(NCL:NCL),NWELTP(NCL:NCL)
!                      AREA(NEL),CLENTH(NLFEE),CWIDTH(NLFEE)
!                      DELTAZ(NCOLMB(NCL):NCETOP,NCL:NCL)
!                      ZVSNOD(NCOLMB(NCL):NCETOP+1,NCL:NCL)
!                      DXQQ(NCL:NCL),DYQQ(NCL:NCL),ZGRUND(NCL:NCL)
!                      DTUZ
!                      ERUZ(NELEE,NLYRBT(NCL,1):NCETOP)
!                      VSTHE(LLEE,NCL),QVSV(LLEE,NCL)
!                      HRF(NCL:NCL),QVSH(4,LLEE,NCL),QOC(NELEE,4)
!                      PNETTO(NCL:NCL),QVSWLI(LLEE,*),QBKB(NLFEE,2)
!                      EEVAP(NCL:NCL)
!        AL.G:         ICMREF(NELEE,12)
!       BK.CW:          NCEBD(NLFEE,2),NCEAB(NLFEE,2)
!                      FNCEBD(NLFEE,2)
!      SED.CS:         DLS(NCL:NCL),GNU(NCL:NCL)
!    PLANT.CC:         XXI
! In+out common
!     COLM.CO:         VSTHEO(NELEE,NCOLMB(NCL):NCETOP),UUAJPO(NELEE,LL)
!                      GGAMMO(NELEE,LL),DSWO(NCL:NCL),ZONEO(NCL:NCL)
!                      QQO(NELEE,LLEE,4),QQQSWO(NELEE,4),QIO(NCL:NCL)
!                      QQRFO(NCL:NCL)
!      SED.CO:         DLSO(NCL:NCL),GNUO(NCL:NCL)
! Output common
!     COLM.C1:         NCEBOT,NCEPSF
!                      SGTSE,SGSTSE,CST2,CST1,CST3
!     COLM.C2:         TTTLSE,DDA,DDB,DDDSW,DDDSW1,DDDLS,DDDLS1
!                      GGGNU,GGGNU1,ZONE,ZONE1,QI,QI1,QQRF,QQRF1
!                      KSP(LL+1),KSPP(LL),TTHET(LL),UUAJP(LL),TTHET1(LL)
!                      UUAJP1(LL),PPHI(LL),PPHI1(LL),GGAMM(LL)
!                      GGAMM1(LL),QQ(LL+1,4),QQ1(LL+1,4),QQQSW(4)
!                      QQQSW1(4)
!                      ISBK
!     COLM.CG:         WELDRA(LL)
!INTEGER :: JBK, JFLINK, JSOL (LLEE), NWORK (4), NLINKA, NCWELL  
!DOUBLEPRECISION VELDUM (LLEE), QQQWEL, QQQWL1, QQRV (LLEE), &
! ROH (LLEE)
!LOGICAL :: ISBDY (4)  
!COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL  
!COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH  
!COMMON / WTOCL / ISBDY  
!                             VARIABLES USED ONLY IN COLMW AND COLMSM
! Workspace common
!        AL.C:         DUMMY(LL)
! Input arguments

INTEGER :: NCL  
! Locals, etc
!INTRINSIC MAX, MOD, SIGN  
INTEGER :: JAL, JSOIL, JDUM, IW, JA, JLYR, JB  
INTEGER :: NAQU, NCE, NCEA, NCLA, NDIFF, NDUM, NELMA  
DOUBLEPRECISION DBK, DMULT, DINV, ROHDUM, OMROH, THEDUM, QVDUM, &
 PHIDUM
DOUBLEPRECISION DUM, DUM0, DUM1, UUOLD, UUNEW, ERRDUM, UIN  



DOUBLEPRECISION Q1 (LLEE), TRAN1 (LLEE), EMULT (LLEE)  
!----------------------------------------------------------------------*
! Factors & indices
!___________________*
SGTSE = SGMA * TSE  

SGSTSE = SGSQ * TSE  
!                             SET FACTORS DEPENDING ON SIGMA
NCEBOT = NCOLMB (NCL)  

NAQU = NLYRBT (NCL, 1)  
!                             SET BOTTOM COLUMN CELL, AND
!                             BOTTOM AQUIFER CELL NUMBERS
NDUM = NCETOP - NAQU + 2  
CALL ALINIT (ONE, NDUM, ROH (NAQU - 1) )  

CALL ALINIT (ONE, NDUM, VELDUM (NAQU - 1) )  
!                             set defaults
JBK = ICMREF (NCL, 1)  
ISBK = JBK.NE.0  
IF (ISBK) THEN  
!                             ELEMENT IS A BANK
   NLINKA = ICMREF (NCL, 4)  
   NDIFF = NLYRBT (NLINKA, 1) - NAQU  
!                             NUMBER & CELL OFFSET FOR ASSOCIATED LINK
   JAL = 0  
  100    JAL = JAL + 1  
   IF (ICMREF (NLINKA, JAL + 4) .NE.NCL) GOTO 100  
   JFLINK = ICMREF (NLINKA, JAL + 8)  
!                             NUMBER FOR FACE ASSOCIATED WITH LINK
   DBK = AREA (NCL) / CLENTH (NLINKA)  
   DMULT = DBK / (DBK + half * CWIDTH (NLINKA) )  
   DINV = ONE / DMULT  
   DO 102 NCE = NAQU - 1, NCEBD (NLINKA, JBK)  
      ROH (NCE) = DMULT  
      VELDUM (NCE) = DINV  
  102    END DO  
   ROH (NCE) = ONE- (ONE-DMULT) * FNCEBD (NLINKA, JBK)  
ELSE  
!                             NOT A BANK
   JFLINK = 0  




ENDIF  
!                             SET ROH (& VELDUM): FOR A BANK
!                             ROH IS THE RATIO OF THE WIDTH OF THE BANK
!                             SOIL COLUMN TO THE SUM OF THE WIDTH OF THE
!                             BANK SOIL COLUMN AND HALF THE WIDTH OF THE
!                             STREAM; IT IS USED IN SUBSURFACE FLOW
!                             CALCULATIONS TO ALLOW THE SAME CODE TO BE
!                             USED FOR BANK AND NON-BANK COLUMNS
!                             NB: ROH IS 1 ABOVE THE BOTTOM OF THE BED
!                             DEEP LAYER.
!                             FOR A NON-BANK, ROH IS 1
!970521                       See also "temporary" section at the end
! Properties for each cell *
!__________________________*
DO 190 NCE = NAQU, NCETOP  
   TRAN1 (NCE) = ERUZ (NCL, NCE)  

  190 END DO  
!                             SET LOCAL VECTOR FOR RATE OF PLANT UPTAKE
!                             OF WATER FOR THE FULL LENGTH OF THE COLUMN
DO 221 JLYR = 1, NLYR (NCL)  
   JSOIL = NTSOIL (NCL, JLYR)  
   DO 212 NCE = MAX (NCEBOT, NLYRBT (NCL, JLYR) ), NLYRBT (NCL, &
    JLYR + 1) - 1
      JSOL (NCE) = JSOIL  
      KSP (NCE) = DELTAZ (NCE, NCL) / Z2  
      KSPP (NCE) = (ZVSNOD (NCE+1, NCL) - ZVSNOD (NCE, NCL) ) &
       / Z2
!                             NB kspp(ncetop) is overwritten below
      TTHET (NCE) = VSTHEO (NCL, NCE)  

      UUAJP (NCE) = UUAJPO (NCL, NCE)  
      IF (JBK.EQ.0) THEN  
!                             regular column element
         TTHET1 (NCE) = VSTHE (NCE, NCL)  
         UUAJP1 (NCE) = QVSV (NCE, NCL)  
      ELSE  
!                             element is (L-shaped) bank
!                             NB uuajp1(nhbed) is overwritten below
         NCEA = NCE+NDIFF  
         IF (NCEA.LE.NCETOP) THEN  
            ROHDUM = ROH (NAQU)  
            OMROH = one - ROHDUM  
            THEDUM = VSTHE (NCEA, NLINKA)  

            QVDUM = QVSV (NCEA, NLINKA)  
            TTHET1 (NCE) = OMROH * THEDUM + ROHDUM * VSTHE (NCE, &
             NCL)
            UUAJP1 (NCE) = OMROH * QVDUM + ROHDUM * QVSV (NCE, &
             NCL)
         ELSE  
            TTHET1 (NCE) = VSTHE (NCE, NCL)  

            UUAJP1 (NCE) = QVSV (NCE, NCL)  

         ENDIF  

      ENDIF  
      VSTHEO (NCL, NCE) = TTHET1 (NCE)  

      UUAJPO (NCL, NCE) = UUAJP1 (NCE)  
      PHIDUM = PHI (JSOIL, TTHET1 (NCE) )  
      PPHI (NCE) = PHI (JSOIL, TTHET (NCE) )  
      PPHI1 (NCE) = PHIDUM  
      GGAMM (NCE) = GGAMMO (NCL, NCE)  
      GGAMM1 (NCE) = (one - XXI * PHIDUM) * ROH (NCE) * TRAN1 ( &
       NCE) / (KSP (NCE) * Z2) + ( (one - PHIDUM) * TTHET1 (NCE) &
       - (one - PPHI (NCE) ) * TTHET (NCE) ) / DTUZ
      GGAMMO (NCL, NCE) = GGAMM1 (NCE)  
  212    END DO  

  221 END DO  
!                             ordinary cells
KSP (NCETOP + 1) = KSP (NCETOP)  
KSPP (NCETOP) = KSP (NCETOP)  

KSPP (NCEBOT - 1) = DELTAZ (NCEBOT, NCL) / Z2  
!                             special cells for KSP*
IF (ISBK) THEN  
   NCE = NHBED (NLINKA, JBK)  
   UUAJP1 (NCE) = QVSV (NCE, NCL)  
ENDIF  
!                             vert. vel. of cell below bed is in top
!                             part of L-shaped column (over-rides above)
NCE = NAQU - 1  
UUAJP (NCE) = UUAJPO (NCL, NCE)  
IF (JBK.EQ.0) THEN  
   UUAJP1 (NCE) = QVSV (NCE, NCL)  
ELSE  
   NCEA = NCE+NDIFF  
   UUAJP1 (NCE) = ( (ONE-ROH (NCE) ) * QVSV (NCEA, NLINKA) &
    + ROH (NCE) * QVSV (NCE, NCL) )
ENDIF  




UUAJPO (NCL, NCE) = UUAJP1 (NCE)  
!                             vert vel for cell below aquifer base
!                             SET cell properties, moisture content,
!                             AND VERTICAL FLOW VALUES, AND
!                             STORE 'OLD' VALUES FOR NEXT TIME STEP
!970314                       NB See "temporary code" at end of routine
! Properties common to every cell *
!_________________________________*
TTTLSE = 1.0D-4  
!                             SET MOISTURE CONTENT FOR LOOSE SEDIMENTS
DDA = DYQQ (NCL)  
DDB = DXQQ (NCL)  
DDDSW = DSWO (NCL)  
DDDSW1 = HRF (NCL) - ZGRUND (NCL)  
DSWO (NCL) = DDDSW1  
DDDLS = DLSO (NCL)  
DDDLS1 = DLS (NCL)  
DLSO (NCL) = DLS (NCL)  
GGGNU = GNUO (NCL)  
GGGNU1 = GNU (NCL)  
GNUO (NCL) = GNU (NCL)  
ZONE = ZONEO (NCL)  
ZONE1 = (ZGRUND (NCL) - ZCOLMB (NCL) ) / Z2  
ZONEO (NCL) = ZONE1  
!                             SET WIDTHS OF COLUMN,
!                             DEPTHS OF SURFACE WATER AND
!                             SEDIMENTS, EROSION RATE, AND
!                             NON-DIMENSIONED SATURATED DEPTH
NCEPSF = NCETOP  
!                             FORMERLY (pre v4.0) THE HIGHEST CELL
!                             NUMBER IN THE SATURATED ZONE;
!                             now lateral transport is allowed
!                             up to the ground surface
CST2 = Z2 / (AREA (NCL) * D0)  
CST1 = CST2 / ZONE1  



CST3 = CST2 / KSP (NCEBOT)  
!                            SET CONSTANTS USED IN CONVECTION TERMS
DO 303 JA = 1, 4  
   NELMA = ICMREF (NCL, JA + 4)  
   ISBDY (JA) = NELMA.EQ.0  
   IF (.NOT.ISBDY (JA) ) THEN  
      IF (ICMREF (NELMA, 1) .EQ.3) THEN  
         NWORK (JA) = ICMREF (NELMA, JA + 4)  
      ELSE  
         NWORK (JA) = NELMA  
      ENDIF  
   ELSE  
      NWORK (JA) = NCL  
!                             ASSUME MIRROR IMAGE IF FACE IS AT THE
!                             BOUNDARY OF CATCHMENT
   ENDIF  



  303 END DO  
!                             SET NWORKj TO THE NUMBER FOR THE COLUMN
!                             ADJACENT TO FACE j
!+++++ MAIN LOOP FOR COLUMN FACES +++++*
!______________________________________*

DO 605 JA = 1, 4  
   DO 318 NCE = NCEBOT - 1, NCETOP + 1  
      QQ (NCE, JA) = zero  
      QQ1 (NCE, JA) = zero  
      DUMMY (NCE) = zero  

  318    END DO  
   IF (JA.EQ.JFLINK) THEN  
!                             IS INSIDE FACE OF BANK
      DO 329 NCE = NCEBOT, NHBED (NLINKA, JBK)  
         NCEA = NCE+NDIFF  
         JB = 1 + MOD (JA + 1, 4)  
         Q1 (NCE) = .5D0 * (QVSH (JA, NCEA, NLINKA) - QVSH (JB, &
          NCEA, NLINKA) )
  329       END DO  
      DO 395 NCE = NHBED (NLINKA, JBK) + 1, NCETOP  
         Q1 (NCE) = QVSH (JA, NCE, NCL)  
  395       END DO  
   ELSE  
!                             neighbour is a land element
      DO 410 NCE = NCEBOT, NCETOP  
         Q1 (NCE) = QVSH (JA, NCE, NCL)  
  410       END DO  
      NCLA = ICMREF (NCL, JA + 4)  
      IF (ISBK.AND.NCLA.GT.0) THEN  
         IF (ICMREF (NCLA, 1) .EQ.1.OR.ICMREF (NCLA, 1) .EQ.2) &
          THEN
!                             add extra flow for end-to-end banks
            DO 496 NCE = NCEBOT, NHBED (NLINKA, JBK)  
               NCEA = NCE+NDIFF  
               Q1 (NCE) = Q1 (NCE) + .5D0 * QVSH (JA, NCEA, &
                NLINKA)
  496             END DO  
         ENDIF  

      ENDIF  


   ENDIF  
!                             SET THE LATERAL FLOW RATES Q1 FOR THE
!                             ENTIRE DEPTH OF FACE JA OF THE
!                             CURRENT COLUMN NCL (incl L-shaped banks)
   DO 511 NCE = NCEBOT, NCETOP  
      QQ1 (NCE, JA) = Q1 (NCE) * (ZONE1 * ROH (NCE) / KSP (NCE) )  
      QQ (NCE, JA) = QQO (NCL, NCE, JA)  
      QQO (NCL, NCE, JA) = QQ1 (NCE, JA)  
  511    END DO  
!                             SET THE OLD AND NEW LATERAL FLOW RATES
!                             FOR THE SATURATED SECTIONS OF THE FACES
!                             OF THE CURRENT COLUMN




  605 END DO  
!                             ++++++++++++ END OF MAIN LOOP ++++++++++++
!__________________________*
DO 712 JDUM = 1, 2  
   QQQSW (JDUM) = QQQSWO (NCL, JDUM)  
   QQQSW (JDUM + 2) = QQQSWO (NCL, JDUM + 2)  
   QQQSW1 (JDUM) = - QOC (NCL, JDUM)  
   QQQSWO (NCL, JDUM) = QQQSW1 (JDUM)  
   QQQSW1 (JDUM + 2) = QOC (NCL, JDUM + 2)  
   QQQSWO (NCL, JDUM + 2) = QQQSW1 (JDUM + 2)  



  712 END DO  
!                             SET RATE OF LATERAL SURFACE WATER FLOW
!                             INTO THE FOUR FACES OF THE COLUMN
! Boundary Conditions *
!_____________________*
NCWELL = NVSWLT (NCL)  
IF (NCWELL.NE.0) THEN  
   QQQWEL = - RSZWLO (NCWELL) * AREA (NCWELL)  
   QQQWL1 = - QVSWEL (NCWELL) * AREA (NCWELL)  
ELSE  
   QQQWEL = zero  
   QQQWL1 = zero  

ENDIF  
!                             irrigation onto grids
QI = QIO (NCL)  
QI1 = - PNETTO (NCL) * AREA (NCL)  

QIO (NCL) = QI1  
!                             SET RATE OF RAIN WATER INFLOW (NEGATIVE
!                             TO CONFORM TO POSITIVE UPWARDS CONVENTION)
DO 813 NCE = NAQU, NCETOP  
  813 WELDRA (NCE) = zero  
IW = NVSWLI (NCL)  
IF (IW.NE.0) THEN  
   DO 818 NCE = NWELBT (NCL), NWELTP (NCL)  
  818    WELDRA (NCE) = QVSWLI (NCE, IW)  

ENDIF  
!                             SET THE RATE OF WELL WITHDRAWL FROM
!                             INDIVIDUAL CELLS
DO 1198 NCE = 1, NCETOP  
   QQRV (NCE) = zero  
 1198 END DO  


IF (ISBK) QQRV (NCEAB (NLINKA, JBK) ) = QBKB (NLINKA, JBK)  
!                             SET RATE OF FLOW INTO BANK CELLS FROM
!                             STREAM WATER. FLOW TAKES PLACE ONLY OVER
!                             THE SATURATED DEPTH BETWEEN CELL NCEAB AND
!                             THE EFFECTIVE BED OF THE CHANNEL
!################### temporary code for calc vertical vels. JE 18/9/91
! re-used by GP 24/1/96
! emult: fraction of the error correction which is removed at each cell
DO 3030 NCE = NCETOP, MAX (1, NCETOP - 4), - 1  
 3030 EMULT (NCE) = zero  
DO 3032 NCE = NCETOP - 5, MAX (1, NCETOP - 7), - 1  
 3032 EMULT (NCE) = 0.1D0  
DO 3034 NCE = NCETOP - 8, MAX (1, NCETOP - 19), - 1  
 3034 EMULT (NCE) = half  
DO 3036 NCE = NCETOP - 20, NCEBOT, - 1  

 3036 EMULT (NCE) = ONE  
UIN = (DDDSW1 - DDDSW) / (Z2SQOD * TSE)  
DUM = zero  
DO 3120 JA = 1, 4  
   DUM = DUM + QQQSW1 (JA)  
 3120 END DO  

UUAJP1 (NCETOP) = UIN + EEVAP (NCL) + (QI1 - DUM) / AREA (NCL)  
DO 3122 NCE = NCETOP, NCEBOT, - 1  
   DUM0 = KSP (NCE) / (ROH (NCE) * ZONE1)  
   DUM = KSP (NCE) * (TTHET1 (NCE) - TTHET (NCE) ) / (ROH (NCE) &
    * Z2OD * TSE)
   DUM = DUM + WELDRA (NCE) + TRAN1 (NCE)  
   DUM1 = QQRV (NCE) + DUM0 * (QQ1 (NCE, 1) + QQ1 (NCE, 2) &
    + QQ1 (NCE, 3) + QQ1 (NCE, 4) )
   UUOLD = UUAJP1 (NCE-1)  
   UUNEW = (DUM - DUM1 / AREA (NCL) + VELDUM (NCE) * UUAJP1 (NCE) &
    ) / VELDUM (NCE-1)
   ERRDUM = UUNEW - UUOLD  
   UUAJP1 (NCE-1) = UUNEW - ERRDUM * EMULT (NCE)  
   UUAJPO (NCL, NCE-1) = UUAJP1 (NCE-1)  

 3122 END DO  
!################### end of temporary code
QQRF = QQRFO (NCL)  
QQRF1 = AREA (NCL) * UUAJP1 (NCEBOT - 1)  

QQRFO (NCL) = QQRF1  
!                             set rate of flow through base of column





END SUBROUTINE COLMW




!FFFFFF DOUBLEPRECISION FUNCTION DISP
DOUBLEPRECISION FUNCTION DISP (NCONT, JSOIL, THETA, UM, UP)  
!                             (VERTICAL) EFFECTIVE LONGITUDINAL
!                             DISPERSION COEFFICIENT FOR SOIL
INTEGER :: NCONT, JSOIL
DOUBLEPRECISION :: THETA, UM, UP
DISP = 3.0D-8  
!                             ########## SOIL INFO NEEDED HERE #########





END FUNCTION DISP



!SSSSSS SUBROUTINE LINKSM (NLINK)  
SUBROUTINE LINKSM (NLINK)  
!                             UPDATES THE CONCENTRATION OF EACH
!                             CONTAMINANT IN LINK NLINK
USE CONT_CC  
USE SED_CS  
USE CONT_CC  
USE COLM_C1  
USE LINK_CC  
USE LINK_CW  
USE SED_CO  
USE PLANT_CC  

!                             INCLUDE ALL THE PARAMETER STATEMENTS
!                             AND COMMON BLOCKS NEEDED
!COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1  
!common / temp / nwell  
!##### nwell and qqqdum used in temporary irrigation code###########
!LOGICAL :: ISLK  
!                             VARIABLES USED ONLY IN LINKW AND LINKSM

INTEGER :: nlink, ncont, nce, jlend, jdum, jla, jsed, na, lfone, ldum, la
DOUBLEPRECISION FBTAD (NSEDEE), FBTAS (NSEDEE), KDDUM (NSEDEE), &
 SSBED1 (NSEDEE), SSBED (NSEDEE), SSD1 (NSEDEE), SSD (NSEDEE), &
 SSF1 (NSEDEE), SSF (NSEDEE)
DOUBLEPRECISION :: ccpbd, ccpbs, qcdum, sumd, sums, dddsum, pb, fdum, fdumc, &
           fdumt, dum, arl, arp, ccpsf, dddum, dsdum, ccpbd1, ccpbs1, ccpsf1, &
           dumx
CCBD1Q = zero  
CCBS1Q = zero  
CCSF1Q = zero  
FCBD1Q = zero  
FCBS1Q = zero  
FCSF1Q = zero  


GCPLAQ = zero  
!                             SET PARENT CONCENTRATIONS AND RETARDATION
!                             VARIABLES TO O FOR 1ST PASS OF DO LOOP 100
DO 100 NCONT = 1, NCON  
   DO 102 JBK = 1, 2  
      CCPBK (JBK, 1) = CCCCO (NBK (JBK), 1, NCONT)  
!                             THIS ELEMENT OF ARRAY CCCCO IS USED TO
!                             HOLD THE EFFECTIVE CONCENTRATION IN
!                             THE FLOW ENTERING THE STREAM VIA THE
!                             STREAM BED
      DO 104 NCE = NCEBK (JBK), NCETOP  
         CCPBK (JBK, NCE) = CCCCO (NBK (JBK), NCE, NCONT)  
         SCPBK (JBK, NCE) = SSSSO (NBK (JBK), NCE, NCONT)  
  104       END DO  
      CCPGS1 (JBK) = CCCC (NBK (JBK), NCETOP, NCONT)  
  102    END DO  
   CCPBD = CCCCO (NLINK, NCETOP - 2, NCONT)  
   CCPBS = CCCCO (NLINK, NCETOP - 1, NCONT)  
   IF (USCP.GT.half) THEN  
      CCPSF = CCCCO (NLINK, NCETOP, NCONT)  
   ELSE  
      CCPSF = ccapin (ncont)  
!                             IF THERE IS NO WATER IN LINK


   ENDIF  
   DO 110 JLEND = 1, 2  
      IF (ISLK (JLEND) ) THEN  
!                             THERE ARE OTHER LINKS ASSOCIATED WITH END
!                             JLEND OF THE CURRENT LINK
         DO 112 JDUM = 1, 3  
            JLA = (JLEND-1) * 3 + JDUM  
            LA = LWORK (JLA)  
            IF (LA.NE.0) THEN  
               CCSFA1 (JLA) = CCCC (LA, NCETOP, NCONT)  
               FCSFA1 (JLA) = FSF (LA, NCONT) + FSFT (LA, NCONT) &
                * TSE+FSFC (LA, NCONT) * (CCSFA1 (JLA) - CCCCO (LA, &
                NCETOP, NCONT) )
            ELSE  
               CCSFA1 (JLA) = zero  
               FCSFA1 (JLA) = zero  
            ENDIF  

  112          END DO  
      ELSE  
!                             END JLEND OF LINK IS AT CATCHMENT BOUNDARY
!                             THE HEAD OF A STREAM, OR A SPRING
         JLA = (JLEND-1) * 3 + 1  
         CCSFA1 (JLA) = CCAPE (NLINK, NCONT)  
         FCSFA1 (JLA) = one  
!                             FOR FLOW INTO CATCHMENT OR SPRING
         DO 114 JDUM = 2, 3  
            JLA = (JLEND-1) * 3 + JDUM  
            CCSFA1 (JLA) = zero  
            FCSFA1 (JLA) = zero  
  114          END DO  
      ENDIF  



  110    END DO  
!                             SET LINK AND BANK CONCENTRATIONS.
!                             NB: IF THE STREAM IS DRY, THE STREAM WATER
!                             CONCENTRATION SET TO THE CONCENTRATION IN
!                             RAIN WATER
   ICP1 = - IIICF (NCONT) * AREA (NLINK) / (D0 * CLENTH (NLINK) )  
!#######################################################################
   qcdum = (qqqsl1 - qqqdum) * ccapi (ncont)  
   if (nwell.ne.0) qcdum = qcdum + qqqdum * ccccw (nwell, ncont)  


   QCP1 = qcdum / (D0 * Z2 * KS)  
!       QCP1 = QQQSL1*CCAPI(NCONT)/(D0*Z2*KS)
!                             SET VARIABLES FOR WET AND DRY INPUT OF
!                             CONTAMINANT FROM ABOVE
!######## temporary code for inclusion of irrigation water in rain water
   SUMD = zero  
   SUMS = zero  
   DO 150 JSED = 1, NSED  
      SUMD = SUMD+GINFD (NLINK, JSED)  
      SUMS = SUMS + GINFS (NLINK, JSED)  
      KDDUM (JSED) = KDDLS (JSED, NCONT)  
  150    END DO  
   IF (ISZERO(SUMD)) then  
      dddum = one  
   else  
      dddum = sumd  
   endif  
   IF (ISZERO(SUMS)) then  
      dsdum = one  
   else  
      dsdum = sums  
   endif  
   DO 152 JSED = 1, NSED  
      FBTAD (JSED) = GINFD (NLINK, JSED) / dddum  
      FBTAS (JSED) = GINFS (NLINK, JSED) / dsdum  

  152    END DO  
!                             SCALE RATES OF INFLITRATION TO GIVE THE
!                             FRACTIONS IN EACH GROUP OF AN EFFECTIVE
!                             SOIL. THE EFFECTIVE SOIL IS THAT WHICH IF
!                             ERODED AT A RATE EQUAL TO THE TOTAL RATE
!                             OF INFILTRATION WOULD RELEASE THE CORRECT
!                             AMOUNT OF SEDIMENTS FOR INFILTRATION

   PB = PBSED (NLINK)  
   FDUM = zero  
   FDUMC = zero  
   FDUMT = zero  
   CALL FRET (CCPBS, GNN (NCONT), PB, PB, FBTAD, FBTAD, KDDUM, PB, &
    PB, PB, FDUM, FDUMC, FDUMT, TSE, NSED, ISADNL)
   DUM = SUMD * CCPBS / CLENTH (NLINK)  
   ICPSBD = (FDUM - PB) * DUM  
   ICSBDC = FDUMC * DUM + ICPSBD  
   ICSBDT = FDUMT * DUM  
!                             SET INFILTRATION VARIABLES FOR BED DEEP
!                             LAYER
   IF (USCP.LT.half) THEN  
!                             THERE IS NO WATER IN LINK
      ICPSBS = zero  
      ICSBSC = zero  
      ICSBST = zero  
   ELSE  
      CALL FRET (CCPSF, GNN (NCONT), one, one, FBTAS, FBTAS, &
       KDDUM, zero, zero, zero, FDUM, FDUMC, FDUMT, TSE, NSED, &
       ISADNL)
      DUM = SUMD * CCPSF / CLENTH (NLINK)  
      ICPSBS = (FDUM - PB) * DUM  
      ICSBSC = FDUMC * DUM + ICPSBS  
      ICSBST = FDUMT * DUM  


   ENDIF  
!                             SET INFILTRATION VARIABLES FOR BED SURFACE
!                             LAYER
   ARL = DLS (NLINK) * CWIDTH (NLINK)  
!                             X-SECIONAL AREA OF LOOSE SEDIMENTS IN BED
   ARP = (ACPBD1 - ACPBS) * Z2SQ  
!                             X-SECTIONAL AREA OF NON-ERODED PARENT
!                             MATERIAL WITHIN BED DEEP LAYER
   DUM = one / (ARL + ARP)  
   DO 200 JSED = 1, NSED  
      SSBED1 (JSED) = DUM * (ARL * FBETA (NLINK, JSED) + ARP * &
       SOSDFN (NSOBED (NLINK), JSED) )
      SSBED (JSED) = FBBEDO (NLINK, JSED)  
      FBBEDO (NLINK, JSED) = SSBED1 (JSED)  
      SSF1 (JSED) = FDEL (NLINK, JSED)  
      SSF (JSED) = FDELO (NLINK, JSED)  
      FDELO (NLINK, JSED) = SSF1 (JSED)  
      SSD1 (JSED) = FBTSD (NLINK, JSED)  
      SSD (JSED) = FBTSDO (NLINK, JSED)  
      FBTSDO (NLINK, JSED) = SSD1 (JSED)  

  200    END DO  

   CALL FRET (CCPBD, GNN (NCONT), THBEDO (NLINK), THBED (NLINK), &
    SSBED, SSBED1, KDDUM, PB, PB, PB, FCPBD, FCPBDC, FCPBDT, TSE, &
    NSED, ISADNL)

   CALL FRET (CCPBS, GNN (NCONT), THBEDO (NLINK), THBED (NLINK), &
    SSBED, SSBED1, KDDUM, PB, PB, PB, FCPBS, FCPBSC, FCPBST, TSE, &
    NSED, ISADNL)
   CALL FRET (CCPSF, GNN (NCONT), one, one, SSF, SSF1, KDDUM, &
    zero, zero, zero, FCPSF, FCPSFC, FCPSFT, TSE, NSED, ISADNL)
   fsf (nlink, ncont) = fcpsf  
   fsfc (nlink, ncont) = fcpsfc  

   fsft (nlink, ncont) = fcpsft  
!                                       save retardation factors for con


   CALL FRET (CCPSF, GNN (NCONT), one, one, SSD, SSD1, KDDUM, &
    zero, zero, zero, FCPSD, FCPSDC, FCPSDT, TSE, NSED, ISADNL)
!                             SET REATRDATION VARIABLES FOR THE BED DEEP
!                             LAYER, BED SURFACE LAYER, STREAM WATER,
!                             AND NEWLY DEPOSITED SEDIMENTS
   DO 250 JBK = 1, 2  
      NA = NBK (JBK)  
      FCPSW1 (JBK) = RSW (NA, NCONT) + RSWT (NA, NCONT) * TSE+ &
       RSWC (NA, NCONT) * (CCCC (NA, NCETOP, NCONT) - CCPBK (JBK, &
       NCONT) )
      DO 252 NCE = NCEBK (JBK), NCETOP  
         FCPBK (JBK, NCE) = FCPBKO (NLINK, JBK, NCE, NCONT)  
         GCPBK (JBK, NCE) = GCPBKO (NLINK, JBK, NCE, NCONT)  
  252       END DO  
!                             NB: FCPBKO AND GCPBKO CALCULATED IN COLMSM


  250    END DO  
!                             SET RETRDATION VARIABLES FOR THE DYNAMIC
!                             AND DEAD SPACE REGIONS OF THE ERODING
!                             BANK SOIL
   ECPBD = zero  
   ECPBDC = zero  
   ECPBDT = zero  
   ECPBS = zero  
   ECPBSC = zero  
   ECPBST = zero  
   ECPSF = zero  
   ECPSFC = zero  

   ECPSFT = zero  
!                             SET RATES OF PLANT UPTAKE
   DUM = CWIDTH (NLINK) / D0  
   ACSBD1 = DUM * ALPHBD (NCONT)  
   ACSBS1 = DUM * ALPHBS (NCONT)  

   GCPLAL = GCPLA (NCONT)  
!                             SET CONTAMINANT INFILTRATION RATE WITH
!                             SEDIMENT; AND CONTAMINANT DECAY RATE
   CCPBD1 = zero  
   CCPBS1 = zero  
   CCPSF1 = zero  


   CALL LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
    NCETOP)
!                             CALCULATES AND RETURNS UPDATED
!                             CONCENTRATIONS
   CCCC (NLINK, NCETOP - 2, NCONT) = CCPBD1  
   CCCC (NLINK, NCETOP - 1, NCONT) = CCPBS1  


   CCCC (NLINK, NCETOP, NCONT) = CCPSF1  
!                             SAVE UPDATED CONCENTRATIONS IN THE GLOBAL
!                             ARRAYS
   CCBD1Q = CCPBD1  
   CCBS1Q = CCPBS1  
   CCSF1Q = CCPSF1  
   FCBD1Q = FCPBD+FCPBDT * TSE+FCPBDC * (CCPBD1 - CCPBD)  
   FCBS1Q = FCPBS + FCPBST * TSE+FCPBSC * (CCPBS1 - CCPBS)  
   FCSF1Q = FCPSF + FCPSFT * TSE+FCPSFC * (CCPSF1 - CCPSF)  


   GCPLAQ = GCPLAL  
!                             SET CONCENTRATIONS, RETARDATION, AND DECAY
!                             VARIABLES FOR PARENT CONTAMINANT FOR NEXT
!                             PASS OF DO LOOP 100

  100 END DO  
RETURN  
END SUBROUTINE LINKSM



!SSSSSS SUBROUTINE LINKW (NLINK)  
SUBROUTINE LINKW (NLINK)  
!
!----------------------------------------------------------------------*
!                             SETS UP THE WATER FLOW DATA FOR USE IN
!                             SUBROUTINE LINKSM AND LINK
!----------------------------------------------------------------------*
! Version:  SHETRAN/MOC/LINKW/4.1
! Modifications:
! RAH  970218  4.1  Swap subscripts: QVSH,DELTAZ,VSTHE (see AL.C).
!----------------------------------------------------------------------*
!
USE SED_CS  
USE COLM_C1  
USE COLM_CG  

USE COLM_CO  
!####################temporary, for irrigation
USE LINK_CC  
USE LINK_CW  
USE BK_CW  

USE PLANT_CC  
!                             INCLUDE ALL THE PARAMETER STATEMENTS
!                             AND COMMON BLOCKS NEEDED
!COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1  
!common / temp / nwell  
!##### nwell and qqqdum used in temporary irrigation code###########
!LOGICAL :: ISLK  
INTEGER :: nlink, jlend, jdum, lfone, ldum, jla, jfdum, jfdumb, nce, jvegbk, &
           ndum, la
DOUBLEPRECISION :: dumx, dum, duma, dmult, sumk, sum, dumk
!                             VARIABLES USED ONLY IN LINKW AND LINKSM
IF (LINKNS (NLINK) ) THEN  
   LENDA (1) = 2  
   LENDA (2) = 2  
   LENDA (3) = 1  
   LENDA (4) = 1  
   LENDA (5) = 1  
   LENDA (6) = 2  
ELSE  
   LENDA (1) = 1  
   LENDA (2) = 2  
   LENDA (3) = 2  
   LENDA (4) = 2  
   LENDA (5) = 1  
   LENDA (6) = 1  

ENDIF  
!                       SET POINTERS FOR THE END OF THE LINKS WHICH
!                       CAN BE ATTACHED TO A GIVEN LINK
ACPBD1 = ACPBI (NLINK) + ARBDEP (NLINK) / Z2SQ  
ACPBS = ACPBSG (NLINK)  
ACPSF1 = ARXL (NLINK) / Z2SQ  
IF (ACPSF1.LT.1.0D-20) THEN  
   USCP = zero  
   ACPBDT = zero  
   ACPSFT = zero  
   QBKB (NLINK, 1) = zero  
   QBKB (NLINK, 2) = zero  
!                             ENSURES BED LAYER CALCULATIONS ARE
!                             CORRECT IF THERE IS NO WATER IN LINK
ELSE  
   USCP = one  
   ACPBDT = (ACPBD1 - ACPBDO (NLINK) ) / TSE  
   ACPSFT = (ACPSF1 - ACPSFO (NLINK) ) / TSE  
ENDIF  
ACPBDO (NLINK) = ACPBD1  
ACPSFO (NLINK) = ACPSF1  
WCPBD1 = Z2SQOD * ACPBDT / ACPBD1  

VCPBK1 = Z2OD * GNUBK (NLINK)  
!                             SET SCALED VARIABLES FOR AREA AND EROSION
NBK (1) = NBANK (NLINK, 1)  
NBK (2) = NBANK (NLINK, 2)  
NCEBK (1) = NHBED (NLINK, 1) + 1  


NCEBK (2) = NHBED (NLINK, 2) + 1  
!                             SET LOCAL BANK NUMBERS AND NUMBERS FOR THE
!                             BOTTOM CELLS FOR THE EXPOSED PART OF THE
!                             BANK SOIL COLUMNS
IF (LINKNS (NLINK) ) THEN  
   LFONE = 2  
ELSE  
   LFONE = 1  

ENDIF  
!                             SET NUMBER FOR THE FACE OF THE LINK WHICH
!                             IS AT END ONE OF THE LINK
LDUM = ICMREF (NLINK, LFONE+4)  
IF (LDUM.GT.0) THEN  
!                             THERE IS ONLY ONE OTHER LINK ASSOCIATED
!                             WITH END ONE OF THE CURRENT LINK
   ISLK (1) = .TRUE.  
   LWORK (1) = 0  
   LWORK (2) = 0  
   LWORK (3) = 0  
   if (linkns (nlink) ) then  
      if (icmref (nlink, 10) .eq.3) lwork (1) = ldum  
      if (icmref (nlink, 10) .eq.4) lwork (2) = ldum  
      if (icmref (nlink, 10) .eq.1) lwork (3) = ldum  
   else  
      if (icmref (nlink, 9) .eq.2) lwork (1) = ldum  
      if (icmref (nlink, 9) .eq.3) lwork (2) = ldum  
      if (icmref (nlink, 9) .eq.4) lwork (3) = ldum  
   endif  
!                             LWORK HOLDS THE NUMBERS OF THE LINKS
!                             ASSOCIATED WITH THE CURRENT LINK
ELSEIF (LDUM.LT.0) THEN  
!                             THERE IS MORE THAN ONE LINK ASSOCIATED
!                             WITH END ONE OF THE CURRENT LINK
   ISLK (1) = .TRUE.  
   LWORK (1) = ICMRF2 ( - LDUM, 3)  
   LWORK (2) = ICMRF2 ( - LDUM, 2)  
   LWORK (3) = ICMRF2 ( - LDUM, 1)  
ELSE  
!                             THERE IS NO LINKS ASSOCIATED WITH END ONE
!                             OF THE CURRENT LINK
   ISLK (1) = .FALSE.  
   LWORK (1) = 0  
   LWORK (2) = 0  
   LWORK (3) = 0  


ENDIF  
LDUM = ICMREF (NLINK, LFONE+6)  
IF (LDUM.GT.0) THEN  
!                             THERE IS ONLY ONE OTHER LINK ASSOCIATED
!                             WITH END TWONE OF THE CURRENT LINK
   ISLK (2) = .TRUE.  
   LWORK (4) = 0  
   LWORK (5) = 0  
   LWORK (6) = 0  
   if (linkns (nlink) ) then  
      if (icmref (nlink, 12) .eq.1) lwork (4) = ldum  
      if (icmref (nlink, 12) .eq.2) lwork (5) = ldum  
      if (icmref (nlink, 12) .eq.3) lwork (6) = ldum  
   else  
      if (icmref (nlink, 11) .eq.4) lwork (4) = ldum  
      if (icmref (nlink, 11) .eq.1) lwork (5) = ldum  
      if (icmref (nlink, 11) .eq.2) lwork (6) = ldum  
   endif  
!                             LWORK HOLDS THE NUMBERS OF THE LINKS
!                             ASSOCIATED WITH THE CURRENT LINK
ELSEIF (LDUM.LT.0) THEN  
!                             THERE IS MORE THAN ONE LINK ASSOCIATED
!                             WITH END TWO OF THE CURRENT LINK
   ISLK (2) = .TRUE.  
   LWORK (4) = ICMRF2 ( - LDUM, 3)  
   LWORK (5) = ICMRF2 ( - LDUM, 2)  
   LWORK (6) = ICMRF2 ( - LDUM, 1)  
ELSE  
!                             THERE IS NO LINKS ASSOCIATED WITH END TWO
!                             OF THE CURRENT LINK
   ISLK (2) = .FALSE.  
   LWORK (4) = 0  
   LWORK (5) = 0  
   LWORK (6) = 0  

ENDIF  
!                             SET LWORK, THE ARRAY HOLDING THE NUMBERS
!                             THE LINKS ASSOCIATED WITH THE CURRENT LINK
DUMX = one / (D0 * Z2)  
DO 100 JLEND = 1, 2  
   IF (ISLK (JLEND) ) THEN  
!                             THERE ARE OTHER LINKS ASSOCIATED WITH END
!                             JLEND OF THE CURRENT LINK
      DO 102 JDUM = 1, 3  
         JLA = (JLEND-1) * 3 + JDUM  
         LA = LWORK (JLA)  
         IF (LA.NE.0) THEN  
            ACSFA1 (JLA) = MAX (1.0d-6, ACPSFO (LA) )  
            DUM = zero  
            PCSFA1 (JLA) = DUMX * ( - QLINK (LA, LENDA (JLA) ) &
             - QDEFF (LA, LENDA (JLA) ) * DUM) / ACSFA1 (JLA)
!                             NB: CONVECTION WITH DISPERSED SEDIMENTS
!                             NEGLECTED
         ELSE  
            ACSFA1 (JLA) = zero  
            PCSFA1 (JLA) = zero  
         ENDIF  

  102       END DO  
   ELSE  
!                             END JLEND OF LINK IS AT CATCHMENT BOUNDARY
!                             THE HEAD OF A STREAM, OR A SPRING
      JLA = (JLEND-1) * 3 + 1  
      ACSFA1 (JLA) = MAX (1.0d-6, ACPSFO (NLINK) )  
      PCSFA1 (JLA) = DUMX * QLINK (NLINK, JLEND) / ACSFA1 (JLA)  
!                             FOR FLOW INTO CATCHMENT OR FROM SPRING
      DO 104 JDUM = 2, 3  
         JLa = (JLEND-1) * 3 + JDUM  
         ACSFA1 (JLA) = zero  
         PCSFA1 (JLA) = zero  
  104       END DO  
   ENDIF  
  100 END DO  
IF (USCP.LT.half) THEN  
   PCSFM1 = zero  
   PCSFP1 = zero  
ELSE  
   DUM = DUMX / ACPSF1  
   DUMA = zero  
   PCSFM1 = DUM * (QLINK (NLINK, 1) + DUMA * QDEFF (NLINK, 1) )  
   PCSFP1 = DUM * (QLINK (NLINK, 2) + DUMA * QDEFF (NLINK, 2) )  


ENDIF  
!                             SET AREA AND PECLET NUMBER FOR THE LINKS
!                             ASSOCIATED WITH THE CURRENT LINKS
QQQSL1 = - PNETTO (NLINK) * AREA (NLINK)  
!                             -VE RATE OF RAIN ARRIVAL AT LINK
!                             (+VE UPWARDS TO CONFORM TO CONVENTION)
!#######################################################################
nwell = NVSWLT (nlink)  
if (nwell.ne.0) then  
   qqqdum = - rszwlo (nwell) * area (nwell)  
else  
   qqqdum = zero  


endif  
!###########temporary, qqqdum is rate of input of well water to stream##

KS = CLENTH (NLINK) / Z2  
!                             SET SCALED LENGTH OF LINK
DUM = DUMX / KS  
DO 150 JBK = 1, 2  
   JFDUM = 2 * JBK - LFONE+1  
!                             FACE NUMBER FOR LINK, ACROSS WHICH WATER
!                             ENTERS FROM BANK JBK

   JFDUMB = ICMREF (NLINK, JFDUM + 8)  
!                               FACE NUMBER FOR BANK, POINTING TOWARDS L
   DO 152 NCE = NCEBK (JBK), NCETOP  
      PCPBK1 (JBK, NCE) = - DUM * QVSH (JFDUMB, NCE, NBK (JBK) )  
  152    END DO  
   PCPSB1 (JBK) = - DUM * QBKB (NLINK, JBK)  
   DMULT = DBLE (2 * JBK - 3)  
!                             MULTIPLIER USED TO OBTAIN CORRECT SIGN FOR
!                             FLOWS INTO THE LINK
   PCPSW1 (JBK) = DMULT * DUM * QOC (NLINK, JFDUM)  
   JVEGBK = NVC (NBK (JBK) )  
   NDUM = NCEBD (NLINK, JBK) + 1  
!                             k IS ONLY USED FOR THE CELLS AT OR ABOVE
!                             THE LEVEL OF THE BOTTOM OF THE BED DEEP
!                             LAYER
   DO 154 NCE = NDUM, NCETOP  
      KSPBK (JBK, NCE) = DELTAZ (NCE, NBK (JBK) ) / z2  
  154    END DO  



  150 END DO  
!                             SET VALUES OF VARIABLES ASSOCIATED WITH
!                             THE ADJACENT STREAM BANKS
SUMK = zero  
SUM = zero  
DO 160 JBK = 1, 2  
   NCE = NDUM  
   DUMK = (one - FNCEBD (NLINK, JBK) ) * KSPBK (JBK, NCE)  
   SUMK = SUMK + DUMK  
   SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK  
   DO 162 NCE = NDUM + 1, NHBED (NLINK, JBK)  
      DUMK = KSPBK (JBK, NCE)  
      SUMK = SUMK + DUMK  
      SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK  
  162    END DO  
   NCE = NHBED (NLINK, JBK) + 1  
   DUMK = FHBED (NLINK, JBK) * KSPBK (JBK, NCE)  
   SUMK = SUMK + DUMK  
   SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK  
  160 END DO  
THBEDO (NLINK) = THBED (NLINK)  



THBED (NLINK) = MIN(PBSED (NLINK), SUM / SUMK)  
!                             SET MOISTURE CONTENT IN STREAM BED, AS THE
!                             WEIGHTED AVERAGE FOR THE CELLS, OF BOTH
!                             BANKS, LYING WITHIN THE BED SURFACE AND
!                             BED DEEP LAYERS
RETURN  



END SUBROUTINE LINKW
! 15/1/96
!                             4/9/91



!SSSSSS SUBROUTINE LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
SUBROUTINE LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
 NCETOP)
!
!                             SETS UP AND SOLVES THE STREAM
!                             LINK DIFFERENCE EQUATIONS
!                             ** FULLY IMPLICIT COUPLING TO BANKS **
!
USE LINK_CC  

USE LINK_CC1
INTEGER :: NCETOP, nc, nk, njda
DOUBLEPRECISION :: CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
           duma1, duma2, duma3, duma4, duma5, duma6, duma7, dump5, dump6, &
           dumb1, dumb2, dumb3, dumb3a, dumb3b, &
           dump1, dump2, dump3, dump4, dump7, dsum, sum, &
           sum1, sum2, sum3, sum4, sum5, alt, altstr, &
           blt, bltstr, dlt, dltstr, elt, eltstr, eltda, dltda, dumf1, &
           flt, fltstr, fltda, hlt, hltstr, hltda, &
           plt, dumq1, qlt, qltda, slt, sltda, gylt, gyltda, gyltsr, &
           wmesf, wmebs, wmebd
DUMA1 = (MAX (zero, - PCSFP1) + MAX (zero, - PCSFM1) ) &
 / KS
DUMA2 = FCPSF + TSE * FCPSFT + FCPSFC * CCPSF  
DUMA3 = MAX (zero, WCPBD1)  
DUMA4 = FCPSD+TSE * FCPSDT + FCPSDC * CCPSF  
SUM3 = zero  
SUM4 = zero  
SUM5 = zero  
DO 1 NK = 1, 2  
   SUM1 = zero  
   SUM2 = zero  
   DO 2 NC = NCEBK (NK), NCETOP  
      SUM1 = SUM1 + MAX (zero, - PCPBK1 (NK, NC) )  
      SUM2 = SUM2 + MAX (zero, PCPBK1 (NK, NC) ) * CCPBK (NK, &
       NC)
      SUM3 = SUM3 + (FCPBK (NK, NC) * CCPBK (NK, NC) + GCPBK (NK, &
       NC) * SCPBK (NK, NC) ) * KSPBK (NK, NC)
    2    END DO  
   SUM4 = SUM4 + SUM1  
   SUM5 = SUM5 + SUM2  
    1 END DO  
DUMA5 = SUM4  
DUMP5 = VCPBK1 * SUM3  
DUMP6 = SUM5  
DUMA6 = MAX (zero, - PCPSW1 (1) ) + MAX (zero, - PCPSW1 (2) &
 )
DUMA7 = MAX (zero, - PCPSB1 (1) ) + MAX (zero, - PCPSB1 (2) &
 )
ALT = ACPSF1 * (one + TSE * (DUMA1 + GCPLAL) ) * DUMA2 + TSE * &
 (DUMA3 * ACPBD1 * DUMA4 + ICSBSC + ECPSFC) + TSE * (DUMA5 + DUMA6 &
 * DUMA2 + DUMA7)


ALTSTR = TSE * ( (ACPSF1 * (one + TSE * (DUMA1 + GCPLAL) ) &
 + TSE * DUMA6) * FCPSFC + TSE * DUMA3 * ACPBD1 * FCPSDC)
!                             SET a AND a*
DUMB1 = MAX (zero, - WCPBD1)  
DUMB2 = FCPBS + TSE * FCPBST + FCPBSC * CCPBS  
DUMB3A = MAX (zero, PCPSB1 (1) )  
DUMB3B = MAX (zero, PCPSB1 (2) )  
DUMB3 = DUMB3A + DUMB3B  
BLT = - TSE * (DUMB1 * ACPBD1 * DUMB2 + DUMB3)  


BLTSTR = - TSE * TSE * DUMB1 * ACPBD1 * FCPBSC  
!                             SET b AND b*
DLT = - TSE * (DUMA3 * ACPBD1 * DUMA4 + ICSBSC + DUMA7)  
DLTSTR = - TSE * TSE * DUMA3 * ACPBD1 * FCPSDC  


DLTDA = - TSE * USCP * ACSBS1  
!                             SET d, d*, AND d'
ELT = (ACPBS + TSE * (GCPLAL * ACPBS + (DUMA3 + DUMB1) * ACPBD1) ) &
 * DUMB2 + TSE * (ICSBDC + ECPBSC + DUMA7 + DUMB3)
ELTSTR = TSE * (ACPBS + TSE * (GCPLAL * ACPBS + (DUMA3 + DUMB1) &
 * ACPBD1) ) * FCPBSC


ELTDA = TSE * (USCP * ACSBS1 + ACSBD1)  
!                             SET e, e*, AND e'
DUMF1 = FCPBD+TSE * FCPBDT + FCPBDC * CCPBD  
FLT = - TSE * (DUMB1 * ACPBD1 * DUMF1 + DUMB3)  
FLTSTR = - TSE * TSE * DUMB1 * ACPBD1 * FCPBDC  

FLTDA = - TSE * ACSBD1  
!                             SET f, f*, AND f'
HLT = - TSE * (DUMA3 * ACPBD1 * DUMB2 + DUMA7 + ICSBDC)  
HLTSTR = - TSE * TSE * DUMA3 * ACPBD1 * FCPBSC  


HLTDA = - TSE * ACSBD1  
!                             SET h, h*, AND h'
DUMP1 = (FCPSF + TSE * FCPSFT) * CCPSF  
DUMP2 = (FCPSD+TSE * FCPSDT) * CCPSF  
DUMP3 = (FCPBS + TSE * FCPBST) * CCPBS  
DSUM = zero  
SUM = zero  
SUM1 = zero  
IF (PCSFM1.GT.0) THEN  
   DO 3 NJDA = 1, 3  
      SUM = SUM + ACSFA1 (NJDA) * MAX (zero, PCSFA1 (NJDA) ) &
       * FCSFA1 (NJDA) * CCSFA1 (NJDA)
      SUM1 = SUM1 + ACSFA1 (NJDA) * PCSFA1 (NJDA)  
    3    END DO  
   IF (NOTZERO(SUM1)) DSUM = ACPSF1 * PCSFM1 * SUM / SUM1  
ENDIF  
SUM = zero  
SUM1 = zero  
IF (PCSFP1.GT.0) THEN  
   DO 5 NJDA = 4, 6  
      SUM = SUM + ACSFA1 (NJDA) * MAX (zero, PCSFA1 (NJDA) ) &
       * FCSFA1 (NJDA) * CCSFA1 (NJDA)
      SUM1 = SUM1 + ACSFA1 (NJDA) * PCSFA1 (NJDA)  
    5    END DO  
   IF (NOTZERO(SUM1)) DSUM = DSUM + ACPSF1 * PCSFP1 * SUM / SUM1  
ENDIF  
DUMP4 = DSUM / KS  
SUM = zero  
DO 4 NK = 1, 2  
   SUM = SUM + MAX (zero, PCPSW1 (NK) ) * FCPSW1 (NK) * CCPGS1 &
    (NK)
    4 END DO  
DUMP7 = SUM  


PLT = - (ACPSF1 * FCPSFT + ACPSFT * FCPSF) * CCPSF - ACPSF1 * &
 (DUMA1 + GCPLAL) * DUMP1 + DUMP4 + DUMP5 - DUMA3 * ACPBD1 * DUMP2 &
 + DUMB1 * ACPBD1 * DUMP3 - USCP * (QCP1 + ICP1) - ICPSBS - TSE * &
 ICSBST + GCPLAQ * ACPSF1 * FCSF1Q * CCSF1Q - ECPSF - TSE * ECPSFT &
 + DUMP6 - DUMA5 * CCPSF + DUMP7 - DUMA6 * DUMP1 + DUMB3 * CCPBS - &
 DUMA7 * CCPSF
!                                       SET p
DUMQ1 = (FCPBD+TSE * FCPBDT) * CCPBD  
QLT = - ACPBS * FCPBST * CCPBS - (GCPLAL * ACPBS + (DUMA3 + DUMB1) &
 * ACPBD1) * DUMP3 + DUMA3 * ACPBD1 * DUMP2 + DUMB1 * ACPBD1 * &
 DUMQ1 - (one - USCP) * (QCP1 + ICP1) + ICPSBS - ICPSBD-ECPBS + &
 TSE * (ICSBST - ICSBDT - ECPBST) + GCPLAQ * ACPBS * FCBS1Q * &
 CCBS1Q + DUMB3 * (CCPBD-CCPBS) - DUMA7 * (CCPBS - CCPSF)


QLTDA = USCP * ACSBS1 * (CCPSF - CCPBS) - ACSBD1 * (CCPBS - CCPBD)  
!                                       SET q ANS q'
SLT = - (ACPBD1 * FCPBDT + ACPBDT * FCPBD) * CCPBD- ( (GCPLAL + &
 DUMB1) * DUMQ1 - DUMA3 * DUMP3) * ACPBD1 + ICPSBD-ECPBD+TSE * &
 (ICSBDT - ECPBDT) + GCPLAQ * ACPBD1 * FCBD1Q * CCBD1Q + DUMB3A * &
 (CCPBK (1, 1) - CCPBD) + DUMB3B * (CCPBK (2, 1) - CCPBD) - DUMA7 * &
 (CCPBD-CCPBS)


SLTDA = ACSBD1 * (CCPBS - CCPBD)  
!                                       SET s AND s'
GYLT = ACPBD1 * (one + TSE * (GCPLAL + DUMB1) ) * DUMF1 + TSE * &
 (ECPBDC + DUMA7 + DUMB3)
GYLTSR = TSE * ACPBD1 * (one + TSE * (GCPLAL + DUMB1) ) * &
 FCPBDC


GYLTDA = TSE * ACSBD1  
!                             SET y, y*, AND Y'
IF (USCP.LT.half) THEN  
   CALL SNL3 (one, zero, zero, zero, zero, - DLT - DLTDA, &
    - DLTSTR, ELT + ELTDA, ELTSTR, - FLT - FLTDA, - FLTSTR, &
    - HLT - HLTDA, - HLTSTR, zero, QLT + QLTDA, SLT + SLTDA, &
    WMESF, WMEBS, WMEBD, GYLT + GYLTDA, GYLTSR)
!                             SPECIAL CASE: NO WATER IN LINK
ELSE  
   CALL SNL3 (ALT - DLTDA, ALTSTR, - BLT + ELTDA + HLTDA, - &
    BLTSTR, zero, - DLT - DLTDA, - DLTSTR, ELT + ELTDA, ELTSTR, &
    - FLT - FLTDA, - FLTSTR, - HLT - HLTDA, - HLTSTR, PLT - QLTDA - &
    SLTDA, QLT + QLTDA, SLT + SLTDA, WMESF, WMEBS, WMEBD, GYLT + &
    GYLTDA, GYLTSR)
ENDIF  
CCPBD1 = CCPBD+TSE * WMEBD  
CCPBS1 = CCPBS + TSE * WMEBS  

CCPSF1 = CCPSF + TSE * WMESF  
!                             SOLVE THE DIFFERENCE EQUATIONS
!                             AND UPDATE THE CONCENTRATIONS
RETURN  
END SUBROUTINE LINK
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!

! 12/8/94




!FFFFFF DOUBLEPRECISION FUNCTION PHI
DOUBLEPRECISION FUNCTION PHI (JSOIL, THETA)  
!                             FRACTION OF SOIL WATER WHICH IS MOBILE
INTEGER :: JSOIL  
DOUBLEPRECISION THETA  
PHI = half  
!                             ########## SOIL INFO NEEDED HERE #########






END FUNCTION PHI



!SSSSSS SUBROUTINE PLCOLM (NCL, NCONT)  
SUBROUTINE PLCOLM (NCL, NCONT)  
!                 Updates the plant compartment concentrations for
!                 for one column for one timestep for one contaminant
USE CONT_CC  
USE COLM_C1  
!                 NB COLM.C1 includes AL.P
USE COLM_C2  

USE COLM_CC  

USE PLANT_CC  
!                 Include parameter statements, water/contaminant
!                 interface COMMON blocks, and plant COMMON blocks
!                 called just before routine COLM
INTEGER :: NCL, NCONT, jplant, nce, jplty, nrbot
DOUBLEPRECISION :: d1dum, d2dum, d3dum, d4dum, o2dum, f1dum, f2dum, pkdum, &
           pmdum, sum, z2dum, xdum, cdum, sdum, tdum, dum, eddum, qdum, bcdum, dum1, dum3, bcpaa1, bcpbb1
IF (NCONT.EQ.1) THEN  
   DO 100 JPLANT = 1, NPL (NCL)  
      GENAA (JPLANT) = zero  
      GENBB (JPLANT) = zero  
  100    END DO  

ENDIF  
!                 Set generation variables to zero if call is for first
!                 contaminant
DO 900 NCE = 1, NCETOP  
   EDCAP (NCE) = zero  
   EDCAPC (NCE) = zero  
   EDCAPT (NCE) = zero  
   ESCAP (NCE) = zero  
   ESCAPS (NCE) = zero  
   ESCAPT (NCE) = zero  



  900 END DO  
!                 Set uptake variables to zero in preparation for
!                 summing net uptake over all plant types on
!                 column NCL
!                 Main calculation loops
DO 1000 JPLANT = 1, NPL (NCL)  
!                 For each plant type on soil column NCL
   JPLTY = NPLTYP (NCL, JPLANT)  
!                 Plant type number

   NRBOT = NCETOP - NRD (JPLTY)  
!                 Number of bottom rooted cell
   D1DUM = DELONE (JPLTY)  
   D2DUM = DELTWO (JPLTY)  
   D3DUM = DELTHR (JPLTY)  
   D4DUM = DELFOU (JPLTY)  
   O2DUM = one - D2DUM  
   F1DUM = PFONE (NCL, JPLANT)  
   F2DUM = PFTWO (JPLTY) / PF2MAX (JPLTY)  
   PKDUM = PKMAX (JPLTY, NCONT)  

   PMDUM = PMASS (JPLTY)  
   GCPL = GCPLA (NCONT)  
!                 Non dimensioned decay variable, set up in MUZ
   GMCPAA = (one - D1DUM)  

   GMCPBB = F2DUM * D1DUM  
   SUM = zero  
   Z2DUM = Z2SQOD * F2DUM * PKDUM  
   DO 1610 NCE = NRBOT, NCETOP  
      XDUM = XXI * PPHI (NCE)  
      CDUM = XDUM * COLCAP (NCE)  
      SDUM = (one - XDUM) * SOLCAP (NCE)  
      TDUM = CDUM + SDUM  
      DUM = Z2DUM * PDZF3 (NCL, NCE, JPLANT) * TDUM  
      SUM = SUM + DUM  
      EDDUM = DUM * F1DUM / (TDUM * (Z2 * KSP (NCE) ) )  
      EDCAP (NCE) = EDCAP (NCE) + CDUM * EDDUM  
      ESCAP (NCE) = ESCAP (NCE) + SDUM * EDDUM  
!                  Set net scaled uptake rates for use in routine COLM
!            ----- NB sums up over all plant types
!            ----- NB THE RECYLING TERMS FOR EDCAP AND ESCAP ARE
!                    ADDED BELOW
 1610    END DO  
   QDUM = SUM / (PMDUM * (GMCPAA + (D3DUM * GMCPBB) ) )  
   QCPAA = GMCPAA * QDUM  

   QCPBB = D3DUM * GMCPBB * QDUM  
!                 Evaluate scaled values for Qa and Qb using
!                 equations in section 3 of WRSRU/TR/9107/12
   GMCBBD = (GMCPBB - GMCBBO (NCL, JPLANT) ) / TSE  
   GMCBBO (NCL, JPLANT) = GMCPBB  
   IF (LTZERO(GMCBBD)) THEN  
      BCDUM = BCPBB (NCL, JPLANT, NCONT)  
      DUM1 = F1DUM * D4DUM * BCDUM * GMCBBD  
      DUM3 = O2DUM * PDZF3 (NCL, NCETOP, JPLANT)  
      EDCAP (NCETOP) = EDCAP (NCETOP) + DUM1 * (D2DUM + DUM3) &
       / (Z2 * KSP (NCETOP) * RHOPL)
      DO 1630 NCE = NRBOT, NCETOP - 1  
         EDCAP (NCE) = EDCAP (NCE) + DUM1 * DUM3 / (Z2 * KSP (NCE) &
          * RHOPL)
 1630       END DO  

   ENDIF  
   CALL PLANT (JPLANT, BCPAA (NCL, JPLANT, NCONT), BCPAA1, BCPBB ( &
    NCL, JPLANT, NCONT), BCPBB1, TSE)
   BCPAA (NCL, JPLANT, NCONT) = BCPAA1  

   BCPBB (NCL, JPLANT, NCONT) = BCPBB1  
!                 Call solve routine and update concentrations

 1000 END DO  
RETURN  










END SUBROUTINE PLCOLM




!SSSSSS SUBROUTINE SLVCLM (N)  
SUBROUTINE SLVCLM (N)  
!                            SOLVES THE DIFFERENCE EQUATIONS
!                            FOR ONE CONTAMINANT AT ONE COLUMN
!                            FOR ONE TIME STEP
USE COLM_CC1  
INTEGER :: n, na, loop
DOUBLEPRECISION ELTE (LLEE), PLTE (LLEE), RHTD (LLEE)
!                            ALLOCATE WORKSPACE
DO 1 NA = 1, N  
   ELTE (NA) = ELT (NA) - GLT (NA) * TLT (NA) / PLT (NA)  
   RHTD (NA) = SLT (NA) + GLT (NA) * QLT (NA) / PLT (NA)  
    1 END DO  
CALL TRIDAG (FLT, ELTE, DLT, RHTD, OME, N)  
DO 2 NA = 1, N  
   EPS (NA) = (QLT (NA) + TLT (NA) * OME (NA) ) / PLT (NA)  

    2 END DO  
!                            ESTIMATE OMEGA AND EPSILON VECTORS
IF (ISADNL) THEN  
!                            GO ROUND LOOP 3 ONLY IF
!                            THERE IS NONLINEAR ADSORPTION
   DO 3 LOOP = 1, 10  
      DO 4 NA = 1, N  
         PLTE (NA) = PLT (NA) + PLTSTR (NA) * EPS (NA)  
         ELTE (NA) = ELT (NA) + ELTSTR (NA) * OME (NA) - GLT (NA) &
          * TLT (NA) / PLTE (NA)
         RHTD (NA) = SLT (NA) + GLT (NA) * QLT (NA) / PLTE (NA)  


    4       END DO  
!                            SET 'NON-LINEAR' COEFFICIENTS


      CALL TRIDAG (FLT, ELTE, DLT, RHTD, OME, N)  
!                            ESTIMATE OMEGA VECTOR
      DO 5 NA = 1, N  
         EPS (NA) = (QLT (NA) + TLT (NA) * OME (NA) ) / PLTE (NA)  
    5       END DO  
!                            ESTIMATE EPSILON VECTOR

    3    END DO  
ELSE  
   RETURN  

ENDIF  
RETURN  

END SUBROUTINE SLVCLM
! 12/8/94



!SSSSSS subroutine RET (C, GN, THO, TH, FRNO, FRN, KDREF, R, RC, RT, DT, &
subroutine RET (C, GN, THO, TH, FRNO, FRN, KDREF, R, RC, RT, DT, &
 NSED, ISNL)
!                             CALCULATES THE GROUND SURFACE RETARDATION
!                             FACTOR, R,
!                             ITS CONCENTRATION DERIVATIVE ,RC,
!                             AND ITS TIME DERIVATIVE ,RT,
!                             DEPENDING ON THE CONCENTRATION IN THE
!                             SURFACE WATER ,C,
!                             PARTICLE SIZE FRACTIONS, FRNO AND FRN,
!                             FREUNDLICH POWER, GN,
!                             OLD AND NEW MOISTURE CONTENTS, THO AND TH,
!                             REFERENCE Kd, KDREF,
!                             SCALED TIME STEP, DT,
!                             NUMBER OF SEDIMENT FRACTIONS, NSED,
!                             AND THE 'NON-LINEAR ADSORPTION' FLAG, ISNL
!
!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
DOUBLEPRECISION :: C, GN, THO, TH, R, RC, RT, DT
DOUBLEPRECISION FRNO ( * ), FRN ( * ), KDREF ( * )  
INTEGER :: NSED, jsed, nj
DOUBLEPRECISION :: dumo, dum, sumo, sum, cdum, dumko, dumk, x1min

LOGICAL :: ISNL  
DUMO = one / THO  
DUM = one / TH  
SUMO = zero  
SUM = zero  
DO 1 JSED = 1, NSED  
   SUMO = SUMO + FRNO (JSED) * KDREF (JSED)  
   SUM = SUM + FRN (JSED) * KDREF (JSED)  
    1 END DO  
IF (.NOT.ISNL) THEN  
!                             IS LINEAR ADSORPTION
   R = one + SUMO * DUMO  
   RT = (SUM * DUM - SUMO * DUMO) / DT  
   RC = zero  
ELSE  
   CDUM = C** (GN - two)  
   DUMKO = SUMO * DUMO * CDUM  
   DUMK = SUM * DUM * CDUM  
   R = one + DUMKO * C  
   RT = (DUMK - DUMKO) * C / DT  
   RC = (GN - one) * DUMKO  
ENDIF  
return  
end subroutine RET











!SSSSSS SUBROUTINE SNL3 (A, AS, B, BS, C, D, DS, E, ES, F, FS, H, HS, P, &
SUBROUTINE SNL3 (A, AS, B, BS, C, D, DS, E, ES, F, FS, H, HS, P, &
 Q, S, X1, X2, X3, AY, AYS)
!                             SOLVES THE COUPLED NON-LINEAR STREAM
!                             DIFFERENCE EQUATIONS:
!
!                 (A+AS.X1) X1 - (B+BS.X2) X2       - ( C ) X3 = P
!                -(D+DS.X1) X1 + (E+ES.X2) X2   - (F+FS.X3) X3 = Q
!                              - (H+HS.X2) X2 + (AY+AYS.X3) X3 = S
!
!      IMPLICIT DOUBLEPRECISION(A-H,O-Z)
!                             FIND ROOTS USING ITERATION METHOD
INTEGER :: nj, njtest
DOUBLEPRECISION :: A, AS, B, BS, C, D, DS, E, ES, F, FS, H, HS, P, &
           Q, S, X1, X2, X3, AY, AYS
DOUBLEPRECISION :: x1min, x2min, x3min, x1old, x2old, x3old, xref, perr, qerr, serr
X1 = zero  
X2 = zero  
X3 = zero  
DO 1 NJ = 1, 100  
   X1 = (P + (B + BS * X2) * X2 + C * X3) / (A + AS * X1)  
   X2 = (Q + (D+DS * X1) * X1 + (F + FS * X3) * X3) / (E+ES * X2)  
   X3 = (S + (H + HS * X2) * X2) / (AY + AYS * X3)  

    1 END DO  
!                             CHECK SOLUTION IS WITHIN THE CONVERGENCE
!                             REGION
IF (ISZERO(AS)) THEN  
   X1MIN = X1  
ELSE  
   X1MIN = ( - A + DABS (B + two * BS * X2) + C) / (two * AS)  
ENDIF  
IF (ISZERO(ES)) THEN  
   X2MIN = X2  
ELSE  
   X2MIN = ( - E+DABS (D+two * DS * X1) + DABS (F + two * FS * &
    X3) ) / (two * ES)
ENDIF  
IF (ISZERO(AYS)) THEN  
   X3MIN = X3  
ELSE  
   X3MIN = ( - AY + DABS (H + two * HS * X2) ) / (two * AYS)  
ENDIF  
IF ( (X1.LT.X1MIN) .OR. (X2.LT.X2MIN) .OR. (X3.LT.X3MIN) ) THEN  
   PRINT '(A40)', ' LINK: FATAL CONVERGENCE ERROR 1 IN SNL3'  
PRINT '(A33)', '       ^^^^^^^^^^^^^^^^^^^^^^^^^'  


ENDIF  
!                             RUN THREE FURTHER ITERATION STEPS TO SEE
!                             IF THE SOLUTION IS STABLE
X1OLD = X1  
X2OLD = X2  
X3OLD = X3  
DO 2 NJTEST = 1, 3  
   X1 = (P + (B + BS * X2) * X2 + C * X3) / (A + AS * X1)  
   X2 = (Q + (D+DS * X1) * X1 + (F + FS * X3) * X3) / (E+ES * X2)  

   X3 = (S + (H + HS * X2) * X2) / (AY + AYS * X3)  
   XREF = ABS (X1) + ABS (X2) + ABS (X3)  
   if (NOTZERO(xref)) then  
      IF (ABS (X1 - X1OLD) + ABS (X2 - X2OLD) + ABS (X3 - X3OLD) &
       / XREF.GT.1.0D-2) THEN
PRINT '(A40)', ' LINK: FATAL CONVERGENCE ERROR 2 IN SNL3'  
PRINT '(A33)', '       ^^^^^^^^^^^^^^^^^^^^^^^^^'  
      ENDIF  
   endif  

    2 END DO  
!                             CHECK THE SOLUTION IS ACCURATE
IF (ISZERO(P)) THEN  
   PERR = zero  
ELSE  
   PERR = ( (A + AS * X1) * X1 - (B + BS * X2) * X2 - C * X3 - P) &
    / P
ENDIF  
IF (ISZERO(Q)) THEN  
   QERR = zero  
ELSE  
   QERR = ( - (D+DS * X1) * X1 + (E+ES * X2) * X2 - (F + FS * X3) &
    * X3 - Q) / Q
ENDIF  
IF (ISZERO(S)) THEN  
   SERR = zero  
ELSE  
   SERR = ( - (H + HS * X2) * X2 + (AY + AYS * X3) * X3 - S) &
    / S
ENDIF  
IF ( (ABS (PERR) + ABS (QERR) + ABS (SERR) ) .GE.1.0D-2) THEN  
   count = count + 1  
   IF (count<10) THEN  
      PRINT '(A35)', ' LINK: CONVERGENCE ERROR 3 IN SNL3'  
!              PRINT '(A27)' , '       ^^^^^^^^^^^^^^^^^^^'
   ELSEif (COUNT == 10) then  
PRINT '(A)', ' LINK: CONVERGENCE ERROR 3 IN SNL3 - MESSAGES NOW SUPPRESSED'
   ENDIF  

ENDIF  
RETURN  
END subroutine SNL3

! 12/8/94



!SSSSSS SUBROUTINE FRET (C, GN, THO, TH, FRNO, FRN, KDREF, PO, P, PREF, F, &
SUBROUTINE FRET (C, GN, THO, TH, FRNO, FRN, KDREF, PO, P, PREF, F, &
 FC, FT, DT, NSED, ISNL)
!                             CALCULATES THE LINK RETARDATION
!                             FACTOR, F,
!                             ITS CONCENTRATION DERIVATIVE ,FC,
!                             AND ITS TIME DERIVATIVE ,FT,
!                             DEPENDING ON THE CONCENTRATION, C, IN THE
!                             APPROPRIATE LINK CELL,
!                             FREUNDLICH POWER, GN,
!                             OLD AND NEW MOISTURE CONTENTS, THO AND TH,
!                             OLD AND NEW FRACTION OF EACH PARTICLE
!                             SIZE, HELD ,IN ARRAY FRNO AND FRN,
!                             REFERENCE KdFOR EACH PARTICLE SIZE, HELD
!                             IN ARRAY KDREF,
!                             OLD AND NEW POROSITIES, PO AND P,
!                             REFERENCE POROSITY, PREF,
!                             SCALED TIME STEP, DT,
!                             NUMBER OF SEDIMENT FRACTIONS, NSED,
!                             AND THE 'NON-LINEAR ADSORPTION' FLAG, ISNL
!
!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
DOUBLEPRECISION :: C, GN, THO, TH, PO, P, PREF, F, &
 FC, FT, DT
DOUBLEPRECISION FRNO ( * ), FRN ( * ), KDREF ( * )  
INTEGER :: NSED, jsed
DOUBLEPRECISION :: duma, dumo, dum, sumo, sum, dumjo, dumj, cdum, dumko, dumk

LOGICAL :: ISNL  
IF (ISZERO(C)) THEN  
   F = THO  
   FC = zero  
   FT = (TH - THO) / DT  
ELSE  
   DUMA = one / (one - PREF)  
   DUMO = (one - PO) * DUMA  
   DUM = (one - P) * DUMA  
   SUMO = zero  
   SUM = zero  
   DO 1 JSED = 1, NSED  
      SUMO = SUMO + FRNO (JSED) * KDREF (JSED)  
      SUM = SUM + FRN (JSED) * KDREF (JSED)  
    1    END DO  
   DUMJO = DUMO * SUMO  
   DUMJ = DUM * SUM  
   IF (.NOT.ISNL) THEN  
!                             IS LINEAR ADSORPTION
      F = THO + DUMJO  
      FC = zero  
      FT = (TH - THO + DUMJ - DUMJO) / DT  
   ELSE  
      CDUM = C** (GN - two)  
      DUMKO = DUMJO * CDUM  
      DUMK = DUMJ * CDUM  
      F = TH + DUMKO * C  
      FC = (GN - one) * DUMKO  
      FT = (TH - THO + (DUMK - DUMKO) * C) / DT  
   ENDIF  
ENDIF  
RETURN  
END SUBROUTINE FRET




!SSSSSS SUBROUTINE PLANT (JPLANT, BCAA, BCAA1, BCBB, BCBB1, TSE)  
SUBROUTINE PLANT (JPLANT, BCAA, BCAA1, BCBB, BCBB1, TSE)  
!
!                       SETS UP AND SOLVES THE PLANT DIFFERENCE
!                       DIFFERENCE EQUATIONS IN
!                       WRSRU/TR/9107/12 SECTION 4
!                       RETURNS THE UPDATED CONCENTRATIONS IN THE
!                       PLANT COMPARTMENTS: BCAA1 AND BCBB1
!
USE PLANT_CC 
INTEGER :: JPLANT
DOUBLEPRECISION :: BCAA, BCAA1, BCBB, BCBB1, TSE
DOUBLEPRECISION :: gdum, wcpaa, topdum, botdum, wcpbb

GDUM = one + GCPL * TSE  
IF (GTZERO(GMCPAA)) THEN  
   WCPAA = (RHOPL * QCPAA + GMCPAA * (GENAA (JPLANT) - GCPL * &
    BCAA) ) / (GMCPAA * GDUM)
   BCAA1 = BCAA + WCPAA * TSE  
ELSE  
!                             No plant mass in compartment A
   BCAA1 = zero  

ENDIF  
IF (GTZERO(GMCPBB)) THEN  
   TOPDUM = RHOPL * QCPBB + GMCPBB * (GENBB (JPLANT) - GCPL * &
    BCBB)
   BOTDUM = GMCPBB * GDUM  
   IF (GEZERO(GMCBBD)) THEN  
      TOPDUM = TOPDUM - BCBB * GMCBBD  
   ELSE  
      BOTDUM = BOTDUM - GMCBBD * TSE  
   ENDIF  
   IF (NOTZERO(BOTDUM)) THEN  
      WCPBB = TOPDUM / BOTDUM  
      BCBB1 = BCBB + WCPBB * TSE  
   ELSE  
      BCBB1 = zero  
   ENDIF  
ELSE  
   BCBB1 = zero  
!                 No mass in compartment B

ENDIF  
GENAA (JPLANT) = GCPL * BCAA  

GENBB (JPLANT) = GCPL * BCBB  
!                 Decay generation values to be used for next
!                 contaminant
RETURN  





END SUBROUTINE PLANT




!SSSSSS SUBROUTINE PLPREP  
SUBROUTINE PLPREP  
!                 Preparatory routine for plant uptake called once
!                 every timestep
USE PLANT_CC  
!                 Include parameter statements, water/contaminant
!                 interface COMMON blocks, and plant COMMON blocks
INTEGER :: jplty
DO 100 JPLTY = 1, NPLT  
   PFTWO (JPLTY) = CLAI (JPLTY)  
   IF (NOTZERO(PFTWO (JPLTY))) THEN  
      DELFOU (JPLTY) = one  
   ELSE  
      DELFOU (JPLTY) = FLEFT (JPLTY)  
   ENDIF  

  100 END DO  
!                 Set f2 delta4 for each plant type
RETURN  
END SUBROUTINE PLPREP
end MODULE CMmod