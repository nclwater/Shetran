MODULE ALmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the AL .F files
USE SGLOBAL
IMPLICIT NONE

CHARACTER(80) :: HEAD0_alread='( nothing read yet )', &
                 HEAD0_alredc='( nothing read yet )', &
                 HEAD0_alredi='( nothing read yet )', &
                 HEAD0_alred2='( nothing read yet )', &
                 HEAD0_alredl='( nothing read yet )', &
                 HEAD0_alredf='( nothing read yet )'
 

PRIVATE
PUBLIC :: ALREAD, ALALLF, ALCHKI, ALCHK, ALINIT, ALSPRD, ALTRAP, &
          ALINTP, ALREDL, ALREDF, ALALLI, ALRED2, ALREDC, ALREDI
CONTAINS



!SSSSSS SUBROUTINE ALALLF (FLAG, N2, MINCAT, IUNIT, OUNIT, LINE, NEL, NLF, &
SUBROUTINE ALALLF (FLAG, N2, MINCAT, IUNIT, OUNIT, LINE, NEL, NLF, &
 NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
 NCAT, AEL, IDUM, DUMMY)
!
!----------------------------------------------------------------------*
!
!  Set a floating-point array for all elements (FLAG=0) or all
!  column elements (FLAG=1), by reading from an input data file.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR74
!  Module:  AL        Program:  SHETRAN
! Modifications:
! RAH  19.09.94  Version 3.4.1 by AB/RAH. File creation date 27.5.94.
!----------------------------------------------------------------------*
!
!
! Input arguments
INTEGER :: FLAG, N2, MINCAT, IUNIT, OUNIT  
INTEGER :: NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE  
INTEGER :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2), ICMREF (NELEE, 4, &
 2:2)
LOGICAL :: BEXBK, LINKNS (NLF)  
CHARACTER (LEN=*) :: LINE  
!
! Output arguments
INTEGER :: NCAT  
DOUBLEPRECISION AEL (1 + NLF * (FLAG / N2) :NELEE- (NELEE-NEL) &
 * (1 / N2), N2)
!
! Workspace arguments
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM  
DOUBLEPRECISION DUMMY (NELEE)  
!
INTEGER :: I1, I2, ICAT, IDUM0, IEL, LN, N, X, XY0, Y  
LOGICAL :: BLINK  
CHARACTER      :: CDUM
CHARACTER(132) :: MSG
CHARACTER(8)   :: NEXT  
!
!
!----------------------------------------------------------------------*
!
!
! Preliminaries
! -------------
!
!     * Initialization
LN = LEN (LINE) + 1  
BLINK = NLF.GT.0.AND.FLAG.EQ.0  
!
!     * Find out how many categories ( if any )
CALL ALREAD (2, IUNIT, OUNIT, LINE, 1, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
NCAT = IDUM (1)  
!
!
! Act on the Value of NCAT
! ------------------------
!
IF (NCAT.LT.MINCAT) THEN  
!
!        Invalid Option
!        --------------
!
   GOTO 8001  
!
!
ELSEIF (NCAT.LT.0) THEN  
!
!        Special Case: Return to Caller
!        ------------------------------
!
   RETURN  
!
!
ELSEIF (NCAT.EQ.0) THEN  
!
!        No Categories
!        -------------
!
!        * Loop over output vectors
   DO 300 I2 = 1, N2  
!
!           * Get values for link elements
      IF (BLINK) THEN  
         NEXT = LINE//'a'  
         CALL ALREAD (3, IUNIT, OUNIT, NEXT (:LN), NLF, 1, IDUM0, &
          CDUM, IDUM, AEL (1, I2) )
      ENDIF  
!
!           * Get values for grid elements ...
      NEXT = LINE//'b'  
      CALL ALREAD (5, IUNIT, OUNIT, NEXT (:LN), NX, NY, IDUM0, &
       CDUM, IDUM, DUMMY)
!
!           * ... and load into element array
      DO 200 Y = 1, NY  
         XY0 = (Y - 1) * NX  
         DO 100 X = 1, NX  
            IEL = ICMXY (X, Y)  
            IF (IEL.GT.0) AEL (IEL, I2) = DUMMY (XY0 + X)  
  100          END DO  
  200       END DO  
!
  300    END DO  
!
!
ELSEIF (N2 * NCAT.LE.NELEE) THEN  
!
!        Use Category Codes
!        ------------------
!
!        * Get list of values for each category
   NEXT = LINE//'c'  
   CALL ALREAD (3, IUNIT, OUNIT, NEXT (:LN), N2, NCAT, IDUM0, &
    CDUM, IDUM, DUMMY)
!
   IF (NCAT.EQ.1) THEN  
!
!           * Uniform value: Set all elements or just columns
      N = NEL - FLAG * NLF  
      I1 = 1 + NEL - N  
      DO 400 I2 = 1, N2  
         CALL ALINIT (DUMMY (I2), N, AEL (I1, I2) )  
  400       END DO  
!
   ELSE  
!
!           * Note: One code applies to all output vectors
!
!           * Get codes & set values for link elements
      IF (BLINK) THEN  
         NEXT = LINE//'d'  
!              * Note: DUMMY should not be overwritten here
         CALL ALREAD (2, IUNIT, OUNIT, NEXT (:LN), NLF, 1, IDUM0, &
          CDUM, IDUM, DUMMY)
         DO 450 IEL = 1, NLF  
            ICAT = IDUM (IEL)  
            IF (ICAT.LT.1.OR.ICAT.GT.NCAT) GOTO 8009  
            DO 440 I2 = 1, N2  
               AEL (IEL, I2) = DUMMY (I2 + (ICAT - 1) * N2)  
  440             END DO  
  450          END DO  
      ENDIF  
!
!           * Get codes & set values for grid elements
      NEXT = LINE//'e'  
      CALL ALREAD (4, IUNIT, OUNIT, NEXT (:LN), NX, NY, NCAT, &
       CDUM, IDUM, DUMMY)
      DO 700 Y = 1, NY  
         XY0 = (Y - 1) * NX  
         DO 600 X = 1, NX  
            IEL = ICMXY (X, Y)  
            IF (IEL.GT.0) THEN  
               ICAT = IDUM (XY0 + X)  
               IF (ICAT.LT.1.OR.ICAT.GT.NCAT) GOTO 8009  
               DO 500 I2 = 1, N2  
                  AEL (IEL, I2) = DUMMY (I2 + (ICAT - 1) * N2)  
  500                END DO  
            ENDIF  
  600          END DO  
  700       END DO  
!
   ENDIF  
!
!
ELSE  
!
!        Insufficient Workspace
!        ----------------------
!
   WRITE (MSG, 9008) NCAT, LINE, N2 * NCAT  
   CALL ERROR (FFFATAL, 8, OUNIT, 0, 0, MSG)  
!
!
ENDIF  
!
!
! Epilogue
! --------
!
!     * All grid elements are defined - now set bank element values
IF (NLF.GT.0.AND.BEXBK.AND.NCAT.NE.1) THEN  
   DO 800 I2 = 1, N2  
      CALL ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, &
       AEL (NLF + 1, I2) )
  800    END DO  
ENDIF  
!
RETURN  
!
!
!     * Invalid option
 8001 WRITE (MSG, 9001) NCAT, LINE  
CALL ERROR (FFFATAL, 1, OUNIT, 0, 0, MSG)  
!
!     * Invalid category number
 8009 WRITE (MSG, 9009) ICAT, NEXT (:LN), NCAT  
CALL ERROR (FFFATAL, 9, OUNIT, IEL, 0, MSG)  
!
!
 9001 FORMAT ( 'Ivalid option NCAT =', I4, ' at title line ', A )  
!
 9008 FORMAT ( 'Insufficient workspace for', I4, ' categories in ', A, &
&         ' : increase NELEE to at least', I6 )
!
 9009 FORMAT ( 'Invalid category value', I4, ' while reading ', A, &
&         ' : should be in range [1,', I4, ']' )
!
!
END SUBROUTINE ALALLF



!SSSSSS SUBROUTINE ALALLI (NCAT, IUNIT, OUNIT, LINE, NEL, NLF, NX, NY, &
SUBROUTINE ALALLI (NCAT, IUNIT, OUNIT, LINE, NEL, NLF, NX, NY, &
 NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, CATTYP, &
 IDUM)
!--------------------------------------------------------------------*
!
!  Reads the catagory type for each grid, bank elements take the
!  same category type as the adjacent grid element
!  There must be nine or fewer category types
!
!--------------------------------------------------------------------*
! Version: 4.2                 Notes:
! Module: AL                 Program: SHETRAN
! Modifications
!--------------------------------------------------------------------*
!
! INPUT ARGUMENTS
INTEGER :: NCAT, IUNIT, OUNIT, NEL, NLF, NX, NY, NELEE, NLFEE, &
 NXEE
INTEGER :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2), ICMREF (NELEE, 4, &
 2:2)
LOGICAL :: BEXBK, LINKNS (NLFEE)  
CHARACTER (LEN=*) :: LINE  
!
! OUPUT ARGUMENTS
INTEGER :: CATTYP (NLF + 1:NEL)  
!
! WORKSPACE ARGUMENTS
INTEGER, DIMENSION(:), INTENT(IN)  :: IDUM  
!
! LOCALS ETC.
!
!      * integers from ALBANK
INTEGER :: BANK1, BANK2, FACE1, FACE2, GRID1, GRID2, ISNS, &
 LINK
!      * integers from ALALLF
INTEGER :: ICAT, IEL, X, XY0, Y  
!
!-------------------------------------------------------------------*
!
!      * Read the catagory type for each element
CALL ALREDI (NCAT, IUNIT, OUNIT, LINE, NX, NY, IDUM)  
DO 700 Y = 1, NY  
   XY0 = (Y - 1) * NX  
   DO 600 X = 1, NX  
      IEL = ICMXY (X, Y)  
      IF (IEL.GT.0) THEN  
         ICAT = IDUM (XY0 + X)  
         IF (ICAT.LT.1.OR.ICAT.GT.NCAT) THEN  
CALL ERROR (FFFATAL, 3090, OUNIT, 0, 0, 'Error in NCELEM in :MN43 in MN data file')
         ENDIF  
         CATTYP (IEL) = ICAT  
      ENDIF  
  600    END DO  
  700 END DO  
!
!
!      * All grid elements are defined - now set bank element values
!      * Copied from ALBANK except an integer array CATTYP is used
!      * instead of the floating point array.
IF (NLF.GT.0.AND.BEXBK) THEN  
!          * Loop over channel links
   DO 100 LINK = 1, NLF  
!
!             * Determine orientation of link
      ISNS = 0  
      IF (LINKNS (LINK) ) ISNS = 1  
!
!            * For each side of the channel: Determine adjacent bank
!            *  element
!            *  number, the number of it's face that lies opposite to
!            *  the
!            *  channel, and the number of the grid element adjacent to
!            *  that face.
      BANK1 = ICMBK (LINK, 1)  
      BANK2 = ICMBK (LINK, 2)  
      FACE1 = 2 - ISNS  
      FACE2 = 4 - ISNS  
      GRID1 = ICMREF (BANK1, FACE1, 2)  
      GRID2 = ICMREF (BANK2, FACE2, 2)  
!
!            * If the grid ( as defined above ) does not exist, then use
!            * the grid corresponding to the opposite side of the channe
!            *  ( precondition on ICMREF disallows GRID1 & GRID2 both
!            *  zero )
      IF (GRID1.EQ.0) GRID1 = GRID2  
      IF (GRID2.EQ.0) GRID2 = GRID1  
!
!           * For each side of the channel, copy the contents of the
!           * array
!           *  from the grid to its corresponding bank
      CATTYP (BANK1) = CATTYP (GRID1)  
      CATTYP (BANK2) = CATTYP (GRID2)  
!
!        * Next channel link
  100    END DO  
ENDIF  
!
END SUBROUTINE ALALLI



!SSSSSS SUBROUTINE ALBANK
SUBROUTINE ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, &
 A)
!
!----------------------------------------------------------------------*
!
! Assign values to the bank elements of an array by copying values
!  associated with neighboring grid elements in the same array
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR51
!  Module:  AL        Program:  SHETRAN
! Modifications:
!  AB   23.5.94  Version 3.4.1 by AB/RAH. File creation date 22.4.94.
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: NEL, NLF, NLFEE, NELEE, ICMBK (NLFEE, 2), ICMREF ( &
 NELEE, 4, 2:2)
LOGICAL :: LINKNS (NLF)  
!
! Input/output arguments
DOUBLEPRECISION A (NLF + 1:NEL)  
!
! Locals, etc
INTEGER :: BANK1, BANK2, FACE1, FACE2, GRID1, GRID2, ISNS, LINK  
!
!
!----------------------------------------------------------------------*
!
!
!     * Loop over channel links
DO 100 LINK = 1, NLF  
!
!        * Determine orientation of link
   ISNS = 0  
   IF (LINKNS (LINK) ) ISNS = 1  
!
!        * For each side of the channel: Determine adjacent bank element
!        *  number, the number of it's face that lies opposite to the
!        *  channel, and the number of the grid element adjacent to
!        *  that face.
   BANK1 = ICMBK (LINK, 1)  
   BANK2 = ICMBK (LINK, 2)  
   FACE1 = 2 - ISNS  
   FACE2 = 4 - ISNS  
   GRID1 = ICMREF (BANK1, FACE1, 2)  
   GRID2 = ICMREF (BANK2, FACE2, 2)  
!
!        * If the grid ( as defined above ) does not exist, then use the
!        *  grid corresponding to the opposite side of the channel
!        *  ( precondition on ICMREF disallows GRID1 & GRID2 both zero )
   IF (GRID1.EQ.0) GRID1 = GRID2  
   IF (GRID2.EQ.0) GRID2 = GRID1  
!
!        * For each side of the channel, copy the contents of the array
!        *  from the grid to its corresponding bank
   A (BANK1) = A (GRID1)  
   A (BANK2) = A (GRID2)  
!
!     * Next channel link
  100 END DO  
!
END SUBROUTINE ALBANK



!SSSSSS SUBROUTINE ALCHK (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
SUBROUTINE ALCHK (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
 OP, OBJ, TOL, SUBJ, COUNT, NOTOK)
!
!----------------------------------------------------------------------*
!
!  Check that a given relation holds between subject and object
!   arrays, and take corrective action and/or raise an error in the
!   event of a failure.
!
!----------------------------------------------------------------------*
!  CAUTION!  Source code for ALCHKI is generated from ALCHK using make:
!  ''''''''  check the makefile before modifying this subroutine.
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR62
!  Module:  AL        Program:  SHETRAN
! Modifications:
!  RAH  17.08.94  Version 3.4.1 by AB/RAH. File created 22.07.94.
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3  
CHARACTER(*), INTENT(IN) :: SNAME, OP  
DOUBLEPRECISION OBJ (N0: * ), TOL  
!
! Input/output arguments
DOUBLEPRECISION SUBJ (N0:N1)  
INTEGER :: COUNT
!
! Workspace arguments
LOGICAL :: NOTOK (N0:N1)  
!
! Locals, etc
INTEGER :: COUNT0, COUNT1, I, INCOBJ, IOBJ, IX (3), NDIM  
INTEGER :: P, POS1, POS2, SGN, SLEN
DOUBLEPRECISION SB, OB, rrr
LOGICAL :: BRESET  
CHARACTER(9)   :: CACT
CHARACTER(132) :: MSG
CHARACTER      :: OP1, OP2  
!
!
!----------------------------------------------------------------------*
!
!
! How many subscripts are there? (ignore any after the 3rd)
! ------------------------------
!
SLEN = LEN (SNAME)  
POS1 = 0  
POS2 = INDEX (SNAME, '(')  
DO 100 NDIM = 0, 2  
   IF (POS2.GT.POS1.AND.POS2.LT.SLEN) THEN  
      IF (NDIM.EQ.1) IX (2) = IX2  
      IF (NDIM.EQ.2) IX (3) = IX3  
      POS1 = POS2  
      POS2 = POS1 + INDEX (SNAME (POS1 + 1:) , ',')  
   ELSE  
      GOTO 101  
   ENDIF  
  100 END DO  
!     * If this point is traversed NDIM=3; if skipped NDIM<3
  101 CONTINUE  
!
!
! What action is required?
! ------------------------
!
BRESET = ACTION.LT.0  
OP1 = OP (1:1)  
OP2 = OP (2:2)  
SGN = + 1  
IF (OP1.EQ.'G') SGN = - 1  
INCOBJ = 0  
IF (OP (LEN (OP) :) .EQ.'a') INCOBJ = 1  
!
!
! Store test results in logical workspace array
! ---------------------------------------------
!
!     * Note:  i Code is replicated to enable vectorization of loops.
!             ii "Requirements" are approximate if TOL>0.
!
IOBJ = N0  
!
IF (OP2.EQ.'T') THEN  
!        * require SUBJ.LT.OBJ or SUBJ.GT.OBJ (depending on SGN)
   DO 210 I = N0, N1  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      NOTOK (I) = SGN * (SB - OB) .GE.TOL * MAX (ABS (SB), &
       ABS (OB) )
      IOBJ = IOBJ + INCOBJ  
  210    END DO  
!
ELSEIF (OP2.EQ.'E') THEN  
!        * require SUBJ.LE.OBJ or SUBJ.GE.OBJ (depending on SGN)
   DO 220 I = N0, N1  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      NOTOK (I) = SGN * (SB - OB) .GT.TOL * MAX (ABS (SB), &
       ABS (OB) )
      IOBJ = IOBJ + INCOBJ  
  220    END DO  
!
ELSE  
!        * require SUBJ.EQ.OBJ
   DO 230 I = N0, N1  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      NOTOK (I) = ABS (SB - OB) .GT.TOL * MAX (ABS (SB), ABS (OB) &
       )
      IOBJ = IOBJ + INCOBJ  
  230    END DO  
!
ENDIF  
!
!
! Count the non-conformances and fix them if required
! ---------------------------------------------------
!
!     * Note: Non-vectorizing loop: keep it short
!
COUNT0 = COUNT  
IOBJ = N0 + INCOBJ * (N1 - N0)  
!     * step backwards so that IX(1), SB & OB refer to 1st non-conformer
DO 300 I = N1, N0, - 1  
   IF (NOTOK (I) ) THEN  
      COUNT = COUNT + 1  
      IX (1) = I  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      IF (BRESET) SUBJ (I) = OB  
   ENDIF  
   IOBJ = IOBJ - INCOBJ  
  300 END DO  
!
!
! Report findings
! ---------------
!
COUNT1 = COUNT - COUNT0  
IF (COUNT1.GT.0) THEN  
!
   CACT = 'Checking'  
   IF (BRESET) CACT = 'Resetting'  
!
!        * print the first occurrence ...
 rrr=sb  !AD
   WRITE (MSG, 9000) CACT, SNAME, OP (:2), OB, rrr, (IX (P), &
    P = 1, NDIM)
   CALL ERROR (ABS (ACTION), ERRNUM, OUNIT, 0, 0, MSG)  
!
   IF (COUNT1.GT.1) THEN  
!           * ... and allude to any others
      WRITE (MSG, 9010) COUNT1 - 1  
      CALL ERROR (0, 12, OUNIT, 0, 0, MSG)  
   ENDIF  
!
ENDIF  
!
 9000 FORMAT( A,1X,A,': expected .',A,'.',1P,G15.7,' but found',G15.7: &
&        ' at position', I5, 2( : ',', I4 )                      )
 9010 FORMAT( '... and similarly at', I4, &
&        ' other positions in the same vector' )
!
END SUBROUTINE ALCHK



!SSSSSS SUBROUTINE ALCHKI (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
SUBROUTINE ALCHKI (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
 OP, OBJ, SUBJ, COUNT, NOTOK)
!
!----------------------------------------------------------------------*
!
!  Check that a given relation holds between subject and object
!   arrays, and take corrective action and/or raise an error in the
!   event of a failure.
!
!----------------------------------------------------------------------*
!  CAUTION!  Source code for ALCHKI is generated from ALCHK using make:
!  ''''''''  check the makefile before modifying this subroutine.
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR62
!  Module:  AL        Program:  SHETRAN
! Modifications:
!  RAH  17.08.94  Version 3.4.1 by AB/RAH. File created 22.07.94.
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3  
CHARACTER (LEN=*) :: SNAME, OP  
INTEGER :: OBJ (N0: * )  
!
! Input/output arguments
INTEGER :: SUBJ (N0:N1)  
INTEGER :: COUNT  
!
! Workspace arguments
LOGICAL :: NOTOK (N0:N1)  
!
! Locals, etc
INTEGER :: COUNT0, COUNT1, I, INCOBJ, IOBJ, IX (3), NDIM  
INTEGER :: P, POS1, POS2, SGN, SLEN  
INTEGER :: SB, OB, iii  
LOGICAL :: BRESET  
CHARACTER(9)   :: CACT
CHARACTER(132) :: MSG
CHARACTER      :: OP1, OP2  
!
!
!----------------------------------------------------------------------*
!
!
! How many subscripts are there? (ignore any after the 3rd)
! ------------------------------
!
SLEN = LEN (SNAME)  
POS1 = 0  
POS2 = INDEX (SNAME, '(')  
DO 100 NDIM = 0, 2  
   IF (POS2.GT.POS1.AND.POS2.LT.SLEN) THEN  
      IF (NDIM.EQ.1) IX (2) = IX2  
      IF (NDIM.EQ.2) IX (3) = IX3  
      POS1 = POS2  
      POS2 = POS1 + INDEX (SNAME (POS1 + 1:) , ',')  
   ELSE  
      GOTO 101  
   ENDIF  
  100 END DO  
!     * If this point is traversed NDIM=3; if skipped NDIM<3
  101 CONTINUE  
!
!
! What action is required?
! ------------------------
!
BRESET = ACTION.LT.0  
OP1 = OP (1:1)  
OP2 = OP (2:2)  
SGN = + 1  
IF (OP1.EQ.'G') SGN = - 1  
INCOBJ = 0  
IF (OP (LEN (OP) :) .EQ.'a') INCOBJ = 1  
!
!
! Store test results in logical workspace array
! ---------------------------------------------
!
!     * Note:  i Code is replicated to enable vectorization of loops.
!
IOBJ = N0  
!
IF (OP2.EQ.'T') THEN  
!        * require SUBJ.LT.OBJ or SUBJ.GT.OBJ (depending on SGN)
   DO 210 I = N0, N1  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      NOTOK (I) = SGN * (SB - OB) .GE.0  
      IOBJ = IOBJ + INCOBJ  
  210    END DO  
!
ELSEIF (OP2.EQ.'E') THEN  
!        * require SUBJ.LE.OBJ or SUBJ.GE.OBJ (depending on SGN)
   DO 220 I = N0, N1  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      NOTOK (I) = SGN * (SB - OB) .GT.0  
      IOBJ = IOBJ + INCOBJ  
  220    END DO  
!
ELSE  
!        * require SUBJ.EQ.OBJ
   DO 230 I = N0, N1  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      NOTOK (I) = ABS (SB - OB) .GT.0  
      IOBJ = IOBJ + INCOBJ  
  230    END DO  
!
ENDIF  
!
!
! Count the non-conformances and fix them if required
! ---------------------------------------------------
!
!     * Note: Non-vectorizing loop: keep it short
!
COUNT0 = COUNT  
IOBJ = N0 + INCOBJ * (N1 - N0)  
!     * step backwards so that IX(1), SB & OB refer to 1st non-conformer
DO 300 I = N1, N0, - 1  
   IF (NOTOK (I) ) THEN  
      COUNT = COUNT + 1  
      IX (1) = I  
      SB = SUBJ (I)  
      OB = OBJ (IOBJ)  
      IF (BRESET) SUBJ (I) = OB  
   ENDIF  
   IOBJ = IOBJ - INCOBJ  
  300 END DO  
!
!
! Report findings
! ---------------
!
COUNT1 = COUNT - COUNT0  
IF (COUNT1.GT.0) THEN  
!
   CACT = 'Checking'  
   IF (BRESET) CACT = 'Resetting'  
!
!        * print the first occurrence ...
iii=sb !AD
   WRITE (MSG, 9000) CACT, SNAME, OP (:2), OB, iii, (IX (P), &
    P = 1, NDIM)
   CALL ERROR (ABS (ACTION), ERRNUM, OUNIT, 0, 0, MSG)  
!
   IF (COUNT1.GT.1) THEN  
!           * ... and allude to any others
      WRITE (MSG, 9010) COUNT1 - 1  
      CALL ERROR (0, 12, OUNIT, 0, 0, MSG)  
   ENDIF  
!
ENDIF  
!
 9000 FORMAT( A,1X,A,': expected .',A,'.',I12,' but found',I12: &
&        ' at position', I5, 2( : ',', I4 )                      )
 9010 FORMAT( '... and similarly at', I4, &
&        ' other positions in the same vector' )
!
END SUBROUTINE ALCHKI



!SSSSSS SUBROUTINE ALINIT (ALPHA, N, X)  
SUBROUTINE ALINIT (ALPHA, N, X)  
!
!----------------------------------------------------------------------*
!
! Initialize an array with a given value
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR67
!  Module:  AL        Program:  SHETRAN
! Modifications:
!  AB   23.5.94  Version 3.4.1 by AB/RAH. File creation date  8.12.93.
!----------------------------------------------------------------------*
!
! Input arguments
DOUBLEPRECISION ALPHA  
INTEGER :: N  
!
! Output arguments
DOUBLEPRECISION X (N)  
!
! Locals, etc
INTEGER :: I  
!
!
!----------------------------------------------------------------------*
!
!
DO 100 I = 1, N  
   X (I) = ALPHA  
  100 END DO  
!
!
END SUBROUTINE ALINIT



!SSSSSS SUBROUTINE ALINTP (LLEE, NCETOP, NEL, NELEE, NLF, NCAT, NCATEE, &
SUBROUTINE ALINTP (LLEE, NCETOP, NEL, NELEE, NLF, NCAT, NCATEE, &
 NTABEE, NCATTY, NCOLMB, NTAB, CTAB, DTAB, DELTAZ, ZVSNOD, CCELL)
!
!--------------------------------------------------------------------*
!
! For each category type, a table of values is known. This contains
! the depth (DTAB) and the concentration (CTAB) at each depth.
! The concentrationin each cell (CCELL) is calculated by linear
! interpolation
! NCAT is the number of category types. NCATEE is the maximum number
! of category types. NTABEE is the maximum number of pairs
! of data in each table.
!
! The depths in the table must start at zero and increase.
!
!--------------------------------------------------------------------*
! Version:                   Notes:
! Module: AL                 Program: SHETRAN
! Modifications
!--------------------------------------------------------------------*
!
! INPUT ARGUMENTS
INTEGER :: LLEE, NCETOP, NEL, NELEE, NLF, NCAT, NCATEE, NTABEE  
INTEGER :: NCATTY (NLF + 1:NEL), NCOLMB (NLF + 1:NEL)  
INTEGER :: NTAB (NCAT)  
DOUBLEPRECISION CTAB (NCATEE, NTABEE), DTAB (NCATEE, NTABEE)  
DOUBLEPRECISION DELTAZ (LLEE, NELEE), ZVSNOD (LLEE, NELEE)  
!
! OUTPUT ARGUMENTS
DOUBLEPRECISION CCELL (NELEE, LLEE)  
!
! LOCALS ETC.
INTEGER :: NCL, NELM, NCATG, NINTB, NTABLE, NTHRTB  
DOUBLEPRECISION DEPTH  
!
!--------------------------------------------------------------------*
!

DO 130 NELM = NLF + 1, NEL  
!        Category number for the element

   NCATG = NCATTY (NELM)  
!        Number of values in the table for this category number

   NINTB = NTAB (NCATG)  
!        The first depth in the table must be zero and the top
!        cell is set to take the concentration at this depth
   CCELL (NELM, NCETOP) = CTAB (NCATG, 1)  
   DEPTH = DELTAZ (NCETOP, NELM) / two  
   NTHRTB = 2  
   DO 140 NCL = NCETOP - 1, NCOLMB (NELM), - 1  

      DEPTH = DEPTH + (ZVSNOD (NCL + 1, NELM) - ZVSNOD (NCL, NELM) &
       )
!           The depth of the cell is greater than the lowest depth in
!           the table and the cell takes the value of the concentration
!           at the lowest specified depth
      IF (DEPTH.GE.DTAB (NCATG, NINTB) ) THEN  
         CCELL (NELM, NCL) = CTAB (NCATG, NINTB)  
         GOTO 140  
!             ^^^^^^^^
      ENDIF  
      DO 150 NTABLE = NTHRTB, NINTB  
         IF (DEPTH.LE.DTAB (NCATG, NTABLE) ) GOTO 300  
!                                            ^^^^^^^^^
         NTHRTB = NTHRTB + 1  
  150       END DO  
  300       CCELL (NELM, NCL) = CTAB (NCATG, NTABLE-1) + (CTAB (NCATG, &
       NTABLE) - CTAB (NCATG, NTABLE-1) ) * ( (DEPTH - DTAB (NCATG, &
       NTABLE-1) ) / (DTAB (NCATG, NTABLE) - DTAB (NCATG, NTABLE-1) &
       ) )
  140    END DO  
  130 END DO  
!
END SUBROUTINE ALINTP





!SSSSSS SUBROUTINE ALREAD (FLAG, IUNIT, OUNIT, LINE, N1, N2, NCAT, CDATA, &
SUBROUTINE ALREAD (FLAG, IUNIT, OUNIT, LINE, N1, N2, NCAT, CDATA, &
 IDATA, RDATA)
!----------------------------------------------------------------------*
!
!  Utility routine to handle an input data file
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/AL/ALREAD/4.1
! Modifications:
! RAH  16.09.94  Version 3.4.1 by AB/RAH.  File created 10.12.93.
!  GP  12.09.94  4.0  Add VSS options (FLAG=) 6 & 7.
! RAH  970804  4.1  Add END specifiers to READs in options 6 & 7.
!                   Renumber error 13 as 16 (was unauthorized).
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FLAG, IUNIT, OUNIT, N1, N2, NCAT  

CHARACTER (LEN=*) :: LINE  
! Output arguments
CHARACTER (LEN=*) :: CDATA  
INTEGER :: IDATA (N1, N2)  

DOUBLEPRECISION RDATA (N1, N2)  
! Locals, etc
CHARACTER (LEN=80) :: HEAD, MSG * 132, FILNAM * 48, FORM * 17
INTEGER :: IX, IY, KY, IDUM1, IDUM2, ICOUNT, I  
LOGICAL :: BOPEN, BNAMED  
!----------------------------------------------------------------------*
! Preliminaries
! -------------

IF (FLAG.GT.0) THEN  
!        * Check data header against what the caller expects to find
   READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
   IF (INDEX (HEAD, LINE) .EQ.0) THEN  
      WRITE (MSG, 9002) LINE, HEAD  
      CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  

   ENDIF  

ELSE  
!        * Get file status and name
   INQUIRE (IUNIT, OPENED = BOPEN, NAMED = BNAMED, NAME = FILNAM)  

   IF (.NOT.BNAMED) FILNAM = '(no name)'  


ENDIF  
! Take Specified Action
! ---------------------


IF (FLAG.EQ.0) THEN  
!        Check that input file is open
!        -----------------------------
   IF (.NOT.BOPEN) GOTO 8000  
!        * Write (and store) an informative message
   WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM  

   WRITE (OUNIT, 9001) HEAD  


ELSEIF (FLAG.EQ. - 1) THEN  
!        Close input file
!        ----------------
   CLOSE (IUNIT)  
!        * Write (and store) an informative message
   WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM  

   WRITE (OUNIT, 9001) HEAD  


ELSEIF (FLAG.EQ.1) THEN  
!        Read a character string
!        -----------------------

   READ (IUNIT, '(A)', ERR = 8100, END = 8100) CDATA  


ELSEIF (FLAG.EQ.2) THEN  
!        Read an integer array
!        ---------------------

   READ (IUNIT, *, ERR = 8200, END = 8200) IDATA  


ELSEIF (FLAG.EQ.3) THEN  
!        Read a floating-point array
!        ---------------------------

   READ (IUNIT, *, ERR = 8300, END = 8300) RDATA  


ELSEIF (FLAG.EQ.4) THEN  
!        Read an integer grid array
!        --------------------------
!        * Set format string to read single digit integers if possible

   IF (NCAT.LT.10) WRITE (FORM, 9410) N1  
!        * All grid rows: North to South
   DO 400 IY = N2, 1, - 1  
      IF (NCAT.LT.10) THEN  
         READ (IUNIT, FORM, ERR = 8420, END = 8420) KY, (IDATA ( &
          IX, IY), IX = 1, N1)
      ELSE  
         READ (IUNIT, *, ERR = 8420, END = 8420) KY, (IDATA (IX, &
          IY), IX = 1, N1)
      ENDIF  
      IF (KY.NE.IY) GOTO 8420  

  400    END DO  


ELSEIF (FLAG.EQ.5) THEN  
!        Read a floating point grid array
!        --------------------------------
!        * All grid rows: North to South
   DO 500 IY = N2, 1, - 1  
      READ (IUNIT, *, ERR = 8430, END = 8430) KY, (RDATA (IX, IY), &
       IX = 1, N1)
      IF (KY.NE.IY) GOTO 8430  

  500    END DO  
ELSEIF (FLAG.EQ.6) THEN  
!
!        Read data in VSS format for each element
!        ----------------------------------------
!
   DO 600 ICOUNT = 1, NCAT  
      READ (IUNIT, *, ERR = 8600, END = 8600) IDUM1, IDUM2  
      READ (IUNIT, *, ERR = 8600, END = 8600) (IDATA (IDUM1, I), &
       I = 1, IDUM2)
      READ (IUNIT, *, ERR = 8600, END = 8600) (RDATA (IDUM1, I), &
       I = 1, IDUM2)

  600    END DO  


ELSEIF (FLAG.EQ.7) THEN  
!        Read soil physical property data for VSS
!        ----------------------------------------
   DO 700 ICOUNT = 1, NCAT  
      READ (IUNIT, *, ERR = 8700, END = 8700) (IDATA (ICOUNT, I), &
       I = 1, 3)
      IF (IDATA (ICOUNT, 1) .NE.ICOUNT) GOTO 8700  
      READ (IUNIT, *, ERR = 8700, END = 8700) (RDATA (ICOUNT, I), &
       I = 1, 8)

  700    END DO  


ENDIF  
! Epilogue
! --------
!     * Store current title as old title
HEAD0_alread = HEAD  


RETURN  
! Error branches
! --------------
!     * File not open
 8000 WRITE (MSG, 9000) LINE, 'not open', IUNIT  

CALL ERROR (FFFATAL, 4, OUNIT, 0, 0, MSG)  
!     * Title line read error
 8010 WRITE (MSG, 9801) LINE, HEAD0_alread  

CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
!     * Char data error
 8100 WRITE (MSG, 9810) 'character', HEAD  

CALL ERROR (FFFATAL, 5, OUNIT, 0, 0, MSG)  
!     * Integer data error
 8200 WRITE (MSG, 9810) 'integer', HEAD  

CALL ERROR (FFFATAL, 6, OUNIT, 0, 0, MSG)  
!     * Real data error
 8300 WRITE (MSG, 9810) 'floating-point', HEAD  

CALL ERROR (FFFATAL, 7, OUNIT, 0, 0, MSG)  
!     * Integer grid error
 8420 WRITE (MSG, 9842) 'integer', IY, HEAD  

CALL ERROR (FFFATAL, 10, OUNIT, 0, 0, MSG)  
!     * Real grid error
 8430 WRITE (MSG, 9842) 'floating-point', IY, HEAD  

CALL ERROR (FFFATAL, 11, OUNIT, 0, 0, MSG)  
!     * VSS format data errors
 8600 WRITE (MSG, 9600) IDUM1, HEAD  

CALL ERROR (FFFATAL, 16, OUNIT, 0, 0, MSG)  
!     * VSS soil physical property data errors
 8700 WRITE (MSG, 9700) ICOUNT, HEAD  



CALL ERROR (FFFATAL, 14, OUNIT, 0, 0, MSG)  
! Format statements
! -----------------
! Note: Take care not to exceed internal file length
 9000 FORMAT ( A, ' data file ', A, ': unit', I3: '; ', A )  
 9001 FORMAT ( 1X, A/ )  
 9002 FORMAT ( 'Title line mismatch: expected "', A, &
&         '" but found "',                   A, '"' )
 9410 FORMAT ( '(I7,1X,', I4, 'I1)' )  
 9600 FORMAT ( 'Reading VSS data for item no. ',I4, &
&         ' under title: ', A )
 9700 FORMAT ( 'Reading soils data for soil no. ',I4, &
&         ' under title: ', A )
 9801 FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
 9810 FORMAT ( 'Reading ', A, ' data under heading: ', A )  

 9842 FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  
END SUBROUTINE ALREAD



!SSSSSS SUBROUTINE ALRED2 (FLAG, IUNIT, OUNIT, LINE)  
SUBROUTINE ALRED2 (FLAG, IUNIT, OUNIT, LINE)  
!
!----------------------------------------------------------------------*
!
!  Utility routine to handle an input data file
!
!  !!!  NB  This subroutine contains ENTRY statements  !!!
!
!----------------------------------------------------------------------*
!       Version: 3.4.2  Context: SHETRAN/AL
! Modifications:
! RAH  16.09.94  3.4.1  Written by AB/RAH.  File created 10.12.93.
! RAH  22.03.95  3.4.2  New header.
!                       Remove arguments N1,...,RDATA & create ENTRY
!                       points ALRDI, etc, including new option ALRDL
!                       (note: arg NCAT removed; CDATA now an array;
!                       RDATA renamed FDATA).
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FLAG, IUNIT, OUNIT  
CHARACTER (LEN=*) :: LINE  
CHARACTER (80) :: HEAD
CHARACTER(48)  :: FILNAM
CHARACTER(132) :: MSG  
LOGICAL :: BOPEN, BNAMED  
!
!
! File Management
! ---------------
!
!     * Get file status and name
INQUIRE (IUNIT, OPENED = BOPEN, NAMED = BNAMED, NAME = FILNAM)  
IF (.NOT.BNAMED) FILNAM = '(no name)'  
!
IF (FLAG.EQ.0) THEN  
!
!        * Check that input file is open
   IF (.NOT.BOPEN) GOTO 8000  
   WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM  
!
ELSE  
!
!        * Close input file
   CLOSE (IUNIT)  
   WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM  
!
ENDIF  
!
!     * HEAD now contains an informative message
WRITE (OUNIT, 9001) HEAD  
!
!
!     * Store current title as old title
HEAD0_alred2 = HEAD  
!
RETURN  
!
!
! Error branches
! --------------
!
!     * File not open
 8000 WRITE (MSG, 9000) LINE, 'not open', IUNIT  
CALL ERROR (FFFATAL, 4, OUNIT, 0, 0, MSG)  
!
 9000 FORMAT ( A, ' data file ', A, ': unit', I3: '; ', A )  
!
 9001 FORMAT ( 1X, A/ )  
!
!
END SUBROUTINE ALRED2



!SSSSSS SUBROUTINE ALREDC (FLAG, IUNIT, OUNIT, LINE, N1, N2, CDATA)  
SUBROUTINE ALREDC (FLAG, IUNIT, OUNIT, LINE, N1, N2, CDATA)  
!
!----------------------------------------------------------------------*
!
!  Utility routine to handle an input data file
!
!----------------------------------------------------------------------*
!       Version: 3.4.2  Context: SHETRAN/AL
! Modifications:
! RAH  16.09.94  3.4.1  Written by AB/RAH.  File created 10.12.93.
! RAH  22.03.95  3.4.2  New header.
!                       Remove arguments N1,...,RDATA & create ENTRY
!                       points ALRDI, etc, including new option ALRDL
!                       (note: arg NCAT removed; CDATA now an array;
!                       RDATA renamed FDATA).
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FLAG, IUNIT, OUNIT  
INTEGER :: N1, N2  
CHARACTER (LEN=*) :: LINE  
!
! Output arguments
CHARACTER (LEN=*) :: CDATA (N1, N2)  
CHARACTER(80)  :: HEAD
CHARACTER(132)  :: MSG  
!
READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
IF (INDEX (HEAD, LINE) .EQ.0) THEN  
   WRITE (MSG, 9002) LINE, HEAD  
   CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  

ENDIF  
!     Read character data
!     -------------------
!

READ (IUNIT, '(A)', ERR = 8100, END = 8100) CDATA  

RETURN  
!     * Title line read error
 8010 WRITE (MSG, 9801) LINE, HEAD0_alredc  

CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
!     * Char data error
 8100 WRITE (MSG, 9810) 'character', HEAD  

CALL ERROR (FFFATAL, 5, OUNIT, 0, 0, MSG)  
!
 9002 FORMAT ( 'Title line mismatch: expected "', A, &
&         '" but found "',                   A, '"' )
 9801 FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
!
 9810 FORMAT ( 'Reading ', A, ' data under heading: ', A )  
!

 9842 FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  
END SUBROUTINE ALREDC



!SSSSSS SUBROUTINE ALREDF (FLAG, IUNIT, OUNIT, LINE, N1, N2, FDATA)  
SUBROUTINE ALREDF (FLAG, IUNIT, OUNIT, LINE, N1, N2, FDATA)  
!
!----------------------------------------------------------------------*
!
!  Utility routine to handle an input data file
!
!----------------------------------------------------------------------*
!       Version: 3.4.2  Context: SHETRAN/AL
! Modifications:
! RAH  16.09.94  3.4.1  Written by AB/RAH.  File created 10.12.93.
! RAH  22.03.95  3.4.2  New header.
!                       Remove arguments N1,...,RDATA & create ENTRY
!                       points ALRDI, etc, including new option ALRDL
!                       (note: arg NCAT removed; CDATA now an array;
!                       RDATA renamed FDATA).
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FLAG, IUNIT, OUNIT  
INTEGER :: N1, N2  
CHARACTER (LEN=*) :: LINE  
!
! Output arguments
DOUBLEPRECISION FDATA (N1, N2)  
!
! Locals, etc
INTEGER :: iy, ky, ix
!
CHARACTER (80) :: HEAD
CHARACTER(132) :: MSG  
!
READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
IF (INDEX (HEAD, LINE) .EQ.0) THEN  
   WRITE (MSG, 9002) LINE, HEAD  
   CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  

ENDIF  
!        Read floating-point data
!        ------------------------
!
IF (FLAG.EQ.0) THEN  
!
!           * Simple array
   READ (IUNIT, *, ERR = 8300, END = 8300) FDATA  
!
ELSE  
!
!           * Grid-based array: read indexed rows, North to South
   DO 500 IY = N2, 1, - 1  
      READ (IUNIT, *, ERR = 8430, END = 8430) KY, (FDATA (IX, IY), &
       IX = 1, N1)
      IF (KY.NE.IY) GOTO 8430  
  500    END DO  
!

ENDIF  

RETURN  
!     * Title line read error
 8010 WRITE (MSG, 9801) LINE, HEAD0_alredf  

CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
!     * Real data error
 8300 WRITE (MSG, 9810) 'floating-point', HEAD  
CALL ERROR (FFFATAL, 7, OUNIT, 0, 0, MSG)  
!
!     * Real grid error
 8430 WRITE (MSG, 9842) 'floating-point', IY, HEAD  
CALL ERROR (FFFATAL, 11, OUNIT, 0, 0, MSG)  
!
!
! Format statements
! -----------------
!
! Note: Take care not to exceed internal file length
!
!
 9002 FORMAT ( 'Title line mismatch: expected "', A, &
&         '" but found "',                   A, '"' )
!
 9801 FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
!
 9810 FORMAT ( 'Reading ', A, ' data under heading: ', A )  
!

 9842 FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  
!
END SUBROUTINE ALREDF



!SSSSSS SUBROUTINE ALREDI (FLAG, IUNIT, OUNIT, LINE, N1, N2, IDATA)  
SUBROUTINE ALREDI (FLAG, IUNIT, OUNIT, LINE, N1, N2, IDATA)  
!
!----------------------------------------------------------------------*
!
!  Utility routine to handle an input data file
!
!----------------------------------------------------------------------*
!       Version: 3.4.2  Context: SHETRAN/AL
! Modifications:
! RAH  16.09.94  3.4.1  Written by AB/RAH.  File created 10.12.93.
! RAH  22.03.95  3.4.2  New header.
!                       Remove arguments N1,...,RDATA & create ENTRY
!                       points ALRDI, etc, including new option ALRDL
!                       (note: arg NCAT removed; CDATA now an array;
!                       RDATA renamed FDATA).
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FLAG, IUNIT, OUNIT  
INTEGER :: N1, N2  
CHARACTER (LEN=*) :: LINE  
!
! Output arguments
INTEGER :: IDATA (N1, N2)  
!
! Locals, etc
INTEGER :: iy, ky, ix
CHARACTER (80) :: HEAD
CHARACTER(17)  :: FORM
CHARACTER(132) :: MSG 
!
READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
IF (INDEX (HEAD, LINE) .EQ.0) THEN  
   WRITE (MSG, 9002) LINE, HEAD  
   CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  

ENDIF  
!        Read integer data
!        -----------------
!
IF (FLAG.EQ.0) THEN  
!
!           * Simple array
   READ (IUNIT, *, ERR = 8200, END = 8200) IDATA  
!
ELSE  
!
!           * Grid-based array: read indexed rows, North to South
!           * (using single digit integers if possible)
   IF (FLAG.LT.10) WRITE (FORM, 9410) N1  
   DO 400 IY = N2, 1, - 1  
      IF (FLAG.LT.10) THEN  
         READ (IUNIT, FORM, ERR = 8420, END = 8420) KY, (IDATA ( &
          IX, IY), IX = 1, N1)
      ELSE  
         READ (IUNIT, *, ERR = 8420, END = 8420) KY, (IDATA (IX, &
          IY), IX = 1, N1)
      ENDIF  
      IF (KY.NE.IY) GOTO 8420  
  400    END DO  
!
ENDIF  
!

RETURN  
!     * Title line read error
 8010 WRITE (MSG, 9801) LINE, HEAD0_alredi  

CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
!     * Integer data error
 8200 WRITE (MSG, 9810) 'integer', HEAD  
CALL ERROR (FFFATAL, 6, OUNIT, 0, 0, MSG)  
!
!     * Integer grid error
 8420 WRITE (MSG, 9842) 'integer', IY, HEAD  
CALL ERROR (FFFATAL, 10, OUNIT, 0, 0, MSG)  
!
!
!
! Format statements
! -----------------
!
! Note: Take care not to exceed internal file length
!
!
 9002 FORMAT ( 'Title line mismatch: expected "', A, &
&         '" but found "',                   A, '"' )
!
 9410 FORMAT ( '(I7,1X,', I4, 'I1)' )  
!
 9801 FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
!
 9810 FORMAT ( 'Reading ', A, ' data under heading: ', A )  
!

 9842 FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  
!
END SUBROUTINE ALREDI



!SSSSSS SUBROUTINE ALREDL (FLAG, IUNIT, OUNIT, LINE, N1, N2, LDATA)  
SUBROUTINE ALREDL (FLAG, IUNIT, OUNIT, LINE, N1, N2, LDATA)  
!
!----------------------------------------------------------------------*
!
!  Utility routine to handle an input data file
!
!----------------------------------------------------------------------*
!       Version: 3.4.2  Context: SHETRAN/AL
! Modifications:
! RAH  16.09.94  3.4.1  Written by AB/RAH.  File created 10.12.93.
! RAH  22.03.95  3.4.2  New header.
!                       Remove arguments N1,...,RDATA & create ENTRY
!                       points ALRDI, etc, including new option ALRDL
!                       (note: arg NCAT removed; CDATA now an array;
!                       RDATA renamed FDATA).
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FLAG, IUNIT, OUNIT  
INTEGER :: N1, N2  
CHARACTER (LEN=*) :: LINE  
!
! Output arguments
LOGICAL :: LDATA (N1, N2)  
CHARACTER (80) :: HEAD
CHARACTER(132) :: MSG 
!
READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
IF (INDEX (HEAD, LINE) .EQ.0) THEN  
   WRITE (MSG, 9002) LINE, HEAD  
   CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  

ENDIF  
!        Read logical data
!        -----------------
!
READ (IUNIT, *, ERR = 8600, END = 8600) LDATA  
!

RETURN  
!     * Title line read error
 8010 WRITE (MSG, 9801) LINE, HEAD0_ALREDL  

CALL ERROR(FFFATAL, 3, OUNIT, 0, 0, MSG)  
!     * Logical data error
 8600 WRITE (MSG, 9810) 'logical', HEAD  
CALL ERROR(FFFATAL, 14, OUNIT, 0, 0, MSG)  
!
!
!
! Format statements
! -----------------
!
! Note: Take care not to exceed internal file length
!
!
 9002 FORMAT ( 'Title line mismatch: expected "', A, &
&         '" but found "',                   A, '"' )
!
 9801 FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
!

 9810 FORMAT ( 'Reading ', A, ' data under heading: ', A )  
!
END SUBROUTINE ALREDL





!SSSSSS SUBROUTINE ALSPRD (M, N, N1, DEL)  
SUBROUTINE ALSPRD (M, N, N1, DEL)  
!----------------------------------------------------------------------*
!
! Choose a sub-sequence of M items from a sequence of N items:
!     N1 is the starting index;  DEL is the stride
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/AL/ALSPRD/4.1
! Modifications:
! RAH  970805  4.1  Create.
!----------------------------------------------------------------------*
! Input arguments

INTEGER :: M, N  
! Output arguments

INTEGER :: N1, DEL  
! Locals, etc
!INTRINSIC DIM, MAX, MOD  
INTEGER :: DNE, MM, NE, NEMAX, NF  


LOGICAL :: TEST  
!----------------------------------------------------------------------*

IF (M.LE.1) THEN  
   N1 = N / (MAX (0, M) + 1) + 1  

   DEL = N  

ELSE  
!         * set the number NE of out-lying items - even if possible
   MM = M - 1  
   NE = MOD (N - 1, MM)  
   NF = NE+MM  
   TEST = MOD (NE, 2) .EQ.1.AND.MOD (NF, 2) .EQ.0.AND.NF.LE.N - M  

   IF (TEST) NE = NF  
!         * add a few if it makes a more uniform spread
   DNE = MM * (1 + MOD (MM, 2) * (1 - MOD (NE, 2) ) )  
   NEMAX = 2 * (N - M) / (M + 1)  

   NE = NE+ (IDIMJE(NEMAX, NE) / DNE) * DNE  
!         * round up
   N1 = 1 + (NE+1) / 2  

   DEL = (N - NE-1) / MM  

ENDIF  
END SUBROUTINE ALSPRD


!SSSSSS SUBROUTINE ALTRAP ()  
SUBROUTINE ALTRAP ()  
!
!----------------------------------------------------------------------*
!
! Set traps for floating-point exceptions
!
!----------------------------------------------------------------------*
! Version:  3.4.1 (sun)    Notes:  SSR79
!  Module:  AL           Program:  SHETRAN
! Modifications:
!  RAH  30.09.94  Version 3.4.1 created.
!  SB 7/3/00 Version 4g-pc remove ieee calls
!----------------------------------------------------------------------*
!
! Locals, etc
INTEGER :: OUT  
PARAMETER (OUT = 0)  
!
INTEGER :: I  
!
!----------------------------------------------------------------------*
!
!      I = IEEE_HANDLER( 'set', 'common', ABORT )
I = 0  
!
IF (I.NE.0) CALL ERROR(WWWARN, 13, OUT, 0, 0, 'Could not set traps for floating-point exceptions')
!
END SUBROUTINE ALTRAP


END MODULE ALmod