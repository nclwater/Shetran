MODULE oc_utils
! Utility routines for overland channel calculations
! Contains OCLTL and LINKNO functions
! Extracted from OCmod.f90

   USE SGLOBAL
   USE AL_C ,     ONLY : LINKNS
   USE AL_G ,     ONLY : ICMREF
   USE oc_common_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OCLTL, LINKNO

CONTAINS

   SUBROUTINE OCLTL (NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
!
! READ IN ARRAY OF ALPHANUMERIC CODES FOR CHANNEL DEFINITION
!
! INPUT PARAMETERS:
!   NNX     X DIMENSION OF GRID
!   NNY     Y DIMENSION OF GRID
!   NXE     X DIMENSION OF ARRAY
!   NYE     Y DIMENSION OF ARRAY
!   INF     INOUT FILE UNIT NUMBER
!   IOF     OUTPUT FILE UNIT NUMBER
!   BPCNTL  LOGICAL PRINT CONTROL
!
! OUTPUT PARAMETERS:
!   IARR    ARRAY OF CODES READ IN FROM FILE
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      CHARACTER (LEN=80)  :: TITLE
      CHARACTER (LEN=1)   :: CODES (11), A1LINE (500)
      INTEGER, INTENT(IN) ::  NNX, NNY, NXE, NYE, INF, IOF
      INTEGER             :: IARR (NXE, NYE), i, j, k, L, m
      LOGICAL, INTENT(IN) :: BPCNTL
      LOGICAL             :: iscycle, iscycle40
      DATA CODES / 'I', '.', ' ', ' ', ' ', 'R', 'W', 'A', 'H', 'F', 'P' /
!
      READ (INF, 10) TITLE
10    FORMAT (A80)
      IF (BPCNTL) WRITE (IOF, 20) TITLE
20    FORMAT (A80)
!
      DO J = 1, NNY
         DO I = 1, NNX
            IARR (I, J) = 0
         END DO
      END DO
!
      I = NNY
      iscycle40 = .FALSE.
      DO 40 J = 1, NNY
         IF(iscycle40) CYCLE
         read (INF, 50) K, (A1LINE (L), L = 1, NNX)
50       FORMAT   (I7, 1X, 500A1)
         IF (BPCNTL) WRITE (IOF, 50) K, (A1LINE (L), L = 1, NNX)
         !
         IF (K.NE.I) THEN
            iscycle40=.TRUE.
            CYCLE
         ENDIF
         I = I - 1

         DO L = 1, NNX   !70
            iscycle=.FALSE.
            DO M = 1, 11
               IF(iscycle) CYCLE
               !AD? IF (A1LINE (L) .EQ.CODES (M) .AND.CODES (M) .NE.' ') THEN
               IF ((A1LINE (L)==CODES(M)) .AND. (CODES(M)/=' ')) THEN
                  IARR (L, K) = M
                  iscycle=.TRUE.
               ENDIF
            ENDDO
         ENDDO  !70
         !
40    END DO
      IF(.NOT. iscycle40) RETURN
!
100   IF (BPCNTL) WRITE (IOF, 110)
110   FORMAT ('  ^^^   INCORRECT COORDINATE')
      STOP
   END SUBROUTINE OCLTL

!FFFFFF INTEGER FUNCTION LINKNO
   INTEGER FUNCTION LINKNO (I, J, NSOUTH)
! GET LINK NUMBER, GIVEN X, Y CO-ORDINATES AND ORIENTATION
      LOGICAL, INTENT(IN) :: NSOUTH
      INTEGER, INTENT(IN) :: i, j
      INTEGER             :: L
      LOGICAL             :: jedum, iscycle !NEEDED FOR AD
      LINKNO = 0
      IF (total_no_links.EQ.0) RETURN
      iscycle=.FALSE.
      DO L = 1, total_no_links
         IF(iscycle) CYCLE
         jedum = NSOUTH.EQV.LINKNS (L)
         IF ((ICMREF (L, 2) .EQ.I) .AND. (ICMREF (L, 3) .EQ.J) .AND. jedum ) THEN
            LINKNO = L
            iscycle=.TRUE.
         ENDIF
      ENDDO
   END FUNCTION LINKNO

END MODULE oc_utils
