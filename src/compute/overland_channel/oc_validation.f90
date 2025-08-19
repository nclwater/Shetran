MODULE oc_validation
! Data validation and checking routines for overland channel calculations
! Contains OCCHK0, OCCHK1, OCCHK2 - extracted from oc_initialization.f90

   USE SGLOBAL
   USE AL_C ,     ONLY : IDUM, NBFACE, CWIDTH, ZBFULL, &
      DUMMY, ZBEFF, ICMBK, BEXBK, QBKB, QBKF, ICMRF2, &
      TIH, DHF, CLENTH, CLENTH, PNETTO, QH, QOC, LINKNS, ARXL
   USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, OCNOW, OCNEXT, OCD, ESWA, QMAX, NOCBCC, &
      NOCBCD, LCODEX, LCODEY, NOCTAB, OHB, OFB
   USE AL_G ,     ONLY : NGDBGN, NX, NY, ICMREF, ICMXY
   USE UTILSMOD , ONLY : HINPUT, FINPUT, AREADR, AREADI, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT
   USE mod_load_filedata ,    ONLY : ALCHK, ALCHKI, ALINIT
   USE OCmod2 ,   ONLY : GETHRF, GETQSA, GETQSA_ALL, SETHRF, SETQSA, CONVEYAN, OCFIX, XSTAB, &
      HRFZZ, qsazz, INITIALISE_OCMOD  !these needed only for ad
   USE OCQDQMOD,  ONLY : OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD !, &  !REST NNEDED ONLY FOR AD
   USE oc_common_data
   USE oc_utils, ONLY : LINKNO

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OCCHK0, OCCHK1, OCCHK2

CONTAINS

!SSSSSS SUBROUTINE OCCHK0
   SUBROUTINE OCCHK0()
!----------------------------------------------------------------------*
!
!  Check static variables & constants input to the OC
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCCHK0/4.2
! Modifications:
! RAH  980130  4.2  New.
!      980203       Add argument NGDBGN; remove OHB,OFB.  NELEE.ge.NX.
!                   INQUIRE first, and use OUNIT for messages.
!      980206       NELEE.ge.NOCTAB*NOCTAB.
!JE Jan 2009        Above restriction removed
!----------------------------------------------------------------------*
      INTEGER       :: ERRNUM, I, IUNDEF, IUNIT, NERR, OUNIT
      INTEGER       :: IDUMS (1), IDUMO (1)
      LOGICAL       :: BOPEN, LDUM1 (1)
      CHARACTER(47) :: MSG
      CHARACTER(11) :: FORM
      CHARACTER(3)  :: NAME
      nerr = 0
!----------------------------------------------------------------------*
! 1. Unit Numbers
! ---------------
!PRI, OCD
      OUNIT = PPPRI
      IUNIT = PPPRI
      NAME = 'PRI'
      DO  I = 0, 1
         INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)
         IF (.NOT.BOPEN) THEN
            WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'
            ERRNUM = 1008
            IF (I.EQ.0) OUNIT = 0
            CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         ELSEIF (FORM.NE.'FORMATTED') THEN
            WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM
            ERRNUM = 1009
            IF (I.EQ.0) OUNIT = 0
            CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         ENDIF
         IUNIT = OCD
         NAME = 'OCD'
      ENDDO
      IDUMS (1) = MIN (PPPRI, OCD)

      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ PRI, OCD ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
! 2. Array Sizes
! --------------
!NELEE
      IDUMS (1) = NELEE
      IDUMO (1) = MAX (NX, total_no_elements)! , NOCTAB*NOCTAB)
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NLFEE
      IDUMS (1) = NLFEE
      IDUMO (1) = MAX (1, total_no_links)
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NXEE
      IDUMS (1) = NXEE
      IDUMO (1) = NX
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NOCTAB
      IDUMS (1) = NOCTAB
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NOCTAB', 'GE', IONE1, IDUMS, NERR, LDUM1)
!NXSCEE
      IDUMS (1) = NXSCEE

      CALL ALCHKI (EEERR, 1002, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXSCEE', 'GT', IONE1, IDUMS, NERR, LDUM1)
! 3. Number of Entities
! ---------------------
!NLF
      IDUMS (1) = total_no_links
      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO1, IDUMS, NERR, LDUM1)
      IDUMO (1) = total_no_elements
      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
!NX, NY
      IDUMS (1) = MIN (NX, NY)
      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ NX, NY ]', 'GE', IONE1, IDUMS, NERR, LDUM1)
!NGDBGN
      IDUMS (1) = NGDBGN
      IDUMO (1) = total_no_links + 1


      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NGDBGN', 'EQ', IDUMO, IDUMS, NERR, LDUM1)
! 4. Finish
! ---------
      IF (NERR.GT.0) THEN
         CALL ERROR(FFFATAL, 1000, OUNIT, 0, 0, 'Error(s) detected while checking OC input variables & constants')
      ENDIF
9100  FORMAT ('File unit ',A,' =',I4,1X,A:1X,A)
   END SUBROUTINE OCCHK0

!SSSSSS SUBROUTINE OCCHK1
   SUBROUTINE OCCHK1(SZLOG, LDUM1)
!----------------------------------------------------------------------*
!
!  Check static OC input arrays
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCCHK1/4.2
! Modifications:
! RAH  980203  4.2  New.
!      980205       Add argument LDUM1.
!----------------------------------------------------------------------*
! Entry requirements:
!  [ NEL, NX, NY ].ge.1     NELEE.ge.NEL    NXEE.ge.NX
!  PRI open for F output    size_of_LDUM1.ge.[NX,NEL]
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN) :: szlog
!INTEGER, INTENT(inout) :: afromICMREF(4)  !AD aliasing
      LOGICAL, INTENT(OUT):: LDUM1 (szlog)  !LDUM1 ( * )
      INTEGER             :: CODE, FACE, I, IELx, IUNDEF, NERR, TYPEE, X, Y
      INTEGER             :: IDUMO (1)
      CHARACTER(23)       :: NAME
      CHARACTER           :: XY (0:1)
      DATA NERR / 0 / , XY / 'X', 'Y' /
!----------------------------------------------------------------------*
! 1. Index Arrays
! ---------------
      IDUMO (1) = total_no_elements
!ICMREF
      DO 110 FACE = 1, 4
         CALL ALCHKI (EEERR, 1057, PPPRI, 1, total_no_elements, FACE, 2, 'ICMREF(iel,face,2)' &
!&, 'LE', IDUMO, ICMREF (1, FACE, 2) , NERR, LDUM1)
!&, 'LE', IDUMO, afromICMREF(FACE) , NERR, LDUM1(1:NEL))
         &, 'LE', IDUMO, ICMREF(1:total_no_elements, 4+FACE) , NERR, LDUM1(1:total_no_elements))
110   END DO
!ICMXY
      DO 120 Y = 1, NY
         CALL ALCHKI (EEERR, 1057, PPPRI, 1, NX, Y, IUNDEF, 'ICMXY(x,y)', &
            'LE', IDUMO, ICMXY (1, Y) , NERR, LDUM1(1:NX))
120   END DO
! 2. Channel Definition Arrays
! ----------------------------
!LCODEX,LCODEY
      NAME = 'validity_of_LCODE?(x,y)'
      DO 230 I = 0, 1
         NAME (18:18) = XY (I)
         DO 220 Y = 1, NY
            DO 210 X = 1, NX
               CODE = 0
               TYPEE = LCODEX (X, Y) * (1 - I) + LCODEY (X, Y) * I
               IF ((TYPEE.GE.7).AND.(TYPEE.LE.11)) THEN
                  ielx = LINKNO (X, Y, I.EQ.0)
                  IF (ielx.LE.0.OR.ielx.GE.NGDBGN) CODE = TYPEE
               ENDIF
               IDUM (X) = CODE
210         END DO
            CALL ALCHKI (EEERR, 1058, PPPRI, 1, NX, Y, IUNDEF, NAME, 'EQ', &
               IZERO1, IDUM, NERR, LDUM1(1:NX))
220      END DO
230   END DO
! 3. Finish
! ---------

      IF (NERR.GT.0) CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking static OC input arrays')
   END SUBROUTINE OCCHK1

!SSSSSS SUBROUTINE OCCHK2
   SUBROUTINE OCCHK2 (DDUM1A, DDUM1B, SZLOG, LDUM1)
!----------------------------------------------------------------------*
!
!  Check OC input data
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCCHK2/4.2
! Modifications:
! RAH  980203  4.2  New: partly from part of OCPLF.
!      980206       Check unit numbers.
!      980218       Don't check units if NONEED.
!----------------------------------------------------------------------*
! Entry requirements:
!  NEL.ge.[NLF,1]    NLFEE.ge.[NLF,1]    PRI open for F output
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN)           :: szlog
      DOUBLEPRECISION, INTENT(OUT) :: DDUM1A (:), DDUM1B(:)
      LOGICAL, INTENT(IN)          :: LDUM1(szlog)  !LDUM1 ( * )
      INTEGER                      :: ERRNUM, I, IELw, IUNDEF, IUNIT, N, NERR
      INTEGER                      :: IDUMS (1)
      LOGICAL                      :: BOPEN, NONEED
      CHARACTER (47)               :: MSG
      CHARACTER(11)                :: FORM
      CHARACTER(3)                 :: NAME
      CHARACTER(19)                :: SUBJ
      DATA NERR / 0 /
!----------------------------------------------------------------------*
! 1. Unit Numbers
! ---------------
!OHB,OFB
      IDUMS (1) = 0
      IUNIT = OHB
      NAME = 'OHB'
      NONEED = NOCHB.EQ.0

      DO I = 0, 1
         IF (.NOT. NONEED) THEN
            IDUMS (1) = MIN (IUNIT, IDUMS (1) )
            INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)
            IF (.NOT.BOPEN) THEN
               WRITE (MSG, 9300) NAME, IUNIT, 'is not connected to a file'
               ERRNUM = 1008
               CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)
               NERR = NERR + 1
            ELSEIF (FORM.NE.'FORMATTED') THEN
               WRITE (MSG, 9300) NAME, IUNIT, 'has format type', FORM
               ERRNUM = 1009
               CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)
               NERR = NERR + 1
            ENDIF
         ENDIF
         IUNIT = OFB
         NAME = 'OFB'
         NONEED = NOCFB.EQ.0
      END DO
      CALL ALCHKI (EEERR, 1003, PPPRI, 1, 1, IUNDEF, IUNDEF, '[ OHB, OFB ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
! 2. Element Properties
! ---------------------
!STRX

      CALL ALCHK(EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRX(iel)', 'GT', ZERO1, ZERO, STRXX, NERR, LDUM1)
!STRY
      CALL ALCHK (EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRY(iel)', 'GT', zero1, zero, STRYY, NERR, LDUM1)
! 3. Cross-section Tables
! -----------------------
!
      IF (total_no_links.GT.0) THEN
!XINH
         CALL ALCHK (EEERR, 1016, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINH(link)[j=1]', 'EQ', zero1, zero , XINH, NERR, LDUM1)
         DO 310 ielw = 1, total_no_links
            N = NXSECT (ielw) - 1
            WRITE (SUBJ, 9310) ielw
            !CALL DCOPY(N, XINH(IEL,1), NLFEE, DDUM1A(1:n), 1)
            !CALL DCOPY(N, XINH(IEL, 2), NLFEE, DDUM1B(1:n), 1)
            DDUM1A(1:n) = XINH(IELw,1:n)
            DDUM1B(1:n) = XINH(ielw, 2:n+1)
            CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GTa', DDUM1A, zero , DDUM1B, NERR, LDUM1)
!XINW
            SUBJ (4:4) = 'W'
            !CALL DCOPY(N, XINW(IEL,1), NLFEE, DDUM1A(1:n), 1)
            !CALL DCOPY(N, XINW(IEL,2), NLFEE, DDUM1B(1:n), 1)
            DDUM1A(1:n) = XINW(ielw,1:n)
            DDUM1B(1:n) = XINW(ielw,2:n+1)
            CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, &
               'GEa', DDUM1A, zero , DDUM1B, NERR, LDUM1)
310      END DO
         DO 320 ielw = 1, total_no_links
            DDUM1A (ielw) = XINW (ielw, NXSECT (ielw) )
320      END DO
         CALL ALCHK (EEERR, 1056, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINW[link,NXSECT(link)]', 'GT', zero1, zero , &
            DDUM1A, NERR, LDUM1)
      ENDIF

      IF (NERR.GT.0) then
!!!! sb 190522 negative strickler for surface storage
         CALL ERROR(WWWARN, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
!    CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
      ENDIF
9300  FORMAT ('File unit ',A,' =',I4,1X,A:1X,A)

9310  FORMAT ('XINH[ link =',I3,'](j)')
   END SUBROUTINE OCCHK2

END MODULE oc_validation
