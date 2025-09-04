MODULE sediment_initialization
   USE SGLOBAL
   USE sediment_common
   USE sediment_transport_capacity, ONLY : SYDR
   USE mod_load_filedata, ONLY : ALINIT, ALCHKI, ALCHK, ALALLF, ALREAD
   USE UTILSMOD

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SYBC, SYERR0, SYERR1, SYERR2, SYERR3, SYINIT, SYREAD

CONTAINS

   ! Error handling procedures for modernizing GOTO statements
   SUBROUTINE handle_insufficient_workspace(nelee, nreq, spr)
      INTEGER, INTENT(IN) :: nelee, nreq, spr
      CHARACTER(LEN=200) :: msg
      WRITE (msg, '("Workspace available is NELEE = ", I5, "; workspace required in subroutine SYREAD is ", I6)') nelee, nreq
      CALL ERROR (1, 2005, spr, 0, 0, msg)
   END SUBROUTINE handle_insufficient_workspace

   SUBROUTINE handle_invalid_nsed(nsed, nsedee, spr)
      INTEGER, INTENT(IN) :: nsed, nsedee, spr
      CHARACTER(LEN=200) :: msg
      WRITE (msg, '("No. of size groups NSED=", I4, " is not in range [1,NSEDEE=", I3, "]")') nsed, nsedee
      CALL ERROR (1, 2006, spr, 0, 0, msg)
   END SUBROUTINE handle_invalid_nsed

   SUBROUTINE handle_nsyb_too_large(nsyb, nsybee, spr)
      INTEGER, INTENT(IN) :: nsyb, nsybee, spr
      CHARACTER(LEN=200) :: msg
      WRITE (msg, '("No. of boundaries NSYB=", I5, " is greater than NSYBEE=", I4, "]")') nsyb, nsybee
      CALL ERROR (1, 2007, spr, 0, 0, msg)
   END SUBROUTINE handle_nsyb_too_large

   SUBROUTINE handle_invalid_boundary_type(bb, itype, spr)
      INTEGER, INTENT(IN) :: bb, itype, spr
      CHARACTER(LEN=200) :: msg
      WRITE (msg, '("Boundary type NSYBCD(", I4, ",2)=", I2, " is not is the range [1,4]")') bb, itype
      CALL ERROR (1, 2008, spr, 0, 0, msg)
   END SUBROUTINE handle_invalid_boundary_type

   SUBROUTINE handle_nsyc1_too_large(nsyc1, nsycee, spr)
      INTEGER, INTENT(IN) :: nsyc1, nsycee, spr
      CHARACTER(LEN=200) :: msg
      WRITE (msg, '("No. of steady flux categories NSYC(1)=", I4, " is greater than NSYCEE=", I3, "]")') nsyc1, nsycee
      CALL ERROR (1, 2009, spr, 0, 0, msg)
   END SUBROUTINE handle_nsyc1_too_large

   SUBROUTINE handle_nsyc3_too_large(nsyc3, nsycee, spr)
      INTEGER, INTENT(IN) :: nsyc3, nsycee, spr
      CHARACTER(LEN=200) :: msg
      WRITE (msg, '("No. of steady rating categories NSYC(3)=", I4, " is greater than NSYCEE=", I3, "]")') nsyc3, nsycee
      CALL ERROR (1, 2010, spr, 0, 0, msg)
   END SUBROUTINE handle_nsyc3_too_large

   SUBROUTINE handle_error_processing(spr)
      INTEGER, INTENT(IN) :: spr
      CALL ERROR (1, 2000, spr, 0, 0, 'Error(s) detected while checking SY input data')
   END SUBROUTINE handle_error_processing

!SSSSSS SUBROUTINE SYBC
   SUBROUTINE SYBC
!!!!STOP ' FATAL ERROR!!  Sediment boundary flows not yet implemented'
   END SUBROUTINE SYBC
!SSSSSS SUBROUTINE SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
   SUBROUTINE SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
      NSEE, NV, NVEE, NX, NXEE, NY, SPR, SYD)
!
!----------------------------------------------------------------------*
!
! Check static variables & constants in the WAT-SY interface.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR82
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  23.09.94  Version 3.4.1 created.
!----------------------------------------------------------------------*
!
      INTEGER :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE
      INTEGER :: NV, NVEE, NX, NXEE, NY, SPR, SYD
!
! Locals, etc
      INTEGER :: FATAL, ERR
      PARAMETER (FATAL = 1, ERR = 2)
!
      INTEGER :: IUNDEF, NERR, jedumdum
      INTEGER :: IDUMS (1), IDUMO (1)
      LOGICAL :: LDUM1 (1)
!
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * Initialize local counter
      NERR = 0
!
!
! 1. Array Sizes
! --------------
!
!NELEE
      IDUMS (1) = NELEE
      IDUMO (1) = MAX (NEL, NV, NX * NY)
      CALL ALCHKI (ERR, 2054, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', &
         IDUMO, IDUMS, NERR, LDUM1)
!NLFEE
      IDUMS (1) = NLFEE
      IDUMO (1) = MAX (1, NLF)
      CALL ALCHKI (ERR, 2055, SPR, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', &
         IDUMO, IDUMS, NERR, LDUM1)
!NLYREE, NSEDEE
      IDUMS (1) = MIN (NLYREE, NSEDEE)
      CALL ALCHKI (ERR, 2056, SPR, 1, 1, IUNDEF, IUNDEF, '[ NLYREE, NSEDEE ]', 'GT', IZERO1, IDUMS, NERR, LDUM1)
!NSEE
      IDUMS (1) = NSEE
      IDUMO (1) = NS
      CALL ALCHKI (ERR, 2057, SPR, 1, 1, IUNDEF, IUNDEF, 'NSEE', 'GE', &
         IDUMO, IDUMS, NERR, LDUM1)
!NVEE
      IDUMS (1) = NVEE
      IDUMO (1) = NV
      CALL ALCHKI (ERR, 2058, SPR, 1, 1, IUNDEF, IUNDEF, 'NVEE', 'GE', &
         IDUMO, IDUMS, NERR, LDUM1)
!NXEE
      IDUMS (1) = NXEE
      IDUMO (1) = NX
      CALL ALCHKI (ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', &
         IDUMO, IDUMS, NERR, LDUM1)
      IDUMO (1) = 9999
      CALL ALCHKI (ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'LE', &
         IDUMO, IDUMS, NERR, LDUM1)
!
!
! 2. Unit Numbers
! ---------------
!
!SPR, SYD
      IDUMS (1) = MIN (SPR, SYD)
      CALL ALCHKI (ERR, 2060, SPR, 1, 1, IUNDEF, IUNDEF, '[ SPR, SYD ]', &
         'GE', IZERO1, IDUMS, NERR, LDUM1)
!
!
! 3. Number of Entities
! ---------------------
!
!NLF
      IDUMS (1) = NLF
      IDUMO (1) = NEL
      CALL ALCHKI (ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', &
         IZERO1, IDUMS, NERR, LDUM1)
      CALL ALCHKI (ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', &
         IDUMO, IDUMS, NERR, LDUM1)
!NS, NV, NX, NY
      jedumdum = MIN (NS, NV)
!""AD IDUMS (1) = MIN (NS, NV, NX, NY)
      IDUMS (1) = MIN (jedumdum, NX, NY)
      CALL ALCHKI (ERR, 2062, SPR, 1, 1, IUNDEF, IUNDEF, '[ NS, NV, NX, NY ]', 'GT', IZERO1, IDUMS, NERR, LDUM1)
!
!
! 4. Epilogue
! -----------
!
      IF (NERR.GT.0) CALL ERROR (FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking WAT-SY interface variables')
!
   END SUBROUTINE SYERR0
!SSSSSS SUBROUTINE SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
   SUBROUTINE SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
      NXEE, NYEE, NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
      NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, &
      ARXL, HRF, ZGRUND, IDUM, IDUM1X, LDUM)
!
!----------------------------------------------------------------------*
!
! Check static & initializing arrays in the WAT-SY interface.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR83
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  26.09.94  Version 3.4.1.  File created 25.09.94.
!----------------------------------------------------------------------*
!
      INTEGER :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, NY, &
         SPR
      INTEGER :: ICMBK (NLFEE, 2), ICMXY (NXEE, NY), ICMREF (NELEE, 4, &
         2:3)
      INTEGER :: ICMRF2 (NLFEE, 3, 2), NLYR (NLF + 1:NEL)
      INTEGER :: NTSOIL (NELEE, NLYREE), NVC (NLF + 1:NEL)
      DOUBLEPRECISION THSAT (NS)
      DOUBLEPRECISION CLENTH (NLFEE), CWIDTH (NLFEE), ZBFULL (NLFEE)
      DOUBLEPRECISION DXQQ (NLF + 1:NEL), DYQQ (NLF + 1:NEL)
      DOUBLEPRECISION AREA (NEL), DHF (NELEE, 4)
      DOUBLEPRECISION ARXL (NLFEE), HRF (NLF + 1:NEL), ZGRUND (NEL)
      LOGICAL :: BEXBK, LINKNS (NLFEE)
!
! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE) :: IDUM
      INTEGER :: IDUM1X ( - 1:NEL + 1)
      LOGICAL :: LDUM (NELEE)
!
! Locals, etc
      INTEGER :: FATAL, ERR
      PARAMETER (FATAL = 1, ERR = 2)
!
      INTEGER :: BANK, COUNT, FACE, FADJ, FEL
      INTEGER :: IADJ, IBR, IBRADJ, ICOL1, IEL, IELP, ILYR, IUNDEF, IX, &
         IY
      INTEGER :: LINK, NCOL, NELP, NERR, P, PADJ
      INTEGER :: IDUM1 (2)
      LOGICAL :: BKXYOK, REFOK, found_match
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * local counter
      NERR = 0
!     * position of 1st column element
      ICOL1 = NLF + 1
!     * number of elements plus one
      NELP = NEL + 1
!
!
! 1. Index Arrays
! ---------------
!
!ICMBK, ICMXY
      COUNT = NERR
!     * initialize column-element counter & marker array
      NCOL = 0
      DO 110 IEL = 0, NLF
         IDUM1X (IEL) = 1
110   END DO
      DO 115 IEL = ICOL1, NELP
         IDUM1X (IEL) = 0
115   END DO
!     * count active grid elements and mark them
      DO 125 IY = 1, NY
         DO 120 IX = 1, NX
            IEL = MAX (0, MIN (ICMXY (IX, IY), NELP) )
            IDUM1X (IEL) = IDUM1X (IEL) + 1
            NCOL = NCOL + MIN (IEL, 1)
120      END DO
125   END DO
!     * similarly for bank elements (if present all must be active)
      IF (BEXBK.AND.NLF.GT.0) THEN
         NCOL = NCOL + 2 * NLF
         DO 135 BANK = 1, 2
            DO 130 LINK = 1, NLF
               IEL = MAX (0, MIN (ICMBK (LINK, BANK), NELP) )
               IDUM1X (IEL) = IDUM1X (IEL) + 1
130         END DO
135      END DO
      ENDIF
!     * watch out for gate-crashers
      IDUM1 (1) = NEL - NLF
      IDUM1X (0) = NCOL
      CALL ALCHKI (ERR, 2075, SPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X (0) , NERR, LDUM)
!     * check that each element has a unique identity
      CALL ALCHKI (ERR, 2076, SPR, 1, NEL, IUNDEF, IUNDEF, &
         'element_count(iel)', 'EQ', IONE1, IDUM1X (1) , NERR, LDUM)
!     * was everything ok?
      BKXYOK = COUNT.EQ.NERR
!
!ICMREF part 1
      IDUM1 (1) = NEL
      IDUM1 (2) = - NLFEE
      REFOK = .TRUE.
      DO 145 FACE = 1, 4
         COUNT = NERR
!        * check that all neighbours are within range
         CALL ALCHKI (ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)' &
         &, 'LE', IDUM1 (1) , ICMREF (1, FACE, 2) , NERR, LDUM)
         CALL ALCHKI (ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)' &
         &, 'GE', IDUM1 (2) , ICMREF (1, FACE, 2) , NERR, LDUM)
!        * check regular faces for range and consistency
         IF (COUNT.EQ.NERR) THEN
            DO 140 IEL = 1, NEL
               IADJ = ICMREF (IEL, FACE, 2)
               IF (IADJ.LE.0) THEN
!                 * not a regular face
                  IDUM (IEL) = 0
               ELSE
                  FADJ = ICMREF (IEL, FACE, 3)
                  IF (FADJ.LT.1.OR.FADJ.GT.4) THEN
!                    * bad face value
                     IDUM (IEL) = 1
                  ELSE
                     IF (ICMREF (IADJ, FADJ, 2) .NE.IEL) THEN
!                       * bad reflection
                        IDUM (IEL) = 2
                     ELSE
                        IDUM (IEL) = 0
!                       * faces don't match?
                        IF (ICMREF (IADJ, FADJ, 3) .NE.FACE) IDUM (IEL) &
                           = 3
                     ENDIF
                  ENDIF
               ENDIF
140         END DO
            CALL ALCHKI (ERR, 2078, SPR, 1, NEL, FACE, IUNDEF, &
               'status_of_ICMREF(iel,face)', 'EQ', IZERO1, IDUM, NERR, LDUM)
         ENDIF
!        * is everything still ok?
         REFOK = REFOK.AND.COUNT.EQ.NERR
145   END DO
!
!ICMREF part 2 (bank element neighbours)
      IF (NLF.GT.0.AND.BEXBK.AND.BKXYOK.AND.REFOK) THEN
!        * set marker array (disallow non-grids other than zero)
         IDUM1X ( - 1) = - 2
         IDUM1X (0) = 0
         DO 150 IEL = 1, NEL
            IDUM1X (IEL) = - 2
150      END DO
         DO 165 IY = 1, NY
            DO 160 IX = 1, NX
               IEL = MAX (0, ICMXY (IX, IY) )
               IDUM1X (IEL) = MIN (IEL, 1)
160         END DO
165      END DO
!        * count number of grid neighours for each link
         DO 170 LINK = 1, NLF
            IDUM (LINK) = 0
170      END DO
         DO 185 BANK = 1, 2
            DO 180 LINK = 1, NLF
               IEL = ICMBK (LINK, BANK)
               FACE = 2 * BANK
               IF (LINKNS (LINK) ) FACE = FACE-1
               IADJ = MAX ( - 1, ICMREF (IEL, FACE, 2) )
               IDUM (LINK) = IDUM (LINK) + IDUM1X (IADJ)
180         END DO
185      END DO
         CALL ALCHKI (ERR, 2079, SPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO1, IDUM, NERR, LDUM)
      ENDIF
!
!ICMRF2
      IF (REFOK) THEN
!        * initialize status array
         DO 190 IBR = 1, NLFEE
            IDUM (IBR) = - 1
190      END DO
!        * check each prospect of each branch
         DO 198 FACE = 1, 4
            DO 196 IEL = 1, NEL
               IADJ = ICMREF (IEL, FACE, 2)
               IF (IADJ.LT.0) THEN
                  IBR = - IADJ
                  IF (IDUM (IBR) .GE.0) THEN
!                    * duplicate reference
                     IDUM (IBR) = IDUM (IBR) + 1
                  ELSE
!                    * initialize status
                     IDUM (IBR) = 0
                     DO 194 P = 1, 3
                        IADJ = ICMRF2 (IBR, P, 1)
                        IF (IADJ.GT.NEL) THEN
!                          * neighbour out of range
                           IDUM (IBR) = IDUM (IBR) + P * 10
                        ELSEIF (IADJ.GT.0) THEN
                           FADJ = ICMRF2 (IBR, P, 2)
                           IF (FADJ.LT.1.OR.FADJ.GT.4) THEN
!                             * bad face value
                              IDUM (IBR) = IDUM (IBR) + P * 100
                           ELSE
                              IBRADJ = - ICMREF (IADJ, FADJ, 2)
                              IF (IBRADJ.LT.1.OR.IBRADJ.GT.NLFEE) THEN
!                                * bad mirror branch
                                 IDUM (IBR) = IDUM (IBR) + P * 1000
                              ELSE
                                 found_match = .FALSE.
                                 DO 192 PADJ = 1, 3
                                    IELP = ICMRF2 (IBRADJ, PADJ, 1)
                                    IF (IELP.EQ.IEL) THEN
                                       FEL = ICMRF2 (IBRADJ, PADJ, 2)
                                       IF (FEL.EQ.FACE) THEN
                                          found_match = .TRUE.
                                          EXIT
                                       ENDIF
                                    ENDIF
192                              END DO
                                 IF (.NOT. found_match) THEN
!                                   * can't find a reference in the mirror
                                    IDUM (IBR) = IDUM (IBR) + P * 10000
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
194                  END DO
                  ENDIF
               ENDIF
196         END DO
198      END DO
         CALL ALCHKI (ERR, 2080, SPR, 1, NLFEE, IUNDEF, IUNDEF, &
            'status_of_ICMRF2(branch)', 'LE', IZERO1, IDUM, NERR, LDUM)
      ENDIF
!
!
! 2. Soil Properties
! ------------------
!
!THSAT
      CALL ALCHK (ERR, 2063, SPR, 1, NS, IUNDEF, IUNDEF, 'THSAT(soil)', &
         'LE', ONE1, ZERO1 (1) , THSAT, NERR, LDUM)
!
!
! 3. Link Properties & Initial State
! ----------------------------------
!
      IF (NLF.GT.0) THEN
!
!CLENTH
         CALL ALCHK (ERR, 2064, SPR, 1, NLF, IUNDEF, IUNDEF, 'CLENTH(link)' &
         &, 'GE', ZERO1, ZERO1 (1) , CLENTH, NERR, LDUM)
!CWIDTH
         CALL ALCHK (ERR, 2065, SPR, 1, NLF, IUNDEF, IUNDEF, 'CWIDTH(link)' &
         &, 'GT', ZERO1, ZERO1 (1) , CWIDTH, NERR, LDUM)
!ZBFULL
         CALL ALCHK (ERR, 2066, SPR, 1, NLF, IUNDEF, IUNDEF, 'ZBFULL(link)' &
         &, 'GEa', ZGRUND, ZERO1 (1) , ZBFULL, NERR, LDUM)
!ARXL
         CALL ALCHK (ERR, 2067, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
         &'GE', ZERO1, ZERO1 (1) , ARXL, NERR, LDUM)
!
      ENDIF
!
!
! 4. Column Properties & Initial State
! ------------------------------------
!
!DXQQ
      CALL ALCHK (ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DXQQ(iel)', 'GT', ZERO1, ZERO1 (1) , DXQQ, NERR, LDUM)
!DYQQ
      CALL ALCHK (ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DYQQ(iel)', 'GT', ZERO1, ZERO1 (1) , DYQQ, NERR, LDUM)
!HRF
      CALL ALCHK (ERR, 2069, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'HRF(iel)' , 'GEa', ZGRUND (ICOL1) , ZERO1 (1) , HRF, NERR, LDUM)
!NLYR
      COUNT = NERR
      IDUM1 (1) = NLYREE
      CALL ALCHKI (ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'GT', IZERO1, NLYR, NERR, LDUM(ICOL1:NEL))
      CALL ALCHKI (ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'LE', IDUM1, NLYR, NERR, LDUM(ICOL1:NEL))
!NTSOIL
      IF (COUNT.EQ.NERR) THEN
         DO 410 IEL = ICOL1, NEL
            ILYR = NLYR (IEL)
            IDUM (IEL) = NTSOIL (IEL, ILYR)
410      END DO
         IDUM1 (1) = NS
         CALL ALCHKI (ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, &
            'NTSOIL[iel,NLYR(iel)]', 'GT', IZERO1, IDUM (ICOL1) , NERR, &
            LDUM)
         CALL ALCHKI (ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, &
            'NTSOIL[iel,NLYR(iel)]', 'LE', IDUM1, IDUM (ICOL1) , NERR, &
            LDUM)
      ENDIF
!NVC
      COUNT = NERR
      IDUM1 (1) = NV
      CALL ALCHKI (ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'GT', IZERO1, NVC, NERR, LDUM)
      CALL ALCHKI (ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'LE', IDUM1, NVC, NERR, LDUM)
!
!
! 5. Element Properties
! ---------------------
!
!AREA
      CALL ALCHK (ERR, 2073, SPR, 1, NEL, IUNDEF, IUNDEF, 'AREA(iel)', &
         'GT', ZERO1, ZERO1 (1) , AREA, NERR, LDUM)
!DHF
      DO 510 FACE = 1, 4
         CALL ALCHK (ERR, 2074, SPR, 1, NEL, FACE, IUNDEF, 'DHF(iel,face)', &
         & 'GT', ZERO1, ZERO1 (1) , DHF (1, FACE) , NERR, LDUM)
510   END DO
!
!
! 6. Epilogue
! -----------
!
      IF (NERR.GT.0) CALL ERROR (FATAL, 2001, SPR, 0, 0, 'Error(s) detected while checking static/initial WAT-SY interface')
!
   END SUBROUTINE SYERR1
!SSSSSS SUBROUTINE SYERR2 (NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
   SUBROUTINE SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
      NV, NSYB, NSYBEE, NSYC, NSYCEE, SPR, ICMREF, ISUSED, NEPS, NFINE, &
      SFB, SRB, ALPHA, DCBEDO, FPCRIT, DLSMAX, NTSOBK, NSYBCD, NBFACE, &
      DRSED, BKB, GKF, GKR, RHOSO, SOSDFN, DRDRIP, FDRIP, XDRIP, PBSED, &
      FCG, FCROCK, PLS, DLS, FBETA, FDEL, ABC, BBC, GBC, IDUM, DUMMY, &
      LDUM)
!
!----------------------------------------------------------------------*
!
! Check for errors in the SY input data.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR43
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  04.10.94  Version 3.4.1 by AB/RAH. File created 3.2.94.
!  BTL  25.05.95  Version 3.4.1 : add DLSMAX
!----------------------------------------------------------------------*
      INTEGER :: NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV
      INTEGER :: NSYB, NSYBEE, NSYC (4), NSYCEE, SPR
      INTEGER :: ICMREF (NELEE, 4, 2:2)
      INTEGER :: ISUSED, NEPS, NFINE, SFB, SRB
      INTEGER :: NTSOBK (NLFEE), NSYBCD (NSYBEE, 3), NBFACE (NEL)
      DOUBLEPRECISION ALPHA, DCBEDO, FPCRIT
      DOUBLEPRECISION DRSED (NSED), BKB (NS), GKF (NS), GKR (NS), &
         RHOSO (NS)
      DOUBLEPRECISION SOSDFN (NSEE, NSED), DRDRIP (NV), FDRIP (NV), &
         XDRIP (NV)
      DOUBLEPRECISION PBSED (NLFEE)
      DOUBLEPRECISION FCG (NLF + 1:NEL), FCROCK (NLF + 1:NEL), PLS (NLF &
         + 1:NEL)
      DOUBLEPRECISION DLS (NEL), FBETA (NELEE, NSED), FDEL (NELEE, NSED)
      DOUBLEPRECISION ABC (NSEDEE, NSYCEE), BBC (NSEDEE, NSYCEE)
      DOUBLEPRECISION GBC (NSEDEE, NSYCEE)
      DOUBLEPRECISION DLSMAX, rdum(nxee*nyee)
!
! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE) :: IDUM
      DOUBLEPRECISION DUMMY (NELEE)
      LOGICAL :: LDUM (NELEE)
!
! Locals, etc
      INTEGER :: FATAL, ERR
      DOUBLEPRECISION TOL
      PARAMETER (FATAL = 1, ERR = 2, TOL = 1D-10)
!
      INTEGER :: BB, COUNT, FACE, ICAT, IUNDEF, IEL, ITYPE, NERR
      INTEGER :: SED, SOIL, jedumdum
      INTEGER :: IDUM1 (1)
!
!
!
!----------------------------------------------------------------------*
!
!
! 0. Preliminaries
! ----------------
!
!     * Local counter
      NERR = 0
!
!
! 1. Static Variables
! -------------------
!
!NEPS
      IDUM (1) = NEPS
      CALL ALCHKI (ERR, 2012, SPR, 1, 1, IUNDEF, IUNDEF, 'NEPS', 'GE', &
         IONE1, IDUM, NERR, LDUM)
      NEPS = IDUM (1)
!FPCRIT
      DUMMY (1) = FPCRIT
      CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'FPCRIT', 'GE', &
         ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
      FPCRIT = DUMMY (1)
!DLSMAX
      DUMMY (1) = DLSMAX
      CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'DLSMAX', 'GE', &
         ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
      DLSMAX = DUMMY (1)
!>>
      IF (NLF.GT.0) THEN
!>>
!ISUSED
         IDUM (1) = ISUSED
         CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', &
            'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', &
            'LE', IONE1, IDUM, NERR, LDUM)
         ISUSED = IDUM (1)
!NFINE
         IDUM (1) = NFINE
         IDUM1 (1) = MIN (1, NSED-1)
         CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', &
            'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', &
            'LE', IDUM1, IDUM, NERR, LDUM)
         NFINE = IDUM (1)
!ALPHA
         IF (NFINE.GT.0) THEN
            DUMMY (1) = ALPHA
            CALL ALCHK (ERR, 2016, SPR, 1, 1, IUNDEF, IUNDEF, 'ALPHA', &
               'GE', ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
            ALPHA = DUMMY (1)
         ENDIF
!DCBEDO
         DUMMY (1) = DCBEDO
         CALL ALCHK (ERR, 2017, SPR, 1, 1, IUNDEF, IUNDEF, 'DCBEDO', &
            'GE', ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
         DCBEDO = DUMMY (1)
!<<
      ENDIF
!<<
!NELEE
      IDUM (1) = NXEE*NYEE
!!!!IDUM1(1) = MAX( NSED, NLF*DIM(NSED,NFINE) )  !AD
      jedumdum = IDIMJE(NSED, NFINE)
      jedumdum = jedumdum * NLF
      idum1(1) = MAX(nsed, jedumdum)
!     * (including local workspace requirements)
      IDUM1 (1) = MAX (IDUM1 (1), NS, NSYB * 2)
      CALL ALCHKI (ERR, 2018, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', &
         IDUM1, IDUM, NERR, LDUM)
!
!
! 2. Sediment, Soil & Vegetation Properties
! -----------------------------------------
!
!     * Process sediment/soil properties if workspace is sufficient
      IF (NELEE.GE.MAX (NSED, NS) ) THEN
!
!DRSED
         COUNT = NERR
         CALL ALCHK (ERR, 2019, SPR, 1, 1, IUNDEF, IUNDEF, 'DRSED(sed)', &
            'GT', ZERO1, ZERO1 (1) , DRSED (1) , NERR, LDUM)

         !original code
         !IF ( NSED.GT.1 .AND. NERR.EQ.COUNT ) THEN
         !        CALL DCOPY( NSED-1, DRSED, 1, IDUM, 1 )
         !        CALL ALCHK    ( ERR,2019,SPR,    2,NSED,IUNDEF,IUNDEF,
         !    $          'DRSED(sed)','GEa',IDUM ,ZERO(1),   DRSED(2),NERR,LDUM )
         !     ENDIF

         IF (NSED.GT.1.AND.NERR.EQ.COUNT) THEN
            !CALL DCOPY( NSED-1, DRSED, 1, IDUM, 1 )
            CALL DCOPY (NSED-1, DRSED, 1, RDUM, 1)
            idum(1:NSED-1) = INT (rdum(1:NSED-1))
            CALL ALCHK (ERR, 2019, SPR, 2, NSED, IUNDEF, IUNDEF, 'DRSED(sed)', &
            & 'GEa', RDUM, ZERO1 (1) , DRSED (2) , NERR, LDUM)
!     $          'DRSED(sed)','GEa',IDUM ,ZERO(1),   DRSED(2),NERR,LDUM
         ENDIF
!GKR
         CALL ALCHK (ERR, 2020, SPR, 1, NS, IUNDEF, IUNDEF, 'GKR(soil)', &
            'GE', zero1, zero1 (1) , GKR, NERR, LDUM)
!GKF
         CALL ALCHK (ERR, 2021, SPR, 1, NS, IUNDEF, IUNDEF, 'GKF(soil)', &
            'GE', zero1, zero1 (1) , GKF, NERR, LDUM)
!RHOSO
         CALL ALCHK (ERR, 2022, SPR, 1, NS, IUNDEF, IUNDEF, 'RHOSO(soil)', &
            'GT', zero1, zero1 (1) , RHOSO, NERR, LDUM)
!BKB
         IF (NLF.GT.0) CALL ALCHK (ERR, 2023, SPR, 1, NS, IUNDEF, IUNDEF, &
            'BKB(soil)', 'GE', zero1, zero1 (1) , BKB, NERR, LDUM)
!SOSDFN
         CALL ALINIT (ZERO1 (1), NS, DUMMY)
         DO 220 SED = 1, NSED
            DO 210 SOIL = 1, NS
               DUMMY (SOIL) = DUMMY (SOIL) + SOSDFN (SOIL, SED)
210         END DO
            CALL ALCHK (ERR, 2024, SPR, 1, NS, SED, IUNDEF, 'SOSDFN(soil,sed)' &
            &, 'GE', zero1, zero1 (1) , SOSDFN (1, SED) , NERR, LDUM)
220      END DO
         CALL ALCHK (ERR, 2024, SPR, 1, NS, IUNDEF, IUNDEF, 'SOSDFN[*][sum_over_sed](soil)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
!XDRIP
         CALL ALCHK (ERR, 2025, SPR, 1, NV, IUNDEF, IUNDEF, 'XDRIP(veg)', &
            'GE', zero1, zero1 (1) , XDRIP, NERR, LDUM)
!DRDRIP
         CALL ALCHK (ERR, 2026, SPR, 1, NV, IUNDEF, IUNDEF, 'DRDRIP(veg)', &
            'GT', zero1, zero1 (1) , DRDRIP, NERR, LDUM)
!FDRIP
         CALL ALCHK (ERR, 2027, SPR, 1, NV, IUNDEF, IUNDEF, 'FDRIP(veg)', &
            'GE', zero1, zero1 (1) , FDRIP, NERR, LDUM)
!
!
! 3. Link Element Properties
! --------------------------
!
!
      ENDIF  ! End of sediment/soil properties processing
!
! 3. Link Element Properties
! --------------------------
!
300   IF (NLF.GT.0) THEN
!
!NTSOBK
         IDUM (1) = NS
         CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'GE', IONE1, NTSOBK, NERR, LDUM)
         CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'LE', IDUM, NTSOBK, NERR, LDUM)
!PBSED
         CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', &
         & 'GE', zero1, zero1 (1) , PBSED, NERR, LDUM)
         CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', &
         & 'LT', ONE1, ZERO1 (1) , PBSED, NERR, LDUM)
!
      ENDIF
!
!
! 4. Column-element Properties
! ----------------------------
!
!FCROCK
      CALL ALCHK (ERR, 2030, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCROCK(iel)', 'LE', ONE1, ZERO1 (1) , FCROCK, NERR, LDUM)
!FCG
      DO 410 IEL = NLF + 1, NEL
         DUMMY (IEL) = ONE1 (1) - FCROCK (IEL)
410   END DO
      CALL ALCHK (ERR, 2031, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCG(iel)', 'LEa', DUMMY (NLF + 1) , ZERO1 (1) , FCG, NERR, LDUM)
!PLS
      CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'GE', zero1, zero1 (1) , PLS, NERR, LDUM)
      CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'LT', ONE1, ZERO1 (1) , PLS, NERR, LDUM)
!
!
! 5. All-element Initialization
! -----------------------------
!
!DLS
      CALL ALCHK (ERR, 2033, SPR, 1, NEL, IUNDEF, IUNDEF, 'DLS(iel)', &
         'GE', zero1, zero1 (1) , DLS, NERR, LDUM)
!FBETA
      CALL ALINIT (ZERO1 (1), NEL, DUMMY)
      DO 520 SED = 1, NSED
         DO 510 IEL = 1, NEL
            DUMMY (IEL) = DUMMY (IEL) + FBETA (IEL, SED)
510      END DO
         CALL ALCHK (ERR, 2034, SPR, 1, NEL, SED, IUNDEF, 'FBETA(iel,sed)', &
         & 'GE', zero1, zero1 (1) , FBETA (1, SED) , NERR, LDUM)
520   END DO
      CALL ALCHK (ERR, 2034, SPR, 1, NEL, IUNDEF, IUNDEF, 'FBETA[*][sum_over_sed](iel)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
!FDEL
      DO 530 SED = 1, NSED
         CALL ALCHK (ERR, 2035, SPR, 1, NEL, SED, IUNDEF, 'FDEL(iel,sed)', &
         &'GE', zero1, zero1 (1) , FDEL (1, SED) , NERR, LDUM)
530   END DO
!
!
! 6. Boundary Data
! ----------------
!
      IF (NSYB.GT.0) THEN
!Process boundary data if workspace is sufficient
         IF (NELEE.GE.NSYB * 2) THEN
!NSYCEE
            IDUM (1) = NSYCEE
            IDUM1 (1) = MAX (NSYC (1) + NSYC (2), NSYC (3) + NSYC (4) )
            CALL ALCHKI (ERR, 2036, SPR, 1, 1, IUNDEF, IUNDEF, 'NSYCEE', &
               'GE', IDUM1, IDUM, NERR, LDUM)
!NSYBCD(BB,1)
            COUNT = NERR
            IDUM1 (1) = NEL
            CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', &
            & 'GE', IONE1, NSYBCD, NERR, LDUM)
            CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', &
            & 'LE', IDUM1, NSYBCD, NERR, LDUM)
!NBFACE
            IF (COUNT.EQ.NERR) THEN
               DO 610 BB = 1, NSYB
                  IEL = NSYBCD (BB, 1)
                  IDUM (BB) = NBFACE (IEL)
610            END DO
               IDUM1 (1) = 4
               CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, &
                  'NBFACE[NSYBCD[*][1]](bdry)', 'GE', IONE1, IDUM, NERR, LDUM)
               CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, &
                  'NBFACE[NSYBCD[*][1]](bdry)', 'LE', IDUM1, IDUM, NERR, LDUM)
            ENDIF
!ICMREF
            IF (COUNT.EQ.NERR) THEN
               DO 620 BB = 1, NSYB
                  IEL = NSYBCD (BB, 1)
                  FACE = NBFACE (IEL)
                  IDUM (BB) = ICMREF (IEL, FACE, 2)
620            END DO
               CALL ALCHKI (ERR, 2039, SPR, 1, NSYB, IUNDEF, IUNDEF, &
                  'ICMREF[NSYBCD[*][1]][NBFACE][2](bdry)', 'EQ', IZERO1, IDUM, &
                  NERR, LDUM)
            ENDIF
!NSYBCD(BB,3)
            DO 630 BB = 1, NSYB
               ITYPE = NSYBCD (BB, 2)
               IDUM (BB) = 1
               IF (MOD (ITYPE, 2) .EQ.0) IDUM (BB) = IDUM (BB) + NSYC ( &
                  ITYPE-1)
               IDUM (NSYB + BB) = IDUM (BB) + NSYC (ITYPE)
630         END DO
            CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', &
            & 'GE', IDUM, NSYBCD (1, 3) , NERR, LDUM)
            CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', &
            & 'LE', IDUM (NSYB + 1) , NSYBCD (1, 3) , NERR, LDUM)
!GBC
            DO 640 ICAT = 1, NSYC (1)
               CALL ALCHK (ERR, 2041, SPR, 1, NSED, ICAT, IUNDEF, 'GBC(sed,icat)' &
               &, 'GE', zero1, zero1 (1) , GBC (1, ICAT) , NERR, LDUM)
640         END DO
!ABC
            DO 650 ICAT = 1, NSYC (3)
               CALL ALCHK (ERR, 2042, SPR, 1, NSED, ICAT, IUNDEF, 'ABC(sed,icat)' &
               &, 'GE', zero1, zero1 (1) , ABC (1, ICAT) , NERR, LDUM)
650         END DO
!BBC
            DO 660 ICAT = 1, NSYC (3)
               CALL ALCHK (ERR, 2043, SPR, 1, NSED, ICAT, IUNDEF, 'BBC(sed,icat)' &
               &, 'GT', zero1, zero1 (1) , BBC (1, ICAT) , NERR, LDUM)
660         END DO
!SFB
            IF (NSYC (2) .GT.0) THEN
               IDUM (1) = SFB
               CALL ALCHKI (ERR, 2044, SPR, 1, 1, IUNDEF, IUNDEF, 'SFB', &
                  'GE', IZERO1, IDUM, NERR, LDUM)
            ENDIF
!SRB
            IF (NSYC (2) .GT.0) THEN
               IDUM (1) = SRB
               CALL ALCHKI (ERR, 2045, SPR, 1, 1, IUNDEF, IUNDEF, 'SRB', &
                  'GE', IZERO1, IDUM, NERR, LDUM)
            ENDIF
!
         ENDIF  ! End of boundary data processing (sufficient workspace)
      ENDIF
!
!
! 7. Epilogue
! -----------
!
700   IF (NERR.GT.0) CALL handle_error_processing(SPR)
!
   END SUBROUTINE SYERR2
!SSSSSS SUBROUTINE SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
   SUBROUTINE SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
      ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, HRF, &
      ZGRUND, QOC, IQ, JMIN, JSORT, LDUM)
!
!----------------------------------------------------------------------*
!
! Check for time-dependent errors in the WAT-SY interface.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR81
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  10.10.94  Version 3.4.1. File created 20.09.94.
!----------------------------------------------------------------------*
!
      INTEGER :: NEL, NELEE, NLF, NLFEE, NV, SPR
      INTEGER :: ICMREF (NELEE, 4, 2:3), ICMRF2 (NLFEE, 3, 2)
      INTEGER :: ISORT (NEL)
      DOUBLEPRECISION DTUZ
      DOUBLEPRECISION CLAI (NV), PLAI (NV), ARXL (NLFEE)
      DOUBLEPRECISION DRAINA (NLF + 1:NEL), PNETTO (NLF + 1:NEL)
      DOUBLEPRECISION HRF (NEL), ZGRUND (NEL), QOC (NELEE, 4), rdum(nelee)
!
! Workspace arguments
      INTEGER :: IQ (NEL), JMIN (NEL), JSORT (0:NEL + 1)
      LOGICAL :: LDUM (NELEE)
!
! Locals, etc
      INTEGER :: FATAL, ERR
      DOUBLEPRECISION TOL
      PARAMETER (FATAL = 1, ERR = 2, TOL = 1D-7)
!
      INTEGER :: FACE, FADJ, I, IADJ, IBR, IEL, IUNDEF, J, NELP, NERR, &
         P
      DOUBLEPRECISION FNQOUT, QADJ, QMIN
      DOUBLEPRECISION DUM1 (1)
!
!     * Water discharge rate
      FNQOUT (IEL, FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * Initialize local counter
      NERR = 0
!
!
! 1. Variables
! ------------
!
!DTUZ
      DUM1 (1) = DTUZ
      CALL ALCHK (ERR, 2046, SPR, 1, 1, IUNDEF, IUNDEF, 'DTUZ', 'GE', &
         zero1, zero1 (1) , DUM1, NERR, LDUM)
!
!
! 2. Vegetative State
! -------------------
!
!CLAI
      CALL ALCHK (ERR, 2047, SPR, 1, NV, IUNDEF, IUNDEF, 'CLAI(veg)', &
         'GE', zero1, zero1 (1) , CLAI, NERR, LDUM)
!PLAI
      CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
         'GE', zero1, zero1 (1) , PLAI, NERR, LDUM)
      CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
         'LE', ONE1, ZERO1 (1) , PLAI, NERR, LDUM)
!
!
! 3. Link State
! -------------
!
      IF (NLF.GT.0) THEN
!
!ARXL
         CALL ALCHK (ERR, 2049, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
         &'GE', zero1, zero1 (1) , ARXL, NERR, LDUM)
!
      ENDIF
!
!
! 4. Columnar State
! -----------------
!
!DRAINA
      CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'GE', zero1, zero1 (1) , DRAINA, NERR, LDUM)
! 10.10.94  Ought to fix WAT module so that we don't need TOL
      CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'LEa', PNETTO, TOL, DRAINA, NERR, LDUM)
!
!
! 5. Elemental State
! ------------------
!
!HRF
      CALL ALCHK (ERR, 2051, SPR, 1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', &
         'GEa', ZGRUND, ZERO1 (1) , HRF, NERR, LDUM)
!
!
! 6. Flux/Ordering
! ----------------
!
!ISORT & QOC
!     * Set JSORT = inverse of ISORT & initialize upper bound JMIN
!       (note that JSORT has overspill elements )
      NELP = NEL + 1
      DO 610 J = 0, NELP
         JSORT (J) = NELP
610   END DO
      DO 620 I = 1, NEL
         IEL = ISORT (I)
         J = MAX (0, MIN (IEL, NELP) )
         JSORT (J) = I
         JMIN (I) = NELP
620   END DO
!     * At this point any element not listed in ISORT has a JSORT
!       value of NELP, which is guaranteed to fail the test below
!     * Update JMIN (used as object of JSORT test) & set QOC status IQ
      DO 650 FACE = 1, 4
         DO IEL = 1, NEL
!           * innocent until proven guilty
            IQ (IEL) = 0
!           * non-discharge faces are ok
            IF (FNQOUT (IEL, FACE) .LE.ZERO1 (1) ) CYCLE
!                                              ^^^^^^^^
            IADJ = ICMREF (IEL, FACE, 2)
            IF (IADJ.GT.0) THEN
               FADJ = ICMREF (IEL, FACE, 3)
               QADJ = FNQOUT (IADJ, FADJ)
!              * do both elements discharge into the same face?
               IF (QADJ.GT.ZERO1 (1) ) IQ (IEL) = 1
!              * IEL must precede IADJ in the ISORT list
               JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL) )
            ELSEIF (IADJ.LT.0) THEN
               IBR = - IADJ
               QMIN = ONE1 (1)
               DO 630 P = 1, 3
                  IADJ = ICMRF2 (IBR, P, 1)
                  IF (IADJ.GT.0) THEN
                     FADJ = ICMRF2 (IBR, P, 2)
                     QADJ = FNQOUT (IADJ, FADJ)
                     QMIN = MIN (QADJ, QMIN)
                     IF (QADJ.LT.zero1 (1) ) THEN
!                       * IEL must precede IADJ in the ISORT list
                        JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL) )
                     ENDIF
                  ENDIF
630            END DO
!              * discharge from IEL has nowhere to go?
               IF (QMIN.GE.zero1 (1) ) IQ (IEL) = 2
            ENDIF
         END DO
!        * Check QOC status at this FACE for all elements
         CALL ALCHKI (ERR, 2052, SPR, 1, NEL, FACE, IUNDEF, &
            'status_of_QOC(iel,face)', 'EQ', IZERO1, IQ, NERR, LDUM)
650   END DO
!     * Check that each donor element listed in ISORT occurs before
!       each of its receptors, and that all elements are listed
      CALL ALCHKI (ERR, 2053, SPR, 1, NEL, IUNDEF, IUNDEF, &
         'position_in_ISORT(iel)', 'LTa', JMIN, JSORT (1) , NERR, LDUM)
!
!
! 7. Epilogue
! -----------
!
      IF (NERR.GT.0) THEN
!
         WRITE (SPR, 9100) 'DTUZ', DTUZ
         WRITE (SPR, 9100) 'CLAI[veg=1,...,NV]', CLAI
         WRITE (SPR, 9100) 'PLAI[veg=1,...,NV]', PLAI
         rdum(1:nlf)=ARXL(1:nlf)  !AD
         WRITE (SPR, 9100) 'ARXL[link=1,...,NLF]', (rdum (IEL) , IEL = 1, NLF)
         WRITE (SPR, 9100) 'DRAINA[col=NLF+1,...,NEL]', DRAINA
         WRITE (SPR, 9100) 'PNETTO[col=NLF+1,...,NEL]', PNETTO
         rdum(1:nel)=hrf(1:nel)  !AD
         WRITE (SPR, 9100) 'HRF[iel=1,...,NEL]', rdum(1:nel)
         WRITE (SPR, 9100) 'ZGRUND[iel=1,...,NEL]', ZGRUND
         WRITE (SPR, 9200) 'ISORT[iel=1,...,NEL]', ISORT
         WRITE (SPR, 9200) 'position_in_ISORT[iel=1,...,NEL]', (JSORT ( &
            IEL) , IEL = 1, NEL)
         DO 710 FACE = 1, 4
            WRITE (SPR, 9150) 'QOC[iel=1,...,NEL][face=', FACE, ']', &
               (QOC (IEL, FACE) , IEL = 1, NEL)
710      END DO
!
         CALL ERROR (ERR, 2003, SPR, 0, 0, 'Error(s) detected'//' while checking time-dependent WAT-SY interface')
!
      ENDIF
!
9100  FORMAT(1X,A,     ':'/1P,(8E10.2))
9150  FORMAT(1X,A,I1,A,':'/1P,(8E10.2))
9200  FORMAT(1X,A,     ':'/   (16I5  ))
!
   END SUBROUTINE SYERR3
!SSSSSS SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
   SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
      NTSOBK, ARXL, DCBEDO, DLS, FBETA, DRSED, HRF, PBSED, PLS, SOSDFN, &
      THSAT, ZGRUND, NTSOTP, ZBFULL, ARBDEP, ARXLOL, DCBED, DCBSED, &
      DDBSED, DRSO50, DWATOL, FETA, GINFD, GINFS, GNU, GNUBK, QSED, &
      DBFULL)
!
!----------------------------------------------------------------------*
!
!  To initialize/define, on the first SY pass, output, saved and static
!   variables.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR61
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  24.5.94  Version 3.4.1 by AB/RAH. File creation date 23.11.93.
!----------------------------------------------------------------------*
      INTEGER :: NEL, NELEE, NLF, NLFEE, NS, NSED, NSEE, NSEDEE
      INTEGER :: NTSOBK (NLFEE), NTSOTP (NLF + 1:NEL)
      DOUBLEPRECISION ARXL (NLFEE), DCBEDO, DLS (NEL), DRSED (NSED)
      DOUBLEPRECISION FBETA (NELEE, NSED), HRF (NLF + 1:NEL), PBSED ( &
         NLFEE)
      DOUBLEPRECISION PLS (NLF + 1:NEL), SOSDFN (NSEE, NSED), THSAT (NS)
      DOUBLEPRECISION ZBFULL (NLFEE), ZGRUND (NEL)

!
! Output arguments
      DOUBLEPRECISION ARBDEP (NLFEE), ARXLOL (NLFEE), DBFULL (NLFEE)
      DOUBLEPRECISION DCBED (NLFEE), DCBSED (NLFEE, NSED)
      DOUBLEPRECISION DDBSED (NLFEE, NSED), DRSO50 (NS), DWATOL (NLF + &
         1:NEL)
      DOUBLEPRECISION FETA (NEL), GINFD (NLFEE, NSED), GINFS (NLFEE, &
         NSED)
      DOUBLEPRECISION GNU (NLF + 1:NEL), GNUBK (NLFEE), QSED (NELEE, &
         NSEDEE, 4)
!
! Locals, etc
!
      DOUBLEPRECISION DCBEDE, DDBEDE, DLSE, FBETAE
      INTEGER :: IEL, LINK, SED, SOIL, FACE
!
!
!----------------------------------------------------------------------*
!
!
!     * Initialize surface erosion rates in each column
      CALL ALINIT (ZERO, NEL - NLF, GNU (NLF + 1) )
!
      IF (NLF.GT.0) THEN
!
!        * Initialize bank erosion rates in each link
         CALL ALINIT (ZERO, NLF, GNUBK)
!
!        * Zero bed sediment accumulator
         CALL ALINIT (ZERO, NLF, ARBDEP)
!
!        * Set old river c/s area equal to current river c/s area
         CALL DCOPY (NLF, ARXL, 1, ARXLOL, 1)
!
      ENDIF
!
!
!     * Loop over sediment types
      DO 200 SED = 1, NSED
!
         IF (NLF.GT.0) THEN
!
!           * Initialize infiltration rates
            CALL ALINIT (ZERO, NLF, GINFD (1, SED) )
            CALL ALINIT (ZERO, NLF, GINFS (1, SED) )
!
         ENDIF
!
!        * Initialize sediment flow rates
         DO 100 FACE = 1, 4
            CALL ALINIT (ZERO, NEL, QSED (1, SED, FACE) )
100      END DO
!
!     * Next sediment type
200   END DO
!
!
!     * Loop over links
      DO 400 LINK = 1, NLF
         DLSE = DLS (LINK)
!
!        * Set ratio of bank soil to bed sediment solid volume fractions
         FETA (LINK) = (1 - THSAT (NTSOBK (LINK) ) ) / (1 - PBSED (LINK) &
            )
!
!        * Set bank full depth
         DBFULL (LINK) = ZBFULL (LINK) - ZGRUND (LINK)
!
!        * Bed layer depths
         DCBEDE = MIN (DLSE, DCBEDO)
         DDBEDE = DIMJE(DLSE, DCBEDE)
         DCBED (LINK) = DCBEDE
!
!        * Loop over sediment types
         DO 300 SED = 1, NSED
!
!           * Initialize sediment depths in both bed layers
            FBETAE = FBETA (LINK, SED)
            DCBSED (LINK, SED) = DCBEDE * FBETAE
            DDBSED (LINK, SED) = DDBEDE * FBETAE
!
!        * Next sediment type
300      END DO
!
!     * Next link
400   END DO
!
!
!     * Loop over column elements
      DO 500 IEL = NLF + 1, NEL
!
!        * Set ratio: surface soil to loose sediment solid vol fractions
         FETA (IEL) = (1 - THSAT (NTSOTP (IEL) ) ) / (1 - PLS (IEL) )
!
!        * Calculate initial surface water depth
         DWATOL (IEL) = HRF (IEL) - ZGRUND (IEL)
!
500   END DO
!
!
!     * Calculate median particle diameter for each soil type
      DO 600 SOIL = 1, NS
         DRSO50 (SOIL) = SYDR (HALF, NSEE, NSED, SOSDFN (SOIL, 1), &
            DRSED)
600   END DO
!
!
   END SUBROUTINE SYINIT
!SSSSSS SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, &
   SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, &
      NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, NSYCEE, NTSOTP, NV, &
      NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC, ALPHA, BBC, BKB, CONCOB, &
      DCBEDO, DLS, DRDRIP, DRSED, DLSMAX, FBETA, FBIC, FCG, FCROCK, &
      FDEL, FDRIP, FICRIT, FPCLAY, FPCRIT, GBC, GKF, GKR, ISACKW, &
      ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSED, NSYB, NSYBCD, &
      NSYC, NTSOBK, PBSED, PLS, RHOSO, SOSDFN, XDRIP, IDUM, DUMMY, &
      DUMSED)
!
!----------------------------------------------------------------------*
!
!  Read SY data input file
!
!----------------------------------------------------------------------*
! Version:  3.4.1         Notes:  SSR75
!  Module:  SY          Program:  SHETRAN
! Modifications:
!  RAH  08.06.94  Version 3.4.1 by AB/RAH. File created 09.12.93.
!  BTL  25.04.95  Version 3.4.1 : read in DLSMAX as second item in SY12
!----------------------------------------------------------------------*
!
! NB: Don't dimension arrays with NSED (undefined) or NLF (may be 0).
!
! Input arguments
      INTEGER :: NEL, NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, &
         NSYCEE
      INTEGER :: NTSOTP (NLF + 1:NEL), NV, NX, NXEE, NYEE, NY, SYD, SPR
      INTEGER :: ICMBK (NLFEE, 2), ICMREF (NELEE, 4, 2:2), ICMXY (NXEE, &
         NY)
      LOGICAL :: BEXBK, LINKNS (NLFEE)
      CHARACTER (LEN=*) :: SYVER
!
! Output arguments
      INTEGER :: ISACKW, ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE
      INTEGER :: NSED, NSYB, NSYBCD (NSYBEE, 3), NSYC (4), NTSOBK ( &
         NLFEE)
      DOUBLEPRECISION ABC (NSEDEE, NSYCEE), ALPHA, BBC (NSEDEE, NSYCEE)
      DOUBLEPRECISION BKB (NS), CONCOB, DCBEDO, DLS (NEL), DRDRIP (NV)
      DOUBLEPRECISION DRSED (NSEDEE), FBETA (NELEE, NSEDEE), FBIC
      DOUBLEPRECISION FCG (NLF + 1:NEL), FCROCK (NLF + 1:NEL)
      DOUBLEPRECISION FDEL (NELEE, NSEDEE), FDRIP (NV), FICRIT
      DOUBLEPRECISION FPCLAY (NS), FPCRIT, GBC (NSEDEE, NSYCEE), &
         GKF (NS)
      DOUBLEPRECISION GKR (NS), PBSED (NLFEE), PLS (NLF + 1:NEL), &
         RHOSO (NS)
      DOUBLEPRECISION SOSDFN (NSEE, NSEDEE), XDRIP (NV)
      DOUBLEPRECISION DLSMAX
!
! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE) :: IDUM
      DOUBLEPRECISION DUMMY (NELEE), DUMSED (NLFEE * NSEDEE)
!
! Locals, etc
      INTEGER :: FATAL, WARN
      PARAMETER (FATAL = 1, WARN = 3)
!
      CHARACTER(80)  :: CDUM
      CHARACTER(132) :: MSG
      CHARACTER(8)   ::  SYDVER
      INTEGER :: BB, IDUM0, I0, IEL, ICAT, ITYPE, NC, NUM_CATEGORIES_TYPES,  NNN, NREQ, &
         SED, SOIL
!
!----------------------------------------------------------------------*
!
!
! 0. Preliminaries
! ----------------
!
!     * Check status of data file
      CALL ALREAD (0, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
!
!     * Print SY job title
      CALL ALREAD (1, SYD, SPR, ':SY01', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
      WRITE (SPR, '(/1X,A/)') CDUM
!
!     * Check & print version number
      CALL ALREAD (1, SYD, SPR, ':SY02', 1, 1, IDUM0, SYDVER, IDUM, &
         DUMMY)
!     * [miss off last character to allow eg '3.4.1' is ok in '3.4.1a' ]
      IF (INDEX (SYDVER, SYVER (:LEN (SYVER) - 1) ) .EQ.0) THEN
         WRITE (MSG, 9011) SYVER, SYDVER
         CALL ERROR (WARN, 2011, SPR, 0, 0, MSG)
      ELSE
         WRITE (SPR, '(4X,2A/)') 'SY Module Version ', SYVER
      ENDIF
!
!
! 1. Static Variables
! -------------------
!
!     * Check workspace array size: part 1
      NREQ = 8
      IF (NELEE.LT.NREQ) CALL handle_insufficient_workspace(NELEE, NREQ, SPR)
!
!     * Integer
      NNN = 5
      IF (NLF.GT.0) NNN = 8
      CALL ALREAD (2, SYD, SPR, ':SY11', NNN, 1, IDUM0, CDUM, IDUM, &
         DUMMY)
      NSED = IDUM (1)
      ISGSED = IDUM (2)
      ISTEC = IDUM (3)
      ISSYOK = IDUM (4)
      NEPS = IDUM (5)
      IF (NLF.GT.0) THEN
         ISACKW = IDUM (6)
         ISUSED = IDUM (7)
         NFINE = IDUM (8)
      ENDIF
      IF (NSED.LT.1.OR.NSED.GT.NSEDEE) CALL handle_invalid_nsed(NSED, NSEDEE, SPR)
!
!     * Floating-point
      NNN = 2
      IF (NLF.GT.0) NNN = 7
      CALL ALREAD (3, SYD, SPR, ':SY12', NNN, 1, IDUM0, CDUM, IDUM, &
         DUMMY)
      FPCRIT = DUMMY (1)
      DLSMAX = DUMMY (2)
      IF (NLF.GT.0) THEN
         ALPHA = DUMMY (3)
         CONCOB = DUMMY (4)
         DCBEDO = DUMMY (5)
         FBIC = DUMMY (6)
         FICRIT = DUMMY (7)
      ENDIF
!
!
! 2. Sediment, Soil & Vegetation Properties
! -----------------------------------------
!
!     * Check workspace array size: part 2
      NREQ = MAX (MAX (5, NSED) * NS, 3 * NV)
      IF (NELEE.LT.NREQ) CALL handle_insufficient_workspace(NELEE, NREQ, SPR)
!
!     * Sediment
      CALL ALREAD (3, SYD, SPR, ':SY21', NSED, 1, IDUM0, CDUM, IDUM, &
         DRSED)
!
!     * Soil
      CALL ALREAD (3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, IDUM, &
         DUMMY)
      CALL DCOPY (NS, DUMMY (1), 5, GKR, 1)
      CALL DCOPY (NS, DUMMY (2), 5, GKF, 1)
      CALL DCOPY (NS, DUMMY (3), 5, RHOSO, 1)
      CALL DCOPY (NS, DUMMY (4), 5, FPCLAY, 1)
      CALL DCOPY (NS, DUMMY (5), 5, BKB, 1)
!
!     * Soil composition
      CALL ALREAD (3, SYD, SPR, ':SY23', NSED, NS, IDUM0, CDUM, IDUM, &
         DUMMY)
      DO 200 SED = 1, NSED
         CALL DCOPY (NS, DUMMY (SED), NSED, SOSDFN (1, SED), 1)
200   END DO
!
!     * Vegetation
      CALL ALREAD (3, SYD, SPR, ':SY24', 3, NV, IDUM0, CDUM, IDUM, &
         DUMMY)
      CALL DCOPY (NV, DUMMY (1), 3, XDRIP, 1)
      CALL DCOPY (NV, DUMMY (2), 3, DRDRIP, 1)
      CALL DCOPY (NV, DUMMY (3), 3, FDRIP, 1)
!
!
! 3. Link Element Properties
! --------------------------
!
      IF (NLF.GT.0) THEN
!
!        * Bank soil type
         CALL ALREAD (2, SYD, SPR, ':SY31', NLF, 1, IDUM0, CDUM, NTSOBK, &
            DUMMY)
!
!        * Porosity of bed sediment
         CALL ALREAD (3, SYD, SPR, ':SY32', NLF, 1, IDUM0, CDUM, IDUM, &
            PBSED)
!
      ENDIF
!
!
! 4. Column-element Properties
! ----------------------------
!
!     * Ground cover
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY41', NEL, NLF, NX, NY, NELEE, &
         NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  FCG, IDUM, &
         DUMMY)
!
!     * Rock cover
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY42', NEL, NLF, NX, NY, NELEE, &
         NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  FCROCK, &
         IDUM, DUMMY)
!
!     * Porosity of loose sediment
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY43', NEL, NLF, NX, NY, NELEE, &
         NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  PLS, IDUM, &
         DUMMY)
!
!
! 5. All-element Initialization
! -----------------------------
!
!     * Initial depth of loose/bed sediment
      CALL ALALLF (0, 1, 0, SYD, SPR, ':SY51', NEL, NLF, NX, NY, NELEE, &
         NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  DLS, IDUM, &
         DUMMY)
!
!     * Initial composition of loose/bed sediment ...
      CALL ALALLF (0, NSED, - 1, SYD, SPR, ':SY52', NEL, NLF, NX, NY, &
         NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  &
         FBETA, IDUM, DUMMY)
!
!     ... with special option to inherit composition of soil
      IF (NUM_CATEGORIES_TYPES .LT.0) THEN
         DO 510 IEL = 1, NLF
            SOIL = NTSOBK (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), &
               NELEE)
510      END DO
         DO 520 IEL = NLF + 1, NEL
            SOIL = NTSOTP (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), &
               NELEE)
520      END DO
      ENDIF
!
!     * Initial concentrations of suspended sediment
      CALL ALALLF (0, NSED, 0, SYD, SPR, ':SY53', NEL, NLF, NX, NY, &
         NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  &
         FDEL, IDUM, DUMMY)
!
!
! 6. Boundary Data
! ----------------
!
!     * (see workspace check above)
!
!     * No of inflow boundary elements & no of categories of each type
      CALL ALREAD (2, SYD, SPR, ':SY61', 5, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSYB = IDUM (1)
      DO 600 ITYPE = 1, 4
         NSYC (ITYPE) = IDUM (1 + ITYPE)
600   END DO
!
      IF (NSYB.GT.0) THEN
!
         IF (NSYB.GT.NSYBEE) CALL handle_nsyb_too_large(NSYB, NSYBEE, SPR)
!
!        * Check workspace array size: part 3
         NREQ = MAX (3 * NSYB, NSED * NSYC (1), NSED * 2 * NSYC (3) )
         IF (NELEE.LT.NREQ) CALL handle_insufficient_workspace(NELEE, NREQ, SPR)
!
!        * Integer boundary data
         CALL ALREAD (2, SYD, SPR, ':SY62', 3, NSYB, IDUM0, CDUM, IDUM, &
            DUMMY)
         I0 = 0
         DO 610 BB = 1, NSYB
            IEL = IDUM (I0 + 1)
            ITYPE = IDUM (I0 + 2)
            ICAT = IDUM (I0 + 3)
            IF (ITYPE.LT.1.OR.ITYPE.GT.4) CALL handle_invalid_boundary_type(BB, ITYPE, SPR)
!           * condense 4 into 2 by adding cats 2 & 4 to lists for 1 & 3
            IF (MOD (ITYPE, 2) .EQ.0) ICAT = ICAT + NSYC (ITYPE-1)
            NSYBCD (BB, 1) = IEL
            NSYBCD (BB, 2) = ITYPE
            NSYBCD (BB, 3) = ICAT
            I0 = I0 + 3
610      END DO
!
!        * Steady flux data
         NC = NSYC (1)
         IF (NC.GT.0) THEN
            IF (NC.GT.NSYCEE) CALL handle_nsyc1_too_large(NC, NSYCEE, SPR)
            CALL ALREAD (3, SYD, SPR, ':SY63', NSED, NC, IDUM0, CDUM, &
               IDUM, DUMMY)
            DO 620 SED = 1, NSED
               CALL DCOPY (NC, DUMMY (SED), NSED, GBC (SED, 1), NSEDEE)
620         END DO
         ENDIF
!
!        * Steady rating curve data
         NC = NSYC (3)
         IF (NC.GT.0) THEN
            IF (NC.GT.NSYCEE) CALL handle_nsyc3_too_large(NC, NSYCEE, SPR)
            CALL ALREAD (3, SYD, SPR, ':SY64', NSED * 2, NC, IDUM0, &
               CDUM, IDUM, DUMMY)
            DO 630 SED = 1, NSED
               CALL DCOPY (NC, DUMMY (2 * SED-1), 2 * NSED, ABC (SED, 1) &
                  , NSEDEE)
               CALL DCOPY (NC, DUMMY (2 * SED), 2 * NSED, BBC (SED, 1), &
                  NSEDEE)
630         END DO
         ENDIF
!
      ENDIF
!
!
! 7. Epilogue
! -----------
!
!     * Close the data file
      CALL ALREAD ( - 1, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, &
         DUMMY)
!
      RETURN
!
!
! Error Branches & Formats
! ------------------------
!
! Format statements
! -----------------
!
9003  FORMAT ( 1X,A )
!
9011  FORMAT ('SY module is version ',A,'; SYD data file is version ',A)
!
!
   END SUBROUTINE SYREAD

END MODULE sediment_initialization
