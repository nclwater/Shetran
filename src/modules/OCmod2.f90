MODULE OCmod2
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces part of the OC.F files
   USE SGLOBAL
!!***ZQ Module 200520
   USE ZQmod,     ONLY : get_ZQTable_value
   USE AL_D,      ONLY : ZQweirsill,ZQTableRef
   IMPLICIT NONE

   DOUBLEPRECISION, PARAMETER   :: F23=2.0D0/3.0D0,      &
      F53=5.0D0/3.0D0,      &
      DZMIN = 1.0D-3,       &
      RDZMIN=3.16227766d-2, & !(=sqrt(dzmin))
      H23MIN=1.0d-2,        & !(=DZMIN^^F23)
      ROOT2G = 4.42944d0      !=sqrt(2x9.81)
   DOUBLEPRECISION, DIMENSION(NELEE)          :: HRFZZ    !water surface elevation - here for data abstraction AD
   DOUBLEPRECISION, DIMENSION(NELEE,4)        :: qsazz    !discharge elevation - here for data abstraction AD

! sb 121212
!DOUBLEPRECISION, DIMENSION(3,NXSCEE,NLFEE) :: xstab
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: xstab

   PRIVATE
   PUBLIC :: GETHRF, SETHRF, GETQSA, SETQSA, GETQSA_ALL, CONVEYAN, OCQBC, OCQMLN, OCQLNK, OCQGRD, OCQBNK, OCFIX, XSTAB, &
      hrfzz, qsazz, OCNODE, initialise_ocmod  !THESE PUBLIC ONLY FOR USE IN AD
CONTAINS

   !FFFFFF DOUBLE PRECISION FUNCTION gethrf
   !--------------------------------------------------------------------*
   ! Retrieves the value of the global hrfzz array at index i.
   !--------------------------------------------------------------------*
   PURE DOUBLE PRECISION FUNCTION gethrf(i)
      
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: i
      
      gethrf = hrfzz(i)
      
   END FUNCTION gethrf

   !SSSSSS SUBROUTINE sethrf
   !--------------------------------------------------------------------*
   ! Sets the value of the global hrfzz array at index i.
   !--------------------------------------------------------------------*
   SUBROUTINE sethrf(i, v)
      
      IMPLICIT NONE
      
      INTEGER, INTENT(IN)          :: i
      DOUBLE PRECISION, INTENT(IN) :: v
      
      hrfzz(i) = v
      
   END SUBROUTINE sethrf


   !FFFFFFR DOUBLE PRECISION FUNCTION getqsa
   !--------------------------------------------------------------------*
   ! Retrieves the value of the global qsazz array at coordinates (i,j).
   !--------------------------------------------------------------------*
   PURE DOUBLE PRECISION FUNCTION getqsa(i, j)
      
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: i, j
      
      getqsa = qsazz(i, j)
      
   END FUNCTION getqsa

   !SSSSSS SUBROUTINE setQSA
   !--------------------------------------------------------------------*
   ! Sets the value of the global qsazz array at coordinates (i,j).
   !--------------------------------------------------------------------*
   SUBROUTINE setqsa(i, j, v)
      
      IMPLICIT NONE
      
      INTEGER, INTENT(IN)          :: i, j
      DOUBLE PRECISION, INTENT(IN) :: v
      
      qsazz(i, j) = v
      
   END SUBROUTINE setqsa


   !FFFFFFR DOUBLE PRECISION FUNCTION getqsa_all
   !--------------------------------------------------------------------*
   ! Retrieves an entire block (rows 1 to n, all 4 columns) of the 
   ! global qsazz array.
   !--------------------------------------------------------------------*
   PURE FUNCTION getqsa_all(n) RESULT(res)
      
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: n
      DOUBLE PRECISION, DIMENSION(n, 4) :: res
      
      res = qsazz(1:n, :)
      
   END FUNCTION getqsa_all

   !SSSSSS SUBROUTINE initialise_ocmod
   !--------------------------------------------------------------------*
   ! Initializes the Over-Catchment (OC) module by dynamically 
   ! allocating the xstab cross-section table.
   !--------------------------------------------------------------------*
   SUBROUTINE initialise_ocmod()
      
      IMPLICIT NONE

      ! print*, nxscee, total_no_links
      
      ! Modernization Fix: Prevent crashes if initialized more than once
      IF (.NOT. ALLOCATED(xstab)) THEN
         ALLOCATE(xstab(3, nxscee, total_no_links))
      END IF
      
      ! print*, 'here'
      
   END SUBROUTINE initialise_ocmod



   !SSSSS SUBROUTINE OCNODE
   SUBROUTINE OCNODE(IELA, ZI, CI, DI, ROOTLI, QJ)
   !----------------------------------------------------------------------*
   ! CALCULATES FLOWS OUT OF NODE AS FUNCTION OF ADJACENT WATER ELEVATIONS
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCNODE/4.27
   ! Modifications:
   !  GP           3.4  Call ERROR & terminate iterations if nc.eq.50.
   !                    Add argument ZNODE (see OCQMLN).
   ! RAH  980212   4.2  Supply missing PRI,FATAL,WARN, to pass to ERROR;
   !                    also remove argument ZNODE (see OCQMLN).
   !                    Explicit typing.  Locals: scrap TESTZ; add TEST.
   !                    Generic intrinsics.  Description: OUT OF, not INTO.
   !                    RETURN immediately if FA=0 (don't overwrite QJ).
   !                    Test NC BEFORE updating A or B.
   !                    Set QJ at absent branches (was in OCQMLN).
   !       980220       Add argument IEL to pass to ERROR (see OCQMLN).
   !       980318       Add argument DI to pass to FNODE (see OCQMLN).
   ! SB    990204 4.27  Problem with conserving mass at junctions
   !                    Adjust QJ so the largest absolute value at a
   !                    junction is modified so that the asum = 0
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : ISZERO, GTZERO
      ! USE OC_MODULE, ONLY : zero, half

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IELA
      DOUBLE PRECISION, INTENT(IN) :: CI(0:3), DI(0:3), ZI(0:3), ROOTLI(0:3)

      ! Input/Output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: QJ(0:3)

      ! Locals
      INTEGER :: J, NC, JMAJOR
      DOUBLE PRECISION :: A, B, FA, FB, FN, FNM1, SIGMAQ, WN
      LOGICAL :: TEST, FAILED

   !----------------------------------------------------------------------*

   ! FIRST GUESSES (CHOOSE VALUES A,B SUCH THAT F(A)*F(B) .le. 0 )
   ! (USE MIN AND MAX OF VALID ELEVATIONS); also, set QJ at absent branches

      A = ZI(0)
      B = A
      
      init_loop: DO J = 1, 3
         IF (ISZERO(ROOTLI(J))) THEN
            QJ(J) = ZERO
         ELSE
            A = MIN(ZI(J), A)
            B = MAX(ZI(J), B)
         END IF
      END DO init_loop
      
      CALL FNODE(A, DI, CI, ZI, ROOTLI, QJ, FA)
      IF (ISZERO(FA)) RETURN
      
      CALL FNODE(B, DI, CI, ZI, ROOTLI, QJ, FB)
      IF (ISZERO(FB)) RETURN

   ! Iterate to convergence, using successive linear interpolation
   
      FN = FA
      FAILED = .FALSE.
      
      ! * Start of iteration loop: set new point WN and calculate FN
      iteration_loop: DO NC = 1, 50
         
         WN = (A * FB - B * FA) / (FB - FA)
         FNM1 = FN
         
         CALL FNODE(WN, DI, CI, ZI, ROOTLI, QJ, FN)
         
         IF (ISZERO(FN)) THEN   ! * Test for convergence (either exact or approximate)
            FAILED = .TRUE.
            EXIT iteration_loop
         END IF
         
         SIGMAQ = ABS(QJ(0)) + ABS(QJ(1)) + ABS(QJ(2)) + ABS(QJ(3))
         
         IF (ABS(FN) <= SIGMAQ * 1.0D-2 .AND. ABS(B - A) <= 1.0D-3) THEN
            JMAJOR = 0
            DO J = 1, 3
               IF (ABS(QJ(J)) > ABS(QJ(JMAJOR))) JMAJOR = J
            END DO
            QJ(JMAJOR) = QJ(JMAJOR) - FN
            EXIT iteration_loop
         END IF
         
         ! * ... carry on: replace either A or B with WN; and
         ! * adjust interpolation factor if sign of F didn't change
         TEST = GTZERO(FN * FNM1)  ! TAKE CARE - PRECEDENCE
         
         IF (FN * FA >= 0.0D0) THEN
            A = WN
            FA = FN
            IF (TEST) FB = FB * HALF
         ELSE
            B = WN
            FB = FN
            IF (TEST) FA = FA * HALF
         END IF
         
      END DO iteration_loop

      ! IF(failed) THEN
      !    IF (ABS(FN) < SIGMAQ * 1.0D-1 .AND. ABS(B - A) < 5.0D-2) THEN
      !       !CALL ERROR(WWWARN, 1027, PPPRI, iela, 0, 'maximum iterations exceeded for OC confluence')
      !    ELSE
      !       !CALL ERROR(FFFATAL, 1028, PPPRI, iela, 0, 'iteration failure for OC confluence')
      !    ENDIF
      ! ENDIF

   END SUBROUTINE OCNODE



   !SSSSSS SUBROUTINE FNODE
   PURE SUBROUTINE FNODE(ZNODE, DI, CI, ZI, ROOTLI, QJ, RESFNODE)
   !----------------------------------------------------------------------*
   ! CALCULATES THE FUNCTION FNODE(ZNODE) = sum OF FLOWS OUT OF A NODE
   ! NB. ROOTLI IS USED AS A FLAG FOR NON-EXISTENT LINKS
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/FNODE/4.2
   ! Modifications:
   ! RAH  980211  4.2  Explicit typing.
   !                   Generic intrinsics.  Replace ci2 array with CMAX.
   !                   Integer DELTA.  New locals CJ,DJ,Q,Qasum.
   !      980212       Scrap local array CI3.  Extend IF block in loop 100.
   !      980318       Set downstream CJ using ZNODE (had (CI(J)+CMAX)/2);
   !                   add argument DI (see OCNODE).
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : ISZERO
      ! USE OC_MODULE, ONLY : ZERO, ONE

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: ZNODE
      DOUBLE PRECISION, INTENT(IN) :: DI(0:3), CI(0:3), ZI(0:3), ROOTLI(0:3)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: QJ(0:3), RESFNODE
      ! NB: QJ(J) is output, but only for those J with ROOTLI(J).ne.0

      ! Locals
      INTEGER :: J
      DOUBLE PRECISION :: CJ, DZ, QASUM, SIG

   !----------------------------------------------------------------------*

      QASUM = ZERO
      QJ = ZERO
      
      flow_loop: DO J = 0, 3
         IF (ISZERO(ROOTLI(J))) CYCLE flow_loop
         
         DZ = ZNODE - ZI(J)
         SIG = SIGN(ONE, DZ)
         CJ = CI(J) + DI(J) * MAX(ZERO, DZ)
         QJ(J) = SIG * CJ * SQRT(SIG * DZ) / ROOTLI(J)
         QASUM = QJ(J) + QASUM
      END DO flow_loop
      
      RESFNODE = QASUM
      
   END SUBROUTINE FNODE


   
   !SSSSSS SUBROUTINE OCCODE
   !----------------------------------------------------------------------*
   !  CALCULATE CONVEYANCE AND DERIVATIVE FOR A CHANNEL LINK.
   !----------------------------------------------------------------------*
   PURE SUBROUTINE OCCODE(ZG, STR, AFROMCWIDTH, AFROMXAFULL, AFROMXSTYPES, Z, CONV, DERIV)
   ! Version:  SHETRAN/OC/OCCODE/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980423  4.2  Explicit typing.  Argument ZG before Z.
   !                   New input args STR (was local), XAFULL, XS replace
   !                   IEL & common STRX, XAREA/NXSECT, XSECT/XCONV/XDERIV.
   !                   Move NXSCEE, CWIDTH from SPEC.OC/AL to arg-list.
   !                   (See callers OCQBC, OCQLNK, OCQMLN.)
   !                   Replace: Z-ZBFULL(IEL) with H-XS(1,NXSCEE); GOTO
   !                   with IF; search for I (DO-loop) with direct calc.
   !                   Rearrange expressions for CONV,DERIV (I.ge.NXSCEE).
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  Z.ge.ZG    [STR,CWIDTH,XAFULL,XS(1,NXSCEE)].gt.0    NXSCEE.ge.1
   !  for i in 1:NXSCEE-1  XS(1,i)=XS(1,NXSCEE)*(i-1)/(NXSCEE-1)
   !                       XS(2,i).ge.0    XS(3,i).gt.0
   ! Exit conditions:          CONV.ge.0     DERIV.gt.0
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! USE OC_MODULE, ONLY : NXSCEE, ONE
      ! USE UTILSMOD, ONLY : DIMJE

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: ZG, STR, AFROMCWIDTH, AFROMXAFULL, Z
      DOUBLE PRECISION, INTENT(IN) :: AFROMXSTYPES(3, NXSCEE)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CONV, DERIV
      
      ! Locals
      INTEGER :: I
      DOUBLE PRECISION :: H, HFULL, XA

   !----------------------------------------------------------------------*
      
      H = Z - ZG
      HFULL = AFROMXSTYPES(1, NXSCEE)

      I = INT((H / HFULL) * DBLE(NXSCEE - 1) + ONE)

      IF (I < NXSCEE) THEN
         ! * use look-up tables
         DERIV = AFROMXSTYPES(3, I)
         CONV  = AFROMXSTYPES(2, I) + DERIV * DIMJE(H, AFROMXSTYPES(1, I))
      ELSE
         ! * calculate values directly
         XA = AFROMXAFULL + AFROMCWIDTH * DIMJE(H, HFULL)
         CALL CONVEYAN(STR, H, CONV, DERIV, 2, XA, AFROMCWIDTH)
      END IF
      
   END SUBROUTINE OCCODE



   !SSSSSS SUBROUTINE OCQBC
   SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, AFROMXAFULL, LINK, AFROMCOCBCD, ZI, AFROMHOCNOW, AFROMQOCF, FROMQ, FROMDQ)
   !----------------------------------------------------------------------*
   !
   !  CALCULATE FLOW AND DERIVATIVE AT AN EXTERNAL BOUNDARY
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCQBC/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980225  4.2  Replace INCLUDEs with arguments (see OCQDQ);
   !                   also scrap INDEX,NCODE and redundant DDDZ,
   !                   and reduce COCBCD dimension by 1 (see SPEC.OC).
   !                   Explicit typing.  Merge types 10 & 4.
   !                   Set DQ* to zero if not defined.  Generic intrinsics.
   !                   Rewrite polynomial expressions without ^^.
   !                   Merge type 8 into 7.or.8, and scrap HU.
   !      980226       Array COEFF, & don't initialize Q,DQU/L (see QWEIR).
   !                   Use AH for A*H.
   !                   Define PARAMETER RDZMIN.  Zero outputs by default.
   !      980409       Arguments (see OCQDQ): scrap output DQI; add inputs
   !                   W,STR,HOCNOW & re-order.  Rename CONV,DERIV,DHF,ZG,
   !                   ZU,DQ0,ZL CONVM,DERIVM,LI,ZGI,ZI,DQ,ZX.
   !                 ! Add NTYPE 3/9 (prescribed head).  4th root RDZMIN.
   !                 ! Fix error in sign of Q,DQ for MTYPE=5 & NTYPE=8.
   !                 ! Use CONVMM in DUM; & use ROOTDZ in 1st DQ term.
   !      980416       Allow ZI.lt.ZX in call to QWEIR.
   !      980427       Input arguments (see OCQDQ): remove IEL,IFACE;
   !                   add NXSCEE,XAFULL,XSTAB.  OCCODE args: remove IEL;
   !                   add NXSCEE,STR,W,XAFULL,XSTAB; move ZI.
   !      980730       Protect against 0^^F23.
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  NXSCEE.ge.2    LI.gt.0    if {7.le.NTYPE.le.8} COCBCD(1:2).ge.0
   !  if {NTYPE.eq.8.or.MOD(NTYPE,6).eq.3.and.NTYPE.ne.3}
   !      ZI.ge.ZGI    [STR,W,XAFULL,XSTAB(1,NXSCEE)].gt.0
   !      for i in 1:NXSCEE-1   XSTAB(1,i)=XSTAB(1,NXSCEE)*(i-1)/(NXSCEE-1)
   !                            XSTAB(2,i).ge.0    XSTAB(3,i).gt.0
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : DIMJE
      ! USE OC_MODULE, ONLY : ZERO, ONE, HALF, DZMIN, RDZMIN, xstab

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NTYPE, LINK
      DOUBLE PRECISION, INTENT(IN) :: LI, ZGI, STR, W, AFROMXAFULL, ZI, AFROMHOCNOW, AFROMQOCF
      DOUBLE PRECISION, INTENT(IN) :: AFROMCOCBCD(5)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: FROMQ, FROMDQ

      ! Locals
      INTEGER :: MTYPE
      DOUBLE PRECISION :: AH, B, C, D, DERIVM, DHH, DQU, DUM, DZ, E
      DOUBLE PRECISION :: H, HM, ROOTDZ, ROOTL
      DOUBLE PRECISION :: SIG, STRW, SUBRIO, ZSILL, ZL, ZU, ZX, COEFF(2)
      DOUBLE PRECISION :: CONVM, CONVMM

   !----------------------------------------------------------------------*

   ! Prologue
   ! --------
   ! Modernization Fix: Default initialize outputs to zero to prevent passing back uninitialized garbage
      FROMQ = ZERO
      FROMDQ = ZERO
      MTYPE = MOD(NTYPE, 6)

   ! Part 1
   ! ------
      SELECT CASE (MTYPE)
         ! Prescribed time-varying head - grid (3) or channel (9)
         ! NB: see Part 2
         CASE (3)
            ZX = AFROMHOCNOW
            FROMQ = ZERO
            FROMDQ = ZERO

         ! Prescribed time-varying flow - grid (4) or channel (10)
         ! NB: QOCF is rate of INFLOW, not discharge
         CASE (4)
            FROMQ = AFROMQOCF
            FROMDQ = ZERO

         ! Flow a polynomial function of head - grid (5) or channel (11)
         CASE (5)
            H = ZI - ZGI
            AH = AFROMCOCBCD(1) * H
            B = AFROMCOCBCD(2)
            C = AFROMCOCBCD(3)
            D = AFROMCOCBCD(4)
            E = AFROMCOCBCD(5)
            
            FROMQ = -((((AH + B) * H + C) * H + D) * H + E)
            FROMDQ = -(((4.0D0 * AH + 3.0D0 * B) * H + 2.0D0 * C) * H + D)
            
         CASE DEFAULT
            ! Weir (7) ... with river in parallel (8) - see Part 2
            IF (NTYPE == 7 .OR. NTYPE == 8) THEN
               COEFF(1) = AFROMCOCBCD(1)
               SUBRIO   = AFROMCOCBCD(2)
               ZSILL    = AFROMCOCBCD(3)
               ZX       = AFROMCOCBCD(4)
               COEFF(2) = COEFF(1)
               
               ZU = MAX(ZX, ZI)
               ZL = MIN(ZX, ZI)
               
               CALL QWEIR(ZU, ZSILL, ZL, COEFF, SUBRIO, FROMQ, DQU, FROMDQ)
               
               IF (ZI >= ZX) THEN
                  FROMQ = -FROMQ
                  FROMDQ = -DQU
               END IF
            END IF
      END SELECT


   ! Part 2
   ! ------
   ! Head, or river-part of river+weir
   ! Note: river has fictitious d/s link, same size as u/s

      IF (MTYPE == 3 .OR. NTYPE == 8) THEN
         DZ = ZX - ZI
         SIG = SIGN(ONE, DZ)
         DZ = SIG * DZ
         ROOTDZ = SQRT(DZ)
         DHH = LI * DBLE(4 - MTYPE)
         ROOTL = SQRT(DHH)

         IF (NTYPE == 3) THEN
            HM = ZI - ZGI
            STRW = STR * W
            CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)
         ELSE
            CALL OCCODE(ZGI, STR, W, AFROMXAFULL, XSTAB(:,:,LINK), ZI, CONVM, DERIVM)
         END IF

         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)

         FROMQ = FROMQ + SIG * CONVM * ROOTDZ / ROOTL
         FROMDQ = FROMDQ + (SIG * DERIVM * ROOTDZ - DUM) / ROOTL
      END IF

   END SUBROUTINE OCQBC



   !SSSSSS SUBROUTINE OCQBNK
   !----------------------------------------------------------------------*
   ! Subroutine to calculate the flow (Q) and derivative of flow (DQ) 
   ! between a channel link and an adjacent land element across a bank.
   ! Uses either a resistance equation or a flat-crested weir equation
   ! depending on the relative bank/ground elevations.
   !----------------------------------------------------------------------*
   SUBROUTINE OCQBNK(W, LI, ZBG, STR, ZI, Q, DQ)
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCQBNK/4.2
   ! Modifications:
   !  GP  Jun 92  3.4  Fix error: zero DQU/L not DQ0/I in "NO-FLOW CASE".
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980406  4.2  Remove local ALPHA: code implicit value 1.
   !                   New input args CLEN,DHH,ZBG,STR,ZI replace old
   !                   INCLUDEs+locals CLENTH/CLEN,DHF/DHH,ZBFULL/ZGRUND,
   !                   STRX/Y,HRF/Z0/ZI & old input args I/JEL,I/JFACE;
   !                   also replace output vars Q,DQ0,DQI with arrays Q,DQ
   !                   (see OCQDQ).  Alphanumeric names.
   !                   Scrap redundant output DDDZ.  Generic intrinsics.
   !                   Replace local DEBIT(*ROOTL) with new local STRW.
   !                   Explicit typing.  Define local constants.
   !                   Resistance: merge cases overflow & not, & introduce
   !                   locals CONVM,DERIVM,DUM; case no overflow - don't
   !                 ! zero DUM contribution to DQ(LO,HI), & replace DZL
   !                 ! limit 0 with -DZMIN; case overflow - scrap special
   !                 ! case DZ.lt.1D-3.  Replace 1D-6 with RDZMIN(=1D-1.5).
   !                   Bring ROOT2G from OCINI/SPEC.OC.  Amend comments.
   !                 ! Replace weir code with call to QWEIR; this fixes
   !                   errors in DQ(LO,LO) drowned, & Q(LO) undrowned - see
   !                   BR/__.  Eliminate locals HI,H0,HL,HU,ZA,ZC, and use
   !                   new locals HI,LO instead of block-IFs.
   !      980408       To conform with OCQGRD: rename CLEN W, replace arg
   !                   DHH - now local - with LI, array STR, local SIG,
   !                   reorder statments.  Eliminate locals ROOTDM,ZU,ZL.
   !                 ! Use H23MIN in DERIVM, CONVMM in DUM.  New local DZL.
   !      980730       Protect against 0^^F23.
   !----------------------------------------------------------------------*
   ! Entry requirements:   W.ge.0    LI(0)+LI(1).gt.0
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : DIMJE
      ! USE OC_MODULE, ONLY : ONE, ZERO, HALF, DZMIN, RDZMIN, ROOT2G, F23

      IMPLICIT NONE

      ! Input arguments
      ! Note: Subscript 0 refers to the link, 1 to the land element
      DOUBLE PRECISION, INTENT(IN)  :: W, LI(0:1), ZBG(0:1), STR(0:1), ZI(0:1)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q(0:1), DQ(0:1, 0:1)
      
      ! Locals
      INTEGER :: HI, LO
      DOUBLE PRECISION :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
      DOUBLE PRECISION :: ROOTDZ, ROOTL, SIG, STRW
      DOUBLE PRECISION :: DZL, ZB, ZG, COEFF(2), RDUM

   !----------------------------------------------------------------------*

      DZ = ZI(1) - ZI(0)
      SIG = SIGN(ONE, DZ)
      HI = (1 + NINT(SIG)) / 2
      LO = 1 - HI
      ZB = ZBG(0)
      ZG = ZBG(1)

      DZL = ZI(LO) - ZB

   ! Channel bank-full lower than adjacent ground: resistance equation
   ! NB: HM has an implicit upstream weighting factor, ie ALPHA=1
      IF (ZG >= ZB) THEN
         DZ = SIG * DZ + MIN(DZL, ZERO)
         ROOTDZ = SQRT(DZ)
         HM = ZI(HI) - ZBG(HI)
         
         DHH = LI(0) + LI(1)
         STRW = W * (STR(0) * LI(0) + STR(1) * LI(1)) / DHH
         ROOTL = SQRT(DHH)
         
         CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)
         
         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)
         
         Q(LO) = CONVM * ROOTDZ / ROOTL
         DQ(LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
         
         IF (DZL < -DZMIN) DUM = ZERO
         
         DQ(LO, LO) = -DUM / ROOTL

   ! Channel bank-full higher than adjacent ground: flat-crested weir eqn
      ELSE
         COEFF(1) = ROOT2G * W
         COEFF(2) = 0.386D0 * COEFF(1)
         
         ! AD aliasing fix: rdum isolates the output variable from DQ array memory
         CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO, HI), RDUM)
         DQ(LO, LO) = RDUM
      END IF

   ! Copy LO to HI
      Q(HI) = -Q(LO)
      DQ(HI, HI) = -DQ(LO, HI)
      DQ(HI, LO) = -DQ(LO, LO)

   END SUBROUTINE OCQBNK



   !SSSSSS SUBROUTINE OCQGRD
   !----------------------------------------------------------------------*
   ! CALCULATE FLOW AND DERIVATIVES BETWEEN TWO LAND ELEMENTS
   !----------------------------------------------------------------------*
   PURE SUBROUTINE OCQGRD(NTYPE, LI, ZGI, STR, W, ZI, Q, DQ)
   ! Version:  SHETRAN/OC/OCQGRD/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980331  4.2  Scrap local ALPHA - use implicit value 1 (upstream).
   !                   New input args W,LI,ZGI,STR,ZI replace old args
   !                   I/JEL,I/JFACE + common DX/YQQ,DHF,ZGRUND,STRX/Y,HRF,
   !                   making use of new locals SIG,HI,LO; scrap redundant
   !                   arg INDEX & output arg DDDZ; replace output vars
   !                   Q,DQ0,DQI with arrays Q,DQ.  (See caller OCQDQ.)
   !                   Locals: scrap Z0,ZI,H0,HI,ROOTDM,HM_53,STRI/J,DEBIT;
   !                   rename HM_23 HM23; W,STR now args (see above); add
   !                   SIG,HI,LO,STRM,CONVM/M,DERIVM,DUM.  Explicit typing.
   !                   Don't INCLUDE SPEC.AL/OC.  Generic intrinsics.
   !                  !Replace 1D-6 with RDZMIN=SQRT(1D-3) (ie 1D-1.5).
   !                  !Scrap block-IF (DZ.LT.0.001) (had DZ/ROOTDM in Q,
   !                  !with DQ=Q/DZ).  Use MAX in DERIVM.
   !                  !Replace CONVM with CONVMM in DUM.
   !      980427       Reorder args (see OCQDQ).
   !                   Replace local STRM with STRW=STRM*W.
   !      980730       Protect against 0^^F23.
   !----------------------------------------------------------------------*
   ! Entry requirements:                    W.gt.0
   !  for i in 0:1    ZI(i).ge.ZGI(i)    LI(i).gt.0    STR(i).ge.0
   ! Exit conditions:  Q(1).eq.-Q(0)
   !  for i in 0:1  DQ(1,i).eq.-DQ(0,i)
   !                    Q(i).gt.0  only_if  ZI(1-i).gt.ZI(i)
   !----------------------------------------------------------------------*
   
      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : DIMJE
      ! USE OC_MODULE, ONLY : ZERO, ONE, HALF, DZMIN, RDZMIN

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NTYPE
      DOUBLE PRECISION, INTENT(IN) :: W, LI(0:1), ZGI(0:1), STR(0:1), ZI(0:1)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q(0:1), DQ(0:1, 0:1)
      
      ! Locals
      INTEGER :: HI, LO
      DOUBLE PRECISION :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
      DOUBLE PRECISION :: ROOTDZ, ROOTL, SIG, STRW

   !----------------------------------------------------------------------*

   ! INTERNAL IMPERMEABLE BOUNDARY
   ! NB: NTYPE 3,4,5 not allowed internally
      IF (NTYPE == 1) THEN
         ! Modernization Fix: Scalar-to-array broadcasting replaces the DO loop
         Q = ZERO
         DQ = ZERO
         RETURN
      END IF

   ! Set up local variables
   ! NB: HM has an implicit upstream weighting factor, ie ALPHA=1; but
   !     note STR is averaged, so CONVM will NOT be strictly "upstream"
   ! Note: ZGI(LO) is not required
      DZ = ZI(1) - ZI(0)
      SIG = SIGN(ONE, DZ)
      HI = (1 + NINT(SIG)) / 2
      LO = 1 - HI
      DZ = SIG * DZ
      ROOTDZ = SQRT(DZ)
      HM = ZI(HI) - ZGI(HI)
      
      DHH = LI(0) + LI(1)
      STRW = W * (STR(0) * LI(0) + STR(1) * LI(1)) / DHH
      ROOTL = SQRT(DHH)

   ! CALCULATE FLOW AND DERIVATIVES
   ! NB:   H23MIN          in DERIVM  prevents small DQ when HM is small
   !        DZMIN          in CONVMM  prevents small DQ when DZ is small
   !       RDZMIN          in DUM     prevents overflow when DZ is small
   !       ROOTDZ (no MAX) in DQ gives symmetric values when DZ is small

      CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)

      CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
      DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)
      
      Q(LO) = CONVM * ROOTDZ / ROOTL
      DQ(LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL

      DQ(LO, LO) = -DUM / ROOTL
      Q(HI) = -Q(LO)
      DQ(HI, HI) = -DQ(LO, HI)

      DQ(HI, LO) = -DQ(LO, LO)

   END SUBROUTINE OCQGRD



   !SSSSSS SUBROUTINE OCQLNK
   !----------------------------------------------------------------------*
   ! CALCULATE FLOW AND DERIVATIVES BETWEEN TWO CHANNEL LINKS
   !----------------------------------------------------------------------*
   SUBROUTINE OCQLNK(NTYPE, LI, ZGI, STR, CW, XA, JXSWORK, AFROMCOCBCD, ZI, Q, DQ)
   ! Version:  SHETRAN/OC/OCQLNK/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980225  4.2  Swap COCBCD subscripts (see SPEC.OC).
   !      980226       Array COEFF (see QWEIR).
   !      980403       Remove local ALPHA: code implicit value 1.
   !                   New input args LI,ZGI,COCBCD,ZI replace old args +
   !                   INCLUDEs DHF/IFACE/JFACE,ZGRUND,COCBCD/INDEX,HRF;
   !                   also replace output vars Q,DQ0,DQI with arrays Q,DQ
   !                   (see OCQDQ).  New locals DUM,DZ,SIG.
   !                 ! Replace ROOTDM with ROOTDZ in DQ(LO,HI).
   !                   Scrap redundant output DDDZ and locals CONVL,DERIVL,
   !                   H0,HI,HU,HL,Z0,ZI.  Eliminate locals CONV0/I/U,
   !                   DERIV0/I,ROOTDM, & replace DERIVU with DERIVM.
   !                   Call OCCODE once not twice.  Generic intrinsics.
   !                 ! Replace 1D-6 with RDZMIN(=1D-1.5).
   !                   Skip conveyance calcs if NTYPE=7.  New locals
   !                   HI,LO,UEL instead of ZU,ZL,DQU,DQL + block-IFs.
   !                 ! Replace CONVM with new local CONVMM in DUM.
   !      980424       Input arguments: remove IEL,JEL; move NTYPE; add
   !                   NXSCEE,STR,CW,XA,XS (see OCQDQ).
   !                   OCCODE args: scrap UEL; add new args above; move ZI.
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : DIMJE
      ! USE OC_MODULE, ONLY : ONE, HALF, DZMIN, RDZMIN, xstab, ZQTableRef
      ! USE ZQ_MODULE, ONLY : get_ZQTable_value, ZQWeirSill

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NTYPE
      DOUBLE PRECISION, INTENT(IN) :: LI(0:1), ZGI(0:1), CW(0:1), AFROMCOCBCD(3)
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:1), STR(0:1), XA(0:1)
      INTEGER, INTENT(IN) :: JXSWORK(0:3)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q(0:1), DQ(0:1, 0:1)
      
      ! Locals
      INTEGER :: HI, LO
      DOUBLE PRECISION :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ
      DOUBLE PRECISION :: ROOTDZ, ROOTL, SIG, SUBRIO, ZSILL
      DOUBLE PRECISION :: COEFF(2), RDUM
      DOUBLE PRECISION :: DZU, WEIRSILL

   !----------------------------------------------------------------------*

   ! Set up local variables - part 1
      DZ = ZI(1) - ZI(0)
      SIG = SIGN(ONE, DZ)
      HI = (1 + NINT(SIG)) / 2
      LO = 1 - HI

   ! Internal weir
   ! NB: NTYPE 1,8,9,10,11 not allowed internally

      IF (NTYPE == 7) THEN
         COEFF(1) = AFROMCOCBCD(1)
         SUBRIO = AFROMCOCBCD(2)
         ZSILL = AFROMCOCBCD(3)
         COEFF(2) = COEFF(1)
         
         ! AD aliasing fix: rdum isolates the output variable from DQ array memory
         CALL QWEIR(ZI(HI), ZSILL, ZI(LO), COEFF, SUBRIO, Q(LO), DQ(LO, HI), RDUM)
         DQ(LO, LO) = RDUM

   ! ***ZQ Module 200520
      ELSE IF (NTYPE == 12) THEN
         ! print*, ZQTableRef, ZI(HI)
         
         Q(LO) = GET_ZQTABLE_VALUE(ZQTABLEREF, ZI(HI))
         WEIRSILL = ZQWEIRSILL(ZQTABLEREF)
         DZU = DIMJE(ZI(HI), WEIRSILL)
         
         ! This works for Crummock. Stability during step changes should be tested e.g. for a small area reservoir
         DQ(LO, HI) = 50.0D0 * 1.5D0 * SQRT(DZU)
         DQ(LO, LO) = 0.0D0
         
         ! write(779,*) ZI(HI), Q(LO), DQ(LO,HI)

   ! Standard Channel Flow
      ELSE
         ! Set up local variables - part 2
         DZ = SIG * DZ
         ROOTDZ = SQRT(DZ)
         DHH = LI(0) + LI(1)
         ROOTL = SQRT(DHH)
         
         ! CALCULATE FLOW AND DERIVATIVES
         ! NB: CONVM has an implicit upstream weighting factor, ie ALPHA=1
         CALL OCCODE(ZGI(HI), STR(HI), CW(HI), XA(HI), XSTAB(:, :, JXSWORK(HI)), ZI(HI), CONVM, DERIVM)
         
         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)
         
         ! Note: ZGI(LO), etc are not required
         Q(LO) = CONVM * ROOTDZ / ROOTL
         DQ(LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
         DQ(LO, LO) = -DUM / ROOTL
      END IF

      Q(HI) = -Q(LO)
      DQ(HI, HI) = -DQ(LO, HI)
      DQ(HI, LO) = -DQ(LO, LO)

   END SUBROUTINE OCQLNK



   !SSSSSS SUBROUTINE OCQMLN
   !----------------------------------------------------------------------*
   ! CALCULATE FLOW AND DERIVATIVES BETWEEN MULTIPLE CHANNEL LINKS
   !----------------------------------------------------------------------*
   SUBROUTINE OCQMLN(IELB, JEL2, LI, ZGI, STR, CW, XA, ZI, QJ, DQIJ, JXSWORK)
   ! Version:  SHETRAN/OC/OCQMLN/4.2
   ! Modifications:
   !  GP  Jul 93  3.4  Add argument ZNODE to OCNODE, & use to set ZOCMLN.
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980212  4.2  Bring WLMIN from SPEC.OC & set here (was in OCINI).
   !                   Remove argument ZNODE from OCNODE, & scrap ZOCMLN.
   !                   Explicit typing.  Remove unnecessary initialization.
   !                   Don't initialize QJ,QDUM1,QDUM2 (see OCNODE).
   !                   Locals: scrap C,H,Z,ZG; add JELJ.
   !                   Generic intrinsics.  Merge loop 200 with 100.
   !                   Recalculate CI,QDUM1 (for DQIJ) only if necessary.
   !      980220       OCNODE arguments: add PRI,IEL.
   !      980224       Arguments (see OCQDQ): scrap output array DDDZ2;
   !                   add PRI (from SPEC.AL), ZGI,ZI (were local), and LI
   !                   (replaces DHF from SPEC.AL); replace input variables
   !                   IFACE,JEL with arrays JEL2,JFACE2 (were local);
   !                   scrap output variable FIRST (& locals I/JROW,I/JND).
   !                   Don't set CI at null branches.  No INCLUDEs.
   !      980225       Remove redundant input JFACE2 (see OCQDQ).
   !                   Don't alter ZI: use local ZJ.
   !      980318       Get DI (new local) from OCCODE & pass it to OCNODE.
   !      980424       Add arguments NXSCEE,STR,CW,XA,XS (see OCQDQ).
   !                   OCCODE input args: scrap JELJ; add new args above;
   !                   move ZI.  Add constant ONEPC for MAX.
   !                 ! Scrap recalculation of CI for WDEPTH.lt.WLMIN.
   !                 ! Scrap treatment (QJ=0) for case single wet branch.
   !----------------------------------------------------------------------*
   ! Let active_j={j in 0:3|JEL2(j).gt.0}
   ! Entry requirements:
   !  NXSCEE.ge.1    PRI.ge.0    PRI open for F output
   !  for j in active_j   LI(j).ge.0                 ZI(j).ge.ZGI(j)
   !                     [STR(j),CW(j),XA(j),XS(1,NXSCEE,j)].gt.0
   !       for i in 1:NXSCEE-1     XS(1,i,j)=XS(1,NXSCEE,j)*(i-1)/(NXSCEE-1)
   !                               XS(2,i,j).ge.0  XS(3,i,j).gt.0
   ! Exit conditions:
   !  asum{QJ(0:3)}=0
   !  for j in active_j   asum{DQIJ(0:3,j)}=0
   !  for j1,j2 in 0:3 QJ(j1).gt.0.and.QJ(j2).lt.0 only if ZI(j1).lt.ZI(j2)
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! USE OC_MODULE, ONLY : ZERO, XSTAB

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IELB, JEL2(0:3)
      DOUBLE PRECISION, INTENT(IN) :: LI(0:3), ZGI(0:3), STR(0:3)
      DOUBLE PRECISION, INTENT(IN) :: CW(0:3), XA(0:3), ZI(0:3)
      INTEGER, INTENT(IN) :: JXSWORK(0:3)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: QJ(0:3), DQIJ(0:3, 0:3)
      ! NB: DQIJ(i,j) is defined for active_j only

      ! Locals
      DOUBLE PRECISION, PARAMETER :: ONEPC = 1.0D-2, WLMIN = 1.0D-3
      INTEGER :: I, J
      DOUBLE PRECISION :: CSAVE, DSAVE, CI(0:3), DI(0:3), QDUM2(0:3)
      DOUBLE PRECISION :: ZINC, ZSAVE, ROOTLI(0:3), ZJ(0:3)

   !----------------------------------------------------------------------*

   ! Calculate conveyance & its derivative (both.ge.0), & set local arrays
      DO J = 0, 3
         IF (JEL2(J) <= 0) THEN
            ! * OCNODE uses ROOTLI as a flag
            ROOTLI(J) = ZERO
         ELSE
            ROOTLI(J) = SQRT(LI(J))
            ZJ(J) = ZI(J)
            CALL OCCODE(ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, JXSWORK(J)), ZJ(J), CI(J), DI(J))
         END IF
      END DO

   ! Find flows out of node
      CALL OCNODE(IELB, ZI, CI, DI, ROOTLI, QJ)

   ! CALC. DQi/DHj
      DO J = 0, 3
         IF (JEL2(J) <= 0) CYCLE
         
         ! * temporarily increase ZJ and recalculate CI,DI
         ZSAVE = ZJ(J)
         CSAVE = CI(J)
         DSAVE = DI(J)
         
         ZINC = MAX(WLMIN, (ZSAVE - ZGI(J)) * ONEPC)  ! zgi is ground elevation
         ZJ(J) = ZSAVE + ZINC
         
         ! Modernization Fix: Changed scalar array pass (XSTAB(1,1,...)) to full slice to match OCCODE interface
         CALL OCCODE(ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, JXSWORK(J)), ZJ(J), CI(J), DI(J))
         
         ! * calculate resultant flows & evaluate derivative
         CALL OCNODE(IELB, ZJ, CI, DI, ROOTLI, QDUM2)
         
         DO I = 0, 3
            DQIJ(I, J) = (QDUM2(I) - QJ(I)) / ZINC
         END DO
         
         ZJ(J) = ZSAVE
         CI(J) = CSAVE
         DI(J) = DSAVE
      END DO

   END SUBROUTINE OCQMLN


   !SSSSSS SUBROUTINE CONVEYAN
   !----------------------------------------------------------------------*
   ! Calculates conveyance and its derivative based on cross-section type.
   !----------------------------------------------------------------------*
   PURE SUBROUTINE CONVEYAN(STR, H, CONV, DERIV, TY, XA, EXTRA)
   ! to bring this all to one place (its messy!)
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! USE OC_MODULE, ONLY : F23, F53

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: TY
      DOUBLE PRECISION, INTENT(IN) :: STR  ! strickler or strickler*width
      DOUBLE PRECISION, INTENT(IN) :: H    ! depth
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: XA, EXTRA  ! x-sect area
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CONV, DERIV
      
      ! Locals
      DOUBLE PRECISION :: HM23
      DOUBLE PRECISION, PARAMETER :: MUL = 10.0D0 / 3.0D0

   !----------------------------------------------------------------------*

      IF (TY == 0) THEN
         IF (H < 1.0D-9) THEN
            CONV = 0.0D0
            DERIV = 0.0D0
         ELSE IF (H < 1.0D-3) THEN
            ! conv  = deriv * h          ! LINEARIZE NEAR ZERO
            CONV = STR * MUL * H * H * (4.0D0 - 1.0D3 * H)  ! TAKE CARE valid only for threshold of 1 mm
            CONV = CONV * XA / H
            DERIV = STR * MUL * H * (8.0D0 - 3.0D3 * H)     ! TAKE CARE valid only for threshold of 1 mm
         ELSE
            HM23 = H**F23
            CONV = STR * XA * HM23      ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
            DERIV = STR * HM23 * F53
         END IF
         
      ELSE IF (TY == 1) THEN
         IF (H < 1.0D-9) THEN
            CONV = 0.0D0
            DERIV = 0.0D0
         ELSE IF (H < 1.0D-3) THEN
            ! conv  = deriv * h          ! LINEARIZE NEAR ZERO
            CONV = STR * MUL * H * H * (4.0D0 - 1.0D3 * H)  ! TAKE CARE valid only for threshold of 1 mm
            DERIV = STR * MUL * H * (8.0D0 - 3.0D3 * H)     ! TAKE CARE valid only for threshold of 1 mm
         ELSE
            HM23 = H**F23
            CONV = STR * H * HM23       ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
            DERIV = STR * HM23 * F53
         END IF
         
      ELSE IF (TY == 2) THEN
         HM23 = H**F23
         CONV = STR * XA * HM23
         DERIV = CONV * (EXTRA / XA + F23 / H)  ! is f23 correct here?
      END IF

      ! Legacy Disabled Block
      ! IF(ty<2) THEN
      !    IF(h<dzmin) THEN
      !        deriv = str * h23min * f23
      !        conv  = deriv * h  !LINEARIZE NEAR ZERO (FOR AD)
      !        hm23  = zero
      !    ELSE
      !        hm23 = h**f23
      !        conv = str * xo * hm23
      !        deriv = str * hm23 * f53  !str * MAX(h23min, hm23) * f53
      !    ENDIF
      ! ELSE

   END SUBROUTINE CONVEYAN



   !SSSSSS SUBROUTINE QWEIR
   !----------------------------------------------------------------------*
   !  CALCULATE FLOW AND DERIVATIVES ACROSS A HORIZONTAL-CRESTED WEIR
   !
   ! INPUT PARAMETERS:
   !            ZU       - GAUGED HEAD ABOVE THE WEIR
   !            ZSILL    - ELEVATION OF THE WEIR SILL
   !            ZL       - GAUGED HEAD BELOW THE WEIR
   !            COEFF(2) - (CONSTANT) WEIR DISCHARGE COEFFICIENT:
   !                       1=DROWNED, 2=UNDROWNED
   !            SUBRIO   - SUBMERGENCE RATIO
   !----------------------------------------------------------------------*
   PURE SUBROUTINE QWEIR(ZU, ZSILL, ZL, COEFF, SUBRIO, Q, DQU, DQL)
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/QWEIR/4.2
   ! Modifications:
   ! RAH  980226  4.2  Make COEFF an array, size 2 (was simple variable)
   !                   (also in callers OCQBC,OCQLNK).  Explicit typing.
   !                   Zero outputs if no flow.  Generic intrinsics.
   !                  !Add missing term CR to DQL (drowned).
   !                   Locals: add DZU,DZL,RDZMIN,CR; scrap ROOTDM.
   !      980730      !Use MAX to ensure DQU.gt.0 (except "no flow" case).
   !                  !New locals DZMIN,DML.  SQRT(DZMIN) was 1D-6.
   !                  !Subtract DZMIN from ZSILL in "no flow" criterion.
   !----------------------------------------------------------------------*
   ! Entry requirements:    [SUBRIO,COEFF(1:2)].ge.0    ZU.ge.ZL
   ! Exit conditions:                   [Q,DQU].ge.0
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : DIMJE
      ! USE OC_MODULE, ONLY : ZERO, HALF, DZMIN, RDZMIN

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: ZU, ZSILL, ZL, SUBRIO, COEFF(2)
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q, DQU, DQL
      
      ! Locals
      DOUBLE PRECISION :: CR, DML, DZU, DZL, ROOTDZ

   !----------------------------------------------------------------------*

   ! NO FLOW ACROSS WEIR
      IF (ZU < ZSILL - DZMIN) THEN
         Q = ZERO
         DQU = ZERO
         DQL = ZERO
      ELSE
         DZU = DIMJE(ZU, ZSILL)
         DZL = ZL - ZSILL

   ! DROWNED WEIR
         IF (DZL > SUBRIO * DZU) THEN
            ROOTDZ = SQRT(ZU - ZL)
            DML = MAX(DZMIN, DZL)
            CR = COEFF(1) * ROOTDZ
            Q = CR * DZL
            DQU = COEFF(1) * DML * HALF / MAX(RDZMIN, ROOTDZ)
            DQL = CR - DQU

   ! UNDROWNED WEIR
         ELSE
            ROOTDZ = SQRT(DZU)
            Q = COEFF(2) * DZU * ROOTDZ
            DQU = COEFF(2) * 1.5D0 * MAX(RDZMIN, ROOTDZ)
            DQL = ZERO
         END IF
      END IF

   END SUBROUTINE QWEIR



   !SSSSSS SUBROUTINE OCFIX
   SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)
   !----------------------------------------------------------------------*
   !
   !  Ensure that discharges and elevations/depths satisfy requirements.
   !
   !  The following conditions are treated:
   !     i flow in direction of non-negative surface elevation gradient;
   !    ii flow  rate  less than a pre-defined minimum meaningful value;
   !   iii water depth less than a pre-defined minimum meaningful value.
   !
   !  Treatment comprises a reduction of existing flow rates, with
   !  corresponding (ie conservative) adjustments to surface elevations.
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCFIX/4.27
   ! Modifications:
   ! RAH  08.10.94  Version 3.4.1. Created 03.10.94 from part of OCSIM.
   !                  !Repeat IEL loop up to NPASS times.  UHCRIT was 0.
   !                  !HRF adjustment (corresponding to flow correction)
   !                   had "*DTOC" missing.
   !                  !Adjust confluence flows too (were disregarded).
   ! RAH  980115  4.2  Add !INTRINSIC.
   !      980617      !Treat cases i & ii (above) for discharges only.
   !                  !Replace "positive" with "non-negative" in case i.
   !                  !New test criteria at confluences (were unreliable).
   !                  !Less severe flow adjustments (were just set to 0).
   !      980618       Give details, in MSG, of any mass created or lost.
   !      980623      !Merge flow & depth loops, with depth as priority.
   !      980624      !Make depth adjustments conservative.
   !                  !Scrap locals KEL,KFACE (KEL wasn't set when JEL=0).
   !                   Swap sign of HERROR; use in ERROR call criteria.
   !      980625      !Adjust HRF(IEL) once only: use ZE in the interim.
   !      980729      !NPASS 50 was 4; also introduce new error 1060.
   !                   Replace statement function FNDXY with array DXY.
   ! SB   990204 4.27  Problem with small flows from a lower to a higher
   !                   element. Modify DQE0
   ! SB   990208 4.27  Problem with small flows from a lower to a higher
   !                   element. Add AOK = FALSE in final depth adjustment
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  NEL.ge.1    NELEE.ge.NEL         DTOC.gt.0
   !  PRI.ge.0    NLFEE.ge.1    AREA(1:NEL).gt.0    PRI open for F output
   !  for iel in 1:NEL  for iface in 1:4
   !      let jel=ICMREF(iel,iface,2)    let ibr=-jel
   !          jel.le.NEL                     ibr.le.NLFEE
   !      {jel.lt.1} .or. {1.le.ICMREF(iel,iface,3).le.4}
   !      {ibr.lt.1} .or. {  for p in 1:3  let pel=ICMRF2(ibr,p,1)
   !                             pel.le.NEL
   !                            {pel.lt.1} .or. {1.le.ICMRF2(ibr,p,2).le.4}
   !                         max{pel}.ge.1  }
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCFIX/4.27

      ! Assumed external module dependencies providing global variables:
      ! NELEE, NLFEE, cellarea, DXQQ, DYQQ, ZGRUND, NOTZERO, ZERO, ONE,
      ! DZMIN, WWWARN, PPPRI, ERROR

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nel, afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
      DOUBLE PRECISION, INTENT(IN) :: dtoc
      DOUBLE PRECISION, DIMENSION(nel), INTENT(IN)    :: inhrf
      DOUBLE PRECISION, DIMENSION(nel), INTENT(OUT)   :: GGGETHRF
      DOUBLE PRECISION, DIMENSION(nel, 4), INTENT(IN) :: inqsa
      DOUBLE PRECISION, DIMENSION(nel, 4), INTENT(OUT):: GGGETQSA

      INTEGER, PARAMETER :: NPASS = 100
      DOUBLE PRECISION, PARAMETER :: UHCRIT = 1.0D-7, HCRIT = 1.0D-7, HERROR = 1.0D-5
      
      INTEGER          :: IELc, IFACE, IBR, idum
      INTEGER          :: JEL, JFACE, PPP, PASSS, PEL, PEL0, PFACE, PFACE0
      DOUBLE PRECISION :: DQE, DZE, QE, ZE, DHQ, DHH, DDZ, DQE0, FDQE, H
      DOUBLE PRECISION :: DQA, DZA, QA, ZA, QQ, QQMIN, Qasum, SGN, ZG, DXY (0:1), rdum4(4)
      LOGICAL          :: AOK, QSMALL, HSMALL, FAIL, FAILP, TEST, FLAG (4)
      CHARACTER(132)   :: MSG

   !----------------------------------------------------------------------*
   ! Control Loop
   ! ------------
      
      ! Fast whole-array copies outside the iteration loop
      GGGETHRF = inhrf
      GGGETQSA = inqsa
      AOK = .FALSE.
      
      pass_loop: DO PASSS = 1, NPASS

         AOK = .TRUE.
         
         element_loop: DO ielc = 1, NEL
            ZE = GGGETHRF (ielc)
            DZE = DTOC / cellarea (ielc)
            DXY (0) = DXQQ (ielc)
            DXY (1) = DYQQ (ielc)

            ZG = ZGRUND (ielc)
            H = ZE - ZG
            HSMALL = (H < HCRIT) .AND. NOTZERO(H)
            FDQE = ZERO
            
            IF (HSMALL) THEN
               DQE0 = -H / DZE
               SGN = SIGN (ONE, DQE0)
               Qasum = ZERO
               
               DO IFACE = 1, 4
                  QE = GGGETQSA (ielc, IFACE)
                  FLAG (IFACE) = QE * SGN < ZERO
                  IF (FLAG (IFACE)) Qasum = Qasum + QE
               END DO
               
               IF (NOTZERO(Qasum)) FDQE = MAX (-ONE, DQE0 / Qasum)
            END IF
            
            ! Face Loop
            Qasum = ZERO
            face_loop: DO IFACE = 1, 4
               QE = GGGETQSA (ielc, IFACE)
               
               TEST = QE < ZERO
               IF (HSMALL) TEST = FLAG (IFACE)
               IF (.NOT. TEST) CYCLE face_loop
               
               QSMALL = -QE < DXY (MOD (IFACE, 2)) * UHCRIT
               TEST = QSMALL .OR. HSMALL
               
               JEL = afromICMREF (ielc, IFACE, 2)
               IF (JEL > 0) THEN
                  JFACE = afromICMREF (ielc, IFACE, 3)
                  FAIL = GGGETHRF (JEL) >= ZE
               ELSE IF (JEL == 0) THEN
                  FAIL = .FALSE.
               ELSE
                  IBR = -JEL
                  QQMIN = ZERO
                  FAIL = .FALSE.
                  
                  confluence_loop: DO PPP = 1, 3
                     PEL = afromICMRF2 (IBR, PPP, 1)
                     IF (PEL < 1) CYCLE confluence_loop
                     
                     PFACE = afromICMRF2 (IBR, PPP, 2)
                     QQ = GGGETQSA (PEL, PFACE) * QE
                     FAILP = (GGGETHRF (PEL) >= ZE) .AND. (QQ < ZERO)
                     
                     IF ((FAILP .OR. TEST) .AND. QQ < QQMIN) THEN
                        JEL = PEL
                        JFACE = PFACE
                        QQMIN = QQ
                     END IF
                     
                     FAIL = FAIL .OR. FAILP
                     PEL0 = PEL
                     PFACE0 = PFACE
                  END DO confluence_loop
                  
                  IF (JEL < 0) THEN
                     JEL = PEL0
                     JFACE = PFACE0
                  END IF
               END IF

               ! Adjustments
               IF (FAIL .OR. TEST) THEN
                  AOK = .FALSE.
                  
                  IF (JEL > 0) THEN
                     DZA = DTOC / cellarea (JEL)
                     ZA = GGGETHRF (JEL)
                     QA = GGGETQSA (JEL, JFACE)
                  END IF
                  
                  IF (HSMALL) THEN
                     DQE = FDQE * QE
                  ELSE IF (QSMALL) THEN
                     DQE = -QE
                  ELSE
                     DDZ = DZMIN + ZA - ZE
                     DQE = MIN (+QA, -QE, DDZ / (DZA + DZE))
                  END IF
                  
                  Qasum = Qasum + DQE
                  GGGETQSA(ielc, IFACE) = QE + DQE
                  ZE = ZE + DQE * DZE
                  
                  IF (JEL > 0) THEN
                     SGN = SIGN (ONE, DQE)
                     DQA = -SGN * MIN (SGN * DQE, SGN * QA)
                     Qasum = Qasum + DQA
                     GGGETQSA(JEL, JFACE) = QA + DQA
                     GGGETHRF(JEL) = ZA + DQA * DZA
                  END IF
                  
                  IF (.NOT. HSMALL) THEN
                     DHQ = Qasum * DZE
                     Qasum = ZERO
                     
                     IF ((ABS (DHQ) > HERROR) .OR. (passs == npass)) THEN
                        rdum4(1) = -QE 
                        rdum4(2) = -1.0D2 * DQE / QE 
                        idum = IFACE 
                        rdum4(4) = DHQ
                        
                        ! PERF FIX: Unrolled the array slice rdum4(1:2)
                        WRITE (MSG, 91030) rdum4(1), rdum4(2), idum, rdum4(4)
                        CALL ERROR(WWWARN, 1030, PPPRI, ielc, 0, MSG)
                     END IF
                  END IF
               END IF
            END DO face_loop

            ! Final Depth Adjustment
            IF (HSMALL) THEN
               AOK = .FALSE.
               DHQ = Qasum * DZE
               DHH = ZG - ZE
               ZE = ZG
               
               IF ((ABS (DHQ) + ABS (DHH) > HERROR) .OR. (passs == npass)) THEN
                  rdum4(1) = H 
                  rdum4(2) = DHQ 
                  rdum4(3) = DHH
                  
                  ! PERF FIX: Unrolled the array slice rdum4(1:3)
                  WRITE (MSG, 91024) rdum4(1), rdum4(2), rdum4(3)
                  CALL ERROR(WWWARN, 1024, PPPRI, ielc, 0, MSG)
               END IF
            END IF
            
            GGGETHRF(ielc) = ZE
         END DO element_loop

         ! Clean break out if network satisfies all stability criteria
         IF (AOK) EXIT pass_loop
         
      END DO pass_loop
      
      IF (.NOT. AOK) CALL ERROR(WWWARN, 1060, PPPRI, 0, 0, 'OC flow criteria could not be met')

      ! FORMAT STATEMENTS (Safely compiled exactly once)
91024 FORMAT('Surface water depth adjusted from', SP, 1PG15.7, ' to zero', ': depth created =', 2G15.7)
91030 FORMAT('Surface water discharge rate', 1PG14.7, ' reduced by', 0PF7.2, '% at face', I4, ': depth created =', SP, 1PG15.7)

   END SUBROUTINE OCFIX

END MODULE OCmod2
