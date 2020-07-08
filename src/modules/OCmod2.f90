MODULE OCmod2
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces part of the OC.F files
USE SGLOBAL
!!***ZQ Module 200520 
USE ZQmod,     ONLY : ZQtable
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

!FFFFFF DOUBLEPRECISION FUNCTION gethrf
DOUBLEPRECISION FUNCTION gethrf(i)
INTEGER, INTENT(IN) :: i
gethrf = hrfzz(i)
END FUNCTION gethrf

!SSSSSS SUBROUTINE sethrf
SUBROUTINE sethrf(i,v)
INTEGER, INTENT(IN)         :: i
DOUBLEPRECISION, INTENT(IN) :: v
hrfzz(i) = v
END SUBROUTINE sethrf


!FFFFFFR DOUBLEPRECISION FUNCTION getqsa
DOUBLEPRECISION FUNCTION getqsa(i,j)
INTEGER, INTENT(IN) :: i, j
getqsa = qsazz(i,j)
END FUNCTION getqsa

!SSSSSS SUBROUTINE setQSA
SUBROUTINE setqsa(i,j, v)
INTEGER, INTENT(IN)         :: i, j
DOUBLEPRECISION, INTENT(IN) :: v
qsazz(i,j) = v
END SUBROUTINE setqsa


!FFFFFFR DOUBLEPRECISION FUNCTION getqsa_all
FUNCTION getqsa_all(n)
INTEGER, INTENT(IN)             :: n
DOUBLEPRECISION, DIMENSION(n,4) :: getqsa_all
getqsa_all = qsazz(1:n,:)
END FUNCTION getqsa_all

!SSSSSS SUBROUTINE initialise_ocmod
SUBROUTINE initialise_ocmod()
!print*,nxscee,total_no_links
ALLOCATE(xstab(3,nxscee,total_no_links))
!print*,'here'
END SUBROUTINE initialise_ocmod



!SSSSSS SUBROUTINE OCNODE
SUBROUTINE OCNODE (iela, ZI, CI, DI, ROOTLI, QJ)  
!----------------------------------------------------------------------*
! CALCULATES FLOWS OUT OF NODE AS FUNCTION OF ADJACENT WATER ELEVATIONS
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCNODE/4.27
! Modifications:
!  GP          3.4  Call ERROR & terminate iterations if nc.eq.50.
!                   Add argument ZNODE (see OCQMLN).
! RAH  980212  4.2  Supply missing PRI,FATAL,WARN, to pass to ERROR;
!                   also remove argument ZNODE (see OCQMLN).
!                   Explicit typing.  Locals: scrap TESTZ; add TEST.
!                   Generic intrinsics.  Description: OUT OF, not INTO.
!                   RETURN immediately if FA=0 (don't overwrite QJ).
!                   Test NC BEFORE updating A or B.
!                   Set QJ at absent branches (was in OCQMLN).
!      980220       Add argument IEL to pass to ERROR (see OCQMLN).
!      980318       Add argument DI to pass to FNODE (see OCQMLN).
! SB   990204 4.27  Problem with conserving mass at junctions
!                   Adjust QJ so the largest absolute value at a
!                   junction is modified so that the asum = 0
!----------------------------------------------------------------------*

INTEGER, INTENT(IN)         :: IELa  
DOUBLEPRECISION, INTENT(IN) :: CI (0:3), DI (0:3), ZI (0:3), ROOTLI (0:3)  
! NB:
!         ROOTLI(J)   is zero for any absent branches J.gt.0
!                     Note: branch J=0 is never absent.
!   DI(J),CI(J),ZI(J) are undefined for absent branches
DOUBLEPRECISION, INTENT(OUT) :: QJ (0:3)  
INTEGER                      :: J, NC  
DOUBLEPRECISION              :: A, B, FA, FB, FN, FNM1, SIGMAQ, WN  
LOGICAL                      :: TEST  
!^^^^RAH/SB 4/2/99 CONSERVE MASS AT JUNCTIONS ^^^^^^^^^^^^^
INTEGER                      :: JMAJOR 
LOGICAL :: iscycle, failed 
!----------------------------------------------------------------------*
!
! FIRST GUESSES (CHOOSE VALUES A,B SUCH THAT F(A)*F(B) .le. 0 )
! (USE MIN AND MAX OF VALID ELEVATIONS); also, set QJ at absent branches
!
A = ZI (0)  
B = A  
DO J = 1, 3  
    IF(ISZERO(ROOTLI(J))) THEN  
        QJ (J) = zero  
    ELSE  
        A = MIN (ZI (J), A)  
        B = MAX (ZI (J), B)  
    ENDIF  
ENDDO  
CALL FNODE(A, DI, CI, ZI, ROOTLI, QJ, FA)  
IF (ISZERO(FA)) RETURN  
CALL FNODE(B, DI, CI, ZI, ROOTLI, QJ, FB)  
IF (ISZERO(FB)) RETURN  
!
! Iterate to convergence, using successive linear interpolation
!
FN = FA  
NC = 0
failed =.FALSE.
iscycle=.FALSE.
!     * Start of iteration loop: set new point WN and calculate FN
DO nc=1,50
    IF(iscycle) CYCLE
    WN   = (A*FB - B*FA) / (FB-FA)  
    FNM1 = FN  
    CALL FNODE(WN, DI, CI, ZI, ROOTLI, QJ, FN)  
    IF (ISZERO(FN)) THEN   !        * Test for convergence (either exact or approximate)
        failed =.TRUE.
        iscycle=.TRUE.
        CYCLE
    ENDIF
    SIGMAQ = ABS(QJ(0) ) + ABS(QJ(1) ) + ABS(QJ(2) ) + ABS(QJ(3) )
    IF (ABS(FN) .LE. SIGMAQ*1.0D-2 .AND. ABS(B-A) .LE. 1.0D-3) THEN
       JMAJOR = 0  
        DO J = 1, 3  
            IF (ABS(QJ(J)) .GT. ABS(QJ(JMAJOR))) JMAJOR = J
        ENDDO 
        QJ(JMAJOR) = QJ(JMAJOR) - FN
        iscycle=.TRUE.
        CYCLE
     ENDIF
    !            * ... carry on: replace either A or B with WN; and
    !            * adjust interpolation factor if sign of F didn't change
    TEST = GTZERO(FN * FNM1)  !TAKE CARE - PRECEDENCE
    IF (FN * FA.GE.0D0) THEN  
        A = WN  
        FA = FN  
        IF (TEST) FB = FB * half  
    ELSE  
        B = WN  
        FB = FN  
        IF (TEST) FA = FA * half  
    ENDIF  
 ENDDO
 IF(failed) THEN
    IF (ABS (FN) .LT.SIGMAQ * 1D-1.AND.ABS (B - A) .LT.5D-2) THEN  
        !CALL ERROR(WWWARN, 1027, PPPRI, iela, 0, 'maximum iterations exceeded for OC confluence')
    ELSE  
        !CALL ERROR(FFFATAL, 1028, PPPRI, iela, 0, 'iteration failure for OC confluence')
    ENDIF
ENDIF
END SUBROUTINE OCNODE

!SSSSSS SUBROUTINE FNODE
SUBROUTINE FNODE (ZNODE, DI, CI, ZI, ROOTLI, QJ, resfnode)  
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
DOUBLEPRECISION, INTENT(IN) ::  ZNODE, DI (0:3), CI (0:3), ZI (0:3), ROOTLI (0:3)  
DOUBLEPRECISION, INTENT(OUT) ::  QJ (0:3), resfnode 
! NB:
!         QJ(J) is output, but only for those J with ROOTLI(J).ne.0
! Locals, etc
INTEGER         :: J  
DOUBLEPRECISION :: CJ, DZ, Qasum, SIG  
!----------------------------------------------------------------------*
Qasum = zero
qj = zero
DO J = 0, 3  
   IF (ISZERO(ROOTLI(J))) CYCLE
!                            >>>>>>>>
   DZ = ZNODE-ZI (J)  
   SIG = SIGN (ONE, DZ)  
   CJ = CI (J) + DI (J) * MAX (ZERO, DZ)  
   QJ (J) = SIG * CJ * SQRT (SIG * DZ) / ROOTLI (J)  
   Qasum = QJ (J) + Qasum  
ENDDO  
resfnode = Qasum  
END SUBROUTINE FNODE

!SSSSSS SUBROUTINE OCCODE
SUBROUTINE OCCODE(ZG, STR, afromCWIDTH, afromXAFULL, afromXStypes, Z, CONV, DERIV)
!----------------------------------------------------------------------*
!  CALCULATE CONVEYANCE AND DERIVATIVE FOR A CHANNEL LINK.
!----------------------------------------------------------------------*
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
!  for i in 1:NXSCEE-1   XS(1,i)=XS(1,NXSCEE)*(i-1)/(NXSCEE-1)
!                        XS(2,i).ge.0    XS(3,i).gt.0
! Exit conditions:          CONV.ge.0      DERIV.gt.0
!----------------------------------------------------------------------*
DOUBLEPRECISION, INTENT(IN) ::  ZG, STR, afromCWIDTH, afromXAFULL, Z
DOUBLEPRECISION, INTENT(IN) ::  afromXStypes(3, NXSCEE)  
DOUBLEPRECISION, INTENT(OUT) :: CONV, DERIV  
INTEGER :: I  
DOUBLEPRECISION H, HFULL, XA
!----------------------------------------------------------------------*asum1
H = Z - ZG  
HFULL = afromXStypes (1, NXSCEE)  

I = INT((H / HFULL) * DBLE(NXSCEE-1) + one)
!I = (H / HFULL) * (NXSCEE-1) + one
IF (I.LT.NXSCEE) THEN  
!         * use look-up tables
   DERIV = afromXStypes (3, I)  
   CONV = afromXStypes (2, I) + DERIV * DIMJE(H, afromXStypes (1, I) )  
ELSE  
!         * calculate values directly
   XA = afromXAFULL + afromCWIDTH * DIMJE(H, HFULL)  
   !CONV = STR * XA * H**F23
   CALL CONVEYAN(str, h, conv, deriv, 2, xa, afromCWIDTH)
   !DERIV = CONV * (afromCWIDTH / XA + F23 / H)

ENDIF  
END SUBROUTINE OCCODE

!SSSSSS SUBROUTINE OCQBC
!SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, afromXAFULL, afromXSTAB, afromCOCBCD, ZI, afromHOCNOW, afromQOCF, fromQ, fromDQ)
SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, afromXAFULL, link, afromCOCBCD, ZI, afromHOCNOW, afromQOCF, fromQ, fromDQ)
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
! Input arguments
INTEGER, INTENT(IN)         :: NTYPE, LINK
DOUBLEPRECISION, INTENT(IN) ::  LI, ZGI, STR, W, afromXAFULL, ZI, afromHOCNOW, afromQOCF, &
                                afromCOCBCD(5) !, afromXSTAB (3, NXSCEE)  
DOUBLEPRECISION, INTENT(OUT) :: fromQ, fromDQ  
INTEGER                      :: MTYPE  
DOUBLEPRECISION              :: AH, B, C, CONVM, CONVMM, D, DERIVM, DHH, DQU, DUM, DZ, E
DOUBLEPRECISION              :: H, HM, ROOTDZ, ROOTL  
DOUBLEPRECISION              :: SIG, STRW, SUBRIO, ZSILL, ZL, ZU, ZX, COEFF (2)  
!----------------------------------------------------------------------*
! Prologue
! --------
MTYPE = MOD (NTYPE, 6)  
! Part 1
! ------
! Prescribed time-varying head - grid (3) or channel (9)
!     NB: see Part 2

IF (MTYPE.EQ.3) THEN  
   ZX = afromHOCNOW  
   fromQ = zero  
   fromDQ = zero  
! Prescribed time-varying flow - grid (4) or channel (10)
!     NB: QOCF is rate of INFLOW, not discharge

ELSEIF (MTYPE.EQ.4) THEN  
   fromQ = afromQOCF  
   fromDQ = zero  
! Flow a polynomial function of head - grid (5) or channel (11)

ELSEIF (MTYPE.EQ.5) THEN  
   H = ZI - ZGI  
   AH = afromCOCBCD (1) * H  
   B = afromCOCBCD (2)  
   C = afromCOCBCD (3)  
   D = afromCOCBCD (4)  
   E = afromCOCBCD (5)  
   fromQ = - ( ( ( (AH + B) * H + C) * H + D) * H + E)  
   fromDQ = - ( ( (4D0 * AH + 3D0 * B) * H + 2D0 * C) * H + D)  
! Weir (7) ... with river in parallel (8) - see Part 2

ELSEIF (NTYPE.EQ.7.OR.NTYPE.EQ.8) THEN  
   COEFF (1) = afromCOCBCD (1)  
   SUBRIO = afromCOCBCD (2)  
   ZSILL = afromCOCBCD (3)  
   ZX = afromCOCBCD (4)  
   COEFF (2) = COEFF (1)  
   ZU = MAX (ZX, ZI)  
   ZL = MIN (ZX, ZI)  
   CALL QWEIR (ZU, ZSILL, ZL, COEFF, SUBRIO, fromQ, DQU, fromDQ)  
   IF (ZI.GE.ZX) THEN  
      fromQ = - fromQ  
      fromDQ = - DQU  
   ENDIF  
ENDIF  
! Part 2
! ------
! Head, or river-part of river+weir
!     Note: river has fictitious d/s link, same size as u/s

IF (MTYPE.EQ.3.OR.NTYPE.EQ.8) THEN  
   DZ = ZX - ZI  
   SIG = SIGN (ONE, DZ)  
   DZ = SIG * DZ  
   ROOTDZ = SQRT (DZ)  
   DHH = LI * DBLE(4 - MTYPE)  

   ROOTL = SQRT (DHH)  
   IF (NTYPE.EQ.3) THEN  
      HM = ZI - ZGI  
      !HM23 = zero  
      !IF (GTZERO(HM)) HM23 = HM**F23  
      STRW = STR * W  
      !CONVM = STRW * HM23 * HM
      CALL CONVEYAN(strw, hm, convm, derivm, 1)
      !DERIVM = STRW * MAX (H23MIN, HM23) * F53  
   ELSE  
      !CALL OCCODE (ZGI, STR, W, afromXAFULL, afromXSTAB, ZI, CONVM, DERIVM)
      CALL OCCODE (ZGI, STR, W, afromXAFULL, XSTAB(:,:,link), ZI, CONVM, DERIVM)

   ENDIF  
   CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)  

   DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)  
   fromQ = fromQ + SIG * CONVM * ROOTDZ / ROOTL  

   fromDQ = fromDQ + (SIG * DERIVM * ROOTDZ - DUM) / ROOTL  

ENDIF  
END SUBROUTINE OCQBC


!SSSSSS SUBROUTINE OCQBNK
SUBROUTINE OCQBNK (W, LI, ZBG, STR, ZI, Q, DQ)  
!----------------------------------------------------------------------*
!
!  CALCULATE FLOW AND DERIVATIVES AT A BANK OF A CHANNEL LINK
!
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
! Entry requirements:    W.ge.0    LI(0)+LI(1).gt.0
!----------------------------------------------------------------------*
! Note: Subscript 0 refers to the link, 1 to the land element
DOUBLEPRECISION, INTENT(IN)  :: W, LI (0:1), ZBG (0:1), STR (0:1), ZI (0:1)  
DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)  
INTEGER                      :: HI, LO  
DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, STRW  
DOUBLEPRECISION              :: DZL, ZB, ZG, COEFF (2), rdum
DZ = ZI (1) - ZI (0)  
SIG = SIGN (ONE, DZ)  
HI = (1 + NINT (SIG) ) / 2  
LO = 1 - HI  
ZB = ZBG (0)  
ZG = ZBG (1)  

DZL = ZI (LO) - ZB  
!
! Channel bank-full lower than adjacent ground: resistance equation
!
!     NB: HM has an implicit upstream weighting factor, ie ALPHA=1

IF (ZG.GE.ZB) THEN  
   DZ = SIG * DZ + MIN (DZL, ZERO)  
   ROOTDZ = SQRT (DZ)  
   HM = ZI (HI) - ZBG (HI)  
   !HM23 = ZERO  
   !IF (HM.GT.ZERO) HM23 = HM**F23  
   DHH = LI (0) + LI (1)  
   STRW = W * (STR (0) * LI (0) + STR (1) * LI (1) ) / DHH  

   ROOTL = SQRT (DHH)  
   !CONVM = STRW * HM23 * HM
   CALL CONVEYAN(strw, hm, convm, derivm, 1)
   !DERIVM = STRW * MAX (H23MIN, HM23) * F53  
   CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)  

   DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)  
   Q (LO) = CONVM * ROOTDZ / ROOTL  
   DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL  
   IF (DZL.LT. - DZMIN) DUM = ZERO  


   DQ (LO, LO) = - DUM / ROOTL  
!
! Channel bank-full higher than adjacent ground: flat-crested weir eqn
!

ELSE  
   COEFF (1) = ROOT2G * W  
   COEFF (2) = 386D-3 * COEFF (1)  

   CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO,HI), rdum)  !AD aliasing
   DQ(LO,LO) = rdum
    


ENDIF  
!
! Copy LO to HI
!
Q (HI) = - Q (LO)  
DQ (HI, HI) = - DQ (LO, HI)  

DQ (HI, LO) = - DQ (LO, LO)  
END SUBROUTINE OCQBNK

!SSSSSS SUBROUTINE OCQGRD  
SUBROUTINE OCQGRD (NTYPE, LI, ZGI, STR, W, ZI, Q, DQ)  
!----------------------------------------------------------------------*
!
!  CALCULATE FLOW AND DERIVATIVES BETWEEN TWO LAND ELEMENTS
!
!----------------------------------------------------------------------*
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
! Entry requirements:                     W.gt.0
!  for i in 0:1    ZI(i).ge.ZGI(i)    LI(i).gt.0    STR(i).ge.0
! Exit conditions:  Q(1).eq.-Q(0)
!  for i in 0:1  DQ(1,i).eq.-DQ(0,i)
!                   Q(i).gt.0  only_if  ZI(1-i).gt.ZI(i)
!----------------------------------------------------------------------*
! Input arguments
INTEGER, INTENT(IN)          :: NTYPE  
DOUBLEPRECISION, INTENT(IN)  :: W, LI (0:1), ZGI (0:1), STR (0:1), ZI (0:1)  
DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)  
INTEGER                      :: HI, LO, I  
DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, STRW  
!----------------------------------------------------------------------*
!
! INTERNAL IMPERMEABLE BOUNDARY
!
! NB: NTYPE 3,4,5 not allowed internally
IF (NTYPE.EQ.1) THEN  
   DO 10 I = 0, 1  
      Q (I) = zero  
      DQ (I, 0) = zero  
      DQ (I, 1) = zero  
   10    END DO  
   RETURN  
!         ^^^^^^
ENDIF  
!
! Set up local variables
!
!     NB: HM has an implicit upstream weighting factor, ie ALPHA=1; but
!         note STR is averaged, so CONVM will NOT be strictly "upstream"
!     Note: ZGI(LO) is not required
DZ = ZI (1) - ZI (0)  
SIG = SIGN (ONE, DZ)  
HI = (1 + NINT (SIG) ) / 2  
LO = 1 - HI  
DZ = SIG * DZ  
ROOTDZ = SQRT (DZ)  
HM = ZI (HI) - ZGI (HI)  
!HM23 = zero  
!IF (GTZERO(HM)) HM23 = HM**F23  
DHH = LI (0) + LI (1)  
STRW = W * (STR (0) * LI (0) + STR (1) * LI (1) ) / DHH  
ROOTL = SQRT (DHH)  
!
! CALCULATE FLOW AND DERIVATIVES
!
! NB:   H23MIN          in DERIVM  prevents small DQ when HM is small
!        DZMIN          in CONVMM  prevents small DQ when DZ is small
!       RDZMIN          in DUM     prevents overflow when DZ is small
!       ROOTDZ (no MAX) in DQ gives symmetric values when DZ is small
!
!CONVM = STRW * HM23 * HM 
CALL CONVEYAN(strw, hm, convm, derivm, 1)
!DERIVM = STRW * MAX (H23MIN, HM23) * F53  
CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)  

DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)  
Q (LO) = CONVM * ROOTDZ / ROOTL  
DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL  

DQ (LO, LO) = - DUM / ROOTL  
Q (HI) = - Q (LO)  
DQ (HI, HI) = - DQ (LO, HI)  

DQ (HI, LO) = - DQ (LO, LO)  
END SUBROUTINE OCQGRD


!SSSSSS SUBROUTINE OCQLNK
SUBROUTINE OCQLNK(NTYPE, LI, ZGI, STR, CW, XA, jXSwork, afromCOCBCD, ZI, Q, DQ)
!----------------------------------------------------------------------*
!
!  CALCULATE FLOW AND DERIVATIVES BETWEEN TWO CHANNEL LINKS
!
!----------------------------------------------------------------------*
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
! Input arguments
INTEGER, INTENT(IN)          :: NTYPE  
DOUBLEPRECISION, INTENT(IN)  :: LI (0:1), ZGI (0:1), CW (0:1), afromCOCBCD(3)  
DOUBLEPRECISION, INTENT(IN)  ::  ZI (0:1), STR (0:1), XA (0:1)
!DOUBLEPRECISION, INTENT(IN)  :: afromXSwork (3, NXSCEE, 0:1)
INTEGER, INTENT(IN)          :: JXSWORK(0:3)  
DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)  
INTEGER                      :: HI, LO  
DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ  
DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, SUBRIO, ZSILL  
DOUBLEPRECISION              :: COEFF (2), rdum
!!***ZQ Module 200520 
DOUBLEPRECISION              :: dzu
DOUBLEPRECISION              :: weirsill
!----------------------------------------------------------------------*
!
! Set up local variables - part 1
!
DZ = ZI (1) - ZI (0)  
SIG = SIGN (ONE, DZ)  
HI = (1 + NINT (SIG) ) / 2  
LO = 1 - HI  
!
! Internal weir
!
! NB: NTYPE 1,8,9,10,11 not allowed internally
!
IF (NTYPE.EQ.7) THEN  
   COEFF (1) = afromCOCBCD (1)  
   SUBRIO = afromCOCBCD (2)  
   ZSILL = afromCOCBCD (3)  
   COEFF (2) = COEFF (1)  
   CALL QWEIR(ZI(HI), ZSILL, ZI(LO), COEFF, SUBRIO, Q(LO), DQ(LO,HI), rdum) !AD ailising
    DQ(LO,LO)=rdum
!!***ZQ Module 200520 
ELSEIF (NTYPE.EQ.12) THEN
    !print*,ZQTableRef,zi(hi)
    CALL ZQTable(ZQTableRef,ZI(HI),Q(LO))
    weirsill=ZQWeirSill(ZQTableRef)
    DZU = DIMJE(ZI(HI), weirsill)
    DQ(LO,HI)=50.0*1.5*sqrt(dzu)                            ! This works for Crummock. Stability during step changes should be tested e.g. for a small area reservoir
    DQ(LO,LO)=0
    write(779,*) zi(hi),Q(lo),dq(lo,hi)
!!***ZQ Module 200520 end
ELSE
    !
    ! Set up local variables - part 2
    !
    DZ = SIG * DZ  
    ROOTDZ = SQRT (DZ)  
    DHH = LI (0) + LI (1)  
    ROOTL = SQRT (DHH)  
    !
    ! CALCULATE FLOW AND DERIVATIVES
    ! NB: CONVM has an implicit upstream weighting factor, ie ALPHA=1
    !
    !CALL OCCODE (ZGI (HI), STR (HI), CW (HI), XA (HI), afromXSwork (:, :, HI), ZI (HI), CONVM, DERIVM)
    CALL OCCODE (ZGI(HI), STR(HI), CW(HI), XA(HI), XSTAB(:, :, jxswork(HI)), ZI(HI), CONVM, DERIVM)
    CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)  
    DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)  
    !     * Note: ZGI(LO),etc are not required
    Q (LO) = CONVM * ROOTDZ / ROOTL  
    DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL  
    DQ (LO, LO) = - DUM / ROOTL  
ENDIF  
Q (HI) = - Q (LO)  
DQ (HI, HI) = - DQ (LO, HI)  
DQ (HI, LO) = - DQ (LO, LO)  
END SUBROUTINE OCQLNK



!SSSSSS SUBROUTINE OCQMLN
SUBROUTINE OCQMLN(ielb, JEL2, LI, ZGI, STR, CW, XA,  ZI, QJ, DQIJ, JXSwork)
!----------------------------------------------------------------------*
!
! CALCULATE FLOW AND DERIVATIVES BETWEEN MULTIPLE CHANNEL LINKS
!
!----------------------------------------------------------------------*
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
!  for j in active_j   LI(j).ge.0                  ZI(j).ge.ZGI(j)
!                    [STR(j),CW(j),XA(j),XS(1,NXSCEE,j)].gt.0
!      for i in 1:NXSCEE-1     XS(1,i,j)=XS(1,NXSCEE,j)*(i-1)/(NXSCEE-1)
!                              XS(2,i,j).ge.0  XS(3,i,j).gt.0
! Exit conditions:
!  asum{QJ(0:3)}=0
!  for j in active_j   asum{DQIJ(0:3,j)}=0
!  for j1,j2 in 0:3 QJ(j1).gt.0.and.QJ(j2).lt.0 only if ZI(j1).lt.ZI(j2)
!----------------------------------------------------------------------*
INTEGER, INTENT(IN)          :: IELb, JEL2 (0:3)  
DOUBLEPRECISION, INTENT(IN)  :: LI (0:3), ZGI (0:3), STR (0:3)  
DOUBLEPRECISION, INTENT(IN)  ::  CW (0:3), XA (0:3), ZI (0:3)
!DOUBLEPRECISION, INTENT(IN)  :: XSwork(3,NXSCEE,0:3)
INTEGER, INTENT(IN)          :: jxswork(0:3)
DOUBLEPRECISION, INTENT(OUT) ::  QJ (0:3), DQIJ (0:3, 0:3)  
! NB:
!     DQIJ(i,j) is defined for active_j only
!
DOUBLEPRECISION             :: ONEPC, WLMIN  
PARAMETER (ONEPC = 1D-2, WLMIN = 1D-3)  
INTEGER                     :: I, J  
DOUBLEPRECISION             :: CSAVE, DSAVE, CI (0:3), DI (0:3), QDUM2 (0:3)  
DOUBLEPRECISION             :: ZINC, ZSAVE, ROOTLI (0:3), ZJ (0:3)  
!----------------------------------------------------------------------*
!
! Calculate conveyance & its derivative (both.ge.0), & set local arrays
!
DO J = 0, 3  
    IF (JEL2 (J) .LE.0) THEN  
        !            * OCNODE uses ROOTLI as a flag
        ROOTLI (J) = zero  
    ELSE  
        ROOTLI (J) = SQRT (LI (J) )  
        ZJ (J) = ZI (J)  
        !CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSwork(:, :, J), ZJ(J), CI(J), DI(J))
        CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, jxswork(J)), ZJ(J), CI(J), DI(J))
    ENDIF  
ENDDO  
!
! Find flows out of node
!
CALL OCNODE (ielb, ZI, CI, DI, ROOTLI, QJ)  
!
! CALC. DQi/DHj
!
DO J = 0, 3  
   IF (JEL2 (J) .LE.0) CYCLE 
    !        * temporarily increase ZJ and recalculate CI,DI
   ZSAVE  = ZJ(J)  
   CSAVE  = CI(J)  
   DSAVE  = DI(J)  
   ZINC   = MAX(WLMIN, (ZSAVE-ZGI(J))*ONEPC)  !zgi is ground elevation
   ZJ (J) = ZSAVE+ZINC  
   !CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSwork(1, 1, J), ZJ(J), CI(J), DI(J) )
   CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSTAB(1, 1, JXSWORK(J)), ZJ(J), CI(J), DI(J) )
                                                                     !++++++++++++++out
    !        * calculate resultant flows & evaluate derivative
   CALL OCNODE (ielb, ZJ, CI, DI, ROOTLI, QDUM2)
                                        !+++++out  
    DO I = 0, 3  
        DQIJ (I, J) = (QDUM2 (I) - QJ (I) ) / ZINC  
    ENDDO  
   ZJ(J) = ZSAVE
   CI(J) = CSAVE  
   DI(J) = DSAVE  
ENDDO  
END SUBROUTINE OCQMLN

!SSSSSS SUBROUTINE conveyan
SUBROUTINE conveyan(str, h, conv, deriv, ty, xa, extra)
!to bring this all to one place (its messy!)
INTEGER, INTENT(IN)         :: ty
DOUBLEPRECISION, INTENT(IN) :: str, & !strickler or strickler*width
                               h      ! depth
DOUBLEPRECISION, INTENT(IN), OPTIONAL :: xa, extra  !!x-sect area
DOUBLEPRECISION, INTENT(OUT)          :: conv, deriv
DOUBLEPRECISION                       :: hm23
DOUBLEPRECISION, PARAMETER            :: mul = 10.0d0/3.0d0
IF(ty==0) THEN
    IF(h<1.0d-9) THEN
        conv = 0.0d0
        deriv = 0.0d0
    ELSEIF(h<1.0d-3) THEN
        !deriv = str * h23min * f23
        !conv  = deriv * h           !LINEARIZE NEAR ZERO
        conv  = str * mul * h * h * (4.0d0 - 1.0d3*h)  !TAKE CARE valid only for threshold of 1 mm
        conv  = conv * xa / h
        deriv = str * mul * h * (8.0d0 - 3.0d3*h)      !TAKE CARE valid only for threshold of 1 mm
    ELSE
        hm23 = h**f23
        conv = str * xa * hm23      !NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
        deriv = str * hm23 * f53
    ENDIF
ELSEIF(ty==1) THEN
    iF(h<1.0d-9) THEN
        conv = 0.0d0
        deriv = 0.0d0
    ELSEIF(h<1.0d-3) THEN
        !deriv = str * h23min * f23
        !conv  = deriv * h           !LINEARIZE NEAR ZERO
        conv  = str * mul * h * h * (4.0d0 - 1.0d3*h)  !TAKE CARE valid only for threshold of 1 mm
        deriv = str * mul * h * (8.0d0 - 3.0d3*h)      !TAKE CARE valid only for threshold of 1 mm
    ELSE
        hm23 = h**f23
        conv = str * h * hm23       !NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
        deriv = str * hm23 * f53
    ENDIF
ELSEIF(ty==2) THEN
    hm23 = h**f23
    conv = str * xa * hm23 
    deriv = conv * (extra / xa + f23 / h)  !is f23 correct here?
ENDIF

!IF(ty<2) THEN
!    IF(h<dzmin) THEN
!        deriv = str * h23min * f23
!        conv  = deriv * h  !LINEARIZE NEAR ZERO (FOR AD)
!        hm23  = zero
!    ELSE
!        hm23 = h**f23
!        conv = str * xo * hm23 
!        deriv = str * hm23 * f53  !str * MAX(h23min, hm23) * f53
!    ENDIF
!ELSE
END SUBROUTINE conveyan

!SSSSSS SUBROUTINE QWEIR 
SUBROUTINE QWEIR (ZU, ZSILL, ZL, COEFF, SUBRIO, Q, DQU, DQL)  
!----------------------------------------------------------------------*
!
!  CALCULATE FLOW AND DERIVATIVES ACROSS A HORIZONTAL-CRESTED WEIR
!
! INPUT PARAMETERS:
!           ZU       - GAUGED HEAD ABOVE THE WEIR
!           ZSILL    - ELEVATION OF THE WEIR SILL
!           ZL       - GAUGED HEAD BELOW THE WEIR
!           COEFF(2) - (CONSTANT) WEIR DISCHARGE COEFFICIENT:
!                      1=DROWNED, 2=UNDROWNED
!           SUBRIO   - SUBMERGENCE RATIO
!
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
DOUBLEPRECISION, INTENT(IN) :: ZU, ZSILL, ZL, SUBRIO, COEFF (2)  
DOUBLEPRECISION, INTENT(OUT) ::  Q, DQU, DQL  
DOUBLEPRECISION CR, DML, DZU, DZL, ROOTDZ  
! NO FLOW ACROSS WEIR
IF (ZU.LT.ZSILL - DZMIN) THEN  
   Q = zero  
   DQU = zero  
   DQL = zero  
ELSE  
   DZU = DIMJE(ZU, ZSILL)  

   DZL = ZL - ZSILL  
! DROWNED WEIR
   IF (DZL.GT.SUBRIO * DZU) THEN  
      ROOTDZ = SQRT (ZU - ZL)  
      DML = MAX (DZMIN, DZL)  
      CR = COEFF (1) * ROOTDZ  
      Q = CR * DZL  
      DQU = COEFF (1) * DML * half / MAX (RDZMIN, ROOTDZ)  
      DQL = CR - DQU  
! UNDROWNED WEIR
   ELSE  
      ROOTDZ = SQRT (DZU)  
      Q = COEFF (2) * DZU * ROOTDZ  
      DQU = COEFF (2) * 1.5D0 * MAX (RDZMIN, ROOTDZ)  
      DQL = zero  
   ENDIF  
ENDIF  
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
INTEGER, INTENT(IN) :: nel, afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)  
DOUBLEPRECISION, INTENT(IN) :: dtoc 
!     NB: QSA is positive in
!     *  NPASS: maximum number of passes through the test loop
!     * UHCRIT: minimum admissible flow rate [L^^2/T]
!     *  HCRIT: minimum admissible surface water depth [L]
!     * HERROR: minimum inoffensive negative surface water depth [L]
!     *  DZMIN: target elevation difference in flow adjustments [L]
INTEGER         :: NPASS  
PARAMETER (NPASS = 100)  
DOUBLEPRECISION :: UHCRIT, HCRIT, HERROR
DOUBLEPRECISION, DIMENSION(nel), INTENT(IN)    :: inhrf
DOUBLEPRECISION, DIMENSION(nel), INTENT(OUT)   :: GGGETHRF
DOUBLEPRECISION, DIMENSION(nel,4), INTENT(IN)  :: inqsa
DOUBLEPRECISION, DIMENSION(nel,4), INTENT(OUT) :: GGGETQSA
PARAMETER (UHCRIT = 1D-7, HCRIT = 1D-7, HERROR = 1D-5)
INTEGER          :: IELc, IFACE, IBR, idum
INTEGER          :: JEL, JFACE, PPP, PASSS, PEL, PEL0, PFACE, PFACE0  
DOUBLEPRECISION  :: DQE, DZE, QE, ZE, DHQ, DHH, DDZ, DQE0, FDQE, H  
DOUBLEPRECISION  :: DQA, DZA, QA, ZA, QQ, QQMIN, Qasum, SGN, ZG, DXY (0:1), rdum4(4)
LOGICAL          :: AOK, QSMALL, HSMALL, FAIL, FAILP, TEST, FLAG (4) 
CHARACTER(132)  :: MSG  
!----------------------------------------------------------------------*
! Control Loop
! ------------
GGGETHRF = inhrf
GGGETQSA = inqsa
aok = .FALSE.
out900 : DO PASSS = 1, NPASS  !AP LOOP PROBLEMS
    IF(aok) THEN
        CYCLE out900  !AD Irreductible entry into loop problem
    ELSE
        AOK = .TRUE.  
    ENDIF
    out400 : DO ielc = 1, NEL  
        ZE = GGGETHRF (ielc)  
        DZE = DTOC / cellarea (ielc)  
        DXY (0) = DXQQ (ielc)  
        DXY (1) = DYQQ (ielc)  
        !           Depth Criterion: flag outflow (D<0) or inflow (D>0) faces
        !           ---------------------------------------------------------
        ZG = ZGRUND (ielc)  
        H = ZE-ZG  
        HSMALL = (H.LT.HCRIT).AND.NOTZERO(H)  
        FDQE = ZERO  
        IF (HSMALL) THEN  
            DQE0 = - H / DZE  
            !^^^^ RAH/SB small flows ^^^^^^^^^^^^^^^^^^^
            SGN = SIGN (ONE, DQE0)  
            Qasum = ZERO  
            DO IFACE = 1, 4  
                QE = GGGETQSA (ielc, IFACE)  
                !^^^^ RAH/SB small flows ^^^^^^^^^^^^^^^^^^^
                FLAG (IFACE) = QE * SGN.LT.ZERO  
                !                   FLAG(IFACE) = QE*DQE0 .LT. ZERO
                IF (FLAG (IFACE) ) Qasum = Qasum + QE  
            ENDDO  
            IF (NOTZERO(Qasum)) FDQE = MAX ( - ONE, DQE0 / Qasum)  
        ENDIF  
        !           Face Loop
        !           ---------
        Qasum = ZERO  
        out300 : DO IFACE = 1, 4  
            QE = GGGETQSA (ielc, IFACE)  
            !              * apply flow criteria to discharges only
            TEST = QE.LT.ZERO  
            IF (HSMALL) TEST = FLAG (IFACE)  
            IF (.NOT.TEST) CYCLE out300 !GOTO 300  
            !                             >>>>>>>>
            QSMALL = - QE.LT.DXY (MOD (IFACE, 2) ) * UHCRIT  
            TEST = QSMALL.OR.HSMALL  
            !              Gradient Criterion & Neighbour Location
            !              ---------------------------------------
            JEL = afromICMREF (ielc, IFACE, 2)  
            IF (JEL.GT.0) THEN  
                !                  * regular face
                JFACE = afromICMREF (ielc, IFACE, 3)  
                FAIL = GGGETHRF (JEL) .GE.ZE  
            ELSEIF (JEL.EQ.0) THEN  
                !                  * external boundary
                FAIL = .FALSE.  
            ELSE  
                !                  * confluence: choose branch with largest flow
                IBR = - JEL  
                QQMIN = ZERO  
                FAIL = .FALSE.  
                out200 : DO PPP = 1, 3  !200  
                    PEL = afromICMRF2 (IBR, PPP, 1)  
                    IF (PEL.LT.1) CYCLE out200 !GOTO 200  
                    PFACE = afromICMRF2 (IBR, PPP, 2)  
                    QQ = GGGETQSA (PEL, PFACE) * QE  
                    FAILP = (GGGETHRF (PEL) .GE.ZE).AND.(QQ.LT.ZERO)  
                    IF ( (FAILP.OR.TEST) .AND.QQ.LT.QQMIN) THEN  
                        JEL = PEL  
                        JFACE = PFACE  
                        QQMIN = QQ  
                    ENDIF  
                    FAIL = FAIL.OR.FAILP  
                    PEL0 = PEL  
                    PFACE0 = PFACE  
                ENDDO out200 !200
                IF (JEL.LT.0) THEN  
                    JEL = PEL0  
                    JFACE = PFACE0  
                ENDIF  
            ENDIF  
            !              Adjustments
            !                 -----------
            IF (FAIL.OR.TEST) THEN  
                AOK = .FALSE.  
                IF (JEL.GT.0) THEN  
                    DZA = DTOC / cellarea (JEL)  
                    ZA = GGGETHRF (JEL)  
                    QA = GGGETQSA (JEL, JFACE)  
                ENDIF  
                IF (HSMALL) THEN  
                    DQE = FDQE * QE  
                ELSEIF (QSMALL) THEN  
                    DQE = - QE  
                ELSE  
                    DDZ = DZMIN + ZA - ZE  
                    DQE = MIN ( + QA, - QE, DDZ / (DZA + DZE) )  
                ENDIF  
                Qasum = Qasum + DQE  
                !CALL SETQSA(ielc, IFACE, QE+DQE)  
                GGGETQSA(ielc, IFACE) = QE+DQE
                ZE = ZE+DQE * DZE  
                IF (JEL.GT.0) THEN  
                    SGN = SIGN (ONE, DQE)  
                    DQA = - SGN * MIN (SGN * DQE, SGN * QA)  
                    Qasum = Qasum + DQA  
                    !CALL SETQSA(JEL, JFACE, QA + DQA)  
                    GGGETQSA(JEL, JFACE) = QA + DQA
                    !CALL SETHRF(JEL, ZA + DQA * DZA) 
                    GGGETHRF(JEL) = ZA + DQA * DZA
                ENDIF  
                IF (.NOT.HSMALL) THEN  
                    DHQ = Qasum * DZE  
                    Qasum = ZERO  
             ! sb 021009 Error message always produced if pass.eq.npass
             IF ((ABS (DHQ) .GT.HERROR) .or.(passs.eq.npass)) THEN
                    rdum4(1)= - QE ; rdum4(2)=- 1D2 * DQE / QE ; idum=IFACE ; rdum4(4)=DHQ !AD
                        WRITE (MSG, 91030) rdum4(1:2),idum,rdum4(4:4)
                        CALL ERROR(WWWARN, 1030, PPPRI, ielc, 0, MSG)  
                    ENDIF  
                ENDIF  
            ENDIF  
        ENDDO out300  
        !           Final Depth Adjustment
        !           ----------------------
        IF (HSMALL) THEN  
            !^^^^ RAH/SB small flows ^^^^^^^^^^^^^^^^^^^
            AOK = .FALSE.  
            DHQ = Qasum * DZE  
            DHH = ZG - ZE  
            ZE = ZG  
            ! sb 021009 Error message always produced if pass.eq.npass
            IF ((ABS (DHQ) + ABS (DHH) .GT.HERROR) .or.(passs.eq.npass)) THEN
            rdum4(1)=H ; rdum4(2)=DHQ ; rdum4(3)=DHH  !AD
                WRITE (MSG, 91024) rdum4(1:3)  
                CALL ERROR(WWWARN, 1024, PPPRI, ielc, 0, MSG)  
            ENDIF  
        ENDIF  
        !CALL SETHRF(ielc, ZE)  
        GGGETHRF(ielc) =ZE
        ! End of Control Loop
        ! -------------------
    ENDDO out400  
    !IF (AOK) EXIT out900 !GOTO 901  
ENDDO out900  
IF(.not.aok) CALL ERROR(WWWARN, 1060, PPPRI, 0, 0, 'OC flow criteria could not be met')

!901 CONTINUE  
!33+15+8+17+30=103

91024 FORMAT( 'Surface water depth adjusted from',SP,1PG15.7,' to zero',         ': depth created =',2G15.7 )
!28+14+11+7+9+2+17+15=103

91030 FORMAT( 'Surface water discharge rate',1PG14.7,' reduced by', &
        0PF7.2,'% at face',I4,': depth created =',SP,1PG15.7 )
END SUBROUTINE OCFIX


END MODULE OCmod2