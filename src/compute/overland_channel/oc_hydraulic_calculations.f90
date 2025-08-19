MODULE oc_hydraulic_calculations
! Module for hydraulic calculations in overland channel flow
! Handles conveyance calculations and weir flow equations
! Extracted from OCmod2.f90 as part of refactoring

   USE SGLOBAL
   USE oc_parameters
   USE oc_data_management, ONLY: XSTAB
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: OCCODE, CONVEYAN, QWEIR

CONTAINS

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
!----------------------------------------------------------------------*
      H = Z - ZG
      HFULL = afromXStypes (1, NXSCEE)

      I = INT((H / HFULL) * DBLE(NXSCEE-1) + one)
      IF (I.LT.NXSCEE) THEN
!         * use look-up tables
         DERIV = afromXStypes (3, I)
         CONV = afromXStypes (2, I) + DERIV * DIMJE(H, afromXStypes (1, I) )
      ELSE
!         * calculate values directly
         XA = afromXAFULL + afromCWIDTH * DIMJE(H, HFULL)
         CALL CONVEYAN(str, h, conv, deriv, 2, xa, afromCWIDTH)
      ENDIF
   END SUBROUTINE OCCODE

!SSSSSS SUBROUTINE conveyan
   SUBROUTINE conveyan(str, h, conv, deriv, ty, xa, extra)
! Unified conveyance calculation subroutine
! Handles different channel types and flow regimes
      INTEGER, INTENT(IN)         :: ty
      DOUBLEPRECISION, INTENT(IN) :: str, & ! strickler or strickler*width
         h      ! depth
      DOUBLEPRECISION, INTENT(IN), OPTIONAL :: xa, extra  ! x-sect area
      DOUBLEPRECISION, INTENT(OUT)          :: conv, deriv
      DOUBLEPRECISION                       :: hm23
      DOUBLEPRECISION, PARAMETER            :: mul = 10.0d0/3.0d0

      IF(ty==0) THEN
         IF(h<1.0d-9) THEN
            conv = 0.0d0
            deriv = 0.0d0
         ELSEIF(h<1.0d-3) THEN
            conv  = str * mul * h * h * (4.0d0 - 1.0d3*h)  ! valid only for threshold of 1 mm
            conv  = conv * xa / h
            deriv = str * mul * h * (8.0d0 - 3.0d3*h)      ! valid only for threshold of 1 mm
         ELSE
            hm23 = h**f23
            conv = str * xa * hm23      ! XA for case 0 but H for case 1
            deriv = str * hm23 * f53
         ENDIF
      ELSEIF(ty==1) THEN
         IF(h<1.0d-9) THEN
            conv = 0.0d0
            deriv = 0.0d0
         ELSEIF(h<1.0d-3) THEN
            conv  = str * mul * h * h * (4.0d0 - 1.0d3*h)  ! valid only for threshold of 1 mm
            deriv = str * mul * h * (8.0d0 - 3.0d3*h)      ! valid only for threshold of 1 mm
         ELSE
            hm23 = h**f23
            conv = str * h * hm23       ! XA for case 0 but H for case 1
            deriv = str * hm23 * f53
         ENDIF
      ELSEIF(ty==2) THEN
         hm23 = h**f23
         conv = str * xa * hm23
         deriv = conv * (extra / xa + f23 / h)
      ENDIF
   END SUBROUTINE conveyan

!SSSSSS SUBROUTINE QWEIR
   SUBROUTINE QWEIR (ZU, ZSILL, ZL, COEFF, SUBRIO, Q, DQU, DQL)
!----------------------------------------------------------------------*
!  CALCULATE FLOW AND DERIVATIVES ACROSS A HORIZONTAL-CRESTED WEIR
!
! INPUT PARAMETERS:
!           ZU       - GAUGED HEAD ABOVE THE WEIR
!           ZSILL    - ELEVATION OF THE WEIR SILL
!           ZL       - GAUGED HEAD BELOW THE WEIR
!           COEFF(2) - (CONSTANT) WEIR DISCHARGE COEFFICIENT:
!                      1=DROWNED, 2=UNDROWNED
!           SUBRIO   - SUBMERGENCE RATIO
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/QWEIR/4.2
! Modifications:
! RAH  980226  4.2  Make COEFF an array, size 2 (was simple variable)
!                   (also in callers OCQBC,OCQLNK).  Explicit typing.
!                   Zero outputs if no flow.  Generic intrinsics.
!                   Add missing term CR to DQL (drowned).
!                   Locals: add DZU,DZL,RDZMIN,CR; scrap ROOTDM.
!      980730       Use MAX to ensure DQU.gt.0 (except "no flow" case).
!                   New locals DZMIN,DML.  SQRT(DZMIN) was 1D-6.
!                   Subtract DZMIN from ZSILL in "no flow" criterion.
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

END MODULE oc_hydraulic_calculations
