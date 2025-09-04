MODULE subsurface_variables
!----------------------------------------------------------------------*
! VSS module variables and type definitions
! Contains all shared variables, parameters, and memory management
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 1-171, 172-176)
!----------------------------------------------------------------------*
   USE SGLOBAL
   IMPLICIT NONE
   PRIVATE

   ! ===== VSS SAVE VARIABLES (moved here for AD) =====
   PUBLIC :: ICSOILsv, JCBCsv, VSAIJsv
   INTEGER         :: ICSOILsv(LLEE,NELEE), JCBCsv(0:5,NELEE)
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: VSAIJsv

   ! ===== TIME-VARYING BOUNDARY CONDITION VARIABLES =====
   PUBLIC :: WLLAST, WLTIME, RWELIN, RLFLST, RLFTIM, RLFPRV
   PUBLIC :: RLHLST, RLHTIM, RLHPRV, RLHNXT, RLGLST, RLGTIM, RLGPRV, RLGNXT
   PUBLIC :: RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT
   PUBLIC :: RLFDUM, RLHDUM, RLGDUM, FIRSTvssim
   DOUBLEPRECISION :: WLLAST=zero, WLTIME=zero, RWELIN(NVSEE)=zero
   DOUBLEPRECISION :: RLFLST=zero, RLFTIM=zero, RLFPRV(NVSEE)=zero
   DOUBLEPRECISION :: RLHLST=zero, RLHTIM=zero, RLHPRV(NVSEE)=zero, RLHNXT(NVSEE)=zero
   DOUBLEPRECISION :: RLGLST=zero, RLGTIM=zero, RLGPRV(NVSEE)=zero, RLGNXT(NVSEE)=zero
   DOUBLEPRECISION :: RBFLST=zero, RBFTIM=zero, RBFPRV(NVSEE)=zero
   DOUBLEPRECISION :: RBHLST=zero, RBHTIM=zero, RBHPRV(NVSEE)=zero, RBHNXT(NVSEE)=zero
   DOUBLEPRECISION :: RLFDUM(NVSEE)=zero, RLHDUM(NVSEE)=zero, RLGDUM(NVSEE)=zero
   LOGICAL :: FIRSTvssim=.TRUE.

   PUBLIC :: errcntallowed
   INTEGER, PARAMETER :: errcntallowed=1000

   ! ===== VSCOM1_INC VARIABLES (Global VSS variables) =====
   ! Logical variables, initialization
   PUBLIC :: BLOWP, BHELEV
   LOGICAL :: BLOWP, BHELEV

   ! Integer variables, initialization
   PUBLIC :: NCSZON, NCRBED, JVSALN, ISRBED
   PUBLIC :: NVSWL, NVSSP, NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD
   PUBLIC :: NVSWLC, NLBTYP, NLBCAT, NBBTYP, NBBCAT
   PUBLIC :: NVSLFT, NVSLFL, NVSLFN, NVSLHT, NVSLHL, NVSLHN
   PUBLIC :: NVSLGT, NVSLGL, NVSLGN
   INTEGER :: NCSZON, NCRBED
   INTEGER :: JVSALN (NELEE, NLYREE, 4), ISRBED (NLFEE)
   INTEGER :: NVSWL, NVSSP, NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD
   INTEGER :: NVSWLC (NELEE), NLBTYP (NELEE)
   INTEGER :: NLBCAT (NELEE), NBBTYP (NELEE), NBBCAT (NELEE)
   INTEGER :: NVSLFT, NVSLFL (NLYREE, NVSEE), NVSLFN (NVSEE)
   INTEGER :: NVSLHT, NVSLHL (NLYREE, NVSEE), NVSLHN (NVSEE)
   INTEGER :: NVSLGT, NVSLGL (NLYREE, NVSEE), NVSLGN (NVSEE)

   ! Integer variables, time-varying
   PUBLIC :: IVSSTO
   INTEGER :: IVSSTO (LLEE, NELEE)

   ! Floating-point variables and arrays, initialization
   PUBLIC :: DCSZON, DCRBED, DCSTOT, DCRTOT, VSZMIN, VSZMAX
   PUBLIC :: VSK3D, DRBED, VSSPZ, VSSPCO, VSWV, VSWL
   DOUBLEPRECISION :: DCSZON (LLEE), DCRBED (LLEE), DCSTOT, DCRTOT, &
      VSZMIN, VSZMAX, VSK3D (NSEE, 3), DRBED (NLFEE), VSSPZ (NELEE), &
      VSSPCO (NELEE), VSWV, VSWL

   ! Floating-point arrays, time-varying
   PUBLIC :: VSKR, WLNOW, RLFNOW, RLHNOW, RLGNOW, RBFNOW, RBHNOW
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSKR !(LLEE, NELEE)
   DOUBLEPRECISION :: WLNOW (NVSEE), RLFNOW (NLYREE, NVSEE), &
      RLHNOW (NLYREE, NVSEE), RLGNOW (NLYREE, NVSEE), RBFNOW (NVSEE), &
      RBHNOW (NVSEE)

   ! ===== VSSOIL_INC VARIABLES (Soil parameter tables) =====
   PUBLIC :: NSOLEE, NVSSOL
   PUBLIC :: VSPPSI, VSPTHE, VSPKR, VSPETA, VSPDTH, VSPDKR, VSPDET
   PUBLIC :: VSPSS, VSPPOR
   INTEGER, PARAMETER :: NSOLEE = 200
   INTEGER :: NVSSOL
   DOUBLEPRECISION :: VSPPSI (NSOLEE), VSPTHE (NSOLEE, NSEE), VSPKR ( &
      NSOLEE, NSEE), VSPETA (NSOLEE, NSEE)
   DOUBLEPRECISION :: VSPDTH (NSOLEE, NSEE), VSPDKR (NSOLEE, NSEE), &
      VSPDET (NSOLEE, NSEE)
   DOUBLEPRECISION :: VSPSS (NSEE), VSPPOR (NSEE)

   ! ===== VSINIT_INC VARIABLES (Initialization variables) =====
   PUBLIC :: BFAST, BSOILP, IVSFLG, IVSNTB, NVSERR, INITYP
   PUBLIC :: VSTRES, VSVGN, VSALPH, VSIPSD, VSZWLB, VSZWLT
   PUBLIC :: TBPSI, TBTHE, TBKR, TBTHEC, TBKRC, VSSPD
   LOGICAL :: BFAST, BSOILP
   INTEGER :: IVSFLG (NSEE), IVSNTB (NSEE), NVSERR, INITYP
   DOUBLEPRECISION :: VSTRES (NSEE), VSVGN (NSEE), VSALPH (NSEE), &
      VSIPSD, VSZWLB (NVSEE), VSZWLT (NVSEE), TBPSI (NVSEE, NSEE), &
      TBTHE (NVSEE, NSEE), TBKR (NVSEE, NSEE), TBTHEC (NVSEE, NSEE), &
      TBKRC (NVSEE, NSEE), VSSPD (NELEE)

   ! Public subroutines
   PUBLIC :: initialise_vsmod

CONTAINS

!SSSSSS SUBROUTINE initialise_vsmod
   SUBROUTINE initialise_vsmod()
!----------------------------------------------------------------------*
! Initialize allocatable arrays for VSS module
!----------------------------------------------------------------------*
      ALLOCATE(vsaijsv(4,top_cell_no,total_no_elements), vskr(top_cell_no,total_no_elements))
   END SUBROUTINE initialise_vsmod

END MODULE subsurface_variables
