MODULE et_variables
!----------------------------------------------------------------------*
!
! Module containing all variables for evapotranspiration calculations
! Created from refactoring of ETmod.f90
!
!----------------------------------------------------------------------*
   USE SGLOBAL
   IMPLICIT NONE

! Physical constants for evapotranspiration calculations
   DOUBLEPRECISION, PARAMETER :: LAMDA=2465000., &  ! Latent heat of vaporization (J/kg)
      GAMMA=0.659, &                                  ! Psychrometric constant (mbar/C)
      RHO=1.2, &                                     ! Air density (kg/m^3)
      CP=1003.                                       ! Specific heat of air (J/kg/C)

! Logical control variables
   LOGICAL :: BAR (NVEE), BMETP, BINETP, BMETAL

! Integer control and mode arrays
   INTEGER :: MODE (NVEE), NF (NVEE), MEASPE (NVEE)
   INTEGER :: MODECS (NVEE), MODEPL (NVEE), MODECL (NVEE), MODEVH (NVEE)
   INTEGER :: NCTCST (NVEE), NCTPLA (NVEE), NCTCLA (NVEE), NCTVHT (NVEE)

! Resistance and physical property arrays
   DOUBLEPRECISION :: RA (NVEE), RC (NVEE), RTOP (NVEE)
   DOUBLEPRECISION :: CSTCAP (NVEE), CK (NVEE), CB (NVEE), DEL (NVEE)
   DOUBLEPRECISION :: PS1 (NVEE, NUZTAB), PSI4 (LLEE), UZALFA (LLEE)

! Vegetation and temporal arrays
   DOUBLEPRECISION :: FET (NVEE, NUZTAB), CSTCA1 (NVEE), PLAI1 (NVEE)
   DOUBLEPRECISION :: RCF (NVEE, NUZTAB), CLAI1 (NVEE), VHT1 (NVEE)
   DOUBLEPRECISION :: RELCST (NVEE, NVBP), TIMCST (NVEE, NVBP)
   DOUBLEPRECISION :: RELPLA (NVEE, NVBP), TIMPLA (NVEE, NVBP)
   DOUBLEPRECISION :: RELCLA (NVEE, NVBP), TIMCLA (NVEE, NVBP)
   DOUBLEPRECISION :: RELVHT (NVEE, NVBP), TIMVHT (NVEE, NVBP)

! Message buffer
   CHARACTER(132) :: msg

! Make everything private by default
   PRIVATE

! Public interface - only expose what's needed by other modules
   PUBLIC :: BMETP, BINETP, BMETAL, MEASPE, CSTCAP, RC, BAR, RA, MODE, &
      NF, CK, CB, MODECS, MODEPL, MODECL, MODEVH, NCTCST, CSTCA1, &
      RELCST, TIMCST, NCTPLA, PLAI1, RELPLA, TIMPLA, NCTCLA, CLAI1, &
      NCTVHT, VHT1, RELVHT, TIMVHT, PS1, RCF, FET, RTOP, RELCLA, &
      TIMCLA, del, psi4, uzalfa, &
      LAMDA, GAMMA, RHO, CP, msg  ! Physical constants and message buffer

END MODULE et_variables
