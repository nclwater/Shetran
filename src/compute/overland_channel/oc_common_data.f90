MODULE oc_common_data
! Extracted module variables from OCmod.f90
! Contains shared variables for overland channel calculations
   USE SGLOBAL

   IMPLICIT NONE

! Module variables extracted from OCmod
   INTEGER            :: NELIND (NELEE)
   INTEGER            :: NROWF, NROWL, NOCHB, NOCFB
   INTEGER            :: NROWEL (NELEE), NROWST (NYEE+1), NXSECT (NLFEE)
   DOUBLEPRECISION    :: HOCLST, HOCNXT, QFLAST, QFNEXT, TDC, TFC
   DOUBLEPRECISION    :: HOCPRV (NOCTAB), QOCFIN (NOCTAB), HOCNXV (NOCTAB)
   DOUBLEPRECISION    :: XINH (NLFEE, NOCTAB)
   DOUBLEPRECISION    :: XINW (NLFEE, NOCTAB)
   DOUBLEPRECISION    :: XAREA (NLFEE, NOCTAB)
   DOUBLEPRECISION    :: dtoc

   PUBLIC

END MODULE oc_common_data
