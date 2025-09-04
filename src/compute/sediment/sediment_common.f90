MODULE sediment_common
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SY .F files
   USE SGLOBAL
   USE mod_load_filedata, ONLY : ALINIT, ALCHKI, ALCHK, ALALLF, ALREAD

   IMPLICIT NONE
   PRIVATE  ! Make everything private by default

   ! Make shared variables and parameters public for use by other sediment modules
   PUBLIC :: FIRST_syackw, K2_syackw, DGRMAX_syackw, ROOT32_syackw
   PUBLIC :: FIRST_sycltr, k1_sycltr
   PUBLIC :: FIRST_sycrit, K1_sycrit, K2_sycrit, K3_sycrit
   PUBLIC :: FIRST_syengh, KG_syengh
   PUBLIC :: FIRST_syfine, WSED_syfine
   PUBLIC :: FIRST_syovtr, K1_syovtr, K3_syovtr, K4_syovtr
   PUBLIC :: NSYBEE, NSYCEE
   PUBLIC :: ISACKW_symain, ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, NEPS_symain
   PUBLIC :: NFINE_symain, NSYB_symain
   PUBLIC :: NSYBCD_symain, NSYC_symain, NTSOBK_symain, PASS_symain, NTSOTP_symain
   PUBLIC :: ALPHA_symain, CONCOB_symain, DCBEDO_symain, FBIC_symain, FICRIT_symain, FPCRIT_symain, SYNOW_symain
   PUBLIC :: DLSMAX_symain, DDBSED_symain
   PUBLIC :: ABC_symain, ACKW_symain, ARXLOL_symain, BBC_symain
   PUBLIC :: BKB_symain, DBFULL_symain
   PUBLIC :: DRDRIP_symain, DRSED_symain, DRSO50_symain, DWATOL_symain, FCG_symain
   PUBLIC :: FCROCK_symain, FDRIP_symain, FETA_symain, FPCLAY_symain
   PUBLIC :: GBC_symain, GKF_symain, GKR_symain, RHOSO_symain, XDRIP_symain

   LOGICAL         :: FIRST_syackw=.TRUE.
   DOUBLEPRECISION :: K2_syackw, DGRMAX_syackw, ROOT32_syackw

   LOGICAL         :: FIRST_sycltr=.TRUE.
   DOUBLEPRECISION :: k1_sycltr

   LOGICAL          :: FIRST_sycrit=.TRUE.
   DOUBLEPRECISION  :: K1_sycrit, K2_sycrit, K3_sycrit

   LOGICAL         :: FIRST_syengh=.TRUE.
   DOUBLEPRECISION :: KG_syengh

   LOGICAL         :: FIRST_syfine=.TRUE.
   DOUBLEPRECISION :: WSED_syfine


   INTEGER, PARAMETER  :: NSYBEE= 40, NSYCEE=10
   INTEGER          :: ISACKW_symain, ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, NEPS_symain
   INTEGER          :: NFINE_symain, NSYB_symain
   INTEGER          :: NSYBCD_symain(NSYBEE,3), NSYC_symain(4), NTSOBK_symain(NLFEE), PASS_symain=0, NTSOTP_symain(NELEE)
   DOUBLEPRECISION  :: ALPHA_symain, CONCOB_symain, DCBEDO_symain, FBIC_symain, FICRIT_symain, FPCRIT_symain, SYNOW_symain
   DOUBLEPRECISION  :: DLSMAX_symain, DDBSED_symain(NLFEE, NSEDEE)
   DOUBLEPRECISION  :: ABC_symain(NSEDEE, NSYCEE), ACKW_symain(5, NSEDEE), ARXLOL_symain(NLFEE), BBC_symain(NSEDEE, NSYCEE)
   DOUBLEPRECISION  :: BKB_symain(NSEE), DBFULL_symain(NLFEE)
   DOUBLEPRECISION  :: DRDRIP_symain(NVEE), DRSED_symain(NSEDEE), DRSO50_symain(NSEE), DWATOL_symain(NELEE), FCG_symain(NELEE)
   DOUBLEPRECISION  :: FCROCK_symain(NELEE), FDRIP_symain(NVEE), FETA_symain(NELEE), FPCLAY_symain(NSEE)
   DOUBLEPRECISION  :: GBC_symain(NSEDEE, NSYCEE), GKF_symain(NSEE), GKR_symain(NSEE), RHOSO_symain(NSEE), XDRIP_symain(NVEE)

   LOGICAL         :: FIRST_syovtr= .TRUE.
   DOUBLEPRECISION :: K1_syovtr, K3_syovtr, K4_syovtr

CONTAINS

END MODULE sediment_common
