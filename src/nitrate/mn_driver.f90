!> summary: Allocation, initialisation and the per-timestep nitrate sequence.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The nitrate component's control flow. [[MNINITIALISE]] allocates the
!> persistent state and workspace, reads and validates the static data,
!> initialises the plant state and the process pools, and clears the two
!> source/sink arrays. Later [[MNCONT]] calls update plant uptake before
!> [[MNMAIN]] reads the scheduled additions, advances the process pools,
!> populates the contaminant source/sink arrays and writes the cumulative
!> output.
!>
!> This is the module the rest of the model sees: [[cm_driver:CMSIM]] calls
!> `MNCONT`, `MNINITIALISE` and `MNISINITIALISED` and nothing else in the
!> component.
!>
!> To preserve legacy timing, the first `CMSIM` call performs only
!> `MNINITIALISE`; its contaminant solve uses zero MN source/sink terms. MN
!> process updates begin on the following `CMSIM` call.
!>
!> @note
!> The implementation overwrites `TA(1:NV)` with 10 deg C in [[MNCONT]].
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_driver

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE array_limits, ONLY: LLEE, NCONEE, nelee, nlfee, NLYREE, NPLTEE, NSEE, NVEE, nxee
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE error_status, ONLY: errstat_alloc
   USE file_units, ONLY: FID_logfile
   USE interpolation, ONLY: ALINTP
   USE mn_state, ONLY: cahum, calit, caman, cdort, chum, chum1, clit, clit1, cman, cman1, &
                       denit, dummy4, dummy6, edeth, emph, emt, enph, ent, gam, gamtmp, &
                       imamm, imdiff, imnit, isimtf, kd1, kd2, khum, klit, kman, knit, &
                       kvol, miner, MN_ALLOCATED_NCETOP, MN_ALLOCATED_NEL, MN_CONFIG, &
                       MN_INITIALISED, MN_WORK, naamm, namm, namm1, nanit, ndnit, ndsnt, &
                       nlit, nlit1, nman, nman1, ntrf, plamm, plnit, plup, pphi, snit, &
                       temp, vol
   USE mn_environment, ONLY: mnedth, mnemph, mnemt, mnenph, mnent, MNTEMP
   USE mn_input, ONLY: MNINT2, MNRED1, MNRED2
   USE mn_nitrogen, ONLY: mnamm, MNGAM, mnltn, mnnit
   USE mn_organic_matter, ONLY: mnco2, mnlthm, mnman
   USE mn_output, ONLY: MNOUT
   USE mn_plant, ONLY: mnplant, MNPLANTINITIALISE
   USE mn_validation, ONLY: MNERR0, MNERR1, MNERR2, MNERR3, MNERR4

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mncont, mninit, mnmain, mninitialise, mnisinitialised

CONTAINS

!> @brief Reports whether the mineral-nitrogen component has completed setup.
   LOGICAL FUNCTION MNISINITIALISED()
      MNISINITIALISED = MN_INITIALISED
   END FUNCTION MNISINITIALISED

!> @brief Allocates persistent mineral-nitrogen state and timestep workspace.
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   SUBROUTINE MNALLOCATE(NEL, NCETOP)
      INTEGER, INTENT(IN) :: NEL, NCETOP

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "mn_driver:MNALLOCATE"

      IF (ALLOCATED(CAHUM)) THEN
         IF (MN_ALLOCATED_NEL /= NEL .OR. MN_ALLOCATED_NCETOP /= NCETOP) &
            CALL RAISE_ERROR(ERRLVL_fatal, 3001, FID_logfile, 0, 0, &
            'MN state was already allocated with different dimensions')
         RETURN
      END IF

      ALLOCATE (CAHUM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CAHUM", location, emsg)
      ALLOCATE (CALIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CALIT", location, emsg)
      ALLOCATE (CAMAN(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CAMAN", location, emsg)
      ALLOCATE (CDORT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CDORT", location, emsg)
      ALLOCATE (CHUM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CHUM", location, emsg)
      ALLOCATE (CHUM1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CHUM1", location, emsg)
      ALLOCATE (CLIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CLIT", location, emsg)
      ALLOCATE (CLIT1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CLIT1", location, emsg)
      ALLOCATE (CMAN(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CMAN", location, emsg)
      ALLOCATE (CMAN1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CMAN1", location, emsg)

      ALLOCATE (DENIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DENIT", location, emsg)
      ALLOCATE (DUMMY4(NCETOP, NEL), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DUMMY4", location, emsg)
      ALLOCATE (DUMMY6(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DUMMY6", location, emsg)

      ALLOCATE (EDETH(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "EDETH", location, emsg)
      ALLOCATE (EMPH(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "EMPH", location, emsg)
      ALLOCATE (EMT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "EMT", location, emsg)
      ALLOCATE (ENPH(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ENPH", location, emsg)
      ALLOCATE (ENT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ENT", location, emsg)

      ALLOCATE (GAM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "GAM", location, emsg)
      ALLOCATE (GAMTMP(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "GAMTMP", location, emsg)
      ALLOCATE (IMAMM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "IMAMM", location, emsg)
      ALLOCATE (IMDIFF(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "IMDIFF", location, emsg)
      ALLOCATE (IMNIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "IMNIT", location, emsg)
      ALLOCATE (ISIMTF(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ISIMTF", location, emsg)

      ALLOCATE (KD1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KD1", location, emsg)
      ALLOCATE (KD2(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KD2", location, emsg)
      ALLOCATE (KHUM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KHUM", location, emsg)
      ALLOCATE (KLIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KLIT", location, emsg)
      ALLOCATE (KMAN(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KMAN", location, emsg)
      ALLOCATE (KNIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KNIT", location, emsg)
      ALLOCATE (KVOL(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KVOL", location, emsg)

      ALLOCATE (MINER(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "MINER", location, emsg)

      ALLOCATE (NAAMM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NAAMM", location, emsg)
      ALLOCATE (NAMM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NAMM", location, emsg)
      ALLOCATE (NAMM1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NAMM1", location, emsg)
      ALLOCATE (NANIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NANIT", location, emsg)
      ALLOCATE (NDNIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NDNIT", location, emsg)
      ALLOCATE (NDSNT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NDSNT", location, emsg)
      ALLOCATE (NLIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NLIT", location, emsg)
      ALLOCATE (NLIT1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NLIT1", location, emsg)
      ALLOCATE (NMAN(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NMAN", location, emsg)
      ALLOCATE (NMAN1(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NMAN1", location, emsg)
      ALLOCATE (NTRF(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NTRF", location, emsg)

      ALLOCATE (PLAMM(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PLAMM", location, emsg)
      ALLOCATE (PLNIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PLNIT", location, emsg)
      ALLOCATE (PLUP(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PLUP", location, emsg)
      ALLOCATE (PPHI(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PPHI", location, emsg)

      ALLOCATE (SNIT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "SNIT", location, emsg)
      ALLOCATE (TEMP(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TEMP", location, emsg)
      ALLOCATE (VOL(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "VOL", location, emsg)

      ALLOCATE (MN_WORK%IDUM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "IDUM", location, emsg)
      ALLOCATE (MN_WORK%DUMMY(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DUMMY", location, emsg)
      ALLOCATE (MN_WORK%LDUM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "LDUM", location, emsg)

      ALLOCATE (MN_WORK%CDPTHB(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CDPTHB", location, emsg)
      ALLOCATE (MN_WORK%CLTFCT(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CLTFCT", location, emsg)
      ALLOCATE (MN_WORK%CMNFCT(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CMNFCT", location, emsg)
      ALLOCATE (MN_WORK%CNRAL(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CNRAL", location, emsg)
      ALLOCATE (MN_WORK%CNRALT(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CNRALT", location, emsg)
      ALLOCATE (MN_WORK%CNRAM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CNRAM", location, emsg)
      ALLOCATE (MN_WORK%CNRAMN(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CNRAMN", location, emsg)
      ALLOCATE (MN_WORK%CTOT(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CTOT", location, emsg)
      ALLOCATE (MN_WORK%NAMFCT(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NAMFCT", location, emsg)
      ALLOCATE (MN_WORK%NDPTHB(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NDPTHB", location, emsg)
      ALLOCATE (MN_WORK%NTOT(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NTOT", location, emsg)

      MN_ALLOCATED_NEL = NEL
      MN_ALLOCATED_NCETOP = NCETOP
   END SUBROUTINE MNALLOCATE

!> @brief Performs the explicit one-time setup for the mineral-nitrogen component.
!>
!> `CMSIM` calls this routine instead of advancing MN on its first call after
!> contaminant setup. This deliberately preserves the legacy one-call delay:
!> initial MN source/sink terms are zero for that contaminant solve, and the
!> first MN process timestep occurs on the following `CMSIM` call.
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   SUBROUTINE MNINITIALISE(MND, MNFC, MNFN, MNPL, MNPR, MNOUTPL, NCETOP, NCON, NEL, NLF, NS, NV, NX, NY, &
      ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NVC, NLYRBT, NTSOIL, D0, TIH, RHOPL, Z2, &
      DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, PLAI, ZVSNOD, BEXBK, LINKNS, CLAI, TA, SSS1, SSS2)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: MND, MNFC, MNFN, MNPL, MNPR, MNOUTPL
      INTEGER, INTENT(IN) :: NCETOP, NCON, NEL, NLF, NS, NV, NX, NY
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMREF(NELEE, 4, 2:2), ICMXY(NXEE, NY)
      INTEGER, INTENT(IN) :: NVC(NELEE), NLYRBT(NEL, NLYREE), NTSOIL(NEL, NLYREE)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE), NLYR(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: D0, TIH, RHOPL, Z2
      DOUBLE PRECISION, INTENT(IN) :: DELONE(NPLTEE), PLAI(NV), CLAI(NV)
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE), DYQQ(NELEE), VSPOR(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL), ZVSNOD(LLEE, NEL), TA(NV)
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1), SSS2(NEL, NCETOP + 1)
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)

      INTEGER, PARAMETER :: NMNEEE = 9, NMNTEE = 10
      INTEGER :: NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E
      INTEGER :: NMN27E, NMN43E, NMN53E
      INTEGER, ALLOCATABLE :: CELEM(:), KD1ELM(:), KD2ELM(:), KHELEM(:), KLELEM(:)
      INTEGER, ALLOCATABLE :: KMELEM(:), KNELEM(:), KVELEM(:), NAELEM(:)
      INTEGER :: NMN15T(NMNEEE), NMN17T(NMNEEE), NMN19T(NMNEEE)
      INTEGER :: NMN21T(NMNEEE), NMN23T(NMNEEE), NMN25T(NMNEEE)
      INTEGER :: NMN27T(NMNEEE), NMN43T(NMNEEE), NMN53T(NMNEEE)
      INTEGER, ALLOCATABLE :: DUMMY2(:, :), IDUM1X(:)
      INTEGER :: DUMMY3(NLYREE)
      DOUBLE PRECISION :: CLITFR, CNRLIT
      DOUBLE PRECISION, ALLOCATABLE :: CTOTTP(:), DAMHLF(:), DCHLF(:), NAMTOP(:)
      DOUBLE PRECISION :: CCONC(NMNEEE, NMNTEE), CDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KD1CNC(NMNEEE, NMNTEE), KD1DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KD2CNC(NMNEEE, NMNTEE), KD2DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KHCONC(NMNEEE, NMNTEE), KHDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KLCONC(NMNEEE, NMNTEE), KLDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KMCONC(NMNEEE, NMNTEE), KMDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KNCONC(NMNEEE, NMNTEE), KNDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KVCONC(NMNEEE, NMNTEE), KVDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: NACONC(NMNEEE, NMNTEE), NADPTH(NMNEEE, NMNTEE)
      LOGICAL :: ISICCD, ISIAMD
      LOGICAL :: LDUM2(LLEE)

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "mn_driver:MNINITIALISE"

      IF (MN_INITIALISED) CALL RAISE_ERROR(ERRLVL_fatal, 3002, MNPR, 0, 0, &
         'MNINITIALISE was called more than once')

      CALL MNALLOCATE(NEL, NCETOP)
      TA(1:NV) = 10.0D0
      CALL MNPLANTINITIALISE(MNPL, MNOUTPL, NEL, NLF, NV, NVC, RHOPL, DELONE, DXQQ, DYQQ, PLAI, CLAI)

      ALLOCATE (CELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CELEM", location, emsg)
      ALLOCATE (KD1ELM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KD1ELM", location, emsg)
      ALLOCATE (KD2ELM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KD2ELM", location, emsg)
      ALLOCATE (KHELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KHELEM", location, emsg)
      ALLOCATE (KLELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KLELEM", location, emsg)
      ALLOCATE (KMELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KMELEM", location, emsg)
      ALLOCATE (KNELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KNELEM", location, emsg)
      ALLOCATE (KVELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KVELEM", location, emsg)
      ALLOCATE (NAELEM(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NAELEM", location, emsg)

      ALLOCATE (DUMMY2(NLYREE, NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DUMMY2", location, emsg)
      ALLOCATE (IDUM1X(NELEE + 3), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "IDUM1X", location, emsg)

      ALLOCATE (CTOTTP(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CTOTTP", location, emsg)
      ALLOCATE (DAMHLF(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DAMHLF", location, emsg)
      ALLOCATE (DCHLF(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DCHLF", location, emsg)
      ALLOCATE (NAMTOP(NELEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NAMTOP", location, emsg)

      CALL MNERR0(LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL, NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE, NV, &
         NVEE, NX, NXEE, NY)
      CALL MNERR1(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, &
         NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, ZVSNOD, BEXBK, LINKNS, DUMMY2, DUMMY3, MN_WORK%IDUM, IDUM1X, &
         MN_WORK%LDUM, LDUM2)
      CALL MNRED1(MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, BEXBK, LINKNS, &
         MN_CONFIG%NBOTCE, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, CELEM(NLF + 1:NEL), &
         KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), &
         KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, &
         NMN27T, NMN43T, NMN53T, MN_CONFIG%AMMDDR, MN_CONFIG%AMMWDR, CLITFR, MN_CONFIG%CNRBIO, MN_CONFIG%CNRHUM, CNRLIT, &
         MN_CONFIG%FE, MN_CONFIG%FH, MN_CONFIG%GNN, MN_CONFIG%KPLAMM, MN_CONFIG%KPLNIT, MN_CONFIG%KUAMM, MN_CONFIG%KUNIT, &
         MN_CONFIG%MNCREF, MN_CONFIG%NITDDR, MN_CONFIG%NITWDR, MN_CONFIG%Q10M, MN_CONFIG%Q10N, CCONC, CDPTH, &
         CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), KD1CNC, KD1DTH, KD2CNC, KD2DTH, MN_CONFIG%KDDSOL, &
         KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), &
         ISICCD, ISIAMD, MN_CONFIG%ISQ10, MN_WORK%IDUM, MN_WORK%DUMMY)
      CALL MNERR2(MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, &
         NMN53E, NMNEEE, NMNTEE, NS, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), &
         KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NMN15T, &
         NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, MN_CONFIG%AMMDDR, MN_CONFIG%AMMWDR, CLITFR, &
         MN_CONFIG%CNRBIO, MN_CONFIG%CNRHUM, CNRLIT, MN_CONFIG%FE, MN_CONFIG%FH, MN_CONFIG%GNN, MN_CONFIG%KPLAMM, &
         MN_CONFIG%KPLNIT, MN_CONFIG%KUAMM, MN_CONFIG%KUNIT, MN_CONFIG%MNCREF, MN_CONFIG%NITDDR, MN_CONFIG%NITWDR, &
         MN_CONFIG%Q10M, MN_CONFIG%Q10N, CCONC, CDPTH, CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), &
         KD1CNC, KD1DTH, KD2CNC, KD2DTH, MN_CONFIG%KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, &
         KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), ISICCD, ISIAMD, MN_WORK%LDUM)
      CALL MNINIT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, &
         NMN53E, NMNEEE, NMNTEE, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), &
         KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NCOLMB, &
         NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, CLITFR, CNRLIT, CCONC, CDPTH, &
         CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), DELTAZ, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KHCONC, &
         KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), &
         ZVSNOD, ISICCD, ISIAMD, SSS1, SSS2, MN_CONFIG%ISBOTC)

      MN_INITIALISED = .TRUE.
   END SUBROUTINE MNINITIALISE

!> @brief Controls the mineral nitrogen component from the contaminant timestep.
!>
!> `MNCONT` is called by [[cm_driver:CMSIM]] after [[mninitialise]] has allocated
!> and initialised the mineral-nitrogen component. It computes potential plant
!> nitrogen uptake with [[mnplant]], then calls [[mnmain]] to advance mineral
!> nitrogen state and fill `SSS1` and `SSS2`, which replace the contaminant
!> source/sink arrays used by the CM transport equations.
!>
!> | Phase | Main action |
!> |:------|:------------|
!> | Temporary temperature setup | Set every vegetation air-temperature entry `TA(1:NV)` to 10.0 before plant uptake and the main nitrogen update. |
!> | Plant uptake | Call [[mnplant]] to calculate nitrogen plant uptake demand and related plant output. |
!> | Main MN update | Call [[mnmain]] to initialise/check/read inputs on the first pass and then update ammonium/nitrate source-sink terms. |
!>
!> The dissolved nitrate concentration fields are supplied through the CM arrays
!> `cccc` and `ssss`; ammonium, litter, humus, manure, and process-rate pools are
!> held internally by `MNmod`. Rates and pools are evaluated over land columns
!> from `NLF+1:NEL`; channel links are not treated as nitrogen soil columns.
!>
!> @note `MNCONT` overwrites the incoming `TA` values with 10 deg C before
!> [[mnplant]] and [[mnmain]] are called.
!> @endnote
!>
!> @warning The legacy source comments note that [[mnplant]] has limited input
!> checking. The main nitrogen update path performs more extensive validation in
!> [[mnerr0]], [[mnerr1]], [[mnerr2]], [[mnerr3]], and [[mnerr4]].
!> @endwarning
!>
!> @warning [[cm_driver:CMSIM]] passes `ICMREF(1:NEL,5)` to the explicit-shape
!> `ICMREF(NEL,4,2:2)` dummy. The MN checks then index four faces, relying on
!> contiguous storage from columns 5--8 beyond the declared one-column actual
!> section. This retained coupling is compiler-sensitive and is not changed
!> here.
!> @endwarning
   SUBROUTINE MNCONT(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, &
      ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NRD, NLYRBT, NTSOIL, &
      D0, TIH, RHOPL, Z2, DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, RDF, ZVSNOD, BEXBK, &
      LINKNS, DTUZ, UZNOW, CLAI, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, SSS1, SSS2)

      IMPLICIT NONE

      ! --- Input arguments ---
      ! Static
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: MNOUT1  !! Carbon budget output unit.
      INTEGER, INTENT(IN) :: MNOUT2  !! Nitrogen budget output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation/meteorological entries.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLF, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NEL, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NX, NY)  !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, *)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, *)  !! Soil type index for each element layer.

      DOUBLE PRECISION, INTENT(IN) :: D0  !! Reference diffusion/dispersion scale used by CM.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: RHOPL  !! Plant dry-matter density used by uptake calculation.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale used by CM and MN temperature diffusion.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLF)  !! True for north-south channel links.

      ! Varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: VSPSI(NCETOP, NEL)  !! Matric potential/pressure head by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.

      ! --- In/Out arguments (Propagated up from MNMAIN / MNPLANT strict architectures) ---
      INTEGER, INTENT(INOUT) :: NCOLMB(NEL)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(INOUT) :: NLYR(NEL)  !! Number of soil layers in each element.
      INTEGER, INTENT(INOUT) :: NRD(NV)  !! Rooting depth in cell counts by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELONE(*)  !! Initial plant biomass/cover scaling by plant type.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NEL)  !! Element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NEL)  !! Element length.
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)  !! Soil porosity by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(*)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: RDF(NV, *)  !! Root density fraction by vegetation type and cell.
      DOUBLE PRECISION, INTENT(INOUT) :: ZVSNOD(*)  !! Vertical node elevation/depth by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI(NV)  !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NEL)  !! Net precipitation/effective rainfall by element.
      DOUBLE PRECISION, INTENT(INOUT) :: TA(NV)  !! Air temperature overwritten with 10 deg C before MN calculations.

      ! --- Output arguments ---
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1)  !! Dynamic-region CM source/sink array.
      DOUBLE PRECISION, INTENT(OUT) :: SSS2(NEL, NCETOP + 1)  !! Dead-space CM source/sink array.

      ! --- Local variables ---
      INTEGER :: I

      !----------------------------------------------------------------------*

      IF (.NOT. MN_INITIALISED) CALL RAISE_ERROR(ERRLVL_fatal, 3003, MNPR, 0, 0, &
         'MNCONT called before MNINITIALISE')

      ! Retained MN behaviour: use a fixed 10 deg C temperature input.
      DO I = 1, NV
         TA(I) = 10.0D0
      END DO

      CALL MNPLANT(NCETOP, NEL, NLF, NV, NCOLMB, NRD, RHOPL, DELONE, DXQQ, DYQQ, DELTAZ, RDF, DTUZ, UZNOW, CLAI)

      CALL MNMAIN(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, ICMBK, &
         ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, &
         ZVSNOD, BEXBK, LINKNS, DTUZ, UZNOW, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, &
         SSS1, SSS2)

   END SUBROUTINE MNCONT

!> @brief Initialises MN pools, parameters, and source/sink terms.
!>
!> `mninit` prepares the land-column MN state over `NLF+1:NEL` and
!> `NCOLMB(element):NCETOP`. It first clears immobilisation-deficit state
!> (`IMDIFF=0`, `ISIMTF=.false.`), then initialises carbon, ammonium, and
!> depth-varying process parameters.
!>
!> | Quantity | Mode | Implemented calculation |
!> | --- | --- | --- |
!> | Initial organic carbon | `ISICCD` true | Exponential profile \(C=C_{top}\exp(-0.693\,z/D_{1/2})\), using `CTOTTP` and `DCHLF`; `CLIT1=CLITFR*C`, `CHUM1=(1-CLITFR)*C`, `NLIT1=CLIT1/CNRLIT`, and manure pools start at zero. |
!> | Initial organic carbon | `ISICCD` false | Interpolate category/profile table `CELEM`, `CCONC`, `CDPTH` with `ALINTP`; split the interpolated total using `CLITFR`, derive `NLIT1`, and set manure pools to zero. |
!> | Initial ammonium | `ISIAMD` true | Exponential profile \(N_{amm}=NAMTOP\exp(-0.693\,z/DAMHLF)\). |
!> | Initial ammonium | `ISIAMD` false | Interpolate category/profile table `NAELEM`, `NACONC`, `NADPTH` with `ALINTP`. |
!> | Process parameters | Always table-based | Interpolate `KHUM`, `KLIT`, `KMAN`, `KNIT`, `KVOL`, `KD1`, and `KD2` from their category/profile tables with `ALINTP`. |
!>
!> The profile depth `z` starts at half the top-cell thickness and then advances
!> downward using adjacent `ZVSNOD` differences. After interpolation,
!> `ISBOTC` is true only if the configured `NBOTCE` is at or below every land
!> column bottom (`NBOTCE >= NCOLMB(element)` for all land elements), and the CM
!> source/sink arrays `SSS1` and `SSS2` are reset to zero.
   SUBROUTINE MNINIT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
      NMNEEE, NMNTEE, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, KMELEM, KNELEM, KVELEM, NAELEM, NCOLMB, NMN15T, NMN17T, NMN19T, NMN21T, &
      NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, CLITFR, CNRLIT, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, DELTAZ, KD1CNC, KD1DTH, KD2CNC, &
      KD2DTH, KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ZVSNOD, ISICCD, &
      ISIAMD, SSS1, SSS2, ISBOTC)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: NBOTCE  !! Requested lower active cell for nitrogen transformations.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: NMN15E  !! Number of humus category entries.
      INTEGER, INTENT(IN) :: NMN17E  !! Number of litter category entries.
      INTEGER, INTENT(IN) :: NMN19E  !! Number of manure category entries.
      INTEGER, INTENT(IN) :: NMN21E  !! Number of nitrification category entries.
      INTEGER, INTENT(IN) :: NMN23E  !! Number of volatilisation category entries.
      INTEGER, INTENT(IN) :: NMN25E  !! Number of KD1 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN27E  !! Number of KD2 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN43E  !! Number of initial-carbon category entries.
      INTEGER, INTENT(IN) :: NMN53E  !! Number of initial-ammonium category entries.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: CELEM(NLF + 1:NEL)  !! Initial-carbon category by element.
      INTEGER, INTENT(IN) :: KD1ELM(NLF + 1:NEL)  !! KD1 denitrification category by element.
      INTEGER, INTENT(IN) :: KD2ELM(NLF + 1:NEL)  !! KD2 denitrification category by element.
      INTEGER, INTENT(IN) :: KHELEM(NLF + 1:NEL)  !! Humus decomposition category by element.
      INTEGER, INTENT(IN) :: KLELEM(NLF + 1:NEL)  !! Litter decomposition category by element.
      INTEGER, INTENT(IN) :: KMELEM(NLF + 1:NEL)  !! Manure decomposition category by element.
      INTEGER, INTENT(IN) :: KNELEM(NLF + 1:NEL)  !! Nitrification category by element.
      INTEGER, INTENT(IN) :: KVELEM(NLF + 1:NEL)  !! Volatilisation category by element.
      INTEGER, INTENT(IN) :: NAELEM(NLF + 1:NEL)  !! Initial-ammonium category by element.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: NMN15T(NMNEEE)  !! Humus table length by category.
      INTEGER, INTENT(IN) :: NMN17T(NMNEEE)  !! Litter table length by category.
      INTEGER, INTENT(IN) :: NMN19T(NMNEEE)  !! Manure table length by category.
      INTEGER, INTENT(IN) :: NMN21T(NMNEEE)  !! Nitrification table length by category.
      INTEGER, INTENT(IN) :: NMN23T(NMNEEE)  !! Volatilisation table length by category.
      INTEGER, INTENT(IN) :: NMN25T(NMNEEE)  !! KD1 table length by category.
      INTEGER, INTENT(IN) :: NMN27T(NMNEEE)  !! KD2 table length by category.
      INTEGER, INTENT(IN) :: NMN43T(NMNEEE)  !! Initial-carbon table length by category.
      INTEGER, INTENT(IN) :: NMN53T(NMNEEE)  !! Initial-ammonium table length by category.

      DOUBLE PRECISION, INTENT(IN) :: CLITFR  !! Fraction of initial organic carbon assigned to litter.
      DOUBLE PRECISION, INTENT(IN) :: CNRLIT  !! Initial litter carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CCONC(NMNEEE, NMNTEE)  !! Initial-carbon profile values.
      DOUBLE PRECISION, INTENT(IN) :: CDPTH(NMNEEE, NMNTEE)  !! Initial-carbon profile depths.
      DOUBLE PRECISION, INTENT(IN) :: CTOTTP(NLF + 1:NEL)  !! Top total-carbon value for decay initialisation.
      DOUBLE PRECISION, INTENT(IN) :: DAMHLF(NLF + 1:NEL)  !! Ammonium decay half-depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DCHLF(NLF + 1:NEL)  !! Carbon decay half-depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: KD1CNC(NMNEEE, NMNTEE)  !! KD1 denitrification profile values.
      DOUBLE PRECISION, INTENT(IN) :: KD1DTH(NMNEEE, NMNTEE)  !! KD1 denitrification profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KD2CNC(NMNEEE, NMNTEE)  !! KD2 denitrification profile values.
      DOUBLE PRECISION, INTENT(IN) :: KD2DTH(NMNEEE, NMNTEE)  !! KD2 denitrification profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KHCONC(NMNEEE, NMNTEE)  !! Humus decomposition profile values.
      DOUBLE PRECISION, INTENT(IN) :: KHDPTH(NMNEEE, NMNTEE)  !! Humus decomposition profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KLCONC(NMNEEE, NMNTEE)  !! Litter decomposition profile values.
      DOUBLE PRECISION, INTENT(IN) :: KLDPTH(NMNEEE, NMNTEE)  !! Litter decomposition profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KMCONC(NMNEEE, NMNTEE)  !! Manure decomposition profile values.
      DOUBLE PRECISION, INTENT(IN) :: KMDPTH(NMNEEE, NMNTEE)  !! Manure decomposition profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KNCONC(NMNEEE, NMNTEE)  !! Nitrification profile values.
      DOUBLE PRECISION, INTENT(IN) :: KNDPTH(NMNEEE, NMNTEE)  !! Nitrification profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KVCONC(NMNEEE, NMNTEE)  !! Volatilisation profile values.
      DOUBLE PRECISION, INTENT(IN) :: KVDPTH(NMNEEE, NMNTEE)  !! Volatilisation profile depths.
      DOUBLE PRECISION, INTENT(IN) :: NACONC(NMNEEE, NMNTEE)  !! Initial-ammonium profile values.
      DOUBLE PRECISION, INTENT(IN) :: NADPTH(NMNEEE, NMNTEE)  !! Initial-ammonium profile depths.
      DOUBLE PRECISION, INTENT(IN) :: NAMTOP(NLF + 1:NEL)  !! Top ammonium value for decay initialisation.
      DOUBLE PRECISION, INTENT(IN) :: ZVSNOD(LLEE, NEL)  !! Vertical node elevation/depth by cell and element.

      LOGICAL, INTENT(IN) :: ISICCD  !! True when initial carbon uses decay-function input.
      LOGICAL, INTENT(IN) :: ISIAMD  !! True when initial ammonium uses decay-function input.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1)  !! Dynamic-region CM source/sink array reset by this routine.
      DOUBLE PRECISION, INTENT(OUT) :: SSS2(NEL, NCETOP + 1)  !! Dead-space CM source/sink array reset by this routine.
      LOGICAL, INTENT(OUT) :: ISBOTC  !! True when `NBOTCE` is valid for all land columns.

      ! Locals
      INTEGER :: NCL, NELM
      DOUBLE PRECISION :: CTOT, DEPTH

      !-------------------------------------------------------------------*

      ! Initialize control arrays
      init_loop: DO NELM = NLF + 1, NEL
         DO NCL = NCOLMB(NELM), NCETOP
            IMDIFF(NELM, NCL) = 0.0D0
            ISIMTF(NELM, NCL) = .FALSE.
         END DO
      END DO init_loop

      ! * calculation of the initial conc. in the carbon pools
      ! * ----------------------------------------------------
      IF (ISICCD) THEN
         ! * an exponential decay rate down the column is used
         decay_c_loop: DO NELM = NLF + 1, NEL
            DO NCL = NCETOP, NCOLMB(NELM), -1
               IF (NCL == NCETOP) THEN
                  DEPTH = DELTAZ(NCETOP, NELM)/2.0D0
               ELSE
                  DEPTH = DEPTH + (ZVSNOD(NCL + 1, NELM) - ZVSNOD(NCL, NELM))
               END IF

               ! * concentration in the organic pools, the manure pool is set to 0
               CTOT = CTOTTP(NELM)*EXP(-0.693D0*DEPTH/DCHLF(NELM))
               CLIT1(NELM, NCL) = CTOT*CLITFR
               CHUM1(NELM, NCL) = CTOT*(1.0D0 - CLITFR)
               NLIT1(NELM, NCL) = CLIT1(NELM, NCL)/CNRLIT
               CMAN1(NELM, NCL) = 0.0D0
               NMAN1(NELM, NCL) = 0.0D0
            END DO
         END DO decay_c_loop
      ELSE
         ! * typical columns are used with linear interpolation between table values
         CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN43E, NMNEEE, NMNTEE, CELEM, NCOLMB(NLF + 1:NEL), NMN43T, &
            CCONC, CDPTH, DELTAZ, ZVSNOD, DUMMY6)

         interp_c_loop: DO NELM = NLF + 1, NEL
            DO NCL = NCOLMB(NELM), NCETOP
               CLIT1(NELM, NCL) = CLITFR*DUMMY6(NELM, NCL)
               CHUM1(NELM, NCL) = (1.0D0 - CLITFR)*DUMMY6(NELM, NCL)
               CMAN1(NELM, NCL) = 0.0D0
               NLIT1(NELM, NCL) = CLIT1(NELM, NCL)/CNRLIT
               NMAN1(NELM, NCL) = 0.0D0
            END DO
         END DO interp_c_loop
      END IF

      ! * calculation of the initial conc. in the ammonium pool
      ! * ----------------------------------------------------
      IF (ISIAMD) THEN
         ! * exponential decay
         decay_n_loop: DO NELM = NLF + 1, NEL
            DO NCL = NCETOP, NCOLMB(NELM), -1
               IF (NCL == NCETOP) THEN
                  DEPTH = DELTAZ(NCETOP, NELM)/2.0D0
               ELSE
                  DEPTH = DEPTH + (ZVSNOD(NCL + 1, NELM) - ZVSNOD(NCL, NELM))
               END IF
               NAMM1(NELM, NCL) = NAMTOP(NELM)*EXP(-0.693D0*DEPTH/DAMHLF(NELM))
            END DO
         END DO decay_n_loop
      ELSE
         ! * typical columns are used with linear interpolation between table values
         CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN53E, NMNEEE, NMNTEE, NAELEM, NCOLMB(NLF + 1:NEL), NMN53T, &
            NACONC, NADPTH, DELTAZ, ZVSNOD, NAMM1)
      END IF

      ! * calculation of the initial values for the decomposition params
      ! * --------------------------------------------------------------

      ! * khum
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN15E, NMNEEE, NMNTEE, KHELEM, NCOLMB(NLF + 1:NEL), NMN15T, &
         KHCONC, KHDPTH, DELTAZ, ZVSNOD, KHUM)

      ! * klit
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN17E, NMNEEE, NMNTEE, KLELEM, NCOLMB(NLF + 1:NEL), NMN17T, &
         KLCONC, KLDPTH, DELTAZ, ZVSNOD, KLIT)

      ! * kman
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN19E, NMNEEE, NMNTEE, KMELEM, NCOLMB(NLF + 1:NEL), NMN19T, &
         KMCONC, KMDPTH, DELTAZ, ZVSNOD, KMAN)

      ! * knit
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN21E, NMNEEE, NMNTEE, KNELEM, NCOLMB(NLF + 1:NEL), NMN21T, &
         KNCONC, KNDPTH, DELTAZ, ZVSNOD, KNIT)

      ! * kvol
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN23E, NMNEEE, NMNTEE, KVELEM, NCOLMB(NLF + 1:NEL), NMN23T, &
         KVCONC, KVDPTH, DELTAZ, ZVSNOD, KVOL)

      ! * kd1
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN25E, NMNEEE, NMNTEE, KD1ELM, NCOLMB(NLF + 1:NEL), NMN25T, &
         KD1CNC, KD1DTH, DELTAZ, ZVSNOD, KD1)

      ! * kd2
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN27E, NMNEEE, NMNTEE, KD2ELM, NCOLMB(NLF + 1:NEL), NMN27T, &
         KD2CNC, KD2DTH, DELTAZ, ZVSNOD, KD2)

      ! * calculation of whether the specified bottom cell is greater
      ! * than the bottom cell in any of the soil columns. if this is
      ! * the case isbotc is true
      ISBOTC = .TRUE.
      DO NELM = NLF + 1, NEL
         IF (NBOTCE < NCOLMB(NELM)) THEN
            ISBOTC = .FALSE.
         END IF
      END DO

      ! * set the source/sink terms to zero
      sink_zero_loop: DO NELM = NLF + 1, NEL
         DO NCL = NCOLMB(NELM), NCETOP
            SSS1(NELM, NCL) = 0.0D0
            SSS2(NELM, NCL) = 0.0D0
         END DO
      END DO sink_zero_loop

   END SUBROUTINE MNINIT

!> @brief Advances the explicitly initialised mineral-nitrogen component.
!>
!> [[mninitialise]] performs all static checks, reads the MND file, initialises
!> process state, and allocates the persistent timestep workspace. `MNMAIN`
!> therefore contains only the timestep update and performs no heap allocation.
!>
!> | Phase | Call order | Purpose |
!> | --- | --- | --- |
!> | Timestep input | [[mnerr3]] -> [[mnred2]] -> [[mnerr4]] -> [[mnint2]] | Check dynamic CM-MN state, read scheduled MNFC/MNFN additions, validate them, and convert concentrations/additions/deposition to cell-based rates. |
!> | Environment | [[mntemp]] -> [[mnemt]] -> [[mnent]] -> [[mnemph]] -> [[mnenph]] -> [[mnedth]] | Update soil temperature and temperature, matric-potential, and saturation response factors. |
!> | Carbon and nitrogen pools | [[mnman]] -> [[mnlthm]] -> [[mnltn]] -> [[mnco2]] -> [[mngam]] -> [[mnamm]] -> [[mnnit]] | Update manure, litter, humus, carbon dioxide production, mineralisation/immobilisation, ammonium, and nitrate source/sink terms. |
!> | Output | [[mnout]] | Write requested detailed MN diagnostics. |
!>
!> Static parameters read by [[mnred1]], including deposition rates, Q10 values,
!> reaction constants, `MNCREF`, and `ISBOTC`, are retained in `MN_CONFIG`.
   SUBROUTINE MNMAIN(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, ICMBK, &
      ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, &
      ZVSNOD, BEXBK, LINKNS, DTUZ, UZNOW, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, &
      SSS1, SSS2)

      IMPLICIT NONE

      ! Input arguments
      ! * static
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: MNOUT1  !! Carbon budget output unit.
      INTEGER, INTENT(IN) :: MNOUT2  !! Nitrogen budget output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation/meteorological entries.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: D0  !! Reference diffusion/dispersion scale used by CM.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale used by CM and MN temperature diffusion.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! * varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: TA(NV)  !! Air temperature by vegetation/meteorological entry.
      DOUBLE PRECISION, INTENT(IN) :: VSPSI(NCETOP, NEL)  !! Matric potential/pressure head by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.

      ! Input/Output arguments (Propagated up from MNERR1, MNERR3 requirements)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(INOUT) :: NLYR(NELEE)  !! Number of soil layers in each element.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE)  !! Element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NELEE)  !! Element length.
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)  !! Soil porosity by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: ZVSNOD(LLEE, NEL)  !! Vertical node elevation/depth by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NELEE)  !! Net precipitation/effective rainfall by element.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1)  !! Dynamic-region CM source/sink array.
      DOUBLE PRECISION, INTENT(OUT) :: SSS2(NEL, NCETOP + 1)  !! Dead-space CM source/sink array.

      LOGICAL :: ISADDC, ISADDN
      LOGICAL :: LDUM2(LLEE)

      !-------------------------------------------------------------------*

      IF (.NOT. MN_INITIALISED) CALL RAISE_ERROR(ERRLVL_fatal, 3003, MNPR, 0, 0, &
         'MNMAIN called before MNINITIALISE')

      ! * checks time varying input variables from cm - mn interface
      CALL MNERR3(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, UZNOW, CCCC, PNETTO, SSSS, VSTHE, VSTHEO, MN_WORK%LDUM, LDUM2)

      ! * reads time varying input data
      CALL MNRED2(MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, DTUZ, TIH, UZNOW, BEXBK, LINKNS, &
         MN_WORK%CDPTHB(NLF + 1:NEL), MN_WORK%CLTFCT(NLF + 1:NEL), MN_WORK%CMNFCT(NLF + 1:NEL), MN_WORK%CNRAL(NLF + 1:NEL), &
         MN_WORK%CNRAM(NLF + 1:NEL), MN_WORK%CTOT(NLF + 1:NEL), MN_WORK%NAMFCT(NLF + 1:NEL), MN_WORK%NDPTHB(NLF + 1:NEL), &
         MN_WORK%NTOT(NLF + 1:NEL), ISADDC, ISADDN, MN_WORK%IDUM, MN_WORK%DUMMY)

      ! * checks time dependent input data read in mnred2
      CALL MNERR4(MNPR, NEL, NELEE, NLF, MN_WORK%CDPTHB(NLF + 1:NEL), MN_WORK%CLTFCT(NLF + 1:NEL), &
         MN_WORK%CMNFCT(NLF + 1:NEL), MN_WORK%CNRAL(NLF + 1:NEL), MN_WORK%CNRAM(NLF + 1:NEL), MN_WORK%CTOT(NLF + 1:NEL), &
         MN_WORK%NAMFCT(NLF + 1:NEL), MN_WORK%NDPTHB(NLF + 1:NEL), MN_WORK%NTOT(NLF + 1:NEL), ISADDC, ISADDN, &
         MN_WORK%DUMMY, MN_WORK%LDUM)

      ! * modifies data read in mnred2 into suitable units and form for the rest of the program
      CALL MNINT2(LLEE, NCETOP, NEL, NELEE, NLF, NLYREE, NCOLMB, NLYR, NLYRBT, NTSOIL, MN_CONFIG%AMMDDR, MN_CONFIG%AMMWDR, &
         MN_CONFIG%MNCREF, MN_CONFIG%NITDDR, MN_CONFIG%NITWDR, DELTAZ, DTUZ, CCCC, MN_WORK%CDPTHB(NLF + 1:NEL), &
         MN_WORK%CLTFCT(NLF + 1:NEL), MN_WORK%CMNFCT(NLF + 1:NEL), MN_WORK%CNRAL(NLF + 1:NEL), MN_WORK%CNRAM(NLF + 1:NEL), &
         MN_WORK%CTOT(NLF + 1:NEL), MN_WORK%NAMFCT(NLF + 1:NEL), MN_WORK%NDPTHB(NLF + 1:NEL), MN_WORK%NTOT(NLF + 1:NEL), &
         PNETTO, SSSS, VSTHE, ISADDC, ISADDN, MN_WORK%CNRALT, MN_WORK%CNRAMN, MN_WORK%DUMMY)

      ! * environmental reduction factors are calculated
      CALL MNTEMP(LLEE, NCETOP, NEL, NELEE, NLF, NV, NCOLMB, Z2, DELTAZ, ZVSNOD, DTUZ, TA)
      CALL MNEMT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%Q10M, MN_CONFIG%ISBOTC, MN_CONFIG%ISQ10)
      CALL MNENT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%Q10N, MN_CONFIG%ISBOTC, MN_CONFIG%ISQ10)
      CALL MNEMPH(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, VSPSI, MN_CONFIG%ISBOTC)
      CALL MNENPH(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, VSPSI, MN_CONFIG%ISBOTC)
      CALL MNEDTH(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NLYREE, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, VSTHE, VSPOR, &
         MN_CONFIG%ISBOTC)

      ! * new concentration of carbon and nitrogen manure pools
      CALL MNMAN(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, MN_WORK%CNRAMN, MN_CONFIG%ISBOTC)

      ! * new concentration of carbon litter and humus pools
      CALL MNLTHM(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%FE, MN_CONFIG%FH, DTUZ, MN_CONFIG%ISBOTC)

      ! * new concentration of nitrogen litter pool
      CALL MNLTN(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%CNRBIO, MN_CONFIG%FE, MN_CONFIG%FH, DTUZ, &
         MN_WORK%CNRALT, MN_CONFIG%ISBOTC)

      ! * carbon dioxide production
      CALL MNCO2(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%FE, MN_CONFIG%FH, MN_CONFIG%ISBOTC)

      ! * mineralization/immobilisation rate
      CALL MNGAM(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%CNRHUM, MN_CONFIG%CNRBIO, MN_CONFIG%FE, &
         MN_CONFIG%FH, DTUZ, MN_CONFIG%ISBOTC)

      ! * new concentration of ammonium
      CALL MNAMM(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NLYREE, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, MN_CONFIG%GNN, &
         MN_CONFIG%KPLAMM, MN_CONFIG%KUAMM, MN_CONFIG%MNCREF, MN_CONFIG%KDDSOL, DTUZ, VSTHE, VSTHEO, MN_CONFIG%ISBOTC)

      ! * new nitrate concentration in dynamic and dead space regions
      CALL MNNIT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, D0, MN_CONFIG%KPLNIT, MN_CONFIG%KUNIT, MN_CONFIG%MNCREF, &
         Z2, DTUZ, VSTHE, VSTHEO, MN_CONFIG%ISBOTC, SSS1, SSS2)

      ! * extra output that may be required that is printed in this subroutine
      CALL MNOUT(MNOUT1, MNOUT2, MN_CONFIG%NBOTCE, NCETOP, NEL, NLF, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, MN_CONFIG%CNRHUM, &
         MN_CONFIG%GNN, MN_CONFIG%MNCREF, DELTAZ, MN_CONFIG%KDDSOL, PPHI, DTUZ, UZNOW, DXQQ, DYQQ, MN_WORK%CNRALT, &
         MN_WORK%CNRAMN, VSTHE, VSTHEO, MN_CONFIG%ISBOTC)

   END SUBROUTINE MNMAIN

END MODULE mn_driver

