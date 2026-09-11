!> summary: Plant nitrogen uptake.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> [[mnplant]] reads the plant-uptake data from `MNPL` and distributes the
!> demand over the root zone; `MNPLANTINITIALISE` sets up the plant state
!> before the first timestep. `MNPLANTINITIALISE` was private inside `MNmod`
!> and is public here because [[mn_driver]] calls it.
!>
!> @warning
!> [[mnplant]] stores every vegetation table in row `NV` and can inspect
!> saved, uninitialised `ISCROP` flags. This is documented rather than
!> corrected.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_plant

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE array_limits, ONLY: LLEE, nelee, NPLTEE
   USE error_status, ONLY: errstat_fileclose, errstat_write
   USE record_readers, ONLY: ALRED2, ALREDC, ALREDF, ALREDI
   USE mn_state, ONLY: MN_PLANT_NVALEE, MN_PLANT_STATE, plup

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnplant, mnplantinitialise

CONTAINS

!> @brief Calculates potential plant nitrogen uptake by rooted cell.
!>
!> Plant uptake is based on canopy leaf area, canopy-density correction,
!> changing plant biomass, rooting depth, and root density fractions. The
!> routine is adapted from the SHETRAN plant component and preserves its
!> simplified assumptions for mixed vegetation in a grid cell.
!>
!> The manual's plant-uptake file `MNPL` supplies a title (`MNP1`) and, for
!> each vegetation type, a canopy-density function table (`MNP10`/`MNP11`) as
!> pairs of density factor `CDI` and time `CDIT` in days from the simulation
!> start. The routine linearly interpolates this table at `UZNOW/24`; if the
!> current time is beyond the table, the canopy-density factor is set to 1.
!> [[mnplantinitialise]] reads this file, writes the title to `MNOUTPL`, closes
!> both units, and initialises retained plant-mixture and mass state. `MNPLANT`
!> then calculates potential uptake on every call after resetting `PLUP` over
!> `NCOLMB(element):NCETOP`.
!>
!> Important plant-index variables retained from the legacy MPL-based logic are:
!>
!> | Variable | Meaning |
!> |:---------|:--------|
!> | `NPLTEE` | Total number of plant types; normally set to the same value as `NVEE`. |
!> | `NPELEE` | Maximum number of plant types in one element; normally set to 2. |
!> | `NPLANT` | Plant slot number within the current element. |
!> | `JPLTY` | Actual vegetation/plant type represented by `NPLANT`. |
!>
!> For plant type \(p\) in element \(e\), the estimated above-ground plant mass
!> is
!>
!> \[
!> M_{e,p} =
!> \frac{CLAI_p\,DELONE_p\,CDI_p(t)}{CLAIMX_p}
!> PFONE_{e,p}\,DXQQ_e\,DYQQ_e\,RHOPL .
!> \]
!>
!> The potential nitrogen uptake demand is based on the positive mass-change
!> rate \(\dot{M}_{e,p}=(M_{e,p}^{new}-M_{e,p}^{old})/\Delta t\). Negative
!> mass change marks cropping and produces no uptake. For growing plants the
!> nitrogen fraction \(f_N\) is a legacy age function of time since crop
!> emergence:
!>
!> \[
!> f_N =
!> \begin{cases}
!> 0.022, & t_c < 360,\\
!> 0.017, & 360 \le t_c < 720,\\
!> 0.015, & 720 \le t_c < 1080,\\
!> 0.012, & t_c \ge 1080.
!> \end{cases}
!> \]
!>
!> The rooted-cell potential uptake added to `PLUP` is then
!>
!> \[
!> PLUP_{e,c} \mathrel{+}=
!> \frac{\dot{M}_{e,p}\,f_N\,RDF_{p,k}}
!>      {\Delta z_{e,c}\,DXQQ_e\,DYQQ_e},
!> \]
!>
!> where `k = NCETOP - c + 1` indexes the root-density fraction and uptake is
!> applied only from the bottom rooted cell `NCETOP - NRD(JPLTY)` to `NCETOP`.
!> The final nitrate/ammonium availability limits are applied later by
!> [[mnnit]] and [[mnamm]].
!>
!> @note The legacy comments describe this as reasonable for deciduous trees and
!> arable crops, but less suitable for permanent grassland where `CLAI` may be
!> held nearly constant in the ET data. The implementation also keeps several
!> MPL-era simplifications: hard-coded `CLAIMX = 2`, at most two plant types per
!> element, plant type 1 as every second type, a named linear-search
!> interpolation loop, and saved state across calls. `MNOUTPL` receives only the
!> input title before both plant units are closed; no timestep plant values are
!> written.
!> @endnote
!>
!> @warning The current table-read loop stores every vegetation type's `MNP11`
!> values in `CDI(NV,*)` and `CDIT(NV,*)`, rather than row `i`, and does not
!> verify that `NVALUE(i)` is at most the fixed limit `NVALEE=30`. The saved
!> `ISCROP` flags are not initialised before their first possible test. Also,
!> `NRBOT=NCETOP-NRD(JPLTY)` is included in the root loop, giving `NRD+1` cell
!> indices when the complete range is valid. These current behaviours can make
!> multi-vegetation uptake or crop-reset results undefined.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-09-06 | SvB | Checked both plant-file `CLOSE` statements through [[error_status:errstat_fileclose]], which recovers the filename from the unit. |
!> | 2026-09-07 | SvB | Status-checked the `MNOUTPL` title `WRITE` through [[error_status:errstat_write]]. |
!> @endhistory
   SUBROUTINE MNPLANTINITIALISE(MNPL, MNOUTPL, NEL, NLF, NV, NVC, RHOPL, DELONE, DXQQ, DYQQ, PLAI, CLAI)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: MNPL, MNOUTPL, NEL, NLF, NV
      INTEGER, INTENT(IN) :: NVC(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: RHOPL, DELONE(NPLTEE), DXQQ(NELEE), DYQQ(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: PLAI(NV), CLAI(NV)

      INTEGER :: I, JPLTY, NDATA, NELM, NPLANT, NTB
      INTEGER :: IDUM(1)
      INTEGER(KIND=I_P) :: ios !! I/O status from closing the plant input and output files.
      DOUBLE PRECISION :: DUMMY(MN_PLANT_NVALEE*2)
      CHARACTER(LEN=200) :: CDUM(1)
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed close.

      CALL ALRED2(0, MNPL, MNOUTPL, 'mnptin')
      CALL ALREDC(0, MNPL, MNOUTPL, ':MNP1', 1, 1, CDUM)
      WRITE (MNOUTPL, '(/1x,A/)', IOSTAT=ios, IOMSG=emsg) CDUM
      CALL errstat_write(ios, "mn_plant:MNPLANTINITIALISE", emsg)

      DO I = 1, NV
         CALL ALREDI(0, MNPL, MNOUTPL, ':MNP10', 1, 1, IDUM)
         MN_PLANT_STATE%NVALUE(I) = IDUM(1)
         NDATA = IDUM(1)*2
         CALL ALREDF(0, MNPL, MNOUTPL, ':MNP11', NDATA, 1, DUMMY)

         DO NTB = 1, IDUM(1)
            MN_PLANT_STATE%CDI(NV, NTB) = DUMMY(2*NTB - 1)
            MN_PLANT_STATE%CDIT(NV, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      CLOSE (MNPL, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, fid=MNPL, iomsg=emsg)

      CLOSE (MNOUTPL, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, fid=MNOUTPL, iomsg=emsg)

      DO NELM = NLF + 1, NEL
         DO I = 1, NPLTEE
            MN_PLANT_STATE%CLAIMX(I) = 2.0D0
         END DO

         MN_PLANT_STATE%NPLTYP(NELM, 1) = NVC(NELM)
         MN_PLANT_STATE%PFONE(NELM, 1) = PLAI(MN_PLANT_STATE%NPLTYP(NELM, 1))

         IF (MN_PLANT_STATE%PFONE(NELM, 1) >= 0.99D0) THEN
            MN_PLANT_STATE%NPL(NELM) = 1
         ELSE
            MN_PLANT_STATE%PFONE(NELM, 2) = 1.0D0 - MN_PLANT_STATE%PFONE(NELM, 1)
            MN_PLANT_STATE%NPL(NELM) = 2
         END IF

         DO I = 1, NEL
            MN_PLANT_STATE%NPLTYP(I, 2) = 1
         END DO

         DO NPLANT = 1, MN_PLANT_STATE%NPL(NELM)
            JPLTY = MN_PLANT_STATE%NPLTYP(NELM, NPLANT)
            MN_PLANT_STATE%GMCPBB(NELM, NPLANT) = &
               CLAI(JPLTY)*DELONE(JPLTY)/MN_PLANT_STATE%CLAIMX(JPLTY)
            MN_PLANT_STATE%MASSB(NELM, NPLANT) = MN_PLANT_STATE%GMCPBB(NELM, NPLANT)* &
               MN_PLANT_STATE%PFONE(NELM, NPLANT)*DXQQ(NELM)*DYQQ(NELM)*RHOPL
            MN_PLANT_STATE%CROPTM(NELM, NPLANT) = 0.0D0
         END DO
      END DO
   END SUBROUTINE MNPLANTINITIALISE

   SUBROUTINE mnplant(ncetop, nel, nlf, nv, ncolmb, nrd, rhopl, delone, dxqq, dyqq, deltaz, rdf, dtuz, uznow, clai)

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: ncetop  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: nel  !! Number of elements.
      INTEGER, INTENT(IN) :: nlf  !! Number of overland/channel links excluded from land-column uptake.
      INTEGER, INTENT(IN) :: nv  !! Number of vegetation types.
      INTEGER, INTENT(IN) :: ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: nrd(nv)  !! Rooting depth in cell counts by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: rhopl  !! Plant dry-matter density used by uptake calculation.
      DOUBLE PRECISION, INTENT(IN) :: delone(npltee)  !! Initial plant biomass/cover scaling by plant type.
      DOUBLE PRECISION, INTENT(IN) :: dxqq(nelee)  !! Element width.
      DOUBLE PRECISION, INTENT(IN) :: dyqq(nelee)  !! Element length.
      DOUBLE PRECISION, INTENT(IN) :: deltaz(llee, nel)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: rdf(nv, llee)  !! Root density fraction by vegetation type and cell.

      !     * time dependent
      DOUBLE PRECISION, INTENT(IN) :: dtuz  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: uznow  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: clai(nv)  !! Current canopy leaf-area index by vegetation type.

      INTEGER :: jplty, nelm, nplant, nrbot
      INTEGER :: i, nce, ndum
      DOUBLE PRECISION :: cdfnc, chgmas, fn, massbo, tmsncr
      DOUBLE PRECISION :: dum, dum2

      !----------------------------------------------------------------------*

      DO nelm = nlf + 1, nel
         DO nce = ncolmb(nelm), ncetop
            plup(nelm, nce) = 0.0d0
         END DO
      END DO

      DO nelm = nlf + 1, nel
         DO nplant = 1, MN_PLANT_STATE%npl(nelm)
            jplty = MN_PLANT_STATE%npltyp(nelm, nplant)

            age_search_loop: DO i = 2, MN_PLANT_STATE%nvalue(jplty)
               IF ((uznow/24.0d0) < MN_PLANT_STATE%cdit(jplty, i)) THEN
                  dum = (MN_PLANT_STATE%cdi(jplty, i) - MN_PLANT_STATE%cdi(jplty, i - 1))/ &
                     (MN_PLANT_STATE%cdit(jplty, i) - MN_PLANT_STATE%cdit(jplty, i - 1))
                  dum2 = uznow/24.0d0 - MN_PLANT_STATE%cdit(jplty, i - 1)
                  cdfnc = MN_PLANT_STATE%cdi(jplty, i - 1) + dum*dum2
                  EXIT age_search_loop
               END IF
            END DO age_search_loop

            ! Use the full-density factor after the last table time.
            IF (i > MN_PLANT_STATE%nvalue(jplty)) cdfnc = 1.0d0

            nrbot = ncetop - nrd(jplty)
            MN_PLANT_STATE%gmcpbb(nelm, nplant) = clai(jplty)*delone(jplty)*cdfnc/MN_PLANT_STATE%claimx(jplty)
            massbo = MN_PLANT_STATE%massb(nelm, nplant)
            MN_PLANT_STATE%massb(nelm, nplant) = MN_PLANT_STATE%gmcpbb(nelm, nplant)* &
               MN_PLANT_STATE%pfone(nelm, nplant)*dxqq(nelm)*dyqq(nelm)*rhopl
            chgmas = (MN_PLANT_STATE%massb(nelm, nplant) - massbo)/dtuz

            IF (chgmas < 0.0d0) THEN
               MN_PLANT_STATE%iscrop(nelm, nplant) = .TRUE.
            ELSE IF (clai(jplty) > 0.0d0) THEN
               IF (MN_PLANT_STATE%iscrop(nelm, nplant)) THEN
                  MN_PLANT_STATE%croptm(nelm, nplant) = uznow
                  MN_PLANT_STATE%iscrop(nelm, nplant) = .FALSE.
               END IF

               tmsncr = uznow - MN_PLANT_STATE%croptm(nelm, nplant)

               IF (tmsncr < 360.0d0) THEN
                  fn = 0.022d0
               ELSE IF (tmsncr < 720.0d0) THEN
                  fn = 0.017d0
               ELSE IF (tmsncr < 1080.0d0) THEN
                  fn = 0.015d0
               ELSE
                  fn = 0.012d0
               END IF

               DO nce = nrbot, ncetop
                  ndum = ncetop - nce + 1
                  plup(nelm, nce) = plup(nelm, nce) + chgmas*fn*rdf(jplty, ndum)/ &
                     (deltaz(nce, nelm)*dxqq(nelm)*dyqq(nelm))
               END DO
            END IF
         END DO
      END DO
   END SUBROUTINE mnplant

END MODULE mn_plant

