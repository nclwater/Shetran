!> summary: The catchment water-balance report.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[FRMB]] accumulates the catchment water balance — precipitation, the
!> evaporation and transpiration terms, the storages, the base flow and the
!> outlet discharge — into `BALANC`, and writes the periodic report on the
!> schedule `MBFLAG` selects. It calls
!> [[legacy_result_files:FRRESP]] afterwards.
!>
!> `TIMB` is the time of the next report and `FIRST_frmb` guards the first
!> call. `PREVTM`, which `FRRESP` reads, belongs to
!> [[legacy_result_files]] rather than here; the other placement would close a
!> cycle between the two modules.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed and standardised the FR frame, including impermeable-bed defaults, `BSOFT`, `TIM` migration to `AL_D`, result output, and hot-start/rescue handling. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the FR `.F` files into a single Fortran 90 module. |
!> | 2020-05 | SB | 4.5 | Added ZQ-module variables and support. |
!> | 2026-03 | SB | 4.6 | Added allocation-based initialisation, date-aware meteorological input, the outlet sediment/contaminant text series and the water-table output. |
!> | 2026-09-11 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mass_balance_report

   USE MOD_PARAMETERS, ONLY: zero
   USE element_geometry, ONLY: cellarea, top_cell_no, total_no_elements, total_no_links, ZGRUND
   USE channel_geometry, ONLY: CLENTH
   USE simulation_clock, ONLY: DTUZ, TIH, UZNOW
   USE AL_D, ONLY: BALANC, MBDAY, MBFACE, MBFLAG, MBLINK, MBMON, MBYEAR, NRAINC, &
                   precip_m_per_s
   USE et_state, ONLY: CSTORE, EEVAP, EINTA, ERZA
   USE snow_state, ONLY: RHOSAR, SD
   USE vs_state, ONLY: DELTAZ, NLYRBT, QBKB, QBKF, QVSV, VSTHE
   USE oc_state, ONLY: ARXL, QOC
   USE oc_node_solver, ONLY: gethrf
   USE legacy_result_files, ONLY: FRRESP
   USE datetime, ONLY: hour_from_date

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FRMB

   DOUBLEPRECISION :: TIMB = zero       !! Next monthly-balance reporting time (h).
   LOGICAL         :: FIRST_frmb = .TRUE. !! True until [[frmb]] initialises its persistent schedule.

CONTAINS

!> @brief Calculates and writes monthly water-balance accumulators.
!>
!> `FRMB` accumulates precipitation, evapotranspiration, discharge, storage,
!> subsurface, snow, and balance terms in cubic metres, resets monthly totals
!> when required, and triggers result output through [[frresp]].
!>
!> All accumulated quantities are in cubic metres. The routine uses the
!> following limited index ranges:
!>
!> | Quantity | Limited range used |
!> |:---------|:-------------------|
!> | Link-indexed arrays | `link = 1:total_no_links` |
!> | `DELTAZ(cell,e)` and `VSTHE(cell,e)` | `cell = NLYRBT(e,1):top_cell_no` |
!> | Rainfall-station lookup | `IPSTN=NRAINC(e)` is retained but no longer used in the precipitation sum. |
!> | `QVSV(cell,e)` | `cell == NLYRBT(e,1)` |
!>
!> Entry conditions are `1 <= top_cell_no <= LLEE`,
!> `1 <= total_no_elements <= NELEE`, and
!> `0 <= total_no_links <= NLFEE`; for each element `e`,
!> `2 <= NLYRBT(e,1) <= LLEE` and `1 <= NRAINC(e) <= NVEE`.
!>
!> Inputs include monthly-balance controls `MBFACE`, `MBFLAG`, `MBLINK`, model
!> dimensions `top_cell_no`, `total_no_elements`, `total_no_links`,
!> geometry/storage arrays `cellarea`, `CLENTH`,
!> `DELTAZ`, `ZGRUND`, `ARXL`, `CSTORE`, `HRF`, `SD`, `VSTHE`, flow terms `QOC`,
!> `QBKB`, `QBKF`, `QVSV`, rainfall and ET terms `precip_m_per_s`, `EINTA`,
!> `EEVAP`, and time controls `TIH` and `DTUZ`. It updates `MBDAY`, `MBMON`,
!> `MBYEAR`, and `BALANC(1:19)` (the declared twentieth entry is untouched).
!> `IPSTN=NRAINC(IEL)` is still set
!> for the legacy rainfall-station pathway but is not used in the current
!> precipitation accumulation.
!>
!> `BALANC` stores both short-period and cumulative water-balance terms:
!>
!> | Index | Meaning |
!> |:------|:--------|
!> | 1:6 | Current reporting-period precipitation, canopy evaporation, soil/surface-water evaporation, transpiration, regional aquifer flux through the model base, and outlet discharge. |
!> | 7:12 | Cumulative totals of the same six flow terms. |
!> | 13 | Canopy storage. |
!> | 14 | Snowpack water-equivalent storage. |
!> | 15 | Subsurface water storage. |
!> | 16 | Surface-water storage on land elements. |
!> | 17 | Channel water storage. |
!> | 18 | Current reporting-period aquifer-channel exchange through channel bed and sides. |
!> | 19 | Cumulative aquifer-channel exchange. |
!>
!> On each timestep, rates are converted to volumes with
!>
!> \[
!> A_t(e)=cellarea_e\,DTUZ.
!> \]
!>
!> The timestep contributions are
!>
!> \[
!> P_m = \sum_e precip_e A_t(e),\qquad
!> E_{can,m} = \sum_e EINTA_e A_t(e),
!> \]
!>
!> \[
!> E_{soil,m} = \sum_e EEVAP_e A_t(e),\qquad
!> T_m = \sum_e ERZA_e A_t(e),
!> \]
!>
!> \[
!> Q_{base,m} = \sum_e QVSV_{NLYRBT(e,1)-1,e} A_t(e).
!> \]
!>
!> Outlet discharge is taken from the configured monthly-balance link and face:
!>
!> \[
!> Q_{out,m} =
!> \begin{cases}
!> |QOC(MBLINK,MBFACE)|\,DTUZ, & MBLINK \ne 0,\\
!> 0, & MBLINK = 0.
!> \end{cases}
!> \]
!>
!> Aquifer-channel exchange is accumulated over all links from bank-bed and
!> bank-face flows:
!>
!> \[
!> Q_{bank,m} =
!> \sum_l \left(QBKB_{l,1}+QBKB_{l,2}+QBKF_{l,1}+QBKF_{l,2}\right)DTUZ.
!> \]
!>
!> These timestep values are added to both `BALANC(1:6)` and `BALANC(7:12)`,
!> while `Q_bank,m` is added to `BALANC(18)` and `BALANC(19)`.
!>
!> Storage terms are recomputed only when output is due (`UZNOW >= TIMB`).
!> Canopy and snow storages convert millimetres over element area to cubic
!> metres with `MPMM = 1D-3`:
!>
!> \[
!> BALANC_{13}=\sum_e CSTORE_e\,cellarea_e\,10^{-3},
!> \]
!>
!> \[
!> BALANC_{14}=\sum_e SD_e\,RHOSAR_e\,cellarea_e\,10^{-3}.
!> \]
!>
!> Subsurface, land-surface, and channel storages are
!>
!> \[
!> BALANC_{15}=\sum_e\sum_{k=NLYRBT(e,1)}^{top}
!> VSTHE_{k,e}\,DELTAZ_{k,e}\,cellarea_e,
!> \]
!>
!> \[
!> BALANC_{16}=\sum_e (HRF_e-ZGRUND_e)cellarea_e,\qquad
!> BALANC_{17}=\sum_l ARXL_l\,CLENTH_l.
!> \]
!>
!> In the storage sums, `e` runs from `total_no_links+1` through
!> `total_no_elements`; channel links contribute separately through `BALANC(17)`.
!>
!> The routine writes these values through [[frresp]] using output-data selector
!> 50. It then advances the next reporting date by one day when `MBFLAG=1`, or
!> to the first day of the next month otherwise, including Gregorian leap-year
!> handling for February. After output, the short-period flow terms
!> `BALANC(1:6)` and `BALANC(18)` are reset to zero; cumulative totals are
!> retained.
!>
!> | Condition after accumulation | Action |
!> |:-----------------------------|:-------|
!> | `UZNOW < TIMB` | Return after updating flow accumulators only. |
!> | `UZNOW >= TIMB`, `MBFLAG=1` | Recompute storages, output, advance `MBDAY` by one calendar day. |
!> | `UZNOW >= TIMB`, `MBFLAG/=1` | Recompute storages, output, advance to day 1 of the next month. |
!>
!> The output selector string passed to [[frresp]] is blank except for position
!> 50, which requests the monthly-balance output block.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | Legacy | - | - | Implemented daily/monthly catchment water-balance accumulation and calendar advancement. |
!> | 2026-04-05 | SvB | 4.6.1 | Replaced legacy array-initialisation calls with explicit slices. |
!> @endhistory
   SUBROUTINE FRMB

      IMPLICIT NONE

      INTEGER, PARAMETER :: MBHOUR = 0, MBMIN = 0
      DOUBLE PRECISION, PARAMETER :: MPMM = 1.0D-3

      ! Modernized DATA statement into parameter array initialization
      INTEGER, PARAMETER :: MONEND(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      INTEGER :: IEL, IPSTN, ICBOTM, IL, I, ICL, LYEAR
      DOUBLE PRECISION :: AT, QBK, AREAE, AREAEM
      DOUBLE PRECISION :: PRECM, CEVAPM, SEVAPM, TRANSM, AQFLXM, DISCHM, BFLOW
      CHARACTER(LEN=50) :: AIOSTO
      LOGICAL :: r

      ! Water flow mass bal variables (BALANC) are (time integrals of):
      ! 1    precipitation
      ! 2    canopy evaporation
      ! 3    evaporation from soil or surface water
      ! 4    transpiration
      ! 5    regional aquifer upflow (flow through the model base)
      ! 6    outlet discharge
      ! 7-12 cumulative totals for variables 1-6
      ! 13   storage in canopy
      ! 14      "    in snowpack
      ! 15      "    in subsurface
      ! 16      "    in surface water
      ! 17      "    in channels
      ! 18   aquifer-channel flow (through channel bed and sides)
      ! 19   cumulative aquifer-channel flow

      ! Initialization
      IF (FIRST_frmb) BALANC(1:19) = ZERO
      FIRST_frmb = .FALSE.

      ! Calculate water volumes based on flow rates
      !     * variables 1-5 (and 7-11)
      PRECM = ZERO
      CEVAPM = ZERO
      SEVAPM = ZERO
      TRANSM = ZERO
      AQFLXM = ZERO

      DO IEL = 1, total_no_elements
         IPSTN = NRAINC(IEL)
         ICBOTM = NLYRBT(IEL, 1) - 1
         AT = cellarea(IEL)*DTUZ
         PRECM = PRECM + precip_m_per_s(IEL)*AT
         CEVAPM = CEVAPM + EINTA(IEL)*AT
         SEVAPM = SEVAPM + EEVAP(IEL)*AT
         TRANSM = TRANSM + ERZA(IEL)*AT
         AQFLXM = AQFLXM + QVSV(ICBOTM, IEL)*AT
      END DO

      !     * variable 6 (and 12)
      DISCHM = ZERO
      IF (MBLINK /= 0) DISCHM = ABS(QOC(MBLINK, MBFACE)*DTUZ)

      !     * variable 18 (and 19)
      BFLOW = ZERO
      DO IL = 1, total_no_links
         QBK = QBKB(IL, 1) + QBKB(IL, 2) + QBKF(IL, 1) + QBKF(IL, 2)
         BFLOW = BFLOW + QBK*DTUZ
      END DO

      ! Update BALANC (note: elements 1:6 & 18 may be reset to zero below)
      DO I = 0, 6, 6
         BALANC(I + 1) = BALANC(I + 1) + PRECM
         BALANC(I + 2) = BALANC(I + 2) + CEVAPM
         BALANC(I + 3) = BALANC(I + 3) + SEVAPM
         BALANC(I + 4) = BALANC(I + 4) + TRANSM
         BALANC(I + 5) = BALANC(I + 5) + AQFLXM
         BALANC(I + 6) = BALANC(I + 6) + DISCHM
         BALANC(18 + I/6) = BALANC(18 + I/6) + BFLOW
      END DO

      ! -------------- Proceed only if output is required now -------------- *

      IF (UZNOW < TIMB) RETURN

      ! Calculate water volumes based on storage
      BALANC(13:17) = ZERO

      DO IEL = total_no_links + 1, total_no_elements
         AREAE = cellarea(IEL)
         AREAEM = AREAE*MPMM
         BALANC(13) = BALANC(13) + CSTORE(IEL)*AREAEM
         BALANC(14) = BALANC(14) + SD(IEL)*RHOSAR(IEL)*AREAEM
         BALANC(16) = BALANC(16) + (GETHRF(IEL) - ZGRUND(IEL))*AREAE

         DO ICL = NLYRBT(IEL, 1), top_cell_no
            BALANC(15) = BALANC(15) + VSTHE(ICL, IEL)*DELTAZ(ICL, IEL)*AREAE
         END DO
      END DO

      DO IL = 1, total_no_links
         BALANC(17) = BALANC(17) + ARXL(IL)*CLENTH(IL)
      END DO

      ! Output the data
      AIOSTO(:49) = ' '
      AIOSTO(50:) = '1'

      CALL FRRESP(AIOSTO, UZNOW, .TRUE.)

      ! Calculate the next output time
      IF (MBFLAG == 1) THEN
         ! * next day
         LYEAR = 0

         IF (MOD(MBYEAR, 4) == 0) THEN
            IF (MOD(MBYEAR, 100) == 0) THEN
               r = MOD(MBYEAR, 400) == 0
            ELSE
               r = .TRUE.
            END IF
         ELSE
            r = .FALSE.
         END IF

         IF (r .AND. MBMON == 2) LYEAR = 1
         MBDAY = MOD(MBDAY, MONEND(MBMON) + LYEAR) + 1
      ELSE
         ! * next month
         MBDAY = 1
      END IF

      IF (MBDAY == 1) THEN
         MBMON = MOD(MBMON, 12) + 1
         IF (MBMON == 1) MBYEAR = MBYEAR + 1
      END IF

      TIMB = HOUR_FROM_DATE(MBYEAR, MBMON, MBDAY, MBHOUR, MBMIN) - TIH

      ! Initialise all short period flow data
      BALANC(1:6) = ZERO
      BALANC(18) = ZERO

   END SUBROUTINE FRMB

END MODULE mass_balance_report

