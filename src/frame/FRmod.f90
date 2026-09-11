!> @brief Coordinates SHETRAN frame initialisation, file I/O, output, and component setup.
!>
!> This module contains the legacy FR "frame" routines that connect SHETRAN's
!> component models to run data, mesh indexing, output control, hot-start/restart
!> files, and component initialisation. It reads the global frame input, opens
!> data files, constructs element/link/bank indexing, calculates element areas
!> and face lengths, initialises ET, snow, overland/channel, VSS, bank, sediment,
!> contaminant, plant, and ZQ-table options, and writes both legacy result output
!> and newer text time series.
!>
!> The routines here are primarily orchestration and data-marshalling code rather
!> than a separate hydrological process formulation. They provide the common
!> bookkeeping that allows the water-flow, sediment, contaminant, snow, ET, and
!> reservoir table modules to share geometry, file units, time controls, and
!> output definitions.
!>
!> | Responsibility | Main routines |
!> |:---------------|:--------------|
!> | Run setup and file handling | [[fropen]], [[infr]], [[frinit]] |
!> | Geometry and topology | [[frdim]], [[frltl]], [[frind]], [[frsort]] |
!> | Optional component setup | [[inet]], [[insm]], [[inbk]], [[incm]], [[inpl]] |
!> | Runtime frame bookkeeping | [[frmb]], [[froutput]] |
!> | Result and restart output | [[frresc]], [[frresp]], [[write_dis]], [[write_dis2]] |
!> | Legacy dummy/checking helpers | [[dinet]], [[dinoc]], [[docin]], [[muerr2]] |
!>
!> Only selected orchestration entry points and a small set of frame state
!> variables are public. Most input readers and helper routines remain private
!> module implementation details, even though they are documented here because
!> they define important file-format and coupling behaviour.
!>
!> @warning
!> [[froutput]] declares saved local variables named `next_hour`, `qoctot`,
!> `uzold`, `sedtot`, `sedfinetot`, and `contamtot`. They shadow the same-named
!> public module variables imported by [[simulation_driver]] for automatic
!> differentiation. Consequently the public copies retain their
!> declaration-time values while output uses the local copies; only
!> module-level `icounter2` is updated by the current output path. The private
!> module `hour_now`, `uznowt`, and `qoctotextra` are likewise shadowed and
!> unused.
!> @endwarning
!>
!> @warning
!> `PREVTM` and `GNUCUM` have no declaration initialisation and no assignment
!> before their first use in [[frresp]]. Output id 44 therefore relies on
!> processor/startup state on its first result-output call. This documentation
!> records the current contract; it does not supply an executable default.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed and standardised the FR frame, including impermeable-bed defaults, `BSOFT`, `TIM` migration to `AL_D`, result output, and hot-start/rescue handling. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the FR `.F` files into this Fortran 90 module. |
!> | 2020-05 | SB | 4.5 | Added ZQ-module variables and support. |
!> | 2026-03 | SB | 4.6 | Added allocation-based initialisation through `INITIALISE_AL_C3` and `INITIALISE_ETMOD`, date-aware meteorological input through `BMETDATES`, outlet sediment/contaminant text series, water-table and virtual-discharge text output, improved diagnostics, and `.pri` reporting of hard-coded array sizes. |
!> | 2026-05-03 | SvB | 4.6.1 | Decomposed `FROUTPUT` into phase, sampling, accumulation, formatting, and I/O helpers without changing its output contracts. |
!> | 2026-07-11 | SvB | 4.6.1 | Made rundata input record-based so blank records, normal EOF, and genuine read failures are distinguished. |
!> | 2026-09-07 | SvB | 4.6.1 | Routed the previously unchecked `READ` statements in [[frinit]], [[frltl]], [[inbk]], [[inet]], [[infr]], and [[insm]] through [[error_status:errstat_read]], reporting `IOSTAT`/`IOMSG`; the hot-start reader now separates a genuine read error from end of file. |
!> @endhistory
MODULE FRmod
   USE stdlib_system, ONLY: join_path
   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, IOSTAT_END
   USE array_limits, ONLY: LLEE, max_no_snowmelt_slugs, NCONEE, nelee, nlfee, NLYREE, NOCTAB, &
                           NOLEE, NPELEE, NPLTEE, NSEDEE, NSEE, NUZTAB, NVBP, NVEE, NVSEE, &
                           nxee, NXSCEE, nyee
   USE build_info, ONLY: BANNER, BDEVER, SHEVER
   USE element_geometry, ONLY: cellarea, DXQQ, DYQQ, top_cell_no, total_no_elements, &
                              total_no_links, ZGRUND
   USE run_context, ONLY: cnam, DIRQQ, filnam, hdf5filename, &
                          visualisation_check_filename, visualisation_plan_filename
   USE simulation_clock, ONLY: UZNOW
   USE cm_input, ONLY: INCM
   USE cm_parameters, ONLY: CCAPE, CCAPR, CCAPB, GNN, alphbd, alphbs, alpha, fads
   USE grid_topology, ONLY: NX, NY, ICMREF, ICMXY, NGDBGN
   USE channel_geometry, ONLY: BEXBK, CWIDTH, CLENTH, FHBED, ICMBK, LINKNS, NHBED, ZBEFF, ZBFULL
   USE et_state, ONLY: CLAI, DRAINA, EEVAP, ESOILA, NV, NRD, NVC, PLAI, PNETTO, RDL, RDF, &
                  initialise_al_c3
   USE bank_setup, ONLY: INBK
   USE oc_state, ONLY: ARXL, QOC
   USE vs_state, ONLY: DELTAZ, JVSACN, JVSDEL, NLYRBT, NLYR, NTSOIL, NVSSPC, NVSSPT, NVSWLI, &
                  NVSWLT, NWELBT, NS, NWELTP, QH, QVSH, QVSSPR, QVSWEL, QVSWLI, QVSV, QBKB, QBKF, &
                  VSPSI, VSTHE, VSPOR, WBERR, ZLYRBT, ZVSNOD, ZVSPSL
   USE input_workspace, ONLY: DUMMY, IDUM
   USE element_geometry, ONLY: DHF, ISORT, NBFACE
   USE file_units, ONLY: BFB, BHB, BUG, CMD, CMP, CMT, CMB, LFB, LHB, LGB, SYD, SPR, VSD, VSI, WLD, &
                  MND, MNFC, MNFN, MNPL, MNPR, MNOUT1, MNOUT2, MNOUTPL
   USE grid_topology, ONLY: ICMRF2
   USE simulation_clock, ONLY: DTUZ, TIH, UZNEXT
   USE AL_D, ONLY: BALANC, DTMET3, DTMET, DTMET2, ISTA, IOCORS, ICLNUM, ICLIST, IODATA, IOELEM, &
                  IOSTA, IOSTEP, IOEND, IORES, IOTIME, MBLINK, MBFACE, MBFLAG, MBYEAR, MBMON, &
                  MBDAY, NRAINC, NMC, NM, NSET, NRAIN, precip_m_per_s, PSTART, PMAX, PALFA, PREST, &
                  RESFIL, TMAX
   USE run_control, ONLY: BEXSZ, BEXEX, BEXSY, BEXCM, BEXSM, BEXOC, BEXET, BEXUZ, BHOTRD, BHOTST, &
                  BHOTTI, BHOTPR, HOTIME, isextradis, isextrapsl, TOUTPUT, &
                  TSH, TCH, BFRTS1, BFRTS2, BINFRP, BSOFT, TITLE, msg
   USE et_state, ONLY: CSTORE, EINTA, ERZA, EPOT, VHT
   USE oc_boundaries, ONLY: NOCBCC, NOCBCD
   USE oc_state, ONLY: DQ0ST, DQIST, DQIST2, LCODEY, LCODEX, OCNOW, QMAX
   USE snow_state, ONLY: MSM, NSMC, RHOSAR, SF, SD, TS
   USE zq_tables, ONLY: iszq
   USE array_limits, ONLY: NCLASS, NXE, NYE
   USE element_geometry, ONLY: BWIDTH, CAREA, DXIN, DYIN, NXM1, NYM1, NXP1, NYP1, NXEP1, NYEP1
   USE file_units, ONLY: BKD, DIS, DIS2, disextra, ETD, EPD, FRD, HOT, TAH, TAL, pslextra, MAS, &
                  MED, OCD, OFB, OHB, PRD, PPD, RES, SMD, TIM, VED, VSE, zqd
   USE grid_topology, ONLY: INGRID
   USE legacy_retained, ONLY: NGRID
   USE simulation_clock, ONLY: TIMEUZ, TTH, UZVAL
   USE oc_indexing, ONLY: LINKNO
   USE oc_validation, ONLY: OCLTL
   USE oc_state, ONLY: STRXX, STRYY
   USE datetime, ONLY: hour_from_date, date_from_hour
   USE grid_arrays, ONLY: AREADR, AREADI
   USE input_validation, ONLY: ALCHK, ALCHKI
   USE interpolation, ONLY: ALINTP

   USE float_compare, ONLY: idimje, notzero
   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, LENGTH_FILEPATH, I_P, &
                             half, izero, one, two, zero, zero1
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_error, ERRLVL_warn, ERR_STOP
   USE error_status, ONLY: errstat_alloc, errstat_dealloc, errstat_fileclose, errstat_fileopen, &
                  errstat_rewind, errstat_read, errstat_write
   USE file_units, ONLY: FID_logfile

   USE snow_input, ONLY: INSM
   USE snow_config, ONLY: HEAD, BINSMP, DDF, RHOS, ZOS, ZDS, ZUS, NSD, RHODEF, IMET
   USE snow_state, ONLY: smelt, tmelt
   USE et_config, ONLY: BAR, BMETP, BINETP, BMETAL, BMETDATES, CSTCAP, CSTCA1, CK, CB, CLAI1, FET, &
                  MEASPE, MODE, MODECS, MODEVH, MODEPL, MODECL, NCTCLA, NCTVHT, NCTCST, NF, NCTPLA, &
                  PS1, PLAI1, RELPLA, RELCST, RA, RC, RCF, RELCLA, RELVHT, RTOP, TIMCST, TIMPLA, &
                  TIMVHT, TIMCLA, VHT1
   USE et_process, ONLY: INITIALISE_ETMOD
   USE et_input, ONLY: INET
   USE vs_input, ONLY: VSIN
   USE vs_soil_tables, ONLY: VSPTHE, NVSSOL, VSPKR, VSPETA, VSPDTH, VSPDKR, VSPDET, VSPPSI
   USE oc_driver, ONLY: OCINI
   USE oc_node_solver, ONLY: gethrf, sethrf, setqsa
   USE mod_parameters, ONLY: RHO_SEDIMENT
   USE sy_state, ONLY: DLS, GNU, FBETA, FDEL, PLS, GINFD, GINFS, GNUBK, QSED, DCBED, DCBSED, &
                  ARBDEP, NSED, FBTSD, QDEFF, NSOBED, PBSED, SOSDFN, SOFN
   USE cm_sediment_previous, ONLY: DLSO, GNUO, FBBEDO, FDELO, FBTSDO
   USE cm_column_geometry, ONLY: ZCOLMB, NOLCE, NOLCEA, NOLBT, JOLFN, NOL, NCOLMB, JKZCOL, SCL, OODO
   USE cm_parameters, ONLY: CCCCo, CCCC, CCCCW, SSSS, SSSSO, IIICF, CCAPIN, KDDSOL, KDDLS, GGLMSO, NCON, GCPLA, CCAPIO, CCAPI, IIICFO
   USE cm_column_scaling, ONLY: Z2, D0, Z2SQ, Z2OD, Z2SQOD, SGMA, SGSQ, OMSGMA, NCETOP
   USE cm_column_previous, ONLY: DSWO, QIO, QQRFO, RSZWLO, ZONEO, QQQSWO, GGAMMO, QQO, VSTHEO, UUAJPO
   USE cm_bank_geometry, ONLY: NBANK, NCEBD, FNCEBD, NCEAB
   USE cm_solver_flags, ONLY: ISPLT, ISMN
   USE cm_link_water, ONLY: DBDI, ACPBSG, DBS, ACPBI, ACPSFO, ACPBDO, THBEDO, THBED
   USE cm_plant_state, ONLY: PMASS, PF2MAX, PKMAX, NPLT, PFONE, NPLTYP, PDZF3, DELONE, NPL, GMCBBO

   USE zq_tables, ONLY: ReadZQTable

   IMPLICIT NONE
   INTEGER :: IAOUT !! Legacy frame-output selector read from the FR data file.
   DOUBLEPRECISION :: ALLOUT !! Next accumulated legacy output-control time (h).
   DOUBLEPRECISION :: DTAO   !! Legacy output interval (h).
   LOGICAL :: BTIME  !! Enable time-series result processing.
   LOGICAL :: BSTORE !! Enable the legacy result-output method.
   LOGICAL :: BPPNET !! Print net precipitation arrays.
   LOGICAL :: BPEPOT !! Print potential-evaporation arrays.
   LOGICAL :: BPQOC  !! Print overland/channel discharge arrays.
   LOGICAL :: BPDEP  !! Print surface-water-depth arrays.
   LOGICAL :: BPQF   !! Print river-level and river-flow arrays.
   LOGICAL :: BPQH   !! Print infiltration arrays.
   LOGICAL :: BPQSZ  !! Print saturated-zone flow arrays.
   LOGICAL :: BPHSZ  !! Print phreatic-surface-level arrays.
   LOGICAL :: BPBAL  !! Print water-balance arrays.
   LOGICAL :: BPSD   !! Print snow-depth arrays.


   INTEGER, SAVE   :: next_hour = 1     !! AD-exported compatibility copy; shadowed by [[froutput]] and remains 1.
   INTEGER, SAVE   :: icounter2 = 0     !! Next whole-day mass-balance output threshold (h).
   INTEGER         :: hour_now          !! Unused module copy shadowed by [[froutput]].
   DOUBLEPRECISION :: qoctot = 0.0d0    !! AD-exported compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: uzold = 0.0d0     !! AD-exported compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: uznowt            !! Unused module copy shadowed by [[froutput]].
   DOUBLEPRECISION :: sedtot = 0.0d0    !! Public compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: sedfinetot = 0.0d0 !! Public compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: contamtot = 0.0d0 !! Public compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: qoctotextra
   !! Unused private module copy shadowed by [[froutput]].
   DOUBLEPRECISION :: PREVTM            !! Previous [[frresp]] call time (h); undefined before the first call.
   DOUBLEPRECISION :: TIMB = zero       !! Next monthly-balance reporting time (h).
   LOGICAL         :: FIRST_frmb = .TRUE. !! True until [[frmb]] initialises its persistent schedule.
   LOGICAL         :: SEDSRT = .FALSE.    !! True after sediment sorting state has been initialised.
   DOUBLEPRECISION :: GNUCUM(NELEE)     !! Cumulative erosion-depth workspace (mm); initially undefined.
   DOUBLEPRECISION :: DLSSRT(NELEE)     !! Loose-sediment-depth baseline captured by [[frresp]] (mm).

   PRIVATE

   PUBLIC :: FROPEN, FRINIT, FRSORT, FROUTPUT, FRMB, FRRESP, FRIND, FRLTL, INCM, & !REST NEEDED FOR AD ONLY
             qoctot, uzold, bsoft, tsh, tch, bstore, btime, next_hour, icounter2, DATE_FROM_HOUR, &
             sedtot, sedfinetot, contamtot

CONTAINS

!> @brief Calculates element dimensions, face lengths, and total catchment area.
!>
!> `FRDIM` derives grid-cell dimensions from half-grid spacing, assigns areas
!> for channel links, banks, and land elements, computes face lengths `DHF`,
!> and accumulates `CAREA`. These geometry terms are used throughout water,
!> sediment, and contaminant calculations.
!>
!> Inputs are the active model dimensions and grid/link geometry from the legacy
!> shared frame state: `total_no_elements`, `NX`, `NY`, `NXM1`, `NYM1`,
!> `ICMREF`, `CWIDTH`, `DXIN`, `DYIN`, and `LINKNS`. Outputs are `CAREA`,
!> `cellarea`, `DHF`, `DXQQ`,
!> `DYQQ`, and the fixed bank-element width `BWIDTH`.
!>
!> The routine first converts half-grid spacings to full cell dimensions:
!>
!> \[
!> DX_1=DXIN_1,\qquad DX_{NX}=DXIN_{NX-1},\qquad
!> DX_i=\frac{DXIN_{i-1}+DXIN_i}{2},
!> \]
!>
!> with the same construction for `DY`. The bank width is currently fixed as
!> `BWIDTH = 10 m`.
!>
!> Initial element dimensions are assigned from element type `ICMREF(IEL,1)`.
!> Grid elements use the full grid dimensions,
!>
!> | `ICMREF(:,1)` | Element type | Initial dimensions |
!> |:--------------|:-------------|:-------------------|
!> | 0 | Land/grid element | `DXQQ=DX(IX)`, `DYQQ=DY(IY)`. |
!> | 1 or 2 | Bank element | Width is `BWIDTH`; along-bank length follows the associated link orientation. |
!> | 3 | Channel link | Width is `CWIDTH(link)`; length `CLENTH(link)` follows the link orientation. |
!>
!> \[
!> DXQQ=DX(IX),\qquad DYQQ=DY(IY).
!> \]
!>
!> Bank elements use `BWIDTH` across the bank and the grid spacing along the
!> associated link: north-south links use `DXQQ=BWIDTH`, `DYQQ=DY(IY)`;
!> east-west links use `DXQQ=DX(IX)`, `DYQQ=BWIDTH`. Channel links use channel
!> width across the channel and grid spacing along the link:
!>
!> \[
!> \begin{array}{ll}
!> DXQQ=CWIDTH,\ DYQQ=DY,\ CLENTH=DY, & \text{north-south link},\\
!> DXQQ=DX,\ DYQQ=CWIDTH,\ CLENTH=DX, & \text{east-west link}.
!> \end{array}
!> \]
!>
!> The dimensions of grid and bank elements are then reduced to remove overlap
!> with adjacent channels and banks. For a grid face adjacent to a channel or
!> bank, the removed width is
!>
!> \[
!> \Delta = 0.5\,CWIDTH + \begin{cases}
!> BWIDTH, & \text{adjacent element is a bank},\\
!> 0, & \text{adjacent element is a channel link}.
!> \end{cases}
!> \]
!>
!> Bank-bank corner overlaps are also removed by subtracting
!> `BWIDTH + 0.5*CWIDTH` from the along-bank dimension of the paired bank
!> elements. The final element area and total catchment area are then
!>
!> \[
!> cellarea_i = DXQQ_i\,DYQQ_i,\qquad CAREA=\sum_i cellarea_i.
!> \]
!>
!> `CATEST` is the uncorrected sum of basic grid-square areas,
!> \(\sum DX(IX)DY(IY)\), used only for optional printed diagnostics comparing
!> the basic catchment area with the element-area sum after channel and bank
!> corrections.
!>
!> Finally, `DHF(IEL,face)` stores the distance from the element computational
!> node to each face. West and south distances are calculated from the neighbour
!> element type and local overlap corrections; east and north distances are the
!> remaining parts of the corrected element dimensions:
!>
!> \[
!> DHF_{east}=DXQQ-DHF_{west},\qquad
!> DHF_{north}=DYQQ-DHF_{south}.
!> \]
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Standardised declarations and inherited frame typing. |
!> | 1997-02-23 | RAH | 4.1 | Made typing explicit. |
!> @endhistory
   SUBROUTINE FRDIM(BINFRP)

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: BINFRP

      ! Locals, etc
      INTEGER :: I1, I2, IEL, IFACE, IL, IL1, IL2, INEXT1, INEXT2, ITYPE
      INTEGER :: IX, IY, J, JEL, JL, JTYPE, K
      DOUBLE PRECISION :: CATEST, DIFF, DX(NXEE), DY(NYEE)

      ! SET VALUE FOR BANK ELEMENT WIDTH
      ! (CURRENTLY HARD-CODED AS A FIXED WIDTH)
      BWIDTH = 10.0D0

      ! --- CALCULATE DX AND DY FROM DXIN AND DYIN
      DX(1) = DXIN(1)
      DX(NX) = DXIN(NXM1)
      DO J = 2, NXM1
         DX(J) = (DXIN(J - 1) + DXIN(J))*0.5D0
      END DO

      DY(1) = DYIN(1)
      DY(NY) = DYIN(NYM1)
      DO K = 2, NYM1
         DY(K) = (DYIN(K - 1) + DYIN(K))*0.5D0
      END DO

      ! --- SET UP BASIC DIMENSIONS OF EACH ELEMENT
      dim_loop: DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)
         IX = ICMREF(IEL, 2)
         IY = ICMREF(IEL, 3)
         IL = ICMREF(IEL, 4)

         IF (ITYPE == 0) THEN
            DXQQ(IEL) = DX(IX)
            DYQQ(IEL) = DY(IY)
         ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
            IF (LINKNS(IL)) THEN
               DXQQ(IEL) = BWIDTH
               DYQQ(IEL) = DY(IY)
            ELSE
               DXQQ(IEL) = DX(IX)
               DYQQ(IEL) = BWIDTH
            END IF
         ELSE IF (ITYPE == 3) THEN
            IF (LINKNS(IEL)) THEN
               DXQQ(IEL) = CWIDTH(IL)
               DYQQ(IEL) = DY(IY)
               CLENTH(IL) = DY(IY)
            ELSE
               DXQQ(IEL) = DX(IX)
               DYQQ(IEL) = CWIDTH(IL)
               CLENTH(IL) = DX(IX)
            END IF
         END IF
      END DO dim_loop

      ! --- CORRECT FOR OVERLAPPING ELEMENTS (NB: CHANNEL LINK OVERLAPS NOT IN)
      ! --- AND CALCULATE ELEMENT AND CATCHMENT AREA

      CAREA = ZERO
      CATEST = ZERO

      overlap_loop: DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)
         IX = ICMREF(IEL, 2)
         IY = ICMREF(IEL, 3)
         IL = ICMREF(IEL, 4)

         IF (ITYPE == 0) THEN
            corner_loop: DO I1 = 5, 8
               ! GRID ELEMENTS (REMOVE WIDTHS OF CHANNEL LINKS, AND POSSIBLY BANK ELEME)
               INEXT1 = ICMREF(IEL, I1)

               IF (INEXT1 > 0) THEN
                  DIFF = ZERO
                  IF (ICMREF(INEXT1, 1) > 0) THEN
                     IL = ICMREF(INEXT1, 4)
                     DIFF = DIFF + 0.5D0*CWIDTH(IL)
                     IF (ICMREF(INEXT1, 1) < 3) DIFF = DIFF + BWIDTH
                  END IF
                  IF (I1 == 5 .OR. I1 == 7) DXQQ(IEL) = DXQQ(IEL) - DIFF
                  IF (I1 == 6 .OR. I1 == 8) DYQQ(IEL) = DYQQ(IEL) - DIFF
               END IF

               ! BANK ELEMENTS (REMOVE OVERLAP OF BANKS/BANKS AND BANK/CHANNEL FOR EACH
               ! CORNER OF EACH GRID ELEMENT)
               I2 = I1 + 1
               IF (I2 == 9) I2 = 5
               INEXT2 = ICMREF(IEL, I2)

               IF (INEXT1 > 0 .AND. INEXT2 > 0) THEN
                  IF ((ICMREF(INEXT1, 1) == 1 .OR. ICMREF(INEXT1, 1) == 2) .AND. &
                      (ICMREF(INEXT2, 1) == 1 .OR. ICMREF(INEXT2, 1) == 2)) THEN

                     IL1 = ICMREF(INEXT1, 4)
                     IL2 = ICMREF(INEXT2, 4)

                     IF (LINKNS(IL1)) THEN
                        DYQQ(INEXT1) = DYQQ(INEXT1) - BWIDTH - 0.5D0*CWIDTH(IL2)
                     ELSE
                        DXQQ(INEXT1) = DXQQ(INEXT1) - BWIDTH - 0.5D0*CWIDTH(IL2)
                     END IF

                     IF (LINKNS(IL2)) THEN
                        DYQQ(INEXT2) = DYQQ(INEXT2) - BWIDTH - 0.5D0*CWIDTH(IL1)
                     ELSE
                        DXQQ(INEXT2) = DXQQ(INEXT2) - BWIDTH - 0.5D0*CWIDTH(IL1)
                     END IF
                  END IF
               END IF
            END DO corner_loop
         END IF

         ! CALCULATE CATCHMENT AREA BY SUMMING ALL BASIC GRID SIZES
         ! AND CATCHMENT AREA OBTAINED BY SUMMING ALL ELEMENT AREAS (INCLUDES OVERLAP)
         IF (ITYPE == 0) CATEST = CATEST + DX(IX)*DY(IY)

      END DO overlap_loop

      ! --- CALCULATE AREA OF EACH ELEMENT
      area_loop: DO IEL = 1, total_no_elements
         cellarea(IEL) = DXQQ(IEL)*DYQQ(IEL)
         CAREA = CAREA + cellarea(IEL)
      END DO area_loop

      ! --- PRINT OUT ELEMENT AREA, TOTAL CATCHMENT AREA, AND PERCENTAGE ERROR
      IF (BINFRP) THEN
         WRITE (FID_logfile, 1500)
         DO IEL = 1, total_no_elements
            WRITE (FID_logfile, 1600) IEL, DXQQ(IEL), DYQQ(IEL), cellarea(IEL)
         END DO

         DIFF = (CAREA - CATEST)*100.0D0/CAREA
         IF (CAREA < 1.0D6) THEN
            WRITE (FID_logfile, 1700) CAREA, CATEST, DIFF
         ELSE
            WRITE (FID_logfile, 1750) CAREA/1.0D6, CATEST/1.0D6, DIFF
         END IF
      END IF

      ! ----- SET UP SPACINGS DHF BETWEEN COMPUTATIONAL NODES AND EDGE OF ELEM
      node_space_loop: DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)
         IX = ICMREF(IEL, 2)
         IY = ICMREF(IEL, 3)
         IL = ICMREF(IEL, 4)

         ! WEST FACE (FACE 3)
         IFACE = 3
         JEL = ICMREF(IEL, IFACE + 4)

         IF (JEL == 0) THEN
            IF (ITYPE == 0) THEN
               DHF(IEL, IFACE) = 0.5D0*DXIN(IX - 1)
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               DHF(IEL, IFACE) = 0.5D0*BWIDTH
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0*CWIDTH(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0*DXIN(IX - 1)
               END IF
            END IF
         ELSE IF (JEL > 0) THEN
            JTYPE = ICMREF(JEL, 1)
            JL = ICMREF(JEL, 4)

            IF (ITYPE == 0) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0*DXIN(IX - 1)
               ELSE IF (JTYPE == 1) THEN
                  DHF(IEL, IFACE) = 0.5D0*(DXIN(IX - 1) - 2.0D0*BWIDTH - CWIDTH(JL))
               ELSE IF (JTYPE == 3) THEN
                  DHF(IEL, IFACE) = 0.5D0*(DXIN(IX - 1) - CWIDTH(JL))
               END IF
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0*BWIDTH
               ELSE IF (JTYPE == 1 .OR. JTYPE == 2) THEN
                  DHF(IEL, IFACE) = 0.5D0*DXQQ(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0*BWIDTH
               END IF
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0*CWIDTH(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0*DXIN(IX - 1)
               END IF
            END IF
         ELSE IF (JEL < 0) THEN
            IF (LINKNS(IEL)) THEN
               DHF(IEL, IFACE) = 0.5D0*CWIDTH(IEL)
            ELSE
               DHF(IEL, IFACE) = 0.5D0*DXIN(IX - 1)
            END IF
         END IF

         ! SOUTH FACE (FACE 4)
         IFACE = 4
         JEL = ICMREF(IEL, IFACE + 4)

         IF (JEL == 0) THEN
            IF (ITYPE == 0) THEN
               DHF(IEL, IFACE) = 0.5D0*DYIN(IY - 1)
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               DHF(IEL, IFACE) = 0.5D0*BWIDTH
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0*DYIN(IY - 1)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0*CWIDTH(IEL)
               END IF
            END IF
         ELSE IF (JEL > 0) THEN
            JTYPE = ICMREF(JEL, 1)
            JL = ICMREF(JEL, 4)

            IF (ITYPE == 0) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0*DYIN(IY - 1)
               ELSE IF (JTYPE == 1) THEN
                  DHF(IEL, IFACE) = 0.5D0*(DYIN(IY - 1) - 2.0D0*BWIDTH - CWIDTH(JL))
               ELSE IF (JTYPE == 3) THEN
                  DHF(IEL, IFACE) = 0.5D0*(DYIN(IY - 1) - CWIDTH(JL))
               END IF
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0*BWIDTH
               ELSE IF (JTYPE == 1 .OR. JTYPE == 2) THEN
                  DHF(IEL, IFACE) = 0.5D0*DYQQ(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0*BWIDTH
               END IF
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0*DYIN(IY - 1)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0*CWIDTH(IEL)
               END IF
            END IF
         ELSE IF (JEL < 0) THEN
            IF (LINKNS(IEL)) THEN
               DHF(IEL, IFACE) = 0.5D0*DYIN(IY - 1)
            ELSE
               DHF(IEL, IFACE) = 0.5D0*CWIDTH(IEL)
            END IF
         END IF

         ! EAST FACE (FACE 1)
         IFACE = 1
         DHF(IEL, IFACE) = DXQQ(IEL) - DHF(IEL, 3)

         ! NORTH FACE (FACE 2)
         IFACE = 2
         DHF(IEL, IFACE) = DYQQ(IEL) - DHF(IEL, 4)

      END DO node_space_loop

      ! ^^^^^^^^^^^^ FORMAT STATEMENTS
1500  FORMAT(/'   INDEX   DXQQ (M)   DYQQ (M)     AREA (M^^2)'/)
1600  FORMAT(' ', 4X, I6, 4X, F7.2, 4X, F7.2, 4X, F12.2)
1700  FORMAT(/' TOTAL CATCHMENT AREA = ', F12.3, ' SQ. METRES. '/ &
          &        ' BASIC CATCHMENT AREA = ', F12.3, ' SQ. METRES. '/ &
          &  ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
          &  F12.3, ' %'/)
1750  FORMAT(/' TOTAL CATCHMENT AREA = ', F12.3, ' SQ. KM. '/ &
          &        ' BASIC CATCHMENT AREA = ', F12.3, ' SQ. KM. '/ &
          &  ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
          &  F12.3, ' %'/)

   END SUBROUTINE FRDIM

!> @brief Builds element, bank, link, grid, and neighbour index arrays.
!>
!> The routine converts grid/link/bank code maps into compact SHETRAN element
!> numbering, including the index arrays needed by contaminant migration. Inputs
!> are the grid dimensions and code maps `NX`, `NY`, `INGRID`, `LCODEX`,
!> `LCODEY`, plus the bank/OC flags `BEXBK` and `BEXOC`. It sets
!> `total_no_elements`, `NGDBGN`, and `total_no_links`, and fills `ICMREF`,
!> `ICMRF2`, `ICMBK`, `ICMXY`,
!> `NBFACE`, `NGRID`, and `LINKNS`, defining the topology later used by OC,
!> VSS, sediment, and contaminant routines.
!>
!> Element numbers are assigned in a fixed order. Channel links are created
!> first from the link-code grids: `LCODEY >= 4` creates east-west links
!> (`LINKNS=.FALSE.`), then `LCODEX >= 4` creates north-south links
!> (`LINKNS=.TRUE.`). Each link has `ICMREF(:,1)=3`, stores its grid location in
!> `ICMREF(:,2:3)`, and stores its own link number in `ICMREF(:,4)`.
!> `total_no_links` is the last link index.
!>
!> | Element group | Creation order | Key indices |
!> |:--------------|:---------------|:------------|
!> | East-west channel links | First, from `LCODEY >= 4` | `ICMREF(:,1)=3`, `LINKNS=.FALSE.` |
!> | North-south channel links | Second, from `LCODEX >= 4` | `ICMREF(:,1)=3`, `LINKNS=.TRUE.` |
!> | Banks | Third, only when `BEXBK` and links exist | `ICMREF(:,1)=1,2`, `ICMREF(:,4)=link`, `ICMBK(link,side)=element` |
!> | Grid elements | Last, for `INGRID >= 0` | `ICMREF(:,1)=0`, `ICMXY(i,j)=element` |
!>
!> If the bank component is active, two bank elements are then created for each
!> link. Bank element type is `1` or `2`, `ICMREF(:,4)` points back to the
!> associated link, and `ICMBK(link,bank)` maps from a link and bank side to the
!> bank element number. Grid elements are added last for every non-negative
!> `INGRID` cell; `ICMXY(i,j)` maps a grid coordinate back to the grid-element
!> number. Consequently
!>
!> \[
!> NGDBGN = total\_no\_links + 1,
!> \]
!>
!> so active land/bank/grid elements begin immediately after the channel links.
!>
!> `ICMREF` columns 5:8 hold the neighbours across faces 1:4
!> (east, north, west, south). For grid elements the neighbour is either the
!> adjacent grid cell, an intervening bank element when banks are enabled, or
!> the channel link itself when OC links exist without banks. In the latter case
!> `ICMREF(:,4)=9999` marks that the grid element is adjacent to a channel
!> system rather than an ordinary soil-only element.
!>
!> | `ICMREF` columns | Meaning |
!> |:-----------------|:--------|
!> | 1 | Element type: 0 grid, 1/2 bank side, 3 channel link. |
!> | 2:3 | Grid coordinate used to locate the element. |
!> | 4 | Associated link for banks/links; `9999` for grid cells adjacent directly to OC links when banks are disabled. |
!> | 5:8 | Neighbour across faces 1:4. Negative values point into `ICMRF2`. |
!> | 9:12 | Reciprocal face number in the neighbour, or the boundary face itself. |
!>
!> Channel-link faces either point to their adjacent banks/grid cells or to
!> other channel links at link nodes. A single connected link is stored directly
!> in `ICMREF(:,5:8)`. If a node has multiple connected links, `FRIND` creates an
!> auxiliary `ICMRF2` entry, stores the connected link numbers in
!> `ICMRF2(idx,1:3)`, and stores `-idx` in the relevant `ICMREF` face column.
!> This negative pointer is used later by routing and contaminant routines to
!> expand multi-link junctions.
!>
!> Bank-element face neighbours are assigned according to the associated link
!> orientation and bank side: one face connects to the channel link, one or more
!> faces may connect to neighbouring bank elements around junctions, and the
!> outer face connects to the adjacent grid cell where present.
!>
!> After all forward neighbours are assigned, `FRIND` checks that each neighbour
!> points back to the current element. For ordinary neighbours it records the
!> reciprocal face in `ICMREF(:,9:12)`. For multi-link nodes it records the
!> reciprocal faces in `ICMRF2(:,4:6)`. Boundary faces keep their own face index
!> in `ICMREF(:,9:12)` and the first boundary face for non-link elements is
!> stored in `NBFACE`.
!>
!> | Neighbour value | Interpretation |
!> |:----------------|:---------------|
!> | `> 0` | Direct neighbouring element number. |
!> | `= 0` | External boundary face. |
!> | `< 0` | Multi-link node reference: use `ICMRF2(-value,1:3)` for links and `ICMRF2(-value,4:6)` for reciprocal faces. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Standardised declarations. |
!> | 1997-02-23 | RAH | 4.1 | Made typing explicit and clarified the header. |
!> | 1998-07-13 | RAH | 4.2 | Removed the dependency on `SPEC.OC`. |
!> @endhistory
   SUBROUTINE FRIND(BINFRP)

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: BINFRP

      ! Locals, etc
      LOGICAL, PARAMETER  :: NSOUTH = .TRUE., EWEST = .FALSE.
      INTEGER :: I, IBANK, ICOUNT, IM1, IN1, INDEX, INDEX2, INEXT1, IP1
      INTEGER :: ITYPE, J, J1, J2, JM1, JN2, JNEXT1, JP1, K, L, L1
      INTEGER :: NEL2, NNODE3, NNODE4
      LOGICAL :: SINGLE

      CHARACTER(LEN=2) :: PDIRN

      !
      ! ^^^^^^^^^^^^ INITIALISE ARRAY AND INDEX NUMBER
      !
      DO I = 1, NELEE
         NGRID(I) = 0
         NBFACE(I) = 0
         ICMREF(I, 1:12) = 0
      END DO

      INDEX = 0
      INDEX2 = 0

      !
      ! ^^^^^^^^^^^^ SET UP INDEX NUMBERS
      !
      ! --- CHANNEL LINKS
      !
      DO J = 1, NY
         DO I = 1, NX
            IF (LCODEY(I, J) >= 4) THEN
               INDEX = INDEX + 1
               ICMREF(INDEX, 1) = 3
               ICMREF(INDEX, 2) = I
               ICMREF(INDEX, 3) = J
               ICMREF(INDEX, 4) = INDEX
               LINKNS(INDEX) = .FALSE.
            END IF
         END DO

         DO I = 1, NX
            IF (LCODEX(I, J) >= 4) THEN
               INDEX = INDEX + 1
               ICMREF(INDEX, 1) = 3
               ICMREF(INDEX, 2) = I
               ICMREF(INDEX, 3) = J
               ICMREF(INDEX, 4) = INDEX
               LINKNS(INDEX) = .TRUE.
            END IF
         END DO
      END DO

      total_no_links = INDEX

      !
      ! --- BANK ELEMENTS
      !
      IF (BEXBK .AND. total_no_links > 0) THEN
         DO IBANK = 1, 2
            DO L = 1, total_no_links
               INDEX = INDEX + 1
               ICMREF(INDEX, 1) = IBANK
               ICMREF(INDEX, 2) = ICMREF(L, 2)
               ICMREF(INDEX, 3) = ICMREF(L, 3)
               ICMREF(INDEX, 4) = L
               ICMBK(L, IBANK) = INDEX
            END DO
         END DO
      END IF

      !
      ! --- GRID CODES
      !
      DO J = 1, NY
         DO I = 1, NX
            IF (INGRID(I, J) >= 0) THEN
               INDEX = INDEX + 1
               ICMREF(INDEX, 2) = I
               ICMREF(INDEX, 3) = J
               ICMXY(I, J) = INDEX
            END IF
         END DO
      END DO

      NGDBGN = total_no_links + 1
      total_no_elements = INDEX

      !
      ! ^^^^^^^^^^^^ SET UP ADJACENT NODES
      !
      DO INDEX = 1, total_no_elements

         ITYPE = ICMREF(INDEX, 1)
         I = ICMREF(INDEX, 2)
         J = ICMREF(INDEX, 3)
         L = ICMREF(INDEX, 4)
         IP1 = I + 1
         JP1 = J + 1
         IM1 = I - 1
         JM1 = J - 1

         ! --- GRID SQUARE
         IF (ITYPE == 0) THEN

            ! FACE 1 (EAST)
            IF (BEXOC .AND. LCODEX(I + 1, J) >= 4) THEN
               L = LINKNO(IP1, J, NSOUTH)
               IF (BEXBK) THEN
                  ICMREF(INDEX, 5) = ICMBK(L, 2)
               ELSE
                  ICMREF(INDEX, 5) = L
                  ICMREF(INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID(I + 1, J) >= 0) ICMREF(INDEX, 5) = ICMXY(I + 1, J)
            END IF

            ! FACE 2 (NORTH)
            IF (BEXOC .AND. LCODEY(I, J + 1) >= 4) THEN
               L = LINKNO(I, JP1, EWEST)
               IF (BEXBK) THEN
                  ICMREF(INDEX, 6) = ICMBK(L, 2)
               ELSE
                  ICMREF(INDEX, 6) = L
                  ICMREF(INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID(I, J + 1) >= 0) ICMREF(INDEX, 6) = ICMXY(I, J + 1)
            END IF

            ! FACE 3 (WEST)
            IF (BEXOC .AND. LCODEX(I, J) >= 4) THEN
               L = LINKNO(I, J, NSOUTH)
               IF (BEXBK) THEN
                  ICMREF(INDEX, 7) = ICMBK(L, 1)
               ELSE
                  ICMREF(INDEX, 7) = L
                  ICMREF(INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID(I - 1, J) >= 0) ICMREF(INDEX, 7) = ICMXY(I - 1, J)
            END IF

            ! FACE 4 (SOUTH)
            IF (BEXOC .AND. LCODEY(I, J) >= 4) THEN
               L = LINKNO(I, J, EWEST)
               IF (BEXBK) THEN
                  ICMREF(INDEX, 8) = ICMBK(L, 1)
               ELSE
                  ICMREF(INDEX, 8) = L
                  ICMREF(INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID(I, J - 1) >= 0) ICMREF(INDEX, 8) = ICMXY(I, J - 1)
            END IF

            ! --- CHANNEL LINK
         ELSE IF (ITYPE == 3) THEN

            ! FACE 1 (EAST)
            IF (LINKNS(L)) THEN
               IF (BEXBK) THEN
                  ICMREF(INDEX, 5) = ICMBK(L, 1)
               ELSE
                  IF (INGRID(I, J) >= 0) ICMREF(INDEX, 5) = ICMXY(I, J)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEX(I + 1, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY(I + 1, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX(I + 1, J - 1) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF(INDEX, 5) = -INDEX2
               END IF

               IF (LCODEX(I + 1, J) >= 4) THEN
                  L1 = LINKNO(IP1, J, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 5) = L1
                  ELSE
                     ICMRF2(INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEY(I + 1, J) >= 4) THEN
                  L1 = LINKNO(IP1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 5) = L1
                  ELSE
                     ICMRF2(INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEX(I + 1, J - 1) >= 4) THEN
                  L1 = LINKNO(IP1, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 5) = L1
                  ELSE
                     ICMRF2(INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! FACE 2 (NORTH)
            IF (.NOT. LINKNS(L)) THEN
               IF (BEXBK) THEN
                  ICMREF(INDEX, 6) = ICMBK(L, 1)
               ELSE
                  IF (INGRID(I, J) >= 0) ICMREF(INDEX, 6) = ICMXY(I, J)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEY(I - 1, J + 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX(I, J + 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY(I, J + 1) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF(INDEX, 6) = -INDEX2
               END IF

               IF (LCODEY(I - 1, J + 1) >= 4) THEN
                  L1 = LINKNO(IM1, JP1, EWEST)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 6) = L1
                  ELSE
                     ICMRF2(INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEX(I, J + 1) >= 4) THEN
                  L1 = LINKNO(I, JP1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 6) = L1
                  ELSE
                     ICMRF2(INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEY(I, J + 1) >= 4) THEN
                  L1 = LINKNO(I, JP1, EWEST)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 6) = L1
                  ELSE
                     ICMRF2(INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! FACE 3 (WEST)
            IF (LINKNS(L)) THEN
               IF (BEXBK) THEN
                  ICMREF(INDEX, 7) = ICMBK(L, 2)
               ELSE
                  IF (INGRID(I - 1, J) >= 0) ICMREF(INDEX, 7) = ICMXY(I - 1, J)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEX(I, J - 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY(I - 1, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX(I, J) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF(INDEX, 7) = -INDEX2
               END IF

               IF (LCODEX(I, J - 1) >= 4) THEN
                  L1 = LINKNO(I, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 7) = L1
                  ELSE
                     ICMRF2(INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEY(I - 1, J) >= 4) THEN
                  L1 = LINKNO(IM1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 7) = L1
                  ELSE
                     ICMRF2(INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEX(I, J) >= 4) THEN
                  L1 = LINKNO(I, J, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 7) = L1
                  ELSE
                     ICMRF2(INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! FACE 4 (SOUTH)
            IF (.NOT. LINKNS(L)) THEN
               IF (BEXBK) THEN
                  ICMREF(INDEX, 8) = ICMBK(L, 2)
               ELSE
                  IF (INGRID(I, J - 1) >= 0) ICMREF(INDEX, 8) = ICMXY(I, J - 1)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEY(I, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX(I, J - 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY(I - 1, J) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF(INDEX, 8) = -INDEX2
               END IF

               IF (LCODEY(I, J) >= 4) THEN
                  L1 = LINKNO(I, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 8) = L1
                  ELSE
                     ICMRF2(INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEX(I, J - 1) >= 4) THEN
                  L1 = LINKNO(I, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 8) = L1
                  ELSE
                     ICMRF2(INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEY(I - 1, J) >= 4) THEN
                  L1 = LINKNO(IM1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF(INDEX, 8) = L1
                  ELSE
                     ICMRF2(INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! --- BANK ELEMENT
         ELSE

            ! FACE 1 (EAST)
            IF (LINKNS(L)) THEN
               IF (ITYPE == 1) THEN
                  IF (INGRID(I, J) >= 0) ICMREF(INDEX, 5) = ICMXY(I, J)
               ELSE
                  ICMREF(INDEX, 5) = L
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEX(I + 1, J) >= 4) THEN
                     L1 = LINKNO(IP1, J, NSOUTH)
                     ICMREF(INDEX, 5) = ICMBK(L1, 2)
                  ELSE IF (LCODEY(I + 1, J) >= 4) THEN
                     L1 = LINKNO(IP1, J, EWEST)
                     ICMREF(INDEX, 5) = ICMBK(L1, 1)
                  ELSE IF (LCODEX(I + 1, J - 1) >= 4) THEN
                     L1 = LINKNO(IP1, JM1, NSOUTH)
                     ICMREF(INDEX, 5) = ICMBK(L1, 1)
                  END IF
               ELSE
                  IF (LCODEX(I + 1, J - 1) >= 4) THEN
                     L1 = LINKNO(IP1, JM1, NSOUTH)
                     ICMREF(INDEX, 5) = ICMBK(L1, 2)
                  ELSE IF (LCODEY(I + 1, J) >= 4) THEN
                     L1 = LINKNO(IP1, J, EWEST)
                     ICMREF(INDEX, 5) = ICMBK(L1, 2)
                  ELSE IF (LCODEX(I + 1, J) >= 4) THEN
                     L1 = LINKNO(IP1, J, NSOUTH)
                     ICMREF(INDEX, 5) = ICMBK(L1, 1)
                  END IF
               END IF
            END IF

            ! FACE 2 (NORTH)
            IF (.NOT. LINKNS(L)) THEN
               IF (ITYPE == 1) THEN
                  IF (INGRID(I, J) >= 0) ICMREF(INDEX, 6) = ICMXY(I, J)
               ELSE
                  ICMREF(INDEX, 6) = L
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEY(I, J + 1) >= 4) THEN
                     L1 = LINKNO(I, JP1, EWEST)
                     ICMREF(INDEX, 6) = ICMBK(L1, 2)
                  ELSE IF (LCODEX(I, J + 1) >= 4) THEN
                     L1 = LINKNO(I, JP1, NSOUTH)
                     ICMREF(INDEX, 6) = ICMBK(L1, 1)
                  ELSE IF (LCODEY(I - 1, J + 1) >= 4) THEN
                     L1 = LINKNO(IM1, JP1, EWEST)
                     ICMREF(INDEX, 6) = ICMBK(L1, 1)
                  END IF
               ELSE
                  IF (LCODEY(I - 1, J + 1) >= 4) THEN
                     L1 = LINKNO(IM1, JP1, EWEST)
                     ICMREF(INDEX, 6) = ICMBK(L1, 2)
                  ELSE IF (LCODEX(I, J + 1) >= 4) THEN
                     L1 = LINKNO(I, JP1, NSOUTH)
                     ICMREF(INDEX, 6) = ICMBK(L1, 2)
                  ELSE IF (LCODEY(I, J + 1) >= 4) THEN
                     L1 = LINKNO(I, JP1, EWEST)
                     ICMREF(INDEX, 6) = ICMBK(L1, 1)
                  END IF
               END IF
            END IF

            ! FACE 3 (WEST)
            IF (LINKNS(L)) THEN
               IF (ITYPE == 1) THEN
                  ICMREF(INDEX, 7) = L
               ELSE
                  IF (INGRID(I - 1, J) >= 0) ICMREF(INDEX, 7) = ICMXY(I - 1, J)
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEX(I, J) >= 4) THEN
                     L1 = LINKNO(I, J, NSOUTH)
                     ICMREF(INDEX, 7) = ICMBK(L1, 1)
                  ELSE IF (LCODEY(I - 1, J) >= 4) THEN
                     L1 = LINKNO(IM1, J, EWEST)
                     ICMREF(INDEX, 7) = ICMBK(L1, 1)
                  ELSE IF (LCODEX(I, J - 1) >= 4) THEN
                     L1 = LINKNO(I, JM1, NSOUTH)
                     ICMREF(INDEX, 7) = ICMBK(L1, 2)
                  END IF
               ELSE
                  IF (LCODEX(I, J - 1) >= 4) THEN
                     L1 = LINKNO(I, JM1, NSOUTH)
                     ICMREF(INDEX, 7) = ICMBK(L1, 1)
                  ELSE IF (LCODEY(I - 1, J) >= 4) THEN
                     L1 = LINKNO(IM1, J, EWEST)
                     ICMREF(INDEX, 7) = ICMBK(L1, 2)
                  ELSE IF (LCODEX(I, J) >= 4) THEN
                     L1 = LINKNO(I, J, NSOUTH)
                     ICMREF(INDEX, 7) = ICMBK(L1, 2)
                  END IF
               END IF
            END IF

            ! FACE 4 (SOUTH)
            IF (.NOT. LINKNS(L)) THEN
               IF (ITYPE == 1) THEN
                  ICMREF(INDEX, 8) = L
               ELSE
                  IF (INGRID(I, J - 1) >= 0) ICMREF(INDEX, 8) = ICMXY(I, J - 1)
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEY(I, J) >= 4) THEN
                     L1 = LINKNO(I, J, EWEST)
                     ICMREF(INDEX, 8) = ICMBK(L1, 1)
                  ELSE IF (LCODEX(I, J - 1) >= 4) THEN
                     L1 = LINKNO(I, JM1, NSOUTH)
                     ICMREF(INDEX, 8) = ICMBK(L1, 1)
                  ELSE IF (LCODEY(I - 1, J) >= 4) THEN
                     L1 = LINKNO(IM1, J, EWEST)
                     ICMREF(INDEX, 8) = ICMBK(L1, 2)
                  END IF
               ELSE
                  IF (LCODEY(I - 1, J) >= 4) THEN
                     L1 = LINKNO(IM1, J, EWEST)
                     ICMREF(INDEX, 8) = ICMBK(L1, 1)
                  ELSE IF (LCODEX(I, J - 1) >= 4) THEN
                     L1 = LINKNO(I, JM1, NSOUTH)
                     ICMREF(INDEX, 8) = ICMBK(L1, 2)
                  ELSE IF (LCODEY(I, J) >= 4) THEN
                     L1 = LINKNO(I, J, EWEST)
                     ICMREF(INDEX, 8) = ICMBK(L1, 2)
                  END IF
               END IF
            END IF

         END IF

      END DO

      NEL2 = INDEX2

      !
      ! ^^^^^^^^^^^^ CHECK INDEX ARRAY FOR CONSISTENCY, AND SET UP
      !              ADJACENT FACES (ICMREF(9-12))
      ! (FOR NORMAL ELEMENTS, CHECK THAT THE ADJACENT ELEMENT POINTS BACK
      !  TO THE CURRENT ELEMENT.
      !  FOR MULTIPLE CHANNEL LINKS AT A NODE, CHECK THAT EACH LINK
      !  POINTS BACK TO THE CURRENT ELEMENT)
      !
      ICOUNT = 0
      NNODE3 = 0
      NNODE4 = 0

      element_check: DO INDEX = 1, total_no_elements
         face_loop: DO I = 1, 4
            INEXT1 = ICMREF(INDEX, I + 4)

            IF (INEXT1 > 0) THEN
               DO J = 1, 4
                  IF (ICMREF(INEXT1, J + 4) == INDEX) THEN
                     ICMREF(INDEX, I + 8) = J
                     CYCLE face_loop
                  END IF
               END DO
               WRITE (FID_logfile, 1100) INDEX, I
               ICOUNT = ICOUNT + 1

            ELSE IF (INEXT1 < 0) THEN
               IF (ICMRF2(-INEXT1, 1) == 0 .OR. ICMRF2(-INEXT1, 2) == 0 .OR. ICMRF2(-INEXT1, 3) == 0) THEN
                  NNODE3 = NNODE3 + 1
               ELSE
                  NNODE4 = NNODE4 + 1
               END IF

               branch_loop: DO J1 = 1, 3
                  IN1 = ICMRF2(-INEXT1, J1)
                  IF (IN1 > 0) THEN
                     DO J = 1, 4
                        JNEXT1 = ICMREF(IN1, J + 4)
                        IF (JNEXT1 < 0) THEN
                           DO J2 = 1, 3
                              JN2 = ICMRF2(-JNEXT1, J2)
                              IF (JN2 == INDEX) THEN
                                 ICMRF2(-INEXT1, J1 + 3) = J
                                 CYCLE branch_loop
                              END IF
                           END DO
                        END IF
                     END DO
                     WRITE (FID_logfile, 1100) INDEX, I
                     ICOUNT = ICOUNT + 1
                  END IF
               END DO branch_loop

            ELSE
               ICMREF(INDEX, I + 8) = I
               IF (ITYPE < 3 .AND. NBFACE(INDEX) == 0) NBFACE(INDEX) = I
            END IF
         END DO face_loop
      END DO element_check

      IF (ICOUNT > 0) WRITE (FID_logfile, 1200) ICOUNT

      !
      ! ^^^^^^^^^^^^ WRITE OUT INDEX ARRAY, IF REQUIRED
      !
      IF (BINFRP) THEN

         WRITE (FID_logfile, 1300) total_no_elements
         DO INDEX = 1, total_no_elements
            PDIRN = ' '
            ITYPE = ICMREF(INDEX, 1)
            IF (ITYPE > 0) THEN
               L = ICMREF(INDEX, 4)
               IF (LINKNS(L)) THEN
                  PDIRN = 'NS'
               ELSE
                  PDIRN = 'EW'
               END IF
            END IF
            WRITE (FID_logfile, 1400) INDEX, (ICMREF(INDEX, K), K=1, 4), &
               PDIRN, (ICMREF(INDEX, K), K=5, 8)
         END DO

         IF (NEL2 > 0) THEN
            WRITE (FID_logfile, 1500) NNODE3/3, NNODE4/4, NEL2
            DO INDEX2 = 1, NEL2
               WRITE (FID_logfile, 1600) INDEX2, (ICMRF2(INDEX2, I), I=1, 3)
            END DO
         END IF

      END IF

      ! FORMAT STATEMENTS
      !
1100  FORMAT(' INCONSISTENCY FOUND AT INDEX:', I4, ' FACE:', I2)
1200  FORMAT(/I4, ' INCONSISTENCIES FOUND IN INDEX ARRAY'/)
1300  FORMAT(' ', /'INDEX ARRAY: NO. OF ELEMENTS = ', I6, // &
             ' ', '     INDEX      TYPE         X         Y      LINK   ', &
             '  FACE1     FACE2     FACE3     FACE4'/ &
             ' ', '     -----      ----         -         -      ----   ', &
             '  -----     -----     -----     -----')
1400  FORMAT(' ', 5(4X, I6), 1X, A2, 1X, I6, 3(4X, I6))
1500  FORMAT(' '/'AUXILIARY INDEX ARRAY FOR CHANNEL NODES: ', / &
             'NO. OF NODES WITH 3 BRANCHES = ', I4, / &
             'NO. OF NODES WITH 4 BRANCHES = ', I4, / &
             'TOTAL NO. OF INDICES         = ', I4// &
             ' ', '   INDEX  LINK 1  LINK 2  LINK 3'/ &
             ' ', '   -----  ------  ------  ------')
1600  FORMAT(' ', 5(4X, I4))

   END SUBROUTINE FRIND

!> @brief Runs the main frame initialisation sequence.
!>
!> `FRINIT` calls the common frame reader, component-specific input routines,
!> geometry/index setup, VSS initialisation, bank/sediment/contaminant setup,
!> hot-start reading, allocation routines, and output header preparation before
!> the first timestep is executed.
!>
!> The initialisation uses shared model dimensions and flags including
!> `top_cell_no`, `total_no_elements`, `NGDBGN`, `total_no_links`, `ICMREF`,
!> `UZNEXT`, `CSTORE`, `BEXBK`, `BEXET`,
!> `BEXOC`, `BEXSM`, `DTAO`, `BHOTRD`, `BINFRP`, and `BSTORE`. It updates
!> meteorological/rainfall category arrays `NMC` and `NRAINC`, consumes the
!> component input/output units already opened by [[fropen]] (`BFB`, `BHB`,
!> `BKD`, `CMB`, `CMD`, `CMP`, `CMT`,
!> `EPD`, `ETD`, `FRD`, `HOT`, `LFB`, `LGB`, `LHB`, `MED`, `OCD`, `OFB`, `OHB`,
!> `PPD`, `PRD`, `FID_logfile`, `RES`, `SMD`, `SPR`, `SYD`, `TIM`, `VED`, `VSD`, `VSI`,
!> `WLD`), and initialises run state such as `BHOTTI`, `HOTIME`, `OCNOW`,
!> `TIMEUZ`, `UZNEXT`, `UZNOW`, `MSM`, and `ALLOUT`.
!>
!> | Stage | Main calls/actions |
!> |:------|:-------------------|
!> | Frame input and allocation | [[infr]], `INITIALISE_AL_C3`, `INITIALISE_ETMOD`. |
!> | Optional component input | [[inet]]/[[dinet]], [[insm]], [[oc_driver:OCINI]]/[[dinoc]]. |
!> | Geometry and subsurface | [[frdim]], [[inbk]] when banks are active, then [[vs_input:VSIN]]. |
!> | Link forcing setup | Copy meteorological/rainfall station codes from the first adjacent non-link element to each channel link. |
!> | Reservoir tables | [[zq_tables:ReadZQTable]] when `ISZQ` is true. |
!> | Hot-start | Scan `HOT` until `HOTIME >= BHOTTI`, restore water-flow arrays through `SETHRF`/`SETQSA`, and write restart output via [[frresp]]. |
!>
!> @note
!> Input units are rewound rather than closed after reading. This preserves the
!> legacy automatic-differentiation workflow noted by the in-line comments.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-05 | RAH | 3.4.1 | Added restart checks and passed simulation time to result output. |
!> | 1996-07-24 | GP | 4.0 | Replaced the separate UZ/SZ/EX initialisation path with VSS. |
!> | 1997-1998 | RAH | 4.1-4.2 | Removed redundant legacy state and standardised restart/output setup. |
!> | 2007-03-01 | SB | 4g-pc | Changed the `AIOSTO` `DATA` statement initialisation. |
!> | 2026-03 | SB | 4.6 | Added allocation-based ET/vegetation setup and ZQ-table initialisation. |
!> @endhistory
   SUBROUTINE FRINIT()

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: IEL, IFACE, JEL, K, ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed `REWIND` or `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:FRINIT' !! Location string for read-error reports.
      DOUBLE PRECISION :: rdd(NELEE), rddq(NELEE, 4)
      CHARACTER(LEN=20) :: AIOSTO
      CHARACTER(LEN=10) :: atemp

      DATA AIOSTO/'11111111111111111111'/

      ! OPEN I/O DATA FILES
      ! CALL FROPEN  !moded to main routine
      !
      ! CALL INITIALIZATION SUBROUTINES.
      ! NOTE: THE ORDER IN WHICH THE SUBROUTINES ARE CALLED IS IMPORTANT.

      CALL INFR
      CALL INITIALISE_AL_C3
      CALL INITIALISE_ETMOD

      IF (BEXET) CALL INET
      IF (.NOT. BEXET) CALL DINET

      IF (BEXSM) CALL INSM
      IF (.NOT. BEXSM) MSM = 0

      IF (BEXOC) CALL OCINI()
      IF (.NOT. BEXOC) CALL DINOC

      ! CALCULATE ELEMENT DIMENSIONS AND AREAS
      ! (MUST BE CALLED AFTER OCINI AND BEFORE VSIN)
      CALL FRDIM(BINFRP)

      IF (BEXBK) CALL INBK

      CALL VSIN

      ! SET UP MET. & RAINFALL STATIONS FOR THE CHANNEL (= ADJACENT BANK/GRID)
      link_loop: DO IEL = 1, total_no_links
         NMC(IEL) = 1
         NRAINC(IEL) = 1

         DO IFACE = 1, 4
            JEL = ICMREF(IEL, 4 + IFACE)
            IF (JEL > 0) THEN
               IF (ICMREF(JEL, 1) /= 3 .AND. NMC(JEL) > 0 .AND. NRAINC(JEL) > 0) THEN
                  NMC(IEL) = NMC(JEL)
                  NRAINC(IEL) = NRAINC(JEL)
                  CYCLE link_loop
               END IF
            END IF
         END DO
      END DO link_loop

      ! ZQ Module 200520
      IF (iszq) CALL ReadZQTable

      ! close data input file units
      REWIND (FRD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (FRD) for AD
      CALL errstat_rewind(ios, fid=FRD, iomsg=emsg)
      REWIND (VSD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (VSD) for AD
      CALL errstat_rewind(ios, fid=VSD, iomsg=emsg)
      REWIND (OCD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (OCD) for AD
      CALL errstat_rewind(ios, fid=OCD, iomsg=emsg)
      REWIND (ETD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (ETD) for AD
      CALL errstat_rewind(ios, fid=ETD, iomsg=emsg)
      REWIND (SMD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (SMD) for AD
      CALL errstat_rewind(ios, fid=SMD, iomsg=emsg)
      REWIND (BKD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (BKD) for AD
      CALL errstat_rewind(ios, fid=BKD, iomsg=emsg)
      REWIND (VSI, IOSTAT=ios, IOMSG=emsg) ! CLOSE (VSI) for AD
      CALL errstat_rewind(ios, fid=VSI, iomsg=emsg)
      ! CALL RES FILE INPUT ROUTINE, IF REQUIRED
      ! IF (BSTORE) CALL INRES(BINFRP)
      REWIND (PPD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (PPD) for AD
      CALL errstat_rewind(ios, fid=PPD, iomsg=emsg)

      ! UPDATE HOTSTART TIME AND READ FROM FILE IF BHOTRD = TRUE
      HOTIME = zero

      IF (BHOTRD) THEN

         hotstart_read: DO
            READ (HOT, *, IOSTAT=ios, IOMSG=emsg) atemp, HOTIME, UZNEXT, top_cell_no, atemp, &
               (CSTORE(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               (rdd(IEL), IEL=1, total_no_elements), atemp, &
               ((rddq(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((QOC(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((DQ0ST(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((DQIST(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((DQIST2(IEL, K), IEL=1, NGDBGN - 1), K=1, 3), atemp, &
               (SD(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               (TS(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               (NSMC(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               ((SMELT(K, IEL), K=1, NSMC(IEL)), IEL=NGDBGN, total_no_elements), atemp, &
               ((tmelt(K, IEL), K=1, NSMC(IEL)), IEL=NGDBGN, total_no_elements), atemp, &
               ((VSPSI(k, iel), k=1, top_cell_no), IEL=1, total_no_elements)

            ! Gracefully exit if end of hotstart file is reached
            IF (ios < 0) THEN
               WRITE (FID_logfile, '(/ A)') ' WARNING: END OF HOTSTART FILE REACHED'
               EXIT hotstart_read
            END IF

            ! a positive status is a genuine read error, not an expected end of file
            CALL errstat_read(ios, location, emsg)

            DO IEL = 1, total_no_elements
               CALL SETHRF(IEL, rdd(IEL))
               DO K = 1, 4
                  CALL SETQSA(IEL, K, rddq(IEL, K))
               END DO
            END DO

            ! Keep reading lines if HOTIME is less than the target BHOTTI
            IF (HOTIME >= BHOTTI) EXIT hotstart_read

         END DO hotstart_read

         WRITE (FID_logfile, '(// A, F10.2, A /)') ' ^^^ HOTSTART OF SIMULATION AT TIME ', HOTIME, ' ^^^'

         ALLOUT = HOTIME + DTAO
         UZNOW = HOTIME
         OCNOW = HOTIME
         UZVAL = UZNOW + UZNEXT
         TIMEUZ = HOTIME
         BHOTTI = HOTIME

         ! --- WRITE SET OF DATA TO RES FILES AT HOTSTART TIME
         CALL FRRESP(AIOSTO, UZNOW, .FALSE.)

      END IF

   END SUBROUTINE FRINIT

!> @brief Reads a gridded numeric-code map used for output class definitions.
!>
!> `NNX` and `NNY` are the grid dimensions to read, while `NXE` and `NYE` are the
!> declared dimensions of output array `IARR`. `INF` is the input file unit,
!> `IOF` is the output/echo file unit, and `BPCNTL` controls whether the read
!> code map is printed. The numeric codes read from `INF` are returned in
!> `IARR`.
!>
!> The file section starts with an 80-character title, then reads `NNY` grid
!> rows. Rows must be supplied from top to bottom: the first row label must be
!> `NNY`, then `NNY-1`, and so on to 1. Each map character is interpreted as:
!>
!> | Character | Stored value |
!> |:----------|:-------------|
!> | `1`-`9` | Corresponding integer code. |
!> | Any other character | 0. |
!>
!> @warning
!> A row-label mismatch writes `INCORRECT COORDINATE` when echoing is enabled
!> and then executes `STOP`.
!> @endwarning
!>
!> @warning
!> The local character buffer has 200 entries although the current grid capacity
!> `NXEE` is 1000, and no guard enforces `NNX<=200`. Larger calls would index
!> beyond `A1LINE`; there is no call to `FRLTL` elsewhere in the current source.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-02 | RAH | 3.4.1 | Replaced the two-byte integer map with default integers. |
!> | 1997-02-23 | RAH | 4.1 | Made typing explicit. |
!> @endhistory
   SUBROUTINE FRLTL(NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NNX, NNY, NXE, NYE, INF, IOF
      LOGICAL, INTENT(IN) :: BPCNTL

      ! Output arguments
      INTEGER, INTENT(OUT) :: IARR(NXE, NYE)

      ! Locals, etc
      INTEGER :: I, J, K, L, M, ios
      ! CHARACTER(LEN=80) :: TITLE
      CHARACTER(LEN=1)  :: A1LINE(200)
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:FRLTL' !! Location string for read-error reports.

      CHARACTER(LEN=1), PARAMETER :: NMERIC(9) = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

      READ (INF, '(A80)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      IF (BPCNTL) WRITE (IOF, '(A80)') TITLE

      IARR(1:NNX, 1:NNY) = 0

      I = NNY
      DO J = 1, NNY
         READ (INF, '(I7, 1X, 500A1)', IOSTAT=ios, IOMSG=emsg) K, (A1LINE(L), L=1, NNX)
         CALL errstat_read(ios, location, emsg)
         IF (BPCNTL) WRITE (IOF, '(I7, 1X, 500A1)') K, (A1LINE(L), L=1, NNX)

         IF (K /= I) THEN
            IF (BPCNTL) WRITE (IOF, '("   ^^^   INCORRECT COORDINATE")')
            WRITE (*, '(A)') 'INCORRECT COORDINATE'
            CALL ERR_STOP(255)
         END IF

         I = I - 1

         outer_loop: DO L = 1, NNX
            DO M = 1, 9
               IF (A1LINE(L) == NMERIC(M)) THEN
                  IARR(L, K) = M
                  CYCLE outer_loop
               END IF
            END DO
         END DO outer_loop

      END DO

   END SUBROUTINE FRLTL

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

!> @brief Opens the run-data controlled input and output files.
!>
!> `FROPEN` prints the SHETRAN banner, opens the frame/run files, reads file
!> names and unit assignments, and prepares legacy output streams used by
!> initialisation and runtime reporting.
!>
!> The routine opens the rundata file `FILNAM` on unit 2 and a run log named
!> `info_<catchment>_SHETRAN_log.txt` on unit 61. It then reads label/name pairs
!> from the rundata file. Blank names and `0` mark optional files as unused;
!> otherwise most names are prefixed with `DIRQQ` and opened on their numeric
!> unit.
!>
!> | Units or entry | Behaviour |
!> |:---------------|:----------|
!> | 10:47 | Ordinary input/output files; missing unit 45 or 46 disables station output, missing unit 47 disables extra discharge output. |
!> | 48 | Stored as `visualisation_plan_filename`; not opened here. |
!> | 49 | Stored as `visualisation_check_filename`; not opened here. |
!> | 50 | Stored as `hdf5filename`; not opened here. |
!> | 51 | A blank name or `0` disables ZQ only; EOF here disables ZQ and every later optional group. A nonblank name is opened exactly as read. |
!> | 52 | Optional extra phreatic-surface-level output configuration, prefixed with `DIRQQ`. |
!> | 53 | Optional nitrate configuration file, prefixed with `DIRQQ`. |
!> | 54:60 | Additional optional nitrate files, prefixed with `DIRQQ`. |
!>
!> Special side effects:
!>
!> | Condition | Side effect |
!> |:----------|:------------|
!> | Routine entry | `ISTA`, `ISEXTRADIS`, `ISZQ`, `ISEXTRAPSL`, and `ISMN` are initially assumed true. |
!> | Unit 22 opened | `BTIME=.TRUE.`, writes an initial message to `TIM`, then rewinds it. |
!> | Unit 27 opened | `RESFIL` stores the resolved filename. |
!> | Required early file list ends before unit 14 | Stops with `ABNORMAL END`. |
!>
!> The contained `read_rundata_record` helper consumes one complete physical
!> record, so an empty record is distinct from EOF. `unit_context` labels read
!> diagnostics; `stop_eof_error` and `stop_rundata_open_error` report the
!> terminal messages and stop through [[error_reporting:ERR_STOP]]. Every other file
!> `OPEN` is checked through [[error_status:errstat_fileopen]], which reports the
!> `IOSTAT`/`IOMSG` and terminates. FORD lists these contained routines in the
!> source page rather than emitting separate procedure pages.
!>
!> @warning
!> Optional ZQ unit 51 is opened using the filename exactly as read, unlike most
!> other optional file entries that are prefixed with `DIRQQ`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Included the catchment name in run-file diagnostics. |
!> | 1997-02-23 | RAH | 4.0 | Standardised file status, time-series setup, and unit assignments. |
!> | 2013-12-16 | SB | - | Missing unit 45 or 46 disables station output (`ISTA`). |
!> | 2015-04-22 | SB | - | Missing unit 47 disables extra discharge output (`ISEXTRADIS`). |
!> | 2020-07-08 | SB | 4.5 | Added the optional ZQ table file on unit 51. |
!> | 2024-03-12 | SB | - | Added the optional extra phreatic-surface-level output configuration on unit 52, and moved the run log to unit 53. |
!> | 2025-09-25 | SB | 4.5.3 | Added the optional nitrate configuration files on units 53--60. |
!> | 2026-04 | SvB | 4.6.1 | Replaced platform-specific path handling with `join_path`. |
!> | 2026-05-11 | SB | - | Added error checking on the initial rundata-file `OPEN`, stopping instead of proceeding silently on failure. |
!> | 2026-07-11 | SvB | 4.6.1 | Distinguished blank records, EOF, and genuine rundata read errors. |
!> | 2026-09-06 | SvB | - | Routed every non-rundata file `OPEN` through [[error_status:errstat_fileopen]] (reporting `IOSTAT`/`IOMSG`) and removed the local `stop_open_error` helper. |
!> | 2026-09-06 | SvB | - | Checked every `CLOSE` of the rundata unit through [[error_status:errstat_fileclose]], which recovers the filename from the unit. |
!> @endhistory
   SUBROUTINE FROPEN

      IMPLICIT NONE

      INTEGER(KIND=I_P) :: I
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg
      LOGICAL :: at_eof
      CHARACTER(LEN=LENGTH_FILEPATH) :: FILNAM2

      !
      BTIME = .FALSE.

      ! WRITE BANNER HEADER TO SCREEN
      WRITE (*, *)
      WRITE (*, *) '**************************'
      WRITE (*, *) BANNER
      WRITE (*, *) '**************************'
      WRITE (*, *)

      ista = .TRUE.
      isextradis = .TRUE.
      iszq = .TRUE.
      isextrapsl = .TRUE.
      ismn = .TRUE.

      OPEN (2, FILE=FILNAM, STATUS='OLD', IOSTAT=ios, IOMSG=emsg)
      IF (ios /= 0) CALL stop_rundata_open_error(FILNAM, emsg)

      FILNAM2 = join_path(DIRQQ, 'info_'//TRIM(CNAM)//'_SHETRAN_log.txt')

      OPEN (61, FILE=FILNAM2, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileopen(ios, FILNAM2, emsg)

      CALL read_rundata_record(FILNAM, at_eof, 'rundata header')
      IF (at_eof) CALL stop_eof_error(CNAM)

      WRITE (61, '(A)') FILNAM
      WRITE (61, *)

      ! Main file reading loop
      DO I = 10, 50
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'description'))
         IF (at_eof) THEN
            IF (I < 14) CALL stop_eof_error(CNAM)
            iszq = .FALSE.
            isextrapsl = .FALSE.
            ismn = .FALSE.
            CLOSE (2, IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
            RETURN
         END IF

         WRITE (61, '(A)') FILNAM
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'filename'))
         IF (at_eof) THEN
            IF (I < 14) CALL stop_eof_error(CNAM)
            iszq = .FALSE.
            isextrapsl = .FALSE.
            ismn = .FALSE.
            CLOSE (2, IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
            RETURN
         END IF

         IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
            IF (I == 45 .OR. I == 46) ista = .FALSE.
            IF (I == 47) isextradis = .FALSE.

            WRITE (61, '("- NOT USED")')
         ELSE
            FILNAM = join_path(DIRQQ, TRIM(FILNAM))
            IF (I == 48) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               visualisation_plan_filename = FILNAM
            ELSE IF (I == 49) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               visualisation_check_filename = FILNAM
            ELSE IF (I == 50) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               hdf5filename = FILNAM
            ELSE
               WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') I, FILNAM

               OPEN (I, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
               CALL errstat_fileopen(ios, FILNAM, emsg)

               IF (I == 27) RESFIL = FILNAM
               IF (I == 22) THEN
                  BTIME = .TRUE.
                  WRITE (TIM, *) 'Reading data sets ...'
                  REWIND (TIM, IOSTAT=ios, IOMSG=emsg)
                  CALL errstat_rewind(ios, fid=TIM, iomsg=emsg)
               END IF
            END IF
         END IF
      END DO

      ! ZQ Module 2020-05-20
      CALL read_rundata_record(FILNAM, at_eof, unit_context(51, 'description'))
      IF (at_eof) THEN
         iszq = .FALSE.
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      CALL read_rundata_record(FILNAM, at_eof, unit_context(51, 'filename'))
      IF (at_eof) THEN
         iszq = .FALSE.
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         iszq = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         OPEN (51, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, FILNAM, emsg)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 51, FILNAM
      END IF

      !extra psl 110324
      CALL read_rundata_record(FILNAM, at_eof, unit_context(52, 'description'))
      IF (at_eof) THEN
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      CALL read_rundata_record(FILNAM, at_eof, unit_context(52, 'filename'))
      IF (at_eof) THEN
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         isextrapsl = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         FILNAM2 = join_path(DIRQQ, TRIM(FILNAM))
         OPEN (52, FILE=FILNAM2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, FILNAM2, emsg)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 52, FILNAM2
      END IF

      !nitrate component 230925
      CALL read_rundata_record(FILNAM, at_eof, unit_context(53, 'description'))
      IF (at_eof) THEN
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      CALL read_rundata_record(FILNAM, at_eof, unit_context(53, 'filename'))
      IF (at_eof) THEN
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         ismn = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         FILNAM = join_path(DIRQQ, TRIM(FILNAM))
         OPEN (53, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, FILNAM, emsg)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 53, FILNAM
      END IF

      ! Remaining nitrate files
      DO I = 54, 60
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'description'))
         IF (at_eof) EXIT

         WRITE (61, '(A)') FILNAM
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'filename'))
         IF (at_eof) EXIT

         IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
            WRITE (61, '("- NOT USED")')
         ELSE
            FILNAM = join_path(DIRQQ, TRIM(FILNAM))
            OPEN (I, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileopen(ios, FILNAM, emsg)
            WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') I, FILNAM
         END IF
      END DO

      CLOSE (2, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, fid=2, iomsg=emsg)

      RETURN

   CONTAINS

!> @brief Reads one complete physical record from the rundata file on unit 2.
!>
!> A blank record is a successful read and is returned as blanks in `line`.
!> End of file sets `at_eof`; every other input error reports the catchment and
!> supplied record `context` on `ERROR_UNIT`, then terminates with `ERROR STOP`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-07-11 | SvB | Distinguished genuine read failures from normal end of file using whole-record input. |
!> @endhistory
      SUBROUTINE read_rundata_record(line, at_eof, context)
         CHARACTER(LEN=*), INTENT(OUT) :: line
         LOGICAL, INTENT(OUT) :: at_eof
         CHARACTER(LEN=*), INTENT(IN) :: context

         INTEGER :: read_status
         CHARACTER(LEN=512) :: message

         ! An '(A)' read consumes exactly one physical record.  In particular,
         ! a blank record is a successful read whose result is all blanks.
         line = ''
         message = ''
         at_eof = .FALSE.
         READ (2, '(A)', IOSTAT=read_status, IOMSG=message) line

         IF (read_status == 0) RETURN
         IF (read_status == IOSTAT_END) THEN
            at_eof = .TRUE.
            RETURN
         END IF

         WRITE (ERROR_UNIT, '(A)') 'ERROR READING RUNDATA FILE '//TRIM(CNAM)// &
            ' ('//TRIM(context)//'): '//TRIM(message)
         CALL ERR_STOP(255)
      END SUBROUTINE read_rundata_record

!> @brief Formats the rundata unit number and record kind for an input diagnostic.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-07-11 | SvB | Added contextual diagnostics for whole-record rundata input. |
!> @endhistory
      FUNCTION unit_context(unit, record_kind) RESULT(context)
         INTEGER, INTENT(IN) :: unit
         CHARACTER(LEN=*), INTENT(IN) :: record_kind
         CHARACTER(LEN=64) :: context

         WRITE (context, '("unit ",I0,1X,A)') unit, TRIM(record_kind)
      END FUNCTION unit_context

      ! Internal helpers to cleanly exit without jumping to bottom labels
!> @brief Reports an unexpected early end of the rundata file and stops the run.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-04-06 | SvB | Replaced the legacy branch to a shared terminal label. |
!> @endhistory
      SUBROUTINE stop_eof_error(c_name)
         CHARACTER(LEN=*), INTENT(IN) :: c_name
         WRITE (*, '("UNEXPECTED -EOF- ON FILE ",A)') c_name
         CALL ERR_STOP(255)
      END SUBROUTINE stop_eof_error

!> @brief Reports failure to open the rundata file and stops the run.
!>
!> Takes the pathname that was actually opened, so that the message names the
!> file the user can go and look at.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-08-31 | SvB | Split from `stop_rundata_error`, which reported the catchment name instead of the path. |
!> | 2026-09-06 | SvB | Added IOMSG output |
!> @endhistory
      SUBROUTINE stop_rundata_open_error(f_name, emsg)
         CHARACTER(LEN=*), INTENT(IN) :: f_name
         CHARACTER(LEN=*), INTENT(IN) :: emsg
         WRITE (*, '("Error opening the rundata file ",A)') TRIM(f_name)
         WRITE (*, '("Error message: ",A)') TRIM(emsg)
         CALL ERR_STOP(255)
      END SUBROUTINE stop_rundata_open_error

   END SUBROUTINE FROPEN

!> @brief Manages additional text time-series output.
!>
!> The routine handles start, timestep, and final-state phases for CSV-style discharge,
!> extra discharge stations, water-table depth, sediment, fine sediment, and
!> contaminant outlet series. It keeps running totals between calls and formats
!> time using `DATE_FROM_HOUR`.
!>
!> `SIMPOS` selects the phase:
!>
!> | `SIMPOS` value | Behaviour |
!> |:---------------|:----------|
!> | `start` | Read optional extra-output control files, open CSV/text outputs, find the outlet link/face, initialise averaging counters. |
!> | starts with `main` | Accumulate current timestep values, write regular output when a `TOUTPUT` interval boundary is crossed, write every-timestep discharge through [[write_dis2]], and write daily mass-balance/optional water-table rows. |
!> | any other value | Write end-of-simulation phreatic-surface and pressure-head output to `VSE` for use as VSI-style initial conditions. |
!>
!> | Contained helper group | Routines |
!> |:-----------------------|:---------|
!> | Start/setup | `initialise_output`, `initialise_extra_discharge_points`, `allocate_extra_discharge`, `initialise_extra_water_table_output`, `find_mass_balance_outlet`, `write_discharge_header`, `initialise_sediment_output`, `initialise_contaminant_output` |
!> | Timestep sampling/averaging | `write_main_output`, `sample_current_values`, `accumulate_interval`, `write_completed_regular_outputs`, `restart_accumulators` |
!> | Formatting/final state | `write_regular_outputs`, `timestamp_from_output_hour`, `write_periodic_mass_balance`, `write_final_state` |
!> | I/O checks | `write_checked`, `stop_on_io_error`, `fatal_on_io_error` |
!>
!> FORD exposes those contained routines on the source page rather than as
!> separate procedure pages; each still has an adjacent source header below.
!>
!> Opened or written outputs are:
!>
!> | Output | Unit/source | Contents |
!> |:-------|:------------|:---------|
!> | `DIS2` | rundata unit 44 | Every-model-timestep outlet discharge with absolute date/time. |
!> | `MAS` | rundata unit 43 | Daily spatially averaged cumulative balance and storage terms in mm over `CAREA`. |
!> | `DIS` | rundata unit 41 | Regular `TOUTPUT`-interval outlet discharge, with optional extra channels. |
!> | `output_<catchment>_water_table_depth.csv` | local unit 683 when `ISEXTRAPSL` | Selected element water-table depth below ground; negative means surface-water depth. |
!> | `output_<catchment>_sediment_all.csv` and `_sediment_fine.csv` | local units 681/682 when `BEXSY` | Outlet sediment discharge for all fractions and fraction 1. |
!> | `output_<catchment>_contaminant.csv` | local unit 684 when `BEXCM` | Outlet relative concentration for contaminant 1. |
!>
!> @warning
!> Two existing write-error messages call `DIS2` unit 41 and `DIS` unit 44.
!> The actual `AL_D` parameter assignments, and therefore the files written,
!> are `DIS=41` and `DIS2=44` as shown above.
!> @endwarning
!>
!> Extra discharge points are read from `DISEXTRA` as `(element, face)` pairs and
!> silently filtered when the element number exceeds `total_no_links`. Extra
!> water-table output elements are read from `PSLEXTRA` and filtered when the
!> element number exceeds `total_no_elements`.
!>
!> @warning
!> Optional-point validation checks only those upper element/link bounds. Zero
!> or negative identifiers, discharge faces outside 1:4, and negative requested
!> counts are not rejected here and can fail during allocation or later indexing.
!> @endwarning
!>
!> When result-file output has not provided `MBLINK`/`MBFACE`, the `start` phase
!> scans channel links and selects the last external OC boundary with boundary
!> type 7, i.e. a weir boundary. If no outlet is found, outlet discharge,
!> sediment, and contaminant series use zero values.
!>
!> Regular discharge, sediment, and contaminant records are accumulated in
!> normalised output time `UZNOW/TOUTPUT`. The value written for `outputhour =
!> next_hour-1` is the mean over the preceding `TOUTPUT` interval and is dated at
!> the interval start. If one model step crosses more than one output interval,
!> intermediate intervals are filled with the current timestep value.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2005-2024 | SB | 4.x | Added every-step and regular discharge, mass-balance, virtual-station, water-table, sediment, and contaminant text output. |
!> | 2026-05-03 | SvB | 4.6.1 | Split the monolithic phase logic into contained helpers. |
!> | 2026-09-06 | SvB | - | Checked every output-file `OPEN` through [[error_status:errstat_fileopen]], reporting `IOSTAT`/`IOMSG`. |
!> | 2026-09-07 | SvB | - | Status-checked the remaining bare header/state `WRITE`s via `write_checked` / `stop_on_io_error`. |
!> @endhistory
   SUBROUTINE FROUTPUT(SIMPOS)

      IMPLICIT NONE

      ! Dummy arguments
      CHARACTER(LEN=5), INTENT(IN) :: SIMPOS

      ! Parameters
      INTEGER, PARAMETER :: SEDALLUNIT = 681
      INTEGER, PARAMETER :: SEDFINEUNIT = 682
      INTEGER, PARAMETER :: PSLFILEUNIT = 683
      INTEGER, PARAMETER :: CONTAMUNIT = 684

      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0
      DOUBLE PRECISION, PARAMETER :: ONE = 1.0D0

      ! Locals: strings
      CHARACTER(LEN=20)  :: disextratext, pslextratext
      CHARACTER(LEN=256) :: filnam
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! IOMSG= text from a failed file OPEN.

      ! Locals: scalars
      INTEGER :: L, iface, nminel, i, j, iel, ios
      INTEGER :: hour_now
      DOUBLE PRECISION :: qocav, qocold
      DOUBLE PRECISION :: sedav, sedfineav, contamav
      DOUBLE PRECISION :: uznowt

      ! Persistent state between calls
      INTEGER, SAVE :: disextrapoints = 0
      INTEGER, SAVE :: pslextrapoints = 0
      DOUBLE PRECISION, SAVE :: uzold = ZERO
      DOUBLE PRECISION, SAVE :: next_hour = ZERO
      DOUBLE PRECISION, SAVE :: qoctot = ZERO
      DOUBLE PRECISION, SAVE :: sedtot = ZERO
      DOUBLE PRECISION, SAVE :: sedfinetot = ZERO
      DOUBLE PRECISION, SAVE :: contamtot = ZERO

      ! Persistent optional-output metadata/state
      INTEGER, ALLOCATABLE, SAVE :: pslextraelement(:)
      INTEGER, ALLOCATABLE, SAVE :: disextraelement(:), disextraface(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: qocavextra(:)

      SELECT CASE (SIMPOS)

      CASE ('start')
         CALL initialise_output()

      CASE DEFAULT
         IF (SIMPOS(1:4) == 'main') THEN
            CALL write_main_output()
         ELSE
            CALL write_final_state()
         END IF

      END SELECT

   CONTAINS

!> @brief Opens and primes the regular and optional runtime output streams.
!>
!> The routine reads optional station lists, writes the `DIS2`, `MAS`, and `DIS`
!> headings, locates the outlet weir, and starts the normalised `TOUTPUT`
!> interval counters. Hot starts seed the previous interval from `BHOTTI`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-04-22 | SB | Added the `DIS2` every-timestep discharge heading line. |
!> | 2026-05-03 | SvB | Extracted the start phase from the monolithic `FROUTPUT` implementation. |
!> @endhistory
      SUBROUTINE initialise_output()
         ! Initialise regular and optional output streams.  The optional point
         ! lists are compacted in-place: invalid element/link IDs are skipped and
         ! the retained count is written back to disextrapoints/pslextrapoints.

         IF (ISextradis) CALL initialise_extra_discharge_points()
         IF (ISextrapsl) CALL initialise_extra_water_table_output()

         CALL write_checked(dis2, &
                            'Simulated discharge at the outlet at every model timestep.', &
                            'Error writing to the discharge every timestep at the catchment outlet file '// &
                            '(unit 41 in the rundata file)')

         CALL write_checked(dis2, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet_Discharge(m3/s)', &
                            'Error writing the every-timestep discharge column header to the catchment outlet '// &
                            'file (unit 41 in the rundata file)')

         CALL write_checked(mas, &
                            'Spatially Averaged Totals (mm) over the simulation', &
                            'Error writing to the the mass balance data file (unit 43 in the rundata file)')

         CALL write_checked(mas, &
                            'Time(Hours),'// &
                            'Cumulative_Precipitation,'// &
                            'Cumulative_Canopy_Evaporation,'// &
                            'Cumulative_Soil_Evaporation,'// &
                            'Cumulative_Transpiration,'// &
                            'Cumulative_Aquifer_Flow,'// &
                            'Cumulative_Discharge,'// &
                            'Canopy_Storage,'// &
                            'Snow_Storage,'// &
                            'Subsurface_Storage,'// &
                            'Land_Surface_Storage,'// &
                            'Channel_Storage', &
                            'Error writing the column header to the mass balance data file (unit 43 in the rundata file)')

         WRITE (dis, '(A,F8.2,A)', IOSTAT=ios) &
            'Simulated discharge(m3/s) at the outlet - regular timestep ', &
            TOUTPUT, &
            ' hours. Simulated discharge is the mean value over the timestep '// &
            'with the date at the start of the timestep'
         CALL stop_on_io_error(ios, &
                               'Error writing to the regular discharge at the catchment outlet file '// &
                               '(unit 44 in the rundata file)')

         CALL find_mass_balance_outlet()
         CALL write_discharge_header()

         uznowt = uznow/TOUTPUT
         next_hour = DBLE(INT(uznowt)) + ONE

         ! Hotstart first time is correct.
         IF (BHOTRD) uzold = DBLE(INT(bhotti/TOUTPUT))

         IF (bexsy) CALL initialise_sediment_output()
         IF (bexcm) CALL initialise_contaminant_output()

      END SUBROUTINE initialise_output

!> @brief Reads and compacts the optional virtual-discharge station list.
!>
!> Each retained record supplies a channel-link number and face. Requests whose
!> link exceeds `total_no_links` are silently discarded; malformed input raises
!> fatal frame error 1068.
!>
!> @warning
!> Non-positive link numbers and face numbers outside 1:4 are retained without
!> validation and will later be used as `QOC` indices.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted optional discharge-point setup from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE initialise_extra_discharge_points()
         READ (disextra, *, IOSTAT=ios)
         CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

         READ (disextra, *, IOSTAT=ios) disextratext, disextrapoints
         CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

         CALL allocate_extra_discharge(disextrapoints)

         j = 0
         DO i = 1, disextrapoints
            READ (disextra, *, IOSTAT=ios) L, iface
            CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

            ! Silently ignore discharge requests beyond the link range, matching
            ! the original behaviour while keeping the retained list compact.
            IF (L <= total_no_links) THEN
               j = j + 1
               disextraelement(j) = L
               disextraface(j) = iface
            END IF
         END DO

         disextrapoints = j
      END SUBROUTINE initialise_extra_discharge_points

!> @brief Reallocates and zeroes persistent arrays for `n` virtual discharge stations.
!>
!> This includes retained link/face identifiers, current samples, and
!> interval-integrated discharge. Existing allocations are discarded.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised virtual-station allocation during the output refactor. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
      SUBROUTINE allocate_extra_discharge(n)
         INTEGER, INTENT(IN) :: n

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "FRmod:allocate_extra_discharge"

         IF (ALLOCATED(disextraelement)) THEN
            DEALLOCATE (disextraelement, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "disextraelement", location, emsg)
         END IF
         IF (ALLOCATED(disextraface)) THEN
            DEALLOCATE (disextraface, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "disextraface", location, emsg)
         END IF
         IF (ALLOCATED(qocavextra)) THEN
            DEALLOCATE (qocavextra, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "qocavextra", location, emsg)
         END IF
         IF (ALLOCATED(qoctotextra)) THEN
            DEALLOCATE (qoctotextra, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "qoctotextra", location, emsg)
         END IF

         ALLOCATE (disextraelement(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "disextraelement", location, emsg)
         ALLOCATE (disextraface(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "disextraface", location, emsg)
         ALLOCATE (qocavextra(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "qocavextra", location, emsg)
         ALLOCATE (qoctotextra(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "qoctotextra", location, emsg)

         disextraelement = 0
         disextraface = 0
         qocavextra = ZERO
         qoctotextra = ZERO
      END SUBROUTINE allocate_extra_discharge

!> @brief Reads selected water-table elements and opens their CSV output.
!>
!> Element numbers above `total_no_elements` are silently discarded. Valid
!> entries become columns containing `ZGRUND-ZVSPSL` in metres below ground;
!> a negative result denotes ponded surface water. Input/open failures use frame
!> error 1069.
!>
!> @warning
!> Non-positive element numbers are retained without validation and will later
!> be used to index `ZGRUND` and `ZVSPSL`.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted optional water-table setup from `FROUTPUT`. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
      SUBROUTINE initialise_extra_water_table_output()

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "FRmod:initialise_extra_water_table_output"

         READ (pslextra, *, IOSTAT=ios)
         CALL fatal_on_io_error(ios, 1069, &
                                'no or incorrect data in input_CATCH_water_table_depth file')

         READ (pslextra, *, IOSTAT=ios) pslextratext, pslextrapoints
         CALL fatal_on_io_error(ios, 1069, &
                                'no or incorrect data in input_CATCH_water_table_depth file')

         IF (ALLOCATED(pslextraelement)) THEN
            DEALLOCATE (pslextraelement, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "pslextraelement", location, emsg)
         END IF
         ALLOCATE (pslextraelement(pslextrapoints), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "pslextraelement", location, emsg)
         pslextraelement = 0

         j = 0
         DO i = 1, pslextrapoints
            READ (pslextra, *, IOSTAT=ios) iel
            CALL fatal_on_io_error(ios, 1069, &
                                   'no or incorrect data in input_CATCH_water_table_depth file')

            ! Silently ignore water-table requests beyond the element range,
            ! preserving the original compaction behaviour.
            IF (iel <= total_no_elements) THEN
               j = j + 1
               pslextraelement(j) = iel
            END IF
         END DO

         pslextrapoints = j

         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_water_table_depth.csv')
         OPEN (PSLFILEUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         CALL write_checked(PSLFILEUNIT, &
                            'Water_Table_depth(m_below_ground). A negative number '// &
                            'means there is surface water with the absolute value '// &
                            'the depth of surface water', &
                            'Error writing to the water-table depth output file '//TRIM(filnam))

         WRITE (PSLFILEUNIT, '(A,*(A,I0))', IOSTAT=ios) 'Time(hours)', &
            (', Element-', pslextraelement(j), j=1, pslextrapoints)
         CALL stop_on_io_error(ios, 'Error writing the column header to the water-table depth output file '//TRIM(filnam))
      END SUBROUTINE initialise_extra_water_table_output

!> @brief Selects the outlet link and face used by text and mass-balance output.
!>
!> The search resets `MBLINK` and `MBFACE`, scans every external channel face,
!> and retains the last boundary whose OC boundary-condition type is 7 (weir).
!> Both values remain zero when no such outlet exists.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted outlet discovery from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE find_mass_balance_outlet()
         ! Find outlet link for mass-balance output when no reservoir files exist.
         ! The outlet must be a weir boundary condition, type 7.
         mblink = 0
         mbface = 0

         DO L = 1, total_no_links
            DO iface = 1, 4
               IF (ICMREF(L, 4 + iface) == 0 .AND. NOCBCC(L) > 0) THEN
                  IF (NOCBCD(NOCBCC(L), 3) == 7) THEN
                     mblink = L
                     mbface = NOCBCD(NOCBCC(L), 2)
                  END IF
               END IF
            END DO
         END DO
      END SUBROUTINE find_mass_balance_outlet

!> @brief Writes the regular-discharge CSV column heading.
!>
!> The first discharge column identifies `MBLINK`; when virtual stations are
!> enabled, one `Channel-<link>` column is appended for every retained point.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted discharge-header formatting from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_discharge_header()
         IF (ISextradis) THEN
            WRITE (dis, '(*(A,I0))', IOSTAT=ios) &
               'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-', &
               mblink, (',Channel-', disextraelement(j), j=1, disextrapoints)
            CALL stop_on_io_error(ios, &
                                  'Error writing the column header to the regular discharge at the catchment '// &
                                  'outlet file (unit 44 in the rundata file)')
         ELSE
            CALL write_checked(dis, &
                               'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-Discharge', &
                               'Error writing the column header to the regular discharge at the catchment '// &
                               'outlet file (unit 44 in the rundata file)')
         END IF
      END SUBROUTINE write_discharge_header

!> @brief Opens and labels the total- and fine-sediment outlet CSV files.
!>
!> Output is enabled only when the sediment component is active. Total sediment
!> combines all fractions; fine sediment is fraction 1. Both fluxes are reported
!> in kg/s as interval means dated at the interval start.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted sediment-output setup from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE initialise_sediment_output()
         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_sediment_all.csv')
         OPEN (SEDALLUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_sediment_fine.csv')
         OPEN (SEDFINEUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         WRITE (SEDALLUNIT, '(A)', IOSTAT=ios) &
            'Sediment discharge at the outlet - All Sediments. '// &
            'This is the mean value over the timestep with the date at the start of the timestep'
         CALL stop_on_io_error(ios, 'Error writing to the sed-all-daily-output.csv file')
         CALL write_checked(SEDALLUNIT, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-Discharge(kg/s)', &
                            'Error writing the column header to the sed-all-daily-output.csv file')

         WRITE (SEDFINEUNIT, '(A)', IOSTAT=ios) &
            'Sediment discharge at the outlet - Fine Sediments. '// &
            'This is the mean value over the timestep with the date at the start of the timestep'
         CALL stop_on_io_error(ios, 'Error writing to the sed-fine-daily-output.csv file')
         CALL write_checked(SEDFINEUNIT, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-Discharge(kg/s)', &
                            'Error writing the column header to the sed-fine-daily-output.csv file')

         sedav = ZERO
      END SUBROUTINE initialise_sediment_output

!> @brief Opens and labels the contaminant-one outlet CSV file.
!>
!> The series contains the interval mean of `CCCC(MBLINK,top_cell_no,1)`,
!> described by the file as relative concentration.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted contaminant-output setup from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE initialise_contaminant_output()
         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_contaminant.csv')
         OPEN (CONTAMUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         WRITE (CONTAMUNIT, '(A)', IOSTAT=ios) &
            'Contaminant Relative Concentration (contaminant 1) at the outlet. '// &
            'This is the mean value over the timestep with the date at the start of the timestep.'
         CALL stop_on_io_error(ios, 'Error writing to the contaminant.csv file')
         CALL write_checked(CONTAMUNIT, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Relative_concentration', &
                            'Error writing the column header to the contaminant.csv file')
      END SUBROUTINE initialise_contaminant_output

!> @brief Processes all additional output for one model timestep.
!>
!> Current outlet values are sampled, integrated over normalised output time,
!> emitted at every crossed regular boundary, and retained for the unfinished
!> interval. The routine also writes every-step discharge and scheduled
!> mass-balance/water-table rows before advancing `uzold`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted the timestep phase from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_main_output()
         ! Accumulate mean values in normalised output-time units.  When the
         ! current model time crosses one or more regular output boundaries, write
         ! one row for the just-completed interval and fill any skipped regular
         ! intervals with the current timestep average.

         CALL sample_current_values(qocav, sedav, sedfineav, contamav)

         uznowt = uznow/TOUTPUT
         hour_now = INT(uznowt)

         IF (hour_now < INT(next_hour)) THEN
            CALL accumulate_interval(uznowt - uzold, qocav, sedav, sedfineav, contamav)
         ELSE
            CALL accumulate_interval(next_hour - uzold, qocav, sedav, sedfineav, contamav)
            CALL write_completed_regular_outputs(hour_now, qocav, sedav, sedfineav, contamav)
            CALL restart_accumulators(uznowt - next_hour, qocav, sedav, sedfineav, contamav)

            next_hour = next_hour + ONE
         END IF

         CALL WRITE_DIS2(mbface, qocav, uznow)
         CALL write_periodic_mass_balance()

         uzold = uznowt

         ! A 1-D run may have no configured outlet face.
         IF (mblink == 0 .AND. mbface == 0) THEN
            qocav = ZERO
         ELSE
            qocold = qoc(mblink, mbface)
         END IF

      END SUBROUTINE write_main_output

!> @brief Samples outlet discharge, sediment flux, and contaminant concentration.
!>
!> When no outlet was found, all returned values are zero. Otherwise total
!> sediment is the sum of all `QSED` fractions multiplied by `RHO_SEDIMENT`, fine
!> sediment is fraction 1, and contaminant output is the top-cell concentration
!> of contaminant 1. Optional station discharges are also refreshed.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted outlet sampling from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE sample_current_values(q_out, sed_out, sedfine_out, contam_out)
         DOUBLE PRECISION, INTENT(OUT) :: q_out
         DOUBLE PRECISION, INTENT(OUT) :: sed_out
         DOUBLE PRECISION, INTENT(OUT) :: sedfine_out
         DOUBLE PRECISION, INTENT(OUT) :: contam_out

         IF (mblink == 0 .AND. mbface == 0) THEN
            q_out = ZERO
            sed_out = ZERO
            sedfine_out = ZERO
            contam_out = ZERO
         ELSE
            q_out = qoc(mblink, mbface)

            IF (bexsy) THEN
               sed_out = ZERO
               DO i = 1, nsed
                  sed_out = sed_out + QSED(mblink, i, mbface)*RHO_SEDIMENT
               END DO
               sedfine_out = QSED(mblink, 1, mbface)*RHO_SEDIMENT
            ELSE
               sed_out = ZERO
               sedfine_out = ZERO
            END IF

            IF (bexcm) THEN
               contam_out = CCCC(mblink, top_cell_no, 1)
            ELSE
               contam_out = ZERO
            END IF
         END IF

         IF (ISextradis) THEN
            DO i = 1, disextrapoints
               qocavextra(i) = qoc(disextraelement(i), disextraface(i))
            END DO
         END IF
      END SUBROUTINE sample_current_values

!> @brief Integrates current samples over part of a regular output interval.
!>
!> `dt` is measured in units of `TOUTPUT`, so the accumulated values become
!> interval means when a complete unit interval is written. Sediment,
!> contaminant, and virtual-station totals are updated only when enabled.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted interval accumulation from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE accumulate_interval(dt, q_mean, sed_mean, sedfine_mean, contam_mean)
         DOUBLE PRECISION, INTENT(IN) :: dt
         DOUBLE PRECISION, INTENT(IN) :: q_mean
         DOUBLE PRECISION, INTENT(IN) :: sed_mean
         DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
         DOUBLE PRECISION, INTENT(IN) :: contam_mean

         qoctot = qoctot + q_mean*dt

         IF (bexsy) THEN
            sedtot = sedtot + sed_mean*dt
            sedfinetot = sedfinetot + sedfine_mean*dt
         END IF

         IF (bexcm) contamtot = contamtot + contam_mean*dt

         IF (ISextradis) THEN
            do i = 1, disextrapoints
               qoctotextra(i) = qoctotextra(i) + qocavextra(i)*dt
            end do
         END IF
      END SUBROUTINE accumulate_interval

!> @brief Writes a completed regular interval and fills any crossed intervals.
!>
!> The accumulated interval is timestamped at `next_hour-1`. If one model
!> timestep spans further boundaries, those intermediate rows use the current
!> sample directly, matching the legacy averaging behaviour.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted boundary-crossing output from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_completed_regular_outputs(hour_now, q_mean, sed_mean, sedfine_mean, contam_mean)
         INTEGER, INTENT(IN) :: hour_now
         DOUBLE PRECISION, INTENT(IN) :: q_mean
         DOUBLE PRECISION, INTENT(IN) :: sed_mean
         DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
         DOUBLE PRECISION, INTENT(IN) :: contam_mean

         DOUBLE PRECISION :: output_hour

         ! output_hour = next_hour - 1.0D0:
         !     mean value over the regular timestep, timestamped at the start
         !     of the timestep.
         ! output_hour = next_hour:
         !     equivalent mean value timestamped at the end of the timestep.
         output_hour = next_hour - ONE

         CALL write_regular_outputs(output_hour, ABS(qoctot), disextrapoints, qoctotextra, &
                                    sedtot, sedfinetot, contamtot)

         DO i = INT(next_hour) + 1, hour_now
            next_hour = DBLE(i)
            output_hour = next_hour - ONE

            CALL write_regular_outputs(output_hour, ABS(q_mean), disextrapoints, qocavextra, &
                                       sed_mean, sedfine_mean, contam_mean)
         END DO
      END SUBROUTINE write_completed_regular_outputs

!> @brief Seeds interval accumulators with the portion after an output boundary.
!>
!> Each enabled total is replaced by its current sample multiplied by `dt`,
!> where `dt` is the remaining fraction of the current `TOUTPUT` interval.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted post-boundary state handling from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE restart_accumulators(dt, q_mean, sed_mean, sedfine_mean, contam_mean)
         DOUBLE PRECISION, INTENT(IN) :: dt
         DOUBLE PRECISION, INTENT(IN) :: q_mean
         DOUBLE PRECISION, INTENT(IN) :: sed_mean
         DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
         DOUBLE PRECISION, INTENT(IN) :: contam_mean

         qoctot = q_mean*dt

         IF (bexsy) THEN
            sedtot = sed_mean*dt
            sedfinetot = sedfine_mean*dt
         END IF

         IF (bexcm) contamtot = contam_mean*dt

         IF (ISextradis) THEN
            qoctotextra(1:disextrapoints) = qocavextra(1:disextrapoints)*dt
            do i = 1, disextrapoints
               qoctotextra(i) = qocavextra(i)*dt
            end do
         END IF
      END SUBROUTINE restart_accumulators

!> @brief Writes one timestamped row to each enabled regular-output stream.
!>
!> `output_hour` is an interval index and is converted to elapsed hours using
!> `TOUTPUT`. Outlet discharge is supplied already non-negative; optional
!> station discharges are made absolute. Sediment and contaminant rows are
!> written only when their components are active.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised regular CSV row formatting during the output refactor. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
      SUBROUTINE write_regular_outputs(output_hour, discharge, disextrapoints, discharge_extra, &
                                       sediment, sediment_fine, contaminant)
         DOUBLE PRECISION, INTENT(IN) :: output_hour
         DOUBLE PRECISION, INTENT(IN) :: discharge
         INTEGER, INTENT(IN) :: disextrapoints
         DOUBLE PRECISION, INTENT(IN) :: discharge_extra(:)
         DOUBLE PRECISION, INTENT(IN) :: sediment
         DOUBLE PRECISION, INTENT(IN) :: sediment_fine
         DOUBLE PRECISION, INTENT(IN) :: contaminant

         CHARACTER(LEN=32) :: stamp
         DOUBLE PRECISION  :: elapsed

         CHARACTER(len=32), DIMENSION(:), allocatable :: buf
         CHARACTER(len=32) :: bufdis

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "FRmod:write_regular_outputs"

         SAVE buf

         IF (ALLOCATED(buf)) THEN
            DEALLOCATE (buf, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "buf", location, emsg)
         END IF
         ALLOCATE (buf(disextrapoints), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "buf", location, emsg)
         buf = ''

         elapsed = output_hour*TOUTPUT
         stamp = timestamp_from_output_hour(output_hour)

         write (bufdis, '(F20.5)') discharge
         bufdis = adjustl(bufdis)
         if (ISextradis) then
            do j = 1, disextrapoints
               write (buf(j), '(F20.5)') abs(discharge_extra(j))
               buf(j) = adjustl(buf(j))
            end do
           WRITE (dis, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis), (',', trim(buf(j)), j=1, disextrapoints)
         else
            WRITE (dis, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
         end if

         if (bexsy) then
            write (bufdis, '(F20.5)') sediment
            bufdis = adjustl(bufdis)
            write (SEDALLUNIT, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
            write (bufdis, '(F20.5)') sediment_fine
            bufdis = adjustl(bufdis)
            write (SEDFINEUNIT, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
         end if
         if (bexcm) then
            write (bufdis, '(F20.5)') contaminant
            bufdis = adjustl(bufdis)
            write (CONTAMUNIT, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
         end if

      END SUBROUTINE write_regular_outputs

!> @brief Converts a regular-output interval index to an absolute timestamp.
!>
!> The timestamp represents `TIH + output_hour*TOUTPUT` and is formatted
!> `yyyy-mm-dd HH:MM:SS`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised output timestamp generation. |
!> @endhistory
      FUNCTION timestamp_from_output_hour(output_hour) RESULT(stamp)
         DOUBLE PRECISION, INTENT(IN) :: output_hour
         CHARACTER(LEN=32) :: stamp
         INTEGER :: c(6)

         c = DATE_FROM_HOUR(tih + output_hour*TOUTPUT)

         WRITE (stamp, '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
            c(1), c(2), c(3), c(4), c(5), c(6)
      END FUNCTION timestamp_from_output_hour

!> @brief Writes scheduled catchment-average balance and water-table rows.
!>
!> Once `UZNOW` exceeds `icounter2`, cumulative balance/storage entries
!> `BALANC(7:17)` are converted from cubic metres to millimetres over `CAREA`
!> and written to `MAS`. The threshold then advances by 24 h. Selected
!> water-table depths are written on the same schedule.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted periodic mass-balance output from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_periodic_mass_balance()
         IF (uznow <= icounter2) RETURN

         WRITE (mas, '(F16.3,11('','',F16.3))') uznow, &
            balanc(7)*1000.0D0/carea, &
            balanc(8)*1000.0D0/carea, &
            balanc(9)*1000.0D0/carea, &
            balanc(10)*1000.0D0/carea, &
            balanc(11)*1000.0D0/carea, &
            balanc(12)*1000.0D0/carea, &
            balanc(13)*1000.0D0/carea, &
            balanc(14)*1000.0D0/carea, &
            balanc(15)*1000.0D0/carea, &
            balanc(16)*1000.0D0/carea, &
            balanc(17)*1000.0D0/carea

         icounter2 = icounter2 + 24.0D0

         IF (ISextrapsl) THEN
            WRITE (PSLFILEUNIT, '(F10.2,*(1A,F10.2))') uznow, &
               (',', zgrund(pslextraelement(i)) - zvspsl(pslextraelement(i)), &
                i=1, pslextrapoints)
         END IF
      END SUBROUTINE write_periodic_mass_balance

!> @brief Writes the final phreatic surface and pressure heads for VSI reuse.
!>
!> With banks active, output begins at element 1; otherwise channel links are
!> omitted and output begins at `total_no_links+1`. Each included element writes
!> `VSPSI` from its bottom active layer through `top_cell_no`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted end-of-simulation state output from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_final_state()
         ! The individual records are written unconditionally; ios carries the
         ! first failure through to a single fatal check at the end.
         ios = 0
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'Output at end of simulation for use as initial conditions in vsi file'
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'This output is by element number'
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios)
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'phreatic surface level '

         IF (bexbk) THEN
            nminel = 1
         ELSE
            nminel = total_no_links + 1
         END IF

         IF (ios == 0) WRITE (vse, '(10(1X,F9.3))', IOSTAT=ios) (zvspsl(j), j=nminel, total_no_elements)
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios)
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'Heads at end of simulation'

         DO iel = 1, total_no_elements
            IF (ios == 0 .AND. (bexbk .OR. iel > total_no_links)) THEN
               WRITE (vse, '(I7)', IOSTAT=ios) iel
               IF (ios == 0) WRITE (vse, '(10(1X,F9.3))', IOSTAT=ios) &
                  (VSPSI(j, iel), j=nlyrbt(iel, 1), top_cell_no)
            END IF
         END DO

         CALL stop_on_io_error(ios, &
                               'Error writing the end-of-simulation VSS state to the vsi initial-conditions '// &
                               'file (unit 42 in the rundata file)')
      END SUBROUTINE write_final_state

!> @brief Writes one text record and applies the standard fatal output check.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised checked heading writes during the output refactor. |
!> @endhistory
      SUBROUTINE write_checked(unit, line, error_message)
         INTEGER, INTENT(IN) :: unit
         CHARACTER(LEN=*), INTENT(IN) :: line
         CHARACTER(LEN=*), INTENT(IN) :: error_message

         WRITE (unit, '(A)', IOSTAT=ios) line
         CALL stop_on_io_error(ios, error_message)
      END SUBROUTINE write_checked

!> @brief Converts a nonzero output status into a console diagnostic and `ERROR STOP`.
!>
!> The supplied message is followed by a reminder to close software that may
!> have locked the output file. A zero status returns without side effects.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised fatal text-output handling. |
!> @endhistory
      SUBROUTINE stop_on_io_error(io_status, message)
         INTEGER, INTENT(IN) :: io_status
         CHARACTER(LEN=*), INTENT(IN) :: message

         IF (io_status == 0) RETURN

         WRITE (*, '(A)') message
         WRITE (*, '(A)') 'Check it is not open in other software (e.g. Excel)'
         CALL ERR_STOP(255)
      END SUBROUTINE stop_on_io_error

!> @brief Routes a nonzero input/output status through the shared frame error service.
!>
!> On failure, `error_code` and `message` are passed to
!> `ERROR(ERRLVL_fatal,...)`; a zero status returns normally.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised numbered fatal I/O checks during the output refactor. |
!> @endhistory
      SUBROUTINE fatal_on_io_error(io_status, error_code, message)
         INTEGER, INTENT(IN) :: io_status
         INTEGER, INTENT(IN) :: error_code
         CHARACTER(LEN=*), INTENT(IN) :: message

         IF (io_status /= 0) CALL RAISE_ERROR(ERRLVL_fatal, error_code, FID_logfile, 0, 0, message)
      END SUBROUTINE fatal_on_io_error

   END SUBROUTINE FROUTPUT

!> @brief Writes one discharge value using the configured mass-balance face sign convention.
!>
!> Faces 1 and 2 preserve the sign of `qoo`; faces 3 and 4 reverse it before
!> writing to the regular discharge unit `DIS`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | Legacy | - | - | Added regular discharge output using the OC face sign convention. |
!> | 2026-04-04 | SvB | 4.6.1 | Standardised Fortran formatting without changing the sign rule. |
!> @endhistory
   SUBROUTINE write_dis(mbface, qoo)
      INTEGER, INTENT(IN)            :: mbface
      DOUBLEPRECISION, INTENT(IN)    :: qoo
      DOUBLEPRECISION                :: qd
      IF ((mbface == 1) .OR. (mbface == 2)) THEN
         qd = qoo
      ELSE
         qd = -qoo
      END IF
      WRITE (dis, '(F20.8)') qd
   END SUBROUTINE write_dis

!> @brief Writes one timestamped discharge record using the configured face sign convention.
!>
!> Faces 1 and 2 preserve the sign of `qoo`; faces 3 and 4 reverse it. The
!> timestamp is `TIH + TME` converted with [[datetime:date_from_hour]], and the
!> row is written to `DIS2` as date/time, simulation hour, and discharge.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2006-03 | SB | 4.x | Added outlet discharge at every model timestep. |
!> | 2026-07-08 | SB | 4.6.1 | Made numeric text formatting explicit and checked output writes. |
!> @endhistory
   SUBROUTINE write_dis2(mbface, qoo, tme)
      INTEGER, INTENT(IN)            :: mbface
      INTEGER                        :: c(6)
      DOUBLEPRECISION, INTENT(IN)    :: qoo, tme
      DOUBLEPRECISION                :: qd
      CHARACTER(128)                 :: dum
      CHARACTER(len=32)              :: bufdis2
      IF ((mbface == 1) .OR. (mbface == 2)) THEN
         qd = qoo
      ELSE
         qd = -qoo
      END IF
      c = DATE_FROM_HOUR(tih + tme)
      WRITE (dum, '(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1), '-', c(2), '-', c(3), ' ', c(4), ':', c(5), ':', c(6)
      WRITE (bufdis2, '(F20.5)') qd
      bufdis2 = adjustl(bufdis2)
      WRITE (dis2, '(A,A1,F0.5,A1,A)') TRIM(dum), ',', tme, ',', TRIM(bufdis2)
   END SUBROUTINE write_dis2

!> @brief Fatal check for an unformatted restart/result-file `WRITE` in `FRRESC` / `FRRESP`.
!>
!> The restart and per-set result files hold heterogeneous array records that
!> cannot pass through a single typed wrapper, so each `WRITE` carries its own
!> `IOSTAT=`/`IOMSG=` and calls this routine, which forwards a non-zero status to
!> [[error_status:errstat_write]] together with the resolved result-file stem.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-09-07 | SvB | Initial version, wiring the `RES` / `IORES` writes to `errstat_write`. |
!> @endhistory
   SUBROUTINE res_write_check(status, iomsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! `IOSTAT=` value from the restart/result-file `WRITE`.
      CHARACTER(LEN=*), INTENT(IN)  :: iomsg  !! `IOMSG=` text from the restart/result-file `WRITE`.

      CALL errstat_write(status, 'FRRESC/FRRESP unformatted result file', iomsg, TRIM(RESFIL))
   END SUBROUTINE res_write_check

!> @brief Writes result-file control headers and opens unformatted result datasets.
!>
!> `FRRESC` serialises output class definitions and common model metadata to the
!> legacy results file, then opens the unformatted result files used by
!> [[frresp]] for selected output sets and data classes.
!>
!> The header contains the SHETRAN version, result filename stem, model
!> dimensions, component file units, output set/class definitions, element/grid
!> topology, VSS connectivity, soil and vegetation tables, bed/channel geometry,
!> boundary and component-enable flags, and VSS soil-property tables. The write
!> order intentionally does not always follow the old COMMON-block ordering
!> because some arrays must be read back in a specific order. `IORES` is filled
!> with the unformatted result-file units opened for the selected output data.
!>
!> | Header section | Main contents |
!> |:---------------|:--------------|
!> | Version/dimensions/topology | `SHEVER`, `NX`, `NY`, `NGDBGN`, element count, `ICMREF`, `ICMXY`, file units. |
!> | Vertical/element geometry | layer counts, cell depths, bank/link maps, faces, bed cells, vegetation/soil/well category maps. |
!> | Physical geometry | element area, channel length/width, `DHF`, `DXQQ`, `DYQQ`, bank fractions, ground and VSS node elevations. |
!> | Run/output controls | component flags, time-step controls, output classes, output data ids, output elements, link-code maps, output timing. |
!> | Soil hydraulic tables | VSS table count and `VSPPSI`, `VSPTHE`, `VSPKR`, `VSPETA`, `VSPDTH`, `VSPDKR`, `VSPDET`. |
!>
!> After the header is written, `RES` is closed so the result header can be
!> inspected before the simulation finishes. Each selected output set then opens
!> one unformatted data file on unit `50+set`, named by appending the two-digit
!> set number to the resolved `RESFIL` stem.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Made typing explicit. |
!> | 1997-1998 | RAH | 4.0-4.2 | Updated VSS metadata, array ordering, output classes, and unformatted result-file setup. |
!> | 2026-09-06 | SvB | - | Checked the unformatted result-file `OPEN` through [[error_status:errstat_fileopen]]. |
!> | 2026-09-06 | SvB | - | Checked the result-file `CLOSE` through [[error_status:errstat_fileclose]]. |
!> | 2026-09-07 | SvB | - | Checked every unformatted header `WRITE` through `res_write_check` / [[error_status:errstat_write]]. |
!> @endhistory
   SUBROUTINE FRRESC

      IMPLICIT NONE

      ! Locals, etc
      INTEGER, PARAMETER :: IDUM0 = 0
      DOUBLE PRECISION, PARAMETER :: FDUM0 = 0.0D0
      LOGICAL, PARAMETER :: LDUM0 = .TRUE.

      INTEGER :: I, ICHAR, ISET, J, K, L
      INTEGER :: ios
      CHARACTER(2) :: ANUM
      CHARACTER(128) :: fname
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! IOMSG= text from a failed result-file OPEN or WRITE.

      ! WRITE SHETRAN VERSION
      !1
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) SHEVER
      CALL res_write_check(ios, emsg)

      ! ALGCB1
      !2
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) NX, NY, NGDBGN, total_no_elements
      CALL res_write_check(ios, emsg)

      ! ALGCB2
      !3-4
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ICMREF(I, J), I=1, total_no_elements), J=1, 12)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ICMXY(I, J), I=1, NX), J=1, NY)
      CALL res_write_check(ios, emsg)

      ! CFILE + DFILE (except SFB,SRB)
      !5
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) FRD, VSD, OCD, ETD, PPD, SMD, BKD, SYD, CMD, MED, PRD, &
         EPD, TIM, FID_logfile, SPR, CMP, BUG, RES, HOT, VSI, VED, WLD, LFB, LHB, &
         LGB, BFB, BHB, OFB, OHB, CMT, CMB
      CALL res_write_check(ios, emsg)

      ! ALCCB1
      !6
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) top_cell_no, total_no_links, NS, NV, ERRLVL_warn, ERRLVL_error, ERRLVL_fatal
      CALL res_write_check(ios, emsg)

      ! IVEG
      !7
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NRD(I), I=1, NV)
      CALL res_write_check(ios, emsg)

      ! VEG
      !8
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((RDF(I, J), J=1, NRD(I)), I=1, NV)
      CALL res_write_check(ios, emsg)

      ! CAREA (ALDCB3 - see also below) + ALCB1A
      !9
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) CAREA, TIH
      CALL res_write_check(ios, emsg)

      ! ALCCB3
      !10-11
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (LINKNS(L), L=1, total_no_links)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) BEXBK
      CALL res_write_check(ios, emsg)

      ! ALCCB5
      !12-27
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ICMBK(I, J), I=1, total_no_links), J=1, 2)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ICMRF2(I, J), I=1, total_no_links), J=1, 6)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (((JVSACN(K, J, I), K=1, 4), J=1, top_cell_no), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (((JVSDEL(K, J, I), K=1, 4), J=1, top_cell_no), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NLYR(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((NLYRBT(I, J), J=1, NLYR(I)), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NBFACE(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((NHBED(I, J), I=1, total_no_links), J=1, 2)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((NTSOIL(I, J), J=1, NLYR(I)), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NVC(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NVSSPC(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NVSSPT(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NVSWLI(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NVSWLT(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NWELBT(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NWELTP(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)

      ! ALCCB7 (except THSAT)
      !28-42
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (cellarea(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (CLENTH(I), I=1, total_no_links)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (CWIDTH(I), I=1, total_no_links)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((DELTAZ(J, I), J=1, top_cell_no), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((DHF(I, J), I=1, total_no_elements), J=1, 4)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (DXQQ(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (DYQQ(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((FHBED(I, J), I=1, total_no_links), J=1, 2)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (RDL(I), I=1, NV)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (VSPOR(I), I=1, NS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (ZBEFF(I), I=1, total_no_links)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (ZBFULL(I), I=1, total_no_links)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (ZGRUND(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ZLYRBT(I, J), J=1, NLYR(I)), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ZVSNOD(J, I), J=1, top_cell_no), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)

      ! ALDCB1 (except MBLINK,MBFACE,MBFLAG)
      !43
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) MSM, IDUM0, NM, NRAIN, NSET, NXP1, NYP1, NXM1, NYM1, &
         NXE, NYE, NXEP1, NYEP1
      CALL res_write_check(ios, emsg)

      ! ALDCB3 (except CAREA - see above)
      !44
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) FDUM0, DTMET, QMAX, BHOTTI, BHOTST, PMAX, PALFA, TMAX, BWIDTH, TTH
      CALL res_write_check(ios, emsg)

      ! ALDCB5
      !45
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) BEXET, LDUM0, LDUM0, BEXOC, LDUM0, BEXSM, LDUM0, &
         BHOTPR, BHOTRD, BEXSY, BEXCM
      CALL res_write_check(ios, emsg)

      ! ALDCB6 (except NOCBCC, NOCBCD)
      !46-59
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NMC(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((INGRID(I, J), I=1, NX), J=1, NY)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (NRAINC(I), I=1, total_no_elements)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (IOCORS(I), I=1, NSET)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (ICLNUM(I), I=1, NCLASS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((ICLIST(I, J), I=1, total_no_elements), J=1, NCLASS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (IODATA(I), I=1, NSET)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (IOELEM(I), I=1, NSET)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((LCODEX(I, J), I=1, NX), J=1, NY)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((LCODEY(I, J), I=1, NX), J=1, NY)
      CALL res_write_check(ios, emsg)

      ! ALDCB8 (except RHOSAR)
      !60-71
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (DXIN(I), I=1, NX)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (DYIN(I), I=1, NY)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (IOSTA(I), I=1, NSET)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (IOSTEP(I), I=1, NSET)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (IOEND(I), I=1, NSET)
      CALL res_write_check(ios, emsg)

      ! VSSOLI/VSSOLR (except VSPSS, VSPPOR)
      !72-79
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) NVSSOL
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) (VSPPSI(I), I=1, NVSSOL)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((VSPTHE(I, J), I=1, NVSSOL), J=1, NS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((VSPKR(I, J), I=1, NVSSOL), J=1, NS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((VSPETA(I, J), I=1, NVSSOL), J=1, NS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((VSPDTH(I, J), I=1, NVSSOL), J=1, NS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((VSPDKR(I, J), I=1, NVSSOL), J=1, NS)
      CALL res_write_check(ios, emsg)
      WRITE (RES, IOSTAT=ios, IOMSG=emsg) ((VSPDET(I, J), I=1, NVSSOL), J=1, NS)
      CALL res_write_check(ios, emsg)

      ! CLOSE RES FILE, SO THAT RESULTS CAN BE INSPECTED USING SHEGRAPH BEFORE
      ! SIMULATION HAS TERMINATED
      !
      CLOSE (RES, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, TRIM(RESFIL), RES, emsg)

      ! OPEN OUTPUT DATA FILES ON FILE UNITS 50 ONWARDS
      !
      IF (NSET > 0) THEN
         ! Modernized: Find the actual length of the filename string
         ICHAR = LEN_TRIM(RESFIL)

         DO ISET = 1, NSET
            IORES(ISET) = 50 + ISET
            WRITE (ANUM, '(I2.2)') ISET
            fname = RESFIL(:ICHAR)//ANUM
            OPEN (IORES(ISET), FILE=TRIM(fname), FORM='UNFORMATTED', IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileopen(ios, TRIM(fname), emsg)
            WRITE (*, '(" OPENING FILE UNIT",I3," TO FILE ",2A)') IORES(ISET), RESFIL(:ICHAR), ANUM
         END DO
      END IF

   END SUBROUTINE FRRESC

!> @brief Writes selected results to legacy result files.
!>
!> Output is controlled by user-defined output sets, output classes, and output
!> times. The routine assembles the requested water-flow and component data into
!> output buffers and writes only the records due on the current call.
!>
!> On each call, data are written only for data types marked with `1` in
!> `AIOSTO`, allowing different SHETRAN components to call `FRRESP`
!> selectively. Entry conditions require `NELEE >= 1`, `1 <= NSET <= NSETEE`,
!> each `IODATA(set)` within the `AIOSTO` range, `IOELEM(set)` either a valid
!> element or a valid output class selector, class lists `ICLNUM`/`ICLIST`
!> within element bounds, contaminant-oriented data types `21:38` and `44`
!> using `1 <= IOCORS(set) <= NCON`, and each `IORES(set)` connected for
!> unformatted output.
!>
!> | Output-id range | Data group | Notes |
!> |:----------------|:-----------|:------|
!> | 1:8 | ET, surface input, storage, and head rates | Fluxes in m/s are converted to mm/hour with `3600000`; canopy storage is written as stored. |
!> | 9, 13, 14, 19, 20 | Column or face arrays | Written immediately as `(RESNOW, array)` records and bypass the scalar `BUFFER`. |
!> | 10:12, 15, 17, 18 | Snow, phreatic/surface depth, channel exchange, springs | Undefined or non-applicable cases use `999.999`. |
!> | 21:31, 44 | Sediment and erosion | `IOCORS=0` means all sediment fractions; positive `IOCORS` selects one fraction. |
!> | 32:38 | Contaminant concentrations | `IOCORS` selects contaminant number; ids 32 and 33 write full vertical profiles. |
!> | 39:43, 45:49 | Wells and placeholders | 39, 40, and 45:49 are undefined; 41/42 write well abstraction, 43 water-balance error. |
!> | 50 | Water-balance summary | `BALANC(j)*1000/CAREA`, so volumes are reported as catchment-depth millimetres. |
!>
!> The selector string `AIOSTO` is a per-call mask: output id `IDATA` is ignored
!> unless `AIOSTO(IDATA:IDATA) == '1'`. When `NOW=.FALSE.`, the routine also
!> enforces `IOTIME`/`IOEND`; when `NOW=.TRUE.`, those timing checks are bypassed.
!>
!> Sediment fraction bounds are selected by two statement functions:
!>
!> \[
!> SFSED1(c)=\max(1,c),\qquad SFSED2(c)=\max(NSED(1-c),c).
!> \]
!>
!> Therefore `IOCORS=0` expands to fractions `1:NSED`, while `IOCORS>0` selects
!> exactly that sediment fraction.
!>
!> Cumulative erosion output id 44 uses elapsed time since `PREVTM` to update
!> `GNUCUM` in mm:
!>
!> \[
!> GNUCUM \leftarrow GNUCUM + GNU(RESNOW-PREVTM)3600\,1000.
!> \]
!>
!> @warning
!> Module state `PREVTM` and `GNUCUM` is not initialised before this update.
!> Unless a caller or processor supplies known startup values, the first
!> cumulative-erosion calculation for output id 44 is undefined.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1997-1998 | RAH | 4.1-4.2 | Updated VSS, sediment, contaminant, well, and water-balance result selectors. |
!> | 2026-04-05 | SvB | 4.6.1 | Replaced removed legacy initialisers while retaining result-file layout. |
!> | 2026-09-07 | SvB | - | Checked every unformatted result `WRITE` through `res_write_check` / [[error_status:errstat_write]]. |
!> @endhistory
   SUBROUTINE FRRESP(AIOSTO, RESNOW, NOW)

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: RESNOW
      LOGICAL, INTENT(IN)          :: NOW
      CHARACTER(LEN=*), INTENT(IN) :: AIOSTO

      ! Locals
      DOUBLE PRECISION, PARAMETER  :: UNDEF = 999.999D0
      INTEGER :: SFSED1, SFSED2
      DOUBLE PRECISION :: DUM1(4)
      INTEGER :: ICLASS, ICORS, IDATA, IEL, ISET, IW, J, K, KK, NOUT
      DOUBLE PRECISION :: BUFFER(NELEE), COLBUF(LLEE)
      DOUBLE PRECISION :: DUMO, DUM0

      LOGICAL :: COLUMN
      INTEGER :: SED
      INTEGER(KIND=I_P) :: ios !! `IOSTAT=` from an unformatted result-file `WRITE`.
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from an unformatted result-file `WRITE`.

      ! --- LOOP OVER ALL OUTPUT SETS
      !
      !^^^^ sb 4/2/99
      !^^^^ cummulative soil loss data type 44
      IF (.NOT. SEDSRT) THEN
         DO J = 1, total_no_elements
            IF (NOTZERO(DLS(J))) SEDSRT = .TRUE.
            DLSSRT(J) = DLS(J)
         END DO
      END IF

      DO J = 1, total_no_elements
         GNUCUM(J) = GNUCUM(J) + GNU(J)*(RESNOW - PREVTM)*3600.0D0*1000.0D0
      END DO

      output_loop: DO ISET = 1, NSET
         COLUMN = .FALSE.

         ! CHECK IF DATA FOR THIS SET IS TO BE OUTPUT NOW.
         IF (.NOT. NOW) THEN
            IF (RESNOW < IOTIME(ISET) - 1.0D-6) CYCLE output_loop
            IF (IOTIME(ISET) >= IOEND(ISET)) CYCLE output_loop
         END IF

         IDATA = IODATA(ISET)
         IF (IDATA < 1 .OR. IDATA > MIN(LEN(AIOSTO), 50)) CYCLE output_loop
         IF (AIOSTO(IDATA:IDATA) /= '1') CYCLE output_loop

         ! SET UP NUMBER OF DATA ITEMS TO BE WRITTEN
         IF (IOELEM(ISET) > 0) THEN
            NOUT = 1
         ELSE
            ICLASS = -IOELEM(ISET)
            NOUT = ICLNUM(ICLASS)
         END IF
         ICORS = IOCORS(ISET)

         ! Array limits for sediment loops
         SFSED1 = MAX(1, ICORS)
         SFSED2 = MAX(NSED*(1 - ICORS), ICORS)

         ! ASSEMBLE OUTPUT BUFFER
         DO J = 1, NOUT
            IF (IOELEM(ISET) > 0) THEN
               IEL = IOELEM(ISET)
            ELSE
               IEL = ICLIST(J, ICLASS)
            END IF

            SELECT CASE (IODATA(ISET))
            CASE (1)
               BUFFER(J) = PNETTO(IEL)*3600000.0D0
            CASE (2)
               BUFFER(J) = EPOT(IEL)*3600000.0D0
            CASE (3)
               BUFFER(J) = ERZA(IEL)*3600000.0D0
            CASE (4)
               BUFFER(J) = ESOILA(IEL)*3600000.0D0
            CASE (5)
               BUFFER(J) = EINTA(IEL)*3600000.0D0
            CASE (6)
               BUFFER(J) = DRAINA(IEL)*3600000.0D0
            CASE (7)
               BUFFER(J) = CSTORE(IEL)
            CASE (8)
               BUFFER(J) = QH(IEL)*3600000.0D0
            CASE (9)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (QVSV(K, IEL), K=1, top_cell_no)
               CALL res_write_check(ios, emsg)
            CASE (10)
               BUFFER(J) = SD(IEL)
            CASE (11)
               BUFFER(J) = TS(IEL)
            CASE (12)
               BUFFER(J) = ZVSPSL(IEL) - ZGRUND(IEL)
            CASE (13)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (((QVSH(KK, K, IEL)), K=1, top_cell_no), KK=1, 4)
               CALL res_write_check(ios, emsg)
            CASE (14)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (QOC(IEL, K), K=1, 4)
               CALL res_write_check(ios, emsg)
            CASE (15)
               BUFFER(J) = GETHRF(IEL) - ZGRUND(IEL)
            CASE (16)
               BUFFER(J) = UNDEF
            CASE (17)
               IF (IEL <= total_no_links) THEN
                  BUFFER(J) = QBKB(IEL, 1) + QBKB(IEL, 2) + QBKF(IEL, 1) + QBKF(IEL, 2)
               ELSE
                  BUFFER(J) = UNDEF
               END IF
            CASE (18)
               BUFFER(J) = QVSSPR(IEL)
            CASE (19)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (VSPSI(K, IEL), K=1, top_cell_no)
               CALL res_write_check(ios, emsg)
            CASE (20)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (VSTHE(K, IEL), K=1, top_cell_no)
               CALL res_write_check(ios, emsg)
            CASE (21)
               DUM0 = DLS(IEL)
               IF (ICORS > 0) DUM0 = DUM0*FBETA(IEL, ICORS)
               BUFFER(J) = 1.0D3*DUM0
            CASE (22)
               DUM0 = 0.0D0
               DO SED = SFSED1, SFSED2
                  DUM0 = DUM0 + FDEL(IEL, SED)
               END DO
               BUFFER(J) = 1.0D3*RHO_SEDIMENT*(1.0D0 - PLS(IEL))*DUM0
            CASE (23)
               BUFFER(J) = GINFD(IEL, ICORS)
            CASE (24)
               BUFFER(J) = GINFS(IEL, ICORS)
            CASE (25)
               BUFFER(J) = 1000.0D0*24.0D0*3600.0D0*GNU(IEL)
            CASE (26)
               BUFFER(J) = 1000.0D0*24.0D0*3600.0D0*GNUBK(IEL)
            CASE (27)
               COLUMN = .TRUE.
               DO K = 1, 4
                  DUM0 = 0.0D0
                  DO SED = SFSED1, SFSED2
                     DUM0 = DUM0 + QSED(IEL, SED, K)
                  END DO
                  DUM1(K) = DUM0*RHO_SEDIMENT
               END DO
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, DUM1
               CALL res_write_check(ios, emsg)
            CASE (28)
               DUM0 = 0.0D0
               DO SED = SFSED1, SFSED2
                  DUM0 = DUM0 + QSED(IEL, SED, 1) + QSED(IEL, SED, 2) + &
                         QSED(IEL, SED, 3) + QSED(IEL, SED, 4)
               END DO
               BUFFER(J) = DUM0*RHO_SEDIMENT
            CASE (29)
               IF (DCBED(IEL) > 0.0D0) THEN
                  BUFFER(J) = DCBSED(IEL, ICORS)/DCBED(IEL)
               ELSE
                  BUFFER(J) = ZERO
               END IF
            CASE (30)
               COLUMN = .TRUE.
               DO K = 1, 4
                  DUM0 = 0.0D0
                  DO SED = SFSED1, SFSED2
                     IF (QOC(IEL, K) > ZERO) THEN
                        DUM0 = DUM0 + QSED(IEL, SED, K)/QOC(IEL, K)
                     ELSE
                        DUMO = ZERO
                     END IF
                  END DO
                  DUM1(K) = 1.0D3*DUM0*RHO_SEDIMENT
               END DO
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (DUM1(K), K=1, 4)
               CALL res_write_check(ios, emsg)
            CASE (31)
               BUFFER(J) = ARBDEP(IEL)
            CASE (32)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (CCCC(IEL, K, ICORS), K=1, top_cell_no)
               CALL res_write_check(ios, emsg)
            CASE (33)
               COLUMN = .TRUE.
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (SSSS(IEL, K, ICORS), K=1, top_cell_no)
               CALL res_write_check(ios, emsg)
            CASE (34)
               BUFFER(J) = CCCC(IEL, top_cell_no, ICORS)
            CASE (35)
               BUFFER(J) = CCCC(IEL, top_cell_no - 1, ICORS)
            CASE (36)
               BUFFER(J) = CCCC(IEL, top_cell_no - 2, ICORS)
            CASE (37)
               BUFFER(J) = CCCC(IEL, NCOLMB(IEL), ICORS)
            CASE (38)
               BUFFER(J) = CCCCW(IEL, ICORS)
            CASE (39:40)
               BUFFER(J) = UNDEF
            CASE (41)
               BUFFER(J) = QVSWEL(IEL)*cellarea(IEL)
            CASE (42)
               COLUMN = .TRUE.
               IW = NVSWLI(IEL)
               IF (IW > 0) THEN
                  DO K = 1, top_cell_no
                     COLBUF(K) = QVSWLI(K, IW)*cellarea(IEL)
                  END DO
               ELSE
                  DO K = 1, top_cell_no
                     COLBUF(K) = 0.0D0
                  END DO
               END IF
               WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (COLBUF(K), K=1, top_cell_no)
               CALL res_write_check(ios, emsg)
            CASE (43)
               BUFFER(J) = WBERR(IEL)
            CASE (44)
               BUFFER(J) = GNUCUM(IEL) - (DLS(IEL) - DLSSRT(IEL))*1000.0D0
            CASE (45:49)
               BUFFER(J) = UNDEF
            CASE (50)
               BUFFER(J) = BALANC(J)*1000.0D0/CAREA
            END SELECT

         END DO

         IF (.NOT. COLUMN) THEN
            WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) RESNOW, (BUFFER(J), J=1, NOUT)
            CALL res_write_check(ios, emsg)
         END IF

         IOTIME(ISET) = RESNOW + IOSTEP(ISET)

      END DO output_loop

      PREVTM = RESNOW
   END SUBROUTINE FRRESP

   ! 14/3/95
   !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!> @brief Sorts active elements for component execution and output ordering.
!>
!> Elements are sorted by descending surface-water elevation, with dry elements
!> sorted by phreatic-surface elevation. For channel links, a dry-link ghost
!> phreatic level is first set to the maximum adjacent non-link phreatic level on
!> the two faces normal to the link direction.
!>
!> | Element state | Temporary list | Stored key |
!> |:--------------|:---------------|:-----------|
!> | Ponded, `GETHRF(IEL)-ZGRUND(IEL) > 1.0E-8` | Column 1 of `ELEV`/`ISTEMP` | `GETHRF(IEL)` |
!> | Dry or non-ponded | Column 2 of `ELEV`/`ISTEMP` | `ZVSPSL(IEL)` |
!> | Dry channel link | Column 2 after ghost update | `MAX(ZVSPSL(adjacent face A), ZVSPSL(adjacent face B))` |
!>
!> Each list is sorted from high to low. When the two lists are merged back into
!> `ISORT`, the implemented comparison uses `ZVSPSL` for the next column-1
!> element and the stored `ELEV(:,2)` key for the next column-2 element; this is
!> the code behaviour, not a fresh comparison against the stored surface-water
!> key.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995-03-14 | - | 3.x | Documented the combined surface-water and phreatic-level ordering. |
!> | 2026-05-03 | SvB | 4.6.1 | Explicitly initialised temporary sorting state for GFortran. |
!> @endhistory
   SUBROUTINE FRSORT

      IMPLICIT NONE

      ! Locals, etc
      DOUBLE PRECISION :: ELEV(NELEE, 2)
      INTEGER :: ISTEMP(NELEE, 2), NSORT(2)
      INTEGER :: NS1, NS2, I, IEL, ITYPE, JEL, IL, L, NDUM, NSTART, NEND, &
                 JUMP, M, K, N, ITEMP, I1, I2, IS
      DOUBLE PRECISION :: HSZ1, HSZ2, ZHIGH, ZLOW, TEMP

      IF (total_no_elements == 1) RETURN

      NS1 = 0
      NS2 = 0

      ! PUT ELEVATIONS INTO LOCAL ARRAYS, DIVIDED INTO SURFACE AND WATER TABLE
      !   ELEMENTS (NB. 'GHOST' PHREATIC SURFACE LEVELS ARE SET UP FOR THE CHANNELS
      !   EQUAL TO THE MAX. PHREATIC ELEVATION OF THE NEIGHBOURING ELEMENTS)
      !
      DO I = 1, total_no_elements

         IEL = ISORT(I)
         ITYPE = ICMREF(IEL, 1)

         IF (ITYPE == 3) THEN
            HSZ1 = zero
            HSZ2 = zero
            IF (LINKNS(IEL)) THEN
               JEL = ICMREF(IEL, 5)
               IF (JEL > 0) HSZ1 = ZVSPSL(JEL)
               JEL = ICMREF(IEL, 7)
               IF (JEL > 0) HSZ2 = ZVSPSL(JEL)
            ELSE
               JEL = ICMREF(IEL, 6)
               IF (JEL > 0) HSZ1 = ZVSPSL(JEL)
               JEL = ICMREF(IEL, 8)
               IF (JEL > 0) HSZ2 = ZVSPSL(JEL)
            END IF
            ZVSPSL(IEL) = MAX(HSZ1, HSZ2)
         END IF

         IL = ICMREF(IEL, 4)
         IF (GETHRF(IEL) - ZGRUND(IEL) > 1.0E-8) THEN
            NS1 = NS1 + 1
            ELEV(NS1, 1) = GETHRF(IEL)
            ISTEMP(NS1, 1) = IEL
         ELSE
            NS2 = NS2 + 1
            ELEV(NS2, 2) = ZVSPSL(IEL)
            ISTEMP(NS2, 2) = IEL
         END IF

      END DO

      NSORT(1) = NS1
      NSORT(2) = NS2

      ! --- SORT ON WATER SURFACE ELEVATIONS, THEN WATER TABLE ELEVATIONS
      !
      column_loop: DO L = 1, 2
         NDUM = NSORT(L)

         ! - CHECK FOR START AND END OF ARRAY TO BE SORTED
         !
         ! PASS ONE (HIGHEST TO LOWEST)
         ! - FIND FIRST POINT (IF ANY) WHERE ELEVATIONS START INCREASING
         NSTART = 0
         DO I = 1, NDUM - 1
            IF (ELEV(I + 1, L) > ELEV(I, L)) THEN
               NSTART = I
               EXIT
            END IF
         END DO

         ! - IF NO INCREASING ELEVATIONS FOUND, THE ARRAY IS ALREADY SORTED
         IF (NSTART == 0) CYCLE column_loop

         ! - FIND HIGHEST POINT IN REST OF ARRAY
         ZHIGH = zero
         DO I = NSTART + 1, NSORT(L)
            IF (ELEV(I, L) > ZHIGH) ZHIGH = ELEV(I, L)
         END DO

         ! - FIND POSITION IN SORTED SECTION OF ARRAY OF ELEVATION 'HIGH'
         DO I = 1, NSTART
            IF (ELEV(I, L) < ZHIGH) THEN
               NSTART = I
               EXIT
            END IF
         END DO

         ! PASS TWO (LOWEST TO HIGHEST)
         ! - FIND FIRST POINT (IF ANY) WHERE ELEVATIONS START DECREASING
         NEND = 0
         DO I = NDUM, 2, -1
            IF (ELEV(I - 1, L) < ELEV(I, L)) THEN
               NEND = I
               EXIT
            END IF
         END DO

         ! - IF NO DECREASING ELEVATIONS FOUND, THE ARRAY IS ALREADY SORTED
         IF (NEND == 0) CYCLE column_loop

         ! - FIND LOWEST POINT IN REST OF ARRAY
         ZLOW = 1.0E10
         DO I = NEND - 1, 1, -1
            IF (ELEV(I, L) < ZLOW) ZLOW = ELEV(I, L)
         END DO

         ! - FIND POSITION IN SORTED SECTION OF ARRAY OF ELEVATION 'ZLOW'
         DO I = NDUM, NEND, -1
            IF (ELEV(I, L) > ZLOW) THEN
               NEND = I
               EXIT
            END IF
         END DO

         ! --- SORT ON ARRAY BETWEEN NSTART AND NEND (Shell Sort)
         JUMP = NEND - NSTART + 1

         gap_loop: DO
            JUMP = JUMP/2
            IF (JUMP == 0) EXIT gap_loop

            DO M = NSTART, NEND - JUMP
               K = M

               inner_sort_loop: DO
                  N = K + JUMP
                  IF (ELEV(K, L) < ELEV(N, L)) THEN
                     ! Swap indices
                     ITEMP = ISTEMP(K, L)
                     ISTEMP(K, L) = ISTEMP(N, L)
                     ISTEMP(N, L) = ITEMP

                     ! Swap elevations
                     TEMP = ELEV(K, L)
                     ELEV(K, L) = ELEV(N, L)
                     ELEV(N, L) = TEMP

                     K = K - JUMP
                     IF (K > 0) CYCLE inner_sort_loop
                  END IF
                  EXIT inner_sort_loop
               END DO inner_sort_loop

            END DO
         END DO gap_loop

         ! --- ARRAY ISTEMP IS SORTED
      END DO column_loop

      ! --- REASSEMBLE ISORT ARRAY
      !
      I1 = 1
      I2 = 1
      IS = 1

      reassemble_loop: DO
         IF (NS1 > 0) THEN
            IF (NS2 == 0 .OR. ZVSPSL(ISTEMP(I1, 1)) > ELEV(I2, 2)) THEN
               ISORT(IS) = ISTEMP(I1, 1)
               I1 = I1 + 1
               IS = IS + 1
            ELSE
               ISORT(IS) = ISTEMP(I2, 2)
               I2 = I2 + 1
               IS = IS + 1
            END IF
         END IF

         IF (I1 > NS1) THEN
            DO I = IS, total_no_elements
               ISORT(I) = ISTEMP(I2, 2)
               I2 = I2 + 1
            END DO
            EXIT reassemble_loop
         END IF

         IF (I2 > NS2) THEN
            DO I = IS, total_no_elements
               ISORT(I) = ISTEMP(I1, 1)
               I1 = I1 + 1
            END DO
            EXIT reassemble_loop
         END IF
      END DO reassemble_loop

      RETURN

      ! FORMAT STATEMENTS
1000  FORMAT(' total_no_elements= ', I4, '  NS1= ', I4, ' NS2= ', I4, ' SFCMAX(*)= ', F7.1, &
             ' sfcmin=', f7.1, ' SZMAX(+)= ', F7.1, ' szmin=', f7.1)
1010  FORMAT(' ', I4, ' ', I4, ' |', A68)

   END SUBROUTINE FRSORT




!> @brief Reads global frame data shared by multiple components.
!>
!> This includes model size, simulation dates, grid spacing, output controls,
!> component execution flags, meteorological/vegetation/soil codes, ground
!> levels, link maps, printing controls, and the optional regular-output interval.
!>
!> | Stage | Main records/actions |
!> |:------|:---------------------|
!> | Run identity and dimensions | Print version/banner, read catchment title, `NX`, `NY`, simulation start/end, and sediment/contaminant start dates. |
!> | Grid and output timing | Read `DXIN`, `DYIN`, `DTAO`, `IAOUT`, `BINFRP`, `BFRTS1`, `BFRTS2`, `BSTORE`, and `PSTART`. |
!> | Timestep controls | Read `PMAX`, `PALFA`, `QMAX`, `TMAX`, and `BSOFT`; cap `TMAX` at 2 h and set `PREST=1+PALFA`. |
!> | Optional printed arrays | Read `BPPNET`, `BPEPOT`, `BPQOC`, `BPDEP`, `BPQF`, `BPQH`, `BPQSZ`, `BPHSZ`, `BPBAL`, and `BPSD` only when `IAOUT=2`. |
!> | Component flags and hot start | Read optional-component flags `BEXSM`, `BEXBK`, `BEXSY`, `BEXCM`, then hot-start controls. |
!> | Codes and geometry | Read station/type counts, discard obsolete river-lining record, read default codes, grid mask, OC link-code layouts, and call [[frind]]. |
!> | Distributed arrays | Read `ZGRUND`; read or default `NMC`, `NRAINC`, and `NVC`; read optional `TOUTPUT`. |
!>
!> Common data read and initialised include:
!>
!> | Data group | Variables |
!> |:-----------|:----------|
!> | Input and echo units | `FRD` and `FID_logfile`, already opened by [[fropen]] |
!> | Job title | run title text |
!> | Model size | `NX`, `NY` |
!> | Simulation start time | `ISYEAR`, `ISMTH`, `ISDAY`, `ISHOUR`, `ISMIN` |
!> | Simulation end time | `IEYEAR`, `IEMTH`, `IEDAY`, `IEHOUR`, `IEMIN` |
!> | Sediment and contaminant start times | `JSYEAR`...`JSMIN`, `JCYEAR`...`JCMIN`; converted to `TSH`/`TCH` only when the component is enabled. |
!> | Node spacing | `DXIN` in x direction, `DYIN` in y direction |
!> | Printing/output control | `DTAO`, `IAOUT`, `BINFRP`, `BFRTS1`, `BFRTS2`, `BSTORE`, `PSTART`, `TOUTPUT` |
!> | Printed-result selection | `BPPNET`, `BPEPOT`, `BPQOC`, `BPDEP`, `BPQF`, `BPQH`, `BPQSZ`, `BPHSZ`, `BPBAL`, `BPSD` |
!> | Component execution control | `BEXSM`, `BEXBK`, `BEXSY`, `BEXCM`; `BEXET`, `BEXUZ`, `BEXOC`, `BEXSZ`, and `BEXEX` are forced true. |
!> | Counts | `NM`, `NRAIN`, `NV`, `NS`; local `NLYRCT` is read and echoed only |
!> | Default met/rain/vegetation codes | `IDMC`, `IDRA`, `IDVE`; `IDLYR` is read but not used here. |
!> | Elevations and geometry | `ZGRUND`, `INGRID`, `LCODEX`, `LCODEY`, `ICMREF` |
!> | Distributed codes | `NMC`, `NRAINC`, `NVC` |
!>
!> The main grid mask is read by row label from top to bottom (`K=NY...1`).
!> Input value `1` is converted to internal catchment value `0`; every other
!> value is converted to `-1`. The row label is checked and a mismatch stops the
!> run immediately.
!>
!> @note
!> The obsolete river-lining record `FR30/FR31` is still consumed from the input
!> stream, but its values are not stored or converted by the active code.
!> @endnote
!>
!> @note
!> `TOUTPUT` is optional. If record `FR52/FR53` is absent or unreadable, the
!> routine uses a 24 h averaging interval.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed common frame input, component flags, grid codes, and output controls. |
!> | 2015-02-13 | SB | - | Added the optional `TOUTPUT` interval for regular text output. |
!> | 2026-03 | SB | 4.6 | Added current hard-coded array-capacity reporting to the print file. |
!> @endhistory
   SUBROUTINE INFR

      IMPLICIT NONE

      INTEGER :: nxplus, isyear, ismth, isday, ishour, ismin, ieyear, iemth, ieday, iehour, iemin, &
                 jsyear, jsmth, jsday, jshour, jsmin, jcyear, jcmth, jcday, jchour, jcmin, j, k, &
                 nlyrct, ipr, idmc, idra, idve, idlyr, i1, i2, i, ipflg, iel, ios
      DOUBLE PRECISION :: tthx
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:INFR' !! Location string for read-error reports.

      WRITE (FID_logfile, 10)
10    FORMAT('1', //T10, '                                E'/T10, &
             ' EUROPEAN HYDROLOGIC SYSTEM  S  H  E  SYSTEME HYDROLOGIQUE EUROPEEN'/T10, &
             '                                S'/)

      ! PRINT THE CURRENT VERSION NUMBER
      IF (BDEVER) THEN
         WRITE (FID_logfile, 16) SHEVER
      ELSE
         WRITE (FID_logfile, 15) SHEVER
      END IF
16    FORMAT(/'SHETRAN VERSION NUMBER: ', F5.1, ' ')
15    FORMAT(/'SHETRAN VERSION NUMBER: ', F5.1)

      WRITE (FID_logfile, 17) BANNER
17    FORMAT(/A80/)

      write (FID_logfile, *)
      write (FID_logfile, *)
      write (FID_logfile, '(A)') ' SHETRAN file folder = '
      write (FID_logfile, '(1X,A)') DIRQQ
      write (FID_logfile, '(A)') ' SHETRAN rundata name = '
      write (FID_logfile, '(A)') ' rundata_'//trim(cnam)//'.txt'
      write (FID_logfile, *)
      write (FID_logfile, *)
      write (FID_logfile, *)

! READ AND PRINT JOB TITLE.
      ! :FR1
      WRITE (FID_logfile, '(A)') 'Catchment Name '
      WRITE (FID_logfile, '(A)') '************** '
      READ (FRD, '(A)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)

      WRITE (FID_logfile, '(A)') TITLE

      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Fixed array sizes in this version of SHETRAN '
      WRITE (FID_logfile, '(A)') '******************************************** '
    WRITE (FID_logfile, '(A)') 'Grid points in x,y directions, river links, total no of elements. THESE ARE THE MOST IMPROTANT ONES'
      WRITE (FID_logfile, '(4(A,I0))') ' NXEE = ', nxee, '  NYEE = ', nyee, '  NLFEE = ', nlfee, '  NELEE = ', nelee
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Grid points in vertical'
      WRITE (FID_logfile, '(1(A,I0))') ' LLEE = ', llee
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Vegetation types, soil typess (NVEE also used for number of precipitation and pet stations)'
      WRITE (FID_logfile, '(2(A,I0))') ' NVEE = ', nvee, '  NSEE = ', nsee
      WRITE (FID_logfile, *)
      WRITE(FID_logfile, '(A)') 'Tables in the VSS component, time varying veg breakpoints, Tables in the ET component (max number of PSI/RCF/FET values, Maximum number of ssoi layers'
      WRITE (FID_logfile, '(4(A,I0))') ' NVSEE = ', NVSEE, '  NVBP = ', NVBP, '  NUZTAB = ', NUZTAB, '  NLYREE = ', NLYREE
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Tables used in OC component, sediment sze fractions'
      WRITE (FID_logfile, '(2(A,I0))') ' NOCTAB = ', NOCTAB, '  NSEDEE = ', NSEDEE
      WRITE (FID_logfile, *)
      WRITE(FID_logfile, '(A)') 'Number of contaminants, number of overlaps, number of plants in an element, total number of plants for contaminants'
      WRITE (FID_logfile, '(4(A,I0))') ' NCONEE = ', NCONEE, '  NOLEE = ', NOLEE, '  NPLTEE = ', NPLTEE, '  NPELEE = ', NPELEE
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Number of snow meltwater slugs, Size of internal tables for channel conveyance'
      WRITE (FID_logfile, '(2(A,I0))') ' max_no_snowmelt_slugs = ', max_no_snowmelt_slugs, '  NXSCEE = ', NXSCEE
      WRITE (FID_logfile, *)

      WRITE (FID_logfile, 20)
20    FORMAT(/' ^^^ ENTER INFR ^^^')

      ! READ AND PRINT MODEL SIZE, TOTAL SIMULATION TIME, GRID SIZES AND
      ! PRINTING CONTROL.
      ! :FR2
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) NX, NY
      CALL errstat_read(ios, location, emsg)
      NXPLUS = 0

      ! :FR4
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN
      CALL errstat_read(ios, location, emsg)

      ! :FR6
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN
      CALL errstat_read(ios, location, emsg)

      ! READ START TIMES FOR SEDIMENT AND CONTAMINANT COMPONENTS
      ! :FR7a
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN
      CALL errstat_read(ios, location, emsg)

      ! :FR7c
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN
      CALL errstat_read(ios, location, emsg)

      NXM1 = NX - 1
      NYM1 = NY - 1
      NXP1 = NX + 1
      NYP1 = NY + 1

      ! :FR8
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(10F7.0)', IOSTAT=ios, IOMSG=emsg) (DXIN(J), J=1, NXM1)
      CALL errstat_read(ios, location, emsg)

      ! :FR10
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(10F7.0)', IOSTAT=ios, IOMSG=emsg) (DYIN(K), K=1, NYM1)
      CALL errstat_read(ios, location, emsg)

      ! :FR12
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(F7.0, I7, 4L7, F7.0)', IOSTAT=ios, IOMSG=emsg) DTAO, IAOUT, BINFRP, BFRTS1, BFRTS2, BSTORE, PSTART
      CALL errstat_read(ios, location, emsg)

      ! :FR20
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(4F7.0,L7)', IOSTAT=ios, IOMSG=emsg) PMAX, PALFA, QMAX, TMAX, BSOFT
      CALL errstat_read(ios, location, emsg)

      ! PMAX = one
      ! PALFA = 0.15D0
      IF (TMAX > 2.0D0) THEN
         WRITE (FID_logfile, *) '^^^ TIMESTEP LIMITED TO 2 HOURS ^^^'
         TMAX = 2.0D0
      END IF

      PREST = (1.0D0 + PALFA)

      IF (IAOUT == 2) THEN
         ! :FR22
         READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
         CALL errstat_read(ios, location, emsg)
         READ (FRD, '(10L7)', IOSTAT=ios, IOMSG=emsg) BPPNET, BPEPOT, BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
         CALL errstat_read(ios, location, emsg)
      END IF

      ! ---- BEX** = TRUE FOR EXECUTION AND FALSE FOR NO EXECUTION
      !      NOTE: COMPONENTS FR,ET,UZ,OC,SZ,EX ARE ALWAYS INCLUDED
      ! :FR24
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(10L7)', IOSTAT=ios, IOMSG=emsg) BEXSM, BEXBK, BEXSY, BEXCM
      CALL errstat_read(ios, location, emsg)
      BEXET = .TRUE.
      BEXUZ = .TRUE.
      BEXOC = .TRUE.
      BEXSZ = .TRUE.
      BEXEX = .TRUE.

      ! LOGICAL PARAMETERS FOR HOT START
      ! :FR26
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(2L7, 2F7.2)', IOSTAT=ios, IOMSG=emsg) BHOTRD, BHOTPR, BHOTTI, BHOTST
      CALL errstat_read(ios, location, emsg)

      ! PRINT INITIALISATION DATA
      WRITE (FID_logfile, 150) NX, NY
150   FORMAT('0'//, ' GRID SPECIFICATION'/80('*')//, ' NX = ', I4, 21X, 'NY = ', I4)
      WRITE (FID_logfile, 160) (DXIN(J), J=1, NXM1)
160   FORMAT('0', 'H-H GRID SIZES (METERS) IN X-DIRECTION', /, (1X, 10G11.4))
      WRITE (FID_logfile, 170) (DYIN(K), K=1, NYM1)
170   FORMAT('0', 'H-H GRID SIZES (METERS) IN Y-DIRECTION', /, (1X, 10G11.4))
      WRITE (FID_logfile, 200)
200   FORMAT(' ', 80('*'))

      ! CONVERT STARTTIME AND ENDTIME TO HOURS.
      TIH = HOUR_FROM_DATE(ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN)
      TTH = HOUR_FROM_DATE(IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN)
      TTHX = TTH - TIH

      WRITE (FID_logfile, 210) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN, IEYEAR, &
         IEMTH, IEDAY, IEHOUR, IEMIN, TTHX
210   FORMAT('0'//, ' START OF SIMULATION  : ', 5I6, /, &
             ' END OF SIMULATION    : ', 5I6, /, &
             ' LENGTH OF SIMULATION : ', F10.2, ' HOURS.')

      ! store start time for mass balance
      mbyear = isyear
      mbmon = ismth
      mbday = isday

      IF (BEXSY) THEN
         TSH = HOUR_FROM_DATE(JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN)
         WRITE (FID_logfile, 211) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN, (TSH - TIH)
211      FORMAT(//' START OF SEDIMENT SIMULATION  : ', 5I6, / &
                 '           AT SIMULATION HOUR  : ', F8.2)
      END IF

      IF (BEXCM) THEN
         TCH = HOUR_FROM_DATE(JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN)
         WRITE (FID_logfile, 212) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN, (TCH - TIH)
212      FORMAT(//' START OF CONTAMINANT SIMULATION  : ', 5I6, / &
                 '               AT SIMULATION HOUR  : ', F8.2)
      END IF

      WRITE (FID_logfile, 215) TMAX
215   FORMAT('0', //, ' BASIC TIMESTEP (HOURS) :', F8.3)

      WRITE (FID_logfile, 220) DTAO
220   FORMAT('0'//, ' PRINTING CONTROL - ALL RESULTS PRINTED AT', &
             ' INTERVALS OF DTAO = ', F7.2, ' HOURS.')

      IF (.NOT. BSTORE) WRITE (FID_logfile, 230)
230   FORMAT('0'//, ' RESULTS NOT REQUIRED ON FILE STORE.')

      IF (BSTORE) WRITE (FID_logfile, 240)
240   FORMAT('0'//, ' RESULTS RECORDED ON FILE STORE.')

      ! READ AND PRINT NM,NRAIN,NV AND NS.
      ! :FR28
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(5I7)', IOSTAT=ios, IOMSG=emsg) NM, NRAIN, NV, NS, NLYRCT
      CALL errstat_read(ios, location, emsg)
      WRITE (FID_logfile, 260) NM, NRAIN, NV, NS, NLYRCT
260   FORMAT('0'//, ' NO. OF METEOROLOGICAL SITES = ', I3, /, &
             ' NO. OF RAINFALL STATIONS = ', I3, /, &
             ' NO. OF VEGETATION TYPES = ', I3, /, &
             ' NO. OF SOIL TYPES = ', I3, /, &
             ' NO. OF SOIL HORIZON CATEGORIES = ', I3)

      ! READ RIVER LINING PARAMETERS.  BLOWP,DB,CCB,BEXTS1
      ! :FR30
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)

      ! SET PRINTING CONTROL FOR SUBROUTINES AREADR AND AREADI.
      IPR = 0
      IF (BINFRP) IPR = 1

      ! READ DEFAULT VALUES FOR MET,RAIN,VEG,SOIL-CODES. APPLIED WHEN > 0
      ! :FR32
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(6I7)', IOSTAT=ios, IOMSG=emsg) IDMC, IDRA, IDVE, IDLYR
      CALL errstat_read(ios, location, emsg)
      WRITE (FID_logfile, 300) IDMC, IDRA, IDVE, IDLYR
300   FORMAT('0', /, ' DEFAULT METEOROLOGICAL STATION CODE =', I3, /, &
             1X, 'DEFAULT RAINFALL STATION CODE       =', I3, /, &
             1X, 'DEFAULT VEGETATION GRID CODE        =', I3, /, &
             1X, 'DEFAULT SOIL HORIZON CATEGORY CODE  =', I3)

      ! READ IN MAIN CATCHMENT DEFINITION ARRAY, INGRID
      ! (NB. THIS IS NOT READ IN USING AREAD ROUTINES, AS THE
      ! INDEX ARRAY ICMREF HASN'T BEEN SET UP YET)
      !
      ! :FR34
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      IF (BINFRP) WRITE (FID_logfile, '( / 20A4)') TITLE

      DO I1 = 1, NY
         K = NY + 1 - I1
         READ (FRD, '(I7, 1X, 500I1)', IOSTAT=ios, IOMSG=emsg) I2, (INGRID(J, K), J=1, NX)
         CALL errstat_read(ios, location, emsg)
         IF (BINFRP) WRITE (FID_logfile, '(I7, 1X, 500I1)') I2, (INGRID(J, K), J=1, NX)

         ! Catchment array definition check
         IF (I2 /= K) THEN
            WRITE (FID_logfile, 314) TITLE, I2
314         FORMAT(//2X, 'ERROR IN DATA ', 20A4, //2X, 'IN THE VICINITY OF ', &
                    'LINE K= ', I5)
            CALL ERR_STOP(255)
         END IF
      END DO

      ! SET INGRID TO BE ITS INTERNAL VALUES FOR SHE (=0 IN CATCHMENT, -1 OTHER)
      DO I = 1, NX
         DO J = 1, NY
            IF (INGRID(I, J) == 1) THEN
               INGRID(I, J) = 0
            ELSE
               INGRID(I, J) = -1
            END IF
         END DO
      END DO

      ! READ THE CODES FOR OVERLAND/CHANNEL FLOW GRID BOUNDARIES
      ! :FR35a
      CALL OCLTL(NXP1, NY, LCODEX, NXE, NYE, FRD, FID_logfile, BINFRP)
      ! :FR35c
      CALL OCLTL(NX, NYP1, LCODEY, NXE, NYE, FRD, FID_logfile, BINFRP)

      ! INITIALISE GLOBAL INDEX ARRAY
      CALL FRIND(BINFRP)

      ! READ / PRINT ARRAYS ZGRUND, NMC, NRAIN, NVC.
      ! SET EQUAL TO DEFAULT VALUES IF THESE ARE TO BE USED.
      !
      ! :FR37
      CALL AREADR(ZGRUND, IPR, FRD, FID_logfile)

      IPFLG = 3
      ! :FR43
      IF (IDMC > 0) CALL AREADI(NMC, IPFLG, IDMC, FID_logfile, NM)
      IF (IDMC <= 0) CALL AREADI(NMC, IPR, FRD, FID_logfile, NM)
      ! :FR46
      IF (IDRA > 0) CALL AREADI(NRAINC, IPFLG, IDRA, FID_logfile, NRAIN)
      IF (IDRA <= 0) CALL AREADI(NRAINC, IPR, FRD, FID_logfile, NRAIN)
      ! :FR49
      IF (IDVE > 0) CALL AREADI(NVC, IPFLG, IDVE, FID_logfile, NV)
      IF (IDVE <= 0) CALL AREADI(NVC, IPR, FRD, FID_logfile, NV)

      ! :FR52
      READ (FRD, '(20A4)', IOSTAT=ios) TITLE
      IF (ios == 0) READ (FRD, *, IOSTAT=ios) TOUTPUT

      ! Check if the optional outputs read successfully
      IF (ios /= 0) TOUTPUT = 24.0D0

      ! INITIALIZATION OF SOME PARAMETERS.
      ALLOUT = DTAO + PSTART
      NXEP1 = NXE + 1
      NYEP1 = NYE + 1

      ! INITIALISATION OF ISORT ARRAY
      DO IEL = 1, total_no_elements
         ISORT(IEL) = IEL
      END DO

      WRITE (FID_logfile, 430)
430   FORMAT('0'//, ' EXIT INFR')

   END SUBROUTINE INFR



!> @brief Supplies ET defaults when the evapotranspiration component is disabled.
!>
!> The routine only writes an `ENTER DINET` message and sets `BMETAL=.TRUE.`.
!> The commented assignments to rainfall, evaporation, interception, root-zone
!> evaporation, drainage, and soil evaporation are inactive. In the current frame
!> initialisation, ET is forced active by [[infr]], so this dummy path is retained
!> mainly for legacy component structure.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1991 | GP | 0.1-0.2 | Added and reduced the legacy dummy-component initialisation set. |
!> | 2026-04 | SvB | 4.6.1 | Retained the inactive compatibility hook during control-flow cleanup. |
!> @endhistory
   SUBROUTINE DINET

      IMPLICIT NONE

      WRITE (*, '(/, /, "ENTER DINET")')
      BMETAL = .TRUE.

      ! PNET=0.0003
      ! PE=0.0
      ! EINT=0.0
      ! ERZ=0.0
      ! DRAIN=0.0
      ! ESOIL=0.0

   END SUBROUTINE DINET

!> @brief Supplies overland/channel defaults when that component is disabled.
!>
!> This routine belongs to the legacy SHETRAN-UK dummy component set (DUM),
!> which contains dummy versions of OC, ET, UZ, SZ, and EXSZOC routines. These
!> minimal dummy components are not currently used. `DINOC` only writes an
!> `ENTER DINOC` message and returns.
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-06 | GP | 0.1 | Added dummy components for use with V-catchment tests. |
!> | 1991-12 | GP | 0.2 | Reduced to minimal versions, not currently used. |
!> @endhistory
   SUBROUTINE DINOC

      IMPLICIT NONE

      WRITE (*, '(/, /, "ENTER DINOC")')

   END SUBROUTINE DINOC

!> @brief Retains the no-op overland/channel input hook required by the legacy component structure.
!>
!> `DOCIN` performs no work and has no side effects.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1991 | GP | 0.1-0.2 | Added the legacy dummy OC component hooks. |
!> | 2026-04-13 | SvB | 4.6.1 | Marked the no-op input hook pure. |
!> @endhistory
   PURE SUBROUTINE DOCIN

      IMPLICIT NONE

   END SUBROUTINE DOCIN


END MODULE FRmod
