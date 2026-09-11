!> summary: The legacy unformatted result and hotstart files.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[FRRESC]] and [[FRRESP]] write the legacy unformatted result file whose
!> metadata — the result-set definitions `IOCORS`, `IODATA`, `IOELEM`,
!> `IOSTA`, `IOSTEP`, `IOEND` and the element classes `ICLIST`/`ICLNUM` — this
!> module also holds. `res_write_check` reports a failed write.
!>
!> **`FRRESC`/`FRRESP` and `res_write_check` are mutually recursive** in two
!> pairs and all three must stay in this module; with `LINK`/`SNL3` in
!> [[cm_channel]] these are the only mutual recursions in the tree.
!>
!> `PREVTM` is [[FRRESP]]'s own state and is here rather than in
!> [[mass_balance_report]], even though [[mass_balance_report:FRMB]] calls
!> `FRRESP`: the other placement would close a cycle.
!>
!> @warning
!> `PREVTM` and `GNUCUM` have no declaration initialisation and no assignment
!> before their first use in [[FRRESP]]. Output id 44 therefore relies on
!> processor/startup state on its first result-output call. This documentation
!> records the current contract; it does not supply an executable default.
!>
!> The legacy binary-result metadata headed by `NSET` has no current producer.
!> [[FRRESC]] has no caller in the current source.
!> @endwarning
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
MODULE legacy_result_files

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, zero, RHO_SEDIMENT
   USE array_limits, ONLY: LLEE, NCLASS, nelee, NSETEE, NXE, NYE
   USE build_info, ONLY: SHEVER
   USE element_geometry, ONLY: BWIDTH, CAREA, cellarea, DHF, DXIN, DXQQ, DYIN, DYQQ, NBFACE, &
                              NXEP1, NXM1, NXP1, NYEP1, NYM1, NYP1, top_cell_no, &
                              total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMRF2, ICMXY, INGRID, NGDBGN, NX, NY
   USE channel_geometry, ONLY: BEXBK, CLENTH, CWIDTH, FHBED, ICMBK, LINKNS, NHBED, ZBEFF, ZBFULL
   USE run_control, ONLY: BEXCM, BEXET, BEXOC, BEXSM, BEXSY, BHOTPR, BHOTRD, BHOTST, BHOTTI
   USE simulation_clock, ONLY: TIH, TTH
   USE file_units, ONLY: BFB, BHB, BKD, BUG, CMB, CMD, CMP, CMT, EPD, ETD, FID_logfile, FRD, &
                         HOT, LFB, LGB, LHB, MED, OCD, OFB, OHB, PPD, PRD, RES, SMD, SPR, SYD, &
                         TIM, VED, VSD, VSI, WLD
   USE met_forcing, ONLY: DTMET, NM, NMC, NRAIN, NRAINC
   USE timestep_control, ONLY: PALFA, PMAX, TMAX
   USE water_balance, ONLY: BALANC
   USE et_state, ONLY: CSTORE, DRAINA, EINTA, EPOT, ERZA, ESOILA, NRD, NV, NVC, PNETTO, RDF, RDL
   USE snow_state, ONLY: MSM, SD, TS
   USE vs_state, ONLY: DELTAZ, JVSACN, JVSDEL, NLYR, NLYRBT, NS, NTSOIL, NVSSPC, NVSSPT, &
                       NVSWLI, NVSWLT, NWELBT, NWELTP, QBKB, QBKF, QH, QVSH, QVSSPR, QVSV, &
                       QVSWEL, QVSWLI, VSPOR, VSPSI, VSTHE, WBERR, ZLYRBT, ZVSNOD, ZVSPSL
   USE vs_soil_tables, ONLY: NVSSOL, VSPDET, VSPDKR, VSPDTH, VSPETA, VSPKR, VSPPSI, VSPTHE
   USE oc_state, ONLY: LCODEX, LCODEY, QMAX, QOC
   USE oc_node_solver, ONLY: gethrf
   USE sy_state, ONLY: ARBDEP, DCBED, DCBSED, DLS, FBETA, FDEL, GINFD, GINFS, GNU, GNUBK, &
                       NSED, PLS, QSED
   USE cm_parameters, ONLY: CCCC, CCCCW, SSSS
   USE cm_column_geometry, ONLY: NCOLMB
   USE float_compare, ONLY: notzero
   USE error_reporting, ONLY: ERRLVL_error, ERRLVL_fatal, ERRLVL_warn
   USE error_status, ONLY: errstat_fileclose, errstat_fileopen, errstat_write

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FRRESP
   PUBLIC :: ALLOUT, BSTORE, BTIME, DTAO, IAOUT, PSTART, RESFIL

   INTEGER :: NSET       !! Number of legacy binary result sets; no current producer was found.
   DOUBLEPRECISION :: PSTART  !! Simulation-relative start time for legacy printed/result output (h).
   INTEGER :: IOCORS(NSETEE)      !! Contaminant/sediment selector for each legacy result set.
   INTEGER :: IODATA(NSETEE)      !! Data-type number for each legacy result set.
   INTEGER :: IOELEM(NSETEE)      !! Positive element number or negative element-class number by legacy result set.
   INTEGER :: IORES(NSETEE)       !! Open unformatted output unit by legacy result set.
   INTEGER :: ICLIST(NELEE,NCLASS) !! Element numbers belonging to each legacy output class.
   INTEGER :: ICLNUM(NCLASS)      !! Number of elements in each legacy output class.
   DOUBLEPRECISION :: IOSTA(NSETEE)  !! Start time for each legacy result set (h).
   DOUBLEPRECISION :: IOSTEP(NSETEE) !! Output interval for each legacy result set (h).
   DOUBLEPRECISION :: IOEND(NSETEE)  !! End time for each legacy result set (h).
   DOUBLEPRECISION :: IOTIME(NSETEE) !! Next output time for each legacy result set (h).
   CHARACTER(len=200) :: RESFIL !! Path used as the stem for legacy unformatted result files.
   INTEGER :: IAOUT !! Legacy frame-output selector read from the FR data file.
   DOUBLEPRECISION :: ALLOUT !! Next accumulated legacy output-control time (h).
   DOUBLEPRECISION :: DTAO   !! Legacy output interval (h).
   LOGICAL :: BTIME  !! Enable time-series result processing.
   LOGICAL :: BSTORE !! Enable the legacy result-output method.
   DOUBLEPRECISION :: PREVTM            !! Previous [[frresp]] call time (h); undefined before the first call.
   LOGICAL         :: SEDSRT = .FALSE.    !! True after sediment sorting state has been initialised.
   DOUBLEPRECISION :: GNUCUM(NELEE)     !! Cumulative erosion-depth workspace (mm); initially undefined.
   DOUBLEPRECISION :: DLSSRT(NELEE)     !! Loose-sediment-depth baseline captured by [[frresp]] (mm).

CONTAINS

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

END MODULE legacy_result_files

