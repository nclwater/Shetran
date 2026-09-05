!> @brief Exposes native SHETRAN state to the visualisation interface.
!>
!> This is the solver-facing boundary of the visualisation subsystem. It is the
!> only visualisation module that directly imports the core hydrology,
!> topology, sediment, and contaminant modules. Most public procedures are
!> small default-real or integer accessors used by
!> [[visualisation_interface_centre]]; [[get_nsed_early]] and
!> [[get_ncon_early]] are called directly by `run_sim` before visualisation
!> metadata is initialized. The module also re-exports the run/output paths and
!> native face constants needed farther to the right.
!>
!> Values use solver indexing at this boundary:
!>
!> | Item | Native convention |
!> |:-----|:------------------|
!> | Element | Positive SHETRAN element number; zero commonly denotes no element. |
!> | Element type | 0 gridsquare, 1 or 2 explicit bank, 3 channel link. |
!> | Face | 1 east, 2 north, 3 west, 4 south. |
!> | Cell layer | Bottom-up solver cell; the centre reverses visualisation order. |
!> | Grid | `i` increases eastward and `j` uses the native SHETRAN orientation; the right interface reverses displayed rows. |
!> | Missing lookup | `i_not_exist=-1` or `r_not_exist=-1.0` where an accessor explicitly supports absence. |
!>
!> `ICMREF` supplies topology: column 1 is the element type, columns 5:8
!> contain neighbours on native faces 1:4, and columns 9:12 contain the
!> reciprocal face in each neighbour. [[bank_no]] and [[river_no]] traverse
!> those columns from a gridsquare. [[element]] maps native grid coordinates
!> through `ICMXY`; [[grid_dx]] and [[grid_dy]] derive cell widths from
!> internode spacings.
!>
!> Output-unit conversions performed here are:
!>
!> | Conversion | Factor | Accessors |
!> |:-----------|:-------|:----------|
!> | m/s to mm/hour | `1000 * 3600` | [[net_rain]], [[pot_evap]], [[trnsp]], [[srf_evap]], [[int_evap]], [[drainage]] |
!> | m/s to mm/day | `1000 * 24 * 3600` | [[s_v_er]] |
!> | m to mm | `1000` | [[s_t_dp]] |
!> | solid-sediment m3/s to kg/s | `RHOSED` | [[s_dis]] |
!>
!> Other accessors preserve the core value and units, although assignment to a
!> default-real function result narrows double-precision model state. In
!> particular, [[snow_dep]] returns millimetres, flow accessors preserve their
!> core signs, and [[version]] intentionally truncates the legacy numeric
!> `SHEVER=4.6` to major version 4.
!>
!> Public services fall into these groups:
!>
!> | Group | Principal exports |
!> |:------|:------------------|
!> | Topology and geometry | `ADJACENT_ELEMENT`, `ETYPE`, `EXISTS`, `IS_*`, `BANK_NO`, `RIVER_NO`, widths and elevations |
!> | Grid and dimensions | `ELEMENT`, `GRID_DX`, `GRID_DY`, `GRID_NX`, `GRID_NY`, `NO_EL`, `TOP_CELL` |
!> | Water state | rainfall/evaporation accessors, storage, depths, `PSI`, `THETA`, `V_FLOW`, `OVR_FLOW`, `BAL_ERR` |
!> | Sediment and contaminants | `NO_SED`, `NO_CON`, `S_T_DP`, `S_V_ER`, `S_DIS`, `C_C_DR`, `C_C_DS` |
!> | Startup and run metadata | `GET_NSED_EARLY`, `GET_NCON_EARLY`, `VERSION`, directories and output filenames |
!>
!> Add a new raw-state accessor here when a catalogue entry in
!> [[visualisation_interface_centre]] cannot be composed from the existing
!> contract. Keep the module default `PRIVATE`, explicitly export only required
!> services, and update the centre's dispatch and metadata together. The private
!> [[adjacent_face]], [[dxx]], and [[dyy]] helpers are not part of that contract;
!> `adjacent_face` currently has no caller.
!>
!> @warning
!> Accessors generally assume valid, initialized indices. [[exists]] tests only
!> `i>0`, not the upper bound; [[is_square]] consequently classifies the missing
!> value zero as a gridsquare. `bank_width` leaves its result undefined for a
!> non-positive bank number. Sediment and contaminant accessors do not inspect
!> `BEXSY` or `BEXCM`; their component state and counts must already exist.
!> `snow_dep` is correctly returned in millimetres here, while the current
!> centre catalogue and `docs/format_hdf5.md` still label it as metres.
!> @endwarning
!>
!> The former perturbation accessors `spatial1` and `space_time1` are not
!> present in the current source tree. The centre catalogue still registers
!> `spatial1`, whose unsupported real dispatch therefore returns its `HUGE`
!> sentinel.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Created the near-SHETRAN interface for SHEGRAPH 2. |
!> | 2004-11-22 | JE | - | Made the interface common to SHETRAN 3 and 4 using compiler-selected state access. |
!> | 2019-11-28 | SB | - | Imported the standard interface into the current repository history. |
!> | 2026-04-04 | SvB | - | Applied the project-wide Fortran formatting pass. |
!> | 2026-04-06 | SvB | - | Rewrote the early sediment and contaminant readers with `IOSTAT` control flow. |
!> | 2026-04-08 | SB | 4.6.1 | Removed Intel directives and SHETRAN 3 paths for IFX. |
!> @endhistory
MODULE visualisation_interface_left

   USE SGLOBAL, ONLY    : dxqq, dyqq, zgrund, total_no_elements, top_cell_no, nlf=>total_no_links
   USE AL_C, ONLY       : cmd, draina, cwidth, nlyr, nlyrbt, ntsoil, nvc, pnetto, qoc, syd, wberr
   USE AL_C, ONLY       : deltaz, esoila, qvsv, vspsi, vsthe, zvspsl
   USE AL_D, ONLY       : bexcm, bexsy, cstore, dxin, dyin, einta, epot, erza, sd
   USE AL_G, ONLY       : icmref, icmxy, nx, ny
   USE SGLOBAL, ONLY       : DIRQQ, shever, ROOTDIR, hdf5filename, uznow, &
      planfile=>visualisation_plan_filename, &
      checkfile=>visualisation_check_filename
   USE CONT_CC, ONLY    : cccc, nnncon=>ncon, ssss
   USE CONST_SY, ONLY   : rhosed
   USE SED_CS, ONLY     : dls, gnu, nnnsed=>nsed, qsed
   USE OCmod2, ONLY     : hrfzz

   USE MOD_PARAMETERS, ONLY : I_P
   USE MOD_ERROR, ONLY : err_check_allocatememorystatus, RAISE_ERROR, ERRLVL_fatal, FID_logfile

   IMPLICIT NONE
   INTEGER, PARAMETER :: east=1          !! Native SHETRAN east-face number.
   INTEGER, PARAMETER :: north=2         !! Native SHETRAN north-face number.
   INTEGER, PARAMETER :: west=3          !! Native SHETRAN west-face number.
   INTEGER, PARAMETER :: south=4         !! Native SHETRAN south-face number.
   INTEGER, PARAMETER :: i_not_exist=-1  !! Private missing integer returned by topology accessors.
   REAL, PARAMETER    :: zero=0.0        !! Retained legacy default-real zero; unused by current routines.
   REAL, PARAMETER    :: half=0.5        !! Retained legacy half factor; current width helpers use literal `0.5`.
   REAL, PARAMETER    :: r_not_exist=-1.0 !! Private missing real returned by selected geometry accessors.
   REAL, PARAMETER    :: m_to_mm     = 1000.0             !! Metres-to-millimetres factor.
   REAL, PARAMETER    :: ps_to_ph    = 3600.0             !! Per-second to per-hour factor.
   REAL, PARAMETER    :: ps_to_pd    = 24.0 * ps_to_ph    !! Per-second to per-day factor.
   REAL, PARAMETER    :: mps_to_mmph = m_to_mm * ps_to_ph !! Metres/second to millimetres/hour factor.
   REAL, PARAMETER    :: mps_to_mmpd = m_to_mm * ps_to_pd !! Metres/second to millimetres/day factor.
   PRIVATE
   PUBLIC :: BAL_ERR,         BANK_NO,       BANK_WIDTH,                                &
      CAN_STOR,        C_C_DR,        C_C_DS,     CELL_THICKNESS,                &
      DRAINAGE,                                                                  &
      ELEMENT,                        ELEMENT_DX, ELEMENT_DY, EXISTS,            &
      GET_NCON_EARLY, GET_NSED_EARLY, GRID_DX,    GRID_DY,    GRID_NX, GRID_NY,  &
      INT_EVAP,       IS_BANK,        IS_LINK,    IS_SQUARE,                     &
      NET_RAIN,       NO_EL,          NO_CON,     NO_SED,                        &
      OVR_FLOW,                                                                  &
      PH_DEPTH,       POT_EVAP,       PSI,                                       &
      RIVER_NO,       RIVER_WIDTH,                                               &
      S_DIS,          S_ELEVATION,    SNOW_DEP,   SOIL_TYPE,  SRF_DEP,           &
      SRF_EVAP,       S_T_DP,         S_V_ER,                                    &
      THETA,          TOP_CELL,       TRNSP,                                     &
      VERSION,        V_FLOW
   PUBLIC :: DIRQQ, ROOTDIR, north, east, south, west, hdf5filename, planfile, checkfile, etype, ADJACENT_ELEMENT

CONTAINS

!> @brief Returns the element adjoining one native face of an element.
!>
!> This is a direct lookup in columns 5:8 of `ICMREF`. A result of zero denotes
!> a catchment boundary; a negative result may denote a confluence record.
!>
!> Returns adjacent element reference, including any boundary or confluence sentinel.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the native-topology accessor for SHEGRAPH 2. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION adjacent_element(iel, face) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      r = ICMREF(iel, face + 4)
   END FUNCTION adjacent_element

!> @brief Returns the reciprocal face recorded for an adjoining element.
!>
!> This private helper reads columns 9:12 of `ICMREF`. It performs no index or
!> boundary validation and is currently retained without a caller.
!>
!> Returns face number in the adjacent element's frame of reference.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the reciprocal-face topology helper. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION adjacent_face(iel, face) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      r = ICMREF(iel, face + 8)
   END FUNCTION adjacent_face

!> @brief Returns an element's cumulative water-balance residual in metres.
!>
!> The double-precision `WBERR` value is narrowed to the default-real result.
!>
!> Returns cumulative element water-balance residual (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the water-balance error accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION bal_err(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = wberr(iel)
   END FUNCTION bal_err

!> @brief Finds the explicit bank element beside a gridsquare face.
!>
!> The starting element must be a valid positive gridsquare number. A face
!> without an explicit type-1 or type-2 bank returns `i_not_exist`.
!>
!> Returns bank element number, or -1 when the lookup does not find a bank.
!>
!> @warning `is_square(0)` is true, so zero is not a safe value for `su`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added bank lookup from a gridsquare face. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION bank_no(su, face) RESULT(r)
      INTEGER, INTENT(IN) :: su   !! Gridsquare element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      INTEGER             :: adj  !! Element adjoining the gridsquare face.
      IF(.NOT.IS_SQUARE(su)) THEN
         r = i_not_exist
      ELSE
         adj = ADJACENT_ELEMENT(su,face)
         IF(IS_BANK(adj)) THEN
            r=adj
         ELSE
            r = i_not_exist
         ENDIF
      ENDIF
   END FUNCTION bank_no

!> @brief Returns the plan width of an explicit bank element in metres.
!>
!> East and west faces select the element's x extent; north and south faces
!> select its y extent. The routine treats every face other than east or west
!> as north/south and does not validate the face number.
!>
!> Returns bank width (m).
!>
!> @warning The result is undefined when `bk` is not positive.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the bank-width geometry accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION bank_width(bk, face) RESULT(r)
      INTEGER, INTENT(IN) :: bk   !! Bank element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      IF(EXISTS(bk)) THEN
         IF(ANY(face==(/east,west/))) THEN
            r = dxqq(bk)
         ELSE
            r = dyqq(bk)
         ENDIF
      ENDIF
   END FUNCTION bank_width

!> @brief Returns canopy interception storage in millimetres.
!>
!> Returns canopy storage `CSTORE` (mm).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the canopy-storage accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION can_stor(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = cstore(iel)
   END FUNCTION can_stor
!> @brief Returns dissolved contaminant concentration for one subsurface cell.
!>
!> `CCCC` stores relative, dimensionless concentration. The contaminant
!> component must be active and its arrays initialized before this lookup.
!>
!> Returns dissolved relative concentration (dimensionless).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the dissolved-contaminant accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION c_c_dr(iel, ilay, ncon) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Bottom-up subsurface cell number.
      INTEGER, INTENT(IN) :: ncon !! Contaminant group number.
      r = cccc(iel, ilay, ncon)
   END FUNCTION c_c_dr

!> @brief Returns sorbed contaminant concentration for one subsurface cell.
!>
!> `SSSS` stores relative, dimensionless concentration. The contaminant
!> component must be active and its arrays initialized before this lookup.
!>
!> Returns sorbed relative concentration (dimensionless).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the sorbed-contaminant accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION c_c_ds(iel, ilay, ncon) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Bottom-up subsurface cell number.
      INTEGER, INTENT(IN) :: ncon !! Contaminant group number.
      r = ssss(iel, ilay, ncon)
   END FUNCTION c_c_ds

!> @brief Returns the thickness of one subsurface cell in metres.
!>
!> A non-positive element returns `r_not_exist`; positive element and layer
!> numbers are used directly without upper-bound checks.
!>
!> Returns cell thickness (m), or -1.0 for a non-positive element number.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the cell-thickness accessor. |
!> | 2004-11-22 | JE | - | Made state access common to SHETRAN 3 and 4. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the obsolete SHETRAN 3 access branch. |
!> @endhistory
   ELEMENTAL REAL FUNCTION cell_thickness(iel, j) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      INTEGER, INTENT(IN) :: j   !! Bottom-up subsurface cell number.
      IF(EXISTS(iel)) THEN
         r = DELTAZ(j,iel)
      ELSE
         r=r_not_exist
      ENDIF
   END FUNCTION cell_thickness

!> @brief Returns element drainage in millimetres per hour.
!>
!> Converts `DRAINA` from m/s using `mps_to_mmph`.
!>
!> Returns drainage rate (mm/hour).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the drainage-rate accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION drainage(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*draina(iel)
   END FUNCTION drainage
!> @brief Derives one native grid-column width in metres.
!>
!> This private helper uses `DXIN(1)` at the first column, `DXIN(NX-1)` at the
!> last, and the mean of the two surrounding internode spacings elsewhere.
!> `i` must lie in 1:`NX`; a single-column grid still accesses `DXIN(1)`.
!>
!> Returns east-west grid-cell width (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the east-west grid-width helper. |
!> @endhistory
   ELEMENTAL REAL FUNCTION dxx(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Native grid-column index.
      IF(i==1) THEN
         r = dxin(1)
      ELSEIF(i==nx) THEN
         r = dxin(nx-1)
      ELSE
         r = (dxin(i-1) + dxin(i)) * 0.5
      ENDIF
   END FUNCTION dxx

!> @brief Derives one native grid-row height in metres.
!>
!> This private helper uses `DYIN(1)` at the first row, `DYIN(NY-1)` at the
!> last, and the mean of the two surrounding internode spacings elsewhere.
!> `i` must lie in 1:`NY`; a single-row grid still accesses `DYIN(1)`.
!>
!> Returns north-south grid-cell height (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the north-south grid-height helper. |
!> @endhistory
   ELEMENTAL REAL FUNCTION dyy(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Native grid-row index.
      IF(i==1) THEN
         r = dyin(1)
      ELSEIF(i==ny) THEN
         r = dyin(ny-1)
      ELSE
         r = (dyin(i-1) + dyin(i)) * 0.5
      ENDIF
   END FUNCTION dyy

!> @brief Maps native grid coordinates to an active element number.
!>
!> The coordinates are used directly in `ICMXY` without bounds checks.
!>
!> Returns element reference stored at the grid location.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the grid-to-element lookup. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION element(i,j) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Native grid-column index.
      INTEGER, INTENT(IN) :: j !! Native grid-row index.
      r = icmxy(i,j)
   END FUNCTION element

!> @brief Returns an element's east-west plan extent in metres.
!>
!> Returns element x extent `DXQQ` (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the element x-extent accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION element_dx(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = dxqq(iel)
   END FUNCTION element_dx

!> @brief Returns an element's north-south plan extent in metres.
!>
!> Returns element y extent `DYQQ` (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the element y-extent accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION element_dy(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = dyqq(iel)
   END FUNCTION element_dy

!> @brief Returns the native type code for an element reference.
!>
!> Type 0 is a gridsquare, types 1 and 2 are explicit banks, and type 3 is a
!> channel link. The special element reference zero also returns type 0.
!>
!> Returns native element type code.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the native element-type accessor. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION etype(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element reference.
      IF(iel/=0) THEN
         r = icmref(iel, 1)
      ELSE
         r = 0
      ENDIF
   END FUNCTION etype

!> @brief Tests whether an integer is a positive element reference.
!>
!> This is a sentinel test only; it does not compare the value with
!> `total_no_elements`.
!>
!> Returns true exactly when `i>0`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the positive-reference test. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Candidate element reference.
      r = i>0
   END FUNCTION exists

!> @brief Reads the contaminant-group count before normal contaminant setup.
!>
!> Starting at the current position of unit `CMD`, the routine scans fixed
!> four-character records until characters 2:4 are `CM3`, then reads `NCON`
!> from the following record into the contaminant module's count. A successful
!> scan rewinds `CMD` for the normal input reader.
!>
!> On end-of-file, scan error, or failure to read the count, the routine calls
!> `ERROR` with fatal severity and returns if control is handed back. That
!> failure path does not rewind the unit.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the early contaminant-count scan for visualisation setup. |
!> | 2026-04-06 | SvB | - | Replaced branch labels with `IOSTAT`-controlled scanning and reads. |
!> @endhistory
   SUBROUTINE get_ncon_early()
      IMPLICIT NONE

      CHARACTER(4)  :: dd   !! Current fixed-length tag record.
      CHARACTER(64) :: mess !! Error message passed to `ERROR`.
      INTEGER       :: ios  !! Input/output status from the current read.

      scan_loop: DO
         READ(cmd, '(A)', IOSTAT=ios) dd

         IF (ios /= 0) THEN
            mess = 'failed to find line :CM3 in contaminant data file'
            mess = 'GET_NCON_EARLY ' // TRIM(mess)
            CALL RAISE_ERROR(ERRLVL_fatal, 1, FID_logfile, 0, 0, mess)
            RETURN
         END IF

         IF (dd(2:4) == 'CM3') THEN
            READ(cmd, *, IOSTAT=ios) nnncon

            IF (ios /= 0) THEN
               mess = 'failed to read NCON '
               mess = 'GET_NCON_EARLY ' // TRIM(mess)
               CALL RAISE_ERROR(ERRLVL_fatal, 1, FID_logfile, 0, 0, mess)
               RETURN
            END IF

            EXIT scan_loop
         END IF

      END DO scan_loop

      REWIND(cmd)

   END SUBROUTINE get_ncon_early

!> @brief Reads the sediment-fraction count before normal sediment setup.
!>
!> Starting at the current position of unit `SYD`, the routine scans fixed
!> five-character records until characters 2:5 are `SY11`, then reads `NSED`
!> from the following record into the sediment module's count. A successful
!> scan rewinds `SYD` for the normal input reader.
!>
!> On end-of-file, scan error, or failure to read the count, the routine calls
!> `ERROR` with fatal severity and returns if control is handed back. That
!> failure path does not rewind the unit.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the early sediment-count scan for visualisation setup. |
!> | 2026-04-06 | SvB | - | Replaced branch labels with `IOSTAT`-controlled scanning and reads. |
!> @endhistory
   SUBROUTINE get_nsed_early()
      IMPLICIT NONE

      CHARACTER(5)  :: dd   !! Current fixed-length tag record.
      CHARACTER(64) :: mess !! Error message passed to `ERROR`.
      INTEGER       :: ios  !! Input/output status from the current read.

      scan_loop: DO
         READ(syd, '(A)', IOSTAT=ios) dd

         IF (ios /= 0) THEN
            mess = 'failed to find line :SY11 in sediment data file'
            mess = 'GET_NSED_EARLY ' // TRIM(mess)
            CALL RAISE_ERROR(ERRLVL_fatal, 1, FID_logfile, 0, 0, mess)
            RETURN
         END IF

         IF (dd(2:5) == 'SY11') THEN
            READ(syd, *, IOSTAT=ios) nnnsed

            IF (ios /= 0) THEN
               mess = 'failed to read NSED '
               mess = 'GET_NSED_EARLY ' // TRIM(mess)
               CALL RAISE_ERROR(ERRLVL_fatal, 1, FID_logfile, 0, 0, mess)
               RETURN
            END IF

            EXIT scan_loop
         END IF

      END DO scan_loop

      REWIND(syd)

   END SUBROUTINE get_nsed_early

!> @brief Returns a native grid-column width in metres.
!>
!> Returns east-west grid-cell width from the private [[dxx]] helper (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the public east-west grid-width accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION grid_dx(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Native grid-column index.
      r = DXX(iel)
   END FUNCTION grid_dx

!> @brief Returns a native grid-row height in metres.
!>
!> Returns north-south grid-cell height from the private [[dyy]] helper (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the public north-south grid-height accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION grid_dy(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Native grid-row index.
      r = DYY(iel)
   END FUNCTION grid_dy

!> @brief Returns the number of native grid columns.
!>
!> Returns `NX`, the x-direction grid count.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the native x-grid count accessor. |
!> @endhistory
   PURE INTEGER FUNCTION grid_nx() RESULT(r)
      r = nx
   END FUNCTION grid_nx

!> @brief Returns the number of native grid rows.
!>
!> Returns `NY`, the y-direction grid count.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the native y-grid count accessor. |
!> @endhistory
   PURE INTEGER FUNCTION grid_ny() RESULT(r)
      r = ny
   END FUNCTION grid_ny

!> @brief Returns intercepted-water evaporation in millimetres per hour.
!>
!> Converts `EINTA` from m/s using `mps_to_mmph`.
!>
!> Returns interception evaporation rate (mm/hour).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the interception-evaporation accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION int_evap(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*einta(iel)
   END FUNCTION int_evap

!> @brief Tests whether an element has native bank type 1 or 2.
!>
!> Returns true for an explicit bank element.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the bank-type predicate. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION is_bank(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element reference.
      INTEGER             :: typ !! Native element type.
      typ = ETYPE(iel)
      r   = typ==1 .OR. typ==2
   END FUNCTION is_bank

!> @brief Tests whether an element has native channel-link type 3.
!>
!> Returns true for a channel-link element.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the channel-link predicate. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION is_link(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element reference.
      INTEGER             :: typ !! Native element type.
      typ = ETYPE(iel)
      r   = typ==3
   END FUNCTION is_link

!> @brief Tests whether an element reference resolves to native type 0.
!>
!> Returns true for a gridsquare and also for the missing reference zero.
!>
!> @warning Because [[etype]] maps zero to type 0, this is not an existence test.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the gridsquare-type predicate. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION is_square(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element reference.
      INTEGER             :: typ !! Native element type.
      typ = ETYPE(iel)
      r   = typ==0
   END FUNCTION is_square

!> @brief Returns net surface water input in millimetres per hour.
!>
!> Converts `PNETTO` from m/s using `mps_to_mmph`. Despite the legacy name,
!> `PNETTO` can include mapped well-irrigation input as well as net rainfall.
!>
!> Returns net surface input rate (mm/hour).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the net-rainfall visualisation accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION net_rain(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*pnetto(iel)
   END FUNCTION net_rain

!> @brief Returns the configured number of contaminant groups.
!>
!> The contaminant component or [[get_ncon_early]] must have initialized the
!> imported module count before this query.
!>
!> Returns number of contaminant groups.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the contaminant-group count accessor. |
!> @endhistory
   PURE INTEGER FUNCTION no_con() RESULT(r)
      r = nnncon
   END FUNCTION no_con

!> @brief Returns the total number of active SHETRAN elements.
!>
!> Returns dynamic element count `total_no_elements`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the active-element count accessor. |
!> @endhistory
   INTEGER FUNCTION no_el() RESULT(r)
      r = total_no_elements
   END FUNCTION no_el

!> @brief Returns the configured number of sediment size fractions.
!>
!> The sediment component or [[get_nsed_early]] must have initialized the
!> imported module count before this query.
!>
!> Returns number of sediment size fractions.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the sediment-fraction count accessor. |
!> @endhistory
   PURE INTEGER FUNCTION no_sed() RESULT(r)
      r = nnnsed
   END FUNCTION no_sed

!> @brief Returns overland-flow discharge through one native element face.
!>
!> The accessor preserves the core `QOC` sign convention and narrows the
!> double-precision value to default real.
!>
!> Returns signed overland discharge (m3/s).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the face-based overland-flow accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION ovr_flow(iel, face) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      r = qoc(iel,face)
   END FUNCTION ovr_flow

!> @brief Returns phreatic-surface depth below ground in metres.
!>
!> Computes `ZGRUND-ZVSPSL`, so a positive value is below ground and a
!> negative value represents a phreatic surface above ground.
!>
!> Returns signed phreatic-surface depth relative to ground (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the phreatic-depth accessor. |
!> | 2004-11-22 | JE | - | Made state access common to SHETRAN 3 and 4. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the obsolete SHETRAN 3 access branch. |
!> @endhistory
   ELEMENTAL REAL FUNCTION ph_depth(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = zgrund(iel)-zvspsl(iel)
   END FUNCTION ph_depth

!> @brief Returns potential evaporation in millimetres per hour.
!>
!> Converts `EPOT` from m/s using `mps_to_mmph`.
!>
!> Returns potential evaporation rate (mm/hour).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the potential-evaporation accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION pot_evap(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*epot(iel)
   END FUNCTION pot_evap

!> @brief Returns matric pressure head for one subsurface cell in metres.
!>
!> The current SHETRAN 4 path reads `VSPSI(ilay,iel)` directly and narrows it
!> to default real. The preceding assignment of `r_not_exist` is immediately
!> overwritten, so it does not provide missing-value protection.
!>
!> Returns matric pressure head (m).
!>
!> @warning Both indices must be valid and the variably saturated state initialized.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the subsurface pressure-head accessor. |
!> | 2004-11-22 | JE | - | Made state access common to SHETRAN 3 and 4. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the obsolete SHETRAN 3 access branch. |
!> @endhistory
   ELEMENTAL REAL FUNCTION psi(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Bottom-up subsurface cell number.
      r = r_not_exist
      r = vspsi(ilay,iel)
   END FUNCTION psi

!> @brief Finds the channel link adjoining a gridsquare face.
!>
!> A directly adjacent link is returned immediately. If an explicit bank lies
!> on the face, the lookup crosses that bank on the same native face to obtain
!> the channel link. Other topology returns `i_not_exist`.
!>
!> Returns channel-link element number, or -1 when no link is found.
!>
!> @warning `is_square(0)` is true, so zero is not a safe value for `su`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added channel-link lookup from a gridsquare face. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION river_no(su, face) RESULT(r)
      INTEGER, INTENT(IN) :: su   !! Gridsquare element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      INTEGER             :: adj  !! Element adjoining the current face.
      IF(.NOT.IS_SQUARE(su)) THEN
         r = i_not_exist
      ELSE
         adj = ADJACENT_ELEMENT(su,face)
         IF(IS_LINK(adj)) THEN
            r = adj
         ELSEIF(IS_BANK(adj)) THEN
            r = ADJACENT_ELEMENT(adj,face)
         ELSE
            r = i_not_exist
         ENDIF
      ENDIF
   END FUNCTION river_no

!> @brief Returns a channel link's width in metres.
!>
!> Returns `CWIDTH(ir)` (m), or -1.0 for a non-positive link number.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the channel-width accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION river_width(ir) RESULT(r)
      INTEGER, INTENT(IN) :: ir !! Channel-link element number.
      IF(EXISTS(ir)) THEN
         r = cwidth(ir)
      ELSE
         r = i_not_exist
      ENDIF
   END FUNCTION river_width

!> @brief Returns solid-sediment mass discharge through an element face.
!>
!> Multiplies the signed solid-sediment volume discharge `QSED` (m3/s) by the
!> sediment density `RHOSED` (kg/m3). The sediment component must be active and
!> initialized; the core face-flow sign is preserved.
!>
!> Returns signed solid-sediment mass discharge (kg/s).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the sediment-discharge accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION s_dis(iel, face, nsed) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: face !! Native face number.
      INTEGER, INTENT(IN) :: nsed !! Sediment size-fraction number.
      r = rhosed*qsed(iel, nsed,face)
   END FUNCTION s_dis

!> @brief Returns an element's ground-surface elevation in metres.
!>
!> Returns ground elevation `ZGRUND` (m), or -1.0 for a non-positive element.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the surface-elevation accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION s_elevation(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      IF(iel>0) THEN
         r = ZGRUND(iel)
      ELSE
         r = r_not_exist
      ENDIF
   END FUNCTION s_elevation

!> @brief Returns snow depth in millimetres.
!>
!> The value is taken directly from `SD`; no conversion is required.
!>
!> Returns snow depth (mm).
!>
!> @warning The current centre catalogue and HDF5 format document this value as metres.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the snow-depth accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION snow_dep(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = sd(iel)
   END FUNCTION snow_dep

!> @brief Maps a subsurface cell to its configured soil type.
!>
!> Channel links and profiles with no nonzero `NLYRBT` entries return zero.
!> Otherwise, the routine advances through the soil-layer lower-bound table
!> until the next boundary exceeds the bottom-up cell number, then returns the
!> corresponding `NTSOIL` entry. Normal initialization supplies a sentinel
!> boundary at `NLYRBT(iel,NLYR+1)=top_cell_no+1`.
!>
!> Returns soil type number, or zero for a link or uninitialized profile.
!>
!> @warning The indices and layer-boundary sentinel are not validated here.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the cell-to-soil-type lookup. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION soil_type(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Bottom-up subsurface cell number.
      INTEGER             :: j    !! Current soil-layer number.
      IF(IS_LINK(iel)) THEN
         r = 0
      ELSEIF(ANY(nlyrbt(iel,:)/=0)) THEN
         j = 1
         DO WHILE (ilay >= nlyrbt(iel,j+1))
            j = j + 1
         ENDDO
         r = ntsoil(iel,j)
      ELSE
         r = 0
      ENDIF
   END FUNCTION soil_type

!> @brief Returns signed surface-water depth relative to ground in metres.
!>
!> Computes `HRFZZ-ZGRUND`; a negative value is retained if the stored water
!> surface lies below the ground elevation.
!>
!> Returns signed surface-water depth (m).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the surface-water-depth accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION srf_dep(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = hrfzz(iel)-zgrund(iel)
   END FUNCTION srf_dep

!> @brief Returns surface evaporation in millimetres per hour.
!>
!> Converts `ESOILA` from m/s using `mps_to_mmph`.
!>
!> Returns surface evaporation rate (mm/hour).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the surface-evaporation accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION srf_evap(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*esoila(iel)
   END FUNCTION srf_evap

!> @brief Returns total mobile-sediment depth in millimetres.
!>
!> Converts `DLS` from metres using `m_to_mm`. The sediment component must be
!> active and initialized.
!>
!> Returns total mobile-sediment depth (mm).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the mobile-sediment-depth accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION s_t_dp(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = m_to_mm*dls(iel)
   END FUNCTION s_t_dp

!> @brief Returns surface sediment erosion/deposition velocity in mm/day.
!>
!> Converts signed `GNU` from m/s using `mps_to_mmpd`. The sediment component
!> must be active and initialized; the core sign is preserved.
!>
!> Returns signed erosion/deposition velocity (mm/day).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the erosion/deposition velocity accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION s_v_er(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmpd*gnu(iel)
   END FUNCTION s_v_er

!> @brief Returns volumetric water content for one subsurface cell.
!>
!> The current SHETRAN 4 path reads `VSTHE(ilay,iel)` directly and narrows the
!> double-precision value to default real.
!>
!> Returns volumetric water content (m3/m3).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the subsurface water-content accessor. |
!> | 2004-11-22 | JE | - | Made state access common to SHETRAN 3 and 4. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the obsolete SHETRAN 3 access branch. |
!> @endhistory
   ELEMENTAL REAL FUNCTION theta(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Bottom-up subsurface cell number.
      r = vsthe(ilay,iel)
   END FUNCTION theta

!> @brief Returns the dynamic number of subsurface cells per element column.
!>
!> Returns `top_cell_no`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the subsurface cell-count accessor. |
!> @endhistory
   PURE INTEGER FUNCTION top_cell() RESULT(r)
      r = top_cell_no
   END FUNCTION top_cell

!> @brief Returns transpiration in millimetres per hour.
!>
!> Converts `ERZA` from m/s using `mps_to_mmph`.
!>
!> Returns transpiration rate (mm/hour).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the transpiration accessor. |
!> @endhistory
   ELEMENTAL REAL FUNCTION trnsp(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*erza(iel)
   END FUNCTION trnsp

!> @brief Returns the integer major SHETRAN version used by visualisation.
!>
!> `INT(SHEVER)` intentionally truncates the numeric version, currently 4.6,
!> to its major version.
!>
!> Returns major version number, currently 4.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the visualisation version accessor. |
!> @endhistory
   PURE INTEGER FUNCTION version() RESULT(r)
      r = INT(shever)
   END FUNCTION version

!> @brief Returns signed vertical water flux for one subsurface cell.
!>
!> The current SHETRAN 4 path reads `QVSV(ilay,iel)` in m/s, preserves its
!> solver sign, and narrows the double-precision value to default real.
!>
!> Returns signed vertical water flux (m/s).
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the vertical subsurface-flow accessor. |
!> | 2004-11-22 | JE | - | Made state access common to SHETRAN 3 and 4. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the obsolete SHETRAN 3 access branch. |
!> @endhistory
   ELEMENTAL REAL FUNCTION v_flow(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Bottom-up subsurface cell number.
      r = qvsv(ilay, iel)
   END FUNCTION v_flow

END MODULE visualisation_interface_left
