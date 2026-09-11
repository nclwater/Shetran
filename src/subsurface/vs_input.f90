!> summary: The subsurface data file and the initial-condition file.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[VSIN]] reads the variably saturated subsurface data file (`VSD`,
!> `VS01`--`VS18`) and, when `INITYP` requests it, the initial-conditions file
!> (`VSI`). It then builds the soil, river-bed and aquifer-zone cells through
!> [[vs_connectivity]] and generates the soil lookup tables through
!> [[vs_soil_tables:VSSOIL]]. `ABORT_VSIN` reports a fatal input failure.
!>
!> The six `*_VSREAD` buffers were `VSREAD`'s own local arrays; they are
!> allocatable module state, allocated by `initialise_vsread_buffers`, because
!> as automatic locals they overflowed the stack. That is the only reason they
!> are visible here.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, half, one, two, zero
   USE array_limits, ONLY: LLEE, nelee, NLYREE, NSEE, NVSEE
   USE element_geometry, ONLY: top_cell_no, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMXY, NGDBGN, NX, NY
   USE channel_geometry, ONLY: BEXBK, ICMBK, ZBEFF
   USE file_units, ONLY: BFB, BHB, FID_logfile, LFB, LHB, VSD, VSI, WLD
   USE input_workspace, ONLY: DUMMY, IDUM
   USE vs_state, ONLY: DELTAZ, initialise_al_c2, NLYR, NLYRBT, NS, NTSOIL, NVSSPC, NVSSPT, &
                       NVSWLI, NVSWLT, NWELBT, NWELTP, VSPOR, VSPSI, ZLYRBT, ZVSNOD, ZVSPSL
   USE vs_config, ONLY: BFAST, BHELEV, BSOILP, DCRBED, DCRTOT, DCSTOT, DCSZON, DRBED, &
                        INITYP, ISRBED, IVSFLG, IVSNTB, IVSSTO, NBBCAT, NBBTYP, NCRBED, &
                        NCSZON, NLBCAT, NLBTYP, NVSBD, NVSBF, NVSBH, NVSERR, NVSLF, NVSLFL, &
                        NVSLFN, NVSLFT, NVSLG, NVSLGL, NVSLGN, NVSLGT, NVSLH, NVSLHL, &
                        NVSLHN, NVSLHT, NVSSP, NVSWL, NVSWLC, TBKR, TBKRC, TBPSI, TBTHE, &
                        TBTHEC, VSALPH, VSIPSD, VSK3D, VSKR, VSSPCO, VSSPD, VSSPZ, VSTRES, &
                        VSVGN, VSWL, VSWV, VSZMAX, VSZMIN, VSZWLB, VSZWLT
   USE vs_soil_tables, ONLY: NSOLEE, NVSSOL, VSPDET, VSPDKR, VSPETA, VSPKR, VSPPOR, VSPPSI, &
                             VSPSS, VSPTHE, VSSOIL
   USE vs_connectivity, ONLY: VSCONC, VSCONL
   USE vs_column_solver, ONLY: VSFUNC
   USE float_compare, ONLY: gtzero, ltzero
   USE record_readers, ONLY: ALREAD
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_error
   USE error_status, ONLY: errstat_alloc, errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: VSIN


   ! Read-buffer arrays for VSREAD, moved to allocatable module state (was
   ! routine-local) to avoid a stack-related crash; see initialise_vsread_buffers.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: IVSDUM_VSREAD !! `VSREAD` work buffer: per-category layer soil-type codes.
   INTEGER, DIMENSION(:), ALLOCATABLE :: IVSCAT_VSREAD   !! `VSREAD` work buffer: layer category selected by each element.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: ISDUM_VSREAD  !! `VSREAD` work buffer: integer fields read from `VS05`.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RVSDUM_VSREAD !! `VSREAD` work buffer: per-category layer boundary depths.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RSDUM_VSREAD  !! `VSREAD` work buffer: real-valued fields read from `VS05`.
   LOGICAL, DIMENSION(:), ALLOCATABLE :: BDONE_VSREAD    !! `VSREAD` work buffer: per-element layer-data-assigned flag.

CONTAINS

!> Allocates and zeroes the [[vsread]] category/layer work buffers.
!>
!> `IVSDUM_VSREAD`, `IVSCAT_VSREAD`, `ISDUM_VSREAD`, `RVSDUM_VSREAD`,
!> `RSDUM_VSREAD`, and `BDONE_VSREAD` were originally declared local to
!> [[vsread]]. They were moved into allocatable module state, allocated once
!> here, to avoid a stack-related crash from their combined size. [[vsread]]
!> calls this routine on every entry; the `ALLOCATED` guard makes repeated
!> calls safe, but the zeroing below always re-runs.
!>
!> @note
!> Unlike [[initialise_vsmod]], this routine is safe to call more than once:
!> allocation happens at most once, but the work arrays are always reset to
!> zero/false so each [[vsread]] call starts from a clean state.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   SUBROUTINE initialise_vsread_buffers()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VSmod:initialise_vsread_buffers"

      IF (.NOT. ALLOCATED(IVSDUM_VSREAD)) THEN
         ALLOCATE (IVSDUM_VSREAD(NELEE, NLYREE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IVSDUM_VSREAD", location, emsg)
         ALLOCATE (IVSCAT_VSREAD(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IVSCAT_VSREAD", location, emsg)
         ALLOCATE (ISDUM_VSREAD(NSEE, 8), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "ISDUM_VSREAD", location, emsg)
         ALLOCATE (RVSDUM_VSREAD(NELEE, NLYREE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "RVSDUM_VSREAD", location, emsg)
         ALLOCATE (RSDUM_VSREAD(NSEE, 8), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "RSDUM_VSREAD", location, emsg)
         ALLOCATE (BDONE_VSREAD(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "BDONE_VSREAD", location, emsg)
      END IF

      ! Initialise to default values
      IVSDUM_VSREAD = 0
      IVSCAT_VSREAD = 0
      ISDUM_VSREAD = 0
      RVSDUM_VSREAD = zero
      RSDUM_VSREAD = zero
      BDONE_VSREAD = .FALSE.

   END SUBROUTINE initialise_vsread_buffers

!> Initialises the VSS component.
!>
!> `VSIN` controls one-time setup of the VSS component before the first
!> timestep. It allocates shared run-size arrays, reads the manual VSS data
!> file through [[vsread]], initialises time-varying boundary input streams,
!> constructs layer and cell connectivity, builds soil hydraulic lookup tables,
!> and creates the initial pressure-head and conductivity fields.
!>
!> The setup sequence is:
!>
!> | Step | Action |
!> |:-----|:-------|
!> | Allocate shared storage | `INITIALISE_AL_C2` allocates arrays needed before `top_cell_no` is known. |
!> | Read VSS input | [[vsread]] loads `VSD` data and returns user aquifer connectivity `IAQCON`. |
!> | Prime boundary files | First records are read for well, lateral-flow/head, and bottom-flow/head files when their category counts are non-zero. |
!> | Build connectivity | [[vsconl]] creates layer connectivity and [[vsconc]] creates cells, node elevations, and cell connectivity. |
!> | Locate wells/springs | `NWELBT`, `NWELTP`, and `NVSSPC` are set from well screen depths and spring source depths. |
!> | Soil tables | [[vssoil]] builds pressure-head lookup tables for each soil/lithology. |
!> | Initial conditions | `INITYP` selects the initial pressure-head setup. |
!> | Initial conductivity | [[vsfunc]] checks/interpolates initial pressure heads and fills `VSKR`; `IVSSTO` stores lookup-table interval indices. |
!>
!> Initial-condition handling follows the manual `VS03`/`VSI` options:
!>
!> | `INITYP` | Initialisation |
!> |:---------|:---------------|
!> | 1 | Uniform phreatic-surface depth `VSIPSD`; equilibrium profile \(VSPSI=z_{psl}-z_{node}\). |
!> | 2 | Phreatic-surface elevations read from the `VSI` file; equilibrium profile. |
!> | 3 | Full cell pressure potentials read from `VSI`; `ZVSPSL` is derived from the highest non-negative pressure head. |
!>
!> Main outputs are well screen cell bounds `NWELBT`/`NWELTP`, spring source
!> cell `NVSSPC`, pressure heads `VSPSI`, phreatic-surface levels `ZVSPSL`,
!> lookup interval state `IVSSTO`, and initial relative conductivity `VSKR`.
!> Data-reading or initialisation errors accumulate in `NVSERR`; any non-zero
!> count raises fatal error 1040 via the contained `ABORT_VSIN`.
!>
!> @note
!> `ISTART` is `1` when explicit banks are present and `total_no_links+1`
!> otherwise. `INITYP=2` and `INITYP=3` therefore read `VSI` data only for
!> `ISTART:total_no_elements`, not necessarily for every manual element listed
!> in the `VSI` table. For `INITYP=3`, each element record must appear in that
!> exact increasing order; a mismatched `IEL` raises error 1041 and then the
!> accumulated fatal error 1040.
!> @endnote
!>
!> @warning
!> Well-screen depths (`VS12b`) and spring source depths (`VS13b`) are assumed
!> to fall inside the generated column cells. If a depth search fails, the code
!> falls through with the loop index beyond the searched range rather than
!> reporting a dedicated bounds error.
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1996-10-21. |
!> | 1997-01-22 | RAH | 4.1 | Removed long/leading comments and lower-case code; amended the externals list; extended the [[vsfunc]] argument list. |
!> | 1997-05-12 | RAH | 4.1 | Swapped the `IVSSTO`/`VSKR` indices and scrapped the local arrays `ICSDUM`/`CKRDUM`; likewise swapped `DELTAZ`, `ZVSNOD`, `VSPSI` and scrapped `CPSDUM`; scrapped the outputs `VSETAN`/`VSKRN`; rationalised and initialised loops 800 and 950; generic intrinsics; made more use of `ISTART`; put labels in order. |
!> | 1997-05-22 | RAH | 4.1 | Defaulted `NWELTP` to 1; used `GOTO` for errors and fixed an error in message 1041. |
!> | 1997-06-30 | RAH | 4.1 | Brought `NAQCON`/`IAQCON` from `VSINIT.INC`, swapped their indices, and passed them to [[vsread]] and [[vsconl]]; used format 9010 in place of 9020; replaced `NGDBGN` with `NLF+1`. |
!> | 2026-04-06/07 | SvB | 4.6 | Replaced the `GOTO 8900`-based fatal-error exit with the contained `ABORT_VSIN` routine, called from the two error sites; converted well/spring search loops and the `INITYP=3` layer loop from labelled `GOTO`s to named `DO`/`EXIT` constructs. Same error conditions and search results. |
!> @endhistory
   SUBROUTINE VSIN()

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NVSEE, total_no_elements, total_no_links, top_cell_no, BEXBK,
      ! NVSERR, NVSWL, NVSLF, NVSLH, NVSBF, NVSBH, WLD, LFB, LHB, BFB, BHB,
      ! NWELBT, NWELTP, NVSSPC, NLYRBT, ZGRUND, NVSWLI, VSZWLB, VSZWLT, ZVSNOD,
      ! VSSPD, DELTAZ, INITYP, ZVSPSL, ZLYRBT, VSIPSD, VSI, VSPSI, NLYR, NTSOIL,
      ! IVSSTO, NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, VSPETA, VSPDKR, VSPDET,
      ! VSKR, ERRLVL_error, ERRLVL_fatal, FID_logfile, ERROR, INITIALISE_AL_C2, VSREAD, VSCONL,
      ! VSCONC, VSSOIL, VSFUNC, half, GTZERO, LTZERO

      IMPLICIT NONE

      ! Locals
      CHARACTER(132) :: MSG
      INTEGER :: IEL, ICL, ILYR, ICBOT, ICTOP, IW, IELIN, ISTART, NAQCON, ios
      INTEGER :: IAQCON(4, NVSEE), ISDUM(LLEE)
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'VSmod:VSIN' !! Location string for read-error reports.
      DOUBLE PRECISION :: DZ, RDUM, ZGI, ZMIN
      DOUBLE PRECISION :: CDUM1(LLEE), CDUM2(LLEE), CDUM3(LLEE), CDUM4(LLEE)

      !----------------------------------------------------------------------*

      ! top_cell_no is unknown at this point. But the code to caculate top_cell_no
      ! uses DELTAZ and ZVSNOD so these use llee
      CALL INITIALISE_AL_C2()

      WRITE (FID_logfile, 9010) 'Start', ' '

      NVSERR = 0
      IF (BEXBK) THEN
         ISTART = 1
      ELSE
         ISTART = total_no_links + 1
      END IF

      ! call VSREAD to read from input data file
      CALL VSREAD(NAQCON, IAQCON)

      ! Trap configuration errors immediately
      IF (NVSERR > 0) THEN
         CALL ABORT_VSIN()
         RETURN
      END IF

      ! read first lines of time-varying files
      IF (NVSWL > 0) THEN
         READ (WLD, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF
      IF (NVSLF > 0) THEN
         READ (LFB, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF
      IF (NVSLH > 0) THEN
         READ (LHB, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF
      IF (NVSBF > 0) THEN
         READ (BFB, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF
      IF (NVSBH > 0) THEN
         READ (BHB, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF

      ! call VSCONL and VSCONC to set up connectivity arrays for layers and cells
      CALL VSCONL(NAQCON, IAQCON)
      CALL VSCONC()

      ! set up cell numbers for wells and springs
      ! set defaults
      DO IEL = 1, total_no_elements
         NWELBT(IEL) = 1
         NWELTP(IEL) = 1
         NVSSPC(IEL) = 0
      END DO

      element_loop_wells_springs: DO IEL = total_no_links + 1, total_no_elements
         ICBOT = NLYRBT(IEL, 1)
         ZGI = ZGRUND(IEL)
         IW = NVSWLI(IEL)

         IF (IW > 0) THEN
            ! Find bottom well node
            RDUM = ZGI - VSZWLB(IW)
            find_bottom: DO ICL = ICBOT, top_cell_no
               IF (RDUM <= ZVSNOD(ICL, IEL)) EXIT find_bottom
            END DO find_bottom
            NWELBT(IEL) = ICL

            ! Find top well node (looping backwards)
            RDUM = ZGI - VSZWLT(IW)
            find_top: DO ICL = top_cell_no, ICBOT, -1
               IF (RDUM >= ZVSNOD(ICL, IEL)) EXIT find_top
            END DO find_top
            NWELTP(IEL) = ICL
         END IF

         RDUM = VSSPD(IEL)

         IF (GTZERO(RDUM)) THEN
            RDUM = ZGI - RDUM

            ! Find specific node based on delta Z
            find_spc: DO ICL = ICBOT, top_cell_no
               DZ = ABS(ZVSNOD(ICL, IEL) - RDUM)
               IF (DZ <= half*DELTAZ(ICL, IEL)) EXIT find_spc
            END DO find_spc
            NVSSPC(IEL) = ICL
         END IF

      END DO element_loop_wells_springs

      ! call VSSOIL to set up soil property tables
      CALL VSSOIL()

      ! set up initial conditions (read from file unit VSI, if required)
      ! type 1 - uniform phreatic surface depth, equilibrium psi profile
      IF (INITYP == 1) THEN
         DO IEL = 1, total_no_elements
            ZVSPSL(IEL) = MAX(ZLYRBT(IEL, 1), ZGRUND(IEL) - VSIPSD)
         END DO

         ! type 2 - varying phreatic surface level, equilibrium psi profile
      ELSE IF (INITYP == 2) THEN
         READ (VSI, '(A)', IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
         READ (VSI, *, IOSTAT=ios, IOMSG=emsg) (ZVSPSL(IEL), IEL=ISTART, total_no_elements)
         CALL errstat_read(ios, location, emsg)

         ! type 3 - 3-dimensional field of psi values (+ init. psl for output)
      ELSE
         READ (VSI, '(A)', IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)

         element_loop_vsi: DO IEL = ISTART, total_no_elements
            READ (VSI, *, IOSTAT=ios, IOMSG=emsg) IELIN
            CALL errstat_read(ios, location, emsg)

            IF (IELIN /= IEL) THEN
               NVSERR = NVSERR + 1
               WRITE (MSG, 9040) IEL
               CALL RAISE_ERROR(ERRLVL_error, 1041, FID_logfile, 0, 0, MSG)
               CALL ABORT_VSIN()
               RETURN
            END IF

            ICBOT = NLYRBT(IEL, 1)
            ICTOP = top_cell_no

            READ (VSI, *, IOSTAT=ios, IOMSG=emsg) VSPSI(ICBOT:ICTOP, IEL)
            CALL errstat_read(ios, location, emsg)

            ZMIN = ZVSNOD(ICBOT, IEL) - half*DELTAZ(ICBOT, IEL)

            search_loop: DO ICL = ICBOT, ICTOP
               IF (LTZERO(VSPSI(ICL, IEL))) EXIT search_loop
            END DO search_loop

            ICL = MAX(ICBOT, ICL - 1)
            ZVSPSL(IEL) = MAX(ZMIN, ZVSNOD(ICL, IEL) + VSPSI(ICL, IEL))

         END DO element_loop_vsi

      END IF

      ! set up equilibrium psi profile for types 1 or 2
      IF (INITYP == 1 .OR. INITYP == 2) THEN
         equilibrium_profile_loop: DO IEL = 1, total_no_elements
            DO ICL = NLYRBT(IEL, 1), top_cell_no
               VSPSI(ICL, IEL) = ZVSPSL(IEL) - ZVSNOD(ICL, IEL)
            END DO
         END DO equilibrium_profile_loop
      END IF

      ! set up initial relative conductivities for all elements
      init_cond_loop: DO IEL = ISTART, total_no_elements

         DO ILYR = 1, NLYR(IEL)
            DO ICL = NLYRBT(IEL, ILYR), NLYRBT(IEL, ILYR + 1) - 1
               ISDUM(ICL) = NTSOIL(IEL, ILYR)
               IVSSTO(ICL, IEL) = 0
            END DO
         END DO

         ICBOT = NLYRBT(IEL, 1)
         ICTOP = top_cell_no

         CALL VSFUNC(NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
                     VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ISDUM(ICBOT), &
                     VSPSI(ICBOT, IEL), IVSSTO(ICBOT, IEL), CDUM1, CDUM2, VSKR(ICBOT, IEL), &
                     CDUM3, CDUM4)

      END DO init_cond_loop

      WRITE (FID_logfile, 9010) 'End', '   '

      RETURN

      ! FORMAT STATEMENTS for the host subroutine
9010  FORMAT(/'!!', 78('#')/1X, A, ' of VSS data ', A, 60('#')/80('#'))
9040  FORMAT('Error reading VSS initial conditions for element ', I4, '.')

   CONTAINS

      !> Reports the accumulated VSS data-reading/initialisation error count
      !> and stops via fatal error 1040. Replaces the legacy `GOTO 8900` exit
      !> from [[vsin]].
      SUBROUTINE ABORT_VSIN()
         WRITE (MSG, 9030) NVSERR
         CALL RAISE_ERROR(ERRLVL_fatal, 1040, FID_logfile, 0, 0, MSG)

         ! Format statement scoped correctly to the internal subroutine
9030     FORMAT(I4, ' Errors have occurred in VSS data reading ', 'or initialisation.')
      END SUBROUTINE ABORT_VSIN

   END SUBROUTINE VSIN

!> Reads static VSS data from the subsurface input file.
!>
!> `VSREAD` reads the manual `VSD` groups `VS01`-`VS18` and populates the module
!> state used by [[vsconl]], [[vsconc]], [[vssoil]], [[vsin]], and the timestep
!> solver. The routine uses `ALREAD` for labelled blocks and increments
!> `NVSERR` or raises fatal errors when required layer/table data are
!> inconsistent.
!>
!> Main input groups and destinations:
!>
!> | Group | Data read | Main arrays/variables filled |
!> |:------|:----------|:-----------------------------|
!> | `VS01` | VSD title | Printed to `FID_logfile`. |
!> | `VS02` | logical flags | `BFAST`, `BSOILP`, `BHELEV`. |
!> | `VS03` | counts and initialisation type | `NS`, `NCSZON`, `NCRBED`, `INITYP`. |
!> | `VS04` | initial phreatic depth and mesh/averaging controls | `VSIPSD`, `VSZMIN`, `VSZMAX`, `VSWV`, `VSWL`. |
!> | `VS05`, `VS05a` | soil/lithology hydraulic parameters and optional tables | `IVSFLG`, `IVSNTB`, `VSK3D`, `VSPOR`, `VSTRES`, `VSPSS`, `VSVGN`, `VSALPH`, `TBPSI`, `TBTHE`, `TBKR`, spline coefficients. |
!> | `VS06`, `VS07` | soil-zone and river-bed cell depths | `DCSZON`, `DCSTOT`, `DCRBED`, `DCRTOT` and helper node-depth arrays. |
!> | `VS08`-`VS08d` | aquifer-zone layer categories, grids, and individual elements | `NLYR`, `NTSOIL`, `ZLYRBT` for grids, banks, and links. |
!> | `VS09`, `VS09a` | river-bed soil type and depth | `ISRBED`, `DRBED`, link bed layers. |
!> | `VS10`, `VS10a` | user-defined aquifer connectivity | `NAQCON`, `IAQCON` for [[vsconl]]. |
!> | `VS11` | boundary category counts | `NVSWL`, `NVSSP`, `NVSLF`, `NVSLH`, `NVSLG`, `NVSBF`, `NVSBH`, `NVSBD`. |
!> | `VS12`-`VS13b` | wells and springs | `NVSWLI`, `NVSWLC`, `NVSWLT`, `VSZWLB`, `VSZWLT`, `NVSSPT`, `VSSPD`, `VSSPZ`, `VSSPCO`. |
!> | `VS14`-`VS16b` | lateral boundary type/category grids and selected-layer lists | `NLBTYP`, `NLBCAT`, `NVSLFN/HN/GN`, `NVSLFL/HL/GL`, `NVSLFT/HT/GT`. |
!> | `VS17`, `VS18` | bottom boundary type/category grids | `NBBTYP`, `NBBCAT`. |
!>
!> Conductivities from `VS05` are converted from m/day to m/s for the solver.
!> `VSZMAX` from `VS04` is stored as the input value plus `1.0e-6`, matching
!> the legacy tolerance used when deciding aquifer-zone cell subdivisions.
!> For tabulated soil options (`IVSFLG = 2` or `4`), the routine reads
!> \(\psi\), \(\theta\), and \(K_r\) tables and builds natural cubic-spline
!> second-derivative coefficients in log10(-\(\psi\)) space. For `IVSFLG = 4`,
!> the manual says entered `K_r` values are not used, but the table still has to
!> be present for input compatibility.
!>
!> Layer category data are expanded to element arrays. Category grids may cover
!> links and grid elements; individual `VS08d` records fill elements whose
!> category is zero. Soil-zone and river-bed layer boundaries are snapped to the
!> computational cell-depth sequences so later cell construction in [[vsconc]]
!> is consistent with `DCSZON` and `DCRBED`.
!>
!> `VS13` itself is treated as a dummy record in this implementation: the number
!> of spring records read from `VS13a`/`VS13b` is `NVSSP` from `VS11`. For `VS16`
!> selected-layer boundary categories, a category with `NLDUM` selected layers
!> contributes `NLDUM` time-series values, while an unlisted category contributes
!> one full-column value; this is why `NVSLFT`, `NVSLHT`, and `NVSLGT` start at
!> their category counts and add `NLDUM - 1` for each selected-layer record.
!>
!> On exit, for each element `e = 1:NEL`, the boundary type arrays are
!> non-negative and the boundary category arrays have valid defaults:
!>
!> | Array | Exit condition |
!> |:------|:---------------|
!> | `NLBTYP(e)` | `0 <= NLBTYP(e)` |
!> | `NBBTYP(e)` | `0 <= NBBTYP(e)` |
!> | `NLBCAT(e)` | `1 <= NLBCAT(e)` |
!> | `NBBCAT(e)` | `1 <= NBBCAT(e)` |
!> | `NVSWLC(e)` | `1 <= NVSWLC(e)` |
!>
!> @note
!> The category/layer work buffers (`IVSDUM_VSREAD`, `IVSCAT_VSREAD`,
!> `ISDUM_VSREAD`, `RVSDUM_VSREAD`, `RSDUM_VSREAD`, `BDONE_VSREAD`) are
!> allocated once in module state by [[initialise_vsread_buffers]], called at
!> the start of every `VSREAD` entry, rather than being routine-local arrays as
!> in the historical `.F`-era implementation; see the module-level history for
!> why.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1996-01-31. |
!> | 1997-02-13 | RAH | 4.1 | Initialised `NLBTYP`, `NLBCAT`, `NVSWLC`, `NBBTYP`, and `NBBCAT`; reversed the `NVSLFL`, `NVSLHL`, and `NVSLGL` subscripts (see [[vssim]]). |
!> | 1997-05-22 | RAH | 4.1 | Initialised `NVSWLI`; fixed errors: used `TBKR` rather than `TBTHE` in loop 21 (`IVSFLG=2`), and added `-1` to `NVSLHT` and `NVSLGT`. |
!> | 1997-06-30 | RAH | 4.1 | Brought `NAQCON`/`IAQCON` from `VSINIT.INC` into the argument list and swapped their indices, fixing an error in the `ALREAD` call; restricted the `VS08b` `ALREAD` call to `NLF > 0`. |
!> | 1997-08-05 | RAH | 4.1 | Ensured `NLBCAT`, `NBBCAT`, and `NVSWLC` are all at least 1. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the labelled `GOTO`-driven "find first free layer slot" and "count link-bed layers" searches (both duplicated for category and per-element data) as `DO WHILE` loops with `CYCLE`d element-skip logic. Same search results. |
!> | 2026-05-03 | SvB | 4.6 | Moved `IVSDUM`, `IVSCAT`, `ISDUM`, `RVSDUM`, `RSDUM`, and `BDONE` from routine-local arrays into allocatable module state (`*_VSREAD`), allocated by [[initialise_vsread_buffers]], to fix a stack-related crash from their combined size. |
!> @endhistory
   SUBROUTINE VSREAD(NAQCON, IAQCON)

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NELEE, NLYREE, NSEE, NVSEE, total_no_elements, NVSWLI, NLBTYP,
      ! NBBTYP, NVSWLC, NLBCAT, NBBCAT, ALREAD, VSD, FID_logfile, IDUM, DUMMY, BFAST,
      ! BSOILP, BHELEV, NS, NCSZON, NCRBED, INITYP, VSIPSD, VSZMIN, VSZMAX,
      ! VSWV, VSWL, IVSFLG, IVSNTB, VSK3D, VSPOR, VSTRES, VSPSS, VSVGN, VSALPH,
      ! VSPPOR, ERROR, ERRLVL_fatal, TBPSI, TBTHE, TBKR, TBTHEC, TBKRC, zero, two, one,
      ! DCSZON, DCSTOT, DCRBED, DCRTOT, BEXBK, total_no_links, NX, NY, ICMXY,
      ! ICMREF, NLYR, NTSOIL, ZLYRBT, ZGRUND, ICMBK, ZBEFF, NGDBGN, ERRLVL_error,
      ! ISRBED, DRBED, NVSWL, NVSSP, NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD,
      ! NVSLFN, NVSLHN, NVSLGN, NVSLFT, NVSLHT, NVSLGT, NVSLFL, NVSLHL, NVSLGL,
      ! NVSWLT, VSZWLB, VSZWLT, NVSSPT, VSSPD, VSSPZ, VSSPCO

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(INOUT) :: NAQCON       !! Number of user-defined aquifer connectivity records read from `VS10`.
      INTEGER, INTENT(INOUT) :: IAQCON(4, NVSEE) !! User-defined aquifer connectivity records read from `VS10a`.

      ! Locals
      INTEGER :: I, I0, IBK, ICAT, IEL, ILYR, IS, ISP, IW, IWT, IX, IXY0, IY
      INTEGER :: ICOUNT, LCOUNT
      INTEGER :: NUM_CATEGORIES_TYPES, NELEM, NCOUNT, NDUM, NSP, NW
      INTEGER :: ILB, NLB, ITYP, NLDUM, ISDUM1, IDUM1(1), ios
      DOUBLE PRECISION :: DCSDUM(0:LLEE)
      DOUBLE PRECISION :: DCSNOD(LLEE), DCRDUM(0:LLEE), DCRNOD(LLEE), SIG, PDUM
      DOUBLE PRECISION :: XDUM(NVSEE), YDUM(NVSEE), Y2DUM(NVSEE), UDUM(NVSEE)
      CHARACTER(LEN=80)  :: CDUM
      CHARACTER(LEN=132) :: MSG
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'VSmod:VSREAD' !! Location string for read-error reports.

      !----------------------------------------------------------------------*
      ! Initialization

      CALL initialise_vsread_buffers()

      DO IEL = 1, total_no_elements
         NVSWLI(IEL) = 0
         NLBTYP(IEL) = 0
         NBBTYP(IEL) = 0
         NVSWLC(IEL) = 1
         NLBCAT(IEL) = 1
         NBBCAT(IEL) = 1
      END DO

      ! VS01 ----- main data file title
      CALL ALREAD(1, VSD, FID_logfile, ':VS01', 1, 1, 0, CDUM, IDUM, DUMMY)
      WRITE (FID_logfile, '(/, 1X, A, /)') TRIM(CDUM)

      ! VS02 ----- logical flags
      READ (VSD, '(A)', IOSTAT=ios, IOMSG=emsg) CDUM
      CALL errstat_read(ios, location, emsg)
      READ (VSD, *, IOSTAT=ios, IOMSG=emsg) BFAST, BSOILP, BHELEV
      CALL errstat_read(ios, location, emsg)

      ! VS03 ----- integer variables
      CALL ALREAD(2, VSD, FID_logfile, ':VS03', 4, 1, 0, CDUM, IDUM, DUMMY)
      NS = IDUM(1)
      NCSZON = IDUM(2)
      NCRBED = IDUM(3)
      INITYP = IDUM(4)

      ! VS04 ----- real variables
      CALL ALREAD(3, VSD, FID_logfile, ':VS04', 5, 1, 0, CDUM, IDUM, DUMMY)
      VSIPSD = DUMMY(1)
      VSZMIN = DUMMY(2)
      VSZMAX = DUMMY(3) + 1.0D-6
      VSWV = DUMMY(4)
      VSWL = DUMMY(5)

      ! VS05 ----- physical property data
      CALL ALREAD(7, VSD, FID_logfile, ':VS05', NSEE, 8, NS, CDUM, ISDUM_VSREAD, RSDUM_VSREAD)

      DO IS = 1, NS
         IVSFLG(IS) = ISDUM_VSREAD(IS, 2)
         IVSNTB(IS) = ISDUM_VSREAD(IS, 3)
         VSK3D(IS, 1) = RSDUM_VSREAD(IS, 1)/(3600.0D0*24.0D0)
         VSK3D(IS, 2) = RSDUM_VSREAD(IS, 2)/(3600.0D0*24.0D0)
         VSK3D(IS, 3) = RSDUM_VSREAD(IS, 3)/(3600.0D0*24.0D0)
         VSPOR(IS) = RSDUM_VSREAD(IS, 4)
         VSTRES(IS) = RSDUM_VSREAD(IS, 5)
         VSPSS(IS) = RSDUM_VSREAD(IS, 6)
         VSVGN(IS) = RSDUM_VSREAD(IS, 7)
         VSALPH(IS) = RSDUM_VSREAD(IS, 8)
         VSPPOR(IS) = VSPOR(IS)
      END DO

      ! VS05a ---- soil characteristic function tabulated data
      DO IS = 1, NS
         IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
            READ (VSD, *, IOSTAT=ios, IOMSG=emsg) ISDUM1
            CALL errstat_read(ios, location, emsg)
            IF (IS /= ISDUM1) THEN
               WRITE (MSG, 9030) IS
               CALL RAISE_ERROR(ERRLVL_fatal, 1051, FID_logfile, 0, 0, MSG)
            END IF

            DO I = 1, IVSNTB(IS)
               READ (VSD, *, IOSTAT=ios, IOMSG=emsg) TBPSI(I, IS), TBTHE(I, IS), TBKR(I, IS)
               CALL errstat_read(ios, location, emsg)
            END DO

            ! set up cubic spline coefficients for theta, using log(psi)
            ! based on routines 'spline' and 'splint' in NUMERICAL RECIPES
            ! FOR FORTRAN (..UNFINISHED), pp 109 and 110
            ! NB assumes 'natural' boundary conditions (ie zero 2nd derivatives)
            DO I = 1, IVSNTB(IS)
               XDUM(I) = LOG10(-TBPSI(I, IS))
               YDUM(I) = TBTHE(I, IS)
            END DO

            NDUM = IVSNTB(IS)
            Y2DUM(1) = zero
            UDUM(1) = zero
            Y2DUM(NDUM) = zero

            DO I = 2, NDUM - 1
               SIG = (XDUM(I) - XDUM(I - 1))/(XDUM(I + 1) - XDUM(I - 1))
               PDUM = SIG*Y2DUM(I - 1) + two
               Y2DUM(I) = (SIG - one)/PDUM
               UDUM(I) = (6.0D0*((YDUM(I + 1) - YDUM(I))/ &
                                 (XDUM(I + 1) - XDUM(I)) - (YDUM(I) - YDUM(I - 1)) &
                                 /(XDUM(I) - XDUM(I - 1)))/(XDUM(I + 1) - XDUM(I - 1)) &
                          - SIG*UDUM(I - 1))/PDUM
            END DO

            DO I = NDUM - 1, 1, -1
               Y2DUM(I) = Y2DUM(I)*Y2DUM(I + 1) + UDUM(I)
            END DO

            DO I = 1, NDUM
               TBTHEC(I, IS) = Y2DUM(I)
            END DO

            ! if required, set up cubic spline coefficients for Kr similarly
            IF (IVSFLG(IS) == 2) THEN
               DO I = 1, IVSNTB(IS)
                  YDUM(I) = TBKR(I, IS)
               END DO

               Y2DUM(1) = zero
               UDUM(1) = zero
               Y2DUM(NDUM) = zero

               DO I = 2, NDUM - 1
                  SIG = (XDUM(I) - XDUM(I - 1))/(XDUM(I + 1) - XDUM(I - 1))
                  PDUM = SIG*Y2DUM(I - 1) + two
                  Y2DUM(I) = (SIG - one)/PDUM
                  UDUM(I) = (6.0D0*((YDUM(I + 1) - YDUM(I))/ &
                                    (XDUM(I + 1) - XDUM(I)) - (YDUM(I) - YDUM(I - 1))/ &
                                    (XDUM(I) - XDUM(I - 1)))/(XDUM(I + 1) - XDUM(I - 1)) &
                             - SIG*UDUM(I - 1))/PDUM
               END DO

               DO I = NDUM - 1, 1, -1
                  Y2DUM(I) = Y2DUM(I)*Y2DUM(I + 1) + UDUM(I)
               END DO

               DO I = 1, NDUM
                  TBKRC(I, IS) = Y2DUM(I)
               END DO
            END IF
         END IF
      END DO

      ! VS06 ----- soil zone cell sizes (start at the ground surface)
      IF (NCSZON > 0) THEN
         CALL ALREAD(3, VSD, FID_logfile, ':VS06', NCSZON, 1, 0, CDUM, IDUM, DCSZON)
      END IF
      WRITE (FID_logfile, *) 'DCSZON: ', (DCSZON(I), I=1, NCSZON)

      DCSTOT = zero
      DCSDUM(0) = zero

      DO I = 1, NCSZON
         DCSTOT = DCSTOT + DCSZON(I)
         DCSDUM(I) = DCSTOT
         DCSNOD(I) = half*(DCSDUM(I) + DCSDUM(I - 1))
      END DO

      DCSNOD(NCSZON + 1) = DCSTOT + VSZMIN

      ! VS07 ----- river bed cell sizes (start at the bed surface)
      IF (NCRBED > 0) THEN
         CALL ALREAD(3, VSD, FID_logfile, ':VS07', NCRBED, 1, 0, CDUM, IDUM, DCRBED)
      END IF
      WRITE (FID_logfile, *) 'DCRBED: ', (DCRBED(I), I=1, NCRBED)

      DCRTOT = zero
      DCRDUM(0) = zero

      DO I = 1, NCRBED
         DCRTOT = DCRTOT + DCRBED(I)
         DCRDUM(I) = DCRTOT
         DCRNOD(I) = half*(DCRDUM(I) + DCRDUM(I - 1))
      END DO

      DCRNOD(NCRBED + 1) = DCRTOT + VSZMIN

      ! VS08 ----- soil/lithology layer definition data
      ! --- read no. of categories and elements
      CALL ALREAD(2, VSD, FID_logfile, ':VS08', 2, 1, 0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM(1)
      NELEM = IDUM(2)

      ! --- category data
      IF (NUM_CATEGORIES_TYPES == 0) THEN
         ! expect all elements to be input individually
         IF (BEXBK) THEN
            NCOUNT = total_no_elements - 2*total_no_links
         ELSE
            NCOUNT = total_no_elements - total_no_links
         END IF

      ELSE
         ! initialise arrays
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM_VSREAD(IEL, ILYR) = 0
               RVSDUM_VSREAD(IEL, ILYR) = zero
            END DO
         END DO

         ! read layer data
         CALL ALREAD(6, VSD, FID_logfile, ':VS08a', NELEE, NLYREE, NUM_CATEGORIES_TYPES, CDUM, IVSDUM_VSREAD, RVSDUM_VSREAD)

         ! for NUM_CATEGORIES_TYPES = 1, set all elements = category 1
         IF (NUM_CATEGORIES_TYPES == 1) THEN
            DO IEL = 1, total_no_elements
               IVSCAT_VSREAD(IEL) = 1
            END DO

            ! for > 1 category read in categories for links (if required) and grids
         ELSE
            IF (BEXBK .AND. total_no_links > 0) THEN
               CALL ALREAD(2, VSD, FID_logfile, ':VS08b', total_no_links, 1, NUM_CATEGORIES_TYPES, CDUM, IVSCAT_VSREAD, DUMMY)
            END IF

            CALL ALREAD(4, VSD, FID_logfile, ':VS08c', NX, NY, NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)

            DO IY = 1, NY
               IXY0 = (IY - 1)*NX
               DO IX = 1, NX
                  IEL = ICMXY(IX, IY)
                  IF (IEL /= 0) IVSCAT_VSREAD(IEL) = IDUM(IXY0 + IX)
               END DO
            END DO
         END IF

         ! move layer data into elements for ...
         NCOUNT = 0
         element_category_loop: DO IEL = 1, total_no_elements
            IF (ICMREF(IEL, 1) == 1 .OR. ICMREF(IEL, 1) == 2 .OR. &
                (.NOT. BEXBK .AND. ICMREF(IEL, 1) == 3)) CYCLE element_category_loop

            IF (IVSCAT_VSREAD(IEL) == 0) THEN
               NCOUNT = NCOUNT + 1
            ELSE
               BDONE_VSREAD(IEL) = .TRUE.
               ICAT = IVSCAT_VSREAD(IEL)
               ICOUNT = 0

               ! Modern DO WHILE replacing GOTO 350 / 355
               DO WHILE (IVSDUM_VSREAD(ICAT, ICOUNT + 1) /= 0)
                  ICOUNT = ICOUNT + 1
               END DO

               ! ...grids
               IF (ICMREF(IEL, 1) == 0) THEN
                  NLYR(IEL) = ICOUNT
                  DO ILYR = 1, NLYR(IEL)
                     NTSOIL(IEL, ILYR) = IVSDUM_VSREAD(ICAT, ILYR)
                     ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - RVSDUM_VSREAD(ICAT, ILYR)
                  END DO

                  ! ...banks
               ELSE
                  DO I = 1, 2
                     IBK = ICMBK(IEL, I)
                     BDONE_VSREAD(IBK) = .TRUE.
                     NLYR(IBK) = ICOUNT
                     DO ILYR = 1, NLYR(IBK)
                        NTSOIL(IBK, ILYR) = IVSDUM_VSREAD(ICAT, ILYR)
                        ZLYRBT(IBK, ILYR) = ZGRUND(IBK) - RVSDUM_VSREAD(ICAT, ILYR)
                     END DO
                  END DO

                  ! ...links (NB uses data from bank 2, which is identical to bank 1)
                  LCOUNT = 0

                  ! Modern DO WHILE replacing GOTO 390 / 395
                  DO WHILE (RVSDUM_VSREAD(ICAT, LCOUNT + 1) >= ZGRUND(IBK) - ZBEFF(IEL) + VSZMIN)
                     LCOUNT = LCOUNT + 1
                  END DO

                  NLYR(IEL) = LCOUNT
                  DO ILYR = 1, NLYR(IEL)
                     NTSOIL(IEL, ILYR) = NTSOIL(IBK, ILYR)
                     ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
                  END DO
               END IF
            END IF
         END DO element_category_loop
      END IF

      ! check no. of category elements consistent with no. of individual elements
      IF (NCOUNT /= NELEM) THEN
         WRITE (MSG, 9000) NCOUNT
         CALL RAISE_ERROR(ERRLVL_fatal, 1032, FID_logfile, 0, 0, MSG)
      END IF

      ! --- element data
      IF (NELEM /= 0) THEN
         ! initialise variables
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM_VSREAD(IEL, ILYR) = 0
               RVSDUM_VSREAD(IEL, ILYR) = zero
            END DO
         END DO

         ! read layer data
         CALL ALREAD(6, VSD, FID_logfile, ':VS08d', NELEE, NLYREE, NELEM, CDUM, IVSDUM_VSREAD, RVSDUM_VSREAD)

         element_data_loop: DO IEL = 1, total_no_elements
            ! ignore banks, links (if no banks), and elements already processed
            IF (BDONE_VSREAD(IEL) .OR. ICMREF(IEL, 1) == 1 .OR. ICMREF(IEL, 1) == 2 .OR. &
                (.NOT. BEXBK .AND. ICMREF(IEL, 1) == 3)) CYCLE element_data_loop

            BDONE_VSREAD(IEL) = .TRUE.
            ICOUNT = 0

            DO WHILE (IVSDUM_VSREAD(IEL, ICOUNT + 1) /= 0)
               ICOUNT = ICOUNT + 1
            END DO

            ! ...grids
            IF (ICMREF(IEL, 1) == 0) THEN
               NLYR(IEL) = ICOUNT
               DO ILYR = 1, NLYR(IEL)
                  NTSOIL(IEL, ILYR) = IVSDUM_VSREAD(IEL, ILYR)
                  ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - RVSDUM_VSREAD(IEL, ILYR)
               END DO

               ! ...banks
            ELSE
               DO I = 1, 2
                  IBK = ICMBK(IEL, I)
                  BDONE_VSREAD(IBK) = .TRUE.
                  NLYR(IBK) = ICOUNT
                  DO ILYR = 1, NLYR(IBK)
                     NTSOIL(IBK, ILYR) = IVSDUM_VSREAD(IEL, ILYR)
                     ZLYRBT(IBK, ILYR) = ZGRUND(IBK) - RVSDUM_VSREAD(IEL, ILYR)
                  END DO
               END DO

               ! ...links
               LCOUNT = 0
               DO WHILE (RVSDUM_VSREAD(IEL, LCOUNT + 1) >= ZGRUND(IBK) - ZBEFF(IEL) + VSZMIN)
                  LCOUNT = LCOUNT + 1
               END DO

               NLYR(IEL) = LCOUNT
               DO ILYR = 1, NLYR(IEL)
                  NTSOIL(IEL, ILYR) = NTSOIL(IBK, ILYR)
                  ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
               END DO
            END IF
         END DO element_data_loop
      END IF

      ! adjust horizon boundaries in soil zone to match computational mesh
      ! and set up ZLYRBT for ground surface
      adjust_horizon_loop: DO IEL = NGDBGN, total_no_elements
         layer_adjust_loop: DO ILYR = NLYR(IEL), 1, -1
            IF (ZGRUND(IEL) - ZLYRBT(IEL, ILYR) > DCSTOT + VSZMIN) EXIT layer_adjust_loop

            search_zone_loop: DO I = 1, NCSZON + 1
               IF (DCSNOD(I) > ZGRUND(IEL) - ZLYRBT(IEL, ILYR)) THEN
                  ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - DCSDUM(I - 1)
                  CYCLE layer_adjust_loop
               END IF
            END DO search_zone_loop
         END DO layer_adjust_loop

         ZLYRBT(IEL, NLYR(IEL) + 1) = ZGRUND(IEL)
      END DO adjust_horizon_loop

      IF (BEXBK) THEN
         DO IEL = 1, total_no_links
            IBK = ICMBK(IEL, 1)
            DO ILYR = 1, NLYR(IEL)
               ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
            END DO
         END DO
      END IF

      ! check that all elements have been set up
      check_done_loop: DO IEL = 1, total_no_elements
         IF (.NOT. BEXBK .AND. ICMREF(IEL, 1) /= 0) CYCLE check_done_loop
         IF (.NOT. BDONE_VSREAD(IEL)) THEN
            WRITE (MSG, 9020) IEL
            CALL RAISE_ERROR(ERRLVL_error, 1033, FID_logfile, 0, 0, MSG)
         END IF
      END DO check_done_loop

      ! VS09 ----- channel bed layer
      IF (total_no_links > 0 .AND. BEXBK) THEN
         ! read soil types for each link
         CALL ALREAD(2, VSD, FID_logfile, ':VS09', total_no_links, 1, 1, CDUM, ISRBED, DUMMY)

         ! read bed depths for each link
         CALL ALREAD(3, VSD, FID_logfile, ':VS09a', total_no_links, 1, 1, CDUM, IDUM, DRBED)

         ! set up channel bed layer for each link
         DO IEL = 1, total_no_links
            IF (DRBED(IEL) > VSZMIN) THEN
               NLYR(IEL) = NLYR(IEL) + 1
               NTSOIL(IEL, NLYR(IEL)) = ISRBED(IEL)
               ZLYRBT(IEL, NLYR(IEL)) = ZBEFF(IEL) - DRBED(IEL)

               IF (ZLYRBT(IEL, NLYR(IEL)) < ZLYRBT(IEL, NLYR(IEL) - 1) + VSZMIN) THEN
                  NLYR(IEL) = NLYR(IEL) - 1
                  NTSOIL(IEL, NLYR(IEL)) = ISRBED(IEL)
               END IF
            END IF
         END DO

         ! adjust horizon boundaries in river bed to match computational mesh
         ! and set up ZLYRBT for river bed surface
         bed_adjust_loop: DO IEL = 1, total_no_links
            layer_bed_loop: DO ILYR = NLYR(IEL), 1, -1
               IF (ZGRUND(IEL) - ZLYRBT(IEL, ILYR) > DCRTOT + VSZMIN) EXIT layer_bed_loop

               search_bed_loop: DO I = 1, NCRBED + 1
                  IF (DCRNOD(I) > ZGRUND(IEL) - ZLYRBT(IEL, ILYR)) THEN
                     ZLYRBT(IEL, ILYR) = ZBEFF(IEL) - DCRDUM(I - 1)
                     CYCLE layer_bed_loop
                  END IF
               END DO search_bed_loop
            END DO layer_bed_loop

            ZLYRBT(IEL, NLYR(IEL) + 1) = ZBEFF(IEL)
         END DO bed_adjust_loop
      END IF

      ! VS10 ----- aquifer zone user-defined connectivities
      ! FIX: Read into the IDUM array first to satisfy strict array-interface
      ! requirements, then assign the value to the scalar NAQCON.
      CALL ALREAD(2, VSD, FID_logfile, ':VS10', 1, 1, 0, CDUM, IDUM, DUMMY)
      NAQCON = IDUM(1)

      IF (NAQCON > 0) THEN
         CALL ALREAD(2, VSD, FID_logfile, ':VS10a', 4, NAQCON, 0, CDUM, IAQCON, DUMMY)
      END IF

      ! VS11 ----- no. of categories for boundary conditions
      CALL ALREAD(2, VSD, FID_logfile, ':VS11', 8, 1, 0, CDUM, IDUM, DUMMY)
      NVSWL = IDUM(1)
      NVSSP = IDUM(2)
      NVSLF = IDUM(3)
      NVSLH = IDUM(4)
      NVSLG = IDUM(5)
      NVSBF = IDUM(6)
      NVSBH = IDUM(7)
      NVSBD = IDUM(8)

      ! wells -----------------------------------------------
      ! VS12 ----- no. of wells
      IF (NVSWL > 0) THEN
         CALL ALREAD(2, VSD, FID_logfile, ':VS12', 1, 1, 0, CDUM, IDUM, DUMMY)
         NW = IDUM(1)

         ! VS12a ---- element, category number, and target element
         CALL ALREAD(2, VSD, FID_logfile, ':VS12a', 3, NW, 0, CDUM, IDUM, DUMMY)
         DO IW = 1, NW
            I0 = 3*(IW - 1)
            IEL = IDUM(I0 + 1)
            NVSWLC(IEL) = MAX(1, IDUM(I0 + 2))
            IWT = IDUM(I0 + 3)
            IF (IWT > 0) NVSWLT(IWT) = IEL
            NVSWLI(IEL) = IW
         END DO

         ! VS12b ---- depth below ground of bottom and top of well screen
         CALL ALREAD(3, VSD, FID_logfile, ':VS12b', 2, NW, 0, CDUM, IDUM, DUMMY)
         DO IW = 1, NW
            VSZWLB(IW) = DUMMY(2*(IW - 1) + 1)
            VSZWLT(IW) = DUMMY(2*(IW - 1) + 2)
         END DO
      END IF

      ! springs ---------------------------------------------
      ! VS13 ----- no. of springs
      IF (NVSSP > 0) THEN
         NSP = NVSSP
         ! VS13a ---- element and target element
         CALL ALREAD(2, VSD, FID_logfile, ':VS13a', 2, NSP, 0, CDUM, IDUM, DUMMY)
         DO ISP = 1, NSP
            IEL = IDUM(2*(ISP - 1) + 1)
            IF (IDUM(2*(ISP - 1) + 2) > 0) NVSSPT(IDUM(2*(ISP - 1) + 2)) = IEL
         END DO

         ! VS13b ---- depth of spring source below ground, elevation of
         !            discharge point, spring coefficient
         CALL ALREAD(3, VSD, FID_logfile, ':VS13b', 3, NSP, 0, CDUM, IDUM1, DUMMY)
         DO ISP = 1, NSP
            IEL = IDUM(2*(ISP - 1) + 1)
            VSSPD(IEL) = DUMMY(3*(ISP - 1) + 1)
            VSSPZ(IEL) = DUMMY(3*(ISP - 1) + 2)
            VSSPCO(IEL) = DUMMY(3*(ISP - 1) + 3)
         END DO
      END IF

      ! lateral boundary conditions -------------------------
      ! VS14 ----- grid of codes (types)
      NDUM = MAX(NVSLF, NVSLH, NVSLG)

      IF (NDUM > 0) THEN
         CALL ALREAD(4, VSD, FID_logfile, ':VS14', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NLBTYP(IEL) = IDUM(IXY0 + IX)
            END DO
         END DO

         ! VS15 ----- grid of category numbers
         CALL ALREAD(4, VSD, FID_logfile, ':VS15', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NLBCAT(IEL) = MAX(1, IDUM(IXY0 + IX))
            END DO
         END DO

         ! VS16 ----- No. of lateral boundary categories (flow, head, and head gr
         ! with b.c/s set only on selected layers
         ! initialise arrays to default values for reading in time-series data
         DO ICAT = 1, NDUM
            NVSLFN(ICAT) = 0
            NVSLHN(ICAT) = 0
            NVSLGN(ICAT) = 0
         END DO

         NVSLFT = NVSLF
         NVSLHT = NVSLH
         NVSLGT = NVSLG

         CALL ALREAD(2, VSD, FID_logfile, ':VS16', 1, 1, 0, CDUM, IDUM, DUMMY)
         NLB = IDUM(1)

         DO ILB = 1, NLB
            ! VS16a ---- b.c. type, category, no. of layers
            CALL ALREAD(2, VSD, FID_logfile, ':VS16a', 3, 1, 0, CDUM, IDUM, DUMMY)
            ITYP = IDUM(1)
            ICAT = IDUM(2)
            NLDUM = IDUM(3)

            ! VS16b ---- layer numbers
            CALL ALREAD(2, VSD, FID_logfile, ':VS16b', NLDUM, 1, 0, CDUM, IDUM, DUMMY)

            IF (ITYP == 3) THEN
               NVSLFN(ICAT) = NLDUM
               NVSLFT = NVSLFT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLFL(I, ICAT) = IDUM(I)
               END DO
            END IF

            IF (ITYP == 4) THEN
               NVSLHN(ICAT) = NLDUM
               NVSLHT = NVSLHT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLHL(I, ICAT) = IDUM(I)
               END DO
            END IF

            IF (ITYP == 5) THEN
               NVSLGN(ICAT) = NLDUM
               NVSLGT = NVSLGT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLGL(I, ICAT) = IDUM(I)
               END DO
            END IF
         END DO
      END IF

      ! bottom boundary conditions --------------------------
      ! VS17 ----- grid of codes (types)
      NDUM = MAX(NVSBF, NVSBH, NVSBD)

      IF (NDUM > 0) THEN
         IF (total_no_links > 0 .AND. BEXBK) THEN
            CALL ALREAD(2, VSD, FID_logfile, ':VS17', total_no_links, 1, 1, CDUM, IDUM, DUMMY)
            DO IEL = 1, total_no_links
               NBBTYP(IEL) = IDUM(IEL)
               NBBTYP(total_no_links + IEL) = IDUM(IEL)
               NBBTYP(2*total_no_links + IEL) = IDUM(IEL)
            END DO
         END IF

         CALL ALREAD(4, VSD, FID_logfile, ':VS17', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NBBTYP(IEL) = IDUM(IXY0 + IX)
            END DO
         END DO

         ! VS18 ----- grid of category numbers
         IF (total_no_links > 0 .AND. BEXBK) THEN
            CALL ALREAD(2, VSD, FID_logfile, ':VS18', total_no_links, 1, 1, CDUM, IDUM, DUMMY)
            DO IEL = 1, total_no_links
               ICAT = MAX(1, IDUM(IEL))
               NBBCAT(IEL) = ICAT
               NBBCAT(total_no_links + IEL) = ICAT
               NBBCAT(2*total_no_links + IEL) = ICAT
            END DO
         END IF

         CALL ALREAD(4, VSD, FID_logfile, ':VS18', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NBBCAT(IEL) = MAX(1, IDUM(IXY0 + IX))
            END DO
         END DO
      END IF

      RETURN

      ! FORMAT statements
9000  FORMAT('Error in number of VSS layer elements. NELEM should be ', I4)
9020  FORMAT('Error reading VSS layers for element ', I4, '.')
9030  FORMAT('Soil type ', I4, ' not expected for soil property tables.')

   END SUBROUTINE VSREAD

END MODULE vs_input

