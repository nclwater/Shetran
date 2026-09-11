!> summary: Portable numeric-kind requests, text-buffer lengths, and legacy missing-value sentinels.
!> author: Sven Berendsen, Newcastle University
!> date: 2020-03-05
!>
!> `mod_parameters` centralizes the numeric kinds and character-buffer sizes
!> used by the newer SHETRAN infrastructure. `R8P`/`R4P` and `I8P` through
!> `I1P` use `selected_real_kind`/`selected_int_kind` to request minimum decimal
!> capabilities from the compiler. Their names do not guarantee a byte width
!> or a particular numeric kind value.
!>
!> `R_P` and `I_P` are project aliases currently set to `R8P` and `I4P`.
!> They do not change the processor's intrinsic default real or integer kinds.
!> The module is `PRIVATE` by default; all 51 parameters are named in its
!> explicit `PUBLIC` list.
!>
!> | Parameter group | Current use outside this module |
!> |:----------------|:--------------------------------|
!> | `R8P`, `I_P` | Numeric declarations across the `core/`, `util/` and `io/` modules, and [[zq_tables]]. |
!> | `LENGTH_FILEPATH`, `LENGTH_LINE` | Path, command-line, and diagnostic buffers in [[run_context]] and [[command_line]]. |
!> | `LENGTH_LINELONG` | Diagnostic-detail buffers in [[visualisation_read_parser]]. |
!> | `LENGTH_LINEVERYLONG`, `LENGTH_TEXT_R8P` | Initial capacity and per-value width of the dated meteorological record buffer in [[met_input]]. |
!> | Remaining kind, buffer, and `NAN_*` parameters | No named external consumer; retained public API. |
!>
!> These are internal compile-time constants and have no user-manual input
!> record.
!>
!> `marker999` is the end-of-input time sentinel produced by
!> [[timeseries_input:FINPUT]] and [[timeseries_input:HINPUT]]. The one-element integer and real
!> constants support legacy scalar/array checker interfaces; `vsmall` is the
!> strict absolute tolerance used by the zero/one comparison helpers, which is
!> why it is here rather than with any one component.
!>
!> The physical constants are gathered here so that a quantity used by more than
!> one component can be seen to be — or not to be — the same number in each. The
!> move preserved every value exactly, including the two pairs that disagree
!> (air density, water density, latent heat of vaporisation) and the three
!> spellings of gravitational acceleration. Their names carry the component they
!> came from precisely because the disagreement is unresolved;
!> `docs/rename/constants_review.md` records what each difference is worth and
!> which merges would be free.
!>
!> The `NAN_*` parameters are ordinary finite sentinel values, not IEEE NaNs.
!> Code must compare or recognize them explicitly; IEEE NaN propagation and
!> predicates do not apply. No current source outside this module uses them.
!>
!> @note
!> A `selected_*_kind` inquiry returns a negative value when the processor
!> cannot provide the requested model. This module has no fallback or explicit
!> guard; a consumer attempting to use such a result as a kind will fail to
!> compile. The requested models are available in the tested gfortran build.
!> @endnote
!>
!> @note
!> The original 2020 source attributes the kind-selection definitions to PENF
!> and records GPL, BSD-2-Clause, BSD-3-Clause, or MIT licensing. This preserves
!> that existing attribution without making a new provenance or licence
!> determination.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-03-05 | SB | - | Initial version. |
!> | 2026-03-28 | SvB | - | Converted the file documentation to FORD style as an example. |
!> | 2026-08-22 | SvB | - | Added `LENGTH_TEXT_R8P`; `LENGTH_LINEVERYLONG` is now used by the dated meteorological reader. |
!> | 2026-09-10 | SvB | - | Gained `sglobal`'s mathematical constants and every physical constant from `CONST_SY`, `ETmod`, `SMmod` and `OCmod2`, under the names in `docs/rename/constants_review.md`; see docs/rename/proposal.md. |
!> @endhistory
module mod_parameters

   implicit none

   PRIVATE

   PUBLIC :: R8P, R4P, R_P, I8P, I4P, I2P, I1P, I_P, LENGTH_FILEPATH, LENGTH_LINE, LENGTH_LINELONG, &
             LENGTH_LINEVERYLONG, LENGTH_TEXT_R8P, NAN_REAL_R8P, NAN_REAL_R_P, NAN_INT_I4P, &
             NAN_INT_I_P, marker999, imarker, izero, ione, izero1, ione1, zero, half, one, two, three, &
             five, vsmall, zero1, one1, L_VAPORISATION_ET, PSYCHROMETRIC_CONSTANT, RHO_AIR_ET, &
             CP_AIR_ET, TWO_THIRDS, FIVE_THIRDS, SQRT_TWO_G, GRAVITY, RHO_SEDIMENT, RHO_WATER_SEDIMENT, &
             NU_WATER, RHO_AIR_SNOW, RHO_WATER_SNOW, CP_AIR_SNOW, CP_WATER, CP_ICE, L_FUSION, &
             L_VAPORISATION_SNOW, GROUND_HEAT_FLUX_SNOW

   ! KIND constants -----------------------------------------------------------
   integer, parameter :: R8P = selected_real_kind(15,307) !! Real kind with precision >= 15 and range >= 307.
   integer, parameter :: R4P = selected_real_kind(6,37)   !! Real kind with precision >= 6 and range >= 37.
   integer, parameter :: R_P = R8P                        !! Project-selected real-kind alias, currently `R8P`.

   integer, parameter :: I8P = selected_int_kind(18) !! Integer kind with decimal range >= 18.
   integer, parameter :: I4P = selected_int_kind(9)  !! Integer kind with decimal range >= 9.
   integer, parameter :: I2P = selected_int_kind(4)  !! Integer kind with decimal range >= 4.
   integer, parameter :: I1P = selected_int_kind(2)  !! Integer kind with decimal range >= 2.
   integer, parameter :: I_P = I4P                   !! Project-selected integer-kind alias, currently `I4P`.


   ! String constants ---------------------------------------------------------
   integer(kind=I_P), parameter :: LENGTH_FILEPATH     =    260 !! Project path buffer; classic Windows `MAX_PATH` size.
   integer(kind=I_P), parameter :: LENGTH_LINE         =    256 !! Short text and diagnostic buffer length.
   integer(kind=I_P), parameter :: LENGTH_LINELONG     =  16384 !! Long text buffer; used for parser diagnostics.
   integer(kind=I_P), parameter :: LENGTH_LINEVERYLONG = 262144 !! Very-long text buffer; upper bound for the dated meteorological record buffer in [[met_input]].
   integer(kind=I_P), parameter :: LENGTH_TEXT_R8P     =     26 !! Characters reserved per free-format `R8P` value, including its separator.


   ! NaN equivalents ----------------------------------------------------------
   real(kind=R8P), parameter    :: NAN_REAL_R8P = -9.35d30     !! Finite missing-value sentinel stored as `R8P`.
   real(kind=R_P), parameter    :: NAN_REAL_R_P = NAN_REAL_R8P !! Same sentinel stored with project real kind `R_P`.
   integer(kind=I_P), parameter :: NAN_INT_I4P  = -9999        !! Finite integer sentinel; `I_P` currently equals `I4P`.
   integer(kind=I_P), parameter :: NAN_INT_I_P  = NAN_INT_I4P  !! Alias of `NAN_INT_I4P` for project integer kind `I_P`.


   ! Mathematical and numerical constants ------------------------------------
   REAL(KIND=R8P), PARAMETER :: marker999 = 999999.9_R8P !! End-of-input time sentinel returned by `FINPUT` and `HINPUT` [h].
   INTEGER(KIND=I_P), PARAMETER :: imarker = INT(marker999) !! Truncated sentinel used internally by `eqmarker`.
   INTEGER(KIND=I_P), PARAMETER :: izero = 0 !! Integer zero constant.
   INTEGER(KIND=I_P), PARAMETER :: ione = 1 !! Integer one constant.
   INTEGER(KIND=I_P), PARAMETER, DIMENSION(1) :: izero1 = [0] !! One-element integer-zero vector for checker calls.
   INTEGER(KIND=I_P), PARAMETER, DIMENSION(1) :: ione1 = [1] !! One-element integer-one vector for checker calls.
   REAL(KIND=R8P), PARAMETER :: zero = 0.0_R8P !! `R8P` zero constant.
   REAL(KIND=R8P), PARAMETER :: half = 0.5_R8P !! `R8P` one-half constant.
   REAL(KIND=R8P), PARAMETER :: one = 1.0_R8P !! `R8P` one constant.
   REAL(KIND=R8P), PARAMETER :: two = 2.0_R8P !! `R8P` two constant.
   REAL(KIND=R8P), PARAMETER :: three = 3.0_R8P !! `R8P` three constant.
   REAL(KIND=R8P), PARAMETER :: five = 5.0_R8P !! `R8P` five constant.
   REAL(KIND=R8P), PARAMETER :: vsmall = 1.0e-20_R8P !! Strict absolute tolerance used by zero/one comparison helpers.
   REAL(KIND=R8P), PARAMETER, DIMENSION(1) :: zero1 = [0.0_R8P] !! One-element `R8P` zero vector for checker calls.
   REAL(KIND=R8P), PARAMETER, DIMENSION(1) :: one1 = [1.0_R8P] !! One-element `R8P` one vector for checker calls.

   ! Physical constants -------------------------------------------------------
   ! Values are carried over unchanged from the component modules that used to
   ! declare them. Where two components disagree about the same quantity the two
   ! values are both kept, under names that say which is which; see
   ! docs/rename/constants_review.md.

   ! Evapotranspiration; from ETmod.
   DOUBLEPRECISION, PARAMETER :: L_VAPORISATION_ET = 2465000. !! Latent heat of vaporisation used by the Penman equations (J/kg).
   DOUBLEPRECISION, PARAMETER :: PSYCHROMETRIC_CONSTANT = 0.659 !! Psychrometric constant used with `DEL` (mb/degree C).
   DOUBLEPRECISION, PARAMETER :: RHO_AIR_ET = 1.2 !! Fixed air density (kg/m3).
   DOUBLEPRECISION, PARAMETER :: CP_AIR_ET = 1003. !! Fixed specific heat capacity of air (J/kg/degree C).

   ! Overland and channel flow; from OCmod2.
   DOUBLEPRECISION, PARAMETER   :: TWO_THIRDS = 2.0D0/3.0D0      !! Exponent \(2/3\) used in Strickler conveyance.
   DOUBLEPRECISION, PARAMETER   :: FIVE_THIRDS = 5.0D0/3.0D0      !! Exponent factor \(5/3\) used by the implemented derivative branches.
   DOUBLEPRECISION, PARAMETER   :: SQRT_TWO_G = 4.42944d0   !! Approximation to \(\sqrt{2g}\) for weir flow.

   ! Sediment transport; from CONST_SY.
   DOUBLEPRECISION, PARAMETER :: GRAVITY = 9.80665d0 !! Gravitational acceleration in metres per second squared.
   DOUBLEPRECISION, PARAMETER :: RHO_SEDIMENT = 2650.0d0  !! Representative sediment-particle density in kilograms per cubic metre.
   DOUBLEPRECISION, PARAMETER :: RHO_WATER_SEDIMENT = 998.0d0   !! Representative water density in kilograms per cubic metre.
   DOUBLEPRECISION, PARAMETER :: NU_WATER = 1.0D-6    !! Representative water kinematic viscosity in square metres per second.

   ! Snow accumulation and melt; from SMmod.
   DOUBLEPRECISION, PARAMETER :: RHO_AIR_SNOW = 1.29d0      !! Density of air (kg/m^3).
   DOUBLEPRECISION, PARAMETER :: RHO_WATER_SNOW = 1000.0d0    !! Density of water (kg/m^3).
   DOUBLEPRECISION, PARAMETER :: CP_AIR_SNOW = 1003.0d0     !! Specific heat of air at constant pressure (J/kg/C).
   DOUBLEPRECISION, PARAMETER :: CP_WATER = 4187.0d0     !! Specific heat of water (J/kg/C).
   DOUBLEPRECISION, PARAMETER :: CP_ICE = 2093.0d0     !! Specific heat of ice (J/kg/C).
   DOUBLEPRECISION, PARAMETER :: L_FUSION = 334000.0d0   !! Latent heat of fusion (J/kg).
   DOUBLEPRECISION, PARAMETER :: L_VAPORISATION_SNOW = 2500000.0d0  !! Latent heat of vaporisation (J/kg).
   DOUBLEPRECISION, PARAMETER :: GROUND_HEAT_FLUX_SNOW = 2.0d0        !! Ground heat flux to snow (W/m^2).

end module mod_parameters
