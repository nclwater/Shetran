!> summary: Channel cross-section tables and the conveyance curve built from them.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[OCXS]] takes the tabulated cross-section input — width and depth at
!> `NXSECT` sample points per section category — and tabulates depth,
!> conveyance and conveyance slope into [[oc_state]]'s `xstab`, which the
!> discharge routines then interpolate rather than re-evaluating the
!> cross-section geometry per timestep. `XAFULL` is the bankfull area.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod, OCQDQMOD; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_cross_sections

   USE MOD_PARAMETERS, ONLY: half, zero
   USE array_limits, ONLY: nlfee, NOCTAB, NXSCEE
   USE element_geometry, ONLY: total_no_links
   USE channel_geometry, ONLY: CWIDTH, ZBEFF, ZBFULL
   USE oc_state, ONLY: STRXX, xstab
   USE oc_conveyance, ONLY: CONVEYAN

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCXS
   PUBLIC :: NXSECT, XAFULL, XAREA, XINH, XINW


   ! Channel cross-section tables (see [[ocxs]])
   DOUBLEPRECISION    :: XAFULL(NLFEE)     !! Full-flow cross-sectional area for each channel link.
   INTEGER            :: NXSECT(NLFEE)         !! Number of width-depth cross-section points for each channel link.
   DOUBLEPRECISION    :: XINH(NLFEE, NOCTAB)   !! Channel cross-section depths above bed.
   DOUBLEPRECISION    :: XINW(NLFEE, NOCTAB)   !! Channel cross-section widths.
   DOUBLEPRECISION    :: XAREA(NLFEE, NOCTAB)  !! Integrated channel cross-section areas.

CONTAINS

!> @brief Builds channel cross-section area and conveyance lookup tables.
!>
!> `OCXS` integrates tabulated width-depth pairs to water area, derives an
!> effective bed elevation for full-bank storage, and fills `XSTAB` with
!> uniformly spaced depth, conveyance, and conveyance-derivative values used
!> by the OC flow calculation.
!>
!> The SHETRAN User Guide and Data Input Manual defines the channel
!> cross-section data in records `OC30`-`OC34`: each cross-section category
!> is supplied as width/depth pairs (`XDEFW`, `XDEFH`), the first depth must
!> be zero, and the final depth defines the bankfull depth. The manual also
!> states that channel flow uses the user-supplied cross-section, while
!> subsurface-flow exchange uses an effective rectangular channel with the
!> same cross-sectional area.
!>
!> In the code these manual fields are stored per link as `XINW(link,j)` and
!> `XINH(link,j)`, with `NXSECT(link)` width/depth pairs and roughness
!> `STRXX(link)`. Entry requirements retained from the legacy routine are:
!> at least one active link, `NXSCEE >= 2`, positive channel widths
!> `CWIDTH(1:total_no_links)`, `NXSECT` values within the allocated
!> `XINH`/`XINW`/`XAREA` table sizes, a positive final tabulated depth, and
!> strictly increasing tabulated depths within each link.
!>
!> Input width-depth pairs are integrated by the trapezoidal rule. For
!> tabulated level \(j\),
!>
!> \[
!> XAREA_j = XAREA_{j-1}
!> + \frac{1}{2}\left(XINW_j+XINW_{j-1}\right)
!>   \left(XINH_j-XINH_{j-1}\right),
!> \]
!>
!> with `XAREA(:,1)=0`. The manual's effective rectangular-channel statement
!> is implemented by shifting the effective bed elevation so that a
!> rectangle of width `CWIDTH` has the same bankfull area as the tabulated
!> cross-section:
!>
!> \[
!> ZBEFF = ZBFULL - XAREA_N/CWIDTH.
!> \]
!>
!> The same bankfull area is retained per link as
!> `XAFULL(link) = XAREA(link,NXSECT(link))` for [[oc_stage_discharge:OCQDQ]]. It
!> depends only on the cross-section tables, so it is built here once rather
!> than on the first [[ocsim]] call.
!>
!> The lookup table `XSTAB` supports the OC flow calculation without
!> repeatedly integrating the irregular cross-section. It has rows:
!>
!> | `XSTAB` row | Meaning |
!> |:------------|:--------|
!> | 1 | Uniformly spaced water depth. |
!> | 2 | Conveyance at that depth. |
!> | 3 | Piecewise-linear derivative of conveyance with respect to depth. |
!>
!> For a table with `NXSCEE` rows, the uniform depth increment is
!>
!> \[
!> \Delta h = XINH_N/(NXSCEE-1),\qquad h_j=(j-1)\Delta h.
!> \]
!>
!> For each lookup depth \(h_j\), the enclosing manual input interval
!> \(H_i \le h_j \le H_{i+1}\) is found. Width is treated as linearly
!> varying between the two tabulated width/depth points:
!>
!> \[
!> \alpha=\frac{h_j-H_i}{H_{i+1}-H_i},
!> \]
!>
!> and the area increment above \(H_i\) is evaluated as a trapezoid:
!>
!> \[
!> A_j = XAREA_i
!> + \frac{1}{2}\left((2-\alpha)W_i+\alpha W_{i+1}\right)
!>   (h_j-H_i).
!> \]
!>
!> `CONVEYAN` converts \(A_j\), depth, and roughness `STRXX` into
!> conveyance. For `OCXS` it is called with `ty=0`, so the main branch used
!> away from near-zero depth is the Gauckler-Manning-Strickler-style
!> relation implemented in [[oc_conveyance:CONVEYAN]]:
!>
!> \[
!> C_j = STRXX\,A_j\,h_j^{2/3}.
!> \]
!>
!> For \(10^{-9} \le h_j < 10^{-3}\) m, the code uses the smoothed
!> polynomial branch in `CONVEYAN` for automatic-differentiation stability:
!>
!> \[
!> C_j = STRXX\,A_j\,\frac{10}{3}\,h_j(4-1000h_j),
!> \]
!>
!> and for smaller depths it returns zero conveyance. The stored derivative
!> in `XSTAB` is not the derivative returned by `CONVEYAN`; `OCXS` stores
!> the finite-difference slope for interval `j-1`:
!>
!> \[
!> XSTAB_{3,j-1} = \frac{C_j-C_{j-1}}{\Delta h},
!> \]
!>
!> so `XSTAB(2,j) + XSTAB(3,j)*(h-XSTAB(1,j))` is continuous and
!> piecewise linear in water depth. As in the legacy routine, conveyance and
!> derivative entries for the final lookup row are not defined.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-03 | RAH | 4.2 | Created this routine, taking it from part of [[OCPLF]]. |
!> | 1998-03-17 | RAH | 4.2 | Fixed the `XAJ` inaccuracy so the stored conveyance derivative is continuous. |
!> | 1998-04-24 | RAH | 4.2 | Merged the legacy `XSECTH`/`XCONV`/`XDERIV` arrays into `XSTAB`. |
!> | 2026-08-18 | SvB | 4.6.5 | Moved the `XAFULL` setup here from the first-call branch of [[ocsim]], reusing this routine's existing per-link loop. |
!> @endhistory
   SUBROUTINE OCXS()

      IMPLICIT NONE

      INTEGER         :: I, IELr, J, N
      DOUBLE PRECISION :: ALPHA, DH, HI, HIP1, HJ, STEPH, STR, W2, XAJ, XCJ, XCJM1, adumy

      !----------------------------------------------------------------------*

      link_loop: DO ielr = 1, total_no_links
         !
         ! LOCAL VARIABLES
         !
         N = NXSECT(ielr)
         STR = STRXX(ielr)

         !
         ! SET UP CROSS-SECTIONAL AREAS FOR EACH OF THE INPUT LEVELS
         !
         XAREA(ielr, 1) = zero

         area_loop: DO J = 2, N
            W2 = XINW(ielr, J) + XINW(ielr, J - 1)
            DH = XINH(ielr, J) - XINH(ielr, J - 1)
            XAREA(ielr, J) = XAREA(ielr, J - 1) + W2*DH*half
         END DO area_loop

         !
         ! EFFECTIVE BED ELEVATION
         !
         ZBEFF(ielr) = ZBFULL(ielr) - XAREA(ielr, N)/CWIDTH(ielr)

         !
         ! FULL-FLOW AREA FOR OCQDQ: the top row of this link's area table
         !
         XAFULL(ielr) = XAREA(ielr, N)

         !
         ! SET UP FULL CROSS-SECTION TABLES OF HEIGHT, CONVEYANCE & DERIVATIVE
         !
         ! NOTE: The formulation is such that
         !             XSTAB(2,j,ielr) + XSTAB(3,j,ielr)*( h - XSTAB(1,j,ielr) )
         !       is a continuous (piecewise linear) function of h
         !
         I = 1
         HI = XINH(ielr, I)
         STEPH = XINH(ielr, N)/(NXSCEE - 1.0d0)
         XCJ = zero
         XSTAB(1, 1, ielr) = zero

         table_loop: DO J = 2, NXSCEE
            XCJM1 = XCJ
            HJ = STEPH*(J - 1)

            ! Advance index I until we bracket the target height HJ
            search_loop: DO
               HIP1 = XINH(ielr, I + 1)
               IF (I >= N - 1 .OR. HIP1 >= HJ) EXIT search_loop
               I = I + 1
               HI = HIP1
            END DO search_loop

            DH = HJ - HI
            ALPHA = DH/(HIP1 - HI)
            W2 = (2.0d0 - ALPHA)*XINW(ielr, I) + ALPHA*XINW(ielr, I + 1)
            XAJ = XAREA(ielr, I) + W2*DH*half

            ! XCJ = STR * XAJ * HJ**TWO_THIRDS
            CALL CONVEYAN(str, hj, xcj, adumy, 0, xaj)

            XSTAB(1, J, ielr) = HJ
            XSTAB(2, J - 1, ielr) = XCJM1
            XSTAB(3, J - 1, ielr) = (XCJ - XCJM1)/STEPH
         END DO table_loop

      END DO link_loop

   END SUBROUTINE OCXS

END MODULE oc_cross_sections

