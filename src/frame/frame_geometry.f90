!> summary: Element and link indexing, plan dimensions, face lengths and processing order.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[FRIND]] constructs the element, link and bank indexing that
!> [[grid_topology]] holds — which element is at which grid coordinate, which
!> elements neighbour which faces, and where the link and bank blocks begin.
!> [[FRDIM]] then computes the plan dimensions, cell areas, face lengths and
!> the total catchment area into [[element_geometry]], and [[FRSORT]] maintains
!> the `ISORT` processing order.
!>
!> Everything downstream depends on this running first: the component setup
!> routines, the solvers and the output all index by element number.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed and standardised the FR frame, including impermeable-bed defaults, `BSOFT`, `TIM` migration to `AL_D`, result output, and hot-start/rescue handling. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the FR `.F` files into a single Fortran 90 module. |
!> | 2020-05 | SB | 4.5 | Added ZQ-module variables and support. |
!> | 2026-03 | SB | 4.6 | Added allocation-based initialisation, date-aware meteorological input, the outlet sediment/contaminant text series and the water-table output. |
!> | 2026-09-11 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> | 2026-09-20 | SvB | - | Removed the uncalled `FRLTL`; `OCLTL` is the same reader with a different code table. See docs/rename_functions_routines/README.md. |
!> @endhistory
MODULE frame_geometry

   USE MOD_PARAMETERS, ONLY: zero
   USE array_limits, ONLY: nelee, nxee, nyee
   USE element_geometry, ONLY: BWIDTH, CAREA, cellarea, DHF, DXIN, DXQQ, DYIN, DYQQ, ISORT, &
                              NBFACE, NXM1, NYM1, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMRF2, ICMXY, INGRID, NGDBGN, NX, NY
   USE channel_geometry, ONLY: BEXBK, CLENTH, CWIDTH, ICMBK, LINKNS
   USE run_control, ONLY: BEXOC
   USE legacy_retained, ONLY: NGRID
   USE file_units, ONLY: FID_logfile
   USE vs_state, ONLY: ZVSPSL
   USE oc_state, ONLY: LCODEX, LCODEY
   USE oc_indexing, ONLY: LINKNO
   USE oc_node_solver, ONLY: gethrf

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FRDIM, FRIND, FRSORT

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

END MODULE frame_geometry

