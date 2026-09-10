!> summary: The static nitrate data and the scheduled nitrogen and carbon additions.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> [[MNRED1]] and [[MNRED2]] read the process constants and spatial fields from
!> records `MN11`--`MN60` of the main nitrate-data file; [[MNINT2]] reads the
!> scheduled inorganic-nitrogen and organic-carbon additions from `MNFN` and
!> `MNFC`. See the User Guide's *Nitrate component data input* section for the
!> record definitions and units.
!>
!> @note
!> [[MNINT2]] hard-codes the mobile-water uptake fraction `PPHI` to 0.5.
!> @endnote
!>
!> @warning
!> [[MNRED1]] leaves `Q10M` and `Q10N` undefined when Q10 mode is disabled,
!> although [[mn_validation:MNERR2]] still checks them.
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
MODULE mn_input

   USE array_limits, ONLY: nyee
   USE error_reporting, ONLY: RAISE_ERROR
   USE datetime, ONLY: hour_from_date
   USE record_readers, ONLY: ALRED2, ALREDC, ALREDF, ALREDI, ALREDL
   USE spatial_fields, ONLY: ALALLF, ALALLI
   USE mn_state, ONLY: cahum, calit, caman, chum, chum1, clit, clit1, cman, cman1, naamm, &
                       namm, namm1, nanit, ndnit, ndsnt, nlit, nlit1, nman, nman1, pphi

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnint2, mnred1, mnred2

CONTAINS

!> @brief Converts time-varying MN inputs into cell-based process rates.
!>
!> `mnint2` carries forward previous pool values, dimensionalises nitrate
!> concentrations, assigns mobile fractions, distributes mineral and organic
!> additions over the specified banding depth, and adds wet/dry deposition to
!> the top active cell.
!>
!> The time-varying inputs come from the manual's external carbon (`MNFC`) and
!> external inorganic nitrogen/fertilizer (`MNFN`) files. Effective rainfall is
!> converted from SHETRAN flow units to millimetres per second as
!> `Pnet_mm = 1000 * PNETTO`, and dimensionless nitrate concentrations are
!> dimensionalised using the `MN14` reference concentration:
!>
!> \[
!> N_d = C\,MNCREF,\qquad N_s = S\,MNCREF.
!> \]
!>
!> The mobile-water nitrate fraction `PPHI` is currently assigned a fixed value
!> of `0.500` in every active cell; the previous call to the `PHI` function is
!> still present only as a comment.
!>
!> For an inorganic nitrogen addition with total `NTOT`, ammonium fraction
!> `NAMFCT`, banding depth `NDPTHB`, cell thickness `\Delta z`, and timestep
!> `\Delta t`, the top-cell-only case (`NDPTHB = 0`) uses
!>
!> \[
!> N_{amm}^{add} = \frac{NTOT\,NAMFCT}{\Delta z_{top}\Delta t},\qquad
!> N_{nit}^{add} = \frac{NTOT(1-NAMFCT)}{\Delta z_{top}\Delta t}.
!> \]
!>
!> When `NDPTHB > 0`, cells fully inside the band use `NDPTHB` in place of
!> `\Delta z_{top}`. The cell cut by the banding depth is multiplied by
!> \(f = d_{overlap}/NDPTHB\) and divided by that cell's own `\Delta z`; cells
!> below the band receive zero addition.
!>
!> Organic carbon additions use the same banding logic with `CTOT`, `CDPTHB`,
!> `CLTFCT`, and `CMNFCT`:
!>
!> \[
!> C_{lit}^{add} = \frac{CTOT\,CLTFCT}{D\Delta t},\quad
!> C_{man}^{add} = \frac{CTOT\,CMNFCT}{D\Delta t},\quad
!> C_{hum}^{add} = \frac{CTOT(1-CLTFCT-CMNFCT)}{D\Delta t},
!> \]
!>
!> where `D` is the top-cell thickness, the banding depth, or the partially
!> overlapped cell thickness with the overlap fraction applied. If no organic
!> carbon is active for an element, `CNRALT` and `CNRAMN` are set to `999.0` and
!> the carbon-addition rates are zeroed. Dry and wet deposition are finally
!> added to the top cell as
!>
!> \[
!> N_{amm}^{dep} = \frac{AMMDDR + AMMWDR\,Pnet_{mm}}{\Delta z_{top}},\qquad
!> N_{nit}^{dep} = \frac{NITDDR + NITWDR\,Pnet_{mm}}{\Delta z_{top}}.
!> \]
   SUBROUTINE MNINT2(LLEE, NCETOP, NEL, NELEE, NLF, NLYREE, NCOLMB, NLYR, NLYRBT, NTSOIL, AMMDDR, AMMWDR, MNCREF, NITDDR, NITWDR, &
      DELTAZ, DTUZ, CCCC, CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, &
      PNETTO, SSSS, VSTHE, ISADDC, ISADDN, CNRALT, CNRAMN, DUMMY)

      IMPLICIT NONE

      ! Input arguments
      ! * stationary
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: NLYREE  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: NLYR(NELEE)  !! Number of soil layers in each element.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: AMMDDR  !! Dry ammonium deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: AMMWDR  !! Wet ammonium deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(IN) :: NITDDR  !! Dry nitrate deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: NITWDR  !! Wet nitrate deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.

      ! * time dependent
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: CDPTHB(NLF + 1:NEL)  !! Carbon banding depth.
      DOUBLE PRECISION, INTENT(IN) :: CLTFCT(NLF + 1:NEL)  !! Litter fraction of added carbon.
      DOUBLE PRECISION, INTENT(IN) :: CMNFCT(NLF + 1:NEL)  !! Manure fraction of added carbon.
      DOUBLE PRECISION, INTENT(IN) :: CNRAL(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added litter.
      DOUBLE PRECISION, INTENT(IN) :: CNRAM(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added manure.
      DOUBLE PRECISION, INTENT(IN) :: CTOT(NLF + 1:NEL)  !! Total external carbon addition.
      DOUBLE PRECISION, INTENT(IN) :: NAMFCT(NLF + 1:NEL)  !! Ammonium fraction of added inorganic nitrogen.
      DOUBLE PRECISION, INTENT(IN) :: NDPTHB(NLF + 1:NEL)  !! Nitrogen banding depth.
      DOUBLE PRECISION, INTENT(IN) :: NTOT(NLF + 1:NEL)  !! Total external inorganic nitrogen addition.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NELEE)  !! Net precipitation/effective rainfall by element.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      LOGICAL, INTENT(IN) :: ISADDC  !! True when a carbon-addition event is active.
      LOGICAL, INTENT(IN) :: ISADDN  !! True when a nitrogen-addition event is active.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CNRALT(NELEE)  !! Element litter C:N ratio for active additions.
      DOUBLE PRECISION, INTENT(OUT) :: CNRAMN(NELEE)  !! Element manure C:N ratio for active additions.

      ! Workspace
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace.

      ! Locals etc.
      INTEGER :: JLYR, JSOIL, NCEBOT, NCE, NCL, NELM
      DOUBLE PRECISION :: FRACDP, KSPTOT

      !-------------------------------------------------------------------*

      ! 1. set old concentrations to new values
      ! ---------------------------------------
      col_init_loop: DO NELM = NLF + 1, NEL

         DO NCL = NCOLMB(NELM), NCETOP
            CMAN(NELM, NCL) = CMAN1(NELM, NCL)
            NMAN(NELM, NCL) = NMAN1(NELM, NCL)
            CLIT(NELM, NCL) = CLIT1(NELM, NCL)
            CHUM(NELM, NCL) = CHUM1(NELM, NCL)
            NLIT(NELM, NCL) = NLIT1(NELM, NCL)
            NAMM(NELM, NCL) = NAMM1(NELM, NCL)
         END DO

         ! 2. calculate the effective rain on the ground surface in mm s-1
         ! ----------------------------------------------------------------
         DUMMY(NELM) = PNETTO(NELM)*1.0D3

         ! 3. convert nitrate concentrations from non dimensional units
         ! ------------------------------------------------------------
         DO NCL = NCOLMB(NELM), NCETOP
            NDNIT(NELM, NCL) = CCCC(NELM, NCL)*MNCREF
            NDSNT(NELM, NCL) = SSSS(NELM, NCL)*MNCREF
         END DO

         ! 4. calculation of the mobile fraction for every element in every cell
         ! ---------------------------------------------------------------------
         NCEBOT = NCOLMB(NELM)
         DO JLYR = 1, NLYR(NELM)
            JSOIL = NTSOIL(NELM, JLYR)
            DO NCL = MAX(NCEBOT, NLYRBT(NELM, JLYR)), NLYRBT(NELM, JLYR + 1) - 1
               ! sb 240925 set value to 0.5 (which is the value set in cmmod.f90 in function phi
               PPHI(NELM, NCL) = 0.500D0
            END DO
         END DO

      END DO col_init_loop

      ! 5. addition of nitrate and ammonium for each element in each cell
      ! -----------------------------------------------------------------
      IF (ISADDN) THEN
         col_nitrate_loop: DO NELM = NLF + 1, NEL

            IF (NTOT(NELM) > 0.0D0) THEN

               ! * there is no banding of the input and only the top cell
               ! * receives fertiliser
               IF (NDPTHB(NELM) == 0.0D0) THEN
                  NAAMM(NELM, NCETOP) = NTOT(NELM)*NAMFCT(NELM)/(DELTAZ(NCETOP, NELM)*DTUZ)
                  NANIT(NELM, NCETOP) = NTOT(NELM)*(1.0D0 - NAMFCT(NELM))/(DELTAZ(NCETOP, NELM)*DTUZ)
                  DO NCE = NCOLMB(NELM), NCETOP - 1
                     NAAMM(NELM, NCE) = 0.0D0
                     NANIT(NELM, NCE) = 0.0D0
                  END DO

                  ! * there is banding of the input
               ELSE
                  KSPTOT = 0.0D0
                  DO NCE = NCETOP, NCOLMB(NELM), -1
                     KSPTOT = KSPTOT + DELTAZ(NCE, NELM)
                     ! * the banding depth is to below this element
                     IF (KSPTOT <= NDPTHB(NELM)) THEN
                        NAAMM(NELM, NCE) = NTOT(NELM)*NAMFCT(NELM)/(NDPTHB(NELM)*DTUZ)
                        NANIT(NELM, NCE) = NTOT(NELM)*(1.0D0 - NAMFCT(NELM))/(NDPTHB(NELM)*DTUZ)
                        ! * the banding depth is to within this element
                     ELSE IF ((KSPTOT - DELTAZ(NCE, NELM)) <= NDPTHB(NELM)) THEN
                        FRACDP = (NDPTHB(NELM) - KSPTOT + DELTAZ(NCE, NELM))/NDPTHB(NELM)
                        NAAMM(NELM, NCE) = NTOT(NELM)*NAMFCT(NELM)*FRACDP/(DELTAZ(NCE, NELM)*DTUZ)
                        NANIT(NELM, NCE) = NTOT(NELM)*(1.0D0 - NAMFCT(NELM))*FRACDP/(DELTAZ(NCE, NELM)*DTUZ)
                        ! * the depth of the element is below the banding depth
                     ELSE
                        NAAMM(NELM, NCE) = 0.0D0
                        NANIT(NELM, NCE) = 0.0D0
                     END IF
                  END DO
               END IF

            ELSE
               DO NCE = NCOLMB(NELM), NCETOP
                  NAAMM(NELM, NCE) = 0.0D0
                  NANIT(NELM, NCE) = 0.0D0
               END DO
            END IF
         END DO col_nitrate_loop
      ELSE
         zero_nitrate_loop: DO NELM = NLF + 1, NEL
            DO NCE = NCOLMB(NELM), NCETOP
               NAAMM(NELM, NCE) = 0.0D0
               NANIT(NELM, NCE) = 0.0D0
            END DO
         END DO zero_nitrate_loop
      END IF

      ! 6. addition of organic matter for each element in each cell
      ! -----------------------------------------------------------
      IF (ISADDC) THEN
         col_organic_loop: DO NELM = NLF + 1, NEL

            IF (CTOT(NELM) > 0.0D0) THEN
               CNRALT(NELM) = CNRAL(NELM)
               CNRAMN(NELM) = CNRAM(NELM)

               ! * there is no banding of the input and only the top cell receives fertiliser
               IF (CDPTHB(NELM) == 0.0D0) THEN
                  CALIT(NELM, NCETOP) = CTOT(NELM)*CLTFCT(NELM)/(DELTAZ(NCETOP, NELM)*DTUZ)
                  CAMAN(NELM, NCETOP) = CTOT(NELM)*CMNFCT(NELM)/(DELTAZ(NCETOP, NELM)*DTUZ)
                  CAHUM(NELM, NCETOP) = CTOT(NELM)*(1.0D0 - CLTFCT(NELM) - CMNFCT(NELM))/(DELTAZ(NCETOP, NELM)*DTUZ)
                  DO NCE = NCOLMB(NELM), NCETOP - 1
                     CALIT(NELM, NCE) = 0.0D0
                     CAMAN(NELM, NCE) = 0.0D0
                     CAHUM(NELM, NCE) = 0.0D0
                  END DO

                  ! * there is banding of the input
               ELSE
                  KSPTOT = 0.0D0
                  DO NCE = NCETOP, NCOLMB(NELM), -1
                     KSPTOT = KSPTOT + DELTAZ(NCE, NELM)
                     ! * the banding depth is to below this element
                     IF (KSPTOT <= CDPTHB(NELM)) THEN
                        CALIT(NELM, NCE) = CTOT(NELM)*CLTFCT(NELM)/(CDPTHB(NELM)*DTUZ)
                        CAMAN(NELM, NCE) = CTOT(NELM)*CMNFCT(NELM)/(CDPTHB(NELM)*DTUZ)
                        CAHUM(NELM, NCE) = CTOT(NELM)*(1.0D0 - CLTFCT(NELM) - CMNFCT(NELM))/(CDPTHB(NELM)*DTUZ)
                        ! * the banding depth is to within this element
                     ELSE IF ((KSPTOT - DELTAZ(NCE, NELM)) <= CDPTHB(NELM)) THEN
                        FRACDP = (CDPTHB(NELM) - (KSPTOT - DELTAZ(NCE, NELM)))/CDPTHB(NELM)
                        CALIT(NELM, NCE) = CTOT(NELM)*CLTFCT(NELM)*FRACDP/(DELTAZ(NCE, NELM)*DTUZ)
                        CAMAN(NELM, NCE) = CTOT(NELM)*CMNFCT(NELM)*FRACDP/(DELTAZ(NCE, NELM)*DTUZ)
                        CAHUM(NELM, NCE) = CTOT(NELM)*(1.0D0 - CLTFCT(NELM) - CMNFCT(NELM))*FRACDP/(DELTAZ(NCE, NELM)*DTUZ)
                        ! * the depth of the element is below the banding depth
                     ELSE
                        CALIT(NELM, NCE) = 0.0D0
                        CAMAN(NELM, NCE) = 0.0D0
                        CAHUM(NELM, NCE) = 0.0D0
                     END IF
                  END DO
               END IF

            ELSE
               ! * set to 999 to avoid divide by zero errors
               CNRALT(NELM) = 999.0D0
               CNRAMN(NELM) = 999.0D0
               DO NCE = NCOLMB(NELM), NCETOP
                  CALIT(NELM, NCE) = 0.0D0
                  CAMAN(NELM, NCE) = 0.0D0
                  CAHUM(NELM, NCE) = 0.0D0
               END DO
            END IF
         END DO col_organic_loop

      ELSE
         zero_organic_loop: DO NELM = NLF + 1, NEL
            ! * set to 999 to avoid divide by zero errors
            CNRALT(NELM) = 999.0D0
            CNRAMN(NELM) = 999.0D0
            DO NCE = NCOLMB(NELM), NCETOP
               CALIT(NELM, NCE) = 0.0D0
               CAMAN(NELM, NCE) = 0.0D0
               CAHUM(NELM, NCE) = 0.0D0
            END DO
         END DO zero_organic_loop
      END IF

      ! 7. addition of wet and dry deposition on fertilizer rate
      ! --------------------------------------------------------
      depo_loop: DO NELM = NLF + 1, NEL
         NAAMM(NELM, NCETOP) = NAAMM(NELM, NCETOP) + AMMDDR/DELTAZ(NCETOP, NELM) + AMMWDR*DUMMY(NELM)/DELTAZ(NCETOP, NELM)
         NANIT(NELM, NCETOP) = NANIT(NELM, NCETOP) + NITDDR/DELTAZ(NCETOP, NELM) + NITWDR*DUMMY(NELM)/DELTAZ(NCETOP, NELM)
      END DO depo_loop

   END SUBROUTINE MNINT2

!> @brief Reads static mineral nitrogen input data.
!>
!> `mnred1` reads the MND file once during [[mninitialise]], echoes the
!> nitrate title to `MNPR`, and fills the static parameter arrays that are later
!> validated by [[mnerr2]] and interpolated by [[mninit]].
!>
!> | Records | Data read |
!> | --- | --- |
!> | `MN11`-`MN14` | Ammonium/nitrate immobilisation and plant-uptake constants, organic-matter fractions and C:N ratios, dry/wet deposition rates, and `MNCREF`. |
!> | `MN15`-`MN28` | Category assignments and depth/value tables for `KHUM`, `KLIT`, `KMAN`, `KNIT`, `KVOL`, `KD1`, and `KD2`. Each category count must be in `1:NMNEEE` and each table length in `1:MNMTEE`; failures are fatal errors `3090` and `3091`. |
!> | `MN30`-`MN31` | Soil ammonium adsorption factor `KDDSOL(soil)` and power `GNN`. |
!> | `MN35`-`MN35a` | Q10 temperature-response flag `ISQ10`; `Q10M` and `Q10N` are read only when `ISQ10` is true. |
!> | `MN40`-`MN46` | Initial-carbon mode. If `ISICCD` is true, read decay-profile inputs `CTOTTP` and `DCHLF`; otherwise read category/profile tables `CELEM`, `CCONC`, and `CDPTH`. `CLITFR` and `CNRLIT` are always read. |
!> | `MN50`-`MN54` | Initial-ammonium mode. If `ISIAMD` is true, read decay-profile inputs `NAMTOP` and `DAMHLF`; otherwise read category/profile tables `NAELEM`, `NACONC`, and `NADPTH`. |
!> | `MN60` | Bottom cell `NBOTCE`, below which nitrogen transformations are not considered when it is valid for all columns. |
!>
!> Spatial category and profile fields are read with `ALALLI`/`ALALLF`, using the
!> grid, bank, and neighbour maps passed from the frame setup. The routine calls
!> `ALRED2` both before and after reading the MND file.
!>
!> @warning `Q10M` and `Q10N` are not assigned when `ISQ10` is false, although
!> [[mnerr2]] unconditionally reads and checks both values. Their values are
!> therefore undefined on that current-code path.
!> @endwarning
   SUBROUTINE MNRED1(MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, BEXBK, LINKNS, NBOTCE, &
      NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, &
      KMELEM, KNELEM, KVELEM, NAELEM, NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, &
      AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, NITWDR, Q10M, &
      Q10N, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, &
      KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ISICCD, ISIAMD, ISQ10, IDUM, DUMMY)

      USE array_limits, ONLY: nyee

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MND  !! Static MND input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! Output arguments
      INTEGER, INTENT(OUT) :: NBOTCE  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(OUT) :: NMN15E  !! Number of humus category entries.
      INTEGER, INTENT(OUT) :: NMN17E  !! Number of litter category entries.
      INTEGER, INTENT(OUT) :: NMN19E  !! Number of manure category entries.
      INTEGER, INTENT(OUT) :: NMN21E  !! Number of nitrification category entries.
      INTEGER, INTENT(OUT) :: NMN23E  !! Number of volatilisation category entries.
      INTEGER, INTENT(OUT) :: NMN25E  !! Number of KD1 denitrification category entries.
      INTEGER, INTENT(OUT) :: NMN27E  !! Number of KD2 denitrification category entries.
      INTEGER, INTENT(OUT) :: NMN43E  !! Number of initial-carbon category entries.
      INTEGER, INTENT(OUT) :: NMN53E  !! Number of initial-ammonium category entries.
      INTEGER, INTENT(OUT) :: CELEM(NLF + 1:NEL)  !! Initial-carbon category by element.
      INTEGER, INTENT(OUT) :: KD1ELM(NLF + 1:NEL)  !! KD1 denitrification category by element.
      INTEGER, INTENT(OUT) :: KD2ELM(NLF + 1:NEL)  !! KD2 denitrification category by element.
      INTEGER, INTENT(OUT) :: KHELEM(NLF + 1:NEL)  !! Humus decomposition category by element.
      INTEGER, INTENT(OUT) :: KLELEM(NLF + 1:NEL)  !! Litter decomposition category by element.
      INTEGER, INTENT(OUT) :: KMELEM(NLF + 1:NEL)  !! Manure decomposition category by element.
      INTEGER, INTENT(OUT) :: KNELEM(NLF + 1:NEL)  !! Nitrification category by element.
      INTEGER, INTENT(OUT) :: KVELEM(NLF + 1:NEL)  !! Volatilisation category by element.
      INTEGER, INTENT(OUT) :: NAELEM(NLF + 1:NEL)  !! Initial-ammonium category by element.
      INTEGER, INTENT(OUT) :: NMN15T(NMNEEE)  !! Humus table length by category.
      INTEGER, INTENT(OUT) :: NMN17T(NMNEEE)  !! Litter table length by category.
      INTEGER, INTENT(OUT) :: NMN19T(NMNEEE)  !! Manure table length by category.
      INTEGER, INTENT(OUT) :: NMN21T(NMNEEE)  !! Nitrification table length by category.
      INTEGER, INTENT(OUT) :: NMN23T(NMNEEE)  !! Volatilisation table length by category.
      INTEGER, INTENT(OUT) :: NMN25T(NMNEEE)  !! KD1 table length by category.
      INTEGER, INTENT(OUT) :: NMN27T(NMNEEE)  !! KD2 table length by category.
      INTEGER, INTENT(OUT) :: NMN43T(NMNEEE)  !! Initial-carbon table length by category.
      INTEGER, INTENT(OUT) :: NMN53T(NMNEEE)  !! Initial-ammonium table length by category.

      DOUBLE PRECISION, INTENT(OUT) :: AMMDDR  !! Dry ammonium deposition rate.
      DOUBLE PRECISION, INTENT(OUT) :: AMMWDR  !! Wet ammonium deposition coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: CLITFR  !! Fraction of initial organic carbon assigned to litter.
      DOUBLE PRECISION, INTENT(OUT) :: CNRBIO  !! Biomass carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(OUT) :: CNRHUM  !! Humus carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(OUT) :: CNRLIT  !! Initial litter carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(OUT) :: FE  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(OUT) :: FH  !! Humification fraction.
      DOUBLE PRECISION, INTENT(OUT) :: GNN  !! Nonlinear ammonium adsorption exponent.
      DOUBLE PRECISION, INTENT(OUT) :: KPLAMM  !! First-order ammonium plant-uptake limit.
      DOUBLE PRECISION, INTENT(OUT) :: KPLNIT  !! First-order nitrate plant-uptake limit.
      DOUBLE PRECISION, INTENT(OUT) :: KUAMM  !! First-order ammonium immobilisation limit.
      DOUBLE PRECISION, INTENT(OUT) :: KUNIT  !! First-order nitrate immobilisation limit.
      DOUBLE PRECISION, INTENT(OUT) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(OUT) :: NITDDR  !! Dry nitrate deposition rate.
      DOUBLE PRECISION, INTENT(OUT) :: NITWDR  !! Wet nitrate deposition coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: Q10M  !! Q10 coefficient for mineralisation.
      DOUBLE PRECISION, INTENT(OUT) :: Q10N  !! Q10 coefficient for nitrification.
      DOUBLE PRECISION, INTENT(OUT) :: CCONC(NMNEEE, NMNTEE)  !! Initial-carbon profile values.
      DOUBLE PRECISION, INTENT(OUT) :: CDPTH(NMNEEE, NMNTEE)  !! Initial-carbon profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: CTOTTP(NLF + 1:NEL)  !! Top total-carbon value for decay initialisation.
      DOUBLE PRECISION, INTENT(OUT) :: DAMHLF(NLF + 1:NEL)  !! Ammonium decay half-depth by element.
      DOUBLE PRECISION, INTENT(OUT) :: DCHLF(NLF + 1:NEL)  !! Carbon decay half-depth by element.
      DOUBLE PRECISION, INTENT(OUT) :: KD1CNC(NMNEEE, NMNTEE)  !! KD1 denitrification profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KD1DTH(NMNEEE, NMNTEE)  !! KD1 denitrification profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KD2CNC(NMNEEE, NMNTEE)  !! KD2 denitrification profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KD2DTH(NMNEEE, NMNTEE)  !! KD2 denitrification profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KDDSOL(NS)  !! Soil ammonium adsorption coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: KHCONC(NMNEEE, NMNTEE)  !! Humus decomposition profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KHDPTH(NMNEEE, NMNTEE)  !! Humus decomposition profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KLCONC(NMNEEE, NMNTEE)  !! Litter decomposition profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KLDPTH(NMNEEE, NMNTEE)  !! Litter decomposition profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KMCONC(NMNEEE, NMNTEE)  !! Manure decomposition profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KMDPTH(NMNEEE, NMNTEE)  !! Manure decomposition profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KNCONC(NMNEEE, NMNTEE)  !! Nitrification profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KNDPTH(NMNEEE, NMNTEE)  !! Nitrification profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KVCONC(NMNEEE, NMNTEE)  !! Volatilisation profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KVDPTH(NMNEEE, NMNTEE)  !! Volatilisation profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: NACONC(NMNEEE, NMNTEE)  !! Initial-ammonium profile values.
      DOUBLE PRECISION, INTENT(OUT) :: NADPTH(NMNEEE, NMNTEE)  !! Initial-ammonium profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: NAMTOP(NLF + 1:NEL)  !! Top ammonium value for decay initialisation.

      LOGICAL, INTENT(OUT) :: ISICCD  !! True when initial carbon uses decay-function input.
      LOGICAL, INTENT(OUT) :: ISIAMD  !! True when initial ammonium uses decay-function input.
      LOGICAL, INTENT(OUT) :: ISQ10  !! True when Q10 temperature response is selected.

      ! Workspace arguments (INTENT(INOUT) because they act as read buffers)
      INTEGER, INTENT(INOUT) :: IDUM(NELEE)  !! Integer workspace for spatial reads.
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace for spatial reads.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1
      INTEGER :: NC, NCAT, NDATA, NTB
      INTEGER :: NMNT(1), IDUMS(1)
      CHARACTER(LEN=200) :: CDUM(1)
      LOGICAL :: LDUM(1)

      !-------------------------------------------------------------------*

      ! preliminaries
      ! -------------
      ! * check status of data file
      CALL ALRED2(0, MND, MNPR, 'MND')

      ! * print title for nitrate simulation
      CALL ALREDC(0, MND, MNPR, ':MN01', 1, 1, CDUM)
      WRITE (MNPR, '(/1X,A/)') CDUM(1)

      ! decomposition parameter rates
      ! -----------------------------
      ! * decomposition parameters for ammonium immobilisation,
      ! * plant uptake of ammonium,immobilisation of nitrate
      ! * and plant uptake of nitrate
      CALL ALREDF(0, MND, MNPR, ':MN11', 4, 1, DUMMY)
      KUAMM = DUMMY(1)
      KPLAMM = DUMMY(2)
      KUNIT = DUMMY(3)
      KPLNIT = DUMMY(4)

      ! further parameters
      ! ------------------
      ! * organic matter effeciency fraction and humification fraction
      ! * and carbon to nitrogen ratio in the biomass and humus
      CALL ALREDF(0, MND, MNPR, ':MN12', 4, 1, DUMMY)
      FE = DUMMY(1)
      FH = DUMMY(2)
      CNRBIO = DUMMY(3)
      CNRHUM = DUMMY(4)

      ! * dry and wet deposition rates of ammonium and nitrate
      CALL ALREDF(0, MND, MNPR, ':MN13', 4, 1, DUMMY)
      AMMDDR = DUMMY(1)
      AMMWDR = DUMMY(2)
      NITDDR = DUMMY(3)
      NITWDR = DUMMY(4)

      ! * reference contaminant concentration
      CALL ALREDF(0, MND, MNPR, ':MN14', 1, 1, DUMMY)
      MNCREF = DUMMY(1)

      ! spatially varying decomposition parameter rates
      ! -----------------------------------------------

      ! khum
      ! ----
      ! * find out how many typical element catagories
      CALL ALREDI(0, MND, MNPR, ':MN15a', 1, 1, IDUM)
      NMN15E = IDUM(1)
      IF ((NMN15E > NMNEEE) .OR. (NMN15E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn15 in mn data file')
      END IF

      ! * read the catagory type for each element into the element number
      CALL ALALLI(NMN15E, MND, MNPR, ':MN15b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KHELEM, IDUM)

      ! * table of values for each typical element
      DO NC = 1, NMN15E
         CALL ALREDI(0, MND, MNPR, ':MN16a', 1, 1, NMNT)
         NMN15T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn16a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN16b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KHDPTH(NC, NTB) = DUMMY(2*NTB - 1)
            KHCONC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! klit
      ! ----
      CALL ALREDI(0, MND, MNPR, ':MN17a', 1, 1, IDUM)
      NMN17E = IDUM(1)
      IF ((NMN17E > NMNEEE) .OR. (NMN17E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn17 in mn data file')
      END IF

      CALL ALALLI(NMN17E, MND, MNPR, ':MN17b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KLELEM, IDUM)

      DO NC = 1, NMN17E
         CALL ALREDI(0, MND, MNPR, ':MN18a', 1, 1, NMNT)
         NMN17T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn18a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN18b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KLDPTH(NC, NTB) = DUMMY(2*NTB - 1)
            KLCONC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! kman
      ! ----
      CALL ALREDI(0, MND, MNPR, ':MN19a', 1, 1, IDUM)
      NMN19E = IDUM(1)
      IF ((NMN19E > NMNEEE) .OR. (NMN19E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn19 in mn data file')
      END IF

      CALL ALALLI(NMN19E, MND, MNPR, ':MN19b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KMELEM, IDUM)

      DO NC = 1, NMN19E
         CALL ALREDI(0, MND, MNPR, ':MN20a', 1, 1, NMNT)
         NMN19T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn20a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN20b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KMDPTH(NC, NTB) = DUMMY(2*NTB - 1)
            KMCONC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! knit
      ! ----
      CALL ALREDI(0, MND, MNPR, ':MN21a', 1, 1, IDUM)
      NMN21E = IDUM(1)
      IF ((NMN21E > NMNEEE) .OR. (NMN21E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn21 in mn data file')
      END IF

      CALL ALALLI(NMN21E, MND, MNPR, ':MN21b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KNELEM, IDUM)

      DO NC = 1, NMN21E
         CALL ALREDI(0, MND, MNPR, ':MN22a', 1, 1, NMNT)
         NMN21T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn22a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN22b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KNDPTH(NC, NTB) = DUMMY(2*NTB - 1)
            KNCONC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! kvol
      ! ----
      CALL ALREDI(0, MND, MNPR, ':MN23a', 1, 1, IDUM)
      NMN23E = IDUM(1)
      IF ((NMN23E > NMNEEE) .OR. (NMN23E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn23 in mn data file')
      END IF

      CALL ALALLI(NMN23E, MND, MNPR, ':MN23b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KVELEM, IDUM)

      DO NC = 1, NMN23E
         CALL ALREDI(0, MND, MNPR, ':MN24a', 1, 1, NMNT)
         NMN23T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn24a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN24b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KVDPTH(NC, NTB) = DUMMY(2*NTB - 1)
            KVCONC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! kd1
      ! ----
      CALL ALREDI(0, MND, MNPR, ':MN25a', 1, 1, IDUM)
      NMN25E = IDUM(1)
      IF ((NMN25E > NMNEEE) .OR. (NMN25E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn25 in mn data file')
      END IF

      CALL ALALLI(NMN25E, MND, MNPR, ':MN25b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KD1ELM, IDUM)

      DO NC = 1, NMN25E
         CALL ALREDI(0, MND, MNPR, ':MN26a', 1, 1, NMNT)
         NMN25T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn26a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN26b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KD1DTH(NC, NTB) = DUMMY(2*NTB - 1)
            KD1CNC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! kd2
      ! ----
      CALL ALREDI(0, MND, MNPR, ':MN27a', 1, 1, IDUM)
      NMN27E = IDUM(1)
      IF ((NMN27E > NMNEEE) .OR. (NMN27E <= 0)) THEN
         CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn27 in mn data file')
      END IF

      CALL ALALLI(NMN27E, MND, MNPR, ':MN27b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KD2ELM, IDUM)

      DO NC = 1, NMN27E
         CALL ALREDI(0, MND, MNPR, ':MN28a', 1, 1, NMNT)
         NMN27T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn28a in mn data file')
         END IF

         NDATA = NMNT(1)*2
         CALL ALREDF(0, MND, MNPR, ':MN28b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KD2DTH(NC, NTB) = DUMMY(2*NTB - 1)
            KD2CNC(NC, NTB) = DUMMY(2*NTB)
         END DO
      END DO

      ! ammonium adsorption
      ! -------------------
      ! * kd parameter
      CALL ALREDF(0, MND, MNPR, ':MN30', NS, 1, KDDSOL)
      ! * power parameter n
      CALL ALREDF(0, MND, MNPR, ':MN31', 1, 1, DUMMY)
      GNN = DUMMY(1)

      ! temperature effect within the soil
      ! ----------------------------------
      ! * for the environmental reduction factor for temperature is a q10
      ! * function being used ? if it is the q10 factors are needed
      CALL ALREDL(0, MND, MNPR, ':MN35', 1, 1, LDUM)
      ISQ10 = LDUM(1)
      IF (ISQ10) THEN
         CALL ALREDF(0, MND, MNPR, ':MN35a', 2, 1, DUMMY)
         Q10M = DUMMY(1)
         Q10N = DUMMY(2)
      END IF

      ! values used to calculate the initial concentrations in the organic pls
      ! ----------------------------------------------------------------------
      ! * for the initial conditions of the carbon litter pool either
      ! * a decay function for each element or an typical elem. is defined
      CALL ALREDL(0, MND, MNPR, ':MN40', 1, 1, LDUM)
      ISICCD = LDUM(1)

      IF (ISICCD) THEN
         ! * total carbon concentration at the ground surface
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, CTOTTP, IDUM, DUMMY)
         ! * depth at which carbon conc. reduced by half
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, DCHLF, IDUM, DUMMY)
      ELSE
         ! * find out how many typical element catagories
         CALL ALREDI(0, MND, MNPR, ':MN43a', 1, 1, IDUM)
         NMN43E = IDUM(1)
         IF ((NMN43E > NMNEEE) .OR. (NMN43E <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn43 in mn data file')
         END IF

         ! * read the catagory type for each element into the element number
         CALL ALALLI(NMN43E, MND, MNPR, ':MN43b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, CELEM, IDUM)

         ! * table of values for each typical element
         DO NC = 1, NMN43E
            CALL ALREDI(0, MND, MNPR, ':MN44a', 1, 1, NMNT)
            NMN43T(NC) = NMNT(1)
            IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
               CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn44a in mn data file')
            END IF

            NDATA = NMNT(1)*2
            CALL ALREDF(0, MND, MNPR, ':MN44b', NDATA, 1, DUMMY)
            DO NTB = 1, NMNT(1)
               CDPTH(NC, NTB) = DUMMY(2*NTB - 1)
               CCONC(NC, NTB) = DUMMY(2*NTB)
            END DO
         END DO
      END IF

      ! * proportion of the carbon in the litter and biomass pool
      CALL ALREDF(0, MND, MNPR, ':MN45', 1, 1, DUMMY)
      CLITFR = DUMMY(1)

      ! * carbon to nitrgen ratio in the litter fraction
      CALL ALREDF(0, MND, MNPR, ':MN46', 1, 1, DUMMY)
      CNRLIT = DUMMY(1)

      ! values used to calculate the initial concentrations in the ammoniumpool
      ! ----------------------------------------------------------------------
      CALL ALREDL(0, MND, MNPR, ':MN50', 1, 1, LDUM)
      ISIAMD = LDUM(1)

      IF (ISIAMD) THEN
         ! * total ammonium concentration at the ground surface
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN51', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, NAMTOP, IDUM, DUMMY)
         ! * depth at which ammonium conc. reduced by half
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN52', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, DAMHLF, IDUM, DUMMY)
      ELSE
         ! * find out how many typical element catagories
         CALL ALREDI(0, MND, MNPR, ':MN53a', 1, 1, IDUM)
         NMN53E = IDUM(1)
         IF ((NMN53E > NMNEEE) .OR. (NMN53E <= 0)) THEN
            CALL RAISE_ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn53 in mn data file')
         END IF

         ! * read the catagory type for each element into the element number
         CALL ALALLI(NMN53E, MND, MNPR, ':MN53b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NAELEM, IDUM)

         ! * table of values for each typical element
         DO NC = 1, NMN53E
            CALL ALREDI(0, MND, MNPR, ':MN54a', 1, 1, NMNT)
            NMN53T(NC) = NMNT(1)
            IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
               CALL RAISE_ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn54a in mn data file')
            END IF

            NDATA = NMNT(1)*2
            CALL ALREDF(0, MND, MNPR, ':MN54b', NDATA, 1, DUMMY)
            DO NTB = 1, NMNT(1)
               NADPTH(NC, NTB) = DUMMY(2*NTB - 1)
               NACONC(NC, NTB) = DUMMY(2*NTB)
            END DO
         END DO
      END IF

      ! cell below which no nitrogen transformations are considered
      ! -----------------------------------------------------------
      CALL ALREDI(0, MND, MNPR, ':MN60', 1, 1, IDUMS)
      NBOTCE = IDUMS(1)

      ! epilogue
      ! --------
      CALL ALRED2(1, MND, MNPR, 'MND')

   END SUBROUTINE MNRED1

!> @brief Reads scheduled nitrogen and carbon additions for the current timestep.
!>
!> `mnred2` maintains saved next-event times for the external inorganic nitrogen
!> (`MNFN`) and external carbon/organic nitrogen (`MNFC`) files. Times read from
!> `MNFN01` and `MNFC01` are converted with [[datetime:hour_from_date]] and
!> shifted by the simulation start hour `TIH`.
!>
!> | File | Activation test | Records read when active | Flag |
!> | --- | --- | --- | --- |
!> | `MNFN` | `UZNOW + DTUZ/3600 > INTIMN` | `MNFN11` total nitrogen, `MNFN21` banding depth, `MNFN31` ammonium fraction, then the next `MNFN01` time. | `ISADDN=.true.` |
!> | `MNFC` | `UZNOW + DTUZ/3600 > INTIMC` | `MNFC11` total carbon, `MNFC21` banding depth, `MNFC31` litter fraction, `MNFC32` litter C:N, `MNFC41` manure fraction, `MNFC42` manure C:N, then the next `MNFC01` time. | `ISADDC=.true.` |
!>
!> If a file is not active in the current timestep, only its flag is set false;
!> the previous data arrays are not overwritten. [[mnerr4]] and [[mnint2]] gate
!> their use with `ISADDN` and `ISADDC`.
!>
!> The source assumes at most one nitrogen and one carbon event per timestep. If
!> more are scheduled, only the first active event is read and the next event
!> remains queued for a later call.
   SUBROUTINE MNRED2(MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, DTUZ, TIH, UZNOW, BEXBK, LINKNS, &
      CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, ISADDC, ISADDN, IDUM, DUMMY)

      USE datetime, ONLY: hour_from_date
      USE array_limits, ONLY: nyee

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CDPTHB(NLF + 1:NEL)  !! Carbon banding depth.
      DOUBLE PRECISION, INTENT(OUT) :: CLTFCT(NLF + 1:NEL)  !! Litter fraction of added carbon.
      DOUBLE PRECISION, INTENT(OUT) :: CMNFCT(NLF + 1:NEL)  !! Manure fraction of added carbon.
      DOUBLE PRECISION, INTENT(OUT) :: CNRAL(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added litter.
      DOUBLE PRECISION, INTENT(OUT) :: CNRAM(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added manure.
      DOUBLE PRECISION, INTENT(OUT) :: CTOT(NLF + 1:NEL)  !! Total external carbon addition.
      DOUBLE PRECISION, INTENT(OUT) :: NAMFCT(NLF + 1:NEL)  !! Ammonium fraction of added inorganic nitrogen.
      DOUBLE PRECISION, INTENT(OUT) :: NDPTHB(NLF + 1:NEL)  !! Nitrogen banding depth.
      DOUBLE PRECISION, INTENT(OUT) :: NTOT(NLF + 1:NEL)  !! Total external inorganic nitrogen addition.
      LOGICAL, INTENT(OUT) :: ISADDC  !! True when a carbon-addition event is active.
      LOGICAL, INTENT(OUT) :: ISADDN  !! True when a nitrogen-addition event is active.

      ! Workspace arguments (INTENT(INOUT) because they act as read buffers)
      INTEGER, INTENT(INOUT) :: IDUM(NELEE)  !! Integer workspace for spatial reads.
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace for spatial reads.

      ! Locals
      INTEGER :: NCAT
      INTEGER :: TIME(5)

      ! Saved state variables
      INTEGER, SAVE :: INTIMC, INTIMN
      INTEGER, SAVE :: PASS = 0

      !-------------------------------------------------------------------*

      PASS = PASS + 1

      ! 1. check data files are open and read first input times
      ! -------------------------------------------------------
      IF (PASS == 1) THEN
         ! * check status of nitrogen fertilizer data file
         CALL ALRED2(0, MNFN, MNPR, 'MNFM')

         ! * time of first nitrogen fertilizer addition
         CALL ALREDI(0, MNFN, MNPR, ':MNFN01', 5, 1, TIME)
         INTIMN = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)

         ! * check status of carbon fertilizer data file
         CALL ALRED2(0, MNFC, MNPR, 'MNFC')

         ! * time of first carbon fertilizer addition
         CALL ALREDI(0, MNFC, MNPR, ':MNFC01', 5, 1, TIME)
         INTIMC = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)
      END IF

      ! 2. read nitrogen data file if fertilization occurs in this timestep
      ! -------------------------------------------------------------------
      IF ((UZNOW + DTUZ/3.6D3) > INTIMN) THEN
         ISADDN = .TRUE.

         ! * total nitrogen fertilizer in each element (kg n m-2)
         CALL ALALLF(1, 1, 0, MNFN, MNPR, ':MNFN11', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, NTOT, IDUM, DUMMY)

         ! * depth the fertilizer is banded over (m)
         CALL ALALLF(1, 1, 0, MNFN, MNPR, ':MNFN21', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, NDPTHB, IDUM, DUMMY)

         ! * ammonium fraction (the remainder is nitrate )
         CALL ALALLF(1, 1, 0, MNFN, MNPR, ':MNFN31', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, NAMFCT, IDUM, DUMMY)

         ! * time of next nitrogen fertilizer addition
         CALL ALREDI(0, MNFN, MNPR, ':MNFN01', 5, 1, TIME)
         INTIMN = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)

      ELSE
         ISADDN = .FALSE.
      END IF

      ! 3. read carbon data file if fertilization occurs in this timestep
      ! -----------------------------------------------------------------
      IF ((UZNOW + DTUZ/3.6D3) > INTIMC) THEN
         ISADDC = .TRUE.

         ! * total carbon fertilizer in each element (kg n m-2)
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC11', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, CTOT, IDUM, DUMMY)

         ! * depth the fertilizer is banded over (m)
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC21', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, CDPTHB, IDUM, DUMMY)

         ! * litter fraction
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC31', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, CLTFCT, IDUM, DUMMY)

         ! * carbon/nitrogen ratio of the litter
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC32', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, CNRAL, IDUM, DUMMY)

         ! * manure fraction (the remainder from the litter and manure is humus)
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, CMNFCT, IDUM, DUMMY)

         ! * carbon/nitrogen ratio of the manure
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
            NCAT, CNRAM, IDUM, DUMMY)

         ! * time of next carbon fertilizer addition
         CALL ALREDI(0, MNFC, MNPR, ':MNFC01', 5, 1, TIME)
         INTIMC = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)

      ELSE
         ISADDC = .FALSE.
      END IF

   END SUBROUTINE MNRED2

END MODULE mn_input

