!> summary: Fixed channel-link geometry and the explicit bank-element mapping.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The shape of the channel network, established during frame and subsurface
!> setup and then read for the rest of the run: link length and width, the
!> effective bed and bankfull elevations, and — when explicit banks are
!> enabled — which elements are the two banks of each link and how they overlap
!> the vertical cells at the bed.
!>
!> Everything here is indexed by channel link over the range established by
!> `FRIND`, with a second bank-side subscript where a link has two banks.
!> Nothing in this module changes during a timestep, which is why it is
!> separate from the overland/channel solver state in [[oc_state]].
!> Module state is public by default.
!>
!> | Array family | Index order | Principal producer |
!> |:-------------|:------------|:-------------------|
!> | `ICMBK`, `NHBED`, `FHBED` | link, bank side | frame and VSS setup |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C; see docs/rename/proposal.md. |
!> @endhistory
MODULE channel_geometry

   USE array_limits, ONLY: nlfee

   IMPLICIT NONE

   INTEGER, DIMENSION(NLFEE, 2) :: ICMBK  !! Explicit bank-element number by link and bank side.
   INTEGER, DIMENSION(NLFEE, 2) :: NHBED  !! Highest VSS cell below the channel bed by link and bank side.
   DOUBLEPRECISION, DIMENSION(NLFEE) :: CLENTH !! Channel-link length (m).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: CWIDTH !! Channel-link width (m).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBEFF  !! Effective channel-bed elevation (m).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBFULL !! Bankfull channel elevation (m).
   DOUBLEPRECISION, DIMENSION(NLFEE, 2) :: FHBED !! Fractional vertical-cell overlap at the channel bed by link and bank side.
   LOGICAL :: BEXBK                            !! Whether explicit bank elements are enabled.
   LOGICAL, DIMENSION(NLFEE) :: LINKNS         !! Whether each channel link is aligned north-south.

END MODULE channel_geometry

