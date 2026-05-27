!> summary: Public facade for the far-right visualisation interface.
!>
!> This module re-exports the visualisation metadata, structure, HDF5 output, and
!> pass-through routines used by the main SHETRAN model. It keeps the calling
!> code insulated from the internal split between metadata registration, time-step
!> output selection, file writing, and shared setup state.
!>
!> Public facade:
!>
!> | Export group | Origin | Purpose |
!> |:-------------|:-------|:--------|
!> | `G_C`, `G_L`, `G_I`, `G_PTR`, `S_PTR` | [[visualisation_metadata]] | Read or update registered visualisation metadata. |
!> | `REGISTER_*_VISUALISATION_METADATA` | [[visualisation_metadata]] | Register static and dynamic output-variable metadata. |
!> | `TIME_TO_RECORD` | [[visualisation_metadata]] | Decide whether the current simulation time should be written. |
!> | `FOR_NEW_TIME`, `SAVE_ITEMS_WORTH` | [[visualisation_structure]] | Prepare selected output items for a new output time. |
!> | HDF5 save/tidy-up routines | [[visualisation_hdf5]] | Write output and close visualisation resources. |
!> | `SEND_P` | [[visualisation_pass]] | Pass values through the legacy visualisation buffer interface. |
MODULE VISUALISATION_INTERFACE_FAR_RIGHT
USE VISUALISATION_METADATA,         ONLY : G_C=>GET_METADATA_C, G_L=>GET_METADATA_L, G_I=>GET_METADATA_I,   &
                                           G_PTR=>GET_METADATA_PTR, S_PTR=>SET_METADATA_PTR,                &
                                           TIME_TO_RECORD,                                                &
                                           REGISTER_STATIC_VISUALISATION_METADATA,                        &
                                           REGISTER_DYNAMIC_VISUALISATION_METADATA
USE VISUALISATION_STRUCTURE,        ONLY : FOR_NEW_TIME, SAVE_ITEMS_WORTH
!USE VISUALISATION_XMLJE,            ONLY : SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP
USE VISUALISATION_HDF5,             ONLY : SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP
USE VISUALISATION_PASS,             ONLY : SEND_P

IMPLICIT NONE

PRIVATE
PUBLIC :: G_C, G_L, G_I, S_PTR, G_PTR,                            &
          TIME_TO_RECORD,                                         &
          REGISTER_STATIC_VISUALISATION_METADATA,                 &
          REGISTER_DYNAMIC_VISUALISATION_METADATA,                &
          FOR_NEW_TIME, SAVE_ITEMS_WORTH,                         &
          SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP, &
          SEND_P

END MODULE VISUALISATION_INTERFACE_FAR_RIGHT
