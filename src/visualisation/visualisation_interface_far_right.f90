!> @brief Re-exports the services used by the right-hand visualisation interface.
!>
!> `VISUALISATION_INTERFACE_FAR_RIGHT` is a façade, not a data store or output
!> backend of its own. It owns no state and contains no procedures. Its sole
!> direct consumer, [[visualisation_interface_right]], imports all downstream
!> metadata, buffering, HDF5, and shared-state services through this module.
!> Solver entry points in `Shetran.f90` and `run_sim.f90` call that right-hand
!> interface rather than this façade directly.
!>
!> The 13 public names retain the interfaces and side effects of their owner
!> modules; the five short metadata names are import aliases rather than wrapper
!> procedures:
!>
!> | Public name(s) here | Owner | Role in `visualisation_interface_right` |
!> |:--------------------|:------|:----------------------------------------|
!> | `G_C`, `G_L`, `G_I` | [[visualisation_metadata]] | Character, logical, and integer metadata getter aliases. |
!> | `G_PTR`, `S_PTR` | [[visualisation_metadata]] | Interoperable buffer-pointer getter and setter aliases. |
!> | `TIME_TO_RECORD` | [[visualisation_metadata]] | Tests whether an item is due at the current simulation time. |
!> | `REGISTER_*_VISUALISATION_METADATA` | [[visualisation_metadata]] | Registers constants or validates dynamic plan items. |
!> | `FOR_NEW_TIME` | [[visualisation_structure]] | Prepares the type-specific value buffer for a new recorded time. |
!> | `SAVE_ITEMS_WORTH` | [[visualisation_structure]] | Generic integer/real operation that stores one selected member value. |
!> | `SAVE_VISUALISATION_DATA_TO_DISK` | [[visualisation_hdf5]] | Initializes or writes the metadata-driven HDF5 output. |
!> | `VISUALISATION_TIDY_UP` | [[visualisation_hdf5]] | Closes the active HDF5 resources at simulation shutdown. |
!> | `SEND_P` | [[visualisation_pass]] | Populates shared geometry, filenames, dimensions, counts, and topology. |
!>
!> The narrow `USE, ONLY` lists make the dependency boundary explicit, while
!> `PRIVATE` followed by the matching `PUBLIC` list prevents other owner-module
!> details from leaking through the façade. Re-exporting does not add argument
!> checks, allocation ownership, cleanup, or error handling; callers remain
!> subject to the contracts of the four owner modules.
!>
!> Earlier source retained a commented alternative import from
!> `VISUALISATION_XMLJE`. That module is absent from the current tree and the
!> HDF5 writer has been the active backend throughout the repository history;
!> the obsolete commented import is therefore not a selectable fallback.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-09-08 | SB | - | Added the façade with HDF5 output, legacy Intel DLL exports, and the commented XML alternative. |
!> | 2026-04-04 | SvB | - | Applied the project-wide Fortran formatting pass without changing behavior. |
!> | 2026-04-08 | SB | 4.6.1 | Switched metadata aliases from integer indices to `C_PTR` and removed Intel exports for IFX. |
!> @endhistory
MODULE VISUALISATION_INTERFACE_FAR_RIGHT
   USE VISUALISATION_METADATA,         ONLY : G_C=>GET_METADATA_C, G_L=>GET_METADATA_L, G_I=>GET_METADATA_I,   &
      G_PTR=>GET_METADATA_PTR, S_PTR=>SET_METADATA_PTR,                &
      TIME_TO_RECORD,                                                &
      REGISTER_STATIC_VISUALISATION_METADATA,                        &
      REGISTER_DYNAMIC_VISUALISATION_METADATA
   USE VISUALISATION_STRUCTURE,        ONLY : FOR_NEW_TIME, SAVE_ITEMS_WORTH
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
