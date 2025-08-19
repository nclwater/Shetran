! Utility module for array increment operations
! Replaces the legacy INCLUDE pattern used in visualization
MODULE increment_utilities
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: increment_item_array, increment_list_array, increment_mask_array, increment_time_array

   ! Type definitions from the original code
   TYPE :: ITEM
      ! Add actual type definition as needed
   END TYPE

   TYPE :: LLIST
      ! Add actual type definition as needed
   END TYPE

   TYPE :: MASK
      ! Add actual type definition as needed
   END TYPE

   TYPE :: TTIME
      ! Add actual type definition as needed
   END TYPE

CONTAINS

   SUBROUTINE increment_item_array(s, n, no_items)
      TYPE(ITEM), DIMENSION(:), POINTER :: s
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(INOUT) :: no_items
      TYPE(ITEM), DIMENSION(:), POINTER :: old => NULL()
      INTEGER :: sz

      IF(ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
      ELSE
         sz = 0
      END IF

      ALLOCATE(s(sz+n))
      IF(sz > 0) THEN
         s(1:sz) = old
         DEALLOCATE(old)
      END IF

      no_items = no_items + 1
   END SUBROUTINE increment_item_array

   SUBROUTINE increment_list_array(s, n, no_lists)
      TYPE(LLIST), DIMENSION(:), POINTER :: s
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(INOUT) :: no_lists
      TYPE(LLIST), DIMENSION(:), POINTER :: old => NULL()
      INTEGER :: sz

      IF(ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
      ELSE
         sz = 0
      END IF

      ALLOCATE(s(sz+n))
      IF(sz > 0) THEN
         s(1:sz) = old
         DEALLOCATE(old)
      END IF

      no_lists = no_lists + 1
   END SUBROUTINE increment_list_array

   SUBROUTINE increment_mask_array(s, n, no_masks)
      TYPE(MASK), DIMENSION(:), POINTER :: s
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(INOUT) :: no_masks
      TYPE(MASK), DIMENSION(:), POINTER :: old => NULL()
      INTEGER :: sz

      IF(ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
      ELSE
         sz = 0
      END IF

      ALLOCATE(s(sz+n))
      IF(sz > 0) THEN
         s(1:sz) = old
         DEALLOCATE(old)
      END IF

      no_masks = no_masks + 1
   END SUBROUTINE increment_mask_array

   SUBROUTINE increment_time_array(s, n, no_times)
      TYPE(TTIME), DIMENSION(:), POINTER :: s
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(INOUT) :: no_times
      TYPE(TTIME), DIMENSION(:), POINTER :: old => NULL()
      INTEGER :: sz

      IF(ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
      ELSE
         sz = 0
      END IF

      ALLOCATE(s(sz+n))
      IF(sz > 0) THEN
         s(1:sz) = old
         DEALLOCATE(old)
      END IF

      no_times = no_times + 1
   END SUBROUTINE increment_time_array

END MODULE increment_utilities
