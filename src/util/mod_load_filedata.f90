!> summary: Shared SHETRAN input-file reading and validation utilities.
!> author: AB / RAH, Newcastle University; JE, Newcastle University
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
!>
!> This module contains the legacy `AL*` input helpers used throughout SHETRAN
!> to read scalar, array, category-table, and interpolated data from model input
!> files. The routines also provide common validation checks, default-value
!> handling, bank-element value propagation, and simple floating-point exception
!> trap setup.
!>
!> @todo figure out for each method what the variable intents are.
!> @todo replace the GOTO-jumps to outisde a loop with EXIT
!> @todo replace the _set var_ then _overwrite, if_ if _if_ or _case_ statements
!> @todo replace the array init with the now standard way (subroutine ALINIT)
!> @todo combine / clean ALREAD, ALRED2, ALREDI, ALREDF, ALREDL, ALREDC
!> @todo use DIMENSION in variable def
!> @todo is ALTRAP still necessary?
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | - | AB/RAH | - | Original `AL*.F` routines. |
!> | 2012-08 | JE | - | Fortran 90 conversion replacing the `AL*.F` files. |
!> | 2020-03-05 | SvenB | - | Formatting/docs cleanup and selected variable renames. |
!> @endhistory
MODULE mod_load_filedata

    USE SGLOBAL
    use mod_parameters

    IMPLICIT NONE

    CHARACTER(len=80) :: HEAD0_alread='( nothing read yet )' !! Last heading seen by `ALREAD`.
    CHARACTER(len=80) :: HEAD0_alredc='( nothing read yet )' !! Last heading seen by `ALREDC`.
    CHARACTER(len=80) :: HEAD0_alredi='( nothing read yet )' !! Last heading seen by `ALREDI`.
    CHARACTER(len=80) :: HEAD0_alred2='( nothing read yet )' !! Last file-management heading from `ALRED2`.
    CHARACTER(len=80) :: HEAD0_alredl='( nothing read yet )' !! Last heading seen by `ALREDL`.
    CHARACTER(len=80) :: HEAD0_alredf='( nothing read yet )' !! Last heading seen by `ALREDF`.
    

    ! --------------------------------------------------------------------------
    ! Private by default 
    PRIVATE

    ! --------------------------------------------------------------------------
    ! Public methods
    PUBLIC :: ALREAD, ALALLF, ALCHKI, ALCHK, ALINIT, ALSPRD, ALTRAP,            &
              ALINTP, ALREDL, ALREDF, ALALLI, ALRED2, ALREDC, ALREDI

    
    ! Code =====================================================================

    CONTAINS

  
    !> Reads a floating-point distributed data array for elements or columns.
    !>
    !> `ALALLF` reads category/grid/list-style input from `IUNIT`, expands the
    !> values to active SHETRAN elements, handles bank-element propagation when
    !> `BEXBK` is enabled, and returns the resulting floating-point array in `AEL`.
    !>
    !> Input format selected by the first value read from `LINE`:
    !>
    !> | `NUM_CATEGORIES_TYPES` | Behaviour |
    !> |:-----------------------|:----------|
    !> | `< MINCAT` | Fatal invalid-option error. |
    !> | `< 0` | Special sentinel; return without filling `AEL`. |
    !> | `0` | Read explicit link values, when requested, then a gridded real array. |
    !> | `1` | Read one category value per output vector and fill all target elements uniformly. |
    !> | `> 1` | Read category values plus link/grid category maps, then expand to elements. |
    !>
    !> `FLAG=0` targets links plus land elements; non-zero `FLAG` targets land
    !> columns only. `N2` is the number of values per element.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19940527 | ? | - | Initial version |
    !> | 19940919 | AB/RAH | - | v3.4.1 |
    !> @endhistory
    SUBROUTINE ALALLF (FLAG, N2, MINCAT, IUNIT, OUNIT, LINE, NEL, NLF,          &
                       NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF,  &
                       BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  AEL, IDUM, DUMMY)
        INTEGER(kind=I_P)   :: FLAG   !! Target selector: `0` includes links; non-zero reads land columns only.
        INTEGER(kind=I_P)   :: N2     !! Number of values to read for each target element.
        INTEGER(kind=I_P)   :: MINCAT !! Minimum allowed category-count option.
        INTEGER(kind=I_P)   :: IUNIT  !! Input unit positioned before the distributed-data section.
        INTEGER(kind=I_P)   :: OUNIT  !! Output/error unit used for echoes and diagnostics.
        INTEGER(kind=I_P)   :: NEL    !! Number of active elements, including links and banks.
        INTEGER(kind=I_P)   :: NLF    !! Number of active channel links.
        INTEGER(kind=I_P)   :: NX     !! Number of active grid columns in the x direction.
        INTEGER(kind=I_P)   :: NY     !! Number of active grid rows in the y direction.
        INTEGER(kind=I_P)   :: NELEE  !! Allocated element dimension.
        INTEGER(kind=I_P)   :: NLFEE  !! Allocated channel-link dimension.
        INTEGER(kind=I_P)   :: NXEE   !! Allocated x-grid dimension.
        INTEGER(kind=I_P)   :: NYEE   !! Allocated y-grid dimension.
        INTEGER(kind=I_P)   :: ICMXY(NXEE, NY)      !! Grid-to-element reference map.
        INTEGER(kind=I_P)   :: ICMBK(NLFEE, 2)      !! Bank element references for each link side.
        INTEGER(kind=I_P)   :: ICMREF(NELEE, 4, 2:2) !! Adjacent element references by element and face.
        LOGICAL             :: BEXBK       !! True when bank elements should inherit adjacent grid values.
        LOGICAL             :: LINKNS(NLF) !! True when a channel link is north-south oriented.
        CHARACTER (LEN=*)   :: LINE        !! Section title stem used to find and echo input records.
        INTEGER(kind=I_P)   :: NUM_CATEGORIES_TYPES !! Category option/count read from the input section.
        REAL(kind=R8P)      :: AEL (1 + NLF * (FLAG / N2) :                     &
                                    NELEE- (NELEE-NEL) * (1 / N2), N2)           !! Expanded element values.
        INTEGER(kind=I_P), DIMENSION(NXEE*NYEE) :: IDUM !! Integer workspace for category maps.
        REAL(kind=R8P)      :: DUMMY (NELEE)             !! Real workspace for gridded or category values.

        INTEGER(kind=I_P)   :: I1    !! First element index for uniform-value assignment.
        INTEGER(kind=I_P)   :: I2    !! Output-vector index.
        INTEGER(kind=I_P)   :: ICAT  !! Category code for the current element.
        INTEGER(kind=I_P)   :: IDUM0 !! Scalar integer scratch argument for `ALREAD`.
        INTEGER(kind=I_P)   :: IEL   !! Element index being assigned or reported.
        INTEGER(kind=I_P)   :: LN    !! Effective length of the generated title stem.
        INTEGER(kind=I_P)   :: N     !! Number of elements assigned in the uniform-value branch.
        INTEGER(kind=I_P)   :: X     !! Grid x-index.
        INTEGER(kind=I_P)   :: XY0   !! Row offset into flattened grid workspace.
        INTEGER(kind=I_P)   :: Y     !! Grid y-index.
        LOGICAL             :: BLINK !! True when link values are read explicitly.
        CHARACTER           :: CDUM  !! Scalar character scratch argument for `ALREAD`.
        CHARACTER(len=132)  :: MSG   !! Error message buffer.
        CHARACTER(len=8)    :: NEXT  !! Generated subsection title stem.
        
        ! Code =================================================================

        ! -------------
        ! Preliminaries
        ! -------------
        !
        ! Initialization
        LN = LEN (LINE) + 1  
        BLINK = NLF > 0.AND.FLAG == 0  
        
        ! Find out how many categories ( if any )
        CALL ALREAD (2, IUNIT, OUNIT, LINE, 1, 1, IDUM0, CDUM, IDUM, DUMMY)
        NUM_CATEGORIES_TYPES = IDUM (1)  
        
        
        ! Act on the Value of NUM_CATEGORIES_TYPES
        ! ------------------------
        
        ! Invalid Option
        IF (NUM_CATEGORIES_TYPES < MINCAT) THEN  
            GOTO 8001  
        
        ! Special Case: Return to Caller
        ELSEIF (NUM_CATEGORIES_TYPES < 0) THEN  
            RETURN  
        
        ! No Categories
        ELSEIF (NUM_CATEGORIES_TYPES == 0) THEN  
            ! Loop over output vectors
            DO I2 = 1, N2  

                ! Get values for link elements
                IF (BLINK) THEN  
                    NEXT = LINE // 'a'  
                    CALL ALREAD (3, IUNIT, OUNIT, NEXT (:LN), NLF, 1, IDUM0,    &
                                 CDUM, IDUM, AEL (1, I2) )
                ENDIF  

                ! Get values for grid elements ...
                NEXT = LINE // 'b'  
                CALL ALREAD (5, IUNIT, OUNIT, NEXT (:LN), NX, NY, IDUM0,        &
                             CDUM, IDUM, DUMMY)
            
                ! ... and load into element array
                DO Y = 1, NY  
                    XY0 = (Y - 1) * NX  
                    DO X = 1, NX  
                        IEL = ICMXY (X, Y)  
                        IF (IEL > 0) AEL (IEL, I2) = DUMMY (XY0 + X)  
                    END DO  
                END DO  
            END DO  
        
        ! Use category codes
        ELSEIF (N2 * NUM_CATEGORIES_TYPES <= NELEE) THEN  

            ! Get list of values for each category
            NEXT = LINE // 'c'  
            CALL ALREAD (3, IUNIT, OUNIT, NEXT (:LN), N2, NUM_CATEGORIES_TYPES, &
                         IDUM0, CDUM, IDUM, DUMMY)

            IF (NUM_CATEGORIES_TYPES == 1) THEN  

                ! Uniform value: Set all elements or just columns
                N = NEL - FLAG * NLF  
                I1 = 1 + NEL - N  
                DO I2 = 1, N2  
                    CALL ALINIT (DUMMY (I2), N, AEL (I1, I2) )  
                END DO  

            ELSE  
                !
                ! Note: One code applies to all output vectors
                !
                ! Get codes & set values for link elements
                IF (BLINK) THEN  
                    NEXT = LINE // 'd'  
                    
                    ! Note: DUMMY should not be overwritten here
                    CALL ALREAD (2, IUNIT, OUNIT, NEXT (:LN), NLF, 1, IDUM0,    &
                                 CDUM, IDUM, DUMMY)
                    
                    DO IEL = 1, NLF  
                        ICAT = IDUM (IEL)  
                        IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) GOTO 8009  
                        DO I2 = 1, N2  
                            AEL (IEL, I2) = DUMMY (I2 + (ICAT - 1) * N2)  
                        END DO  
                    END DO  
                ENDIF  
                
                ! Get codes & set values for grid elements
                NEXT = LINE // 'e'  
                CALL ALREAD (4, IUNIT, OUNIT, NEXT (:LN), NX, NY,               &
                             NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)
                
                DO Y = 1, NY  
                    XY0 = (Y - 1) * NX  
                    DO X = 1, NX  
                        IEL = ICMXY (X, Y)  
                        IF (IEL > 0) THEN  
                            ICAT = IDUM (XY0 + X)  

                            ! error if out of bounds
                            IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES)      &
                                GOTO 8009  
                            DO I2 = 1, N2  
                                AEL (IEL, I2) = DUMMY (I2 + (ICAT - 1) * N2)
                            END DO
                        ENDIF 
                    END DO
                END DO 
            ENDIF  
        
        ! Insufficient Workspace
        ELSE
            WRITE (MSG, 9008) NUM_CATEGORIES_TYPES,  LINE,                      &
                              N2 * NUM_CATEGORIES_TYPES  
            CALL ERROR (FFFATAL, 8, OUNIT, 0, 0, MSG)  
        ENDIF  
        !
        !
        ! Epilogue
        ! --------
        !
        ! All grid elements are defined - now set bank element values
        IF (NLF > 0 .AND. BEXBK .AND. NUM_CATEGORIES_TYPES .NE. 1) THEN  
            DO I2 = 1, N2  
                CALL ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF,     &
                             AEL (NLF + 1, I2) )
            END DO  
        ENDIF 
        
        RETURN 
        

        ! Errors ---------------------------------------------------------------
        ! Invalid option
8001    WRITE (MSG, 9001) NUM_CATEGORIES_TYPES,  LINE  
        CALL ERROR (FFFATAL, 1, OUNIT, 0, 0, MSG) 
         
        
        ! Invalid category number
8009    WRITE (MSG, 9009) ICAT, NEXT (:LN), NUM_CATEGORIES_TYPES  
        CALL ERROR (FFFATAL, 9, OUNIT, IEL, 0, MSG)
        
        ! Format Statements ----------------------------------------------------
9001    FORMAT ( 'Ivalid option NUM_CATEGORIES_TYPES =', I4, ' at title line ', A )

9008    FORMAT ( 'Insufficient workspace for', I4, ' categories in ', A,        &
                 ' : increase NELEE to at least', I6 )
        
9009    FORMAT ( 'Invalid category value', I4, ' while reading ', A,            &
                 ' : should be in range [1,', I4, ']' )
        
    END SUBROUTINE ALALLF


    !> Reads category identifiers for each active grid and bank element.
    !>
    !> `ALALLI` expands the category map from the input file into element order.
    !> Bank elements inherit the category of the adjacent grid element, matching
    !> the manual rule that distributed category data are supplied by grid cell.
    !> Category identifiers outside `1:NUM_CATEGORIES_TYPES` are fatal.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | ? | ? | - | Initial version |
    !> @endhistory
    SUBROUTINE ALALLI (NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NEL, NLF, NX,  &
                       NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK,     &
                       LINKNS, CATTYP, IDUM)
        INTEGER(kind=I_P)   :: NUM_CATEGORIES_TYPES !! Number of valid category types.
        INTEGER(kind=I_P)   :: IUNIT !! Input unit positioned before the category-grid section.
        INTEGER(kind=I_P)   :: OUNIT !! Output/error unit used for diagnostics.
        INTEGER(kind=I_P)   :: NEL   !! Number of active elements, including links and banks.
        INTEGER(kind=I_P)   :: NLF   !! Number of active channel links.
        INTEGER(kind=I_P)   :: NX    !! Number of active grid columns in the x direction.
        INTEGER(kind=I_P)   :: NY    !! Number of active grid rows in the y direction.
        INTEGER(kind=I_P)   :: NELEE !! Allocated element dimension.
        INTEGER(kind=I_P)   :: NLFEE !! Allocated channel-link dimension.
        INTEGER(kind=I_P)   :: NXEE  !! Allocated x-grid dimension.
        INTEGER(kind=I_P)   :: ICMXY(NXEE, NY)       !! Grid-to-element reference map.
        INTEGER(kind=I_P)   :: ICMBK(NLFEE, 2)       !! Bank element references for each link side.
        INTEGER(kind=I_P)   :: ICMREF(NELEE, 4, 2:2) !! Adjacent element references by element and face.
        LOGICAL             :: BEXBK         !! True when bank categories should be copied from grid cells.
        LOGICAL             :: LINKNS(NLFEE) !! True when a channel link is north-south oriented.
        CHARACTER (LEN=*)   :: LINE          !! Section title stem used to find and echo input records.
        INTEGER(kind=I_P)   :: CATTYP (NLF + 1:NEL) !! Category type by land or bank element.
        INTEGER, DIMENSION(:), INTENT(IN)   :: IDUM !! Integer workspace filled by `ALREDI`.

        INTEGER(kind=I_P)   :: BANK1 !! First bank element adjacent to the current link.
        INTEGER(kind=I_P)   :: BANK2 !! Second bank element adjacent to the current link.
        INTEGER(kind=I_P)   :: FACE1 !! Face on `BANK1` opposite the current link.
        INTEGER(kind=I_P)   :: FACE2 !! Face on `BANK2` opposite the current link.
        INTEGER(kind=I_P)   :: GRID1 !! Grid element used as the category source for `BANK1`.
        INTEGER(kind=I_P)   :: GRID2 !! Grid element used as the category source for `BANK2`.
        INTEGER(kind=I_P)   :: ISNS  !! Orientation offset: 1 for north-south links, otherwise 0.
        INTEGER(kind=I_P)   :: LINK  !! Channel-link index.
        INTEGER(kind=I_P)   :: ICAT !! Category code for the current grid element.
        INTEGER(kind=I_P)   :: IEL  !! Element index mapped from the current grid cell.
        INTEGER(kind=I_P)   :: X    !! Grid x-index.
        INTEGER(kind=I_P)   :: XY0  !! Row offset into flattened grid workspace.
        INTEGER(kind=I_P)   :: Y    !! Grid y-index.
        
        ! Code =================================================================

        ! Read the catagory type for each element
        CALL ALREDI (NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NX, NY, IDUM)  
        DO Y = 1, NY  
            XY0 = (Y - 1) * NX  
            DO X = 1, NX  
                IEL = ICMXY (X, Y)  
                IF (IEL > 0) THEN  
                    ICAT = IDUM (XY0 + X)  
                    IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN  
                        CALL ERROR (FFFATAL, 3090, OUNIT, 0, 0,                 &
                                    'Error in ALALLI -reading spatially distributed category types')
                    ENDIF  
                    CATTYP (IEL) = ICAT  
                ENDIF  
            END DO  
        END DO  
        
        ! All grid elements are defined - now set bank element values
        ! Copied from ALBANK except an INTEGER(kind=I_P) array CATTYP is used
        ! instead of the floating point array.
        IF (NLF > 0.AND.BEXBK) THEN  
        ! Loop over channel links
            DO LINK = 1, NLF  

                ! Determine orientation of link
                ISNS = 0  
                IF (LINKNS (LINK) ) ISNS = 1  

                ! For each side of the channel: Determine adjacent bank element
                ! number, the number of it's face that lies opposite to the
                ! channel, and the number of the grid element adjacent to
                ! that face.
                BANK1 = ICMBK (LINK, 1)  
                BANK2 = ICMBK (LINK, 2)  
                FACE1 = 2 - ISNS  
                FACE2 = 4 - ISNS  
                GRID1 = ICMREF (BANK1, FACE1, 2)  
                GRID2 = ICMREF (BANK2, FACE2, 2)  

                ! If the grid ( as defined above ) does not exist, then use
                ! the grid corresponding to the opposite side of the channe
                ! ( precondition on ICMREF disallows GRID1 & GRID2 both zero )
                IF (GRID1 == 0) GRID1 = GRID2  
                IF (GRID2 == 0) GRID2 = GRID1  
                
                ! For each side of the channel, copy the contents of the array
                ! from the grid to its corresponding bank
                CATTYP (BANK1) = CATTYP (GRID1)  
                CATTYP (BANK2) = CATTYP (GRID2)  
                !
                ! Next channel link
            END DO  
        ENDIF  
        
        return

    END SUBROUTINE ALALLI


    !> Copies adjacent grid-cell values onto bank elements.
    !>
    !> `ALBANK` fills the bank portions of an element array from the neighbouring
    !> square elements identified by `ICMBK`, `LINKNS`, and `ICMREF`. It is used
    !> after gridded distributed data have been read so that bank elements carry
    !> the same parameter value as their adjacent land element.
    !>
    !> Entry assumptions:
    !>
    !> | Item | Requirement |
    !> |:-----|:------------|
    !> | `ICMBK(link,1:2)` | Valid bank-element references for every active link. |
    !> | `ICMREF(bank,face,2)` | At least one of the two opposite-side grid references is non-zero. |
    !> | `A` | Defined for all possible source grid elements before bank values are copied. |
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19940422 | ? | - | Initial version |
    !> | 19940523 | AB/RAH | - | Version 3.4.1 |
    !> @endhistory
    SUBROUTINE ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, A)
        INTEGER(kind=I_P), INTENT(IN)   :: NEL   !! Number of active elements, including links and banks.
        INTEGER(kind=I_P), INTENT(IN)   :: NLF   !! Number of active channel links.
        INTEGER(kind=I_P), INTENT(IN)   :: NLFEE !! Allocated channel-link dimension.
        INTEGER(kind=I_P), INTENT(IN)   :: NELEE !! Allocated element dimension.
        INTEGER(kind=I_P), INTENT(IN)   :: ICMBK (NLFEE, 2)      !! Bank element references for each link side.
        INTEGER(kind=I_P), INTENT(IN)   :: ICMREF (NELEE, 4, 2:2) !! Adjacent element references by element and face.
        LOGICAL, INTENT(IN)             :: LINKNS (NLF) !! True when a channel link is north-south oriented.
        REAL(kind=R8P), INTENT(INOUT)   :: A (NLF + 1:NEL) !! Element array whose bank entries are overwritten.

        INTEGER(kind=I_P)   :: BANK1 !! First bank element adjacent to the current link.
        INTEGER(kind=I_P)   :: BANK2 !! Second bank element adjacent to the current link.
        INTEGER(kind=I_P)   :: FACE1 !! Face on `BANK1` opposite the current link.
        INTEGER(kind=I_P)   :: FACE2 !! Face on `BANK2` opposite the current link.
        INTEGER(kind=I_P)   :: GRID1 !! Grid element used as the value source for `BANK1`.
        INTEGER(kind=I_P)   :: GRID2 !! Grid element used as the value source for `BANK2`.
        INTEGER(kind=I_P)   :: ISNS  !! Orientation offset: 1 for north-south links, otherwise 0.
        INTEGER(kind=I_P)   :: LINK  !! Channel-link index.
    
    
        ! Code =================================================================

        !
        ! Loop over channel links
        DO LINK = 1, NLF  
            
            ! Determine orientation of link
            ISNS = 0  
            IF (LINKNS (LINK) ) ISNS = 1  
            
            ! For each side of the channel: Determine adjacent bank element
            !  number, the number of it's face that lies opposite to the
            !  channel, and the number of the grid element adjacent to
            !  that face.
            BANK1 = ICMBK (LINK, 1)  
            BANK2 = ICMBK (LINK, 2)  
            FACE1 = 2 - ISNS  
            FACE2 = 4 - ISNS  
            GRID1 = ICMREF (BANK1, FACE1, 2)  
            GRID2 = ICMREF (BANK2, FACE2, 2)  
            
            ! If the grid ( as defined above ) does not exist, then use the
            ! grid corresponding to the opposite side of the channel
            ! ( precondition on ICMREF disallows GRID1 & GRID2 both zero )
            IF (GRID1 == 0) GRID1 = GRID2  
            IF (GRID2 == 0) GRID2 = GRID1  
            
            ! For each side of the channel, copy the contents of the array
            ! from the grid to its corresponding bank
            A (BANK1) = A (GRID1)  
            A (BANK2) = A (GRID2)  
            !
            ! Next channel link
        END DO  
        
    END SUBROUTINE ALBANK


    !> Checks real-valued input data against a named validation rule.
    !>
    !> `ALCHK` compares observed values in `OBJ` with one or more subject values
    !> in `SUBJ` using the operator named by `OP`, with tolerance `TOL`. It
    !> reports invalid input through `ERROR` according to `ACTION`, increments
    !> `COUNT`, and flags individual failures in `NOTOK`.
    !>
    !> Validation controls:
    !>
    !> | Item | Meaning |
    !> |:-----|:--------|
    !> | `OP='LT'`/`'GT'` | Require `SUBJ < OBJ` or `SUBJ > OBJ`, using tolerance `TOL`. |
    !> | `OP='LE'`/`'GE'` | Require `SUBJ <= OBJ` or `SUBJ >= OBJ`, using tolerance `TOL`. |
    !> | Other `OP(2:2)` | Require approximate equality with tolerance `TOL`. |
    !> | `OP` ending in `a` | Compare against `OBJ(i)` instead of a scalar `OBJ(N0)`. |
    !> | `ACTION < 0` | Reset failing `SUBJ` values to `OBJ` before reporting. |
    !>
    !> @note The legacy source says `ALCHKI` was generated from `ALCHK` by
    !> `make`; keep the real and integer routines behaviourally aligned.
    !> @endnote
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19940722 | ? | - | Initial version |
    !> | 19940817 | AB/RAH | - | Version 3.4.1 |
    !> @endhistory
    SUBROUTINE ALCHK (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME,           &
                      OP, OBJ, TOL, SUBJ, COUNT, NOTOK)
        INTEGER(kind=I_P)            :: ACTION !! Error action; negative values also reset bad data.
        INTEGER(kind=I_P)            :: ERRNUM !! Error number passed to `ERROR`.
        INTEGER(kind=I_P)            :: OUNIT  !! Output/error unit used by `ERROR`.
        INTEGER(kind=I_P)            :: N0     !! First vector index checked.
        INTEGER(kind=I_P)            :: N1     !! Last vector index checked.
        INTEGER(kind=I_P)            :: IX2    !! Optional second subscript printed in diagnostics.
        INTEGER(kind=I_P)            :: IX3    !! Optional third subscript printed in diagnostics.
        CHARACTER(LEN=*), INTENT(IN) :: SNAME  !! Subject name, optionally including subscript syntax.
        CHARACTER(LEN=*), INTENT(IN) :: OP     !! Validation operator and optional array suffix.
        REAL(kind=R8P)               :: OBJ(N0:*) !! Scalar or vector of comparison values.
        REAL(kind=R8P)               :: TOL    !! Relative tolerance for real comparisons.
        REAL(kind=R8P)               :: SUBJ(N0:N1) !! Subject values checked and optionally reset.
        INTEGER(kind=I_P)            :: COUNT  !! Cumulative nonconformance count.
        LOGICAL                      :: NOTOK(N0:N1) !! Workspace flags for failing positions.

        INTEGER(kind=I_P)   :: COUNT0 !! `COUNT` value on entry.
        INTEGER(kind=I_P)   :: COUNT1 !! Number of failures found by this call.
        INTEGER(kind=I_P)   :: I      !! Vector index.
        INTEGER(kind=I_P)   :: INCOBJ !! `OBJ` index increment: 0 for scalar, 1 for vector.
        INTEGER(kind=I_P)   :: IOBJ   !! Current index into `OBJ`.
        INTEGER(kind=I_P)   :: IX(3)  !! Subscripts printed for the first failing value.
        INTEGER(kind=I_P)   :: NDIM   !! Number of subscripts detected in `SNAME`, capped at 3.
        INTEGER(kind=I_P)   :: P      !! Diagnostic subscript loop index.
        INTEGER(kind=I_P)   :: POS1   !! Previous delimiter position while parsing `SNAME`.
        INTEGER(kind=I_P)   :: POS2   !! Current delimiter position while parsing `SNAME`.
        INTEGER(kind=I_P)   :: SGN    !! Direction multiplier: `+1` for less-than, `-1` for greater-than.
        INTEGER(kind=I_P)   :: SLEN   !! Length of `SNAME`.
        REAL(kind=R8P)      :: SB     !! Subject value for the current or first failing position.
        REAL(kind=R8P)      :: OB     !! Object value for the current or first failing position.
        REAL(kind=R8P)      :: rrr    !! Real diagnostic copy of `SB`.
        LOGICAL             :: BRESET !! True when failing values are reset to `OBJ`.
        CHARACTER(len=9)    :: CACT   !! Diagnostic action text.
        CHARACTER(len=132)  :: MSG    !! Error message buffer.
        CHARACTER           :: OP1    !! First operator character, selects direction.
        CHARACTER           :: OP2    !! Second operator character, selects strict/inclusive/equal test.
    
        ! Code =================================================================

        !
        ! How many subscripts are there? (ignore any after the 3rd)
        ! ------------------------------
        !
        SLEN = LEN (SNAME)  
        POS1 = 0  
        POS2 = INDEX (SNAME, '(')  
        DO NDIM = 0, 2  
            IF (POS2 > POS1.AND.POS2 < SLEN) THEN  
                IF (NDIM == 1) IX (2) = IX2  
                IF (NDIM == 2) IX (3) = IX3  
                POS1 = POS2  
                POS2 = POS1 + INDEX (SNAME (POS1 + 1:) , ',')  
            ELSE  
                GOTO 101  
            ENDIF  
        END DO  
        
        ! If this point is traversed NDIM=3; if skipped NDIM<3
  101   CONTINUE  
  
        ! What action is required?
        ! ------------------------
        !
        BRESET = ACTION < 0  
        OP1    = OP (1:1)  
        OP2    = OP (2:2)  
        SGN    = + 1  
        IF (OP1 == 'G') SGN = - 1  
        INCOBJ = 0  
        IF (OP (LEN (OP) :)  == 'a') INCOBJ = 1  
        
        ! Store test results in logical workspace array
        ! ---------------------------------------------
        !
        ! Note:  i Code is replicated to enable vectorization of loops.
        !   ii "Requirements" are approximate if TOL>0.
        !
        IOBJ = N0  
        
        IF (OP2 == 'T') THEN  
            ! require SUBJ < OBJ or SUBJ > OBJ (depending on SGN)
            DO I = N0, N1  
                SB        = SUBJ (I)  
                OB        = OBJ (IOBJ)  
                NOTOK (I) = SGN * (SB - OB) >= TOL * MAX (ABS (SB), ABS (OB) )
                IOBJ      = IOBJ + INCOBJ  
            END DO  
            
        ELSEIF (OP2 == 'E') THEN  
            ! require SUBJ <= OBJ or SUBJ >= OBJ (depending on SGN)
            DO I = N0, N1  
                SB        = SUBJ (I)  
                OB        = OBJ (IOBJ)  
                NOTOK (I) = SGN * (SB - OB)  > TOL * MAX (ABS (SB), ABS (OB) )
                IOBJ      = IOBJ + INCOBJ  
            END DO  
            
        ELSE  
            ! require SUBJ == OBJ
            DO I = N0, N1  
                SB        = SUBJ (I)  
                OB        = OBJ (IOBJ)  
                NOTOK (I) = ABS (SB - OB)  > TOL * MAX (ABS (SB), ABS (OB))
                IOBJ      = IOBJ + INCOBJ  
            END DO  
        ENDIF  
        
        ! Count the non-conformances and fix them if required
        ! ---------------------------------------------------
        !
        ! Note: Non-vectorizing loop: keep it short
        !
        COUNT0 = COUNT  
        IOBJ   = N0 + INCOBJ * (N1 - N0)  
        
        ! step backwards so that IX(1), SB & OB refer to 1st non-conformer
        DO I = N1, N0, - 1  
            IF (NOTOK (I) ) THEN  
                COUNT  = COUNT + 1  
                IX (1) = I  
                SB     = SUBJ (I)  
                OB     = OBJ (IOBJ)  
                IF (BRESET) SUBJ (I) = OB  
            ENDIF  
            IOBJ = IOBJ - INCOBJ
        END DO  
        
        ! Report findings
        ! ---------------
        !
        COUNT1 = COUNT - COUNT0  
        IF (COUNT1 > 0) THEN  
            CACT             = 'Checking'  
            IF (BRESET) CACT = 'Resetting'   

            ! print the first occurrence ...
            rrr = sb  !AD
            WRITE (MSG, 9000) CACT, SNAME, OP (:2), OB, rrr, (IX (P),           &
                   P = 1, NDIM)
            CALL ERROR (ABS (ACTION), ERRNUM, OUNIT, 0, 0, MSG)  

            IF (COUNT1 > 1) THEN  
                ! ... and allude to any others
                WRITE (MSG, 9010) COUNT1 - 1  
                CALL ERROR (0, 12, OUNIT, 0, 0, MSG)  
            ENDIF
        ENDIF  
        
        ! Format Statements ----------------------------------------------------
 9000   FORMAT( A,1X,A,': expected .',A,'.',1P,G15.7,' but found',G15.7:        &
               ' at position', I5, 2( : ',', I4 ))
 9010   FORMAT('... and similarly at', I4,                                      &
               ' other positions in the same vector')
               
    END SUBROUTINE ALCHK


    !> Checks integer input data against a named validation rule.
    !>
    !> `ALCHKI` is the integer counterpart of [[ALCHK]]. It applies the operator
    !> named by `OP` to `OBJ` and `SUBJ`, reports invalid input through `ERROR`
    !> according to `ACTION`, increments `COUNT`, and marks failures in `NOTOK`.
    !>
    !> Validation controls:
    !>
    !> | Item | Meaning |
    !> |:-----|:--------|
    !> | `OP='LT'`/`'GT'` | Require `SUBJ < OBJ` or `SUBJ > OBJ`. |
    !> | `OP='LE'`/`'GE'` | Require `SUBJ <= OBJ` or `SUBJ >= OBJ`. |
    !> | Other `OP(2:2)` | Require exact integer equality. |
    !> | `OP` ending in `a` | Compare against `OBJ(i)` instead of a scalar `OBJ(N0)`. |
    !> | `ACTION < 0` | Reset failing `SUBJ` values to `OBJ` before reporting. |
    !>
    !> @note The legacy source says this routine was generated from `ALCHK` by
    !> `make`; keep the integer and real routines behaviourally aligned.
    !> @endnote
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19940722 | ? | - | Initial version |
    !> | 19940817 | AB/RAH | - | Version 3.4.1 |
    !> @endhistory
    SUBROUTINE ALCHKI (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME,          &
                       OP, OBJ, SUBJ, COUNT, NOTOK)
        INTEGER(kind=I_P) :: ACTION !! Error action; negative values also reset bad data.
        INTEGER(kind=I_P) :: ERRNUM !! Error number passed to `ERROR`.
        INTEGER(kind=I_P) :: OUNIT  !! Output/error unit used by `ERROR`.
        INTEGER(kind=I_P) :: N0     !! First vector index checked.
        INTEGER(kind=I_P) :: N1     !! Last vector index checked.
        INTEGER(kind=I_P) :: IX2    !! Optional second subscript printed in diagnostics.
        INTEGER(kind=I_P) :: IX3    !! Optional third subscript printed in diagnostics.
        CHARACTER(LEN=*)  :: SNAME  !! Subject name, optionally including subscript syntax.
        CHARACTER(LEN=*)  :: OP     !! Validation operator and optional array suffix.
        INTEGER(kind=I_P) :: OBJ(N0:*) !! Scalar or vector of comparison values.
        INTEGER(kind=I_P) :: SUBJ(N0:N1) !! Subject values checked and optionally reset.
        INTEGER(kind=I_P) :: COUNT  !! Cumulative nonconformance count.
        LOGICAL           :: NOTOK(N0:N1) !! Workspace flags for failing positions.

        INTEGER(kind=I_P)   :: COUNT0 !! `COUNT` value on entry.
        INTEGER(kind=I_P)   :: COUNT1 !! Number of failures found by this call.
        INTEGER(kind=I_P)   :: I      !! Vector index.
        INTEGER(kind=I_P)   :: INCOBJ !! `OBJ` index increment: 0 for scalar, 1 for vector.
        INTEGER(kind=I_P)   :: IOBJ   !! Current index into `OBJ`.
        INTEGER(kind=I_P)   :: IX(3)  !! Subscripts printed for the first failing value.
        INTEGER(kind=I_P)   :: NDIM   !! Number of subscripts detected in `SNAME`, capped at 3.
        INTEGER(kind=I_P)   :: P      !! Diagnostic subscript loop index.
        INTEGER(kind=I_P)   :: POS1   !! Previous delimiter position while parsing `SNAME`.
        INTEGER(kind=I_P)   :: POS2   !! Current delimiter position while parsing `SNAME`.
        INTEGER(kind=I_P)   :: SGN    !! Direction multiplier: `+1` for less-than, `-1` for greater-than.
        INTEGER(kind=I_P)   :: SLEN   !! Length of `SNAME`.
        INTEGER(kind=I_P)   :: SB     !! Subject value for the current or first failing position.
        INTEGER(kind=I_P)   :: OB     !! Object value for the current or first failing position.
        INTEGER(kind=I_P)   :: iii    !! Integer diagnostic copy of `SB`.
        LOGICAL             :: BRESET !! True when failing values are reset to `OBJ`.
        CHARACTER(len=9)    :: CACT   !! Diagnostic action text.
        CHARACTER(len=132)  :: MSG    !! Error message buffer.
        CHARACTER           :: OP1    !! First operator character, selects direction.
        CHARACTER           :: OP2    !! Second operator character, selects strict/inclusive/equal test.
        
        
        ! Code =================================================================
        
        !
        ! How many subscripts are there? (ignore any after the 3rd)
        ! ------------------------------
        !
        SLEN = LEN (SNAME)  
        POS1 = 0  
        POS2 = INDEX (SNAME, '(')  
        DO NDIM = 0, 2  
            IF (POS2 > POS1.AND.POS2 < SLEN) THEN  
                IF (NDIM == 1) IX (2) = IX2  
                IF (NDIM == 2) IX (3) = IX3  
                POS1 = POS2  
                POS2 = POS1 + INDEX (SNAME (POS1 + 1:) , ',')  
            ELSE  
                GOTO 101  
            ENDIF  
        END DO  
        
        ! If this point is traversed NDIM=3; if skipped NDIM<3
   101  CONTINUE  
   
        !
        ! What action is required?
        ! ------------------------
        !
        BRESET = ACTION < 0  
        OP1    = OP (1:1)  
        OP2    = OP (2:2)  
        SGN    = + 1  
        IF (OP1 == 'G') SGN = - 1  
        INCOBJ = 0  
        IF (OP (LEN (OP) :)  == 'a') INCOBJ = 1  
        
        !
        ! Store test results in logical workspace array
        ! ---------------------------------------------
        !
        ! Note:  i Code is replicated to enable vectorization of loops.
        !
        IOBJ = N0  
        !
        IF (OP2 == 'T') THEN  
            ! require SUBJ < OBJ or SUBJ > OBJ (depending on SGN)
            DO I = N0, N1  
                SB        = SUBJ (I)  
                OB        = OBJ (IOBJ)  
                NOTOK (I) = SGN * (SB - OB)  >= 0  
                IOBJ      = IOBJ + INCOBJ
            END DO  
            
        ELSEIF (OP2 == 'E') THEN  
            ! require SUBJ <= OBJ or SUBJ >= OBJ (depending on SGN)
            DO I = N0, N1  
                SB        = SUBJ (I)  
                OB        = OBJ (IOBJ)  
                NOTOK (I) = SGN * (SB - OB)  > 0  
                IOBJ      = IOBJ + INCOBJ  
            END DO  
            
        ELSE  
            ! require SUBJ == OBJ
            DO I = N0, N1  
              SB        = SUBJ (I)  
              OB        = OBJ (IOBJ)  
              NOTOK (I) = ABS (SB - OB)  > 0  
              IOBJ      = IOBJ + INCOBJ  
            END DO  
        ENDIF  
        
        !
        ! Count the non-conformances and fix them if required
        ! ---------------------------------------------------
        !
        ! Note: Non-vectorizing loop: keep it short
        !
        COUNT0 = COUNT  
        IOBJ   = N0 + INCOBJ * (N1 - N0) 

        ! step backwards so that IX(1), SB & OB refer to 1st non-conformer
        DO I = N1, N0, - 1  
            IF (NOTOK (I) ) THEN  
                COUNT  = COUNT + 1  
                IX (1) = I  
                SB     = SUBJ (I)  
                OB     = OBJ (IOBJ)  
                IF (BRESET) SUBJ (I) = OB  
            ENDIF  
            IOBJ = IOBJ - INCOBJ
        END DO  
        
        !
        ! Report findings
        ! ---------------
        !
        COUNT1 = COUNT - COUNT0  
        IF (COUNT1 > 0) THEN 
            CACT = 'Checking'  
            IF (BRESET) CACT = 'Resetting'  
            
            ! print the first occurrence ...
            iii = sb !AD
            WRITE (MSG, 9000) CACT, SNAME, OP (:2), OB, iii, (IX (P),           &
                              P = 1, NDIM)
            CALL ERROR (ABS (ACTION), ERRNUM, OUNIT, 0, 0, MSG)  
            
            IF (COUNT1 > 1) THEN  
                ! ... and allude to any others
                WRITE (MSG, 9010) COUNT1 - 1  
                CALL ERROR (0, 12, OUNIT, 0, 0, MSG)  
            ENDIF  
        ENDIF  
        
        ! Format Statements ----------------------------------------------------
 9000   FORMAT( A,1X,A,': expected .',A,'.',I12,' but found',I12:               &
                       ' at position', I5, 2( : ',', I4 ))
 9010   FORMAT( '... and similarly at', I4, ' other positions in the same vector')

    END SUBROUTINE ALCHKI


    !> Initialises every entry of an array to one real value.
    !>
    !> `ALINIT` sets `X(1:N)` to `ALPHA`. It is a small legacy helper used where
    !> distributed arrays must be reset before reading or expansion.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931208 | ? | - | Initial version |
    !> | 19940523 | AB/RAH | - | Version 3.4.1 |
    !> @endhistory
    SUBROUTINE ALINIT (ALPHA, N, X)  
        REAL(kind=R8P)      :: ALPHA !! Value assigned to every element of `X`.
        INTEGER(kind=I_P)   :: N     !! Number of values to initialise.
        REAL(kind=R8P)      :: X(N)  !! Output array filled with `ALPHA`.
        INTEGER(kind=I_P)   :: I     !! Array index.
    
        ! Code =================================================================
        
        DO I = 1, N  
            X (I) = ALPHA  
        END DO  

    END SUBROUTINE ALINIT


    !> Interpolates initial contaminant concentrations from water-depth tables.
    !>
    !> `ALINTP` uses each element's category and nodal water depths to interpolate
    !> concentration values from the supplied depth/concentration tables. Table
    !> depths must start at zero and increase monotonically, as required by the
    !> contaminant input format.
    !>
    !> For each land element and cell, the interpolation is:
    !>
    !> \[
    !> C(z)=C_1+(C_2-C_1)\frac{z-z_1}{z_2-z_1}
    !> \]
    !>
    !> where `z` is the cell-centre depth below the surface and `(z1,C1)` and
    !> `(z2,C2)` are the bracketing table entries for the element category.
    !> Cells deeper than the last table depth use the last table concentration.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | ? | ? | - | Initial version |
    !> @endhistory
    SUBROUTINE ALINTP (LLEE, NCETOP, NEL, NELEE, NLF, NUM_CATEGORIES_TYPES,     &
                       MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY,      &
                       NCOLMB, NTAB,TABLE_CONCENTRATION, TABLE_WATER_DEPTH,     &
                       DELTAZ, ZVSNOD, CELL_CONCENTRATION)
        INTEGER(kind=I_P)   :: LLEE  !! Allocated vertical-cell dimension.
        INTEGER(kind=I_P)   :: NCETOP !! Top active cell index.
        INTEGER(kind=I_P)   :: NEL   !! Number of active elements, including links and banks.
        INTEGER(kind=I_P)   :: NELEE !! Allocated element dimension.
        INTEGER(kind=I_P)   :: NLF   !! Number of active channel links; interpolation starts at `NLF+1`.
        INTEGER(kind=I_P)   :: NUM_CATEGORIES_TYPES !! Number of concentration categories.
        INTEGER(kind=I_P)   :: MAX_NUM_CATEGORY_TYPES !! Allocated category-table dimension.
        INTEGER(kind=I_P)   :: MAX_NUM_DATA_PAIRS !! Allocated depth/concentration-pair dimension.
        INTEGER(kind=I_P)   :: NCATTY(NLF + 1:NEL) !! Concentration category by land element.
        INTEGER(kind=I_P)   :: NCOLMB(NLF + 1:NEL) !! Bottom active contaminant cell by land element.
        INTEGER(kind=I_P)   :: NTAB(NUM_CATEGORIES_TYPES) !! Number of table entries in each category.
        REAL(kind=R8P), DIMENSION(MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS) :: &
            TABLE_CONCENTRATION !! Concentration table by category and depth entry.
        REAL(kind=R8P), DIMENSION(MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS) :: &
            TABLE_WATER_DEPTH   !! Water-depth table by category and entry.
        REAL(kind=R8P)      :: DELTAZ(LLEE, NELEE) !! Vertical cell thickness by cell and element.
        REAL(kind=R8P)      :: ZVSNOD(LLEE, NELEE) !! Vertical node elevation by cell and element.
        REAL(kind=R8P)      :: CELL_CONCENTRATION(NEL, NCETOP) !! Interpolated concentration by element and cell.

        INTEGER(kind=I_P)   :: NCL    !! Cell index.
        INTEGER(kind=I_P)   :: NELM   !! Land-element index.
        INTEGER(kind=I_P)   :: NCATG  !! Category for `NELM`.
        INTEGER(kind=I_P)   :: NINTB  !! Number of table entries for `NCATG`.
        INTEGER(kind=I_P)   :: NTABLE !! Current bracketing table-entry index.
        INTEGER(kind=I_P)   :: NTHRTB !! First candidate table-entry index for the next cell.
        REAL(kind=R8P)      :: DEPTH  !! Cell-centre depth below the ground surface.
        

        ! Code =================================================================
        
        DO NELM = NLF + 1, NEL  
            ! Category number for the element
            NCATG = NCATTY (NELM)  
        
            ! Number of values in the table for this category number
            NINTB = NTAB (NCATG)  

            ! The first depth in the table must be zero and the top
            ! cell is set to take the concentration at this depth
            CELL_CONCENTRATION (NELM, NCETOP) =TABLE_CONCENTRATION (NCATG, 1)  
            DEPTH                = DELTAZ (NCETOP, NELM) / two  
            NTHRTB               = 2  
            DO NCL = NCETOP - 1, NCOLMB (NELM), - 1  

                DEPTH = DEPTH + (ZVSNOD (NCL + 1, NELM) - ZVSNOD (NCL, NELM))
                ! The depth of the cell is greater than the lowest depth in
                ! the table and the cell takes the value of the concentration
                ! at the lowest specified depth
                IF (DEPTH >= TABLE_WATER_DEPTH (NCATG, NINTB) ) THEN  
                   CELL_CONCENTRATION (NELM, NCL) =TABLE_CONCENTRATION (NCATG, NINTB)  
                   CYCLE    ! SvenB: was a GOTO before, I assume jump to outer loop (both label:140)
                ENDIF  
              
                DO NTABLE = NTHRTB, NINTB  
                    IF (DEPTH <= TABLE_WATER_DEPTH (NCATG, NTABLE) ) GOTO 300  
        !                                  ^^^^^^^^^
                    NTHRTB = NTHRTB + 1  
                END DO  

    300         CELL_CONCENTRATION (NELM, NCL) =                                &
                    TABLE_CONCENTRATION (NCATG, NTABLE-1)                       &
                    + (TABLE_CONCENTRATION (NCATG, NTABLE)                      &
                    - TABLE_CONCENTRATION (NCATG, NTABLE-1) )                   &
                    * ((DEPTH - TABLE_WATER_DEPTH (NCATG, NTABLE-1))            & 
                    / (TABLE_WATER_DEPTH (NCATG, NTABLE)                        &
                    - TABLE_WATER_DEPTH (NCATG, NTABLE-1)))
            END DO  
        END DO  

    END SUBROUTINE ALINTP

                       
                       

    !> Reads one legacy AL input record for character, integer, or real data.
    !>
    !> `ALREAD` interprets the numeric input `FLAG`, reads the requested data
    !> form from `IUNIT`, echoes diagnostics to `OUNIT`, and returns values in
    !> the matching output array. It covers the original mixed-format input cases
    !> used by distributed SHETRAN parameters.
    !>
    !> `FLAG` modes:
    !>
    !> | `FLAG` | Action |
    !> |:-------|:-------|
    !> | `-1` | Close `IUNIT` and echo the file status. |
    !> | `0` | Check that `IUNIT` is open and echo the file status. |
    !> | `1` | Read one character record into `CDATA`. |
    !> | `2` | Read a free-format integer array into `IDATA`. |
    !> | `3` | Read a free-format floating-point array into `RDATA`. |
    !> | `4` | Read an indexed integer grid, rows `N2` down to 1. |
    !> | `5` | Read an indexed floating-point grid, rows `N2` down to 1. |
    !> | `6` | Read VSS per-category layer-number and real-value records. |
    !> | `7` | Read VSS soil physical-property records. |
    !>
    !> For positive `FLAG`, the routine first reads and checks a title line
    !> against `LINE`; mismatches are warnings, while read/data errors are fatal.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931210 | ? | - | Initial version |
    !> | 19940912 | GP | - | 4.0  Add VSS options (FLAG=) 6 & 7. |
    !> | 19940916 | AB/RAH | - | Version 3.4.1 |
    !> | 19970804 | RAH | - | 4.1  Add END specifiers to READs in options 6 and 7; renumber error 13 as 16. |
    !> @endhistory
    SUBROUTINE ALREAD (FLAG, IUNIT, OUNIT, LINE, N1, N2, NUM_CATEGORIES_TYPES,  &
                       CDATA, IDATA, RDATA)
        INTEGER(kind=I_P) :: FLAG !! Reader action selector.
        INTEGER(kind=I_P) :: IUNIT !! Input unit to inspect, read, or close.
        INTEGER(kind=I_P) :: OUNIT !! Output/error unit used for echoes and diagnostics.
        INTEGER(kind=I_P) :: N1    !! First data dimension or number of grid columns.
        INTEGER(kind=I_P) :: N2    !! Second data dimension or number of grid rows.
        INTEGER(kind=I_P) :: NUM_CATEGORIES_TYPES !! Category/record count, or grid integer-code limit.
        CHARACTER(LEN=*)  :: LINE  !! Expected title line or file-status label.
        CHARACTER(LEN=*)  :: CDATA !! Character output for `FLAG=1`.
        INTEGER(kind=I_P) :: IDATA(N1, N2) !! Integer output/work array.
        REAL(kind=R8P)    :: RDATA(N1, N2) !! Floating-point output/work array.

        CHARACTER(LEN=80)  :: HEAD  !! Current title or status message.
        CHARACTER(LEN=140) :: MSG   !! Error message buffer.
        CHARACTER(LEN=48)  :: FILNAM !! File name returned by `INQUIRE`.
        CHARACTER(LEN=17)  :: FORM  !! Generated fixed-format integer-grid format.
        INTEGER(kind=I_P)  :: IX     !! Grid x-index.
        INTEGER(kind=I_P)  :: IY     !! Grid y-index.
        INTEGER(kind=I_P)  :: KY     !! Row number read from an indexed grid row.
        INTEGER(kind=I_P)  :: IDUM1  !! VSS item/category index read from input.
        INTEGER(kind=I_P)  :: IDUM2  !! VSS item value count read from input.
        INTEGER(kind=I_P)  :: ICOUNT !! Category/record loop index.
        INTEGER(kind=I_P)  :: I      !! Implied-DO index.
        LOGICAL            :: BOPEN  !! True when `IUNIT` is open.
        LOGICAL            :: BNAMED !! True when `IUNIT` has an associated filename.

        ! Code =================================================================

        !----------------------------------------------------------------------*
        ! Preliminaries
        ! -------------

        IF (FLAG > 0) THEN  
            ! Check data header against what the caller expects to find
            READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
            IF (INDEX (HEAD, LINE)  == 0) THEN  
                WRITE (MSG, 9002) LINE, HEAD  
                CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)
            ENDIF  

        ELSE  
            ! Get file status and name
            INQUIRE (IUNIT, OPENED = BOPEN, NAMED = BNAMED, NAME = FILNAM)  
            IF (.NOT.BNAMED) FILNAM = '(no name)'  
        ENDIF  

        ! Take Specified Action
        ! ---------------------

        ! Check that input file is open
        IF (FLAG == 0) THEN  
            IF (.NOT.BOPEN) GOTO 8000  
        
            ! Write (and store) an informative message
            WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM  
            WRITE (OUNIT, 9001) HEAD  
        
        ! Close input file
        ELSEIF (FLAG ==  - 1) THEN  

            CLOSE (IUNIT)  
        
            ! Write (and store) an informative message
            WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM  
            WRITE (OUNIT, 9001) HEAD  

        ! Read a character string
        ELSEIF (FLAG == 1) THEN
           READ (IUNIT, '(A)', ERR = 8100, END = 8100) CDATA 

        ! Read an INTEGER(kind=I_P) array
        ELSEIF (FLAG == 2) THEN  
            READ (IUNIT, *, ERR = 8200, END = 8200) IDATA  

        ! Read a floating-point array
        ELSEIF (FLAG == 3) THEN
            READ (IUNIT, *, ERR = 8300, END = 8300) RDATA  
            
        ! Read an INTEGER(kind=I_P) grid array
        ELSEIF (FLAG == 4) THEN  

            ! Set format string to read single digit integers if possible
            IF (NUM_CATEGORIES_TYPES < 10) WRITE (FORM, 9410) N1  
        
            ! All grid rows: North to South
            DO IY = N2, 1, - 1  
                IF (NUM_CATEGORIES_TYPES < 10) THEN  
                    READ (IUNIT, FORM, ERR = 8420, END = 8420) KY,              &
                        (IDATA (IX, IY), IX = 1, N1)

                ELSE  
                    READ (IUNIT, *, ERR = 8420, END = 8420) KY,                 &
                        (IDATA (IX, IY), IX = 1, N1)
                ENDIF  

                IF (KY .NE. IY) GOTO 8420  
            END DO  
            
        ! Read a floating point grid array
        ELSEIF (FLAG == 5) THEN  
            
            ! All grid rows: North to South
            DO IY = N2, 1, - 1  
                READ (IUNIT, *, ERR = 8430, END = 8430) KY,                     &
                    (RDATA (IX, IY), IX = 1, N1)
                IF (KY .NE. IY) GOTO 8430  
            END DO  

        ! Read data in VSS format for each element
        ELSEIF (FLAG == 6) THEN 

            DO ICOUNT = 1, NUM_CATEGORIES_TYPES  
                READ (IUNIT, *, ERR = 8600, END = 8600) IDUM1, IDUM2  
                READ (IUNIT, *, ERR = 8600, END = 8600) (IDATA (IDUM1, I),      &
                    I = 1, IDUM2)
                READ (IUNIT, *, ERR = 8600, END = 8600) (RDATA (IDUM1, I),      &
                    I = 1, IDUM2)
            END DO  

        ! Read soil physical property data for VSS
        ELSEIF (FLAG == 7) THEN  
            
            DO ICOUNT = 1, NUM_CATEGORIES_TYPES  
                READ (IUNIT, *, ERR = 8700, END = 8700) (IDATA (ICOUNT, I),     &
                    I = 1, 3)
                IF (IDATA (ICOUNT, 1) .NE. ICOUNT) GOTO 8700  
                READ (IUNIT, *, ERR = 8700, END = 8700) (RDATA (ICOUNT, I),     &
                    I = 1, 8)
            END DO  
        ENDIF  

        ! Epilogue
        ! --------
        ! Store current title as old title
        HEAD0_alread = HEAD 

        RETURN  
        
        ! Errors ---------------------------------------------------------------

        ! File not open
 8000   WRITE (MSG, 9000) LINE, 'not open', IUNIT  
        CALL ERROR (FFFATAL, 4, OUNIT, 0, 0, MSG)  

        ! Title line read error
 8010   WRITE (MSG, 9801) LINE, HEAD0_alread  
        CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG) 

        ! Char data error
 8100   WRITE (MSG, 9810) 'character', HEAD  
        CALL ERROR (FFFATAL, 5, OUNIT, 0, 0, MSG)  
        
        ! INTEGER(kind=I_P) data error
 8200   WRITE (MSG, 9810) 'integer', HEAD  
        CALL ERROR (FFFATAL, 6, OUNIT, 0, 0, MSG)  
        
        ! Real data error
 8300   WRITE (MSG, 9810) 'floating-point', HEAD  
        CALL ERROR (FFFATAL, 7, OUNIT, 0, 0, MSG)  
        
        ! INTEGER(kind=I_P) grid error
 8420   WRITE (MSG, 9842) 'integer', IY, HEAD 
        CALL ERROR (FFFATAL, 10, OUNIT, 0, 0, MSG)

        ! Real grid error
 8430   WRITE (MSG, 9842) 'floating-point', IY, HEAD  
        CALL ERROR (FFFATAL, 11, OUNIT, 0, 0, MSG)  
        
        ! VSS format data errors
 8600   WRITE (MSG, 9600) IDUM1, HEAD  
        CALL ERROR (FFFATAL, 16, OUNIT, 0, 0, MSG)  
        
        ! VSS soil physical property data errors
 8700   WRITE (MSG, 9700) ICOUNT, HEAD  
        CALL ERROR (FFFATAL, 14, OUNIT, 0, 0, MSG)  
        
        
        ! Format Statements ----------------------------------------------------
        
        ! -----------------
        ! Note: Take care not to exceed internal file length
 9000   FORMAT ( A, ' data file ', A, ': unit', I3: '; ', A )  
 9001   FORMAT ( 1X, A/ )  
 9002   FORMAT ( 'Title line mismatch: expected "', A,                          &
                 '" but found "',                   A, '"' )
 9410   FORMAT ( '(I7,1X,', I4, 'I1)' )  
 9600   FORMAT ( 'Reading VSS data for item no. ',I4, ' under title: ', A )
 9700   FORMAT ( 'Reading soils data for soil no. ',I4, ' under title: ', A )
 9801   FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
 9810   FORMAT ( 'Reading ', A, ' data under heading: ', A )  

 9842   FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  
 
    END SUBROUTINE ALREAD


    !> Reads and checks an AL input section header.
    !>
    !> `ALRED2` handles the shared part of the refactored AL reader family before
    !> type-specific records are read by [[ALREDC]], [[ALREDF]], [[ALREDI]], or
    !> [[ALREDL]]. The `FLAG` selects the legacy input option described in the
    !> manual's distributed-data formats.
    !>
    !> `FLAG=0` checks that the input file is open; any other value closes it.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931210 | ? | - | Initial version |
    !> | 19940916 | AB/RAH | - | Version 3.4.1 |
    !> | 19950322 | RAH | - | New header; replaced former ENTRY-point interface with `ALRED*` routines. |
    !> @endhistory
    SUBROUTINE ALRED2 (FLAG, IUNIT, OUNIT, LINE)  
        INTEGER(kind=I_P)   :: FLAG  !! File-management selector: `0` check open, otherwise close.
        INTEGER(kind=I_P)   :: IUNIT !! Input unit to inspect or close.
        INTEGER(kind=I_P)   :: OUNIT !! Output/error unit used for echoes and diagnostics.
        CHARACTER (LEN=*)   :: LINE  !! File-status label written in diagnostics.
!  sb change 011025     CHARACTER (80)      :: HEAD
!        CHARACTER(48)       :: FILNAM
!        CHARACTER(132)      :: MSG  
        CHARACTER (152)      :: HEAD   !! Current file-status message.
        CHARACTER(120)       :: FILNAM !! File name returned by `INQUIRE`.
        CHARACTER(200)       :: MSG    !! Error message buffer.
        LOGICAL              :: BOPEN  !! True when `IUNIT` is open.
        LOGICAL              :: BNAMED !! True when `IUNIT` has an associated filename.
        
        ! Code -----------------------------------------------------------------
    
        !
        ! File Management
        ! ---------------
        !
        ! Get file status and name
        INQUIRE (IUNIT, OPENED = BOPEN, NAMED = BNAMED, NAME = FILNAM)  
        IF (.NOT.BNAMED) FILNAM = '(no name)'  
    
        IF (FLAG == 0) THEN  
    
            ! Check that input file is open
            IF (.NOT.BOPEN) GOTO 8000  
            WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM  
            
        ELSE
            ! Close input file
            CLOSE (IUNIT)  
            WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM 
        ENDIF  
    
        ! HEAD now contains an informative message
        WRITE (OUNIT, 9001) HEAD  
    
        ! Store current title as old title
        HEAD0_alred2 = HEAD  
            
        RETURN  
    

        ! Errors ---------------------------------------------------------------

        ! File not open
 8000   WRITE (MSG, 9000) LINE, 'not open', IUNIT  
        CALL ERROR (FFFATAL, 4, OUNIT, 0, 0, MSG)  
    
        ! Formats --------------------------------------------------------------
 9000   FORMAT ( A, ' data file ', A, ': unit', I3: '; ', A )  

 9001   FORMAT ( 1X, A/ )  

    END SUBROUTINE ALRED2


    !> Reads character data for a legacy AL input option.
    !>
    !> `ALREDC` is the character-valued member of the `ALRED*` reader family. It
    !> applies the option selected by `FLAG`, reads from `IUNIT`, echoes to
    !> `OUNIT` as required, and stores values in `CDATA`.
    !>
    !> `FLAG` is retained for interface consistency with the other `ALRED*`
    !> routines but is not used by the current character reader.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931210 | ? | - | Initial version |
    !> | 19940916 | AB/RAH | - | Version 3.4.1 |
    !> | 19950322 | RAH | - | New header; replaced former ENTRY-point interface with `ALRED*` routines. |
    !> @endhistory
    SUBROUTINE ALREDC (FLAG, IUNIT, OUNIT, LINE, N1, N2, CDATA)  
        INTEGER(kind=I_P)   :: FLAG  !! Unused option selector retained for interface consistency.
        INTEGER(kind=I_P)   :: IUNIT !! Input unit positioned before the title line.
        INTEGER(kind=I_P)   :: OUNIT !! Output/error unit used for diagnostics.
        INTEGER(kind=I_P)   :: N1    !! First output dimension.
        INTEGER(kind=I_P)   :: N2    !! Second output dimension.
        CHARACTER (LEN=*)   :: LINE  !! Expected title-line substring.
        CHARACTER(LEN=*)    :: CDATA (N1, N2) !! Character data read from the next record.
!        CHARACTER(len=80)   :: HEAD
!        CHARACTER(len=132)  :: MSG  
! sb 011025
        CHARACTER(len=150)   :: HEAD !! Title line read from `IUNIT`.
        CHARACTER(len=200)   :: MSG  !! Error message buffer.

        ! Code -----------------------------------------------------------------

        READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
        IF (INDEX (HEAD, LINE)  == 0) THEN  
            WRITE (MSG, 9002) LINE, HEAD  
            CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  
        ENDIF  

        !  Read character data
        !  -------------------    
        READ (IUNIT, '(A)', ERR = 8100, END = 8100) CDATA  
    
        RETURN  


        ! Errors ---------------------------------------------------------------

        ! Title line read error
 8010   WRITE (MSG, 9801) LINE, HEAD0_alredc  
        CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
        
        ! Char data error
 8100   WRITE (MSG, 9810) 'character', HEAD  
        CALL ERROR (FFFATAL, 5, OUNIT, 0, 0, MSG)  
        

        ! Format ---------------------------------------------------------------

 9002   FORMAT ( 'Title line mismatch: expected "', A,                          & 
                 '" but found "',                   A, '"' )

 9801   FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
        
 9810   FORMAT ( 'Reading ', A, ' data under heading: ', A )  
        
 9842   FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  

    END SUBROUTINE ALREDC


    !> Reads real data for a legacy AL input option.
    !>
    !> `ALREDF` is the floating-point member of the `ALRED*` reader family. It
    !> applies the option selected by `FLAG`, reads from `IUNIT`, echoes to
    !> `OUNIT` as required, and stores values in `FDATA`.
    !>
    !> `FLAG=0` reads a simple free-format array. Any other value reads an
    !> indexed grid from row `N2` down to 1 and checks each row number.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931210 | ? | - | Initial version |
    !> | 19940916 | AB/RAH | - | Version 3.4.1 |
    !> | 19950322 | RAH | - | New header; replaced former ENTRY-point interface with `ALRED*` routines. |
    !> @endhistory
    SUBROUTINE ALREDF (FLAG, IUNIT, OUNIT, LINE, N1, N2, FDATA)  
        INTEGER(kind=I_P)   :: FLAG  !! `0` for simple array, non-zero for indexed grid.
        INTEGER(kind=I_P)   :: IUNIT !! Input unit positioned before the title line.
        INTEGER(kind=I_P)   :: OUNIT !! Output/error unit used for diagnostics.
        INTEGER(kind=I_P)   :: N1    !! First output dimension or number of grid columns.
        INTEGER(kind=I_P)   :: N2    !! Second output dimension or number of grid rows.
        CHARACTER (LEN=*)   :: LINE  !! Expected title-line substring.
        REAL(kind=R8P)      :: FDATA (N1, N2) !! Floating-point data read from the input file.
        INTEGER(kind=I_P)   :: iy !! Grid row index, read from north to south.
        INTEGER(kind=I_P)   :: ky !! Row number read from the input grid.
        INTEGER(kind=I_P)   :: ix !! Grid column index.
        CHARACTER(len=80)   :: HEAD !! Title line read from `IUNIT`.
        CHARACTER(len=132)  :: MSG  !! Error message buffer.

        ! Code =================================================================

        READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
        IF (INDEX (HEAD, LINE)  == 0) THEN  
            WRITE (MSG, 9002) LINE, HEAD  
            CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  
        ENDIF  

        ! Read floating-point data
        ! ------------------------
        IF (FLAG == 0) THEN  
            ! Simple array
            READ (IUNIT, *, ERR = 8300, END = 8300) FDATA  
        
        ELSE
            ! Grid-based array: read indexed rows, North to South
            DO IY = N2, 1, - 1  
                READ (IUNIT, *, ERR = 8430, END = 8430) KY, (FDATA (IX, IY),    &
                IX = 1, N1)
                IF (KY .NE. IY) GOTO 8430  
            END DO  
        ENDIF  
    
        RETURN  

        ! Error ----------------------------------------------------------------

        ! Title line read error
 8010   WRITE (MSG, 9801) LINE, HEAD0_alredf  
        CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
        
        ! Real data error
 8300   WRITE (MSG, 9810) 'floating-point', HEAD  
        CALL ERROR (FFFATAL, 7, OUNIT, 0, 0, MSG)  
        
        ! Real grid error
 8430   WRITE (MSG, 9842) 'floating-point', IY, HEAD  
        CALL ERROR (FFFATAL, 11, OUNIT, 0, 0, MSG)  
        
        ! Format ---------------------------------------------------------------
        !
        ! Note: Take care not to exceed internal file length
        !
 9002   FORMAT ( 'Title line mismatch: expected "', A,                          &
                 '" but found "',                   A, '"' )
        
 9801   FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
        
 9810   FORMAT ( 'Reading ', A, ' data under heading: ', A )  
    
 9842   FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  

    END SUBROUTINE ALREDF


    !> Reads integer data for a legacy AL input option.
    !>
    !> `ALREDI` is the integer-valued member of the `ALRED*` reader family. It
    !> applies the option selected by `FLAG`, reads from `IUNIT`, echoes to
    !> `OUNIT` as required, and stores values in `IDATA`.
    !>
    !> `FLAG=0` reads a simple free-format array. Non-zero `FLAG` reads an
    !> indexed grid from row `N2` down to 1; values use compact `I1` format when
    !> `FLAG < 10`, otherwise list-directed input.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931210 | ? | - | Initial version |
    !> | 19940916 | AB/RAH | - | Version 3.4.1 |
    !> | 19950322 | RAH | - | New header; replaced former ENTRY-point interface with `ALRED*` routines. |
    !> @endhistory
    SUBROUTINE ALREDI (FLAG, IUNIT, OUNIT, LINE, N1, N2, IDATA)
        INTEGER(kind=I_P)   :: FLAG  !! `0` for simple array, non-zero for indexed grid.
        INTEGER(kind=I_P)   :: IUNIT !! Input unit positioned before the title line.
        INTEGER(kind=I_P)   :: OUNIT !! Output/error unit used for diagnostics.
        INTEGER(kind=I_P)   :: N1    !! First output dimension or number of grid columns.
        INTEGER(kind=I_P)   :: N2    !! Second output dimension or number of grid rows.
        CHARACTER(LEN=*)    :: LINE  !! Expected title-line substring.
        INTEGER(kind=I_P)   :: IDATA (N1, N2) !! Integer data read from the input file.
        INTEGER(kind=I_P)   :: iy   !! Grid row index, read from north to south.
        INTEGER(kind=I_P)   :: ky   !! Row number read from the input grid.
        INTEGER(kind=I_P)   :: ix   !! Grid column index.
        CHARACTER(len=80)   :: HEAD !! Title line read from `IUNIT`.
        CHARACTER(len=17)   :: FORM !! Generated fixed-format integer-grid format.
        CHARACTER(len=132)  :: MSG  !! Error message buffer.
        
        ! Code -----------------------------------------------------------------

        READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
        IF (INDEX (HEAD, LINE)  == 0) THEN  
            WRITE (MSG, 9002) LINE, HEAD  
            CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  
        ENDIF  

        ! Read INTEGER(kind=I_P) data
        ! -----------------
        IF (FLAG == 0) THEN
            ! Simple array
            READ (IUNIT, *, ERR = 8200, END = 8200) IDATA  
        
        ELSE  
            ! Grid-based array: read indexed rows, North to South
            ! (using single digit integers if possible)
            IF (FLAG < 10) WRITE (FORM, 9410) N1  
            DO IY = N2, 1, - 1  
                IF (FLAG < 10) THEN  
                    READ (IUNIT, FORM, ERR = 8420, END = 8420) KY,              &
                        (IDATA (IX, IY), IX = 1, N1)
                ELSE  
                    READ (IUNIT, *, ERR = 8420, END = 8420) KY,                 &
                        (IDATA (IX, IY), IX = 1, N1)
                ENDIF  
                IF (KY .NE. IY) GOTO 8420
            END DO
        ENDIF  
    
        RETURN 


        ! Error ----------------------------------------------------------------

        ! Title line read error
 8010   WRITE (MSG, 9801) LINE, HEAD0_alredi  
        CALL ERROR (FFFATAL, 3, OUNIT, 0, 0, MSG)  
        
        ! INTEGER(kind=I_P) data error
 8200   WRITE (MSG, 9810) 'integer', HEAD  
        CALL ERROR (FFFATAL, 6, OUNIT, 0, 0, MSG)  
        
        ! INTEGER(kind=I_P) grid error
 8420   WRITE (MSG, 9842) 'integer', IY, HEAD  
        CALL ERROR (FFFATAL, 10, OUNIT, 0, 0, MSG)  
        

        ! Format ---------------------------------------------------------------
        !
        ! Note: Take care not to exceed internal file length
        !
        !
 9002   FORMAT ( 'Title line mismatch: expected "', A,                          &
          &      '" but found "',                   A, '"' )
        
 9410   FORMAT ( '(I7,1X,', I4, 'I1)' )  
        
 9801   FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
        
 9810   FORMAT ( 'Reading ', A, ' data under heading: ', A )  
    
 9842   FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )  
    
    END SUBROUTINE ALREDI


    !> Reads logical data for a legacy AL input option.
    !>
    !> `ALREDL` is the logical-valued member of the `ALRED*` reader family. It
    !> applies the option selected by `FLAG`, reads from `IUNIT`, echoes to
    !> `OUNIT` as required, and stores values in `LDATA`.
    !>
    !> `FLAG` is retained for interface consistency with the other `ALRED*`
    !> routines but is not used by the current logical reader.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | 19931210 | ? | - | Initial version |
    !> | 19940916 | AB/RAH | - | Version 3.4.1 |
    !> | 19950322 | RAH | - | New header; replaced former ENTRY-point interface with `ALRED*` routines. |
    !> @endhistory
    SUBROUTINE ALREDL (FLAG, IUNIT, OUNIT, LINE, N1, N2, LDATA)
        INTEGER(kind=I_P) :: FLAG  !! Unused option selector retained for interface consistency.
        INTEGER(kind=I_P) :: IUNIT !! Input unit positioned before the title line.
        INTEGER(kind=I_P) :: OUNIT !! Output/error unit used for diagnostics.
        INTEGER(kind=I_P) :: N1    !! First output dimension.
        INTEGER(kind=I_P) :: N2    !! Second output dimension.
        CHARACTER (LEN=*) :: LINE  !! Expected title-line substring.
        LOGICAL           :: LDATA (N1, N2) !! Logical data read from the input file.
        CHARACTER (80)    :: HEAD !! Title line read from `IUNIT`.
        CHARACTER(132)    :: MSG  !! Error message buffer.

        ! Code -----------------------------------------------------------------

        READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD  
        IF (INDEX (HEAD, LINE)  == 0) THEN  
            WRITE (MSG, 9002) LINE, HEAD  
            CALL ERROR (WWWARN, 2, OUNIT, 0, 0, MSG)  
        ENDIF  

        ! Read logical data
        ! -----------------
        READ (IUNIT, *, ERR = 8600, END = 8600) LDATA  
    
        RETURN  


        ! Error ----------------------------------------------------------------

        ! Title line read error
 8010   WRITE (MSG, 9801) LINE, HEAD0_ALREDL  
        CALL ERROR(FFFATAL, 3, OUNIT, 0, 0, MSG)  
        
        ! Logical data error
 8600   WRITE (MSG, 9810) 'logical', HEAD  
        CALL ERROR(FFFATAL, 14, OUNIT, 0, 0, MSG)  
        

        ! Format ---------------------------------------------------------------
        !
        ! Note: Take care not to exceed internal file length
        !
        !
 9002   FORMAT ( 'Title line mismatch: expected "', A,                          &
                 '" but found "',                   A, '"' )
        
 9801   FORMAT ( 'Reading heading: ', A, '; last item was: ', A )  
    
 9810   FORMAT ( 'Reading ', A, ' data under heading: ', A )  
        
    END SUBROUTINE ALREDL


    !> Chooses an approximately even subsequence from a longer sequence.
    !>
    !> For `M` requested items from `N` available items, `ALSPRD` returns the
    !> first index `N1` and stride `DEL` for a representative subsequence. The
    !> routine is used by AL input/output helpers when only a subset of entries
    !> should be printed.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | ? | ? | - | Initial version |
    !> | 19970805 | RAH | - | 4.1  Create. |
    !> @endhistory
    SUBROUTINE ALSPRD (M, N, N1, DEL) 
        INTEGER(kind=I_P) :: M   !! Requested number of items in the printed subsequence.
        INTEGER(kind=I_P) :: N   !! Number of available items in the full sequence.
        INTEGER(kind=I_P) :: N1  !! First selected index.
        INTEGER(kind=I_P) :: DEL !! Stride between selected indices.
        INTEGER(kind=I_P) :: DNE   !! Candidate increment for the number of excluded items.
        INTEGER(kind=I_P) :: MM    !! `M-1`, the number of printed intervals.
        INTEGER(kind=I_P) :: NE    !! Number of excluded/outlying items.
        INTEGER(kind=I_P) :: NEMAX !! Maximum useful excluded-item count adjustment.
        INTEGER(kind=I_P) :: NF    !! Alternative excluded-item count.
        LOGICAL           :: TEST  !! True when `NF` gives a more even spread.


        ! Code -----------------------------------------------------------------
    
        IF (M <= 1) THEN  
           N1  = N / (MAX (0, M) + 1) + 1  
           DEL = N  
    
        ELSE  
            ! set the number NE of out-lying items - even if possible
            MM   = M - 1  
            NE   = MOD (N - 1, MM)  
            NF   = NE+MM  
            TEST = (MOD(NE, 2) == 1)  .AND.                                     &
                   (MOD(NF, 2)  == 0) .AND.                                     &
                   (NF <= N - M)
    
            IF (TEST) NE = NF  
            
            ! add a few if it makes a more uniform spread
            DNE   = MM * (1 + MOD (MM, 2) * (1 - MOD (NE, 2) ) )  
            NEMAX = 2 * (N - M) / (M + 1)  
    
            NE = NE+ (IDIMJE(NEMAX, NE) / DNE) * DNE  
            ! round up
            N1 = 1 + (NE+1) / 2  
    
            DEL = (N - NE-1) / MM
        ENDIF  
    END SUBROUTINE ALSPRD


    !> Initialises legacy floating-point exception handling.
    !>
    !> `ALTRAP` is retained as the AL-layer hook for enabling floating-point
    !> traps. In the current PC-oriented code path the original IEEE setup calls
    !> have been removed, so the routine only preserves the historical interface.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | ? | ? | - | Initial version |
    !> | 19940930 | RAH | - | Version 3.4.1 created. |
    !> | 20000307 | StevenB | - | Version 4g-pc remove ieee calls |
    !> @endhistory
    SUBROUTINE ALTRAP ()  
        INTEGER(kind=I_P), parameter :: OUT = 0 !! Output unit used if trap setup fails.
        INTEGER(kind=I_P) :: I !! Legacy trap setup status; currently forced to zero.
        
        ! Code -----------------------------------------------------------------

        !   I = IEEE_HANDLER( 'set', 'common', ABORT )
        I = 0  
        IF (I .NE. 0) CALL ERROR(WWWARN, 13, OUT, 0, 0,                         &
                        'Could not set traps for floating-point exceptions')

        RETURN
    END SUBROUTINE ALTRAP

END MODULE mod_load_filedata
