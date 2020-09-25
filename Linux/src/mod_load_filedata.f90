!-------------------------------------------------------------------------------
!
!> @file mod_load_filedata.f90 
! 
!> @author AB / RAH, Newcastle University
!> @author JE, Newcastle University
!> @author Stephen Birkinshaw, Newcastle University
! 
!> @brief Gets the input filename(s) and methdos for reading data from files.
!
!> @todo figure out for each method what the variable intents are.
!> @todo replace the GOTO-jumps to outisde a loop with EXIT
!> @todo replace the _set var_ then _overwrite, if_ if _if_ or _case_ statements
!> @todo replace the array init with the now standard way (subroutine ALINIT)
!> @todo combine / clean ALREAD, ALRED2, ALREDI, ALREDF, ALREDL, ALREDC
!> @todo use DIMENSION in variable def
!> @todo is ALTRAP still necessary?
!
! REVISION HISTORY:
! ?        - ?     - ?
! 201208?? - JE    - F90-conversion: replaces the AL*.F files
! 20200305 - SvenB - formatting & some doc-strings
!                  - renamed NCAT, NTABEE, CCELL, NCATEE, CTAB, DTAB
! 
!-------------------------------------------------------------------------------
MODULE mod_load_filedata

    USE SGLOBAL
    use mod_parameters

    IMPLICIT NONE

    CHARACTER(len=80)   :: HEAD0_alread='( nothing read yet )',                 &
                           HEAD0_alredc='( nothing read yet )',                 &
                           HEAD0_alredi='( nothing read yet )',                 &
                           HEAD0_alred2='( nothing read yet )',                 &
                           HEAD0_alredl='( nothing read yet )',                 &
                           HEAD0_alredf='( nothing read yet )'
    

    ! --------------------------------------------------------------------------
    ! Private by default 
    PRIVATE

    ! --------------------------------------------------------------------------
    ! Public methods
    PUBLIC :: ALREAD, ALALLF, ALCHKI, ALCHK, ALINIT, ALSPRD, ALTRAP,            &
              ALINTP, ALREDL, ALREDF, ALALLI, ALRED2, ALREDC, ALREDI

    
    ! Code =====================================================================

    CONTAINS

  
    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Set a floating-point array for all elements (FLAG=0) or all
    !! column elements (FLAG=1), by reading from an input data file.
    ! 
    ! Notes: can be found in SSR74
    !
    ! REVISION HISTORY:
    ! 19940527 - ?      - Initial version
    ! 19940919 - AB/RAH - v3.4.1
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALALLF (FLAG, N2, MINCAT, IUNIT, OUNIT, LINE, NEL, NLF,          &
                       NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF,  &
                       BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  AEL, IDUM, DUMMY)

        ! IO-vars
        INTEGER(kind=I_P)   :: FLAG, N2, MINCAT, IUNIT, OUNIT  
        INTEGER(kind=I_P)   :: NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE  
        INTEGER(kind=I_P)   :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2),              &
                               ICMREF (NELEE, 4, 2:2)
        LOGICAL             :: BEXBK, LINKNS (NLF)  
        CHARACTER (LEN=*)   :: LINE  
        !
        ! Output arguments
        INTEGER(kind=I_P)   :: NUM_CATEGORIES_TYPES  
        REAL(kind=R8P)      :: AEL (1 + NLF * (FLAG / N2) :                     &
                                    NELEE- (NELEE-NEL) * (1 / N2), N2)
        
        ! Other vars
        INTEGER(kind=I_P), DIMENSION(NXEE*NYEE) :: IDUM  
        REAL(kind=R8P)      :: DUMMY (NELEE)  
        
        INTEGER(kind=I_P)   :: I1, I2, ICAT, IDUM0, IEL, LN, N, X, XY0, Y  
        LOGICAL             :: BLINK  
        CHARACTER           :: CDUM
        CHARACTER(len=132)  :: MSG
        CHARACTER(len=8)    :: NEXT  
        
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Reads the catagory type for each grid, bank elements take the
    !! same category type as the adjacent grid element
    !! There must be nine or fewer category types
    ! 
    ! Note: Version 4.2
    !
    ! REVISION HISTORY:
    ! ?        - ?      - Initial version
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALALLI (NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NEL, NLF, NX,  &
                       NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK,     &
                       LINKNS, CATTYP, IDUM)
    
        ! INPUT ARGUMENTS
        INTEGER(kind=I_P)   :: NUM_CATEGORIES_TYPES !< number of category types
        INTEGER(kind=I_P)   :: IUNIT, OUNIT, NEL, NLF, NX, NY, NELEE, NLFEE, NXEE
        INTEGER(kind=I_P)   :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2),              &
                               ICMREF (NELEE, 4, & 2:2)
        LOGICAL             :: BEXBK, LINKNS (NLFEE)  
        CHARACTER (LEN=*)   :: LINE  
        
        ! OUPUT ARGUMENTS
        INTEGER(kind=I_P)   :: CATTYP (NLF + 1:NEL)  
        
        ! WORKSPACE ARGUMENTS
        INTEGER, DIMENSION(:), INTENT(IN)   :: IDUM  
        
        ! LOCALS ETC.
        !
        ! integers from ALBANK
        INTEGER(kind=I_P)   :: BANK1, BANK2, FACE1, FACE2, GRID1, GRID2, ISNS,  &
                               LINK
        ! integers from ALALLF
        INTEGER(kind=I_P)   :: ICAT, IEL, X, XY0, Y  
        
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
                                    'Error in NCELEM in :MN43 in MN data file')
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Assign values to the bank elements of an array by copying values
    !! associated with neighboring grid elements in the same array
    ! 
    ! Note: SSR51
    !
    ! REVISION HISTORY:
    ! 19940422 - ?      - Initial version
    ! 19940523 - AB/RAH - Version 3.4.1
    !
    !> @param[in]     NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF
    !> @param[inout]  A
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, A)
        
        ! Input arguments
        INTEGER(kind=I_P), INTENT(IN)   :: NEL, NLF, NLFEE, NELEE,              &
                                           ICMBK (NLFEE, 2),                    &
                                           ICMREF (NELEE, 4, 2:2)
        LOGICAL, INTENT(IN)             :: LINKNS (NLF)  

        !
        ! Input/output arguments
        REAL(kind=R8P), INTENT(INOUT)   :: A (NLF + 1:NEL)  

        !
        ! Locals, etc
        INTEGER(kind=I_P)   :: BANK1, BANK2, FACE1, FACE2, GRID1, GRID2, ISNS,  &
                               LINK  
    
    
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Check that a given relation holds between subject and object
    !! arrays, and take corrective action and/or raise an error in the
    !! event of a failure.
    ! 
    ! Note: SSR62
    !
    !---------------------------------------------------------------------------
    !  CAUTION!  Source code for ALCHKI is generated from ALCHK using make:
    !  ''''''''  check the makefile before modifying this subroutine.
    !---------------------------------------------------------------------------
    !
    ! REVISION HISTORY:
    ! 19940722 - ?      - Initial version
    ! 19940817 - AB/RAH - Version 3.4.1
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALCHK (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME,           &
                      OP, OBJ, TOL, SUBJ, COUNT, NOTOK)
                      
        ! Input arguments
        INTEGER(kind=I_P)               :: ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3  
        CHARACTER(LEN=*), INTENT(IN)    :: SNAME, OP  
        REAL(kind=R8P)                  :: OBJ (N0: * ), TOL  
        
        ! Input/output arguments
        REAL(kind=R8P)      :: SUBJ (N0:N1)  
        INTEGER(kind=I_P)   :: COUNT
        
        ! Workspace arguments
        LOGICAL             :: NOTOK (N0:N1)  
        
        ! Locals, etc
        INTEGER(kind=I_P)   :: COUNT0, COUNT1, I, INCOBJ, IOBJ, IX (3), NDIM  
        INTEGER(kind=I_P)   :: P, POS1, POS2, SGN, SLEN
        REAL(kind=R8P)      :: SB, OB, rrr
        LOGICAL             :: BRESET  
        CHARACTER(len=9)    :: CACT
        CHARACTER(len=132)  :: MSG
        CHARACTER           :: OP1, OP2  
    
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !!  Check that a given relation holds between subject and object
    !!  arrays, and take corrective action and/or raise an error in the
    !!  event of a failure.
    ! 
    ! Note: SSR62
    !
    !---------------------------------------------------------------------------
    !  CAUTION!  Source code for ALCHKI is generated from ALCHK using make:
    !  ''''''''  check the makefile before modifying this subroutine.
    !---------------------------------------------------------------------------
    !
    ! REVISION HISTORY:
    ! 19940722 - ?      - Initial version
    ! 19940817 - AB/RAH - Version 3.4.1
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALCHKI (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME,          &
                       OP, OBJ, SUBJ, COUNT, NOTOK)
     
        ! Input arguments
        INTEGER(kind=I_P)   :: ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3  
        CHARACTER (LEN=*)   :: SNAME, OP  
        INTEGER(kind=I_P)   :: OBJ (N0: * ) 
        
        ! Input/output arguments
        INTEGER(kind=I_P)   :: SUBJ (N0:N1)  
        INTEGER(kind=I_P)   :: COUNT  
        
        ! Workspace arguments
        LOGICAL     :: NOTOK (N0:N1)  
        
        ! Locals, etc
        INTEGER(kind=I_P)   :: COUNT0, COUNT1, I, INCOBJ, IOBJ, IX (3), NDIM  
        INTEGER(kind=I_P)   :: P, POS1, POS2, SGN, SLEN  
        INTEGER(kind=I_P)   :: SB, OB, iii  
        LOGICAL             :: BRESET  
        CHARACTER(len=9)    :: CACT
        CHARACTER(len=132)  :: MSG
        CHARACTER           :: OP1, OP2  
        
        
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Initialize an array with a given value
    ! 
    ! Note: SSR67
    !
    ! REVISION HISTORY:
    ! 19931208 - ?      - Initial version
    ! 19940523 - AB/RAH - Version 3.4.1
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALINIT (ALPHA, N, X)  
        
        ! Input arguments
        REAL(kind=R8P)      :: ALPHA  
        INTEGER(kind=I_P)   :: N  
        
        ! Output arguments
        REAL(kind=R8P)      :: X (N)  
        
        ! Locals, etc
        INTEGER(kind=I_P)   :: I  
    
        ! Code =================================================================
        
        DO I = 1, N  
            X (I) = ALPHA  
        END DO  

    END SUBROUTINE ALINIT


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! For each category type, a table of values is known. This contains
    !! the depth (TABLE_WATER_DEPTH) and the concentration (TABLE_CONCENTRATION) 
    !! at each depth. The concentrationin each cell (CELL_CONCENTRATION) is 
    !! calculated by linear interpolation.
    !! NUM_CATEGORIES_TYPES is the number of category types. 
    !! MAX_NUM_CATEGORY_TYPES is the maximum number of category types. 
    !! MAX_NUM_DATA_PAIRS is the maximum number of pairs of data in each table.
    !!
    !! The depths in the table must start at zero and increase.
    ! 
    ! Note: SSR51
    !
    ! REVISION HISTORY:
    ! ?        - ?      - Initial version
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALINTP (LLEE, NCETOP, NEL, NELEE, NLF, NUM_CATEGORIES_TYPES,     &
                       MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY,      &
                       NCOLMB, NTAB,TABLE_CONCENTRATION, TABLE_WATER_DEPTH,     &
                       DELTAZ, ZVSNOD, CELL_CONCENTRATION)
     
        ! INPUT ARGUMENTS
        INTEGER(kind=I_P)   :: LLEE, NCETOP, NEL, NELEE, NLF
        INTEGER(kind=I_P)   :: NUM_CATEGORIES_TYPES     !< number of category types 
        INTEGER(kind=I_P)   :: MAX_NUM_CATEGORY_TYPES   !< maximum number of category types
        INTEGER(kind=I_P)   :: MAX_NUM_DATA_PAIRS       !< maximum number of data pairs
        INTEGER(kind=I_P)   :: NCATTY (NLF + 1:NEL), NCOLMB (NLF + 1:NEL)  
        INTEGER(kind=I_P)   :: NTAB (NUM_CATEGORIES_TYPES)  
        REAL(kind=R8P)      :: TABLE_CONCENTRATION                              &
                                (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS)    !< table of concentrations
        REAL(kind=R8P)      :: TABLE_WATER_DEPTH                                &
                                (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS)    !< table of water depths
        REAL(kind=R8P)      :: DELTAZ (LLEE, NELEE), ZVSNOD (LLEE, NELEE)  
        
        ! OUTPUT ARGUMENTS
        REAL(kind=R8P)      :: CELL_CONCENTRATION (NELEE, LLEE) !< concentration in each cell 
        
        ! LOCALS ETC.
        INTEGER(kind=I_P)   :: NCL, NELM, NCATG, NINTB, NTABLE, NTHRTB  
        REAL(kind=R8P)      :: DEPTH  
        

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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Utility routine to handle an input data file (CHAR, INT, REAL)
    !
    !> @todo replace ID numbers with named parameter values for better legibility
    !
    ! REVISION HISTORY:
    ! 19931210 - ?      - Initial version
    ! 19940912 - GP     - 4.0  Add VSS options (FLAG=) 6 & 7.
    ! 19940916 - AB/RAH - Version 3.4.1
    ! 19970804 - RAH    - 4.1  Add END specifiers to READs in options 6 & 7.
    !                     Renumber error 13 as 16 (was unauthorized).
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALREAD (FLAG, IUNIT, OUNIT, LINE, N1, N2, NUM_CATEGORIES_TYPES,  &
                       CDATA, IDATA, RDATA)
                       
        ! Input arguments
        INTEGER(kind=I_P)   :: FLAG, IUNIT, OUNIT, N1, N2, NUM_CATEGORIES_TYPES  
        CHARACTER (LEN=*)   :: LINE  

        ! Output arguments
        CHARACTER (LEN=*)   :: CDATA  
        INTEGER(kind=I_P)   :: IDATA (N1, N2) 
        REAL(kind=R8P)      :: RDATA (N1, N2)  

        ! Locals, etc
        CHARACTER (LEN=80)  :: HEAD, MSG * 132, FILNAM * 48, FORM * 17
        INTEGER(kind=I_P)   :: IX, IY, KY, IDUM1, IDUM2, ICOUNT, I  
        LOGICAL             :: BOPEN, BNAMED  

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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Utility routine to handle an input data file
    !!    
    !! !!!  NB  This subroutine contains ENTRY statements  !!!
    !
    ! REVISION HISTORY:
    ! 19931210 - ?      - Initial version
    ! 19940916 - AB/RAH - Version 3.4.1
    ! 19950322 - RAH    - New header.
    !                     Remove arguments N1,...,RDATA & create ENTRY
    !                     points ALRDI, etc, including new option ALRDL
    !                     (note: arg NUM_CATEGORIES_TYPES removed; 
    !                     CDATA now an array;
    !                     RDATA renamed FDATA).
    !---------------------------------------------------------------------------
    SUBROUTINE ALRED2 (FLAG, IUNIT, OUNIT, LINE)  

        ! Input arguments
        INTEGER(kind=I_P)   :: FLAG, IUNIT, OUNIT  
        CHARACTER (LEN=*)   :: LINE  
        CHARACTER (80)      :: HEAD
        CHARACTER(48)       :: FILNAM
        CHARACTER(132)      :: MSG  
        LOGICAL             :: BOPEN, BNAMED  
        
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Utility routine to handle an input data file (CHAR)´.
    !
    ! REVISION HISTORY:
    ! 19931210 - ?      - Initial version
    ! 19940916 - AB/RAH - Version 3.4.1
    ! 19950322 - RAH    - New header.
    !                     Remove arguments N1,...,RDATA & create ENTRY
    !                     points ALRDI, etc, including new option ALRDL
    !                     (note: arg NUM_CATEGORIES_TYPES removed; CDATA now an array;
    !                     RDATA renamed FDATA).
    !---------------------------------------------------------------------------  
    SUBROUTINE ALREDC (FLAG, IUNIT, OUNIT, LINE, N1, N2, CDATA)  
        
        ! Input arguments
        INTEGER(kind=I_P)   :: FLAG, IUNIT, OUNIT  
        INTEGER(kind=I_P)   :: N1, N2  
        CHARACTER (LEN=*)   :: LINE  

        ! Output arguments
        CHARACTER(LEN=*)    :: CDATA (N1, N2)  
        CHARACTER(len=80)   :: HEAD
        CHARACTER(len=132)  :: MSG  

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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Utility routine to handle an input data file (REAL).
    !
    ! REVISION HISTORY:
    ! 19931210 - ?      - Initial version
    ! 19940916 - AB/RAH - Version 3.4.1
    ! 19950322 - RAH    - New header.
    !                     Remove arguments N1,...,RDATA & create ENTRY
    !                     points ALRDI, etc, including new option ALRDL
    !                     (note: arg NUM_CATEGORIES_TYPES removed; 
    !                     CDATA now an array;
    !                     RDATA renamed FDATA).
    !---------------------------------------------------------------------------
    SUBROUTINE ALREDF (FLAG, IUNIT, OUNIT, LINE, N1, N2, FDATA)  

        ! Input arguments
        INTEGER(kind=I_P)   :: FLAG, IUNIT, OUNIT  
        INTEGER(kind=I_P)   :: N1, N2  
        CHARACTER (LEN=*)   :: LINE  

        ! Output arguments
        REAL(kind=R8P)      :: FDATA (N1, N2)  

        ! Locals, etc
        INTEGER(kind=I_P)   :: iy, ky, ix

        CHARACTER(len=80)   :: HEAD
        CHARACTER(len=132)  :: MSG  

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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Utility routine to handle an input data file
    !
    ! REVISION HISTORY:
    ! 19931210 - ?      - Initial version
    ! 19940916 - AB/RAH - Version 3.4.1
    ! 19950322 - RAH    - New header.
    !                     Remove arguments N1,...,RDATA & create ENTRY
    !                     points ALRDI, etc, including new option ALRDL
    !                     (note: arg NUM_CATEGORIES_TYPES removed; CDATA now an array;
    !                     RDATA renamed FDATA).
    !---------------------------------------------------------------------------
    SUBROUTINE ALREDI (FLAG, IUNIT, OUNIT, LINE, N1, N2, IDATA)

        ! Input arguments
        INTEGER(kind=I_P)   :: FLAG, IUNIT, OUNIT  
        INTEGER(kind=I_P)   :: N1, N2  
        CHARACTER(LEN=*)    :: LINE  

        ! Output arguments
        INTEGER(kind=I_P)   :: IDATA (N1, N2)  

        ! Locals, etc
        INTEGER(kind=I_P)   :: iy, ky, ix
        CHARACTER(len=80)   :: HEAD
        CHARACTER(len=17)   :: FORM
        CHARACTER(len=132)  :: MSG 
        
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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Utility routine to handle an input data file
    !
    ! REVISION HISTORY:
    ! 19931210 - ?      - Initial version
    ! 19940916 - AB/RAH - Version 3.4.1
    ! 19950322 - RAH    - New header.
    !                     Remove arguments N1,...,RDATA & create ENTRY
    !                     points ALRDI, etc, including new option ALRDL
    !                     (note: arg NUM_CATEGORIES_TYPES removed; CDATA now an array;
    !                     RDATA renamed FDATA).
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALREDL (FLAG, IUNIT, OUNIT, LINE, N1, N2, LDATA)

        ! Input arguments
        INTEGER(kind=I_P) :: FLAG, IUNIT, OUNIT  
        INTEGER(kind=I_P) :: N1, N2  
        CHARACTER (LEN=*) :: LINE  

        ! Output arguments
        LOGICAL :: LDATA (N1, N2)  
        CHARACTER (80) :: HEAD
        CHARACTER(132) :: MSG 

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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Choose a sub-sequence of M items from a sequence of N items:
    !! N1 is the starting index;  DEL is the stride
    !
    ! REVISION HISTORY:
    ! ?        - ?      - Initial version
    ! 19970805 - RAH    - 4.1  Create.
    !---------------------------------------------------------------------------
    SUBROUTINE ALSPRD (M, N, N1, DEL) 
    
        ! Input arguments
        INTEGER(kind=I_P) :: M, N  
        
        ! Output arguments
        INTEGER(kind=I_P) :: N1, DEL  
        
        ! Locals, etc
        INTEGER(kind=I_P) :: DNE, MM, NE, NEMAX, NF  

        LOGICAL :: TEST  


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


    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Set traps for floating-point exceptions
    !
    ! Note: SSR79
    !
    ! REVISION HISTORY:
    ! ?        - ?       - Initial version
    ! 19940930 - RAH     - Version 3.4.1 created.
    ! 20000307 - StevenB - Version 4g-pc remove ieee calls
    !--------------------------------------------------------------------------- 
    SUBROUTINE ALTRAP ()  

        ! Locals, etc
        INTEGER(kind=I_P), parameter :: OUT = 0

        INTEGER(kind=I_P) :: I  
        
        ! Code -----------------------------------------------------------------

        !   I = IEEE_HANDLER( 'set', 'common', ABORT )
        I = 0  
        IF (I .NE. 0) CALL ERROR(WWWARN, 13, OUT, 0, 0,                         &
                        'Could not set traps for floating-point exceptions')

        RETURN
    END SUBROUTINE ALTRAP

END MODULE mod_load_filedata