!code inserted in INCLUDE_* functions
INTEGER, INTENT(IN)            :: n       !increment
INTEGER                        :: sz
IF(ASSOCIATED(s)) THEN ; sz=SIZE(s) ; old=>s ; NULLIFY(s) ; ELSE ; sz=0 ; ENDIF
ALLOCATE(s(sz+n))
IF(sz>0) THEN ; s(1:sz)=old ; DEALLOCATE(old) ; ENDIF
!IF(PRESENT(default)) s(sz+1:) = default
