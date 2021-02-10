!code inserted in EXTEND_*s functions
INTEGER  :: sz, n   !increment
n=SIZE(ext)
IF(n>0) THEN
    CALL INCREMENT(s, n)
    sz = SIZE(s)
    s(sz-n+1:) = ext
ENDIF
