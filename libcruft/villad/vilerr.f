      SUBROUTINE VILERR ( IER, LSTOP )
C
      INTEGER  IER
      LOGICAL  LSTOP
C
C***********************************************************************
C
C     THIS SUBROUTINE HANDLES ERRORS FOR THE SUBROUTINES JCOBI, DFOPR,
C     INTRP, AND RADAU GIVEN BY VILLADSEN AND MICHELSEN.
C
C     PARAMETER LIST:
C
C       IER    : ERROR NUMBER
C       LSTOP  : LOGICAL FLAG
C
C                LSTOP = .TRUE.   ==>  FATAL ERROR, PROGRAM TERMINATION
C                LSTOP = .FALSE.  ==>  WARNING ERROR, NORMAL RETURN
C
C     COMMON BLOCKS:      NONE
C
C     REQUIRED ROUTINES:  NONE
C
C***********************************************************************
C
C -- BEGIN
C
      IF      ( IER .EQ.  1) THEN
C
        WRITE(*,*) '** VILERR : Illegal value for N0 '
C
      ELSE IF ( IER .EQ.  2) THEN
C
        WRITE(*,*) '** VILERR : Illegal value for N1 '
C
      ELSE IF ( IER .EQ.  3 ) THEN
C
        WRITE(*,*) '** VILERR : Insufficient dimension for problem '
C
      ELSE IF ( IER .EQ.  4 ) THEN
C
        WRITE(*,*) '** VILERR : Index less than zero in DFOPR '
C
      ELSE IF ( IER .EQ.  5 ) THEN
C
        WRITE(*,*) '** VILERR : Index greater than NTOTAL in DFOPR '
C
      ELSE IF ( IER .EQ.  6 ) THEN
C
        WRITE(*,*) '** VILERR : Illegal ID in DFOPR '
C
      ELSE IF ( IER .EQ.  7 ) THEN
C
        WRITE(*,*) '** VILERR : Number of interpolation points '
        WRITE(*,*) '            less than 1 '
C
      ELSE IF ( IER .EQ.  8 ) THEN
C
        WRITE(*,*) '** VILERR : Illegal ID in RADAU '
C
      ELSE IF ( IER .EQ.  9 ) THEN
C
        WRITE(*,*) '** VILERR : ID = 1 but N1 not equal to 1 in RADAU '
C
      ELSE IF ( IER .EQ. 10 ) THEN
C
        WRITE(*,*) '** VILERR : ID = 2 but N0 not equal to 1 in RADAU '
C
      ELSE IF ( IER .EQ. 11 ) THEN
C
        WRITE(*,*) '** VILERR : ID = 3 but N0 not equal to 1 or '
        WRITE(*,*) '            N1 not equal to 1 in RADAU '
C
      ELSE
C
        WRITE(*,*) 'UNRECOGNIZED ERROR FLAG SET FOR VILERR '
C
      END IF
C
      IF ( LSTOP ) THEN
C
C -- PROGRAM EXECUTION TERMINATES HERE
C
        CALL XSTOPX (' ')
C
      ELSE
      END IF
C
      RETURN
      END
