      SUBROUTINE RADAU
     +  (
     +  ND, N, N0, N1, ID, ALPHA, BETA, ROOT, DIF1, VECT
     +  )
C
      INTEGER           ND, N, N0, N1, ID
      DOUBLE PRECISION  ALPHA, BETA, ROOT(ND), DIF1(ND), VECT(ND)
C
C***********************************************************************
C
C     RADAU OR LOBATTO QUADRATURE
C
C     VILLADSEN AND MICHELSEN, PAGES 133-135, 419
C
C     INPUT PARAMETERS:
C
C       ND     : THE DIMENSION OF THE VECTORS DIF1, DIF2, DIF3, AND ROOT
C
C       N      : THE DEGREE OF THE JACOBI POLYNOMIAL, (i.e. THE NUMBER
C                OF INTERIOR INTERPOLATION POINTS)
C
C       N0     : DETERMINES WHETHER X = 0 IS INCLUDED AS AN
C                INTERPOLATION POINT
C
C                  N0 = 0  ==>  X = 0 IS NOT INCLUDED
C                  N0 = 1  ==>  X = 0 IS INCLUDED
C
C       N1     : DETERMINES WHETHER X = 1 IS INCLUDED AS AN
C                INTERPOLATION POINT
C
C                  N1 = 0  ==>  X = 1 IS NOT INCLUDED
C                  N1 = 1  ==>  X = 1 IS INCLUDED
C
C       ID     : INDICATOR
C
C                  ID = 1  ==>  RADAU QUADRATURE WEIGHTS INCLUDING X = 1
C                  ID = 2  ==>  RADAU QUADRATURE WEIGHTS INCLUDING X = 0
C                  ID = 3  ==>  LOBATTO QUADRATURE WEIGHTS INCLUDING
C                               BOTH X = 0 AND X = 1
C
C       ALPHA  : THE VALUE OF ALPHA IN THE DESCRIPTION OF THE JACOBI
C                POLYNOMIAL
C
C       BETA   : THE VALUE OF BETA IN THE DESCRIPTION OF THE JACOBI
C                POLYNOMIAL
C
C                FOR A MORE COMPLETE EXPLANATION OF ALPHA AN BETA, SEE
C                VILLADSEN AND MICHELSEN, PAGES 57 TO 59
C
C       ROOT   : ONE DIMENSIONAL VECTOR CONTAINING ON EXIT THE
C                N + N0 + N1 ZEROS OF THE NODE POLYNOMIAL USED IN THE
C                INTERPOLATION ROUTINE
C
C       DIF1   : ONE DIMENSIONAL VECTOR CONTAINING THE FIRST DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C       THE NODE POLYNOMIAL IS GIVEN BY
C
C                     N0    (ALPHA',BETA')          N1
C         P  (X)  =  X   * P (X)           * (X - 1)
C          NT               N
C
C       THE ARGUMENTS ALPHA' AND BETA' TO BE USED IN JCOBI FOR
C       CALCULATION OF ROOT AND DIF1 DEPEND ON WHETHER X = 0 , X = 1 OR
C       BOTH ARE USED AS EXTRA QUADRATURE POINTS.  THUS:
C
C         ID = 1:  ALPHA' = ALPHA + 1, BETA' = BETA
C         ID = 2:  ALPHA' = ALPHA    , BETA' = BETA + 1
C         ID = 3:  ALPHA' = ALPHA + 1, BETA' = BETA + 1
C
C       NOTE:
C
C         ID = 1  REQUIRES THAT N0 = 0 OR 1, N1 = 1
C         ID = 2  REQUIRES THAT N0 = 1     , N1 = 0 OR 1
C         ID = 3  REQUIRES THAT N0 = 1     , N1 = 1
C
C     OUTPUT PARAMETERS:
C
C       VECT   : VECTOR OF THE NT COMPUTED QUADRATURE WEIGHTS,
C                NORMALIZED SUCH THAT
C
C                   NT
C                  SUM  VECT(I) = 1
C                  I=1
C
C                FOR A MORE COMPLETE EXPLANATION SEE VILLADSEN AND
C                MICHELSEN, PAGES 133 TO 135
C
C     COMMON BLOCKS:      NONE
C
C     REQUIRED ROUTINES:  VILERR
C
C***********************************************************************
C
      INTEGER           I,NT,IER
      DOUBLE PRECISION  AX,S,X
      DOUBLE PRECISION  ZERO,ONE
      LOGICAL           LSTOP
C
      PARAMETER ( ZERO = 0.0D+00, ONE = 1.0D+00 )
C
C -- ERROR CHECKING
C
      IF ((N0 .NE. 0) .AND. (N0 .NE. 1)) THEN
        IER   = 1
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((N1 .NE. 0) .AND. (N1 .NE. 1)) THEN
        IER   = 2
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF (ND .LT. (N + N0 + N1)) THEN
        IER   = 3
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((N + N0 + N1) .LT. 1) THEN
        IER   = 7
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((ID .NE. 1) .AND. (ID.NE. 2) .AND. (ID .NE. 3)) THEN
        IER   = 8
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((ID .EQ. 1) .AND. (N1 .NE. 1)) THEN
        IER   = 9
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((ID .EQ. 2) .AND. (N0 .NE. 1)) THEN
        IER   = 10
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((ID .EQ. 3) .AND. ((N0 .NE. 1) .OR. (N1 .NE. 1))) THEN
        IER   = 11
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
C -- EVALUATE RADAU OR LOBATTO QUADRATURE WEIGHTS
C
      S  = ZERO
      NT = N + N0 + N1
C
      DO 40 I = 1,NT
C
        X = ROOT(I)
C
        IF      (ID .EQ. 1) THEN
          AX = X
          IF (N0 .EQ. 0) THEN
            AX = ONE/AX
          ELSE
          END IF
        ELSE IF (ID .EQ. 2) THEN
          AX = ONE - X
          IF (N1 .EQ. 0) THEN
            AX = ONE/AX
          ELSE
          END IF
        ELSE IF (ID .EQ. 3) THEN
          AX = ONE
        ELSE
        END IF
C
        VECT(I) = AX/DIF1(I)**2
C
   40 CONTINUE
C
      IF (ID .NE. 2) THEN
        VECT(NT) = VECT(NT)/(ONE + ALPHA)
      ELSE
      END IF
C
      IF (ID .GT. 1) THEN
        VECT(1)  = VECT( 1)/(ONE + BETA)
      ELSE
      END IF
C
      DO 50 I = 1,NT
        S = S + VECT(I)
   50 CONTINUE
C
      DO 60 I = 1,NT
        VECT(I) = VECT(I)/S
   60 CONTINUE
C
      RETURN
      END
