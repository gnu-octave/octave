      SUBROUTINE DFOPR
     +  (
     +  ND, N, N0, N1, I, ID, DIF1, DIF2, DIF3, ROOT, VECT
     +  )
      INTEGER           ND, N, N0, N1, I, ID
      DOUBLE PRECISION  DIF1(ND), DIF2(ND), DIF3(ND), ROOT(ND), VECT(ND)
C
C***********************************************************************
C
C     VILLADSEN AND MICHELSEN, PAGES 133-134, 419
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
C       I      : THE INDEX OF THE NODE FOR WHICH THE WEIGHTS ARE TO BE
C                CALCULATED
C
C       ID     : INDICATOR
C
C                  ID = 1  ==>  FIRST DERIVATIVE WEIGHTS ARE COMPUTED
C                  ID = 2  ==>  SECOND DERIVATIVE WEIGHTS ARE COMPUTED
C                  ID = 3  ==>  GAUSSIAN WEIGHTS ARE COMPUTED (IN THIS
C                               CASE, THE VALUE OF I IS IRRELEVANT)
C
C     OUTPUT PARAMETERS:
C
C       DIF1   : ONE DIMENSIONAL VECTOR CONTAINING THE FIRST DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C       DIF2   : ONE DIMENSIONAL VECTOR CONTAINING THE SECOND DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C       DIF3   : ONE DIMENSIONAL VECTOR CONTAINING THE THIRD DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C       VECT   : ONE DIMENSIONAL VECTOR OF COMPUTED WEIGHTS
C
C     COMMON BLOCKS:      NONE
C
C     REQUIRED ROUTINES:  VILERR
C
C***********************************************************************
C
      INTEGER           J,NT,IER
      DOUBLE PRECISION  AX,X,Y
      DOUBLE PRECISION  ZERO,ONE,TWO,THREE
      LOGICAL          LSTOP
C
      PARAMETER ( ZERO = 0.0D+00, ONE    = 1.0D+00,
     +            TWO  = 2.0D+00, THREE  = 3.0D+00 )
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
      IF ((ID .NE. 1) .AND. (ID.NE. 2) .AND. (ID .NE. 3)) THEN
        IER   = 6
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF (ID .NE. 3) THEN
        IF (I .LT. 1) THEN
          IER   = 4
          LSTOP = .TRUE.
          CALL VILERR(IER,LSTOP)
        ELSE
        END IF
C
        IF (I .GT. (N + N0 + N1)) THEN
          IER   = 5
          LSTOP = .TRUE.
          CALL VILERR(IER,LSTOP)
        ELSE
        END IF
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
C -- EVALUATE DISCRETIZATION MATRICES AND GAUSSIAN QUADRATURE
C -- WEIGHTS.  QUADRATURE WEIGHTS ARE NORMALIZED TO SUM TO ONE.
C
      NT = N + N0 + N1
C
      IF (ID .NE. 3) THEN
        DO 20 J = 1,NT
C
          IF (J .EQ. I) THEN
            IF (ID .EQ. 1) THEN
              VECT(I) = DIF2(I)/DIF1(I)/TWO
            ELSE
              VECT(I) = DIF3(I)/DIF1(I)/THREE
            END IF
          ELSE
            Y       = ROOT(I) - ROOT(J)
            VECT(J) = DIF1(I)/DIF1(J)/Y
            IF (ID .EQ. 2) THEN
              VECT(J) = VECT(J)*(DIF2(I)/DIF1(I) - TWO/Y)
            ELSE
            END IF
          END IF
C
   20   CONTINUE
      ELSE
        Y = ZERO
C
        DO 25 J = 1,NT
C
          X  = ROOT(J)
          AX = X*(ONE - X)
C
          IF(N0 .EQ. 0) THEN
            AX = AX/X/X
          ELSE
          END IF
C
          IF(N1 .EQ. 0) THEN
            AX = AX/(ONE - X)/(ONE - X)
          ELSE
          END IF
C
          VECT(J) = AX/DIF1(J)**2
          Y       = Y + VECT(J)
C
   25   CONTINUE
C
        DO 60 J = 1,NT
          VECT(J) = VECT(J)/Y
   60   CONTINUE
C
      END IF
C
      RETURN
      END
