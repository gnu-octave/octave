      SUBROUTINE INTRP ( ND, NT, X, ROOT, DIF1, XINTP )
C
      INTEGER           ND, NT
      DOUBLE PRECISION  ROOT(ND), DIF1(ND), XINTP(ND)
C
C***********************************************************************
C
C     LAGRANGE INTERPOLATION
C
C     VILLADSEN AND MICHELSEN, PAGES 132-133, 420
C
C     INPUT PARAMETERS:
C
C       NT     : THE TOTAL NUMBER OF INTERPOLATION POINTS FOR WHICH THE
C                VALUE OF THE DEPENDENT VARIABLE Y IS KNOWN.  NOTE:
C
C                  NT = N + N0 + N1
C
C       X      : THE ABCISSA X WHERE Y(X) IS DESIRED
C
C       ROOT   : ONE DIMENSIONAL VECTOR CONTAINING ON EXIT THE
C                N + N0 + N1 ZEROS OF THE NODE POLYNOMIAL USED IN THE
C                INTERPOLATION ROUTINE
C
C       DIF1   : ONE DIMENSIONAL VECTOR CONTAINING THE FIRST DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C     OUTPUT PARAMETERS:
C
C       XINTP  : THE VECTOR OF INTERPOLATION WEIGHTS
C
C                Y(X) IS GIVEN BY:
C
C                            NT
C                  Y(X)  =  SUM  XINTRP(I) * Y(I)
C                           I=1
C
C     COMMON BLOCKS:      NONE
C
C     REQUIRED ROUTINES:  VILERR
C
C***********************************************************************
C
      INTEGER           I,IER
      DOUBLE PRECISION  POL,Y,X
      DOUBLE PRECISION  ZERO,ONE
      LOGICAL           LSTOP
C
      PARAMETER ( ZERO = 0.0D+00, ONE = 1.0D+00 )
C
C -- ERROR CHECKING
C
      IF (ND .LT. NT) THEN
        IER   = 3
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF (NT .LT. 1) THEN
        IER   = 7
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
C -- EVALUATE LAGRANGIAN INTERPOLATION COEFFICIENTS
C
      POL = ONE
C
      DO 5 I = 1,NT
C
        Y        = X - ROOT(I)
        XINTP(I) = ZERO
C
        IF (Y .EQ. ZERO) THEN
          XINTP(I) = ONE
        ELSE
        END IF
C
        POL = POL*Y
C
    5 CONTINUE
C
      IF (POL .NE. ZERO) THEN
        DO 6 I = 1,NT
          XINTP(I) = POL/DIF1(I)/(X - ROOT(I))
    6   CONTINUE
      ELSE
      END IF
C
      RETURN
      END
