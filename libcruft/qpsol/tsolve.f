      SUBROUTINE TSOLVE( MODE, NROWT, N, T, Y )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            MODE, NROWT, N
      DOUBLE PRECISION   T(NROWT,N), Y(N)
C
C  *********************************************************************
C  TSOLVE  SOLVES EQUATIONS INVOLVING A REVERSE-TRIANGULAR MATRIX  T
C  AND A RIGHT-HAND-SIDE VECTOR  Y,  RETURNING THE SOLUTION IN  Y.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF SEPTEMBER 1981.
C  *********************************************************************
C
      INTEGER            J, JJ, L, LROW, N1
      DOUBLE PRECISION   YJ, ZERO
      DATA               ZERO /0.0D+0/
C
      N1 = N + 1
      IF (MODE .NE. 1) GO TO 400
C
C  MODE = 1  ---  SOLVE  T * Y(NEW) = Y(OLD).
C
      DO 100 J = 1, N
         JJ = N1 - J
         YJ = Y(J)/T(J,JJ)
         Y(J) = YJ
         L  = JJ - 1
         IF (L .GT. 0  .AND.  YJ .NE. ZERO)
     *   CALL AXPY( L, (-YJ), T(J+1,JJ), L, 1, Y(J+1), L, 1 )
  100 CONTINUE
      GO TO 700
C
C  MODE = 2  ---  SOLVE  T(TRANSPOSE) * Y(NEW) = Y(OLD).
C
  400 DO 500 J = 1, N
         JJ = N1 - J
         YJ = Y(J)/T(JJ,J)
         Y(J) = YJ
         L  = JJ - 1
         LROW = NROWT*(L - 1) + 1
         IF (L .GT. 0  .AND.  YJ .NE. ZERO)
     *   CALL AXPY( L, (-YJ), T(JJ,J+1), LROW, NROWT, Y(J+1), L, 1 )
  500 CONTINUE
C
C  REVERSE THE SOLUTION VECTOR.
C
  700 IF (N .LE. 1) RETURN
      L = N/2
      DO 800 J = 1, L
         JJ    = N1 - J
         YJ    = Y(J)
         Y(J)  = Y(JJ)
         Y(JJ) = YJ
  800 CONTINUE
      RETURN
C
C  END OF TSOLVE
      END
