      SUBROUTINE RSOLVE( MODE, NROWR, N, R, Y )
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            MODE, NROWR, N
      DOUBLE PRECISION   R(NROWR,N), Y(N)
C
C  *********************************************************************
C  RSOLVE  SOLVES EQUATIONS INVOLVING AN UPPER-TRIANGULAR MATRIX  R
C  AND A RIGHT-HAND-SIDE VECTOR  Y,  RETURNING THE SOLUTION IN  Y.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF SEPTEMBER 1981.
C  *********************************************************************
C
      INTEGER            J, JJ
      DOUBLE PRECISION   YJ, ZERO
      DOUBLE PRECISION   DOT
      DATA               ZERO /0.0D+0/
C
      IF (MODE .NE. 1) GO TO 400
C
C  MODE = 1  ---  SOLVE  R * Y(NEW) = Y(OLD).
C
      DO 100 JJ = 1, N
         J  = N + 1 - JJ
         YJ = Y(J)/R(J,J)
         Y(J) = YJ
         IF (J .GT. 1  .AND.  YJ .NE. ZERO)
     *   CALL AXPY( J-1, (-YJ), R(1,J), J, 1, Y, J, 1 )
  100 CONTINUE
      RETURN
C
C  MODE = 2  ---  SOLVE  R(TRANSPOSE) * Y(NEW) = Y(OLD).
C
  400 DO 500 J = 1, N
         YJ = Y(J)
         IF (J .GT. 1)
     *   YJ = YJ - DOT( J-1, R(1,J), J, 1, Y, J, 1 )
         Y(J) = YJ/R(J,J)
  500 CONTINUE
      RETURN
C
C  END OF RSOLVE
      END
