*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMTSOL( MODE, NROWT, N, T, Y )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            MODE, NROWT, N
      DOUBLE PRECISION   T(NROWT,*), Y(N)

************************************************************************
*     CMTSOL  solves equations involving a reverse-triangular matrix  T
*     and a right-hand-side vector  y,  returning the solution in  y.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original Fortran 77 version written February-1985.
************************************************************************
      PARAMETER        ( ZERO = 0.0D+0 )

      N1 = N + 1
      IF (MODE .EQ. 1) THEN

*        Mode = 1  ---  Solve  T * y(new) = y(old).

         DO 100 J = 1, N
            JJ = N1 - J
            YJ = Y(J)/T(J,JJ)
            Y(J) = YJ
            L  = JJ - 1
            IF (L .GT. 0  .AND.  YJ .NE. ZERO)
     $      CALL DAXPY( L, (-YJ), T(J+1,JJ), 1, Y(J+1), 1 )
  100    CONTINUE
      ELSE

*        Mode = 2  ---  Solve  T' y(new) = y(old).

         DO 500 J = 1, N
            JJ = N1 - J
            YJ = Y(J)/T(JJ,J)
            Y(J) = YJ
            L  = JJ - 1
            IF (L .GT. 0  .AND.  YJ .NE. ZERO)
     $      CALL DAXPY( L, (-YJ), T(JJ,J+1), NROWT, Y(J+1), 1 )
  500    CONTINUE
      END IF

*     Reverse the solution vector.

      IF (N .GT. 1) THEN
         L = N/2
         DO 800 J = 1, L
            JJ    = N1 - J
            YJ    = Y(J)
            Y(J)  = Y(JJ)
            Y(JJ) = YJ
  800    CONTINUE
      END IF

      RETURN

*     End of  CMTSOL.

      END
