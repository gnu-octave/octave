*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DROT3G( X, Y, CS, SN )

      DOUBLE PRECISION   X, Y, CS, SN

      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

C
C  DROT3G  generates a plane rotation that reduces the vector (X, Y) to
C  the vector (A, 0),  where A is defined as follows...
C
C     If both X and Y are negligibly small, or
C     if Y is negligible relative to Y,
C     then  A = X,  and the identity rotation is returned.
C
C     If X is negligible relative to Y,
C     then  A = Y,  and the swap rotation is returned.
C
C     Otherwise,  A = sign(X) * sqrt( X**2 + Y**2 ).
C
C  In all cases,  X and Y are overwritten by A and 0,  and CS will lie
C  in the closed interval (0, 1).  Also,  the absolute value of CS and
C  SN (if nonzero) will be no less than the machine precision,  EPS.
C
C  DROT3G  guards against overflow and underflow.
C  It is assumed that  FLMIN .lt. EPS**2  (i.e.  RTMIN .lt. EPS).
C
C  Systems Optimization Laboratory, Stanford University.
C  Original version dated January 1982.
C  F77 version dated 28-June-1986.
C  This version of DROT3G dated 28-June-1986.
C
      DOUBLE PRECISION   A, B, EPS, ONE, RTMIN, ZERO
      LOGICAL            FIRST
      INTRINSIC          ABS, MAX, SQRT
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      SAVE               FIRST , EPS   , RTMIN
      DATA               FIRST / .TRUE. /

      IF( FIRST )THEN
         FIRST = .FALSE.
         EPS    = WMACH(3)
         RTMIN  = WMACH(6)
      END IF

      IF (Y .EQ. ZERO) THEN

         CS = ONE
         SN = ZERO

      ELSE IF (X .EQ. ZERO) THEN

         CS = ZERO
         SN = ONE
         X  = Y

      ELSE

         A      = ABS(X)
         B      = ABS(Y)
         IF (MAX(A,B) .LE. RTMIN) THEN
            CS = ONE
            SN = ZERO
         ELSE
            IF (A .GE. B) THEN
               IF (B .LE. EPS*A) THEN
                  CS = ONE
                  SN = ZERO
                  GO TO 900
               ELSE
                  A  = A * SQRT( ONE + (B/A)**2 )
               END IF
            ELSE
               IF (A .LE. EPS*B) THEN
                  CS = ZERO
                  SN = ONE
                  X  = Y
                  GO TO 900
               ELSE
                  A  = B * SQRT( ONE + (A/B)**2 )
               END IF
            END IF
            IF (X .LT. ZERO) A = - A
            CS = X/A
            SN = Y/A
            X  = A
         END IF
      END IF

  900 Y  = ZERO

      RETURN

*     End of  DROT3G

      END
