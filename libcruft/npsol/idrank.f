*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      INTEGER           FUNCTION IDRANK( N, X, INCX, TOL )
      INTEGER                            N, INCX
      DOUBLE PRECISION                   X( * ), TOL

C  IDRANK finds the first element of the n element vector x for which
C
C     abs( x( k ) ).le.( tol*max ( abs(x(1)), ..., abs(x(k-1)) )
C
C  and returns the value ( k - 1 ) in the function name IDRANK. If no
C  such k exists then IDRANK is returned as n.
C
C  If TOL is supplied as less than zero then the value EPSMCH, where
C  EPSMCH is the relative machine precision, is used in place of TOL.
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 21-January-1985.
C     Sven Hammarling, Nag Central Office.
C     Modified by PEG, 19-December-1985.

      INTRINSIC                          ABS   , MAX
      INTEGER                            IX    , K
      DOUBLE PRECISION                   TOLRNK, XMAX  , ZERO
      PARAMETER                        ( ZERO  = 0.0D+0 )

      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      K = 0
      IF (N .GE. 1) THEN
         TOLRNK = TOL
         IF (TOL .LT. ZERO) TOLRNK = WMACH(3)

         IF( INCX .GT. 0 )THEN
            IX = 1
         ELSE
            IX = 1 - ( N - 1 )*INCX
         END IF

         XMAX = ABS( X(IX) )

*+       WHILE (K .LT. N) LOOP
   10    IF    (K .LT. N) THEN
            IF (ABS( X(IX) ) .LE. XMAX*TOLRNK) GO TO 20
            XMAX = MAX( XMAX, ABS( X(IX) ) )
            K    = K  + 1
            IX   = IX + INCX
            GO TO 10
         END IF
*+       END WHILE

      END IF
   20 IDRANK = K
      RETURN

*     End of IDRANK.

      END
