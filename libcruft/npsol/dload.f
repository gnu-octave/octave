*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DLOAD ( N, CONST, X, INCX )
      INTEGER            N, INCX
      DOUBLE PRECISION   CONST
      DOUBLE PRECISION   X( * )
C
C  DLOAD  performs the operation
C
C     x = const*e,   e' = ( 1  1 ... 1 ).
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 22-September-1983.
C     Sven Hammarling, Nag Central Office.
C
      INTEGER            IX
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )

      IF( N.LT.1 )RETURN

      IF( CONST.NE.ZERO )THEN
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            X( IX ) = CONST
   10    CONTINUE
      ELSE
         DO 20, IX = 1, 1 + ( N - 1 )*INCX, INCX
            X( IX ) = ZERO
   20    CONTINUE
      END IF

      RETURN

*     End of DLOAD .

      END
