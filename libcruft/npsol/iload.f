*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE ILOAD ( N, ICONST, IX, INCIX )
      INTEGER            N, INCIX
      INTEGER            ICONST
      INTEGER            IX( * )
C
C  ILOAD   performs the operation
C
C     ix = iconst*e,   e' = ( 1  1 ... 1 ).
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 22-September-1983.
C     Sven Hammarling, Nag Central Office.
C
      INTEGER            JX

      IF( N.LT.1 )RETURN

      IF( ICONST.NE.0 )THEN
         DO 10, JX = 1, 1 + ( N - 1 )*INCIX, INCIX
            IX( JX ) = ICONST
   10    CONTINUE
      ELSE
         DO 20, JX = 1, 1 + ( N - 1 )*INCIX, INCIX
            IX( JX ) = 0
   20    CONTINUE
      END IF

      RETURN

*     End of ILOAD .

      END
