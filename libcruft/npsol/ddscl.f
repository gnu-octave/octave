*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DDSCL ( N, D, INCD, X, INCX )
      INTEGER            N, INCD, INCX
      DOUBLE PRECISION   D( * ), X( * )
C
C  DDSCL  performs the operation
C
C     x := diag( d )*x
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 22-September-1983.
C     Sven Hammarling, Nag Central Office.
C
      EXTERNAL           DSCAL
      INTEGER            I     , ID    , IX

      IF( N.GE.1 )THEN
         IF( INCD.EQ.0 )THEN

            CALL DSCAL ( N, D( 1 ), X, INCX )

         ELSE IF( ( INCD.EQ.INCX ).AND.( INCD.GT.0 ) )THEN
            DO 10, ID = 1, 1 + ( N - 1 )*INCD, INCD
               X( ID ) = D( ID )*X( ID )
   10       CONTINUE
         ELSE
            IF( INCX.GE.0 )THEN
               IX = 1
            ELSE
               IX = 1 - ( N - 1 )*INCX
            END IF
            IF( INCD.GT.0 )THEN
               DO 20, ID = 1, 1 + ( N - 1 )*INCD, INCD
                  X( IX ) = D( ID )*X( IX )
                  IX      = IX + INCX
   20          CONTINUE
            ELSE
               ID = 1 - ( N - 1 )*INCD
               DO 30, I = 1, N
                  X( IX ) = D( ID )*X( IX )
                  ID      = ID + INCD
                  IX      = IX + INCX
   30          CONTINUE
            END IF
         END IF
      END IF

      RETURN

*     End of DDSCL .

      END
