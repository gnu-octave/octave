*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DDDIV ( N, D, INCD, X, INCX )
      INTEGER            N, INCD, INCX
      DOUBLE PRECISION   D( * ), X( * )
C
C     DDDIV  performs the operation
C
C     x := diag( d )(inverse)*x
C
      PARAMETER        ( ONE = 1.0 )
      EXTERNAL           DSCAL
      INTEGER            I     , ID    , IX

      IF( N.GE.1 )THEN
         IF( INCD.EQ.0 )THEN

            CALL DSCAL ( N, (ONE/D( 1 )), X, INCX )

         ELSE IF( ( INCD.EQ.INCX ).AND.( INCD.GT.0 ) )THEN
            DO 10, ID = 1, 1 + ( N - 1 )*INCD, INCD
               X( ID ) = X( ID )/D( ID )
   10       CONTINUE
         ELSE
            IF( INCX.GE.0 )THEN
               IX = 1
            ELSE
               IX = 1 - ( N - 1 )*INCX
            END IF
            IF( INCD.GT.0 )THEN
               DO 20, ID = 1, 1 + ( N - 1 )*INCD, INCD
                  X( IX ) = X( IX )/D( ID )
                  IX      = IX + INCX
   20          CONTINUE
            ELSE
               ID = 1 - ( N - 1 )*INCD
               DO 30, I = 1, N
                  X( IX ) = X( IX )/D( ID )
                  ID      = ID + INCD
                  IX      = IX + INCX
   30          CONTINUE
            END IF
         END IF
      END IF

      RETURN

*     End of DDDIV .

      END
