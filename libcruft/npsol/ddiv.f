*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      DOUBLE PRECISION FUNCTION DDIV  ( A, B, FAIL )
      DOUBLE PRECISION                  A, B
      LOGICAL                           FAIL
C
C  DDIV   returns the value div given by
C
C     div = ( a/b                 if a/b does not overflow,
C           (
C           ( 0.0                 if a .eq. 0.0,
C           (
C           ( sign( a/b )*flmax   if a .ne. 0.0 and a/b would overflow,
C
C  where flmax is a large value, via the function name. In addition if
C  a/b would overflow then fail is returned as true, otherwise fail is
C  returned as false.
C
C  Note that when a and b are both zero, fail is returned as true,
C  but div is returned as 0.0. in all other cases of overflow div is
C  such that abs( div ) = flmax.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 26-October-1982.
C     Sven Hammarling, Nag Central Office.
C
      INTRINSIC           ABS   , SIGN
      LOGICAL             FIRST
      DOUBLE PRECISION    ABSB  , FLMAX , FLMIN
      DOUBLE PRECISION    ONE   ,         ZERO
      PARAMETER         ( ONE   = 1.0D+0, ZERO  = 0.0D+0 )

      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      SAVE                FIRST , FLMIN , FLMAX
      DATA                FIRST / .TRUE. /

      IF( A.EQ.ZERO )THEN
         DDIV   = ZERO
         IF( B.EQ.ZERO )THEN
            FAIL = .TRUE.
         ELSE
            FAIL = .FALSE.
         END IF
         RETURN
      END IF

      IF( FIRST )THEN
         FIRST  = .FALSE.
         FLMIN  = WMACH( 5 )
         FLMAX  = WMACH( 7 )
      END IF

      IF( B.EQ.ZERO )THEN
         DDIV   = SIGN( FLMAX, A )
         FAIL   = .TRUE.
      ELSE
         ABSB   = ABS( B )
         IF( ABSB.GE.ONE )THEN
            FAIL = .FALSE.
            IF( ABS( A ).GE.ABSB*FLMIN )THEN
               DDIV   = A/B
            ELSE
               DDIV   = ZERO
            END IF
         ELSE
            IF( ABS( A ).LE.ABSB*FLMAX )THEN
               FAIL   = .FALSE.
               DDIV   = A/B
            ELSE
               FAIL   = .TRUE.
               DDIV   = FLMAX
               IF( ( ( A.LT.ZERO ).AND.( B.GT.ZERO ) ).OR.
     $             ( ( A.GT.ZERO ).AND.( B.LT.ZERO ) )     )
     $            DDIV   = -DDIV
            END IF
         END IF
      END IF

      RETURN

*     End of DDIV  .

      END
