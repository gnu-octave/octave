*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      DOUBLE PRECISION FUNCTION DNORM ( SCALE, SSQ )
      DOUBLE PRECISION                  SCALE, SSQ
C
C  DNORM  returns the value norm given by
C
C     norm = ( scale*sqrt( ssq ), scale*sqrt( ssq ) .lt. flmax
C            (
C            ( flmax,             scale*sqrt( ssq ) .ge. flmax
C
C  via the function name.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 22-October-1982.
C     Sven Hammarling, Nag Central Office.
C
      INTRINSIC           SQRT
      LOGICAL             FIRST
      DOUBLE PRECISION    FLMAX , SQT
      DOUBLE PRECISION    ONE
      PARAMETER         ( ONE   = 1.0D+0 )

      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      SAVE                FIRST , FLMAX
      DATA                FIRST / .TRUE. /

      IF( FIRST )THEN
         FIRST = .FALSE.
         FLMAX = WMACH( 7 )
      END IF

      SQT = SQRT( SSQ )
      IF( SCALE.LT.FLMAX/SQT )THEN
         DNORM  = SCALE*SQT
      ELSE
         DNORM  = FLMAX
      END IF

      RETURN

*     End of DNORM .

      END
