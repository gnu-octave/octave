*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      DOUBLE PRECISION FUNCTION MCEPS ()

*     MCEPS  returns approximately the relative machine precision via
*     the function name.
*
*     The value returned is given by
*
*        MCEPS  = (1/2)*beta**( 1 - t )   when   rnd = true
*
*        MCEPS  =       beta**( 1 - t )   when   rnd = false,
*
*     where beta is the base of the machine, t is the number of ( beta )
*     digits in the mantissa and rnd is true when rounding occurs and is
*     false when chopping occurs. This is the Wilkinson unit rounding
*     error.
*
*
*  Nag Fortran 77 O( 1 ) basic linear algebra routine (EPSLON).
*
*  -- Written on 26-November-1984.
*     Sven Hammarling, Nag Central Office.

      EXTERNAL           MCENV1
      LOGICAL            FIRST , RND
      INTEGER            BETA  , T
      DOUBLE PRECISION   BASE  , EPS

      SAVE               EPS   , FIRST
      DATA               FIRST / .TRUE. /

      IF( FIRST )THEN
         FIRST = .FALSE.

         CALL MCENV1( BETA, T, RND )

         BASE = BETA
         IF( RND )THEN
            EPS = ( BASE**( 1 - T ) )/2
         ELSE
            EPS =   BASE**( 1 - T )
         END IF
      END IF

      MCEPS  = EPS
      RETURN

*     End of MCEPS  (EPSLON).

      END
