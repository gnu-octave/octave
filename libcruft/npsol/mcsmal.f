*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      DOUBLE PRECISION FUNCTION MCSMAL()

*     MCSMAL is intended to return a small positive value such that the
*     reciprocal of MCSMAL does not overflow.
*
*
*  Nag Fortran 77 O( 1 ) basic linear algebra routine (SMALL).
*
*  -- Written on 28-November-1984.
*     Sven Hammarling, Nag Central Office.

      EXTERNAL                  MCENV2
      LOGICAL                   FIRST
      INTEGER                   BETA  , EMIN  , T
      DOUBLE PRECISION          BASE  , EPS   , FLMIN , RMIN

      SAVE                      FIRST , FLMIN
      DATA                      FIRST / .TRUE. /

      IF( FIRST )THEN
         FIRST = .FALSE.
         CALL MCENV2( BETA, T, EPS, EMIN, RMIN )
         BASE  =  BETA
         FLMIN =  RMIN*BASE**4
      END IF

      MCSMAL = FLMIN
      RETURN

*     End of MCSMAL (SMALL).

      END
