*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      DOUBLE PRECISION  FUNCTION MCSTOR( A, B )
      DOUBLE PRECISION                   A, B

*     MCSTOR is intended to force A and B to be stored prior to doing
*     the addition of A and B. For use in situations where optimizers
*     might hold one of these in a register.
*
*
*  Nag Fortran 77 O( 1 ) basic linear algebra routine (STORE).
*
*  -- Written on 28-November-1984.
*     Sven Hammarling, Nag Central Office.

      MCSTOR  = A + B

      RETURN

*     End of MCSTOR (STORE).

      END
