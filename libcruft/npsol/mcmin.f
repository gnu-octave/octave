*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE MCMIN ( EMIN, START, XBASE, RBASE, BASE )
      INTEGER            EMIN, BASE
      DOUBLE PRECISION   START, XBASE, RBASE

*     Service routine for MCENV2.
*
*
*  Nag Fortran 77 O( 1 ) basic linear algebra routine (GETMIN).
*
*  -- Written on 6-January-1986.
*     Sven Hammarling and Mick Pont, Nag Central Office.

      EXTERNAL           MCSTOR
      INTEGER            I
      DOUBLE PRECISION   A     , B1    , B2    , C1    , C2    , D1
      DOUBLE PRECISION   D2    , MCSTOR, ZERO

      A    = START
      ZERO = 0
      EMIN = 1
      B1   = MCSTOR( A/XBASE, ZERO )
      C1   = A
      C2   = A
      D1   = A
      D2   = A
*+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
*+   $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP
   10 IF   ( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
     $       ( D1.EQ.A ).AND.( D2.EQ.A )      )THEN
         EMIN = EMIN - 1
         A    = B1
         B1   = MCSTOR( A /XBASE, ZERO )
         C1   = MCSTOR( B1*XBASE, ZERO )
         D1   = ZERO
         DO 20, I = 1, BASE
            D1 = D1 + B1
   20    CONTINUE
         B2   = MCSTOR( A *RBASE, ZERO )
         C2   = MCSTOR( B2/RBASE, ZERO )
         D2   = ZERO
         DO 30, I = 1, BASE
            D2 = D2 + B2
   30    CONTINUE
         GO TO 10
      END IF
*+    END WHILE
      RETURN

*     End of MCMIN (GETMIN).

      END
