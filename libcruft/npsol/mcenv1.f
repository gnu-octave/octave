*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE MCENV1( BETA, T, RND )
      LOGICAL            RND
      INTEGER            BETA, T

*     MCENV1 returns the machine parameters given by:
*
*        BETA - INTEGER.
*               The base of the machine.
*
*        T    - INTEGER.
*               The number of ( BETA ) digits in the mantissa.
*
*        RND  - LOGICAL.
*               Whether proper rounding ( RND = .TRUE. ) or chopping
*               ( RND = .FALSE. ) occurs in addition. This may not be a
*               reliable guide to the way in which the machine perfoms
*               its arithmetic.
*
*     The routine is based on the routine ENVRON by Malcolm
*     and incorporates suggestions by Gentleman and Marovich. See
*
*        Malcolm M. A. (1972) Algorithms to reveal properties of
*           floating-point arithmetic. Comms. of the ACM, 15, 949-951.
*
*        Gentleman W. M. and Marovich S. B. (1974) More on algorithms
*           that reveal properties of floating point arithmetic units.
*           Comms. of the ACM, 17, 276-277.
*
*
*  Nag Fortran 77 O( 1 ) basic linear algebra routine (ENVRON).
*
*  -- Written on 26-November-1984.
*     Sven Hammarling and Mick Pont, Nag Central Office.

      EXTERNAL           MCSTOR
      LOGICAL            FIRST , LRND
      INTEGER            LBETA , LT
      DOUBLE PRECISION   A     , B     , C     , F     , ONE   , QTR
      DOUBLE PRECISION   MCSTOR

      SAVE               FIRST , LBETA , LRND  , LT
      DATA               FIRST / .TRUE. /

      IF( FIRST )THEN
         FIRST = .FALSE.
         ONE   = 1

*        LBETA, LT and LRND are the local values of BETA, T and RND.
*
*        Throughout this routine we use the function MCSTOR to ensure
*        that relevant values are stored and not held in registers, or
*        are not affected by optimizers.
*
*        Compute  a = 2.0**m  with the smallest positive integer m such
*        that
*
*           fl( a + 1.0 ) = a.

         A = 1
         C = 1

*+       WHILE( C.EQ.ONE )LOOP
   10    IF   ( C.EQ.ONE )THEN
            A = 2*A
            C = MCSTOR( A,  ONE )
            C = MCSTOR( C, -A   )
            GO TO 10
         END IF
*+       END WHILE

*        Now compute  b = 2.0**m  with the smallest positive integer m
*        such that
*
*           fl( a + b ) .gt. a.

         B = 1
         C = MCSTOR( A, B )

*+       WHILE( C.EQ.A )LOOP
   20    IF   ( C.EQ.A )THEN
            B = 2*B
            C = MCSTOR( A, B )
            GO TO 20
         END IF
*+       END WHILE

*        Now compute the base. a and b are neighbouring floating point
*        numbers in the interval ( beta**t, beta**( t + 1 ) ) and so
*        their difference is beta. Adding 0.25 to c is to ensure that it
*        is truncated to beta and not ( beta - 1 ).


         QTR   = ONE/4
         C     = MCSTOR( C, -A )
         LBETA = C + QTR

*        Now determine whether rounding or chopping occurs, by adding
*        a bit less than beta/2 and a bit more than beta/2 to a.

         B = LBETA
         F = MCSTOR( B/2, -B/100 )
         C = MCSTOR( F, A )
         IF( C.EQ.A) THEN
            LRND = .TRUE.
         ELSE
            LRND = .FALSE.
         END IF
         F = MCSTOR( B/2,  B/100 )
         C = MCSTOR( F, A )
         IF( ( LRND ).AND.( C.EQ.A ) )
     $      LRND = .FALSE.

*        Now find the mantissa, t. It should be the integer part of
*        log to the base beta of a, however it is safer to determine t
*        by powering. So we find t as the smallest positive integer
*        for which
*
*           fl( beta**t + 1.0 ) = 1.0.

         LT = 0
         A  = 1
         C  = 1

*+       WHILE( C.EQ.ONE )LOOP
   30    IF   ( C.EQ.ONE )THEN
            LT = LT + 1
            A  = A*LBETA
            C  = MCSTOR( A,  ONE )
            C  = MCSTOR( C, -A   )
            GO TO 30
         END IF
*+       END WHILE

      END IF

      BETA = LBETA
      T    = LT
      RND  = LRND
      RETURN

*     End of MCENV1 (ENVRON).

      END
