*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE MCENV2( BETA, T, EPS, EMIN, RMIN )
      INTEGER            BETA, T, EMIN
      DOUBLE PRECISION   EPS, RMIN

*     MCENV2 returns the machine parameters given by:
*
*        BETA - INTEGER.
*               The base of the machine.
*
*        T    - INTEGER.
*               The number of ( BETA ) digits in the mantissa.
*
*        EPS  - REAL.
*               The smallest positive number such that
*
*                  fl( 1.0 - EPS ) .lt. 1.0,
*
*               where fl denotes the computed value.
*
*        EMIN - INTEGER.
*               The minimum exponent before (gradual) underflow occurs.
*
*        RMIN - REAL.
*               The smallest normalized number for the machine given by
*               BASE**( EMIN - 1 ), where BASE is the floating point
*               value of BETA.
*
*
*     The computation of EPS, EMIN and RMIN is based on a routine,
*     PARANOIA by W. Kahan of the University of California at Berkeley.
*
*
*  Nag Fortran 77 O( 1 ) basic linear algebra routine (ENVIRN).
*
*  -- Written on 6-January-1986.
*     Sven Hammarling, Mick Pont and Janet Welding, Nag Central Office.

      EXTERNAL           MCENV1, MCMIN , MCSTOR
      INTRINSIC          ABS   , MAX
      LOGICAL            FIRST , IWARN , LRND
      INTEGER            GNMIN , GPMIN , I     , LBETA , LEMIN , LEMIN1
      INTEGER            LEMIN2, LT    , NGNMIN, NGPMIN
      DOUBLE PRECISION   A     , B     , C     , HALF  , LEPS  , LRMIN
      DOUBLE PRECISION   ONE   , RBASE , SIXTH , SMALL , MCSTOR, THIRD
      DOUBLE PRECISION   TWO   , XBASE , ZERO

      COMMON    /SOL1CM/ NOUT

      SAVE               FIRST , IWARN , LBETA , LEMIN , LEPS  , LRMIN
      SAVE               LT
      DATA               FIRST / .TRUE. /      , IWARN / .FALSE. /

      IF( FIRST )THEN
         FIRST = .FALSE.
         ZERO  = 0
         ONE   = 1
         TWO   = 2

*        LBETA, LT, LEPS, LEMIN and LRMIN are the local values of BETA,
*        T, EPS, EMIN and RMIN.
*
*        Throughout this routine we use the function MCSTOR to ensure
*        that relevant values are stored and not held in registers, or
*        are not affected by optimizers.
*
*        MCENV1 returns the parameters LBETA and LT. ( LRND is not used
*        here. )

         CALL MCENV1( LBETA, LT, LRND )

*        Start to find EPS.

         B    = LBETA
         A    = B**( -LT )
         LEPS = A

*        Try some tricks to see whether or not this is the correct EPS.

         B     = TWO/3
         HALF  = ONE/2
         SIXTH = MCSTOR( B    , -HALF  )
         THIRD = MCSTOR( SIXTH,  SIXTH )
         B     = MCSTOR( THIRD, -HALF  )
         B     = MCSTOR( B    ,  SIXTH )
         B     = ABS   ( B )
         IF( B.LT.LEPS )
     $      B = LEPS

         LEPS = 1

*+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP
   10    IF   ( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )THEN
            LEPS = B
            C    = MCSTOR( HALF*LEPS, ( TWO**5 )*( LEPS**2 ) )
            C    = MCSTOR( HALF     , -C                     )
            B    = MCSTOR( HALF     ,  C                     )
            C    = MCSTOR( HALF     , -B                     )
            B    = MCSTOR( HALF     ,  C                     )
            GO TO 10
         END IF
*+       END WHILE

         IF( A.LT.LEPS )
     $      LEPS = A

*        Computation of EPS complete. Now find EMIN.
*        First compute the next floating point value below 1.0, a, and
*        keep dividing a by BETA until (gradual) underflow occurs.
*        This is detected when we cannot recover the previous a.

         XBASE = LBETA
         RBASE = 1/XBASE
         SMALL = ONE
         DO 20, I = 1, LT - 1
            SMALL = MCSTOR( SMALL/LBETA, ZERO )
   20    CONTINUE
         A     = MCSTOR( ONE, SMALL )
         CALL MCMIN ( NGPMIN,  ONE, XBASE, RBASE, LBETA )
         CALL MCMIN ( NGNMIN, -ONE, XBASE, RBASE, LBETA )
         CALL MCMIN (  GPMIN,    A, XBASE, RBASE, LBETA )
         CALL MCMIN (  GNMIN,   -A, XBASE, RBASE, LBETA )
         LEMIN = 0
         IF( ( NGPMIN.EQ.NGNMIN ).AND.( GPMIN.EQ.GNMIN ) )THEN
            IF( NGPMIN.EQ.GPMIN )THEN
               LEMIN = NGPMIN
            ELSE IF( NGPMIN.LT.GPMIN )THEN
               IF( ABS( GPMIN - NGPMIN - LT ).LT.3 )THEN
                  LEMIN =  GPMIN
               ELSE
                  LEMIN =  NGPMIN
                  IWARN = .TRUE.
               END IF
            ELSE
               WRITE( NOUT, 9999 )
               CALL XSTOPX (' ')
            END IF
         ELSE
            IF( NGPMIN.EQ.GPMIN )THEN
               LEMIN1 = NGPMIN
            ELSE IF( NGPMIN.LT.GPMIN )THEN
               IF( ABS( GPMIN - NGPMIN - LT ).LT.3 )THEN
                  LEMIN1 =  GPMIN
               ELSE
                  LEMIN1 =  NGPMIN
                  IWARN  = .TRUE.
               END IF
            ELSE
               WRITE( NOUT, 9999 )
               CALL XSTOPX (' ')
            END IF
            IF( NGNMIN.EQ.GNMIN )THEN
               LEMIN2 = NGNMIN
            ELSE IF( NGNMIN.LT.GNMIN )THEN
               IF( ABS( GNMIN - NGNMIN - LT ).LT.3 )THEN
                  LEMIN2 =  GNMIN
               ELSE
                  LEMIN2 =  NGNMIN
                  IWARN  = .TRUE.
               END IF
            ELSE
               WRITE( NOUT, 9999 )
               CALL XSTOPX (' ')
            END IF
            LEMIN = MAX( LEMIN1, LEMIN2 )
         END IF
***
* Comment out this IF block if Emin is ok
         IF( IWARN )THEN
            FIRST = .TRUE.
            WRITE( NOUT, 9998 )LEMIN
         END IF
***

*        Finally compute RMIN by successive division by BETA.
*        We could compute RMIN as base**( EMIN - 1 ), but some machines
*        underflow during this computation.

         LRMIN = 1
         DO 30, I = 1, 1 - LEMIN
            LRMIN = LRMIN/LBETA
   30    CONTINUE
      END IF

      BETA = LBETA
      T    = LT
      EPS  = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      RETURN

 9999 FORMAT( // ' ** ERROR . No reliable value for Emin could be',
     $           ' found.' / ' Please contact Stanford University.'// )
 9998 FORMAT( // ' WARNING. The value Emin may be incorrect:-  Emin = ',
     $           I8 / ' If after inspection the value Emin looks',
     $           ' acceptable please comment out ' / ' the IF block',
     $           ' as marked within the code of routine mcenv2,' /
     $           ' otherwise contact Stanford University. ' / )

*     End of MCENV2 (ENVIRN).

      END
