*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DGRFG ( N, ALPHA, X, INCX, TOL, ZETA )
      INTEGER            N, INCX
      DOUBLE PRECISION   ALPHA, X( * ), TOL, ZETA
C
C  DGRFG  generates details of a generalized Householder reflection such
C  that
C
C     P*( alpha ) = ( beta ),   P'*P = I.
C       (   x   )   (   0  )
C
C  P is given in the form
C
C     P = I - ( zeta )*( zeta  z' ),
C             (   z  )
C
C  where z is an n element vector and zeta is a scalar that satisfies
C
C     1.0 .le. zeta .le. sqrt( 2.0 ).
C
C  zeta is returned in ZETA unless x is such that
C
C     max( abs( x( i ) ) ) .le. max( eps*abs( alpha ), tol )
C
C  where eps is the relative machine precision and tol is the user
C  supplied value TOL, in which case ZETA is returned as 0.0 and P can
C  be taken to be the unit matrix.
C
C  beta is overwritten on alpha and z is overwritten on x.
C  the routine may be called with  n = 0  and advantage is taken of the
C  case where  n = 1.
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 30-August-1984.
C     Sven Hammarling, Nag Central Office.
C     This version dated 28-September-1984.
C
      EXTERNAL           DSSQ  , DSCAL
      INTRINSIC          ABS   , MAX   , SIGN  , SQRT
      LOGICAL            FIRST
      DOUBLE PRECISION   BETA  , EPS   , SCALE , SSQ
      DOUBLE PRECISION   ONE   ,         ZERO
      PARAMETER        ( ONE   = 1.0D+0, ZERO  = 0.0D+0 )

      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      IF( N.LT.1 )THEN
         ZETA = ZERO
      ELSE IF( ( N.EQ.1 ).AND.( X( 1 ).EQ.ZERO ) )THEN
         ZETA = ZERO
      ELSE

         EPS    =  WMACH( 3 )

*        Treat case where P is a 2 by 2 matrix specially.

         IF( N.EQ.1 )THEN

*           Deal with cases where  ALPHA = zero  and
*           abs( X( 1 ) ) .le. max( EPS*abs( ALPHA ), TOL )  first.

            IF( ALPHA.EQ.ZERO )THEN
               ZETA   =  ONE
               ALPHA  =  ABS( X( 1 ) )
               X( 1 ) = -SIGN( ONE, X( 1 ) )
            ELSE IF( ABS( X( 1 ) ).LE.MAX( EPS*ABS( ALPHA ),
     $                                     TOL ) )THEN
               ZETA   =  ZERO
            ELSE
               IF( ABS( ALPHA ).GE.ABS( X( 1 ) ) )THEN
                  BETA = ABS ( ALPHA  )*
     $                   SQRT( ONE + ( X( 1 )/ALPHA )**2 )
               ELSE
                  BETA = ABS ( X( 1 ) )*
     $                   SQRT( ONE + ( ALPHA/X( 1 ) )**2 )
               END IF
               ZETA   =  SQRT( ( ABS( ALPHA ) + BETA )/BETA )
               IF( ALPHA.GE.ZERO )BETA = -BETA
               X( 1 ) = -X( 1 )/( ZETA*BETA )
               ALPHA  =  BETA
            END IF
         ELSE

*           Now P is larger than 2 by 2.

            SSQ   = ONE
            SCALE = ZERO

            CALL DSSQ  ( N, X, INCX, SCALE, SSQ )

*           Treat cases where  SCALE = zero,
*           SCALE .le. max( EPS*abs( ALPHA ), TOL )  and
*           ALPHA = zero  specially.
*           Note that  SCALE = max( abs( X( i ) ) ).

            IF( ( SCALE.EQ.ZERO ).OR.
     $          ( SCALE.LE.MAX( EPS*ABS( ALPHA ), TOL ) ) )THEN
               ZETA  = ZERO
            ELSE IF( ALPHA.EQ.ZERO )THEN
               ZETA  = ONE
               ALPHA = SCALE*SQRT( SSQ )

               CALL DSCAL ( N, -ONE/ALPHA, X, INCX )

            ELSE
               IF( SCALE.LT.ABS( ALPHA ) )THEN
                  BETA = ABS ( ALPHA )*
     $                   SQRT( ONE + SSQ*( SCALE/ALPHA )**2 )
               ELSE
                  BETA = SCALE*
     $                   SQRT( SSQ +     ( ALPHA/SCALE )**2 )
               END IF
               ZETA = SQRT( ( BETA + ABS( ALPHA ) )/BETA )
               IF( ALPHA.GT.ZERO )BETA = -BETA

               CALL DSCAL( N, -ONE/( ZETA*BETA ), X, INCX )

               ALPHA = BETA
            END IF
         END IF
      END IF
      RETURN

*     End of DGRFG .

      END
