*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSGSET( PRBTYP, LINOBJ, SINGLR, UNITGZ, UNITQ,
     $                   N, NCLIN, NFREE,
     $                   NROWA, NQ, NROWR, NRANK, NZ, NZ1,
     $                   ISTATE, KX,
     $                   BIGBND, TOLRNK, NUMINF, SUMINF,
     $                   BL, BU, A, RES, FEATOL,
     $                   GQ, CQ, R, X, WTINF, ZY, WRK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*2        PRBTYP
      LOGICAL            LINOBJ, SINGLR, UNITGZ, UNITQ
      INTEGER            ISTATE(*), KX(N)
      DOUBLE PRECISION   BL(*), BU(*), A(NROWA,*),
     $                   RES(*), FEATOL(*)
      DOUBLE PRECISION   GQ(N), CQ(*), R(NROWR,*), X(N), WTINF(*),
     $                   ZY(NQ,*)
      DOUBLE PRECISION   WRK(N)

************************************************************************
*     LSGSET  finds the number and weighted sum of infeasibilities for
*     the bounds and linear constraints.   An appropriate transformed
*     gradient vector is returned in  GQ.
*
*     Positive values of  ISTATE(j)  will not be altered.  These mean
*     the following...
*
*               1             2           3
*           a'x = bl      a'x = bu     bl = bu
*
*     Other values of  ISTATE(j)  will be reset as follows...
*           a'x lt bl     a'x gt bu     a'x free
*              - 2           - 1           0
*
*     If  x  is feasible,  LSGSET computes the vector Q(free)'g(free),
*     where  g  is the gradient of the the sum of squares plus the
*     linear term.  The matrix Q is of the form
*                    ( Q(free)  0       ),
*                    (   0      I(fixed))
*     where  Q(free)  is the orthogonal factor of  A(free)  and  A  is
*     the matrix of constraints in the working set.  The transformed
*     gradients are stored in GQ.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 31-October-1984.
*     Level 2 Blas added 11-June-1986.
*     This version of LSGSET dated 24-June-1986.
************************************************************************
      EXTERNAL           DDOT  , IDRANK
      INTRINSIC          ABS   , MAX   , MIN
      PARAMETER        ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )

      BIGUPP =   BIGBND
      BIGLOW = - BIGBND

      NUMINF =   0
      SUMINF =   ZERO
      CALL DLOAD ( N, ZERO, GQ, 1 )

      DO 200 J = 1, N+NCLIN
         IF (ISTATE(J) .LE. 0) THEN
            FEASJ  = FEATOL(J)
            IF (J .LE. N) THEN
               CTX = X(J)
            ELSE
               K   = J - N
               CTX = DDOT  ( N, A(K,1), NROWA, X, 1 )
            END IF
            ISTATE(J) = 0

*           See if the lower bound is violated.

            IF (BL(J) .GT. BIGLOW) THEN
               S = BL(J) - CTX
               IF (S     .GT. FEASJ ) THEN
                  ISTATE(J) = - 2
                  WEIGHT    = - WTINF(J)
                  GO TO 160
               END IF
            END IF

*           See if the upper bound is violated.

            IF (BU(J) .GE. BIGUPP) GO TO 200
            S = CTX - BU(J)
            IF (S     .LE. FEASJ ) GO TO 200
            ISTATE(J) = - 1
            WEIGHT    =   WTINF(J)

*           Add the infeasibility.

  160       NUMINF = NUMINF + 1
            SUMINF = SUMINF + ABS( WEIGHT ) * S
            IF (J .LE. N) THEN
               GQ(J) = WEIGHT
            ELSE
               CALL DAXPY ( N, WEIGHT, A(K,1), NROWA, GQ, 1 )
            END IF
         END IF
  200 CONTINUE

*     ------------------------------------------------------------------
*     Install  GQ,  the transformed gradient.
*     ------------------------------------------------------------------
      SINGLR = .FALSE.
      UNITGZ = .TRUE.

      IF (NUMINF .GT. 0) THEN
         CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ, KX, GQ, ZY, WRK )
      ELSE IF (NUMINF .EQ. 0  .AND.  PRBTYP .EQ. 'FP') THEN
         CALL DLOAD ( N, ZERO, GQ, 1 )
      ELSE

*        Ready for the Optimality Phase.
*        Set NZ1 so that Rz1 is nonsingular.

         IF (NRANK .EQ. 0) THEN
            IF (LINOBJ) THEN
               CALL DCOPY ( N, CQ, 1, GQ, 1 )
            ELSE
               CALL DLOAD ( N, ZERO, GQ, 1 )
            END IF
            NZ1    = 0
         ELSE

*           Compute  GQ = - R' * (transformed residual)

            CALL DCOPY ( NRANK, RES, 1, GQ, 1 )
            CALL DSCAL ( NRANK, (-ONE), GQ, 1 )
            CALL DTRMV ( 'U', 'T', 'N', NRANK, R, NROWR, GQ, 1 )
            IF (NRANK .LT. N)
     $         CALL DGEMV( 'T', NRANK, N-NRANK, -ONE,R(1,NRANK+1),NROWR,
     $                      RES, 1, ZERO, GQ(NRANK+1), 1 )

            IF (LINOBJ) CALL DAXPY ( N, ONE, CQ, 1, GQ, 1 )
            UNITGZ = .FALSE.
            NZ1    = IDRANK( MIN(NRANK, NZ), R, NROWR+1, TOLRNK )
         END IF
      END IF

      RETURN

*     End of  LSGSET.

      END
