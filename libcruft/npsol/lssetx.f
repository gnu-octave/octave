*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSSETX( LINOBJ, ROWERR, UNITQ,
     $                   NCLIN, NACTIV, NFREE, NRANK, NZ,
     $                   N, NCTOTL, NQ, NROWA, NROWR, NROWT,
     $                   ISTATE, KACTIV, KX,
     $                   JMAX, ERRMAX, CTX, XNORM,
     $                   A, AX, BL, BU, CQ, RES, RES0, FEATOL,
     $                   R, T, X, ZY, P, WORK )

      IMPLICIT           DOUBLE PRECISION (A-H,O-Z)
      LOGICAL            LINOBJ, ROWERR, UNITQ
      INTEGER            ISTATE(NCTOTL), KACTIV(N), KX(N)
      DOUBLE PRECISION   A(NROWA,*), AX(*), BL(NCTOTL), BU(NCTOTL),
     $                   CQ(*), RES(*), RES0(*), FEATOL(NCTOTL), P(N),
     $                   R(NROWR,*), T(NROWT,*), ZY(NQ,*), X(N)
      DOUBLE PRECISION   WORK(NCTOTL)

************************************************************************
*  LSSETX  computes the point on a working set that is closest to the
*  input vector  x  (in the least-squares sense).  The norm of  x, the
*  transformed residual vector  Pr - RQ'x,  and the constraint values
*  Ax  are also initialized.
*
*  If the computed point gives a row error of more than the feasibility
*  tolerance, an extra step of iterative refinement is used.  If  x  is
*  still infeasible,  the logical variable  ROWERR  is set.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version written 31-October-1984.
*  This version dated 29-December-1985.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      EXTERNAL           IDAMAX, DDOT
      INTRINSIC          ABS, MIN
      PARAMETER        ( NTRY  = 2 )
      PARAMETER        ( ZERO  = 0.0D+0, ONE = 1.0D+0 )

*     ------------------------------------------------------------------
*     Move  x  onto the simple bounds in the working set.
*     ------------------------------------------------------------------
      DO 100 K = NFREE+1, N
          J   = KX(K)
          IS  = ISTATE(J)
          BND = BL(J)
          IF (IS .GE. 2) BND  = BU(J)
          IF (IS .NE. 4) X(J) = BND
  100 CONTINUE

*     ------------------------------------------------------------------
*     Move  x  onto the general constraints in the working set.
*     We shall make  ntry  tries at getting acceptable row errors.
*     ------------------------------------------------------------------
      KTRY   = 1
      JMAX   = 1
      ERRMAX = ZERO

*     REPEAT
  200    IF (NACTIV .GT. 0) THEN

*           Set  work = residuals for constraints in the working set.
*           Solve for p, the smallest correction to x that gives a point
*           on the constraints in the working set.  Define  p = Y*(py),
*           where  py  solves the triangular system  T*(py) = residuals.

            DO 220 I = 1, NACTIV
               K   = KACTIV(I)
               J   = N + K
               BND = BL(J)
               IF (ISTATE(J) .EQ. 2) BND = BU(J)
               WORK(I) = BND - DDOT  ( N, A(K,1), NROWA, X, 1 )
  220       CONTINUE

            CALL CMTSOL( 1, NROWT, NACTIV, T(1,NZ+1), WORK )
            CALL DLOAD ( N, ZERO, P, 1 )
            CALL DCOPY ( NACTIV, WORK, 1, P(NZ+1), 1 )

            CALL CMQMUL( 2, N, NZ, NFREE, NQ, UNITQ, KX, P, ZY, WORK )
            CALL DAXPY ( N, ONE, P, 1, X, 1 )
         END IF

*        ---------------------------------------------------------------
*        Compute the 2-norm of  x.
*        Initialize  Ax  for all the general constraints.
*        ---------------------------------------------------------------
         XNORM  = DNRM2 ( N, X, 1 )
         IF (NCLIN .GT. 0)
     $      CALL DGEMV ( 'N', NCLIN, N, ONE, A, NROWA,
     $                   X, 1, ZERO, AX, 1 )

*        ---------------------------------------------------------------
*        Check the row residuals.
*        ---------------------------------------------------------------
         IF (NACTIV .GT. 0) THEN
            DO 300 K = 1, NACTIV
               I   = KACTIV(K)
               J   = N + I
               IS  = ISTATE(J)
               IF (IS .EQ. 1) WORK(K) = BL(J) - AX(I)
               IF (IS .GE. 2) WORK(K) = BU(J) - AX(I)
  300       CONTINUE

            JMAX   = IDAMAX( NACTIV, WORK, 1 )
            ERRMAX = ABS( WORK(JMAX) )
         END IF

         KTRY = KTRY + 1
*     UNTIL    (ERRMAX .LE. FEATOL(JMAX) .OR. KTRY .GT. NTRY
      IF (.NOT.(ERRMAX .LE. FEATOL(JMAX) .OR. KTRY .GT. NTRY)) GO TO 200

      ROWERR = ERRMAX .GT. FEATOL(JMAX)

*     ==================================================================
*     Compute the linear objective value  c'x  and the transformed
*     residual  Pr  -  RQ'x = RES0  -  RQ'x.
*     ==================================================================
      IF (NRANK .GT. 0  .OR.  LINOBJ) THEN
         CALL DCOPY ( N, X, 1, P, 1 )
         CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ, KX, P, ZY, WORK )
      END IF

      CTX = ZERO
      IF (LINOBJ)
     $   CTX = DDOT  ( N, CQ, 1, P, 1 )

      IF (NRANK .GT. 0) THEN

         CALL DTRMV ( 'U', 'N', 'N', NRANK, R, NROWR, P, 1 )
         IF (NRANK .LT. N)
     $      CALL DGEMV ( 'N', NRANK, N-NRANK, ONE, R(1,NRANK+1), NROWR,
     $                   P(NRANK+1), 1, ONE, P, 1 )

         CALL DCOPY ( NRANK,       RES0, 1, RES, 1 )
         CALL DAXPY ( NRANK, -ONE, P   , 1, RES, 1 )

      END IF

      IF (LSDBG  .AND.  ILSDBG(2) .GT. 0)
     $   WRITE (NOUT, 2200) (X(J), J = 1, N)

      RETURN

 2200 FORMAT(/ ' //LSSETX// Variables after refinement ... '/ (5G12.3))

*     End of  LSSETX.

      END
