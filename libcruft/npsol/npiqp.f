*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPIQP ( FEASQP, UNITQ, NQPERR, MINITS,
     $                   N, NCLIN, NCNLN, NROWA, NROWJ, NROWQP, NROWR,
     $                   LINACT, NLNACT, NACTIV, NFREE, NZ, NUMINF,
     $                   ISTATE, KACTIV, KX,
     $                   DXNORM, GDX, QPCURV,
     $                   AQP, ADX, ANORM, AX, BL, BU,
     $                   C, CJAC, CLAMDA, CMUL, CS,
     $                   DLAM, DSLK, DX, QPBL, QPBU, QPTOL,
     $                   R, RHO, SLK, VIOLN, X,
     $                   WTINF, IW, W )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            FEASQP, UNITQ
      INTEGER            ISTATE(*), KACTIV(N), KX(N)
      INTEGER            IW(*)
      DOUBLE PRECISION   AQP(NROWQP,*), ADX(*), ANORM(*), AX(*),
     $                   BL(*), BU(*),
     $                   C(*), CJAC(NROWJ,*), CLAMDA(*), CMUL(*), CS(*)
      DOUBLE PRECISION   DLAM(*), DSLK(*), DX(N)
      DOUBLE PRECISION   QPBL(*), QPBU(*),
     $                   QPTOL(*), R(NROWR,*), RHO(*), SLK(*),
     $                   VIOLN(*), X(N), WTINF(*)
      DOUBLE PRECISION   W(*)

************************************************************************
*     NPIQP   does the following:
*
*     (1)  Generate the upper and lower bounds for the QP  subproblem.
*
*     (2)  Compute the  TQ  factors of the rows of  AQP  specified by
*          the array  ISTATE.  The part of the factorization defined by
*          the first contiguous group of linear constraints does not
*          need to be recomputed.  The remaining rows (which could be
*          comprised of both linear and nonlinear constraints) are
*          included as new rows of the  TQ  factorization stored in
*          T and ZY.  Note that if there are no nonlinear constraints,
*          no factorization is required.
*
*     (3)  Solve the  QP  subproblem.
*                 minimize     1/2 (W p - d)'(Wp - d) + g'p
*
*                 subject to   qpbl .le. (  p ) .le. qpbu,
*                                        ( Ap )
*
*          where  W  is a matrix (not stored) such that  W'W = H  and
*          WQ = R,  d  is the zero vector,  and  g  is the gradient.
*          If the subproblem is infeasible, compute the point which
*          minimizes the sum of infeasibilities.
*
*    (4)   Find the value of each slack variable for which the merit
*          function is minimized.
*
*    (5)   Compute  DSLK,  DLAM  and  DX,  the search directions for
*          the slack variables, the multipliers and the variables.
*
*  Systems Optimization Laboratory, Stanford University.
*  Fortran 66 version written 10-January-1983.
*  This version of NPIQP dated 31-July-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT , NCOLT , NQ
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9
      COMMON    /SOL5CM/ ASIZE , DTMAX , DTMIN
      COMMON    /SOL6CM/ RCNDBD, RFROBN, DRMAX , DRMIN

      INTEGER            LOCLS
      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      LOGICAL            CMDBG, LSDBG, NPDBG
      PARAMETER        ( LDBG = 5 )
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG
      COMMON    /CMDEBG/ ICMDBG(LDBG), CMDBG

*-----------------------------------------------------------------------
      PARAMETER         (MXPARM = 30)
      INTEGER            IPRMLS(MXPARM), IPSVLS
      DOUBLE PRECISION   RPRMLS(MXPARM), RPSVLS

      COMMON    /LSPAR1/ IPSVLS(MXPARM),
     $                   IDBGLS, ITMAX1, ITMAX2, LCRASH, LDBGLS, LPROB ,
     $                   MSGLS , NN    , NNCLIN, NPROB , IPADLS(20)

      COMMON    /LSPAR2/ RPSVLS(MXPARM),
     $                   BIGBND, BIGDX , BNDLOW, BNDUPP, TOLACT, TOLFEA,
     $                   TOLRNK, RPADLS(23)

      EQUIVALENCE       (IPRMLS(1), IDBGLS), (RPRMLS(1), BIGBND)

      SAVE      /LSPAR1/, /LSPAR2/
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
      INTEGER            IPRMNP(MXPARM), IPSVNP
      DOUBLE PRECISION   RPRMNP(MXPARM), RPSVNP

      COMMON    /NPPAR1/ IPSVNP(MXPARM),
     $                   IDBGNP, ITMXNP, JVRFY1, JVRFY2, JVRFY3, JVRFY4,
     $                   LDBGNP, LFORMH, LVLDER, LVERFY, MSGNP , NLNF  ,
     $                   NLNJ  , NLNX  , NNCNLN, IPADNP(15)

      COMMON    /NPPAR2/ RPSVNP(MXPARM),
     $                   CDINT , CTOL  , EPSRF , ETA   , FDINT , FTOL  ,
     $                   RPADNP(24)

      EQUIVALENCE       (IPRMNP(1), IDBGNP), (RPRMNP(1), CDINT)

      SAVE      /NPPAR1/, /NPPAR2/
*-----------------------------------------------------------------------
      EQUIVALENCE  (IDBGNP, IDBG  ), (ITMXNP, NMAJOR), (ITMAX2, NMINOR)
      EQUIVALENCE  (LDBGLS, MNRDBG), (LDBGNP, MJRDBG), (MSGLS , MSGQP )

      CHARACTER*8        NAMES(1)
      LOGICAL            LINOBJ, OVERFL, QPNAMD, VERTEX
      INTRINSIC          ABS   , MIN   , MAX
      EXTERNAL           DDIV  , DDOT  , DNRM2
      PARAMETER        ( QPNAMD =.FALSE.,VERTEX =.FALSE. )
      PARAMETER        ( ZERO   =0.0D+0, ONE    =1.0D+0, TWO   =2.0D+0 )
      PARAMETER        ( HUNDRD =1.0D+2                                )

      IDBGSV = IDBG
      IF (NPDBG) THEN
         IDBG   = 0
      ELSE
         IDBG = NMINOR + 1
      END IF
      LSDBG  = NPDBG
      CMDBG  = NPDBG
      CALL ICOPY ( LDBG, ILSDBG, 1, ICMDBG, 1 )

      LRPQ   = LOCLS( 5)
      LRPQ0  = LOCLS( 6)
      LHPQ   = LOCLS( 8)
      LGQ    = LOCLS( 9)
      LT     = LOCLS(11)
      LZY    = LOCLS(12)
      LWRK1  = LOCLS(14)

      NRPQ   = 0
      NGQ    = 1

      FEASQP =  .TRUE.
      LINOBJ =  .TRUE.

      BIGLOW = - BIGBND
      BIGUPP =   BIGBND
      SSQ1   =   ZERO

      NPLIN  = N     + NCLIN
      NCTOTL = NPLIN + NCNLN
      NCQP   = NCLIN + NCNLN
      NRANK  = N
      NREJTD = 0

*     ==================================================================
*     Generate the upper and lower bounds upon the search direction, the
*     weights on the sum of infeasibilities and the nonlinear constraint
*     violations.
*     ==================================================================
      WSCALE = - ONE
      DO 170 J = 1, NCTOTL

         IF (J .LE. N) THEN
            CON = X(J)
         ELSE IF (J .LE. NPLIN) THEN
            CON = AX(J-N)
         ELSE
            CON = C(J-NPLIN)
         END IF

         BLJ = BL(J)
         BUJ = BU(J)
         IF (BLJ .GT. BIGLOW) BLJ = BLJ - CON
         IF (BUJ .LT. BIGUPP) BUJ = BUJ - CON

         WEIGHT = ONE
         IF (J .LE. NPLIN) THEN
            IF (ABS(BLJ) .LE. QPTOL(J)) BLJ = ZERO
            IF (ABS(BUJ) .LE. QPTOL(J)) BUJ = ZERO
         ELSE
            I    = J - NPLIN
            VIOL = ZERO
            IF (BL(J) .GT. BIGLOW) THEN
               IF (BLJ .GT. ZERO) THEN
                  VIOL   = BLJ
                  WEIGHT = BLJ
                  IF (RHO(I) .GT. ZERO) WEIGHT = VIOL*RHO(I)
                  WSCALE = MAX( WSCALE,   WEIGHT )
                  GO TO 160
               END IF
            END IF

            IF (BU(J) .LT. BIGUPP) THEN
               IF (BUJ .LT. ZERO) THEN
                  VIOL   =   BUJ
                  WEIGHT = - BUJ
                  IF (RHO(I) .GT. ZERO) WEIGHT = - VIOL*RHO(I)
                  WSCALE = MAX( WSCALE, - WEIGHT )
               END IF
            END IF

*           Set the vector of nonlinear constraint violations.

  160       VIOLN(I) = VIOL
         END IF

         WTINF(J) = WEIGHT
         QPBL(J)  = BLJ
         QPBU(J)  = BUJ

  170 CONTINUE

      IF (WSCALE .GT. ZERO) THEN
         WSCALE = ONE/WSCALE
         CALL DSCAL ( NCTOTL, (WSCALE), WTINF, 1 )
      END IF

*     Set the maximum allowable condition estimator of the constraints
*     in the working set.  Note that a relatively well-conditioned
*     working set is used to start the QP iterations.

      CONDMX = MAX( ONE/EPSPT3, HUNDRD )

      IF (NCNLN .GT. 0) THEN
*        ===============================================================
*        Refactorize part of the  QP  constraint matrix.
*        ===============================================================
*        Load the new Jacobian into the  QP  matrix  A.  Compute the
*        2-norms of the rows of the Jacobian.

         DO 180 J = 1, N
            CALL DCOPY ( NCNLN, CJAC(1,J), 1, AQP(NCLIN+1,J), 1 )
  180    CONTINUE

         DO 190 J = NCLIN+1, NCQP
            ANORM(J) = DNRM2 ( N, AQP(J,1), NROWQP )
  190    CONTINUE

*        Count the number of linear constraints in the working set and
*        move them to the front of KACTIV.  Compute the norm of the
*        matrix of constraints in the working set.
*        Let K1  point to the first nonlinear constraint.  Constraints
*        with indices KACTIV(K1),..., KACTIV(NACTIV)  must be
*        refactorized.

         ASIZE  = ZERO
         LINACT = 0
         K1     = NACTIV + 1
         DO 200 K = 1, NACTIV
            I     = KACTIV(K)
            ASIZE = MAX( ASIZE, ANORM(I) )

            IF (I .LE. NCLIN) THEN
               LINACT = LINACT + 1
               IF (LINACT .NE. K) THEN
                  ISWAP  = KACTIV(LINACT)
                  KACTIV(LINACT) = I
                  KACTIV(K)      = ISWAP
               END IF
            ELSE

*              Record the old position of the 1st. nonlinear constraint.

               IF (K1 .GT. NACTIV) K1 = K
            END IF
  200    CONTINUE

         IF (NACTIV .LE. 1 )
     $      CALL DCOND ( NCQP, ANORM, 1, ASIZE, AMIN )

*        Compute the absolute values of the nonlinear constraints in
*        the working set.  Use DX as workspace.

         DO 210 K = LINACT+1, NACTIV
            J = N + KACTIV(K)
            IF (ISTATE(J) .EQ. 1) DX(K) = ABS( QPBL(J) )
            IF (ISTATE(J) .GE. 2) DX(K) = ABS( QPBU(J) )
  210    CONTINUE

*        Sort the elements of KACTIV corresponding to nonlinear
*        constraints in descending order of violation (i.e.,
*        the first element of KACTIV for a nonlinear constraint
*        is associated with the most violated constraint.)
*        In this way, the rows of the Jacobian corresponding
*        to the more violated constraints tend to be included
*        in the  TQ  factorization.

*        The sorting procedure is taken from the simple insertion
*        sort in D. Knuth, ACP Volume 3, Sorting and Searching,
*        Page 81.  It should be replaced by a faster sort if the
*        number of active nonlinear constraints becomes large.

         DO 230 K = LINACT+2, NACTIV
            L     = K
            VIOL  = DX(L)
            KVIOL = KACTIV(L)
*           WHILE (L .GT. LINACT+1  .AND.  DX(L-1) .LT. VIOL) DO
  220       IF    (L .GT. LINACT+1                          ) THEN
               IF (                        DX(L-1) .LT. VIOL) THEN
                  DX(L)     = DX(L-1)
                  KACTIV(L) = KACTIV(L-1)
                  L         = L - 1
                  GO TO 220
               END IF
*           END WHILE
            END IF
            DX(L)     = VIOL
            KACTIV(L) = KVIOL
  230    CONTINUE

         K2     = NACTIV
         NACTIV = K1     - 1
         NZ     = NFREE  - NACTIV

*        Update the factors  R,  T  and  Q  to include constraints
*        K1  through  K2.

         IF (K1 .LE. K2)
     $      CALL LSADDS( UNITQ, VERTEX,
     $                   INFORM, K1, K2, NACTIV, NARTIF, NZ, NFREE,
     $                   NRANK, NREJTD, NRPQ, NGQ,
     $                   N, NQ, NROWQP, NROWR, NROWT,
     $                   ISTATE, KACTIV, KX,
     $                   CONDMX,
     $                   AQP, R, W(LT), W(LRPQ), W(LGQ),
     $                   W(LZY), W(LWRK1), DX )
      END IF

*     ==================================================================
*     Solve for DX, the vector of minimum two-norm that satisfies the
*     constraints in the working set.
*     ==================================================================
      CALL NPSETX( UNITQ,
     $             NCQP, NACTIV, NFREE, NZ,
     $             N, NLNX, NCTOTL, NQ, NROWQP, NROWR, NROWT,
     $             ISTATE, KACTIV, KX,
     $             DXNORM, GDX,
     $             AQP, ADX, QPBL, QPBU, W(LRPQ), W(LRPQ0), DX, W(LGQ),
     $             R, W(LT), W(LZY), W(LWRK1) )

*     ==================================================================
*     Solve a quadratic program for the search direction  DX  and
*     multiplier estimates  CLAMDA.
*     ==================================================================
*     If there is no feasible point for the subproblem,  the sum of
*     infeasibilities is minimized subject to the linear constraints
*     (1  thru  JINF)  being satisfied.

      JINF  = N + NCLIN

      NTRY  = 1
*+    REPEAT
  450    CALL LSCORE( 'QP subproblem', QPNAMD, NAMES, LINOBJ, UNITQ,
     $                NQPERR, MINITS, JINF, NCQP, NCTOTL,
     $                NACTIV, NFREE, NRANK, NZ, NZ1,
     $                N, NROWQP, NROWR,
     $                ISTATE, KACTIV, KX,
     $                GDX, SSQ, SSQ1, SUMINF, NUMINF, DXNORM,
     $                QPBL, QPBU, AQP, CLAMDA, ADX,
     $                QPTOL, R, DX, IW, W )

         IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $      WRITE (NOUT, 8000) NQPERR

         NVIOL = 0
         IF (NUMINF .GT. 0) THEN

*           Count the violated linear constraints.

            DO 460 J = 1, NPLIN
               IF (ISTATE(J) .LT. 0) NVIOL = NVIOL + 1
  460       CONTINUE

            IF (NVIOL .GT. 0) THEN
               NTRY   = NTRY + 1
               UNITQ  = .TRUE.
               NACTIV = 0
               NFREE  = N
               NZ     = N
               CALL ILOAD ( NCTOTL, (0), ISTATE, 1 )

               CALL NPSETX( UNITQ,
     $                      NCQP, NACTIV, NFREE, NZ,
     $                      N, NLNX, NCTOTL, NQ, NROWQP, NROWR, NROWT,
     $                      ISTATE, KACTIV, KX,
     $                      DXNORM, GDX,
     $                      AQP, ADX, QPBL, QPBU, W(LRPQ), W(LRPQ0),
     $                      DX, W(LGQ), R, W(LT), W(LZY), W(LWRK1) )
            END IF
         END IF
      IF (.NOT. (NVIOL .EQ. 0  .OR.  NTRY .GT. 2)) GO TO 450
*+    UNTIL (    NVIOL .EQ. 0  .OR.  NTRY .GT. 2)

*     ==================================================================
*     Count the number of nonlinear constraint gradients in the  QP
*     working set.  Make sure that all small  QP  multipliers associated
*     with nonlinear inequality constraints have the correct sign.
*     ==================================================================
      NLNACT  = 0
      IF (NACTIV .GT. 0  .AND.  NCNLN .GT. 0) THEN
         DO 500 K = 1, NACTIV
            L     = KACTIV(K)
            IF (L .GT. NCLIN) THEN
               NLNACT = NLNACT + 1
               J      = N      + L
               IF (ISTATE(J) .EQ. 1) CLAMDA(J) = MAX( ZERO, CLAMDA(J) )
               IF (ISTATE(J) .EQ. 2) CLAMDA(J) = MIN( ZERO, CLAMDA(J) )
            END IF
  500    CONTINUE
      END IF

      LINACT = NACTIV - NLNACT

*     ------------------------------------------------------------------
*     Extract various useful quantities from the QP solution.
*     ------------------------------------------------------------------
*     Compute  HPQ = R'R(pq)  from the transformed gradient of the QP
*     objective function and  R(pq)  from the transformed residual.

      CALL DSCAL ( N, (-ONE), W(LRPQ), 1 )
      CALL DAXPY ( N, (-ONE), W(LGQ) , 1, W(LHPQ), 1 )
      QPCURV = TWO*SSQ

      IF (NCNLN .GT. 0) THEN
         IF (NUMINF .GT. 0) THEN
            FEASQP = .FALSE.
            CALL DLOAD ( NCTOTL, (ZERO), CLAMDA, 1 )

            IF (NZ .GT. 0) THEN
*              ---------------------------------------------------------
*              Compute a null space component for the search direction
*              as the solution of  Z'HZ(pz) = -Z'g - Z'HY(py).
*              ---------------------------------------------------------
*              Overwrite DX with the transformed search direction
*              Q'(dx).  The first NZ components of DX are zero.

               CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ,
     $                      KX, DX, W(LZY), W(LWRK1) )

*              Overwrite the first NZ components of DX with the solution
*              of  (Rz)u = -(v + w),  where  (Rz)'w = Z'g  and  v  is
*              vector of first NZ components of  R(pq).

               CALL DCOPY ( NZ, W(LGQ), 1, DX, 1 )
               CALL DTRSV ( 'U', 'T', 'N', NZ, R, NROWR, DX, 1 )

               CALL DAXPY ( NZ, (ONE), W(LRPQ), 1, DX, 1 )

               CALL DTRSV ( 'U', 'N', 'N', NZ, R, NROWR, DX, 1 )
               CALL DSCAL ( NZ, (-ONE), DX, 1 )

*              Recompute RPQ, HPQ, GDX and QPCURV.

               CALL DCOPY ( NLNX, DX, 1, W(LRPQ), 1 )
               CALL DTRMV ( 'U', 'N', 'N', NLNX, R, NROWR, W(LRPQ), 1 )
               IF (NLNX .LT. N)
     $            CALL DGEMV( 'N', NLNX, N-NLNX, ONE, R(1,NLNX+1),NROWR,
     $                        DX(NLNX+1), 1, ONE, W(LRPQ), 1 )

               GDX    = DDOT  ( N, W(LGQ) , 1, DX     , 1 )
               QPCURV = DDOT  ( N, W(LRPQ), 1, W(LRPQ), 1 )

               CALL CMQMUL( 3, N, NZ, NFREE, NQ, UNITQ,
     $                      KX, DX, W(LZY), W(LWRK1) )

*              ---------------------------------------------------------
*              Recompute ADX and the 2-norm of DX.
*              ---------------------------------------------------------
               DXNORM  = DNRM2 ( N, DX, 1 )
               IF (NCQP .GT. 0)
     $            CALL DGEMV ( 'N', NCQP, N, ONE, AQP, NROWQP,
     $                         DX, 1, ZERO, ADX, 1 )

               IF (NPDBG  .AND.  INPDBG(2) .GT. 0)
     $            WRITE (NOUT, 8100) (DX(J), J = 1, N)
            END IF

            CALL DCOPY ( NLNX, W(LRPQ), 1, W(LHPQ), 1 )
            CALL DTRMV ( 'U', 'T', 'N', NLNX, R, NROWR, W(LHPQ), 1 )
            IF (NLNX .LT. N)
     $         CALL DGEMV ( 'T', NLNX, N-NLNX, ONE, R(1,NLNX+1), NROWR,
     $                      W(LRPQ), 1, ZERO, W(LHPQ+NLNX), 1 )
         END IF

*        ===============================================================
*        For given values of the objective function and constraints,
*        attempt to minimize the merit function with respect to each
*        slack variable.
*        ===============================================================
         DO 600 I = 1, NCNLN
            J      = NPLIN + I
            CON    = C(I)

            IF (      .NOT. FEASQP  .AND.
     $          VIOLN(I) .NE. ZERO  .AND.  RHO(I) .LE. ZERO )
     $         RHO(I) = ONE

            QUOTNT = DDIV  ( CMUL(I), SCALE*RHO(I), OVERFL )

*           Define the slack variable to be  CON - MULT / RHO.
*           Force each slack to lie within its upper and lower bounds.

            IF (BL(J) .GT. BIGLOW) THEN
               IF (QPBL(J) .GE. - QUOTNT) THEN
                  SLK(I) = BL(J)
                  GO TO 550
               END IF
            END IF

            IF (BU(J) .LT. BIGUPP) THEN
               IF (QPBU(J) .LE. - QUOTNT) THEN
                  SLK(I) = BU(J)
                  GO TO 550
               END IF
            END IF

            SLK(I) = CON - QUOTNT

*           The slack has been set within its bounds.

  550       CS(I)  = CON - SLK(I)

*           ------------------------------------------------------------
*           Compute the search direction for the slacks and multipliers.
*           ------------------------------------------------------------
            DSLK(I) = ADX(NCLIN+I) + CS(I)

            IF (FEASQP) THEN
*
*              If any constraint is such that  (DLAM)*(C - S)  is
*              positive,  the merit function may be reduced immediately
*              by substituting the QP multiplier.
*
               DLAM(I)  = CLAMDA(J) - CMUL(I)
               IF (DLAM(I) * CS(I) .GE. ZERO) THEN
                  CMUL(I) = CLAMDA(J)
                  DLAM(I) = ZERO
               END IF
            ELSE

*              The  QP  subproblem was infeasible.

               DLAM(I) = ZERO

               IF (ISTATE(J) .LT. 0  .OR.  VIOLN(I) .NE. ZERO)
     $            DSLK(I)  = ZERO

            END IF
  600    CONTINUE

         IF (.NOT. FEASQP)
     $      RHONRM = DNRM2 ( NCNLN, RHO, 1 )

         IF (NPDBG  .AND.  INPDBG(2) .GT. 0) THEN
            WRITE (NOUT, 8200) (VIOLN(I), I=1,NCNLN)
            WRITE (NOUT, 8300) (SLK(I)  , I=1,NCNLN)
         END IF
      END IF

      CALL ICOPY ( LDBG, INPDBG, 1, ICMDBG, 1 )
      IDBG   = IDBGSV

      RETURN

 8000 FORMAT(/ ' //NPIQP // NQPERR'
     $       / ' //NPIQP // ',  I6 )
 8100 FORMAT(/ ' //NPIQP // DX recomputed with null space portion...'
     $       / (5G12.3))
 8200 FORMAT(/ ' //NPIQP // Violations = '/ (1P5E15.6))
 8300 FORMAT(/ ' //NPIQP // Slacks     = '/ (1P5E15.6))

*     End of  NPIQP .

      END
