*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPCORE( NAMED, NAMES, UNITQ, INFORM, MAJITS,
     $                   N, NCLIN, NCNLN, NCTOTL, NACTIV, NFREE, NZ,
     $                   NROWA, NROWJ, NROWUJ, NROWQP, NROWR,
     $                   NFUN, NGRAD, ISTATE, KACTIV, KX,
     $                   OBJF, FDNORM, XNORM, OBJFUN, CONFUN,
     $                   AQP, AX, BL, BU, C, CJAC, UJAC, CLAMDA,
     $                   FEATOL, GRAD, UGRAD, R, X, IW, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            NAMED
      INTEGER            ISTATE(*), KACTIV(N), KX(N)
      INTEGER            IW(*)
      DOUBLE PRECISION   AQP(NROWQP,*), AX(*), BL(NCTOTL), BU(NCTOTL),
     $                   C(*), CJAC(NROWJ,*), UJAC(NROWUJ,*)
      DOUBLE PRECISION   CLAMDA(NCTOTL), FEATOL(NCTOTL), GRAD(N),
     $                   UGRAD(N), R(NROWR,*), X(N)
      DOUBLE PRECISION   W(LENW)
      EXTERNAL           OBJFUN, CONFUN

      DOUBLE PRECISION   ASIZE, DTMAX, DTMIN
      CHARACTER*8        NAMES(*)

************************************************************************
*  NPCORE  is the core routine for  NPSOL,  a sequential quadratic
*  programming (SQP) method for nonlinearly constrained optimization.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version      February-1982.
*  This version of NPCORE dated  4-August-1986.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT , NCOLT , NQ
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9
      COMMON    /SOL5CM/ ASIZE , DTMAX , DTMIN
      COMMON    /SOL6CM/ RCNDBD, RFROBN, DRMAX, DRMIN

      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      PARAMETER         (LENNP = 35)
      COMMON    /SOL1NP/ LOCNP(LENNP)
      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET
      COMMON    /SOL5NP/ LVRFYC, JVERFY(4)
      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      PARAMETER         (LDBG = 5)
      LOGICAL            CMDBG, NPDBG
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG
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

      LOGICAL            GOODGQ, NEWGQ
      LOGICAL            CENTRL, CONVRG, CONVPT, DONE  , ERROR , FEASQP
      LOGICAL            INFEAS, NEEDFD, OPTIML, OVERFL, UNITQ
      LOGICAL            KTCOND(2)
      INTRINSIC          ABS   , MAX   , MIN   , MOD   , REAL  , SQRT
      EXTERNAL           DDIV  , DDOT  , DNRM2

      CHARACTER*4        LSUMRY
      CHARACTER*2        JOB
      PARAMETER        ( JOB  = 'NP' )
      PARAMETER        ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
      PARAMETER        ( GROWTH=1.0D+2                              )

*     Specify machine-dependent parameters.

      EPSMCH = WMACH(3)
      FLMAX  = WMACH(7)
      RTMAX  = WMACH(8)

      LANORM = LOCLS( 2)
      LRPQ   = LOCLS( 5)
      LQRWRK = LOCLS( 6)
      LHPQ   = LOCLS( 8)
      LGQ    = LOCLS( 9)
      LRLAM  = LOCLS(10)
      LT     = LOCLS(11)
      LZY    = LOCLS(12)
      LWTINF = LOCLS(13)
      LWRK1  = LOCLS(14)
      LQPTOL = LOCLS(15)

      LIPERM = LOCNP( 2)
      LAQP   = LOCNP( 3)
      LADX   = LOCNP( 4)
      LBL    = LOCNP( 5)
      LBU    = LOCNP( 6)
      LDX    = LOCNP( 7)
      LGQ1   = LOCNP( 8)
      LX1    = LOCNP(11)
      LWRK2  = LOCNP(12)
      LCS1   = LOCNP(13)
      LCS2   = LOCNP(14)
      LC1MUL = LOCNP(15)
      LCMUL  = LOCNP(16)
      LCJDX1 = LOCNP(17)
      LDLAM  = LOCNP(18)
      LDSLK  = LOCNP(19)
      LRHO   = LOCNP(20)
      LWRK3  = LOCNP(21)
      LSLK1  = LOCNP(22)
      LSLK   = LOCNP(23)
      LNEEDC = LOCNP(24)
      LHFRWD = LOCNP(25)
      LHCTRL = LOCNP(26)

      LCJAC1 = LAQP   + NCLIN
      LCJDX  = LADX   + NCLIN
      LVIOLN = LWRK3

*     Initialize

      LSUMRY = '    '
      NQPINF = 0

      NPLIN  = N     + NCLIN
      NCQP   = NCLIN + NCNLN
      NL     = MIN( NPLIN + 1, NCTOTL )

      NROWJ1 = MAX( NCQP , 1 )

      NEEDFD = LVLDER .EQ. 0  .OR.  LVLDER .EQ. 2
     $                        .OR. (LVLDER .EQ. 1  .AND.  NCNLN .GT. 0)

      ALFA   = ZERO
      ALFDX  = ZERO
      RTFTOL = SQRT( FTOL )
      ROOTN  = SQRT( REAL(N) )

*     If debug printing is required,  turn off any extensive printing
*     until iteration  IDBG.

      MSGSV1 = MSGNP
      MSGSV2 = MSGQP
      IF (IDBG .LE. NMAJOR  .AND.  IDBG .GT. 0) THEN
         MSGNP = 0
         IF (MSGSV1 .GE. 5) MSGNP = 5
         MSGQP = 0
         IF (MSGSV2 .GE. 5) MSGQP = 5
      END IF

*     ------------------------------------------------------------------
*     Information from the feasibility phase will be used to generate a
*     hot start for the first QP subproblem.
*     ------------------------------------------------------------------
      CALL DCOPY ( NCTOTL, FEATOL, 1, W(LQPTOL), 1 )

      MAJITS = 0
      NSTATE = 0

      LVLDIF = 0
      IF (NEEDFD) LVLDIF = 1

      OBJALF = OBJF
      IF (NCNLN .GT. 0) THEN
         OBJALF = OBJALF - DDOT  ( NCNLN, W(LCMUL), 1, C, 1 )

         INCRUN = .TRUE.
         RHONRM = ZERO
         RHODMP = ONE
         SCALE  = ONE
         CALL DLOAD ( NCNLN, (ZERO), W(LRHO), 1 )
      END IF

      NEWGQ  = .FALSE.

*+    REPEAT
*+       REPEAT

  100       CENTRL = LVLDIF .EQ. 2

            IF (NEWGQ) THEN
               IF (NEEDFD) THEN
*                 ------------------------------------------------------
*                 Compute any missing gradient elements and the
*                 transformed gradient of the objective.
*                 ------------------------------------------------------
                  CALL NPFD  ( CENTRL, MODE,
     $                         NROWJ, NROWUJ, N, NCNLN,
     $                         BIGBND, CDINT, FDINT, FDNORM, OBJF,
     $                         CONFUN, OBJFUN, IW(LNEEDC),
     $                         BL, BU, C, W(LWRK2), W(LWRK3),CJAC,UJAC,
     $                         GRAD, UGRAD, W(LHFRWD), W(LHCTRL), X,
     $                         W, LENW )
                  INFORM = MODE
                  IF (MODE .LT. 0) GO TO 800

               END IF

               CALL DCOPY ( N, GRAD, 1, W(LGQ), 1 )
               CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ,
     $                      KX, W(LGQ), W(LZY), W(LWRK1) )

               NEWGQ  = .FALSE.
            END IF

*           ============================================================
*           (1) Solve an inequality quadratic program (IQP) for the
*               search direction and multiplier estimates.
*           (2) For each nonlinear inequality constraint,  compute
*               the slack variable for which the merit function is
*               minimized.
*           (3) Compute the search direction for the slack variables
*               and multipliers.
*
*           Note that the array VIOLN is WRK3.
*           ============================================================
            CALL NPIQP ( FEASQP, UNITQ, NQPERR, MINITS,
     $                   N, NCLIN, NCNLN, NROWA, NROWJ, NROWQP,NROWR,
     $                   LINACT, NLNACT, NACTIV, NFREE, NZ, NUMINF,
     $                   ISTATE, KACTIV, KX,
     $                   DXNORM, GDX, QPCURV,
     $                   AQP, W(LADX), W(LANORM), AX, BL, BU,
     $                   C, CJAC, CLAMDA, W(LCMUL), W(LCS1),
     $                   W(LDLAM), W(LDSLK), W(LDX), W(LBL), W(LBU),
     $                   W(LQPTOL), R, W(LRHO), W(LSLK), W(LVIOLN), X,
     $                   W(LWTINF), IW, W )

            IF (FEASQP) THEN
               NQPINF = 0
            ELSE
               NQPINF = NQPINF + 1
               LSUMRY(2:2) = 'Infeasible subproblem'
            END IF

*           ============================================================
*           Compute quantities needed for the convergence test.
*           ============================================================
*           Compute the norms of the projected gradient and the
*           gradient with respect to the free variables.

            GZNORM = ZERO
            IF (NZ .GT. 0)
     $         GZNORM = DNRM2 ( NZ   , W(LGQ), 1 )
            GFNORM = GZNORM
            IF (NFREE .GT. 0  .AND.  NACTIV .GT. 0)
     $         GFNORM = DNRM2 ( NFREE, W(LGQ), 1 )

*           If the forward-difference estimate of the transformed
*           gradient of the Lagrangian function is small,  switch to
*           central differences, recompute the derivatives and re-solve
*           the QP.

            GOODGQ = .TRUE.
            IF (NEEDFD  .AND.  .NOT. CENTRL) THEN

               GLNORM = DNRM2 ( N, W(LHPQ), 1 )
               IF (NCNLN .EQ. 0) THEN
                  CNORM = ZERO
               ELSE
                  CNORM = DNRM2 ( NCNLN, C, 1 )
               END IF

               GLTEST = (ONE + ABS(OBJF) + ABS(CNORM))*EPSRF/FDNORM
               IF (GLNORM .LE. GLTEST) THEN
                  GOODGQ      = .FALSE.
                  LSUMRY(3:3) = 'Central differences'
                  LVLDIF      = 2
                  NEWGQ       = .TRUE.
               END IF

            END IF

*+       UNTIL     (GOODGQ)
         IF (.NOT.  GOODGQ ) GO TO 100

*        ===============================================================
*        (1) Compute the number of constraints that are violated by more
*            than FEATOL.
*        (2) Compute the 2-norm of the residuals of the constraints in
*            the QP working set.
*        ===============================================================
         CALL NPFEAS( N, NCLIN, NCNLN, ISTATE,
     $                BIGBND, CVNORM, ERRMAX, JMAX, NVIOL,
     $                AX, BL, BU, C, FEATOL, X, W(LWRK2) )

*        Define small quantities that reflect the magnitude of OBJF and
*        the norm of GRAD(free).

         OBJSIZ = ONE + ABS( OBJF )
         XSIZE  = ONE +  XNORM
         GTEST  = MAX( OBJSIZ, GFNORM )
         DINKY  = RTFTOL * GTEST

         IF (NACTIV .EQ. 0) THEN
            CONDT = ZERO
         ELSE IF (NACTIV .EQ. 1) THEN
            CONDT = DTMIN
         ELSE
            CONDT = DDIV  ( DTMAX, DTMIN, OVERFL )
         END IF

         CALL DCOND ( N, R, NROWR+1, DRMAX, DRMIN )

         CONDH = DDIV  ( DRMAX, DRMIN, OVERFL )
         IF (CONDH .LT. RTMAX) THEN
            CONDH = CONDH*CONDH
         ELSE
            CONDH = FLMAX
         END IF

         IF (NZ .EQ. 0) THEN
            CONDHZ = ONE
         ELSE IF (NZ .EQ. N) THEN
            CONDHZ = CONDH
         ELSE
            CALL DCOND ( NZ, R, NROWR+1, DRZMAX, DRZMIN )
            CONDHZ = DDIV  ( DRZMAX, DRZMIN, OVERFL )
            IF (CONDHZ .LT. RTMAX) THEN
               CONDHZ = CONDHZ*CONDHZ
            ELSE
               CONDHZ = FLMAX
            END IF
         END IF

*        ---------------------------------------------------------------
*        Test for convergence.
*        The point test CONVPT checks for a K-T point at the initial
*        point or after a large change in X.
*        ---------------------------------------------------------------
         CONVPT    = GZNORM .LE. EPSPT8*GTEST  .AND.  NVIOL  .EQ. 0

         KTCOND(1) = GZNORM .LT. DINKY
         KTCOND(2) = NVIOL  .EQ. 0
         OPTIML    = KTCOND(1)  .AND.  KTCOND(2)

         CONVRG    = MAJITS .GT. 0  .AND.  ALFDX .LE. RTFTOL*XSIZE

         INFEAS    =       CONVRG         .AND.  .NOT. FEASQP
     $               .OR.  NQPINF .GT. 7

         DONE      = CONVPT  .OR.  (CONVRG  .AND. OPTIML)
     $                       .OR.   INFEAS

         OBJALF = OBJF
         GRDALF = GDX
         GLF1   = GDX
         IF (NCNLN .GT. 0) THEN
            GLF1   = GLF1
     $                 - DDOT( NCNLN, W(LCJDX), 1, CLAMDA(NL), 1 )

*           Compute the value and directional derivative of the
*           augmented Lagrangian merit function.
*           The penalty parameters may be increased or decreased.

            CALL NPMRT ( FEASQP, N, NCLIN, NCNLN,
     $                   OBJALF, GRDALF, QPCURV,
     $                   ISTATE,
     $                   W(LCJDX), W(LCMUL), W(LCS1),
     $                   W(LDLAM), W(LRHO), W(LVIOLN),
     $                   W(LWRK1), W(LWRK2) )
         END IF

*        ===============================================================
*        Print the details of this iteration.
*        ===============================================================
         CALL NPPRT ( KTCOND, CONVRG, LSUMRY, MSGNP, MSGQP,
     $                NROWR, NROWT, N, NCLIN, NCNLN,
     $                NCTOTL, NACTIV, LINACT, NLNACT, NZ, NFREE,
     $                MAJITS, MINITS, ISTATE, ALFA, NFUN,
     $                CONDHZ, CONDH, CONDT, OBJALF, OBJF,
     $                GFNORM, GZNORM, CVNORM,
     $                AX, C, R, W(LT), W(LVIOLN), X, W(LWRK1) )

         ALFA  = ZERO
         ERROR = MAJITS .GE. NMAJOR

         IF (.NOT. (DONE  .OR.  ERROR)) THEN
            MAJITS = MAJITS + 1

            IF (MAJITS .EQ. IDBG) THEN
               NPDBG = .TRUE.
               CMDBG =  NPDBG
               MSGNP =  MSGSV1
               MSGQP =  MSGSV2
            END IF

*           Make copies of information needed for the BFGS update.

            CALL DCOPY ( N, X     , 1, W(LX1) , 1 )
            CALL DCOPY ( N, W(LGQ), 1, W(LGQ1), 1 )

            IF (NCNLN .GT. 0) THEN
               CALL DCOPY ( NCNLN, W(LCJDX), 1, W(LCJDX1), 1 )
               CALL DCOPY ( NCNLN, W(LCMUL), 1, W(LC1MUL), 1 )
               CALL DCOPY ( NCNLN, W(LSLK) , 1, W(LSLK1) , 1 )
            END IF

*           ============================================================
*           Compute the parameters for the linesearch.
*           ============================================================
*           Compute ALFMAX, the largest feasible step.  Also compute
*           ALFBND,  a tentative upper bound on the step.  If the
*           merit function is decreasing at ALFBND and certain
*           conditions hold,  ALFBND will be increased in multiples
*           of two (subject to not being greater than ALFMAX).

            ALFMAX = DDIV  ( BIGDX, DXNORM, OVERFL )
            ALFMIN = ONE
            IF (.NOT. FEASQP) ALFMIN = ZERO

            CALL NPALF ( INFO, N, NCLIN, NCNLN,
     $                   ALFA, ALFMIN, ALFMAX, BIGBND, DXNORM,
     $                   W(LANORM), W(LADX), AX, BL, BU,
     $                   W(LDSLK), W(LDX), W(LSLK), X )

            ALFMAX = ALFA
            IF (ALFMAX .LT. ONE + EPSPT3  .AND.  FEASQP)
     $         ALFMAX = ONE

            IF (NCNLN .EQ. 0) THEN
               ALFBND = ALFMAX
            ELSE
               IF (NEEDFD) ALFMAX = ONE
               ALFBND = MIN( ONE, ALFMAX )
            END IF
            ALFA   = ONE

            ALFSML = ZERO
            IF (NEEDFD  .AND. .NOT. CENTRL) THEN
               ALFSML = DDIV  ( FDNORM, DXNORM, OVERFL )
               ALFSML = MIN   ( ALFSML, ALFMAX )
            END IF

*           ============================================================
*           Compute the steplength using safeguarded interpolation.
*           ============================================================
            CALL NPSRCH( NEEDFD, NLSERR, N, NCNLN,
     $                   NROWJ, NROWUJ, NFUN, NGRAD,
     $                   IW(LNEEDC), CONFUN, OBJFUN,
     $                   ALFA, ALFBND, ALFMAX, ALFSML, DXNORM,
     $                   EPSRF, ETA, GDX, GRDALF, GLF1, GLF2,
     $                   OBJF, OBJALF, QPCURV, XNORM,
     $                   C, CJAC, UJAC, W(LCJDX),
     $                   W(LC1MUL), W(LCMUL), W(LCS1),
     $                   W(LCS2), W(LDX), W(LDLAM), W(LDSLK), GRAD,
     $                   UGRAD, CLAMDA(NL), W(LRHO),
     $                   W(LSLK1), W(LSLK), W(LX1), X, W, LENW )

*           ------------------------------------------------------------
*           NPSRCH  sets NLSERR to the following values...
*
*           NLSERR will be negative if the user set MODE LT 0.
*
*           Values of NLSERR occurring with a nonzero value of ALFA.
*           1 -- if the search was successful and ALFA LT ALFMAX.
*           2 -- if the search was successful and ALFA  = ALFMAX.
*           3 -- if the search ended after MFSRCH iterations.
*
*           Values of NLSERR occurring with a zero value of ALFA....
*           4 -- if ALFMAX was too small.
*           6 -- if no improved point could be found.
*           7 -- if the input value of GDX is non-negative.
*           ------------------------------------------------------------
            IF (NLSERR .LT. 0) THEN
               INFORM = NLSERR
               GO TO 800
            END IF

            ERROR  = NLSERR .GE. 4
            IF (ERROR) THEN
*              ---------------------------------------------------------
*              The linesearch failed to find a better point.
*              If exact gradients or central differences are being used,
*              or the KT conditions are satisfied, stop.  Otherwise,
*              switch to central differences and re-solve the QP.
*              ---------------------------------------------------------
               IF (NEEDFD  .AND.  .NOT. CENTRL) THEN
                  IF (.NOT. OPTIML) THEN
                     ERROR       = .FALSE.
                     LSUMRY(3:3) = 'Central differences'
                     LVLDIF      = 2
                     NEWGQ       = .TRUE.
                  END IF
               END IF
            ELSE
               IF (NEEDFD) THEN
*                 ======================================================
*                 Compute the missing gradients.
*                 ======================================================
                  MODE  = 1
                  NGRAD = NGRAD + 1

                  IF (NCNLN .GT. 0) THEN
                     CALL ILOAD ( NCNLN, (1), IW(LNEEDC), 1 )

                     CALL CONFUN( MODE, NCNLN, N, NROWUJ, IW(LNEEDC),
     $                            X, W(LWRK1), UJAC, NSTATE )
                     INFORM = MODE
                     IF (MODE .LT. 0) GO TO 800

                     DO 410 J = 1, N
                        CALL DCOPY (NCNLN, UJAC(1,J), 1, CJAC(1,J), 1 )
  410                CONTINUE
                  END IF

                  CALL OBJFUN( MODE, N, X, OBJ, UGRAD, NSTATE )
                  INFORM = MODE
                  IF (MODE .LT. 0) GO TO 800

                  CALL DCOPY ( N, UGRAD, 1, GRAD, 1 )

                  CALL NPFD  ( CENTRL, MODE,
     $                         NROWJ, NROWUJ, N, NCNLN,
     $                         BIGBND, CDINT, FDINT, FDNORM, OBJF,
     $                         CONFUN, OBJFUN, IW(LNEEDC),
     $                         BL, BU, C, W(LWRK2), W(LWRK3),CJAC,UJAC,
     $                         GRAD, UGRAD, W(LHFRWD), W(LHCTRL), X,
     $                         W, LENW )

                  INFORM = MODE
                  IF (MODE .LT. 0) GO TO 800

                  GDX  =  DDOT( N, GRAD, 1, W(LDX), 1 )
                  GLF2 =  GDX
                  IF (NCNLN .GT. 0) THEN
                     CALL DGEMV ( 'N', NCNLN, N, ONE, CJAC, NROWJ,
     $                            W(LDX), 1, ZERO, W(LCJDX), 1 )
                     GLF2 = GLF2 -
     $                      DDOT( NCNLN, W(LCJDX), 1, CLAMDA(NL), 1 )
                  END IF
               END IF

               CALL DCOPY ( N, GRAD, 1, W(LGQ), 1 )
               CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ,
     $                      KX, W(LGQ), W(LZY), W(LWRK1) )

               XNORM  = DNRM2 ( N, X, 1 )

               IF (NCNLN .GT. 0  .AND.  ALFA .GE. ONE)
     $            CALL DCOPY ( NCNLN, CLAMDA(NL), 1, W(LCMUL), 1 )

               IF (NCLIN .GT. 0)
     $            CALL DAXPY ( NCLIN, ALFA, W(LADX), 1, AX, 1 )
               ALFDX   = ALFA * DXNORM

*              =========================================================
*              Update the factors of the approximate Hessian of the
*              Lagrangian function.
*              =========================================================
               CALL NPUPDT( LSUMRY, UNITQ,
     $                      N, NCNLN, NFREE, NZ,
     $                      NROWJ1, NROWJ, NQ, NROWR, KX,
     $                      ALFA, GLF1, GLF2, QPCURV,
     $                      W(LCJAC1), CJAC, W(LCJDX1), W(LCJDX),
     $                      W(LCS1), W(LCS2), W(LGQ1), W(LGQ),
     $                      W(LHPQ), W(LRPQ), CLAMDA(NL), R,
     $                      W(LWRK3), W(LZY), W(LWRK2), W(LWRK1) )

               CALL DCOND ( N, R, NROWR+1, DRMAX, DRMIN )
               COND   = DDIV  ( DRMAX, DRMIN, OVERFL )

               IF (      COND   .GT. RCNDBD
     $             .OR.  RFROBN .GT. ROOTN*GROWTH*DRMAX) THEN
*                 ------------------------------------------------------
*                 Reset the condition estimator and range-space
*                 partition of Q'HQ.
*                 ------------------------------------------------------
                  IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $               WRITE (NOUT, 9000) RFROBN, DRMAX, DRMIN,COND,RCNDBD

                  LSUMRY(4:4) = 'Refactorize Hessian'

                  CALL NPRSET( UNITQ,
     $                         N, NFREE, NZ, NQ, NROWR,
     $                         IW(LIPERM), KX,
     $                         W(LGQ), R, W(LZY), W(LWRK1), W(LQRWRK) )
               END IF
            END IF
         END IF

*+    UNTIL     (DONE  .OR.  ERROR)
      IF (.NOT. (DONE  .OR.  ERROR) ) GO TO 100

*     ======================end of main loop============================

      IF (DONE) THEN
         IF (CONVPT  .OR.  OPTIML) THEN
            INFORM = 0
         ELSE IF (INFEAS) THEN
            INFORM = 3
         END IF
      ELSE IF (ERROR) THEN
         IF (MAJITS .GE. NMAJOR) THEN
            INFORM = 4
         ELSE IF (OPTIML) THEN
            INFORM = 1
         ELSE
            INFORM = 6
         END IF
      END IF

*     ------------------------------------------------------------------
*     Set  CLAMDA.  Print the full solution.
*     ------------------------------------------------------------------
  800 MSGNP = MSGSV1
      MSGQP = MSGSV2
      IF (MSGNP .GT. 0)
     $   WRITE (NOUT, 2100) INFORM, MAJITS, NFUN, NGRAD

      CALL CMPRT ( MSGNP, NFREE, NROWQP,
     $             N, NCLIN, NCNLN, NCTOTL, BIGBND,
     $             NAMED, NAMES, LENNAM,
     $             NACTIV, ISTATE, KACTIV, KX,
     $             AQP, BL, BU, C, CLAMDA, W(LRLAM), X )

      RETURN

 2100 FORMAT(/ ' Exit  NP phase.  INFORM = ', I2, ' MAJITS = ', I5,
     $         '   NFUN = ', I5, '   NGRAD = ', I5 )

 9000 FORMAT(/ ' //NPCORE//        RFROBN         DRMAX         DRMIN'
     $       / ' //NPCORE//', 1P3E14.2
     $       / ' //NPCORE//          COND        RCNDBD'
     $       / ' //NPCORE//', 1P2E14.2 )

*     End of  NPCORE.

      END
