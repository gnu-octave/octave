*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPSRCH( NEEDFD, INFORM, N, NCNLN,
     $                   NROWJ, NROWUJ, NFUN, NGRAD,
     $                   NEEDC, CONFUN, OBJFUN,
     $                   ALFA, ALFBND, ALFMAX, ALFSML, DXNORM,
     $                   EPSRF, ETA, GDX, GRDALF, GLF1, GLF,
     $                   OBJF, OBJALF, QPCURV, XNORM,
     $                   C, CJAC, UJAC, CJDX, CMUL1, CMUL, CS1, CS,
     $                   DX, DLAM, DSLK, GRAD, UGRAD, QPMUL, RHO,
     $                   SLK1, SLK, X1, X, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            NEEDFD
      INTEGER            NEEDC(*)
      DOUBLE PRECISION   DX(N), GRAD(N), UGRAD(N), X1(N), X(N)
      DOUBLE PRECISION   C(*), CJAC(NROWJ,*), UJAC(NROWUJ,*), CJDX(*),
     $                   CMUL1(*), CMUL(*), CS1(*), CS(*)
      DOUBLE PRECISION   DLAM(*), DSLK(*), QPMUL(*),
     $                   RHO(*), SLK1(*), SLK(*)
      DOUBLE PRECISION   W(LENW)
      EXTERNAL           OBJFUN, CONFUN

************************************************************************
*  NPSRCH finds the steplength ALFA that gives sufficient decrease in
*  the augmented Lagrangian merit function.
*
*  On exit,  if INFORM = 1, 2 or 3,  ALFA will be a nonzero steplength
*  with an associated merit function value  OBJALF  which is lower than
*  that at the base point. If  INFORM = 4, 5, 6 or 7,  ALFA  is zero
*  and  OBJALF will be the merit value at the base point.
*
*  Systems Optimization Laboratory, Stanford University, California.
*  Original version written  27-May-1985.
*  Level 2 BLAS added 12-June-1986.
*  This version of NPSRCH dated 12-July-1986.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      PARAMETER         (LENNP = 35)
      COMMON    /SOL1NP/ LOCNP(LENNP)
      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET
      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      LOGICAL            NPDBG
      PARAMETER        ( LDBG = 5 )
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            DEBUG , DONE  , FIRST , IMPRVD
      EXTERNAL           DDOT  , DNRM2
      INTRINSIC          ABS   , MAX   , MIN   , SQRT
      PARAMETER        ( ZERO   =0.0D+0, HALF   =0.5D+0, ONE   =1.0D+0 )
      PARAMETER        ( TWO    =2.0D+0                                )
      PARAMETER        ( TOLG   =1.0D-1                                )

      EPSMCH = WMACH(3)

      LC     = LOCLS(14)
      LWORK  = LOCNP(12)
      LCJDX  = LOCNP(21)

      IF (.NOT. NEEDFD  .AND.  NCNLN .GT. 0)
     $   CS1JDX = DDOT( NCNLN, CS1, 1, CJDX, 1 )

*     ------------------------------------------------------------------
*     Set the input parameters and tolerances for SRCHC and SRCHQ.
*
*     TOLRX   is the tolerance on relative changes in DX resulting from
*             changes in ALFA.
*
*     TOLAX   is the tolerance on absolute changes in DX resulting from
*             changes in ALFA.
*
*     TOLABS  is the tolerance on absolute changes in ALFA.
*
*     TOLREL  is the tolerance on relative changes in ALFA.
*
*     TOLTNY  is the magnitude of the smallest allowable value of ALFA.
*             If  M(TOLABS) - M(0) .gt. EPSAF,  the linesearch tries
*             steps in the range  TOLTNY .LE. ALFA .LE. TOLABS.
*     ------------------------------------------------------------------
      NSTATE = 0
      DEBUG  = NPDBG  .AND.  INPDBG(4) .GT. 0

      EPSAF  = EPSRF*(ONE + ABS( OBJALF ))

      TOLAX  = EPSPT8
      TOLRX  = EPSPT8

      TOLABS = ALFMAX
      IF (TOLRX*XNORM + TOLAX .LT. DXNORM*ALFBND)
     $   TOLABS = (TOLRX*XNORM + TOLAX) /  DXNORM
      TOLREL = MAX( TOLRX , EPSMCH )

      T      = ZERO
      DO 10 J = 1, N
         S = ABS( DX(J) )
         Q = ABS( X(J) )*TOLRX + TOLAX
         IF (S .GT. T*Q) T = S / Q
   10 CONTINUE

      TOLTNY = TOLABS
      IF (T*TOLABS .GT. ONE) TOLTNY = ONE / T

      OLDF   = OBJALF
      OLDG   = GRDALF

      IF (NCNLN .GT. 0) CALL ILOAD ( NCNLN, (1), NEEDC, 1 )

      MODE  = 2
      IF (NEEDFD) MODE = 0

      FIRST  = .TRUE.

*     ------------------------------------------------------------------
*     Commence main loop, entering SRCHC or SRCHQ two or more times.
*     FIRST = true for the first entry,  false for subsequent entries.
*     DONE  = true indicates termination, in which case the value of
*     INFORM gives the result of the search.
*     ------------------------------------------------------------------
*+    REPEAT
  100    IF (NEEDFD) THEN
            CALL SRCHQ ( DEBUG, DONE, FIRST, IMPRVD, INFORM,
     $                   ALFMAX, ALFSML, EPSAF, ETA,
     $                   XTRY, FTRY,       OLDF, OLDG,
     $                   TOLABS, TOLREL, TOLTNY,
     $                   ALFA, ALFBST, FBEST        )
         ELSE
            CALL SRCHC ( DEBUG, DONE, FIRST, IMPRVD, INFORM,
     $                   ALFMAX,         EPSAF, ETA,
     $                   XTRY, FTRY, GTRY, OLDF, OLDG,
     $                   TOLABS, TOLREL, TOLTNY,
     $                   ALFA, ALFBST, FBEST, GBEST )
         END IF

         IF (IMPRVD) THEN
            OBJF   = TOBJ
            OBJALF = FTRY

            IF (NCNLN .GT. 0)
     $         CALL DCOPY ( NCNLN, W(LC), 1, C, 1 )

            IF (.NOT. NEEDFD) THEN
               CALL DCOPY ( N, UGRAD, 1, GRAD, 1 )
               GDX    = TGDX
               GLF    = TGLF

               IF (NCNLN .GT. 0) THEN
                  CALL DCOPY ( NCNLN, W(LCJDX), 1, CJDX, 1 )
                  DO 120  J = 1, N
                     CALL DCOPY ( NCNLN, UJAC(1,J), 1, CJAC(1,J), 1 )
  120             CONTINUE
               END IF
            END IF
         END IF

*        ---------------------------------------------------------------
*        If DONE = FALSE,  the problem functions must be computed for
*        the next entry to SRCHC or SRCHQ.
*        If DONE = TRUE,   this is the last time through.
*        ---------------------------------------------------------------
         IF (.NOT. DONE) THEN

            NFUN  = NFUN  + 1
            IF (.NOT. NEEDFD) NGRAD = NGRAD + 1

            CALL DCOPY ( N,       X1, 1, X, 1 )
            CALL DAXPY ( N, ALFA, DX, 1, X, 1 )
            IF (NCNLN .GT. 0) THEN

*              Compute the new estimates of the multipliers and slacks.
*              If the step length is greater than one,  the multipliers
*              are fixed as the QP-multipliers.

               IF (ALFA .LE. ONE) THEN
                  CALL DCOPY ( NCNLN,       CMUL1, 1, CMUL, 1 )
                  CALL DAXPY ( NCNLN, ALFA, DLAM , 1, CMUL, 1 )
               END IF
               CALL DCOPY ( NCNLN,       SLK1, 1, SLK, 1 )
               CALL DAXPY ( NCNLN, ALFA, DSLK, 1, SLK, 1 )

*              ---------------------------------------------------------
*              Compute the new constraint vector and Jacobian.
*              ---------------------------------------------------------
               CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                      NEEDC, X, W(LC), UJAC, NSTATE )
               IF (MODE .LT. 0) GO TO 999

               CALL DCOPY ( NCNLN,         W(LC), 1, CS, 1 )
               CALL DAXPY ( NCNLN, (-ONE), SLK  , 1, CS, 1 )

               CALL DCOPY ( NCNLN, CS , 1, W(LWORK), 1 )
               CALL DDSCL ( NCNLN, RHO, 1, W(LWORK), 1 )

               FTERM  =            DDOT( NCNLN, CMUL    , 1, CS, 1 ) -
     $                  HALF*SCALE*DDOT( NCNLN, W(LWORK), 1, CS, 1 )

            END IF

*           ------------------------------------------------------------
*           Compute the value and gradient of the objective function.
*           ------------------------------------------------------------
            CALL OBJFUN( MODE, N, X, TOBJ, UGRAD, NSTATE )
            IF (MODE .LT. 0) GO TO 999

            FTRY   = TOBJ
            IF (NCNLN .GT. 0) FTRY = TOBJ  - FTERM

*           ------------------------------------------------------------
*           Compute auxiliary gradient information.
*           ------------------------------------------------------------
            IF (.NOT. NEEDFD) THEN
               GTRY   = DDOT( N, UGRAD, 1, DX, 1 )
               TGDX   = GTRY
               TGLF   = GTRY
               IF (NCNLN .GT. 0) THEN

*                 Compute the Jacobian times the search direction.

                  CALL DGEMV ( 'N', NCNLN, N, ONE, UJAC, NROWUJ, DX, 1,
     $                         ZERO, W(LCJDX), 1 )

                  CALL DCOPY ( NCNLN,         W(LCJDX), 1, W(LWORK), 1 )
                  CALL DAXPY ( NCNLN, (-ONE), DSLK    , 1, W(LWORK), 1 )

                  GTRY   = GTRY - DDOT( NCNLN, CMUL, 1, W(LWORK), 1 )
                  IF (ALFA .LE. ONE)
     $               GTRY   = GTRY - DDOT( NCNLN, DLAM, 1, CS      , 1 )

                  CALL DDSCL ( NCNLN, RHO , 1, W(LWORK), 1 )
                  GTRY = GTRY  +
     $                     SCALE*DDOT( NCNLN, W(LWORK), 1, CS   , 1 )

                  TGLF = TGDX  - DDOT( NCNLN, W(LCJDX), 1, QPMUL, 1 )

*                 ------------------------------------------------------
*                 If ALFBND .LE. ALFA .LT. ALFMAX and the norm of the
*                 quasi-Newton update is bounded, set ALFMAX to be ALFA.
*                 This will cause the linesearch to stop if the merit
*                 function is decreasing at the boundary.
*                 ------------------------------------------------------
                  IF (ALFBND .LE. ALFA  .AND.  ALFA .LT. ALFMAX) THEN

                     CSJDX  = DDOT   ( NCNLN, CS, 1, W(LCJDX), 1 )

                     IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $                  WRITE (NOUT, 1400) CSJDX, CS1JDX, CURVLF

                     CURVLF = TGLF  - GLF1
                     CURVC  = ABS( CSJDX - CS1JDX )
                     RHOBFS = MAX( QPCURV*TOLG - CURVLF, ZERO )
                     IF (RHOBFS .LE. CURVC*RHOMAX) THEN
                        ALFMAX = ALFA
                     ELSE
                        ALFBND = MIN( TWO*ALFA, ALFMAX )
                     END IF
                     IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $                  WRITE(NOUT,1300) ALFBND, ALFA, ALFMAX
                  END IF
               END IF
            END IF
         END IF
*+    UNTIL (      DONE)
      IF    (.NOT. DONE) GO TO 100

      ALFA = ALFBST
      IF (.NOT. IMPRVD) THEN
         CALL DCOPY ( N,       X1, 1, X, 1 )
         CALL DAXPY ( N, ALFA, DX, 1, X, 1 )
         IF (NCNLN .GT. 0) THEN
            IF (ALFA .LE. ONE) THEN
               CALL DCOPY ( NCNLN,       CMUL1, 1, CMUL, 1 )
               CALL DAXPY ( NCNLN, ALFA, DLAM , 1, CMUL, 1 )
            END IF
            CALL DCOPY ( NCNLN,         SLK1 , 1, SLK, 1 )
            CALL DAXPY ( NCNLN,   ALFA, DSLK , 1, SLK, 1 )
            CALL DCOPY ( NCNLN,         C    , 1, CS , 1 )
            CALL DAXPY ( NCNLN, (-ONE), SLK  , 1, CS , 1 )
         END IF
      END IF

      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1200) INFORM

      RETURN

*     The user wants to stop.

  999 INFORM = MODE
      RETURN

 1200 FORMAT(/ ' //NPSRCH// INFORM  = ', I4 )
 1300 FORMAT(/ ' //NPSRCH//        ALFBND          ALFA        ALFMAX'
     $       / ' //NPSRCH//', 1P3E14.2 )
 1400 FORMAT(/ ' //NPSRCH//         CSJDX        CS1JDX        CURVLF'
     $       / ' //NPSRCH//', 1P3E14.2 )

*     End of  NPSRCH.

      END
