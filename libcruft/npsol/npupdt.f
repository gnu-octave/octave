*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPUPDT( LSUMRY, UNITQ,
     $                   N, NCNLN, NFREE, NZ,
     $                   NROWJ1, NROWJ2, NQ, NROWR, KX,
     $                   ALFA, GLF1, GLF2, QPCURV,
     $                   CJAC1, CJAC2, CJDX1, CJDX2,
     $                   CS1, CS2, GQ1, GQ2, HPQ, RPQ,
     $                   QPMUL, R, OMEGA, ZY, WRK1, WRK2 )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*4        LSUMRY
      LOGICAL            UNITQ
      INTEGER            KX(N)
      DOUBLE PRECISION   CJAC1(NROWJ1,*), CJAC2(NROWJ2,*),
     $                   CJDX1(*), CJDX2(*), CS1(*), CS2(*),
     $                   GQ1(N), GQ2(N), HPQ(N), RPQ(N), QPMUL(*),
     $                   R(NROWR,*), OMEGA(*), ZY(NQ,*)
      DOUBLE PRECISION   WRK1(N+NCNLN), WRK2(N)

************************************************************************
*  NPUPDT  computes the BFGS update for the approximate Hessian of the
*  Lagrangian.  If the approximate curvature of the Lagrangian function
*  is negative,  a nonnegative penalty vector OMEGA(i) of minimum two
*  norm is computed such that the approximate curvature of the augmented
*  Lagrangian will be positive. If no finite penalty vector exists,  the
*  BFGS update is performed with the approximate curvature modified to
*  be a small positive value.
*
*  On entry,  GQ1 and GQ2 contain the transformed objective gradients at
*  X1 and X2,  HPQ contains  R'R(pq), the transformed Hessian times the
*  transformed search direction.  The vectors GQ1 and HPQ are not saved.
*  If the regular BFGS quasi-Newton update could not be performed, the
*  first character of LSUMRY is loaded with 'M'.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 66 version written April 1984.
*  Level 2 BLAS added 12-June-1986.
*  This version of NPUPTD dated  4-August-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL6CM/ RCNDBD, RFROBN, DRMAX, DRMIN

      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      LOGICAL            NPDBG
      PARAMETER        ( LDBG   = 5 )
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            OVERFL, SSBFGS
      INTRINSIC          MAX   , MIN   , SQRT
      EXTERNAL           IDAMAX, DDIV  , DDOT  , DNRM2
      PARAMETER        ( ZERO   = 0.0D+0, ONE    = 1.0D+0 )
      PARAMETER        ( TOLG   = 1.0D-1                  )

      IF (NCNLN .GT. 0) CALL DLOAD ( NCNLN, ZERO, OMEGA, 1 )

*     ------------------------------------------------------------------
*     Set CURVL = (G2 - G1)'DX,  the approximate curvature along DX of
*     the (augmented) Lagrangian.  At first, the curvature is not scaled
*     by the steplength ALFA.
*     ------------------------------------------------------------------
      CURVL  = GLF2 -   GLF1
      TINYCL =        QPCURV * TOLG
      SSBFGS = CURVL .LE. ALFA*TINYCL
      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1000) SSBFGS, TINYCL, CURVL

*     ------------------------------------------------------------------
*     Test if CURVL is sufficiently positive.  If there are no nonlinear
*     constraints,  no update can be performed.
*     ------------------------------------------------------------------
      IF (CURVL  .LT. TINYCL) THEN
         LSUMRY(1:1) = 'Modified BFGS'
         IF (NCNLN .GT. 0) THEN
            QMAX = ZERO
            DO 200 I = 1, NCNLN
               QI  = CJDX2(I)*CS2(I) - CJDX1(I)*CS1(I)
               QMAX = MAX( QMAX, QI )
               IF (QI .LE. ZERO) WRK1(I) = ZERO
               IF (QI .GT. ZERO) WRK1(I) = QI
  200       CONTINUE

            QNORM = DNRM2 ( NCNLN, WRK1, 1 )

            TEST  = MAX( TINYCL - CURVL, ZERO )
            BETA  = DDIV  ( QMAX*TEST, QNORM*QNORM, OVERFL )
            IF (BETA .LT. RHOMAX  .AND.  .NOT. OVERFL) THEN
               LSUMRY(1:1) = ' '
               BETA  = TEST/(QNORM*QNORM)
               DO 210 I = 1, NCNLN
                  QI       = WRK1(I)
                  OMEGA(I) =            BETA*QI
                  CURVL    = CURVL    + BETA*QI*QI
  210          CONTINUE

               IF (NPDBG) THEN
                  IMAX = IDAMAX( NCNLN, OMEGA, 1 )
                  IF (INPDBG(1) .GT. 0)
     $               WRITE (NOUT, 1250) OMEGA(IMAX)

                  IF (INPDBG(2) .GT. 0)
     $               WRITE (NOUT, 1300) (OMEGA(I), I=1,NCNLN)
               END IF
            END IF
         END IF
      END IF

*     ------------------------------------------------------------------
*     Compute the difference in the augmented Lagrangian gradient.
*     ------------------------------------------------------------------
*     Update GQ1 to include the augmented Lagrangian terms.

      IF (NCNLN .GT. 0) THEN

         DO 310 I = 1, NCNLN
            WRK1(I) = - QPMUL(I) + OMEGA(I) * CS1(I)
  310    CONTINUE
         CALL DGEMV ( 'T', NCNLN, N, ONE, CJAC1, NROWJ1, WRK1, 1,
     $                ZERO, WRK2, 1 )

         DO 320 I = 1, NCNLN
            WRK1(I) =   QPMUL(I) - OMEGA(I) * CS2(I)
  320    CONTINUE
         CALL DGEMV ( 'T', NCNLN, N, ONE, CJAC2, NROWJ2, WRK1, 1,
     $                ONE, WRK2, 1 )

         CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ, KX, WRK2, ZY, WRK1 )
         CALL DAXPY ( N, ONE, WRK2, 1, GQ1, 1 )
      END IF

      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1100) ALFA  , CURVL

      IF (CURVL .LT. TINYCL) CURVL  = TINYCL

      DO 330 J = 1, N
         WRK2(J) = GQ2(J) - GQ1(J)
  330 CONTINUE

      RTGTP  = SQRT(QPCURV)
      RTYTS  = SQRT(ALFA*CURVL)
      ETA    = ONE
      IF (SSBFGS)
     $   ETA = RTYTS / (RTGTP*ALFA)

      TRACE1 = DNRM2 ( N,  HPQ, 1 ) /  RTGTP
      TRACE2 = DNRM2 ( N, WRK2, 1 ) / (RTYTS*ETA)
      RFROBN = ETA*SQRT( ABS(  (RFROBN - TRACE1)*(RFROBN + TRACE1)
     $                                 + TRACE2**2) )

*     ==================================================================
*     Update the Cholesky factor of  Q'HQ.
*     ==================================================================
*     Normalize the vector  RPQ ( = R(pq) ).

      CALL DSCAL ( N, (ONE / RTGTP), RPQ, 1 )

*     Do the self-scaled or regular BFGS update.
*     Form the vector WRK1 = gamma * (GQ2 - GQ1) - beta * R'R*PQ,
*     where  gamma = 1/SQRT( CURV ) = 1/SQRT( (GQ2 - GQ1)'SQ )

      CALL DSCAL ( N, (ONE / RTGTP), HPQ, 1 )

      IF (SSBFGS) THEN
         DO 410 J   = 1, N
            CALL DSCAL ( J, ETA, R(1,J), 1 )
            WRK1(J) = WRK2(J)/RTYTS  -  ETA * HPQ(J)
  410    CONTINUE
      ELSE
         DO 420 J   = 1, N
            WRK1(J) = WRK2(J)/RTYTS  -        HPQ(J)
  420    CONTINUE
      END IF

*     Perform the update to  R = R + RPQ*WRK1'.
*     RPQ is overwritten and HPQ is used as workspace.

      CALL CMR1MD( N, 0, N, NROWR, N, N, R, HPQ, RPQ, WRK1 )

      RETURN

 1000 FORMAT(/ ' //NPUPDT// SSBFGS    min. CURVL         CURVL '
     $       / ' //NPUPDT//   ', L4, 1P2E14.2 )
 1100 FORMAT(/ ' //NPUPDT//          ALFA         CURVL '
     $       / ' //NPUPDT//', 1P2E14.2 )
 1250 FORMAT(/ ' //NPUPDT//   OMEGA(IMAX)'
     $       / ' //NPUPDT//', 1PE14.2 )
 1300 FORMAT(/ ' //NPUPDT//  Penalty parameters = '  / (1P5E15.6))

*     End of  NPUPDT.

      END
