*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPRSET( UNITQ,
     $                   N, NFREE, NZ, NQ, NROWR,
     $                   IPERM, KX,
     $                   GQ, R, ZY, WORK, QRWORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            IPERM(N), KX(N)
      DOUBLE PRECISION   GQ(N), R(NROWR,*), ZY(NQ,*)
      DOUBLE PRECISION   WORK(N), QRWORK(2*N)

************************************************************************
*  NPRSET  bounds the condition estimator of the transformed Hessian.
*  On exit, R is of the form
*               ( DRz   0     )
*               (  0  sigma*I )
*  where D is a diagonal matrix such that DRz has a bounded condition
*  number,  I is the identity matrix and sigma  is the geometric mean
*  of the largest and smallest elements of DRz. The QR factorization
*  with interchanges is used to give diagonals of DRz that are
*  decreasing in modulus.
*
*  Systems Optimization Laboratory, Stanford University.
*  This version of NPRSET dated  4-August-1986.
************************************************************************

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL6CM/ RCNDBD, RFROBN, DRMAX, DRMIN

      LOGICAL            NPDBG
      PARAMETER         (LDBG   = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            OVERFL
      INTRINSIC          MAX   , MIN   , LOG   , REAL  , SQRT
      EXTERNAL           DDIV  , DDOT  , DNORM , DNRM2
      PARAMETER        ( ZERO   =0.0D+0, HALF =0.5D+0, ONE    =1.0D+0 )

*     ==================================================================
*     Bound the condition estimator of Q'HQ.
*     The scheme used here reduces the modulus of the larger
*     diagonals while increasing the modulus of the smaller ones.
*     ==================================================================
      IF (NZ .GT. 1) THEN
*        ---------------------------------------------------------------
*        Refactorize Rz.  Interchanges are used to give diagonals
*        of decreasing magnitude.
*        ---------------------------------------------------------------
         CALL DGEQRP( 'Column iterchanges', NZ, NZ, R, NROWR,
     $                WORK, IPERM, QRWORK, INFO )

         DO 110 J = 1, NZ
            JMAX = IPERM(J)
            IF (JMAX .GT. J) THEN
               IF (UNITQ) THEN
                  JSAVE    = KX(JMAX)
                  KX(JMAX) = KX(J)
                  KX(J)    = JSAVE
               ELSE
                  CALL DSWAP ( NFREE, ZY(1,JMAX), 1, ZY(1,J), 1 )
               END IF

               GJMAX    = GQ(JMAX)
               GQ(JMAX) = GQ(J)
               GQ(J)    = GJMAX
            END IF
  110    CONTINUE
      END IF

      IF (NZ .EQ. 0) THEN
         DRGM  = ONE
      ELSE
         COND  = DDIV  ( ABS(R(1,1)), ABS(R(NZ,NZ)), OVERFL )

         IF (COND .GT. RCNDBD) THEN
            IF (N .GT. 1) THEN
               PWR = LOG(RCNDBD)/LOG(COND) - ONE
               DO 120 K = 1, NZ
                  ROWSCL = ABS( R(K,K) )**PWR
                  CALL DSCAL ( NZ-K+1, ROWSCL, R(K,K), NROWR )
  120          CONTINUE
            END IF
         END IF
         DRGM  = HALF*SQRT(ABS( R(1,1)*R(NZ,NZ) ))
      END IF

*     Reset the range-space partition of the Hessian.

      IF (NZ .LT. N) THEN
         DO 130 J = NZ+1, N
            CALL DLOAD ( J, ZERO, R(1,J), 1 )
  130    CONTINUE
         CALL DLOAD ( N-NZ, DRGM, R(NZ+1,NZ+1), NROWR+1 )
      END IF

*     Recompute the Frobenius norm of R.

      SCLE  = SQRT(REAL(N - NZ))*DRGM
      SUMSQ = ONE
      DO 140 J = 1, NZ
         CALL DSSQ  ( J, R(1,J), 1, SCLE, SUMSQ )
  140 CONTINUE
      RFROBN = DNORM( SCLE, SUMSQ )

      RETURN

*     End of  NPRSET.

      END
