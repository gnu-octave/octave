*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPSETX( UNITQ,
     $                   NCQP, NACTIV, NFREE, NZ,
     $                   N, NLNX, NCTOTL, NQ, NROWQP, NROWR, NROWT,
     $                   ISTATE, KACTIV, KX,
     $                   DXNORM, GDX,
     $                   AQP, ADX, BL, BU, RPQ, RPQ0, DX, GQ,
     $                   R, T, ZY, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            ISTATE(NCTOTL), KACTIV(N), KX(N)
      DOUBLE PRECISION   AQP(NROWQP,*), ADX(*), BL(NCTOTL), BU(NCTOTL),
     $                   RPQ(NLNX), RPQ0(NLNX), GQ(N), R(NROWR,*),
     $                   T(NROWT,*), ZY(NQ,*), DX(N), WORK(N)
************************************************************************
*  NPSETX   defines a point which lies on the initial working set for
*  the QP subproblem.  This routine is a similar to LSSETX except that
*  advantage is taken of the fact that the initial estimate of the
*  solution of the least-squares subproblem is zero.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version written 31-October-1984.
*  Level 2 BLAS added 12-June-1986.
*  This version of NPSETX dated 11-June-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      EXTERNAL           DDOT, DNRM2
      INTRINSIC          ABS , MIN
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      NFIXED = N - NFREE

      GDX    = ZERO
      CALL DLOAD ( N   , ZERO, DX  , 1 )
      CALL DLOAD ( NLNX, ZERO, RPQ , 1 )
      CALL DLOAD ( NLNX, ZERO, RPQ0, 1 )

      IF (NACTIV + NFIXED .GT. 0) THEN

*        Set  work = residuals for constraints in the working set.
*        Solve for  dx,  the smallest correction to  x  that gives a
*        point on the constraints in the working set.
*        Set the fixed variables on their bounds,  solve the triangular
*        system  T*(dxy) = residuals,  and define  dx = Y*(dxy).
*        Use  (dxy)  to update  d(=Pr)  as  d = d - R'(  0  ).
*                                                     ( dxy )

         DO 100 I = 1, NFIXED
            J   = KX(NFREE+I)
            IF (ISTATE(J) .LE. 3) THEN
               BND   = BL(J)
               IF (ISTATE(J) .EQ. 2) BND = BU(J)
               DX(J) = BND
               WORK(NFREE+I) = BND
            ELSE
               WORK(NFREE+I) = ZERO
            END IF
  100    CONTINUE

         DO 110 I = 1, NACTIV
            K   = KACTIV(I)
            J   = N + K
            BND = BL(J)
            IF (ISTATE(J) .EQ. 2) BND = BU(J)
            WORK(NZ+I) = BND - DDOT  ( N, AQP(K,1), NROWQP, DX, 1 )
  110    CONTINUE

         IF (NACTIV .GT. 0)
     $      CALL CMTSOL( 1, NROWT, NACTIV, T(1,NZ+1), WORK(NZ+1) )
         CALL DCOPY ( NACTIV+NFIXED, WORK(NZ+1), 1, DX(NZ+1), 1 )
         IF (NZ .GT. 0)
     $      CALL DLOAD ( NZ, ZERO, DX, 1 )

         GDX  = DDOT  ( NACTIV+NFIXED, GQ(NZ+1), 1, DX(NZ+1), 1 )

         IF (NZ .LT. N) THEN
            CALL DGEMV ('N', NZ, N-NZ, -ONE, R(1,NZ+1), NROWR,
     $                  DX(NZ+1), 1, ONE, RPQ, 1 )
            IF (NZ .LT. NLNX) THEN
               NR  = NROWR
               IF (NZ+1 .EQ. N) NR = 1
               CALL DCOPY ( NLNX-NZ, DX(NZ+1), 1, RPQ(NZ+1), 1 )
               CALL DSCAL ( NLNX-NZ, (-ONE),      RPQ(NZ+1), 1 )
               CALL DTRMV ( 'U', 'N', 'N', NLNX-NZ, R(NZ+1,NZ+1), NR,
     $                      RPQ(NZ+1), 1 )
               IF (NLNX .LT. N) THEN
                  NR = NROWR
                  IF (NLNX+1 .EQ. N) NR = N - NZ
                  CALL DGEMV( 'N', NLNX-NZ, N-NLNX, -ONE,R(NZ+1,NLNX+1),
     $                        NR, DX(NLNX+1), 1, ONE, RPQ(NZ+1), 1 )
               END IF
            END IF
         END IF

         CALL CMQMUL( 2, N, NZ, NFREE, NQ, UNITQ, KX, DX, ZY, WORK )
      END IF

*     ------------------------------------------------------------------
*     Compute the 2-norm of  DX.
*     Initialize  A*DX.
*     ------------------------------------------------------------------
      DXNORM  = DNRM2 ( N, DX, 1 )
      IF (NCQP .GT. 0)
     $   CALL DGEMV ( 'N', NCQP, N, ONE, AQP, NROWQP, DX, 1, ZERO,ADX,1)

      IF (NPDBG  .AND.  INPDBG(2) .GT. 0)
     $   WRITE (NOUT, 1200) (DX(J), J = 1, N)

      RETURN

 1200 FORMAT(/ ' //NPSETX// Variables after NPSETX ... '/ (5G12.3))

*     End of  NPSETX.

      END
