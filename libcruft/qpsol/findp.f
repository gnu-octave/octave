      SUBROUTINE FINDP ( NULLR, UNITPG, UNITQ,
     *                   N, NCLIN, NCLIN0, NCTOTL, NQ,
     *                   NROWA, NROWRT, NCOLRT, NCOLR, NCOLZ, NFREE,
     *                   ISTATE, KFREE,
     *                   DINKY, GTP, PNORM, RDLAST, ZTGNRM,
     *                   A, AP, P, QTG, RT, V, ZY, WORK )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            NULLR, UNITPG, UNITQ
      INTEGER            N, NCLIN, NCLIN0, NCTOTL, NQ, NROWA,
     *                   NROWRT, NCOLRT, NCOLR, NFREE
      INTEGER            ISTATE(NCTOTL), KFREE(N)
      DOUBLE PRECISION   DINKY, GTP, PNORM, RDLAST, ZTGNRM
      DOUBLE PRECISION   A(NROWA,N), AP(NCLIN0), QTG(N), P(N),
     *                   RT(NROWRT,NCOLRT), V(N), ZY(NQ,NQ)
      DOUBLE PRECISION   WORK(N)
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C  FINDP   COMPUTES THE FOLLOWING QUANTITIES FOR  LPCORE,  QPCORE  AND
C  LCCORE ...
C
C  (1) THE SEARCH DIRECTION  P  (AND ITS 2-NORM).
C  (2) THE VECTOR  V  SUCH THAT  R(T)V = - Z(T)G(FREE).  THIS VECTOR IS
C      REQUIRED BY  LCCORE  ONLY.
C  (3) THE VECTOR  AP,  WHERE  A  IS THE MATRIX OF LINEAR CONSTRAINTS.
C      AND, IF  NULLR  IS FALSE,
C  (4) THE  (NCOLR)-TH DIAGONAL ELEMENT OF THE CHOLESKY FACTOR OF THE
C      PROJECTED HESSIAN.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  ORIGINAL VERSION OF DECEMBER 1982. REV. MAY 1983.
C  *********************************************************************
C
      INTEGER            I, J
      DOUBLE PRECISION   ONE
      DOUBLE PRECISION   DOT, V2NORM
      DATA               ONE /1.0D+0/
C
      CALL COPYVC( NCOLR,          QTG, NCOLR, 1, P, NCOLR, 1 )
      CALL SSCALE( NCOLR, (- ONE), P  , NCOLR, 1 )
      IF (NULLR) GO TO 200
      RDLAST   = RT(NCOLR,NCOLR)
C  ***
C  CORRECTION INSERTED BY MHW, 22 OCT 1985.
C  THIS ENSURES A NON-ZERO SEARCH DIRECTION.
C  ***
      IF (NCOLR .LT. NCOLZ .AND. ZTGNRM .LE. DINKY)  P(NCOLR) = RDLAST
C
C  ---------------------------------------------------------------------
C  SOLVE THE SYSTEM   R(T)R (PZ) = - Z(T)G(FREE).
C  ---------------------------------------------------------------------
      IF (UNITPG) GO TO 120
C
C  PERFORM THE FORWARD SUBSTITUTION  R(T)V = - Z(T)G(FREE).
C
      CALL RSOLVE( 2, NROWRT, NCOLR, RT, P )
      GO TO 130
C
C  THE PROJECTED GRADIENT IS A MULTIPLE OF THE UNIT VECTOR, THE FORWARD
C  SUBSTITUTION MAY BE AVOIDED.
C
  120 IF (ZTGNRM .LE. DINKY) P(NCOLR) = - ONE
      IF (ZTGNRM .GT. DINKY) P(NCOLR) =   P(NCOLR) / RDLAST
C
C  PERFORM THE BACKWARD SUBSTITUTION   R(PZ) = P.
C
  130 CALL COPYVC( NCOLR, P, NCOLR, 1, V, NCOLR, 1 )
      CALL RSOLVE( 1, NROWRT, NCOLR, RT, P )
C
C  ---------------------------------------------------------------------
C  THE VECTOR  (PZ)  HAS BEEN COMPUTED.
C  ---------------------------------------------------------------------
C  COMPUTE THE DIRECTIONAL DERIVATIVE  G(T)P = (GZ)(T)(PZ).
C
  200 GTP    = DOT( NCOLR, QTG, NCOLR, 1, P, NCOLR, 1 )
C
C  ---------------------------------------------------------------------
C  COMPUTE  P = Z * PZ.
C  ---------------------------------------------------------------------
C  NACTIV  AND  KACTIV  ARE NOT USED IN  ZYPROD.  N  AND  KFREE  SERVE
C  AS ARGUMENTS FOR  NACTIV  AND  KACTIV.
C
      CALL ZYPROD( 1, N, N, NCOLR, NFREE, NQ, UNITQ,
     *             KFREE, KFREE, P, ZY, WORK )
C
      PNORM = V2NORM( NFREE, WORK, NFREE, 1 )
      IF (MSG .GE. 80) WRITE (NOUT, 1100) (P(J), J = 1, N)
C
C  ---------------------------------------------------------------------
C  COMPUTE  AP.
C  ---------------------------------------------------------------------
      IF (NCLIN .EQ. 0) GO TO 900
      CALL ZEROVC( NCLIN, AP, NCLIN, 1 )
      DO 410 J = 1, N
         IF (ISTATE(J) .GT. 0) GO TO 410
         CALL AXPY( NCLIN, P(J), A(1,J), NCLIN, 1, AP, NCLIN, 1 )
  410 CONTINUE
      IF (MSG .GE. 80  .AND.  NCLIN .GT. 0)
     *   WRITE (NOUT, 1000) (AP(I), I = 1, NCLIN)
C
  900 RETURN
C
 1000 FORMAT(/ 20H //FINDP //  AP ...  / (1P5E15.5))
 1100 FORMAT(/ 20H //FINDP //   P ...  / (1P5E15.5))
C
C  END OF FINDP
      END
