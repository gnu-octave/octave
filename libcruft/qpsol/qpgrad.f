      SUBROUTINE QPGRAD( MODE, UNITQ, QPHESS, N, NACTIV,
     *                   NCTOTL, NFREE, NHESS, NQ, NROWH, NCOLH, JADD,
     *                   KACTIV, KFREE, ALFA, OBJQP, GFIXED, GTP,
     *                   CVEC, HESS, P, QTG, SCALE, X, ZY, WRK1, WRK2 )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            MODE, N, NACTIV, NCTOTL, NFREE, NHESS, NQ,
     *                   NROWH, NCOLH, JADD
      INTEGER            KACTIV(N), KFREE(N)
      DOUBLE PRECISION   ALFA, OBJQP, GFIXED, GTP
      DOUBLE PRECISION   CVEC(N), HESS(NROWH,NCOLH), P(N), QTG(N),
     *                   SCALE(NCTOTL), X(N), ZY(NQ,NQ)
      DOUBLE PRECISION   WRK1(N), WRK2(N)
      LOGICAL            UNITQ
      EXTERNAL           QPHESS
C
      LOGICAL            SCLDQP
      COMMON    /SOL2LP/ SCLDQP
C
C  *********************************************************************
C  QPGRAD  COMPUTES OR UPDATES...
C  (1)  OBJQP, THE VALUE OF THE QUADRATIC OBJECTIVE FUNCTION, AND
C  (2)  THE VECTORS  Q(FREE)(T)G(FREE)  AND  G(FIXED),  WHERE  Q(FREE)
C       IS THE ORTHOGONAL FACTOR OF THE  A(FREE)  AND  A  IS THE MATRIX
C       OF CONSTRAINTS IN THE WORKING SET.  THESE VECTORS ARE STORED IN
C       ELEMENTS  1,2,...,NFREE  AND  NFREE+1,...,N,  RESPECTIVELY,  OF
C       THE ARRAY  QTG.
C  (3)  THE COMPONENT OF THE GRADIENT VECTOR CORRESPONDING TO A BOUND
C       CONSTRAINT THAT HAS JUST BEEN ADDED TO THE WORKING SET.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  ORIGINAL VERSION OF OCTOBER 1982.
C  *********************************************************************
C
      INTEGER            JTHCOL, NCOLZ
      DOUBLE PRECISION   DELTAF
      DOUBLE PRECISION   DOT
      DOUBLE PRECISION   ZERO  , HALF  , ONE
      DATA               ZERO  , HALF  , ONE
     *                  /0.0D+0, 0.5D+0, 1.0D+0/
C
      JTHCOL = 0
      GO TO ( 100, 200, 300 ), MODE
C
C  ---------------------------------------------------------------------
C  MODE = 1  ---  COMPUTE THE OBJECTIVE FUNCTION AND GRADIENT FROM
C                 SCRATCH.  ALLOW FOR A DIAGONAL SCALING OF  X.
C  ---------------------------------------------------------------------
  100 CALL COPYVC( N, X    , N, 1, WRK1, N, 1 )
C
      IF (SCLDQP) CALL DSCALE( N, SCALE, N, 1, WRK1, N, 1 )
C
      CALL QPHESS( N, NROWH, NCOLH, JTHCOL, HESS, WRK1, QTG )
      OBJQP  = HALF*DOT( N, QTG , N, 1, WRK1, N, 1 )
     *            + DOT( N, CVEC, N, 1, WRK1, N, 1 )
      CALL AXPY  ( N, ONE, CVEC , N, 1, QTG, N, 1 )
C
      IF (SCLDQP) CALL DSCALE( N, SCALE, N, 1, QTG, N, 1 )
C
C  COMPUTE  Q(FREE)(T)(G(FREE)  AND  G(FIXED).  THE ELEMENTS OF  G(FREE)
C  ARE NOT STORED.
C
      CALL ZYPROD( 6, N, NACTIV, NCOLZ, NFREE, NQ, UNITQ,
     *             KACTIV, KFREE, QTG, ZY, WRK1 )
C
      GO TO 900
C
C  ---------------------------------------------------------------------
C  MODE = 2  ---  IF THE QP OBJECTIVE FUNCTION IS REDUCED BY A POSITIVE
C                 STEP  ALFA,  OR  ALFA  IS NEGATIVE, UPDATE  OBJF,
C                 Q(FREE)(T)G(FREE)  AND  G(FIXED)  CORRESPONDING TO
C                 THE CHANGE,  X = X + ALFA P.
C  ---------------------------------------------------------------------
  200 CALL QPHESS( N, NROWH, NCOLH, JTHCOL, HESS, P, WRK1 )
C
      IF (SCLDQP) CALL DSCALE( N, SCALE, N, 1, WRK1, N, 1 )
C
C  UPDATE  OBJQP.
C
      DELTAF = ALFA*GTP + HALF*ALFA*ALFA*DOT( N, P, N, 1, WRK1, N, 1 )
      IF (DELTAF .GT. ZERO  .AND.  ALFA .GT. ZERO) GO TO 999
      OBJQP  = OBJQP + DELTAF
C
C  UPDATE THE ARRAY  QTG.  USE THE ARRAY  P  AS TEMPORARY WORK SPACE.
C
      CALL ZYPROD( 6, N, NACTIV, NCOLZ, NFREE, NQ, UNITQ,
     *             KACTIV, KFREE, WRK1, ZY, WRK2 )
C
      CALL AXPY  ( N, ALFA, WRK1, N, 1, QTG, N, 1 )
      GO TO 900
C
C  ---------------------------------------------------------------------
C  MODE = 3  ---  COMPUTE THE  JADD-TH COMPONENT OF THE GRADIENT VECTOR.
C  ---------------------------------------------------------------------
  300 JTHCOL = JADD
      CALL ZEROVC( N, WRK2, N, 1 )
      WRK2(JTHCOL) = ONE
      CALL QPHESS( N, NROWH, NCOLH, JTHCOL, HESS, WRK2, WRK1 )
C
      IF (      SCLDQP) CALL DSCALE( N, SCALE, N, 1, WRK1, N, 1 )
      IF (      SCLDQP)
     *GFIXED = SCALE(JADD)*(DOT( N, WRK1, N, 1, X, N, 1 ) + CVEC(JADD))
      IF (.NOT. SCLDQP)
     *GFIXED =              DOT( N, WRK1, N, 1, X, N, 1 ) + CVEC(JADD)
C
  900 NHESS  = NHESS + 1
      RETURN
C
C  THE STEP  ALFA  DOES NOT DECREASE THE OBJECTIVE FUNCTION.
C
  999 MODE = - 1
      RETURN
C
C  END OF QPGRAD
      END
