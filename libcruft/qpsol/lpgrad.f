      SUBROUTINE LPGRAD( LP, N, NCTOTL, NROWA,
     *                   BIGBND, FEAMIN, NUMINF, SUMINF, ISTATE,
     *                   A, BL, BU, CVEC, FEATOL, GRAD, X )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            N, NCTOTL, NROWA, NUMINF
      INTEGER            ISTATE(NCTOTL)
      DOUBLE PRECISION   BIGBND, FEAMIN, SUMINF
      DOUBLE PRECISION   A(NROWA,N), BL(NCTOTL), BU(NCTOTL), CVEC(N),
     *                   FEATOL(NCTOTL), GRAD(N), X(N)
      LOGICAL            LP
C
C  *********************************************************************
C  IF NUMINF .GT. 0,  LPGRAD  FINDS THE NUMBER AND WEIGHTED SUM OF
C  INFEASIBILITIES FOR THE BOUNDS AND LINEAR CONSTRAINTS. AN
C  APPROPRIATE GRADIENT VECTOR IS RETURNED IN  GRAD.
C  IF NUMINF = 0,  AND IF AN LP PROBLEM IS BEING SOLVED,  GRAD  WILL BE
C  LOADED WITH THE TRUE LINEAR OBJECTIVE.
C
C  POSITIVE VALUES OF  ISTATE(J)  WILL NOT BE ALTERED.  THESE MEAN
C  THE FOLLOWING...
C
C            1          2         3
C        A*X = BL   A*X = BU   BL = BU
C
C  OTHER VALUES OF  ISTATE(J)  WILL BE RESET AS FOLLOWS...
C        A*X LT BL   A*X GT BU   A*X FREE
C           - 2         - 1         0
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF SEPTEMBER 1981.  REV. OCT. 1982. JAN. 1983.
C  *********************************************************************
C
      INTEGER            J, K, LROWA
      DOUBLE PRECISION   ATX, FEASJ, S, WEIGHT, ZERO
      DOUBLE PRECISION   DOT
      DOUBLE PRECISION   DABS
      LOGICAL            NOLOW, NOUPP
      DATA               ZERO /0.0D+0/
C
      LROWA  = NROWA*(N - 1) + 1
      IF (NUMINF .EQ. 0) GO TO 500
      NUMINF = 0
      SUMINF = ZERO
      CALL ZEROVC( N, GRAD, N, 1 )
C
      DO 200 J = 1, NCTOTL
C
C        DO NOTHING IF THE VARIABLE OR CONSTRAINT IS AT A BOUND.
C
         IF (ISTATE(J) .GT. 0) GO TO 200
         FEASJ  = FEATOL(J)
         NOLOW  = BL(J) .LE. (- BIGBND)
         NOUPP  = BU(J) .GE.    BIGBND
         K      = J - N
         IF (J .LE. N) ATX = X(J)
         IF (J .GT. N) ATX = DOT( N, A(K,1), LROWA, NROWA, X, N, 1 )
         ISTATE(J) = 0
C
C        SEE IF THE LOWER BOUND IS VIOLATED.
C
         IF (NOLOW) GO TO 150
         S = BL(J) - ATX
         IF (S .LE. FEASJ) GO TO 150
         ISTATE(J) = - 2
         WEIGHT    = - FEAMIN/FEASJ
         GO TO 160
C
C        SEE IF THE UPPER BOUND IS VIOLATED.
C
  150    IF (NOUPP) GO TO 200
         S = ATX - BU(J)
         IF (S .LE. FEASJ) GO TO 200
         ISTATE(J) = - 1
         WEIGHT    =   FEAMIN/FEASJ
C
C        ADD THE INFEASIBILITY.
C
  160    NUMINF = NUMINF + 1
         SUMINF = SUMINF + DABS( WEIGHT ) * S
         IF (J .LE. N) GRAD(J) = WEIGHT
         IF (J .GT. N)
     *   CALL AXPY  ( N, WEIGHT, A(K,1), LROWA, NROWA, GRAD, N, 1 )
  200 CONTINUE
C
C  IF FEASIBLE, INSTALL TRUE OBJECTIVE.
C
  500 IF (LP  .AND.  NUMINF .EQ. 0)
     *CALL COPYVC( N, CVEC, N, 1, GRAD, N, 1 )
      RETURN
C
C  END OF LPGRAD
      END
