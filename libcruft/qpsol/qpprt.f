      SUBROUTINE QPPRT ( ORTHOG, ISDEL, ITER, JADD, JDEL, NACTIV,
     *                   NCOLR, NCOLZ, NFREE, N, NCLIN, NCLIN0, NCTOTL,
     *                   NROWA, NROWRT, NCOLRT, NHESS,
     *                   ISTATE, KFREE,
     *                   ALFA, CONDH, CONDT, OBJ, GFNORM, ZTGNRM, EMAX,
     *                   A, RT, X, WRK1, WRK2 )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            ORTHOG
      INTEGER            ISDEL, ITER, JADD, JDEL, NACTIV, NCOLR, NCOLZ,
     *                   NFREE, N, NCLIN, NCLIN0, NCTOTL, NROWA,
     *                   NROWRT, NCOLRT, NHESS
      INTEGER            ISTATE(NCTOTL), KFREE(N)
      DOUBLE PRECISION   ALFA, CONDH, CONDT, OBJ, GFNORM, ZTGNRM, EMAX
      DOUBLE PRECISION   A(NROWA,N), RT(NROWRT,NCOLRT), X(N)
      DOUBLE PRECISION   WRK1(N), WRK2(NCLIN0)
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C  QPPRT  PRINTS VARIOUS LEVELS OF OUTPUT FOR  QPCORE.
C
C           MSG    CUMULATIVE RESULT
C           ---    -----------------
C
C        LE   0    NO OUTPUT.
C
C        EQ   1    NOTHING NOW (BUT FULL OUTPUT LATER).
C
C        EQ   5    ONE TERSE LINE OF OUTPUT.
C
C        GE  10    SAME AS 5 (BUT FULL OUTPUT LATER).
C
C        GE  15    NOTHING MORE IF  ITER .LT. ISTART.
C                  OTHERWISE,  X,  ISTATE  AND  KFREE.
C
C        GE  20    MULTIPLIERS (PRINTED OUTSIDE QPPRT).
C                  THE ARRAY  AX.
C
C        GE  30    DIAGONALS OF  T  AND  R.
C
C        GE  80    DEBUG OUTPUT.
C
C        EQ  99    CVEC  AND  HESS  (CALLED FROM QPDUMP).
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF APRIL 1982.  REV. OCT. 1982.
C  *********************************************************************
C
      INTEGER            INCT, J, K, LADD, LDEL, LENT, LROWA, L1, L2
      INTEGER            LSTATE(6)
      DOUBLE PRECISION   DOT
      DATA               LSTATE(1), LSTATE(2) /      1H ,       1HL /
      DATA               LSTATE(3), LSTATE(4) /      1HU,       1HE /
      DATA               LSTATE(5)            /      1HT            /
      DATA               LSTATE(6)            /      1HV            /
C
      IF (MSG .LT. 5) GO TO 900
C
      LDEL   = 0
      LADD   = 0
      IF (JDEL .GT. 0) LDEL = ISDEL
      IF (JDEL .LT. 0) LDEL = 5
      IF (JDEL .LT. 0) JDEL = - JDEL
      IF (JADD .GT. 0) LADD = ISTATE(JADD)
      LDEL   = LSTATE(LDEL + 1)
      LADD   = LSTATE(LADD + 1)
      IF (MSG .GE. 15) GO TO 100
C
C  ---------------------------------------------------------------------
C  PRINT HEADING (POSSIBLY) AND TERSE LINE.
C  ---------------------------------------------------------------------
      IF (ITER .GT. 0  .OR.  JDEL .GT. 0) GO TO 50
      IF (      ORTHOG) WRITE (NOUT, 1100)
      IF (.NOT. ORTHOG) WRITE (NOUT, 1110)
   50 WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD, ALFA, NHESS,
     *                   OBJ, NCOLZ, GFNORM, ZTGNRM, CONDT, CONDH, EMAX
      GO TO 900
C
C  ---------------------------------------------------------------------
C  PRINT TERSE LINE,  X,  ISTATE,  KFREE.
C  ---------------------------------------------------------------------
  100 WRITE (NOUT, 1000) ITER
      IF (      ORTHOG) WRITE (NOUT, 1100)
      IF (.NOT. ORTHOG) WRITE (NOUT, 1110)
      WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD, ALFA, NHESS,
     *                   OBJ, NCOLZ, GFNORM, ZTGNRM, CONDT, CONDH, EMAX
      WRITE (NOUT, 1300) X
      WRITE (NOUT, 1600) (ISTATE(J), J=1,N)
      L1 = N + 1
      L2 = N + NCLIN
      IF (L1     .LE. L2) WRITE (NOUT, 1610) (ISTATE(J), J=L1,L2)
      IF (NFREE  .GT.  0) WRITE (NOUT, 1700) (KFREE(K), K=1,NFREE)
C
C  ---------------------------------------------------------------------
C  COMPUTE AND PRINT  AX.  USE  WORK  TO AVOID SIDE EFFECTS.
C  ---------------------------------------------------------------------
      IF (MSG  .LT. 20) GO TO 900
      IF (NCLIN .EQ. 0) GO TO 300
      LROWA  = NROWA*(N - 1) + 1
      DO 250 K = 1, NCLIN
         WRK2(K) = DOT( N, A(K,1), LROWA, NROWA, X, N, 1 )
  250 CONTINUE
      WRITE (NOUT, 2000) (WRK2(K), K=1,NCLIN)
C
C  ---------------------------------------------------------------------
C  PRINT ALL THE DIAGONALS OF  T  AND  R.
C  ---------------------------------------------------------------------
  300 IF (MSG .LT. 30) GO TO 900
      LENT   = NROWRT*(NACTIV - 1) + 1
      INCT   = NROWRT - 1
      IF (NACTIV .GT. 0) CALL COPYVC( NACTIV, RT(NACTIV,NCOLZ+1),
     *                                LENT, INCT, WRK1, NACTIV, 1 )
      IF (NACTIV .GT. 0) WRITE (NOUT, 3000) (WRK1(J), J=1,NACTIV)
      IF (NCOLZ  .GT. 0) WRITE (NOUT, 3100) (RT(J,J), J=1,NCOLZ)
C
  900 RETURN
C
 1000 FORMAT(/// 18H ================= / 13H QP ITERATION, I5
     *         / 18H ================= )
 1100 FORMAT(// 5H  ITN, 12H JDEL  JADD , 10H      STEP,
     *   6H NHESS, 12H   OBJECTIVE, 6H NCOLZ, 11H NORM GFREE,
     *   10H  NORM ZTG, 9H   COND T, 9H COND ZHZ, 10H  HESS MOD)
 1110 FORMAT(// 5H  ITN, 12H JDEL  JADD , 10H      STEP,
     *   6H NHESS, 12H   OBJECTIVE, 6H NCOLZ, 11H   NORM QTG,
     *   10H  NORM ZTG, 9H   COND T, 9H COND ZHZ, 10H  HESS MOD)
 1200 FORMAT(I5, I5, A1, I5, A1, 1PE10.2, I6, 1PE12.4, I6,
     *   1PE11.2, 1PE10.2, 1P2E9.1, 1PE10.2)
 1300 FORMAT(/ 13H QP VARIABLES                            / (1P5E15.6))
 1600 FORMAT(/ 35H STATUS OF THE QP BOUND CONSTRAINTS      / (1X, 10I4))
 1610 FORMAT(/ 37H STATUS OF THE QP GENERAL CONSTRAINTS    / (1X, 10I4))
 1700 FORMAT(/ 26H LIST OF FREE QP VARIABLES               / (1X, 10I4))
 2000 FORMAT(/ 40H VALUES OF QP GENERAL LINEAR CONSTRAINTS / (1P5E15.6))
 3000 FORMAT(/ 40H DIAGONALS OF QP WORKING SET FACTOR  T   / (1P5E15.6))
 3100 FORMAT(/ 40H DIAGONALS OF QP PRJ. HESSIAN FACTOR  R  / (1P5E15.6))
C
C  END OF QPPRT
      END
