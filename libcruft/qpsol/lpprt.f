      SUBROUTINE LPPRT ( LP, NROWA, NROWRT, NCOLRT,
     *                   N, NCLIN, NCLIN0, NCTOTL,
     *                   NFREE, ISDEL, NACTIV, NCOLZ, ITER, JADD, JDEL,
     *                   ALFA, CONDT, NUMINF, SUMINF, OBJLP,
     *                   ISTATE, KFREE,
     *                   A, RT, X, WRK1, WRK2 )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            NROWA, NROWRT, NCOLRT, N, NCLIN, NCLIN0,
     *                   NCTOTL, NFREE, ISDEL, NACTIV, NCOLZ, ITER,
     *                   JADD, JDEL, NUMINF
      INTEGER            ISTATE(NCTOTL), KFREE(N)
      DOUBLE PRECISION   ALFA, CONDT, SUMINF, OBJLP
      DOUBLE PRECISION   A(NROWA,N), RT(NROWRT,NCOLRT), X(N)
      DOUBLE PRECISION   WRK1(N), WRK2(NCLIN0)
      LOGICAL            LP
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C
C  LPPRT  PRINTS VARIOUS LEVELS OF OUTPUT FOR  LPCORE.
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
C        GE  20    MULTIPLIERS (PRINTED OUTSIDE LPPRT).
C                  THE ARRAY  AX.
C
C        GE  30    DIAGONALS OF  T.
C
C        GE  80    DEBUG OUTPUT.
C
C        EQ  99    A,  BL,  BU,  CVEC,  X  (CALLED FROM LPDUMP).
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF DECEMBER 1981.  REV. NOV. 1982.
C  *********************************************************************
C
      INTEGER            INCT, J, K, LADD, LDEL, LENT, LROWA, L1, L2
      INTEGER            LSTATE(5)
      DOUBLE PRECISION   DOT
      DATA               LSTATE(1), LSTATE(2) /      1H ,       1HL/
      DATA               LSTATE(3), LSTATE(4) /      1HU,       1HE/
      DATA               LSTATE(5)            /      1HT           /
C
      IF (MSG .LT. 5) GO TO 900
C
      LDEL   = 0
      LADD   = 0
      IF (JDEL .GT. 0) LDEL = ISDEL
      IF (JADD .GT. 0) LADD = ISTATE(JADD)
      LDEL   = LSTATE(LDEL + 1)
      LADD   = LSTATE(LADD + 1)
      IF (MSG .GE. 15) GO TO 100
C
C  ---------------------------------------------------------------------
C  PRINT HEADING (POSSIBLY) AND TERSE LINE.
C  ---------------------------------------------------------------------
      IF (.NOT. LP  .AND.  ITER .EQ. 0) WRITE (NOUT, 1100)
      IF (      LP  .AND.  ITER .EQ. 0) WRITE (NOUT, 1110)
      IF (.NOT. LP) WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD,
     *   ALFA, CONDT, NUMINF, SUMINF
      IF (      LP) WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD,
     *   ALFA, CONDT, NUMINF, SUMINF, OBJLP
      GO TO 900
C
C  ---------------------------------------------------------------------
C  PRINT TERSE LINE,  X,  ISTATE  AND  KFREE.
C  ---------------------------------------------------------------------
  100 WRITE (NOUT, 1000) ITER
      IF (.NOT. LP) WRITE (NOUT, 1100)
      IF (      LP) WRITE (NOUT, 1110)
      IF (.NOT. LP) WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD,
     *   ALFA, CONDT, NUMINF, SUMINF
      IF (      LP) WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD,
     *   ALFA, CONDT, NUMINF, SUMINF, OBJLP
      WRITE (NOUT, 1300) (X(J)     , J=1,N)
      WRITE (NOUT, 1600) (ISTATE(J), J=1,N)
      L1 = N + 1
      L2 = N + NCLIN
      IF (L1     .LE. L2) WRITE (NOUT, 1610) (ISTATE(J), J=L1,L2)
      IF (NFREE  .GT.  0) WRITE (NOUT, 1700) (KFREE(K), K=1,NFREE)
C
C  ---------------------------------------------------------------------
C  COMPUTE AND PRINT  AX.  USE  WORK = AP  TO AVOID SIDE EFFECTS.
C  ---------------------------------------------------------------------
      IF (MSG .LT. 20) GO TO 900
      IF (NCLIN .EQ. 0) GO TO 300
      LROWA  = NROWA*(N - 1) + 1
      DO 250 K = 1, NCLIN
         WRK2(K) = DOT( N, A(K,1), LROWA, NROWA, X, N, 1 )
  250 CONTINUE
      WRITE (NOUT, 2000) (WRK2(K), K=1,NCLIN)
C
C  ---------------------------------------------------------------------
C  PRINT THE DIAGONALS OF  T.
C  ---------------------------------------------------------------------
  300 IF (MSG .LT. 30) GO TO 900
      LENT   = NROWRT*(NACTIV - 1) + 1
      INCT   = NROWRT - 1
      IF (NACTIV .NE. 0) CALL COPYVC( NACTIV, RT(NACTIV,NCOLZ+1),
     *                                LENT, INCT, WRK1, NACTIV, 1 )
      IF (NACTIV .NE. 0) WRITE (NOUT, 3000) (WRK1(J), J=1,NACTIV)
C
  900 RETURN
C
 1000 FORMAT(/// 18H ================= / 13H LP ITERATION, I5
     *         / 18H ================= )
 1100 FORMAT(// 5H  ITN, 12H JDEL  JADD , 6X, 4HSTEP, 10H    COND T,
     *   7H NUMINF, 8X, 7H SUMINF)
 1110 FORMAT(// 5H  ITN, 12H JDEL  JADD , 6X, 4HSTEP, 10H    COND T,
     *   7H NUMINF, 8X, 7H SUMINF, 9X, 6H LPOBJ)
 1200 FORMAT(I5, I5, A1, I5, A1, 1P2E10.2, I7, 1P2E15.6)
 1300 FORMAT(/ 13H LP VARIABLES                            / (1P5E15.6))
 1600 FORMAT(/ 37H STATUS OF THE LP BOUND   CONSTRAINTS    / (1X, 10I4))
 1610 FORMAT(/ 37H STATUS OF THE LP GENERAL CONSTRAINTS    / (1X, 10I4))
 1700 FORMAT(/ 26H LIST OF FREE LP VARIABLES               / (1X, 10I4))
 2000 FORMAT(/ 40H VALUES OF LP GENERAL LINEAR CONSTRAINTS / (1P5E15.6))
 3000 FORMAT(/ 40H DIAGONALS OF LP WORKING SET FACTOR  T   / (1P5E15.6))
C
C  END OF LPPRT
      END
