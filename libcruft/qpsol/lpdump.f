      SUBROUTINE LPDUMP( N, NCLIN, NCTOTL, NROWA,
     *                   LCRASH, LP, MINSUM, NAMED, VERTEX,
     *                   ISTATE, A, AX, BL, BU, CVEC, X )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            LP, MINSUM, NAMED, VERTEX
      INTEGER            N, NCLIN, NCTOTL, NROWA, LCRASH
      INTEGER            ISTATE(NCTOTL)
      DOUBLE PRECISION   A(NROWA,N), AX(NROWA), BL(NCTOTL), BU(NCTOTL)
      DOUBLE PRECISION   CVEC(N), X(N)
C
      INTEGER            NOUT, MSG, ISTART
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C  LPDUMP  PRINTS  A, BL, BU, CVEC, X, A*X,
C                  COLD, LP, MINSUM, NAMED, VERTEX, AND POSSIBLY ISTATE.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF APRIL 1982.  REV. OCT. 1982.
C  *********************************************************************
C
      INTEGER            I, J, K, LROWA
      DOUBLE PRECISION   ATX, DOT
C
C  PRINT  WMACH  AND THE LOGICALS.
C
      WRITE (NOUT, 1000)
      DO 10 I = 1, 11
         WRITE (NOUT, 1100) I, WMACH(I)
   10 CONTINUE
      WRITE (NOUT, 1200) LCRASH, LP, MINSUM, NAMED, VERTEX
C
C  PRINT  A  BY ROWS AND COMPUTE  AX = A*X.
C
      IF (NCLIN .EQ. 0) GO TO 200
      LROWA  = NROWA*(N - 1) + 1
      DO 100 K = 1, NCLIN
         WRITE (NOUT, 1500) K
         WRITE (NOUT, 1600) (A(K,J), J=1,N)
         AX(K) = DOT( N, A(K,1), LROWA, NROWA, X, N, 1 )
  100 CONTINUE
C
C  PRINT  BL, BU  AND  X OR AX.
C
  200 WRITE (NOUT, 2000)
      DO 300 J = 1, NCTOTL
         IF (J .GT. N) GO TO 250
         K      = J
         ATX    = X(J)
         GO TO 290
C
  250    K      = J - N
         ATX    = AX(K)
         IF (K .EQ. 1) WRITE (NOUT, 2100)
C
  290    WRITE (NOUT, 2200) K, BL(J), BU(J), ATX
  300 CONTINUE
C
C  PRINT  CVEC, ISTATE.
C
      IF (LP           ) WRITE (NOUT, 3000) (CVEC(I)  , I=1,N)
      IF (LCRASH .GT. 0) WRITE (NOUT, 3100) (ISTATE(J), J=1,NCTOTL)
      RETURN
C
 1000 FORMAT(1H1 / 19H OUTPUT FROM LPDUMP / 19H ******************)
 1100 FORMAT(/ 7H WMACH(, I2, 3H) =, G15.6)
 1200 FORMAT(/ 9H LCRASH =, I3, 4X, 9H LP     =, L3, 4X,
     *         9H MINSUM =, L3, 4X, 9H NAMED  =, L3, 4X,
     *         9H VERTEX =, L3)
 1500 FORMAT(/ 4H ROW, I6, 11H  OF  A ...)
 1600 FORMAT(5G15.6)
 2000 FORMAT(/ 14X, 42HJ      BL(J)          BU(J)           X(J))
 2100 FORMAT(/ 14X, 42HI    BL(N+I)        BU(N+I)         A(I)*X)
 2200 FORMAT(I15, 3G15.6)
 3000 FORMAT(/ 9H CVEC ... / (5G15.6))
 3100 FORMAT(/ 11H ISTATE ... / (10I4))
C
C  END OF LPDUMP
      END
