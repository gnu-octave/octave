C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     FILE LPSUBS66 FORTRAN
C
C     LPBGST   LPCORE   LPCRSH   LPDUMP   LPGRAD   LPPRT
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LPBGST( N, NACTIV, NCTOTL, NFREE, JBIGST, KBIGST,
     *                   ISTATE, KACTIV,
     *                   DINKY, FEAMIN, TRULAM, FEATOL, RLAMDA )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            N, NACTIV, NCTOTL, NFREE, JBIGST, KBIGST
      INTEGER            ISTATE(NCTOTL), KACTIV(N)
      DOUBLE PRECISION   DINKY, FEAMIN, TRULAM
      DOUBLE PRECISION   FEATOL(NCTOTL), RLAMDA(N)
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C  FIND THE BIGGEST SCALED MULTIPLIER LARGER THAN UNITY.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  ORIGINAL VERSION DECEMBER 1982.
C  *********************************************************************
C
      INTEGER            IS, J, K, NFIXED, NLAM
      DOUBLE PRECISION   BIGGST, RLAM
      DOUBLE PRECISION   ONE
      DOUBLE PRECISION   DABS
      DATA               ONE/1.0D+0/
C
      JBIGST = 0
      NFIXED = N      - NFREE
      NLAM   = NFIXED + NACTIV
      IF (NLAM .EQ. 0) GO TO 900
C
      BIGGST = ONE + DINKY
      DO 110 K = 1, NLAM
         J      = KACTIV(K)
         IF (K .LE. NACTIV) J = J + N
         IS     = ISTATE(J)
         IF (IS .LT. 1) GO TO 110
         RLAM   = RLAMDA(K)
         IF (IS .EQ. 2) RLAM =     - RLAM
         IF (IS .EQ. 3) RLAM = DABS( RLAM )
         RLAM   = (FEATOL(J)/FEAMIN)*RLAM
C
         IF (BIGGST .GE. RLAM) GO TO 110
         BIGGST = RLAM
         TRULAM = RLAMDA(K)
         JBIGST = J
         KBIGST = K
  110 CONTINUE
      IF (MSG .GE. 80) WRITE (NOUT, 9000) JBIGST, BIGGST
C
  900 RETURN
C
 9000 FORMAT(/ 33H //LPBGST// JBIGST         BIGGST
     *       / 13H //LPBGST//  , I5,         G15.4 )
C
C  END OF LPBGST
      END
