      SUBROUTINE PRTSOL( NFREE, NROWA, NROWJ,
     *                   N, NCLIN, NCNLN, NCTOTL, BIGBND,
     *                   NAMED, NAMES, LENNAM,
     *                   NACTIV, ISTATE, KACTIV,
     *                   A, BL, BU, C, CLAMDA, RLAMDA, X )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            NAMED
      INTEGER            NFREE, NROWA, NROWJ, N, NCLIN, NCNLN,
     *                   NCTOTL, LENNAM, NACTIV
      INTEGER            ISTATE(NCTOTL), KACTIV(N), NAMES(4,LENNAM)
      DOUBLE PRECISION   BIGBND
      DOUBLE PRECISION   A(NROWA,N), BL(NCTOTL), BU(NCTOTL), C(NROWJ),
     *                   CLAMDA(NCTOTL), RLAMDA(N), X(N)
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C  PRTSOL  EXPANDS THE LAGRANGE MULTIPLIERS INTO  CLAMDA.
C  IF  MSG .GE. 10  OR  MSG .EQ. 1,  PRTSOL  THEN PRINTS  X, A*X, C(X),
C  THEIR BOUNDS,  THE MULTIPLIERS, AND THE RESIDUALS (DISTANCE TO THE
C  NEAREST BOUND).
C  PRTSOL  IS CALLED BY  LPCORE, QPCORE, LCCORE AND NPCORE  JUST BEFORE
C  THEY EXIT.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF MARCH 1982. REV. OCT. 1982.
C  *********************************************************************
C
      INTEGER            I, IP, IS, J, K, L, LROWA, LS,
     *                   NFIXED, NLAM, NPLIN
      INTEGER            ID(9), ID3(3), ID4(4), LSTATE(7)
      DOUBLE PRECISION   B1, B2, RES, RES2, V, WLAM
      DOUBLE PRECISION   DOT
      DOUBLE PRECISION   DABS
      DATA                ID(1), ID(2), ID(3), ID(4), ID(5)
     *                   / 2HVA,  2HRB,  2HL ,  2HLN,  2HCO /
      DATA                ID(6), ID(7), ID(8), ID(9)
     *                   / 2HN ,  2HNL,  2HCO,  2HN /
      DATA                LSTATE(1), LSTATE(2)
     *                   /     2H--,      2H++/
      DATA                LSTATE(3), LSTATE(4)
     *                   /     2HFR,      2HLL /
      DATA                LSTATE(5), LSTATE(6)
     *                   /     2HUL,      2HEQ /
      DATA                LSTATE(7)
     *                   /     2HTB /
C
      NPLIN  = N + NCLIN
      LROWA  = NROWA*(N - 1) + 1
C
C  EXPAND BOUND, LINEAR AND NONLINEAR MULTIPLIERS INTO  CLAMDA.
C
      CALL ZEROVC( NCTOTL, CLAMDA, NCTOTL, 1 )
      NFIXED = N      - NFREE
      NLAM   = NACTIV + NFIXED
      IF (NLAM .EQ. 0) GO TO 180
C
      DO 150 K = 1, NLAM
         J      = KACTIV(K)
         IF (K .LE. NACTIV) J = J + N
         CLAMDA(J) = RLAMDA(K)
  150 CONTINUE
C
  180 IF (MSG .LT. 10  .AND.  MSG .NE. 1) RETURN
C
      WRITE (NOUT, 1100)
      ID3(1) = ID(1)
      ID3(2) = ID(2)
      ID3(3) = ID(3)
C
      DO 500 J = 1, NCTOTL
         B1     = BL(J)
         B2     = BU(J)
         WLAM   = CLAMDA(J)
         IS     = ISTATE(J)
         LS     = LSTATE(IS + 3)
         IF (J .LE. N    ) GO TO 190
         IF (J .LE. NPLIN) GO TO 200
         GO TO 300
C
C
C        SECTION 1 -- THE VARIABLES  X.
C        ------------------------------
  190    K      = J
         V      = X(J)
         GO TO 400
C
C
C        SECTION 2 -- THE LINEAR CONSTRAINTS  A*X.
C        -----------------------------------------
  200    IF (J .NE. N + 1) GO TO 220
         WRITE (NOUT, 1200)
         ID3(1) = ID(4)
         ID3(2) = ID(5)
         ID3(3) = ID(6)
C
  220    K      = J - N
         V      = DOT( N, A(K,1), LROWA, NROWA, X, N, 1 )
         GO TO 400
C
C
C        SECTION 3 -- THE NONLINEAR CONSTRAINTS  C(X).
C        ---------------------------------------------
C
  300    IF (NCNLN .LE. 0) GO TO 500
         IF (J .NE. NPLIN + 1) GO TO 320
         WRITE (NOUT, 1300)
         ID3(1) = ID(7)
         ID3(2) = ID(8)
         ID3(3) = ID(9)
C
  320    K      = J - NPLIN
         V      = C(K)
C
C
C        PRINT A LINE FOR THE J-TH VARIABLE OR CONSTRAINT.
C        -------------------------------------------------
  400    RES    = V - B1
         RES2   = B2 - V
         IF (DABS(RES) .GT. DABS(RES2)) RES = RES2
         IP     = 1
         IF (B1 .LE. ( - BIGBND )) IP = 2
         IF (B2 .GE.     BIGBND  ) IP = IP + 2
         IF (.NOT. NAMED) GO TO 490
C
         DO 450 L = 1, 4
            ID4(L) = NAMES(L,J)
  450    CONTINUE
         IF (IP .EQ. 1) WRITE (NOUT, 2100) ID4,    LS, V,B1,B2,WLAM,RES
         IF (IP .EQ. 2) WRITE (NOUT, 2200) ID4,    LS, V,   B2,WLAM,RES
         IF (IP .EQ. 3) WRITE (NOUT, 2300) ID4,    LS, V,B1,   WLAM,RES
         IF (IP .EQ. 4) WRITE (NOUT, 2400) ID4,    LS, V,      WLAM,RES
         GO TO 500
C
  490    IF (IP .EQ. 1) WRITE (NOUT, 3100) ID3, K, LS, V,B1,B2,WLAM,RES
         IF (IP .EQ. 2) WRITE (NOUT, 3200) ID3, K, LS, V,   B2,WLAM,RES
         IF (IP .EQ. 3) WRITE (NOUT, 3300) ID3, K, LS, V,B1,   WLAM,RES
         IF (IP .EQ. 4) WRITE (NOUT, 3400) ID3, K, LS, V,      WLAM,RES
  500 CONTINUE
C
      RETURN
C
 1100 FORMAT(// 22H VARIABLE        STATE, 5X, 6H VALUE,
     *   6X, 12H LOWER BOUND, 4X, 12H UPPER BOUND,
     *   17H  LAGR MULTIPLIER, 13H     RESIDUAL /)
 1200 FORMAT(// 22H LINEAR CONSTR   STATE, 5X, 6H VALUE,
     *   6X, 12H LOWER BOUND, 4X, 12H UPPER BOUND,
     *   17H  LAGR MULTIPLIER, 13H     RESIDUAL /)
 1300 FORMAT(// 22H NONLNR CONSTR   STATE, 5X, 6H VALUE,
     *   6X, 12H LOWER BOUND, 4X, 12H UPPER BOUND,
     *   17H  LAGR MULTIPLIER, 13H     RESIDUAL /)
 2100 FORMAT(1X, 4A2, 10X, A2, 3G16.7, G16.7, G16.4)
 2200 FORMAT(1X, 4A2, 10X, A2, G16.7, 5X, 5H NONE, 6X, G16.7,
     *   G16.7, G16.4)
 2300 FORMAT(1X, 4A2, 10X, A2, 2G16.7, 5X, 5H NONE, 6X, G16.7, G16.4)
 2400 FORMAT(1X, 4A2, 10X, A2,  G16.7, 5X, 5H NONE, 11X, 5H NONE,
     *   6X, G16.7, G16.4)
 3100 FORMAT(1X, 2A2, A1, I3, 10X, A2, 3G16.7, G16.7, G16.4)
 3200 FORMAT(1X, 2A2, A1, I3, 10X, A2,  G16.7,
     *   5X, 5H NONE, 6X, G16.7, G16.7, G16.4)
 3300 FORMAT(1X, 2A2, A1, I3, 10X, A2, 2G16.7, 5X, 5H NONE, 6X,
     *   G16.7, G16.4)
 3400 FORMAT(1X, 2A2, A1, I3, 10X, A2,  G16.7,
     *   5X, 5H NONE, 11X, 5H NONE, 6X, G16.7, G16.4)
C
C  END OF PRTSOL
      END
