      SUBROUTINE CHKDAT( NERROR, LIWORK, LWORK, LITOTL, LWTOTL,
     *                   NROWA, N, NCLIN, NCNLN, NCTOTL,
     *                   ISTATE, KACTIV,
     *                   LCRASH, NAMED, NAMES, LENNAM,
     *                   BIGBND, A, BL, BU, FEATOL, X )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            NERROR, LIWORK, LWORK, LITOTL, LWTOTL, NROWA,
     *                   N, NCLIN, NCNLN, NCTOTL, LCRASH,
     *                   LENNAM
      INTEGER            ISTATE(NCTOTL), KACTIV(N), NAMES(4,LENNAM)
      DOUBLE PRECISION   BIGBND
      DOUBLE PRECISION   A(NROWA,N), BL(NCTOTL), BU(NCTOTL),
     *                   FEATOL(NCTOTL), X(N)
      LOGICAL            NAMED
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
C  *********************************************************************
C  CHKDAT  CHECKS THE DATA INPUT TO THE VARIOUS OPTIMIZERS.
C
C  THE FOLLOWING QUANTITIES ARE NOT CHECKED...
C     NROWA, N, NCLIN, NCTOTL
C     KACTIV
C     A, X
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF APRIL 1982.  REV. OCT. 1982.
C  *********************************************************************
C
      INTEGER            IS, J, K, L, L1, L2, NPLIN
      INTEGER            ID(9)
      DOUBLE PRECISION   B1, B2, FTOL, ONE, TEST, ZERO
      LOGICAL            OK
      DATA               ID(1), ID(2), ID(3), ID(4), ID(5)
     *                  / 2HVA,  2HRB,  2HL ,  2HLN,  2HCO/
      DATA               ID(6), ID(7), ID(8), ID(9)
     *                  / 2HN ,  2HNL,  2HCO,  2HN /
      DATA               ONE/1.0D+0/, ZERO/0.0D+0/
C
      NERROR = 0
C
C  ---------------------------------------------------------------------
C  CHECK THAT THERE IS ENOUGH WORKSPACE TO SOLVE THE PROBLEM.
C  ---------------------------------------------------------------------
      OK     = LITOTL .LE. LIWORK  .AND.  LWTOTL .LE. LWORK
      IF (OK  .AND.  MSG .LE. 0) GO TO 100
      WRITE (NOUT, 1000) LIWORK, LWORK, LITOTL, LWTOTL
      IF (OK) GO TO 100
      NERROR = NERROR + 1
      WRITE (NOUT, 1010)
C
C  ---------------------------------------------------------------------
C  CHECK THE BOUNDS ON ALL VARIABLES AND CONSTRAINTS.
C  ---------------------------------------------------------------------
  100 DO 200 J = 1, NCTOTL
         B1     = BL(J)
         B2     = BU(J)
         OK     = B1 .LE. B2
         IF (OK) GO TO 200
         NERROR = NERROR + 1
         K      = J
         L1     = 1
         IF (J .GT. N)         K  = J - N
         IF (J .GT. N)         L1 = 4
         IF (J .GT. N + NCLIN) K  = K - NCLIN
         IF (J .GT. N + NCLIN) L1 = 7
         L2     = L1 + 2
         IF (.NOT. NAMED) WRITE (NOUT, 1100) (ID(L), L=L1,L2), K, B1,B2
         IF (      NAMED) WRITE (NOUT, 1200) (NAMES(L,J), L=1,4), B1,B2
  200 CONTINUE
C
C  ---------------------------------------------------------------------
C  CHECK  BIGBND  AND  FEATOL.
C  ---------------------------------------------------------------------
      OK     = BIGBND .GT. ZERO
      IF (OK) GO TO 300
      NERROR = NERROR + 1
      WRITE (NOUT, 1300) BIGBND
C
  300 DO 320 J = 1, NCTOTL
         FTOL   = FEATOL(J)
         TEST   = ONE + FTOL
         OK     = TEST .GT. ONE
         IF (OK) GO TO 320
         WRITE (NOUT, 1400) J, FTOL
  320 CONTINUE
C
C  ---------------------------------------------------------------------
C  IF WARM START, CHECK  ISTATE.
C  ---------------------------------------------------------------------
  400 IF (LCRASH .EQ. 0) GO TO 900
      NPLIN  = N + NCLIN
C
      DO 420 J = 1, NPLIN
         IS     = ISTATE(J)
         OK     = IS .GE. (- 2)   .AND.   IS .LE. 4
         IF (OK) GO TO 420
         NERROR = NERROR + 1
         WRITE (NOUT, 1500) J, IS
  420 CONTINUE
C
  900 RETURN
C
 1000 FORMAT(/ 30H WORKSPACE PROVIDED IS     IW(, I6,
     *   6H),  W(, I6, 2H).
     *       / 30H TO SOLVE PROBLEM WE NEED  IW(, I6,
     *   6H),  W(, I6, 2H).)
 1010 FORMAT(/ 44H XXX  NOT ENOUGH WORKSPACE TO SOLVE PROBLEM.)
 1100 FORMAT(/ 21H XXX  THE BOUNDS ON  , 2A2, A1, I3,
     *   26H  ARE INCONSISTENT.   BL =, G16.7, 7H   BU =, G16.7)
 1200 FORMAT(/ 21H XXX  THE BOUNDS ON  , 4A2,
     *   26H  ARE INCONSISTENT.   BL =, G16.7, 7H   BU =, G16.7)
 1300 FORMAT(/ 32H XXX  BIGBND  IS NOT POSITIVE..., G16.6)
 1400 FORMAT(/ 24H ***  WARNING -- FEATOL(, I4, 16H )  IS LESS THAN,
     *   21H MACHINE PRECISION..., G16.6)
 1500 FORMAT(/ 15H XXX  COMPONENT, I5, 23H  OF  ISTATE  IS OUT OF,
     *   9H RANGE..., I10)
C
C  END OF CHKDAT
      END
