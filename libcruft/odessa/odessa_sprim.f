      SUBROUTINE ODESSA_SPRIME (NEQ, Y, YH, NYH, NROW, NCOL, WM, IWM,
     1  EWT, SAVF, FTEM, DFDP, PAR, F, JAC, DF, PJAC, PDF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NEQ(*), Y(*), YH(NROW,NCOL,*), WM(*), IWM(*),
     1  EWT(*), SAVF(*), FTEM(*), DFDP(NROW,*), PAR(*)
      EXTERNAL F, JAC, DF, PJAC, PDF
      PARAMETER (ONE=1.0D0,ZERO=0.0D0)
      COMMON /ODE001/ ROWND, ROWNS(173),
     1  RDUM1(37),EL0, H, RDUM2(6),
     2  IOWND1(14), IOWNS(4),
     3  IDUM1(3), IERPJ, IDUM2(6),
     4  MITER, IDUM3(4), N, IDUM4(5)
      COMMON /ODE002/ RDUM3(3),
     1  IOWND2(3), IDUM5, NSV, IDUM6, NSPE, IDUM7, IERSP, JOPT, IDUM8
C-----------------------------------------------------------------------
C ODESSA_SPRIME IS CALLED BY ODESSA TO INITIALIZE THE YH ARRAY. IT IS ALSO
C CALLED BY ODESSA_STODE TO REEVALUATE FIRST ORDER DERIVATIVES WHEN KFLAG
C .LE. -3. ODESSA_SPRIME COMPUTES THE FIRST DERIVATIVES OF THE SENSITIVITY
C COEFFICIENTS WITH RESPECT TO THE INDEPENDENT VARIABLE T...
C
C        ODESSA_SPRIME = D(DY/DP)/DT = JAC*DY/DP + DF/DP
C                   WHERE JAC = JACOBIAN MATRIX
C                       DY/DP = SENSITIVITY MATRIX
C                       DF/DP = INHOMOGENEITY MATRIX
C THIS ROUTINE USES THE COMMON VARIABLES EL0, H, IERPJ, MITER, N,
C NSV, NSPE, IERSP, JOPT
C-----------------------------------------------------------------------
C CALL ODESSA_PREPJ WITH JOPT = 1.
C IF MITER = 2 OR 5, EL0 IS TEMPORARILY SET TO -1.0 AND H IS
C TEMPORARILY SET TO 1.0D0.
C-----------------------------------------------------------------------
      NSPE = NSPE + 1
      JOPT = 1
      IF (MITER .EQ. 1 .OR. MITER .EQ. 4) GO TO 10
      HTEMP = H
      ETEMP = EL0
      H = ONE
      EL0 = -ONE
 10   CALL PJAC (NEQ, Y, YH, NYH, WM, IWM, EWT, SAVF, FTEM,
     1   PAR, F, JAC, JOPT)
      IF (IERPJ .NE. 0) GO TO 300
      JOPT = 0
      IF (MITER .EQ. 1 .OR. MITER .EQ. 4) GO TO 20
      H = HTEMP
      EL0 = ETEMP
C-----------------------------------------------------------------------
C CALL ODESSA_PREPDF AND LOAD DFDP(*,JPAR).
C-----------------------------------------------------------------------
 20   DO 30 J = 2,NSV
        JPAR = J - 1
        CALL PDF (NEQ, Y, WM, SAVF, FTEM, DFDP(1,JPAR), PAR,
     1     F, DF, JPAR)
 30   CONTINUE
C-----------------------------------------------------------------------
C COMPUTE JAC*DY/DP AND STORE RESULTS IN YH(*,*,2).
C-----------------------------------------------------------------------
      GO TO (40,40,310,100,100) MITER
C THE JACOBIAN IS FULL.------------------------------------------------
C FOR EACH ROW OF THE JACOBIAN..
 40   DO 70 IROW = 1,N
C AND EACH COLUMN OF THE SENSITIVITY MATRIX..
        DO 60 J = 2,NSV
          SUM = ZERO
C TAKE THE VECTOR DOT PRODUCT..
          DO 50 I = 1,N
            IPD = IROW + N*(I-1) + 2
            SUM = SUM + WM(IPD)*YH(I,J,1)
 50       CONTINUE
          YH(IROW,J,2) = SUM
 60     CONTINUE
 70   CONTINUE
      GO TO 200
C THE JACOBIAN IS BANDED.-----------------------------------------------
 100  ML = IWM(1)
      MU = IWM(2)
      ICOUNT = 1
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      NMU = N - MU
      ML1 = ML + 1
C FOR EACH ROW OF THE JACOBIAN..
      DO 160 IROW = 1,N
        IF (IROW .GT. ML1) GO TO 110
        IPD = MBAND + IROW + 1
        IYH = 1
        LBAND = MU + IROW
        GO TO 120
 110    ICOUNT = ICOUNT + 1
        IPD = ICOUNT*MEBAND + 2
        IYH = IYH + 1
        LBAND = LBAND - 1
        IF (IROW .LE. NMU) LBAND = MBAND
C AND EACH COLUMN OF THE SENSITIVITY MATRIX..
 120    DO 150 J = 2,NSV
          SUM = ZERO
          I1 = IPD
          I2 = IYH
C TAKE THE VECTOR DOT PRODUCT.
          DO 140 I = 1,LBAND
            SUM = SUM + WM(I1)*YH(I2,J,1)
            I1 = I1 + MEBAND - 1
            I2 = I2 + 1
 140      CONTINUE
          YH(IROW,J,2) = SUM
 150    CONTINUE
 160  CONTINUE
C-----------------------------------------------------------------------
C ADD THE INHOMOGENEITY TERM, I.E., ADD DFDP(*,JPAR) TO YH(*,JPAR+1,2).
C-----------------------------------------------------------------------
 200  DO 220 J = 2,NSV
        JPAR = J - 1
        DO 210 I = 1,N
          YH(I,J,2) = YH(I,J,2) + DFDP(I,JPAR)
 210    CONTINUE
 220  CONTINUE
      RETURN
C-----------------------------------------------------------------------
C ERROR RETURNS.
C-----------------------------------------------------------------------
 300  IERSP = -1
      RETURN
 310  IERSP = -2
      RETURN
C------------------------END OF SUBROUTINE ODESSA_SPRIME-----------------------
      END
