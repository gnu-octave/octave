      SUBROUTINE RIBESL(X,ALPHA,NB,IZE,B,NCALC)
C-------------------------------------------------------------------
C
C  This routine calculates Bessel functions I SUB(N+ALPHA) (X)
C  for non-negative argument X, and non-negative order N+ALPHA,
C  with or without exponential scaling.
C
C
C Explanation of variables in the calling sequence
C
C X     - Working precision non-negative real argument for which
C         I's or exponentially scaled I's (I*EXP(-X))
C         are to be calculated.  If I's are to be calculated,
C         X must be less than EXPARG (see below).
C ALPHA - Working precision fractional part of order for which
C         I's or exponentially scaled I's (I*EXP(-X)) are
C         to be calculated.  0 .LE. ALPHA .LT. 1.0.
C NB    - Integer number of functions to be calculated, NB .GT. 0.
C         The first function calculated is of order ALPHA, and the 
C         last is of order (NB - 1 + ALPHA).
C IZE   - Integer type.  IZE = 1 if unscaled I's are to calculated,
C         and 2 if exponentially scaled I's are to be calculated.
C B     - Working precision output vector of length NB.  If the routine
C         terminates normally (NCALC=NB), the vector B contains the 
C         functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
C         corresponding exponentially scaled functions.
C NCALC - Integer output variable indicating possible errors.
C         Before using the vector B, the user should check that
C         NCALC=NB, i.e., all orders have been calculated to
C         the desired accuracy.  See error returns below.
C 
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C   beta   = Radix for the floating-point system
C   minexp = Smallest representable power of beta
C   maxexp = Smallest power of beta that overflows
C   it     = Number of bits in the mantissa of a working precision
C            variable
C   NSIG   = Decimal significance desired.  Should be set to
C            INT(LOG10(2)*it+1).  Setting NSIG lower will result
C            in decreased accuracy while setting NSIG higher will
C            increase CPU time without increasing accuracy.  The
C            truncation error is limited to a relative error of
C            T=.5*10**(-NSIG).
C   ENTEN  = 10.0 ** K, where K is the largest integer such that
C            ENTEN is machine-representable in working precision
C   ENSIG  = 10.0 ** NSIG
C   RTNSIG = 10.0 ** (-K) for the smallest integer K such that
C            K .GE. NSIG/4
C   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
C   XLARGE = Upper limit on the magnitude of X when IZE=2.  Bear
C            in mind that if ABS(X)=N, then at least N iterations
C            of the backward recursion will be executed.  The value
C            of 10.0 ** 4 is used on every machine.
C   EXPARG = Largest working precision argument that the library
C            EXP routine can handle and upper limit on the
C            magnitude of X when IZE=1; approximately 
C            LOG(beta**maxexp)
C
C
C     Approximate values for some important machines are:
C
C                        beta       minexp      maxexp       it
C
C  CRAY-1        (S.P.)    2        -8193        8191        48
C  Cyber 180/855
C    under NOS   (S.P.)    2         -975        1070        48
C  IEEE (IBM/XT,
C    SUN, etc.)  (S.P.)    2         -126         128        24
C  IEEE (IBM/XT,
C    SUN, etc.)  (D.P.)    2        -1022        1024        53
C  IBM 3033      (D.P.)   16          -65          63        14
C  VAX           (S.P.)    2         -128         127        24
C  VAX D-Format  (D.P.)    2         -128         127        56
C  VAX G-Format  (D.P.)    2        -1024        1023        53
C
C
C                        NSIG       ENTEN       ENSIG      RTNSIG
C
C CRAY-1        (S.P.)    15       1.0E+2465   1.0E+15     1.0E-4
C Cyber 180/855
C   under NOS   (S.P.)    15       1.0E+322    1.0E+15     1.0E-4
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)    16       1.0D+308    1.0D+16     1.0D-4
C IBM 3033      (D.P.)     5       1.0D+75     1.0D+5      1.0D-2
C VAX           (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
C VAX D-Format  (D.P.)    17       1.0D+38     1.0D+17     1.0D-5
C VAX G-Format  (D.P.)    16       1.0D+307    1.0D+16     1.0D-4
C
C
C                         ENMTEN      XLARGE   EXPARG 
C
C CRAY-1        (S.P.)   1.84E-2466   1.0E+4    5677 
C Cyber 180/855
C   under NOS   (S.P.)   1.25E-293    1.0E+4     741
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)   4.70E-38     1.0E+4      88  
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)   8.90D-308    1.0D+4     709
C IBM 3033      (D.P.)   2.16D-78     1.0D+4     174
C VAX           (S.P.)   1.17E-38     1.0E+4      88
C VAX D-Format  (D.P.)   1.17D-38     1.0D+4      88
C VAX G-Format  (D.P.)   2.22D-308    1.0D+4     709
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  In case of an error,  NCALC .NE. NB, and not all I's are
C  calculated to the desired accuracy.
C
C  NCALC .LT. 0:  An argument is out of range. For example,
C     NB .LE. 0, IZE is not 1 or 2, or IZE=1 and ABS(X) .GE. EXPARG.
C     In this case, the B-vector is not calculated, and NCALC is
C     set to MIN0(NB,0)-1 so that NCALC .NE. NB.
C
C  NB .GT. NCALC .GT. 0: Not all requested function values could
C     be calculated accurately.  This usually occurs because NB is
C     much larger than ABS(X).  In this case, B(N) is calculated
C     to the desired accuracy for N .LE. NCALC, but precision
C     is lost for NCALC .LT. N .LE. NB.  If B(N) does not vanish
C     for N .GT. NCALC (because it is too small to be represented),
C     and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
C     significant figures of B(N) can be trusted.
C
C
C Intrinsic functions required are:
C
C     DBLE, EXP, DGAMMA, GAMMA, INT, MAX, MIN, REAL, SQRT
C
C
C Acknowledgement
C
C  This program is based on a program written by David J.
C  Sookne (2) that computes values of the Bessel functions J or
C  I of real argument and integer order.  Modifications include
C  the restriction of the computation to the I Bessel function
C  of non-negative real argument, the extension of the computation
C  to arbitrary positive order, the inclusion of optional
C  exponential scaling, and the elimination of most underflow.
C  An earlier version was published in (3).
C
C References: "A Note on Backward Recurrence Algorithms," Olver,
C              F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
C              pp 941-947.
C
C             "Bessel Functions of Real Argument and Integer Order,"
C              Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
C              125-132.
C
C             "ALGORITHM 597, Sequence of Modified Bessel Functions
C              of the First Kind," Cody, W. J., Trans. Math. Soft.,
C              1983, pp. 242-245.
C
C  Latest modification: May 30, 1989
C
C  Modified by: W. J. Cody and L. Stoltz
C               Applied Mathematics Division
C               Argonne National Laboratory
C               Argonne, IL  60439
C
C-------------------------------------------------------------------
      INTEGER IZE,K,L,MAGX,N,NB,NBMX,NCALC,NEND,NSIG,NSTART
      DOUBLE PRECISION DGAMMA,             
     1 ALPHA,B,CONST,CONV,EM,EMPAL,EMP2AL,EN,ENMTEN,ENSIG,
     2 ENTEN,EXPARG,FUNC,HALF,HALFX,ONE,P,PLAST,POLD,PSAVE,PSAVEL,
     3 RTNSIG,SUM,TEMPA,TEMPB,TEMPC,TEST,TOVER,TWO,X,XLARGE,ZERO
      DIMENSION B(NB)
C-------------------------------------------------------------------
C  Mathematical constants
C-------------------------------------------------------------------
      DATA ONE,TWO,ZERO,HALF,CONST/1.0D0,2.0D0,0.0D0,0.5D0,1.585D0/
C-------------------------------------------------------------------
C  Machine-dependent parameters
C-------------------------------------------------------------------
      DATA NSIG,XLARGE,EXPARG /16,1.0D4,709.0D0/
      DATA ENTEN,ENSIG,RTNSIG/1.0D308,1.0D16,1.0D-4/
      DATA ENMTEN/8.9D-308/
C-------------------------------------------------------------------
C  Statement functions for conversion
C-------------------------------------------------------------------
      CONV(N) = DBLE(N)
      FUNC(X) = DGAMMA(X)
C-------------------------------------------------------------------
C Check for X, NB, OR IZE out of range.
C-------------------------------------------------------------------
      IF ((NB.GT.0) .AND. (X .GE. ZERO) .AND.
     1    (ALPHA .GE. ZERO) .AND. (ALPHA .LT. ONE) .AND.
     2    (((IZE .EQ. 1) .AND. (X .LE. EXPARG)) .OR.
     3     ((IZE .EQ. 2) .AND. (X .LE. XLARGE)))) THEN
C-------------------------------------------------------------------
C Use 2-term ascending series for small X
C-------------------------------------------------------------------
            NCALC = NB
            MAGX = INT(X)
            IF (X .GE. RTNSIG) THEN
C-------------------------------------------------------------------
C Initialize the forward sweep, the P-sequence of Olver
C-------------------------------------------------------------------
                  NBMX = NB-MAGX
                  N = MAGX+1
                  EN = CONV(N+N) + (ALPHA+ALPHA)
                  PLAST = ONE
                  P = EN / X
C-------------------------------------------------------------------
C Calculate general significance test
C-------------------------------------------------------------------
                  TEST = ENSIG + ENSIG
                  IF (2*MAGX .GT. 5*NSIG) THEN
                        TEST = SQRT(TEST*P)
                     ELSE
                        TEST = TEST / CONST**MAGX
                  END IF
                  IF (NBMX .GE. 3) THEN
C-------------------------------------------------------------------
C Calculate P-sequence until N = NB-1.  Check for possible overflow.
C-------------------------------------------------------------------
                     TOVER = ENTEN / ENSIG
                     NSTART = MAGX+2
                     NEND = NB - 1
                     DO 100 K = NSTART, NEND
                        N = K
                        EN = EN + TWO
                        POLD = PLAST
                        PLAST = P
                        P = EN * PLAST/X + POLD
                        IF (P .GT. TOVER) THEN
C-------------------------------------------------------------------
C To avoid overflow, divide P-sequence by TOVER.  Calculate
C P-sequence until ABS(P) .GT. 1.
C-------------------------------------------------------------------
                           TOVER = ENTEN
                           P = P / TOVER
                           PLAST = PLAST / TOVER
                           PSAVE = P
                           PSAVEL = PLAST
                           NSTART = N + 1
   60                      N = N + 1
                              EN = EN + TWO
                              POLD = PLAST
                              PLAST = P
                              P = EN * PLAST/X + POLD
                           IF (P .LE. ONE) GO TO 60
                           TEMPB = EN / X
C-------------------------------------------------------------------
C Calculate backward test, and find NCALC, the highest N
C such that the test is passed.
C-------------------------------------------------------------------
                           TEST = POLD*PLAST / ENSIG
                           TEST = TEST*(HALF-HALF/(TEMPB*TEMPB))
                           P = PLAST * TOVER
                           N = N - 1
                           EN = EN - TWO
                           NEND = MIN0(NB,N)
                           DO 80 L = NSTART, NEND
                              NCALC = L
                              POLD = PSAVEL
                              PSAVEL = PSAVE
                              PSAVE = EN * PSAVEL/X + POLD
                              IF (PSAVE*PSAVEL .GT. TEST) GO TO 90
   80                      CONTINUE
                           NCALC = NEND + 1
   90                      NCALC = NCALC - 1
                           GO TO 120
                        END IF
  100                CONTINUE
                     N = NEND
                     EN = CONV(N+N) + (ALPHA+ALPHA)
C-------------------------------------------------------------------
C Calculate special significance test for NBMX .GT. 2.
C-------------------------------------------------------------------
                     TEST = MAX(TEST,SQRT(PLAST*ENSIG)*SQRT(P+P))
                  END IF
C-------------------------------------------------------------------
C Calculate P-sequence until significance test passed.
C-------------------------------------------------------------------
  110             N = N + 1
                     EN = EN + TWO
                     POLD = PLAST
                     PLAST = P
                     P = EN * PLAST/X + POLD
                  IF (P .LT. TEST) GO TO 110
C-------------------------------------------------------------------
C Initialize the backward recursion and the normalization sum.
C-------------------------------------------------------------------
  120             N = N + 1
                  EN = EN + TWO
                  TEMPB = ZERO
                  TEMPA = ONE / P
                  EM = CONV(N) - ONE
                  EMPAL = EM + ALPHA
                  EMP2AL = (EM - ONE) + (ALPHA + ALPHA)
                  SUM = TEMPA * EMPAL * EMP2AL / EM
                  NEND = N - NB
                  IF (NEND .LT. 0) THEN
C-------------------------------------------------------------------
C N .LT. NB, so store B(N) and set higher orders to zero.
C-------------------------------------------------------------------
                        B(N) = TEMPA
                        NEND = -NEND
                        DO 130 L = 1, NEND
  130                      B(N+L) = ZERO
                     ELSE
                        IF (NEND .GT. 0) THEN
C-------------------------------------------------------------------
C Recur backward via difference equation, calculating (but
C not storing) B(N), until N = NB.
C-------------------------------------------------------------------
                           DO 140 L = 1, NEND
                              N = N - 1
                              EN = EN - TWO
                              TEMPC = TEMPB
                              TEMPB = TEMPA
                              TEMPA = (EN*TEMPB) / X + TEMPC
                              EM = EM - ONE
                              EMP2AL = EMP2AL - ONE
                              IF (N .EQ. 1) GO TO 150
                              IF (N .EQ. 2) EMP2AL = ONE
                              EMPAL = EMPAL - ONE
                              SUM = (SUM + TEMPA*EMPAL) * EMP2AL / EM
  140                      CONTINUE
                        END IF
C-------------------------------------------------------------------
C Store B(NB)
C-------------------------------------------------------------------
  150                   B(N) = TEMPA
                        IF (NB .LE. 1) THEN
                           SUM = (SUM + SUM) + TEMPA
                           GO TO 230
                        END IF
C-------------------------------------------------------------------
C Calculate and Store B(NB-1)
C-------------------------------------------------------------------
                        N = N - 1
                        EN = EN - TWO
                        B(N)  = (EN*TEMPA) / X + TEMPB
                        IF (N .EQ. 1) GO TO 220
                        EM = EM - ONE
                        EMP2AL = EMP2AL - ONE
                        IF (N .EQ. 2) EMP2AL = ONE
                        EMPAL = EMPAL - ONE
                        SUM = (SUM + B(N)*EMPAL) * EMP2AL / EM
                  END IF
                  NEND = N - 2
                  IF (NEND .GT. 0) THEN
C-------------------------------------------------------------------
C Calculate via difference equation and store B(N), until N = 2.
C-------------------------------------------------------------------
                     DO 200 L = 1, NEND
                        N = N - 1
                        EN = EN - TWO
                        B(N) = (EN*B(N+1)) / X +B(N+2)
                        EM = EM - ONE
                        EMP2AL = EMP2AL - ONE
                        IF (N .EQ. 2) EMP2AL = ONE
                        EMPAL = EMPAL - ONE
                        SUM = (SUM + B(N)*EMPAL) * EMP2AL / EM
  200                CONTINUE
                  END IF
C-------------------------------------------------------------------
C Calculate B(1)
C-------------------------------------------------------------------
                  B(1) = TWO*EMPAL*B(2) / X + B(3)
  220             SUM = (SUM + SUM) + B(1)
C-------------------------------------------------------------------
C Normalize.  Divide all B(N) by sum.
C-------------------------------------------------------------------
  230             IF (ALPHA .NE. ZERO)
     1               SUM = SUM * FUNC(ONE+ALPHA) * (X*HALF)**(-ALPHA)
                  IF (IZE .EQ. 1) SUM = SUM * EXP(-X)
                  TEMPA = ENMTEN
                  IF (SUM .GT. ONE) TEMPA = TEMPA * SUM
                  DO 260 N = 1, NB
                     IF (B(N) .LT. TEMPA) B(N) = ZERO
                     B(N) = B(N) / SUM
  260             CONTINUE
                  RETURN
C-------------------------------------------------------------------
C Two-term ascending series for small X.
C-------------------------------------------------------------------
               ELSE
                  TEMPA = ONE
                  EMPAL = ONE + ALPHA
                  HALFX = ZERO
                  IF (X .GT. ENMTEN) HALFX = HALF * X
                  IF (ALPHA .NE. ZERO) TEMPA = HALFX**ALPHA /FUNC(EMPAL)
                  IF (IZE .EQ. 2) TEMPA = TEMPA * EXP(-X)
                  TEMPB = ZERO
                  IF ((X+ONE) .GT. ONE) TEMPB = HALFX * HALFX
                  B(1) = TEMPA + TEMPA*TEMPB / EMPAL
                  IF ((X .NE. ZERO) .AND. (B(1) .EQ. ZERO)) NCALC = 0
                  IF (NB .GT. 1) THEN
                     IF (X .EQ. ZERO) THEN
                           DO 310 N = 2, NB
                              B(N) = ZERO
  310                      CONTINUE
                        ELSE
C-------------------------------------------------------------------
C Calculate higher-order functions.
C-------------------------------------------------------------------
                           TEMPC = HALFX
                           TOVER = (ENMTEN + ENMTEN) / X
                           IF (TEMPB .NE. ZERO) TOVER = ENMTEN / TEMPB
                           DO 340 N = 2, NB
                              TEMPA = TEMPA / EMPAL
                              EMPAL = EMPAL + ONE
                              TEMPA = TEMPA * TEMPC
                              IF (TEMPA .LE. TOVER*EMPAL) TEMPA = ZERO
                              B(N) = TEMPA + TEMPA*TEMPB / EMPAL
                              IF ((B(N) .EQ. ZERO) .AND. (NCALC .GT. N))
     1                             NCALC = N-1
  340                      CONTINUE
                     END IF
                  END IF
            END IF
         ELSE
            NCALC = MIN0(NB,0)-1
      END IF
      RETURN
C---------- Last line of RIBESL ----------
      END
