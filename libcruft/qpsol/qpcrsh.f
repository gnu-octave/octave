      SUBROUTINE QPCRSH( UNITQ, QPHESS, N, NCOLR, NCOLZ, NCTOTL, NFREE,
     *                   NHESS, NQ, NROWH, NCOLH, NROWRT, NCOLRT,
     *                   KFREE, HSIZE,
     *                   HESS, RT, SCALE, ZY, HZ, WRK )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            N, NCOLR, NCOLZ, NCTOTL, NFREE, NHESS, NQ,
     *                   NROWH, NCOLH, NROWRT, NCOLRT
      INTEGER            KFREE(N)
      DOUBLE PRECISION   HSIZE
      DOUBLE PRECISION   HESS(NROWH,NCOLH), RT(NROWRT,NCOLRT),
     *                   SCALE(NCTOTL), ZY(NQ,NQ), HZ(N)
      DOUBLE PRECISION   WRK(N)
      LOGICAL            UNITQ
      EXTERNAL           QPHESS
C
      INTEGER            NOUT, MSG, ISTART
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
C
      LOGICAL            SCLDQP
      COMMON    /SOL2LP/ SCLDQP
C
C  *********************************************************************
C  QPCRSH  COMPUTES THE CHOLESKY FACTOR  R  OF THE PROJECTED HESSIAN
C  Z(T) H Z,  GIVEN  Z  AND ITS DIMENSIONS  NFREE BY NCOLZ.
C  IF THE PROJECTED HESSIAN IS INDEFINITE, A SMALLER CHOLESKY
C  FACTORIZATION  R1(T) R1 = Z1(T) H Z1  IS RETURNED, WHERE  Z1  IS
C  COMPOSED OF  NCOLR  COLUMNS OF  Z.  COLUMN INTERCHANGES ARE
C  USED TO MAXIMIZE  NCOLR.  THESE ARE APPLIED TO  Z.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  ORIGINAL VERSION OF JANUARY 1983.
C  *********************************************************************
C
      INTEGER            I, J, JTHCOL, J1, K, KMAX, KSAVE, LEN, NUM
      DOUBLE PRECISION   D, DMAX, DMIN, EPSMCH, T
      DOUBLE PRECISION   DSQRT
      DOUBLE PRECISION   DABS, DMAX1
      DOUBLE PRECISION   ZERO  , ONE
      DATA               ZERO  , ONE
     *                  /0.0D+0, 1.0D+0/
C
      EPSMCH = WMACH(3)
C
      NCOLR  = 0
      IF (NCOLZ .EQ. 0) GO TO 900
C
C  ---------------------------------------------------------------------
C  COMPUTE  Z(T) H Z  AND STORE THE UPPER-TRIANGULAR SYMMETRIC PART
C  IN THE FIRST  NCOLZ  COLUMNS OF  RT.
C  ---------------------------------------------------------------------
      DO 200 K = 1, NCOLZ
         CALL ZEROVC( N, WRK, N, 1 )
         IF (UNITQ) GO TO 130
C
C        EXPAND THE COLUMN OF  Z  INTO AN  N-VECTOR.
C
         DO 120 I = 1, NFREE
            J       = KFREE(I)
            WRK(J) = ZY(I,K)
  120    CONTINUE
         IF (SCLDQP) CALL DSCALE( N, SCALE, N, 1, WRK, N, 1 )
         JTHCOL = 0
         GO TO 150
C
C        ONLY BOUNDS ARE IN THE WORKING SET.  THE  K-TH COLUMN OF  Z  IS
C        JUST A COLUMN OF THE IDENTITY MATRIX.
C
  130    JTHCOL = KFREE(K)
         WRK(JTHCOL) = ONE
C
C        SET  RT(*,K)  =  TOP OF   H * (COLUMN OF  Z).
C
  150    CALL QPHESS( N, NROWH, NCOLH, JTHCOL, HESS, WRK, HZ )
         NHESS  = NHESS + 1
C
         IF (UNITQ  .AND.  SCLDQP)
     *   CALL SSCALE( N, SCALE(JTHCOL), HZ, N, 1 )
         IF (SCLDQP)
     *   CALL DSCALE( N, SCALE, N, 1,   HZ, N, 1 )
C
         CALL ZYPROD( 4, N, NFREE, NCOLZ, NFREE, NQ, UNITQ,
     *                KFREE, KFREE, HZ, ZY, WRK )
C
         CALL COPYVC( NCOLZ, HZ, NCOLZ, 1, RT(1,K), NCOLZ, 1 )
C
C        UPDATE AN ESTIMATE OF THE SIZE OF THE PROJECTED HESSIAN.
C
         HSIZE  = DMAX1( HSIZE, DABS(RT(K,K)) )
  200 CONTINUE
C
C  ---------------------------------------------------------------------
C  FORM THE CHOLESKY FACTORIZATION  R(T) R  =  Z(T) H Z  AS FAR AS
C  POSSIBLE, USING SYMMETRIC ROW AND COLUMN INTERCHANGES.
C  ---------------------------------------------------------------------
      DMIN   = EPSMCH * HSIZE
C
      DO 400 J = 1, NCOLZ
C
C        FIND THE MAXIMUM REMAINING DIAGONAL.
C
         KMAX   = J
         DMAX   = RT(J,J)
         DO 310 K = J, NCOLZ
            D     = RT(K,K)
            IF (DMAX .GE. D) GO TO 310
            DMAX  = D
            KMAX  = K
  310    CONTINUE
C
C        SEE IF THE DIAGONAL IS BIG ENOUGH.
C
         IF (DMAX .LE. DMIN) GO TO 500
         NCOLR  = J
C
C        PERMUTE THE COLUMNS OF  Z.
C
         IF (KMAX .EQ. J) GO TO 350
         IF (UNITQ) GO TO 315
         CALL COPYVC( NFREE, ZY(1,KMAX), NFREE, 1, WRK      , NFREE, 1 )
         CALL COPYVC( NFREE, ZY(1,J)   , NFREE, 1, ZY(1,KMAX),NFREE, 1 )
         CALL COPYVC( NFREE, WRK       , NFREE, 1, ZY(1,J)   ,NFREE, 1 )
         GO TO 312
C
C        Z  IS NOT STORED EXPLICITLY.
C
  315    KSAVE       = KFREE(KMAX)
         KFREE(KMAX) = KFREE(J)
         KFREE(J)    = KSAVE
C
C        INTERCHANGE ROWS AND COLUMNS OF THE PROJECTED HESSIAN.
C
  312    DO 320 I = 1, J
            T          = RT(I,KMAX)
            RT(I,KMAX) = RT(I,J)
            RT(I,J)    = T
  320    CONTINUE
C
         DO 330 K = J, KMAX
            T          = RT(K,KMAX)
            RT(K,KMAX) = RT(J,K)
            RT(J,K)    = T
  330    CONTINUE
C
         DO 340 K = KMAX, NCOLZ
            T          = RT(KMAX,K)
            RT(KMAX,K) = RT(J,K)
            RT(J,K)    = T
  340    CONTINUE
C
         RT(KMAX,KMAX) = RT(J,J)
C
C        SET THE DIAGONAL ELEMENT OF  R.
C
  350    D       = DSQRT(DMAX)
         RT(J,J) = D
         IF (J .EQ. NCOLZ) GO TO 400
C
C        SET THE ABOVE-DIAGONAL ELEMENTS OF THE K-TH ROW OF  R,
C        AND UPDATE THE ELEMENTS OF ALL REMAINING ROWS.
C
         J1     = J + 1
         DO 360 K = J1, NCOLZ
            T       = RT(J,K)/D
            RT(J,K) = T
C
C           R(I,K)  =  R(I,K)  - T * R(J,I),   I = J1, K.
C
            NUM    = K - J
            LEN    = NROWRT*(NUM - 1) + 1
            IF (T .NE. ZERO)
     *      CALL AXPY( NUM, (- T), RT(J,J1), LEN, NROWRT,
     *                             RT(J1,K), NUM, 1 )
  360    CONTINUE
  400 CONTINUE
C
  500 IF (NCOLR .EQ. NCOLZ) GO TO 900
      IF (MSG .GE. 80) WRITE (NOUT, 1000) NCOLR, NCOLZ
C
  900 RETURN
C
 1000 FORMAT(/ 42H //QPCRSH//  INDEFINITE PROJECTED HESSIAN.
     *       / 20H //QPCRSH//  NCOLR =, I5, 6X, 7HNCOLZ =, I5)
C
C  END OF QPCRSH
      END
