      SUBROUTINE ZYPROD( MODE, N, NACTIV, NCOLZ, NFREE, NQ, UNITQ,
     *                   KACTIV, KFREE, V, ZY, WRK )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            MODE, N, NACTIV, NCOLZ, NFREE, NQ
      INTEGER            KACTIV(N), KFREE(N)
      DOUBLE PRECISION   V(N), ZY(NQ,NQ), WRK(N)
C
C  *********************************************************************
C  ZYPROD  TRANSFORMS THE VECTOR  V  IN VARIOUS WAYS USING THE
C  MATRIX  Q = ( Z  Y )  DEFINED BY THE INPUT PARAMETERS.
C
C     MODE               RESULT
C     ----               ------
C
C       1                V = Z*V
C       2                V = Y*V
C       3                V = Q*V       (NOT YET USED)
C
C  ON INPUT,  V  IS ASSUMED TO BE ORDERED AS  ( V(FREE)  V(FIXED) ).
C  ON OUTPUT, V  IS A FULL N-VECTOR.
C
C
C       4                V = Z(T)*V
C       5                V = Y(T)*V
C       6                V = Q(T)*V
C
C  ON INPUT,  V  IS A FULL N-VECTOR.
C  ON OUTPUT, V  IS ORDERED AS  ( V(FREE)  V(FIXED) ).
C
C       7                V = Y(T)*V
C       8                V = Q(T)*V
C
C  ON INPUT,  V  IS A FULL N-VECTOR.
C  ON OUTPUT, V  IS AS IN MODES 5 AND 6 EXCEPT THAT V(FIXED) IS NOT SET.
C
C  BEWARE THAT  NCOLZ  WILL SOMETIMES BE  NCOLR.
C  ALSO, MODES  1, 4, 7 AND 8  DO NOT INVOLVE  V(FIXED).
C  NACTIV  AND  THE ARRAY  KACTIV  ARE NOT USED FOR THOSE CASES.
C  ORIGINAL VERSION  APRIL 1983. MODES 7 AND 8 ADDED  APRIL 1984.
C  *********************************************************************
C
      INTEGER            J, J1, J2, K, KA, KW, L, LENV, NFIXED
      DOUBLE PRECISION   ZERO
      DOUBLE PRECISION   DOT
      DATA               ZERO /0.0D+0/
C
      NFIXED = N - NFREE
      J1     = 1
      J2     = NFREE
      IF (MODE .EQ. 1  .OR.  MODE .EQ. 4) J2 = NCOLZ
      IF (MODE .EQ. 2  .OR.  MODE .EQ. 5  .OR.  MODE .EQ. 7)
     *J1 = NCOLZ + 1
      LENV   = J2 - J1 + 1
      IF (MODE .GE. 4) GO TO 400
C
C  ---------------------------------------------------------------------
C  MODE = 1, 2  OR  3.
C  ---------------------------------------------------------------------
      IF (NFREE .GT. 0) CALL ZEROVC( NFREE, WRK, NFREE, 1 )
C
C  COPY  V(FIXED)  INTO THE END OF  WRK.
C
      IF (MODE .EQ. 1  .OR.  NFIXED .EQ. 0) GO TO 100
      CALL COPYVC( NFIXED,   V(NFREE+1), NFIXED, 1,
     *                     WRK(NFREE+1), NFIXED, 1 )
C
C  SET  WRK  =  RELEVANT PART OF  ZY * V.
C
  100 IF (LENV .LE. 0) GO TO 200
      IF (UNITQ) CALL COPYVC( LENV, V(J1), LENV, 1, WRK(J1), LENV, 1 )
      IF (UNITQ) GO TO 200
      DO 120 J = J1, J2
         IF (V(J) .NE. ZERO)
     *   CALL AXPY( NFREE, V(J), ZY(1,J), NFREE, 1, WRK, NFREE, 1 )
  120 CONTINUE
C
C  EXPAND  WRK  INTO  V  AS A FULL N-VECTOR.
C
  200 CALL ZEROVC( N, V, N, 1 )
      IF (NFREE .EQ. 0) GO TO 300
      DO 220 K = 1, NFREE
         J    = KFREE(K)
         V(J) = WRK(K)
  220 CONTINUE
C
C  COPY  WRK(FIXED)  INTO THE APPROPRIATE PARTS OF  V.
C
  300 IF (MODE .EQ. 1  .OR.  NFIXED .EQ. 0) GO TO 900
      DO 320 L = 1, NFIXED
         KW      = NFREE  + L
         KA      = NACTIV + L
         J       = KACTIV(KA)
         V(J)    = WRK(KW)
  320 CONTINUE
      GO TO 900
C
C  ---------------------------------------------------------------------
C  MODE = 4, 5, 6, 7  OR  8.
C  ---------------------------------------------------------------------
C  PUT THE FIXED COMPONENTS OF  V  INTO THE END OF  WRK.
C
  400 IF (MODE .EQ. 4  .OR.  MODE .GT. 6  .OR.  NFIXED .EQ. 0) GO TO 500
      DO 420 L = 1, NFIXED
         KW      = NFREE  + L
         KA      = NACTIV + L
         J       = KACTIV(KA)
         WRK(KW) = V(J)
  420 CONTINUE
C
C  PUT THE FREE  COMPONENTS OF  V  INTO THE BEGINNING OF  WRK.
C
  500 IF (NFREE .EQ. 0) GO TO 600
      DO 520 K = 1, NFREE
         J      = KFREE(K)
         WRK(K) = V(J)
  520 CONTINUE
C
C  SET  V  =  RELEVANT PART OF  ZY(T) * WRK.
C
      IF (LENV .LE. 0) GO TO 600
      IF (UNITQ) CALL COPYVC( LENV, WRK(J1), LENV, 1, V(J1), LENV, 1 )
      IF (UNITQ) GO TO 600
      DO 540 J = J1, J2
         V(J) = DOT( NFREE, ZY(1,J), NFREE, 1, WRK, NFREE, 1 )
  540 CONTINUE
C
C  COPY THE FIXED COMPONENTS OF  WRK  INTO THE END OF  V.
C
  600 IF (MODE .EQ. 4  .OR.  MODE .GT. 6  .OR.  NFIXED .EQ. 0) GO TO 900
      CALL COPYVC( NFIXED, WRK(NFREE+1), NFIXED, 1,
     *                       V(NFREE+1), NFIXED, 1 )
C
  900 RETURN
C
C  END OF ZYPROD
      END
