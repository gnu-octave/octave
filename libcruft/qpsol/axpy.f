C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     FILE BLAS66 FORTRAN
C
C     AXPY     CONDVC   COPYMX   COPYVC   DOT      DSCALE   ELM
C     ELMGEN   ETAGEN   QUOTNT   REFGEN   ROTGEN   ROT3     SSCALE
C     V2NORM   ZEROVC
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE AXPY  ( N, A, X, LENX, INCX, Y, LENY, INCY )
C
      INTEGER            N, LENX, INCX, LENY, INCY
      DOUBLE PRECISION   A, X(LENX), Y(LENY)
C
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
C
C  AXPY  REPLACES  Y  BY  A*X + Y.
C
      INTEGER            I, IX, IY, NINCX
      DOUBLE PRECISION   FLMIN, ONE, TINY, UNDFLW, U, V, ZERO
      DOUBLE PRECISION   ABSA, ABSX, DU, DV
      DOUBLE PRECISION   DABS
      DATA               ZERO/0.0D+0/, ONE/1.0D+0/
C
      IF (N .LT.    1) RETURN
      IF (A .EQ. ZERO) RETURN
      IX = 1
      IY = 1
      UNDFLW = WMACH(9)
      IF (UNDFLW .GT. ZERO) GO TO 110
C
C  NO UNDERFLOW TEST REQUIRED.
C  DO THE MOST COMMON CASE SPECIALLY (INCX = INCY).
C
      IF (INCX .NE. INCY) GO TO 50
      NINCX  = N * INCX
      DO 40 I = 1, NINCX, INCX
         Y(I) = A * X(I) + Y(I)
   40 CONTINUE
      RETURN
C
   50 DO 100 I = 1, N
         Y(IY) = A * X(IX) + Y(IY)
         IX    = IX + INCX
         IY    = IY + INCY
  100 CONTINUE
      RETURN
C
C  UNDERFLOW TEST REQUIRED.
C
  110 FLMIN  = WMACH(5)
      ABSA   = DABS( A )
      TINY   = FLMIN
      IF (ABSA .LT. ONE) TINY = FLMIN / ABSA
      DO 160 I = 1, N
         ABSX   = DABS( X(IX) )
         IF (ABSX .LT. TINY) GO TO 150
         U      = Y(IY)
         DU     = DABS( U )
         V      = A * X(IX)
         DV     = DABS( V )
         IF (U .GE. ZERO) GO TO 120
         IF (V .LT. ZERO) GO TO 140
         GO TO 130
  120    IF (V .GE. ZERO) GO TO 140
  130    Y(IY) = ZERO
         IF (DU .LE. FLMIN + DV  .AND.  DV .LE. FLMIN + DU) GO TO 150
  140    Y(IY)  = V  + U
  150    IX     = IX + INCX
         IY     = IY + INCY
  160 CONTINUE
         RETURN
C
C  END OF AXPY
      END
