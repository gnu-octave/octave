      SUBROUTINE ZEROVC( N, X, LENX, INCX )
C
      INTEGER            N, LENX, INCX
      DOUBLE PRECISION   X(LENX)
C
C  SET X TO ZERO.
C
      INTEGER            I, IX
      DOUBLE PRECISION   ZERO
C
      DATA               ZERO/0.0D+0/
C
      IF (N    .LT. 1) RETURN
      IF (INCX .EQ. 1) GO TO 50
      IX = 1
      DO 10 I = 1, N
         X(IX) = ZERO
         IX = IX + INCX
   10 CONTINUE
      RETURN
C
   50 DO 60 I = 1, N
         X(I) = ZERO
   60 CONTINUE
      RETURN
C
C  END OF ZEROVC
      END
