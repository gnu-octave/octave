      SUBROUTINE SSCALE( N, A, X, LENX, INCX )
C
      INTEGER            N, LENX, INCX
      DOUBLE PRECISION   A
      DOUBLE PRECISION   X(LENX)
C
C  SCALE THE VECTOR  X  BY THE SCALAR  A.
C
      INTEGER             I, IX
C
      IF (N .LT. 1) RETURN
      IF (INCX .EQ. 1) GO TO 50
      IX = 1
      DO 10 I = 1, N
         X(IX) = A * X(IX)
         IX    = IX + INCX
   10 CONTINUE
      RETURN
C
   50 DO 60 I = 1, N
         X(I) = A * X(I)
   60 CONTINUE
      RETURN
C
C  END OF SSCALE
      END
