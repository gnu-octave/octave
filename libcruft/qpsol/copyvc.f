      SUBROUTINE COPYVC( N, X, LENX, INCX, Y, LENY, INCY )
C
      INTEGER            N, LENX, INCX, LENY, INCY
      DOUBLE PRECISION   X(LENX), Y(LENY)
C
C  COPY THE FIRST N ELEMENTS OF X INTO Y.
C
      INTEGER            I, IX, IY
C
      IF (N .LT. 1) RETURN
      IF (INCX .EQ. 1  .AND.  INCY .EQ. 1) GO TO 50
      IX = 1
      IY = 1
      DO 10 I = 1, N
         Y(IY) = X(IX)
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
C
   50 DO 60 I = 1, N
         Y(I) = X(I)
   60 CONTINUE
      RETURN
C
C  END OF COPYVC
      END
