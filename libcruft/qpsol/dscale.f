      SUBROUTINE DSCALE( N, D, LEND, INCD, X, LENX, INCX )
C
      INTEGER            N, LEND, INCD, LENX, INCX
      DOUBLE PRECISION   D(LEND), X(LENX)
C
C  DSCALE  PERFORMS DIAGONAL SCALING ON THE VECTOR  X,
C  REPLACING  X(I)  BY  D(I)*X(I)  FOR  N  VALUES OF  I.
C
      INTEGER            I, ID, IX
C
      IF (N .LT. 1) RETURN
      ID = 1
      IX = 1
      DO 100 I = 1, N
         X(IX) = D(ID)*X(IX)
         ID    = ID + INCD
         IX    = IX + INCX
  100 CONTINUE
      RETURN
C
C  END OF DSCALE
      END
