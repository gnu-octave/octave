      SUBROUTINE ELM   ( ORTHOG, N, X, LENX, INCX, Y, LENY, INCY,
     *                   CS, SN )
C
      LOGICAL            ORTHOG
      INTEGER            N, LENX, INCX, LENY, INCY
      DOUBLE PRECISION   CS, SN
      DOUBLE PRECISION   X(LENX), Y(LENY)
C
C  *********************************************************************
C  IF  ORTHOG  IS TRUE,  ELM  APPLIES A PLANE ROTATION.  OTHERWISE,
C  ELM  COMPUTES THE TRANSFORMATION  (X Y)*E  AND RETURNS THE RESULT
C  IN  (X Y),  WHERE THE 2 BY 2 MATRIX  E  IS DEFINED BY  CS  AND  SN
C  AS FOLLOWS...
C
C     E  =  ( 1  SN )  IF  CS .GT. ZERO,    E  =  (     1 )  OTHERWISE.
C           (     1 )                             ( 1  SN )
C
C  VERSION 1, APRIL 5 1983.
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  *********************************************************************
C
      INTEGER            I, IX, IY
      DOUBLE PRECISION   XI, YI, ZERO
C
      IF (ORTHOG) GO TO 800
      ZERO   = 0.0
      IF (CS .LE. ZERO) GO TO 200
      IF (SN .EQ. ZERO) RETURN
      CALL AXPY  ( N, SN, X, LENX, INCX, Y, LENY, INCY )
      RETURN
C
  200 IX     = 1
      IY     = 1
      IF (SN .EQ. ZERO) GO TO 300
C
      DO 210 I = 1, N
         XI    = X(IX)
         YI    = Y(IY)
         X(IX) = YI
         Y(IY) = XI + YI*SN
         IX    = IX + INCX
         IY    = IY + INCY
  210 CONTINUE
      RETURN
C
C  TREAT AN INTERCHANGE SPECIALLY.
C
  300 DO 310 I = 1, N
         XI    = X(IX)
         X(IX) = Y(IY)
         Y(IY) = XI
         IX    = IX + INCX
         IY    = IY + INCY
  310 CONTINUE
      RETURN
C
C
  800 CALL ROT3  ( N, X, LENX, INCX, Y, LENY, INCY, CS, SN )
      RETURN
C
C  END OF ELM
      END
