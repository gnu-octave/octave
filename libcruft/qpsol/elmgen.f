      SUBROUTINE ELMGEN( ORTHOG, X, Y, CS, SN )
C
      LOGICAL            ORTHOG
      DOUBLE PRECISION   X, Y, CS, SN
C
C  *********************************************************************
C  IF  ORTHOG  IS TRUE,  ELMGEN  GENERATES A PLANE ROTATION.  OTHERWISE,
C  ELMGEN  GENERATES AN ELIMINATION TRANSFORMATION  E  SUCH THAT
C  (X Y)*E  =  (X  0)   OR   (Y  0),  DEPENDING ON THE RELATIVE
C  SIZES OF  X  AND  Y.
C
C  VERSION 1, APRIL 5 1983.
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  *********************************************************************
C
      DOUBLE PRECISION   DABS
      DOUBLE PRECISION   ZERO, ONE
      DATA               ZERO, ONE /0.0D+0, 1.0D+0/
C
      IF (ORTHOG) GO TO 800
      CS     = ONE
      SN     = ZERO
      IF (Y .EQ. ZERO) RETURN
      IF (DABS(X) .LT. DABS(Y)) GO TO 200
      SN     = - Y/X
      GO TO 300
C
  200 CS     =   ZERO
      SN     = - X/Y
      X      =   Y
C
  300 Y      =   ZERO
      RETURN
C
C
  800 CALL ROTGEN( X, Y, CS, SN )
      RETURN
C
C  END OF ELMGEN
      END
