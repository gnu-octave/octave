      SUBROUTINE DIF ( NT, ROOT, DIF1, DIF2, DIF3 )
C
      INTEGER           NT
      DOUBLE PRECISION  ROOT(NT), DIF1(NT), DIF2(NT), DIF3(NT)

C
C***********************************************************************
C
C     SUBROUTINE DIF
C
C     THIS ROUTINE IS NOT GIVEN SEPARATELY BY VILLADSEN AND MICHELSEN
C     BUT AS PART OF JCOBI
C
C     DIF COMPUTES THE FIRST THREE DERIVATIVES OF THE NODE POLYNOMIAL
C
C                     N0     (ALPHA,BETA)           N1
C       P  (X)  =  (X)   *  P (X)         *  (1 - X)
C        NT                   N
C
C     AT THE INTERPOLATION POINTS.  EACH OF THE PARAMETERS N0 AND N1
C     MAY BE GIVEN THE VALUE 0 OR 1.  NT = N + N0 + N1
C
C     THE VALUES OF ROOT MUST BE KNOWN BEFORE A CALL TO DIF IS POSSIBLE.
C     THEY MAY BE COMPUTED USING JCOBI.
C
C     PARAMETER LIST:     SEE THE SUBROUTINE JCOBI
C
C     COMMON BLOCKS:      NONE
C
C     REQUIRED ROUTINES:  VILERR
C
C***********************************************************************
C
      INTEGER           I,J,IER
      DOUBLE PRECISION  X,Y
      DOUBLE PRECISION  ZERO,ONE,TWO,THREE
      LOGICAL           LSTOP
C
      PARAMETER ( ZERO = 0.0D+00, ONE   = 1.0D+00,
     +            TWO  = 2.0D+00, THREE = 3.0D+00 )
C
C -- ERROR CHECKING
C
      IF (NT .LT. 1) THEN
        IER   = 7
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
C -- EVALUATE DERIVATIVES OF NODE POLYNOMIAL USING RECURSION FORMULAS
C
      DO 40 I = 1,NT
C
        X       = ROOT(I)
        DIF1(I) = ONE
        DIF2(I) = ZERO
        DIF3(I) = ZERO
C
        DO 30 J = 1,NT
C
          IF (J .NE. I) THEN
            Y       = X - ROOT(J)
            DIF3(I) = Y*DIF3(I) + THREE*DIF2(I)
            DIF2(I) = Y*DIF2(I) + TWO  *DIF1(I)
            DIF1(I) = Y*DIF1(I)
          ELSE
          END IF
C
   30   CONTINUE
   40 CONTINUE
C
      RETURN
      END
