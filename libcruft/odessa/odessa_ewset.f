      SUBROUTINE ODESSA_EWSET (N, ITOL, RTOL, ATOL, YCUR, EWT)
C-----------------------------------------------------------------------
C THIS SUBROUTINE SETS THE ERROR WEIGHT VECTOR EWT ACCORDING TO
C    EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(I),  I = 1,...,N,
C WITH THE SUBSCRIPT ON RTOL AND/OR ATOL POSSIBLY REPLACED BY 1 ABOVE,
C DEPENDING ON THE VALUE OF ITOL.
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RTOL(*), ATOL(*), YCUR(N), EWT(N)
      RTOLI = RTOL(1)
      ATOLI = ATOL(1)
      DO 10 I = 1,N
        IF (ITOL .GE. 3) RTOLI = RTOL(I)
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        EWT(I) = RTOLI*ABS(YCUR(I)) + ATOLI
 10     CONTINUE
      RETURN
C----------------------- END OF SUBROUTINE ODESSA_EWSET -----------------------
      END
