      SUBROUTINE TQADD ( ORTHOG, UNITQ,
     *                   INFORM, K1, K2, NACTIV, NCOLZ, NFREE,
     *                   N, NCTOTL, NQ, NROWA, NROWRT, NCOLRT,
     *                   ISTATE, KACTIV, KFREE,
     *                   CONDMX,
     *                   A, QTG, RT, ZY, WRK1, WRK2 )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            ORTHOG, UNITQ
      INTEGER            INFORM, K1, K2, N, NCTOTL, NQ, NROWA,
     *                   NROWRT, NCOLRT, NACTIV, NCOLZ, NFREE
      INTEGER            ISTATE(NCTOTL), KACTIV(N), KFREE(N)
      DOUBLE PRECISION   CONDMX
      DOUBLE PRECISION   A(NROWA,N), QTG(N), RT(NROWRT,NCOLRT),
     *                   ZY(NQ,NQ), WRK1(N), WRK2(N)
C
      DOUBLE PRECISION   ASIZE, DTMAX, DTMIN
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN
C
C  *********************************************************************
C  TQADD  INCLUDES GENERAL LINEAR CONSTRAINTS  K1  THRU  K2  AS NEW
C  COLUMNS OF THE TQ FACTORIZATION STORED IN  RT, ZY.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF SEPTEMBER 1981.  REV. OCT 1982, JAN 1983.
C  *********************************************************************
C
      INTEGER            I, IADD, IFIX, ISWAP, JADD, K, L
      DOUBLE PRECISION   CSLAST, SNLAST
C
      DO 200 K = K1, K2
         IADD = KACTIV(K)
         JADD = N + IADD
         IF (NACTIV .EQ. NFREE) GO TO 100
C
         CALL ADDCON( .FALSE., .FALSE., ORTHOG, UNITQ, INFORM,
     *                IFIX, IADD, JADD, NACTIV, NCOLZ, NCOLZ, NFREE,
     *                N, NQ, NROWA, NROWRT, NCOLRT, KFREE,
     *                CONDMX, CSLAST, SNLAST,
     *                A, QTG, RT, ZY, WRK1, WRK2 )
C
         IF (INFORM .GT. 0) GO TO 100
         NACTIV = NACTIV + 1
         NCOLZ  = NCOLZ  - 1
         GO TO 200
C
  100    ISTATE(JADD) = 0
         KACTIV(K)    = - KACTIV(K)
  200 CONTINUE
C
      IF (NACTIV .EQ. K2) RETURN
C
C  SOME OF THE CONSTRAINTS WERE CLASSED AS DEPENDENT AND NOT INCLUDED
C  IN THE FACTORIZATION.  MOVE ACCEPTED INDICES TO THE FRONT OF  KACTIV
C  AND SHIFT REJECTED INDICES (WITH NEGATIVE VALUES) TO THE END.
C
      L     = K1 - 1
      DO 300 K = K1, K2
         I         = KACTIV(K)
         IF (I .LT. 0) GO TO 300
         L         = L + 1
         IF (L .EQ. K) GO TO 300
         ISWAP     = KACTIV(L)
         KACTIV(L) = I
         KACTIV(K) = ISWAP
  300 CONTINUE
      RETURN
C
C  END OF TQADD
      END
