*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSADDS( UNITQ, VERTEX,
     $                   INFORM, K1, K2, NACTIV, NARTIF, NZ, NFREE,
     $                   NRANK, NREJTD, NRES, NGQ,
     $                   N, NQ, NROWA, NROWR, NROWT,
     $                   ISTATE, KACTIV, KX,
     $                   CONDMX,
     $                   A, R, T, RES, GQ,
     $                   ZY, WRK1, WRK2 )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ, VERTEX
      INTEGER            ISTATE(*), KACTIV(N), KX(N)
      DOUBLE PRECISION   CONDMX
      DOUBLE PRECISION   A(NROWA,*), R(NROWR,*),
     $                   T(NROWT,*), RES(N,*), GQ(N,*), ZY(NQ,*)
      DOUBLE PRECISION   WRK1(N), WRK2(N)

************************************************************************
*     LSADDS  includes general constraints K1 thru K2 as new rows of
*     the TQ factorization stored in T, ZY.  If NRANK is nonzero, the
*     changes in Q are reflected in NRANK by N triangular factor R such
*     that
*                         W  =  P ( R ) Q,
*                                 ( 0 )
*     where  P  is orthogonal.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written  October-31-1984.
*     This version of LSADDS dated 30-December-1985.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN

      EXTERNAL           DNRM2
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      RTMAX  = WMACH(8)

*     Estimate the condition number of the constraints that are not
*     to be refactorized.

      IF (NACTIV .EQ. 0) THEN
         DTMAX = ZERO
         DTMIN = ONE
      ELSE
         CALL DCOND ( NACTIV, T(NACTIV,NZ+1), NROWT-1, DTMAX, DTMIN )
      END IF

      DO 200 K = K1, K2
         IADD = KACTIV(K)
         JADD = N + IADD
         IF (NACTIV .LT. NFREE) THEN

            CALL LSADD ( UNITQ,
     $                   INFORM, IFIX, IADD, JADD,
     $                   NACTIV, NZ, NFREE, NRANK, NRES, NGQ,
     $                   N, NROWA, NQ, NROWR, NROWT,
     $                   KX, CONDMX,
     $                   A, R, T, RES, GQ, ZY,
     $                   WRK1, WRK2 )

            IF (INFORM .EQ. 0) THEN
               NACTIV = NACTIV + 1
               NZ     = NZ     - 1
            ELSE
               ISTATE(JADD) =   0
               KACTIV(K)    = - KACTIV(K)
            END IF
         END IF
  200 CONTINUE

      IF (NACTIV .LT. K2) THEN

*        Some of the constraints were classed as dependent and not
*        included in the factorization.  Re-order the part of  KACTIV
*        that holds the indices of the general constraints in the
*        working set.  Move accepted indices to the front and shift
*        rejected indices (with negative values) to the end.

         L      = K1 - 1
         DO 300 K = K1, K2
            I         = KACTIV(K)
            IF (I .GE. 0) THEN
               L      = L + 1
               IF (L .NE. K) THEN
                  ISWAP     = KACTIV(L)
                  KACTIV(L) = I
                  KACTIV(K) = ISWAP
               END IF
            END IF
  300    CONTINUE

*        If a vertex is required, add some temporary bounds.
*        We must accept the resulting condition number of the working
*        set.

         IF (VERTEX) THEN
            CNDMAX = RTMAX
            NZADD  = NZ
            DO 320 IARTIF = 1, NZADD
               ROWMAX = ZERO
               DO 310 I = 1, NFREE
                  RNORM = DNRM2 ( NZ, ZY(I,1), NQ )
                  IF (ROWMAX .LT. RNORM) THEN
                     ROWMAX = RNORM
                     IFIX   = I
                  END IF
  310          CONTINUE
               JADD = KX(IFIX)

               CALL LSADD ( UNITQ,
     $                      INFORM, IFIX, IADD, JADD,
     $                      NACTIV, NZ, NFREE, NRANK, NRES, NGQ,
     $                      N, NROWA, NQ, NROWR, NROWT,
     $                      KX, CNDMAX,
     $                      A, R, T, RES, GQ, ZY,
     $                      WRK1, WRK2 )

               NFREE  = NFREE  - 1
               NZ     = NZ     - 1
               NARTIF = NARTIF + 1
               ISTATE(JADD) = 4
  320       CONTINUE
         END IF
      END IF

      NREJTD = K2 - NACTIV

      RETURN

*     End of  LSADDS.

      END
