*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSBNDS( UNITQ,
     $                   INFORM, NZ, NFREE, NRANK, NRES, NGQ,
     $                   N, NQ, NROWA, NROWR, NROWT,
     $                   ISTATE, KX,
     $                   CONDMX,
     $                   A, R, T, RES, GQ,
     $                   ZY, WRK1, WRK2 )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            ISTATE(*), KX(N)
      DOUBLE PRECISION   CONDMX
      DOUBLE PRECISION   A(NROWA,*), R(NROWR,*),
     $                   T(NROWT,*), RES(N,*), GQ(N,*), ZY(NQ,*)
      DOUBLE PRECISION   WRK1(N), WRK2(N)

************************************************************************
*     LSBNDS updates the factor R as KX is reordered to reflect the
*     status of the bound constraints given by ISTATE.  KX is reordered
*     so that the fixed variables come last.  One of two alternative
*     are used to reorder KX. One method needs fewer accesses to KX, the
*     other gives a matrix Rz with more rows and columns.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written  30-December-1985.
*     This version dated 30-December-1985.
************************************************************************

      NFIXED = N - NFREE

      IF (NRANK .LT. N  .AND.  NRANK .GT. 0) THEN
*        ---------------------------------------------------------------
*        R is specified but singular.  Try and keep the dimension of Rz
*        as large as possible.
*        ---------------------------------------------------------------
         NACTV = 0
         NFREE = N
         NZ    = N

         J     = N
*+       WHILE (J .GT. 0  .AND.  N-NFREE .LT. NFIXED) DO
  100    IF    (J .GT. 0  .AND.  N-NFREE .LT. NFIXED) THEN
            IF (ISTATE(J) .GT. 0) THEN
               JADD = J
               DO 110 IFIX = NFREE, 1, -1
                  IF (KX(IFIX) .EQ. JADD) GO TO 120
  110          CONTINUE

*              Add bound JADD.

  120          CALL LSADD ( UNITQ,
     $                      INFORM, IFIX, IADD, JADD,
     $                      NACTV, NZ, NFREE, NRANK, NRES, NGQ,
     $                      N, NROWA, NQ, NROWR, NROWT,
     $                      KX, CONDMX,
     $                      A, R, T, RES, GQ, ZY,
     $                      WRK1, WRK2 )

               NFREE = NFREE - 1
               NZ    = NZ    - 1
            END IF
            J = J - 1
            GO TO 100
*+       END WHILE
         END IF
      ELSE
*        ---------------------------------------------------------------
*        R is of full rank,  or is not specified.
*        ---------------------------------------------------------------
         IF (NFIXED .GT. 0) THEN

*           Order KX so that the free variables come first.

            LSTART = NFREE + 1
            DO 250 K = 1, NFREE
               J = KX(K)
               IF (ISTATE(J) .GT. 0) THEN
                  DO 220 L = LSTART, N
                     J2 = KX(L)
                     IF (ISTATE(J2) .EQ. 0) GO TO 230
  220             CONTINUE

  230             KX(K)  = J2
                  KX(L)  = J
                  LSTART = L + 1

                  IF (NRANK .GT. 0)
     $               CALL CMRSWP( N, NRES, NRANK, NROWR, K, L,
     $                            R, RES, WRK1 )
               END IF
  250       CONTINUE

         END IF
         NZ = NFREE
      END IF

      RETURN

*     End of  LSBNDS.

      END
