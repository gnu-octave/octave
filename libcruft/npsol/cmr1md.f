*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMR1MD( N, NU, NRANK, NROWR, LENV, LENW,
     $                   R, U, V, W )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            N, NU, NRANK, NROWR, LENV, LENW
      DOUBLE PRECISION   R(NROWR,*), U(N,*), V(N), W(N)
************************************************************************
*     CMR1MD  modifies the  nrank*n  upper-triangular matrix  R  so that
*     Q*(R + v*w')  is upper triangular,  where  Q  is orthogonal,
*     v  and  w  are vectors, and the modified  R  overwrites the old.
*     Q  is the product of two sweeps of plane rotations (not stored).
*     If required,  the rotations are applied to the NU columns of
*     the matrix  U.
*
*     The matrix V*W' is an (LENV) by (LENW) matrix.
*     The vector V is overwritten.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version   October  1984.
*     This version of  CMR1MD  dated 18-September-1985.
************************************************************************
      INTRINSIC          MIN

      J = MIN( LENV, NRANK )
      IF (NRANK .GT. 0) THEN

*        ===============================================================
*        Reduce components  1  thru  (J-1)  of  V  to zero,  using a
*        backward sweep of rotations.  The rotations create a horizontal
*        spike in the  j-th  row of  R.  This row is stored in  V.
*        (Note that  DROT3G  sets  V(K) = 0  below as required.)
*        ===============================================================
         LROWJ  = N - J + 1
         VJ     = V(J)
         CALL DCOPY ( LROWJ, R(J,J), NROWR, V(J), 1 )
         LROWK  = LROWJ
         DO 400 K = J-1, 1, -1
            LROWK  = LROWK + 1
            CALL DROT3G( VJ, V(K), CS, SN )
            CALL DROT3 ( LROWK, V(K)  , 1, R(K,K), NROWR, CS, SN )

            IF (NU .GT. 0)
     $      CALL DROT3 ( NU   , U(J,1), N, U(K,1), N    , CS, SN )
  400    CONTINUE

*        ===============================================================
*        Add a multiple of elements  1  thru  LENW  of  W  to the row
*        spike of  R  (stored in elements  1  thru  N  of  V).
*        ===============================================================
         CALL DAXPY ( LENW, VJ, W, 1, V, 1 )

*        ===============================================================
*        Eliminate the row spike  (held in  V)  using a forward sweep
*        of rotations.
*        ===============================================================
         DO 600 K = 1, J-1
            LROWK  = LROWK - 1
            L      = K     + 1
            CALL DROT3G( R(K,K), V(K), CS, SN )
            CALL DROT3 ( LROWK, R(K,L), NROWR, V(L)  , 1, CS, SN )

            IF (NU .GT. 0)
     $      CALL DROT3 ( NU   , U(K,1), N    , U(J,1), N, CS, SN )
  600    CONTINUE
         CALL DCOPY ( LROWJ, V(J), 1, R(J,J), NROWR )
      END IF

      RETURN

*     End of  CMR1MD

      END
