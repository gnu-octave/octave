*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMRSWP( N, NU, NRANK, NROWR, I, J, R, U, V )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            N, NU, NRANK, NROWR, I, J
      DOUBLE PRECISION   R(NROWR,*), U(N,*), V(N)

************************************************************************
*     CMRSWP  interchanges the  I-th  and  J-th  (I .LT. J)  columns of
*     an  NRANK*N  upper-triangular matrix  R   and restores the
*     resulting matrix to upper-triangular form.  The final matrix  R
*     is equal to Q(R + VW')  where  V  and  W  are defined as
*         V   =  Rj  -  Ri      and    W  =  Ei  -  Ej
*     with  Ri  and  Rj  the Ith and Jth columns of  R,  Ei  and  Ej
*     unit vectors.
*
*     The vector V is used as workspace.  R is overwritten.  Q is the
*     product of two sweeps of plane rotations (not stored).
*     If required,  the rotations are applied to the  nu  columns of
*     the matrix  U.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 31-October-1984.
*     This version of  CMRSWP  dated  18-September-1985.
************************************************************************
      INTRINSIC          MIN
      INTEGER            K, L, LENI1, LENJ, LROWJ, LROWK
      DOUBLE PRECISION   CS, SN, VJ

      LENJ   = MIN( J, NRANK )
      IF (LENJ .GT. 0) THEN
         CALL DCOPY ( LENJ, R(1,J), 1, V, 1 )
         IF (I .LE. NRANK) V(I)  = V(I) - R(I,I)
         LENI1 = MIN( I-1, NRANK )
         IF (LENI1 .GT. 0) THEN
            CALL DCOPY ( LENI1, R(1,I), 1, R(1,J), 1 )
            CALL DCOPY ( LENI1, V     , 1, R(1,I), 1 )
         END IF
      END IF
      IF (I .LE. NRANK) THEN

*        ===============================================================
*        Reduce components I thru  (LENJ-1) of V to zero,  using a
*        backward sweep of rotations.  The rotations create a horizontal
*        spike in the LENJ-th row of  R.  This row is stored in V.
*        (Note that  DROT3G  sets  V(K) = 0  below as required.)
*        ===============================================================
         LROWJ  = N - LENJ + 1
         VJ     = V(LENJ)
         CALL DCOPY ( LROWJ, R(LENJ,LENJ), NROWR, V(LENJ), 1 )
         LROWK  = LROWJ
         DO 400 K = LENJ-1, I, -1
            LROWK  = LROWK + 1
            CALL DROT3G( VJ, V(K), CS, SN )
            CALL DROT3 ( LROWK, V(K)     , 1, R(K,K), NROWR, CS, SN )

            IF (NU .GT. 0)
     $      CALL DROT3 ( NU   , U(LENJ,1), N, U(K,1), N    , CS, SN )
  400    CONTINUE

*        ===============================================================
*        Add a multiple of elements I thru J of W to the
*        horizontal spike of  R  (held in elements I thru J of V).
*        ===============================================================
         V(I) = V(I) + VJ
         V(J) = V(J) - VJ

*        ===============================================================
*        Eliminate the row spike  (held in V)  using a forward sweep
*        of rotations.
*        ===============================================================
         DO 600 K = I, LENJ-1
            LROWK  = LROWK - 1
            L      = K     + 1
            CALL DROT3G( R(K,K), V(K), CS, SN )
            CALL DROT3 ( LROWK, R(K,L), NROWR, V(L)     , 1, CS, SN )

            IF (NU .GT. 0)
     $      CALL DROT3 ( NU   , U(K,1), N    , U(LENJ,1), N, CS, SN )
  600    CONTINUE
         CALL DCOPY ( LROWJ, V(LENJ), 1, R(LENJ,LENJ), NROWR )
      END IF

      RETURN

*     End of  CMRSWP

      END
