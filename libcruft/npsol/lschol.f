*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSCHOL( NROWH, N, NRANK, TOLRNK, KX, H, INFORM )

      IMPLICIT           DOUBLE PRECISION (A-H,O-Z)
      INTEGER            KX(*)
      DOUBLE PRECISION   H(NROWH,*)

************************************************************************
*     LSCHOL  forms the Cholesky factorization of the positive
*     semi-definite matrix H such that
*                   PHP'  =  R'R
*     where  P  is a permutation matrix and  R  is upper triangular.
*     The permutation P is chosen to maximize the diagonal of R at each
*     stage.  Only the diagonal and super-diagonal elements of H are
*     used.
*
*     Output:
*
*         INFORM = 0   the factorization was computed successfully,
*                      with the Cholesky factor written in the upper
*                      triangular part of H and P stored in KX.
*                  1   the matrix H was indefinite.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version of LSCHOL dated  2-February-1981.
*     Level 2 Blas added 29-June-1986.
*     This version of LSCHOL dated  30-June-1986.
************************************************************************

      COMMON    /SOL1CM/ NOUT
      INTRINSIC          ABS   , MAX   , SQRT
      EXTERNAL           IDAMAX
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      INFORM = 0
      NRANK  = 0

*     Main loop for computing rows of  R.

      DO 200 J = 1, N

*        Find maximum available diagonal.

         KMAX = J - 1 + IDAMAX( N-J+1, H(J,J), NROWH+1 )
         DMAX = H(KMAX,KMAX)

         IF (DMAX .LE. TOLRNK*ABS(H(1,1))) GO TO 300

*        Perform a symmetric interchange if necessary.

         IF (KMAX .NE. J) THEN
            K        = KX(KMAX)
            KX(KMAX) = KX(J)
            KX(J)    = K

            CALL DSWAP ( J       , H(1,J)   , 1, H(1,KMAX), 1     )
            CALL DSWAP ( KMAX-J+1, H(J,KMAX), 1, H(J,J)   , NROWH )
            CALL DSWAP ( N-KMAX+1, H(KMAX,KMAX), NROWH,
     $                             H(J,KMAX)   , NROWH )

            H(KMAX,KMAX) = H(J,J)
         END IF

*        Set the diagonal of  R.

         D      = SQRT( DMAX )
         H(J,J) = D
         NRANK  = NRANK + 1

         IF (J .LT. N) THEN

*           Set the super-diagonal elements of this row of R and update
*           the elements of the block that is yet to be factorized.

            CALL DSCAL ( N-J,   (ONE/D), H(J  ,J+1), NROWH )
            CALL DSYR  ( 'U', N-J, -ONE, H(J  ,J+1), NROWH,
     $                                   H(J+1,J+1), NROWH )
         END IF

  200 CONTINUE
*     ------------------------------------------------------------------
*     Check for the semi-definite case.
*     ------------------------------------------------------------------
  300 IF (NRANK .LT. N) THEN

*        Find the largest element in the unfactorized block.

         SUPMAX = ZERO
         DO 310 I = J, N-1
            K      = I + IDAMAX( N-I, H(I,I+1), NROWH )
            SUPMAX = MAX( SUPMAX, ABS(H(I,K)) )
  310    CONTINUE

         IF (SUPMAX .GT. TOLRNK*ABS(H(1,1))) THEN
            WRITE (NOUT, 1000) DMAX, SUPMAX
            INFORM = 1
         END IF
      END IF

      RETURN

 1000 FORMAT(' XXX  Hessian appears to be indefinite.'
     $      /' XXX  Maximum diagonal and off-diagonal ignored',
     $             ' in the Cholesky factorization:', 1P2E22.14 )

*     End of LSCHOL.

      END
