*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSGETP( LINOBJ, SINGLR, UNITGZ, UNITQ,
     $                   N, NCLIN, NFREE,
     $                   NROWA, NQ, NROWR, NRANK, NUMINF, NZ1,
     $                   ISTATE, KX, CTP, PNORM,
     $                   A, AP, RES, HZ, P,
     $                   GQ, CQ, R, ZY, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            LINOBJ, SINGLR, UNITGZ, UNITQ
      INTEGER            ISTATE(N+NCLIN), KX(N)
      DOUBLE PRECISION   A(NROWA,*), AP(*), RES(*), HZ(*), P(N),
     $                   GQ(N), CQ(*), R(NROWR,*), ZY(NQ,*)
      DOUBLE PRECISION   WORK(N)

************************************************************************
*     LSGETP  computes the following quantities for  LSCORE.
*     (1) The vector  (hz1) = (Rz1)(pz1).
*         If X is not yet feasible,  the product is computed directly.
*         If  Rz1 is singular,  hz1  is zero.  Otherwise  hz1  satisfies
*         the equations
*                        Rz1'hz1 = -gz1,
*         where  g  is the total gradient.  If there is no linear term
*         in the objective,  hz1  is set to  dz1  directly.
*     (2) The search direction P (and its 2-norm).  The vector P is
*         defined as  Z*(pz1), where  (pz1)  depends upon whether or
*         not X is feasible and the nonsingularity of  (Rz1).
*         If  NUMINF .GT. 0,  (pz1)  is the steepest-descent direction.
*         Otherwise,  x  is the solution of the  NZ1*NZ1  triangular
*         system   (Rz1)*(pz1) = (hz1).
*     (3) The vector Ap,  where A is the matrix of linear constraints.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 31-October-1984.
*     Level 2 Blas added 11-June-1986.
*     This version of LSGETP dated 11-June-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      EXTERNAL           DDOT  , DNRM2
      INTRINSIC          MIN
      PARAMETER        ( ZERO = 0.0D+0, ONE  = 1.0D+0 )

      IF (SINGLR) THEN
*        ---------------------------------------------------------------
*        The triangular factor for the current objective function is
*        singular,  i.e., the objective is linear along the last column
*        of Z1.  This can only occur when UNITGZ is TRUE.
*        ---------------------------------------------------------------
         IF (NZ1 .GT. 1) THEN
            CALL DCOPY ( NZ1-1, R(1,NZ1), 1, P, 1 )
            CALL DTRSV ( 'U', 'N', 'N', NZ1-1, R, NROWR, P, 1 )
         END IF
         P(NZ1) = - ONE

         GTP = DDOT  ( NZ1, GQ, 1, P, 1 )
         IF (GTP .GT. ZERO) CALL DSCAL ( NZ1, (-ONE), P, 1 )

         IF (NZ1 .LE. NRANK) THEN
            IF (NUMINF .EQ. 0) THEN
               IF (UNITGZ) THEN
                  HZ(NZ1) = R(NZ1,NZ1)*P(NZ1)
               ELSE
                  CALL DLOAD ( NZ1, (ZERO), HZ, 1 )
               END IF
            ELSE
               HZ(1)   = R(1,1)*P(1)
            END IF
         END IF
      ELSE
*        ---------------------------------------------------------------
*        The objective is quadratic in the space spanned by Z1.
*        ---------------------------------------------------------------
         IF (LINOBJ) THEN
            IF (UNITGZ) THEN
               IF (NZ1 .GT. 1)
     $            CALL DLOAD ( NZ1-1, (ZERO), HZ, 1 )
               HZ(NZ1) = - GQ(NZ1)/R(NZ1,NZ1)
            ELSE
               CALL DCOPY ( NZ1, GQ  , 1, HZ, 1 )
               CALL DSCAL ( NZ1, (-ONE), HZ, 1 )
               CALL DTRSV ( 'U', 'T', 'N', NZ1, R, NROWR, HZ, 1 )
            END IF
         ELSE
            CALL DCOPY ( NZ1, RES, 1, HZ, 1 )
         END IF

*        Solve  Rz1*pz1 = hz1.

         CALL DCOPY ( NZ1, HZ, 1, P, 1 )
         CALL DTRSV ( 'U', 'N', 'N', NZ1, R, NROWR, P, 1 )
      END IF

*     Compute  p = Z1*pz1  and its norm.

      IF (LINOBJ)
     $   CTP = DDOT  ( NZ1, CQ, 1, P, 1 )
      PNORM  = DNRM2 ( NZ1, P, 1 )

      CALL CMQMUL( 1, N, NZ1, NFREE, NQ, UNITQ, KX, P, ZY, WORK )

      IF (LSDBG  .AND.  ILSDBG(2) .GT. 0)
     $   WRITE (NOUT, 1000) (P(J), J = 1, N)

*     Compute  Ap.

      IF (NCLIN .GT. 0) THEN
         CALL DLOAD ( NCLIN, ZERO, AP, 1 )
         DO 410 J = 1, N
            IF (ISTATE(J) .LE. 0)
     $         CALL DAXPY( NCLIN, P(J), A(1,J), 1, AP, 1 )
  410    CONTINUE
         IF (LSDBG  .AND.  ILSDBG(2) .GT. 0)
     $   WRITE (NOUT, 1100) (AP(I), I = 1, NCLIN)
      END IF

      RETURN

 1000 FORMAT(/ ' //LSGETP//   P ... ' / (1P5E15.5))
 1100 FORMAT(/ ' //LSGETP//  AP ... ' / (1P5E15.5))

*     End of  LSGETP.

      END
