*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPFEAS( N, NCLIN, NCNLN, ISTATE,
     $                   BIGBND, CVNORM, ERRMAX, JMAX, NVIOL,
     $                   AX, BL, BU, C, FEATOL, X, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            ISTATE(N+NCLIN+NCNLN)
      DOUBLE PRECISION   AX(*), BL(N+NCLIN+NCNLN), BU(N+NCLIN+NCNLN)
      DOUBLE PRECISION   C(*), FEATOL(N+NCLIN+NCNLN), X(N)
      DOUBLE PRECISION   WORK(N+NCLIN+NCNLN)
************************************************************************
*  NPFEAS  computes the following...
*  (1)  The number of constraints that are violated by more
*       than  FEATOL  and the 2-norm of the constraint violations.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version      April    1984.
*  This version of  NPFEAS  dated  16-October-1985.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            NPDBG
      PARAMETER        ( LDBG = 5 )
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      EXTERNAL           IDAMAX, DNRM2
      INTRINSIC          ABS
      PARAMETER        ( ZERO = 0.0D+0 )

      BIGLOW = - BIGBND
      BIGUPP =   BIGBND

*     ==================================================================
*     Compute NVIOL, the number of constraints violated by more than
*     FEATOL,  and CVNORM,  the 2-norm of the constraint
*     violations and residuals of the constraints in the QP working set.
*     ==================================================================
      NVIOL  = 0

      DO 200 J = 1, N+NCLIN+NCNLN
         FEASJ  = FEATOL(J)
         RES    = ZERO

         IF (J .LE. N + NCLIN) THEN

*           Bound or general linear constraint.

            IF (J .LE. N) THEN
               CON =  X(J)
            ELSE
               CON = AX(J-N)
            END IF

            TOLJ   = FEASJ
         ELSE

*           Nonlinear constraint.

            CON    = C(J-N-NCLIN)
            TOLJ   = ZERO
         END IF

*        Check for constraint violations.

         IF (BL(J) .GT. BIGLOW) THEN
            RES    = BL(J) - CON
            IF (RES .GT.   FEASJ ) NVIOL = NVIOL + 1
            IF (RES .GT.    TOLJ ) GO TO 190
         END IF

         IF (BU(J) .LT. BIGUPP) THEN
            RES    = BU(J) - CON
            IF (RES .LT. (-FEASJ)) NVIOL = NVIOL + 1
            IF (RES .LT.  (-TOLJ)) GO TO 190
         END IF

*        This constraint is satisfied,  but count the residual as a
*        violation if the constraint is in the working set.

         IS     = ISTATE(J)

         IF (IS .EQ. 0) THEN
            RES = ZERO
         ELSE IF (IS .EQ. 1  .OR.  IS .LE. -2) THEN
            RES = BL(J) - CON
         ELSE IF (IS .GE. 2  .OR.  IS .EQ. -1) THEN
            RES = BU(J) - CON
         END IF

         IF (ABS( RES ) .GT. FEASJ) NVIOL = NVIOL + 1

*        Set the array of violations.

  190    WORK(J) = RES
  200 CONTINUE

      JMAX   = IDAMAX( N+NCLIN+NCNLN, WORK, 1 )
      ERRMAX = ABS ( WORK(JMAX) )

      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1000) ERRMAX, JMAX

      CVNORM = DNRM2 ( N+NCLIN+NCNLN, WORK, 1 )

      RETURN

 1000 FORMAT(/ ' //NPFEAS//  The maximum violation is ', 1PE14.2,
     $                     ' in constraint', I5 )

*     End of  NPFEAS.

      END
