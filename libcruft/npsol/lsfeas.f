*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSFEAS( N, NCLIN, ISTATE,
     $                   BIGBND, CVNORM, ERRMAX, JMAX, NVIOL,
     $                   AX, BL, BU, FEATOL, X, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            ISTATE(N+NCLIN)
      DOUBLE PRECISION   AX(*), BL(N+NCLIN), BU(N+NCLIN)
      DOUBLE PRECISION   FEATOL(N+NCLIN), X(N)
      DOUBLE PRECISION   WORK(N+NCLIN)

************************************************************************
*  LSFEAS  computes the following...
*  (1)  The number of constraints that are violated by more
*       than  FEATOL  and the 2-norm of the constraint violations.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version      April    1984.
*  This version of  LSFEAS  dated  17-October-1985.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      EXTERNAL           IDAMAX, DNRM2
      INTRINSIC          ABS
      PARAMETER        ( ZERO = 0.0D+0 )

      BIGLOW = - BIGBND
      BIGUPP =   BIGBND

*     ==================================================================
*     Compute NVIOL,  the number of constraints violated by more than
*     FEATOL,  and CVNORM,  the 2-norm of the constraint violations and
*     residuals of the constraints in the working set.
*     ==================================================================
      NVIOL  = 0

      DO 200 J = 1, N+NCLIN
         FEASJ  = FEATOL(J)
         IS     = ISTATE(J)
         RES    = ZERO

         IF (IS .GE. 0  .AND.  IS .LT. 4) THEN
            IF (J .LE. N) THEN
               CON =  X(J)
            ELSE
               I   = J - N
               CON = AX(I)
            END IF

            TOLJ   = FEASJ

*           Check for constraint violations.

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

*           This constraint is satisfied,  but count the residual as a
*           violation if the constraint is in the working set.

            IF (IS .LE. 0) RES = ZERO
            IF (IS .EQ. 1) RES = BL(J) - CON
            IF (IS .GE. 2) RES = BU(J) - CON
            IF (ABS( RES ) .GT. FEASJ) NVIOL = NVIOL + 1
         END IF
  190    WORK(J) = RES
  200 CONTINUE

      JMAX   = IDAMAX( N+NCLIN, WORK, 1 )
      ERRMAX = ABS ( WORK(JMAX) )

      IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $   WRITE (NOUT, 1000) ERRMAX, JMAX

      CVNORM  = DNRM2 ( N+NCLIN, WORK, 1 )

      RETURN

 1000 FORMAT(/ ' //LSFEAS//  The maximum violation is ', 1PE14.2,
     $                     ' in constraint', I5 )

*     End of  LSFEAS.

      END
