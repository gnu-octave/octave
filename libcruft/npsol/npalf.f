*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  NPSUBS FORTRAN
*
*     NPALF    NPCHKD   NPCORE   NPCRSH   NPDFLT   NPFD     NPFEAS
*     NPFILE   NPIQP    NPKEY    NPLOC    NPMRT    NPOPTN   NPPRT
*     NPRSET   NPSETX   NPSRCH   NPUPDT   NPSOL
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPALF ( INFORM, N, NCLIN, NCNLN,
     $                   ALFA, ALFMIN, ALFMAX, BIGBND, DXNORM,
     $                   ANORM, ADX, AX, BL, BU,
     $                   DSLK, DX, SLK, X )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION   ANORM(*), ADX(*), AX(*), BL(*), BU(*),
     $                   DSLK(*), DX(N), SLK(*), X(N)

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      LOGICAL            CMDBG
      INTEGER            LCMDBG
      PARAMETER         (LCMDBG = 5)
      COMMON    /CMDEBG/ ICMDBG(LCMDBG), CMDBG

************************************************************************
*  NPALF   finds a step ALFA such that the point x + ALFA*P reaches one
*  of the slacks or linear constraints.  The step ALFA is the maximum
*  step that can be taken without violating one of the slacks or linear
*  constraints that is currently satisfied.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 77 version written  June 1986.
*  This version of NPALF dated  27-June-1986.
************************************************************************
      INTRINSIC          ABS, MAX, MIN
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      IF (CMDBG  .AND.  ICMDBG(3) .GT. 0) WRITE (NOUT, 1000)

      ALFA   = ALFMAX
      J      = 1

*+    WHILE (J .LE. N+NCLIN+NCNLN .AND. ALFA .GT. ALFMIN) DO
  100 IF    (J .LE. N+NCLIN+NCNLN .AND. ALFA .GT. ALFMIN) THEN

         IF      (J .LE. N      ) THEN
            AXI    =  X(J)
            ADXI   = DX(J)
            ROWNRM = ONE
         ELSE IF (J .LE. N+NCLIN) THEN
            I      = J - N
            AXI    = AX(I)
            ADXI   = ADX(I)
            ROWNRM = ANORM(I) + ONE
         ELSE
            I      = J - N - NCLIN
            AXI    = SLK(I)
            ADXI   = DSLK(I)
            ROWNRM = ONE
         END IF

         RES = - ONE
         IF (ADXI .LE. - EPSPT9*ROWNRM*DXNORM) THEN

*           Constraint decreasing.

            ADXI = - ADXI
            IF (BL(J) .GT. -BIGBND) RES = AXI   - BL(J)

         ELSE IF (ADXI .GT.   EPSPT9*ROWNRM*DXNORM) THEN

*           Constraint increasing.

            IF (BU(J) .LT.  BIGBND) RES = BU(J) - AXI

         END IF

         IF (RES .GT. ZERO  .AND.  ALFA*ADXI .GT. RES)
     $      ALFA  = RES / ADXI

         IF (CMDBG  .AND.  ICMDBG(3) .GT. 0)
     $      WRITE (NOUT, 1200) J, RES, ADXI, ALFA

         J = J + 1
         GO TO 100
*+    END WHILE
      END IF

*     ==================================================================
*     Determine ALFA, the bound on the step to be taken.
*     ==================================================================
      ALFA   = MAX( ALFA, ALFMIN )

      INFORM = 0
      IF (ALFA .GE. ALFMAX) INFORM = 1

      IF (CMDBG  .AND.  ICMDBG(1) .GT. 0  .AND.  INFORM .GT. 0)
     $   WRITE (NOUT, 9010) ALFA

      RETURN

 1000 FORMAT(/ ' NPALF  entered'
     $       / '    J            RES             AP           ALFA '/)
 1200 FORMAT( I5, 3G15.5 )
 9010 FORMAT(/ ' //NPALF //  No finite step.'
     $       / ' //NPALF //             ALFA'
     $       / ' //NPALF //  ', G15.4 )

*     End of  NPALF .

      END
