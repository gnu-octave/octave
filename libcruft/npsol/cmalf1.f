*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  CMSUBS FORTRAN
*
*     CMALF1   CMALF    CMCHK    CMPERM   CMPRT    CMQMUL   CMR1MD
*     CMRSWP   CMTSOL
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMALF1( FIRSTV, NEGSTP, BIGALF, BIGBND, PNORM,
     $                   JADD1 , JADD2 , PALFA1, PALFA2,
     $                   ISTATE, N, NROWA, NCTOTL,
     $                   ANORM, AP, AX, BL, BU, FEATOL, P, X )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            FIRSTV, NEGSTP
      INTEGER            ISTATE(NCTOTL)
      DOUBLE PRECISION   ANORM(*), AP(*), AX(*)
      DOUBLE PRECISION   BL(NCTOTL), BU(NCTOTL), FEATOL(NCTOTL),
     $                   P(N), X(N)

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      LOGICAL            CMDBG
      INTEGER            LCMDBG
      PARAMETER         (LCMDBG = 5)
      COMMON    /CMDEBG/ ICMDBG(LCMDBG), CMDBG

************************************************************************
*     CMALF1  finds steps PALFA1, PALFA2 such that
*        X + PALFA1*P  reaches a linear constraint that is currently not
*                      in the working set but is satisfied.
*        X + PALFA2*P  reaches a linear constraint that is currently not
*                      in the working set but is violated.
*     The constraints are perturbed by an amount FEATOL, so that PALFA1
*     is slightly larger than it should be,  and PALFA2 is slightly
*     smaller than it should be.  This gives some leeway later when the
*     exact steps are computed by CMALF.
*
*     Constraints in the working set are ignored  (ISTATE(j) .GE. 1).
*
*     If NEGSTP is true, the search direction will be taken to be  - P.
*
*
*     Values of ISTATE(j)....
*
*        - 2         - 1         0           1          2         3
*     a'x lt bl   a'x gt bu   a'x free   a'x = bl   a'x = bu   bl = bu
*
*     The values  -2  and  -1  do not occur once a feasible point has
*     been found.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original Fortran 66 version written  May 1980.
*     This version of CMALF1 dated 26-June-1986.
************************************************************************
      LOGICAL            LASTV
      INTRINSIC          ABS
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      IF (CMDBG  .AND.  ICMDBG(3) .GT. 0) WRITE (NOUT, 1100)
      LASTV  = .NOT. FIRSTV
      JADD1  = 0
      JADD2  = 0
      PALFA1 = BIGALF

      PALFA2 = ZERO
      IF (FIRSTV) PALFA2 = BIGALF

      DO 200 J = 1, NCTOTL
         JS = ISTATE(J)
         IF (JS .LE. 0) THEN
            IF (J .LE. N) THEN
               ATX    = X(J)
               ATP    = P(J)
               ROWNRM = ONE
            ELSE
               I      = J - N
               ATX    = AX(I)
               ATP    = AP(I)
               ROWNRM = ONE  +  ANORM(I)
            END IF
            IF (NEGSTP) ATP = - ATP

            IF ( ABS( ATP ) .LE. EPSPT9*ROWNRM*PNORM) THEN

*              This constraint appears to be constant along P.  It is
*              not used to compute the step.  Give the residual a value
*              that can be spotted in the debug output.

               RES = - ONE
            ELSE IF (ATP .LE. ZERO  .AND.  JS .NE. -2) THEN
*              ---------------------------------------------------------
*              a'x  is decreasing and the lower bound is not violated.
*              ---------------------------------------------------------
*              First test for smaller PALFA1.

               ABSATP = - ATP
               IF (BL(J) .GT. (-BIGBND)) THEN
                  RES    = ATX - BL(J) + FEATOL(J)
                  IF (BIGALF*ABSATP .GT. ABS( RES )) THEN
                     IF (PALFA1*ABSATP .GT. RES)  THEN
                        PALFA1 = RES / ABSATP
                        JADD1  = J
                     END IF
                  END IF
               END IF

               IF (JS .EQ. -1) THEN

*                 The upper bound is violated.  Test for either larger
*                 or smaller PALFA2, depending on the value of FIRSTV.

                  RES    = ATX - BU(J) - FEATOL(J)
                  IF (BIGALF*ABSATP .GT. ABS( RES )) THEN
                     IF (FIRSTV  .AND.  PALFA2*ABSATP .GT. RES  .OR.
     $                    LASTV  .AND.  PALFA2*ABSATP .LT. RES) THEN
                        PALFA2 = RES / ABSATP
                        JADD2  = J
                     END IF
                  END IF
               END IF
            ELSE IF (ATP .GT. ZERO  .AND.  JS .NE. -1) THEN
*              ---------------------------------------------------------
*              a'x  is increasing and the upper bound is not violated.
*              ---------------------------------------------------------
*              Test for smaller PALFA1.

               IF (BU(J) .LT. BIGBND) THEN
                  RES = BU(J) - ATX + FEATOL(J)
                  IF (BIGALF*ATP .GT. ABS( RES )) THEN
                     IF (PALFA1*ATP .GT. RES) THEN
                        PALFA1 = RES / ATP
                        JADD1  = J
                     END IF
                  END IF
               END IF

               IF (JS .EQ. -2) THEN

*                 The lower bound is violated.  Test for a new PALFA2.

                  RES  = BL(J) - ATX - FEATOL(J)
                  IF (BIGALF*ATP .GT. ABS( RES )) THEN
                     IF (FIRSTV  .AND.  PALFA2*ATP .GT. RES  .OR.
     $                    LASTV  .AND.  PALFA2*ATP .LT. RES) THEN
                        PALFA2 = RES / ATP
                        JADD2  = J
                     END IF
                  END IF
               END IF
            END IF

            IF (CMDBG  .AND.  ICMDBG(3) .GT. 0)
     $         WRITE (NOUT, 1200) J, JS, FEATOL(J), RES,
     $                            ATP, JADD1, PALFA1, JADD2, PALFA2
         END IF
  200 CONTINUE

      RETURN

 1100 FORMAT(/ '    J  JS         FEATOL        RES             AP',
     $         '     JADD1       PALFA1     JADD2       PALFA2' /)
 1200 FORMAT(I5, I4, 3G15.5, 2(I6, G17.7))

*     End of  CMALF1.

      END
