*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMALF ( FIRSTV, HITLOW, ISTATE, INFORM, JADD,
     $                   N, NROWA, NCLIN, NCTOTL, NUMINF,
     $                   ALFA, PALFA, ATPHIT, BIGALF, BIGBND, PNORM,
     $                   ANORM, AP, AX, BL, BU, FEATOL, P, X )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            ISTATE(NCTOTL)
      DOUBLE PRECISION   ANORM(*), AP(*), AX(*),
     $                   BL(NCTOTL), BU(NCTOTL), FEATOL(NCTOTL),
     $                   P(N), X(N)
      LOGICAL            FIRSTV, HITLOW

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      LOGICAL            CMDBG
      INTEGER            LCMDBG
      PARAMETER         (LCMDBG = 5)
      COMMON    /CMDEBG/ ICMDBG(LCMDBG), CMDBG

************************************************************************
*  CMALF   finds a step ALFA such that the point x + ALFA*P reaches one
*  of the linear constraints (including bounds).  Two possible steps are
*  defined as follows...
*
*  ALFA1   is the maximum step that can be taken without violating
*          one of the linear constraints that is currently satisfied.
*  ALFA2   reaches a linear constraint that is currently violated.
*          Usually this will be the furthest such constraint along P,
*          but if FIRSTV = .TRUE. it will be the first one along P.
*          This is used only when the problem has been determined to be
*          infeasible, and the sum of infeasibilities are being
*          minimized.  (ALFA2  is not defined if NUMINF = 0.)
*
*  ALFA will usually be the minimum of ALFA1 and ALFA2.
*  ALFA could be negative (since we allow inactive constraints
*  to be violated by as much as FEATOL).  In such cases, a
*  third possible step is computed, to find the nearest satisfied
*  constraint (perturbed by FEATOL) along the direction  - P.
*  ALFA  will be reset to this step if it is shorter.  This is the
*  only case for which the final step  ALFA  does not move X exactly
*  onto a constraint (the one denoted by JADD).
*
*  Constraints in the working set are ignored  (ISTATE(j) ge 1).
*
*  JADD    denotes which linear constraint is reached.
*
*  HITLOW  indicates whether it is the lower or upper bound that
*          has restricted ALFA.
*
*  Values of ISTATE(j)....
*
*     - 2         - 1         0           1          2         3
*  a'x lt bl   a'x gt bu   a'x free   a'x = bl   a'x = bu   bl = bu
*
*  The values -2 and -1 do not occur once a feasible point has been
*  found.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 66 version written  May 1980.
*  This version of  CMALF  dated  10-June-1986.
************************************************************************
      LOGICAL            HLOW1, HLOW2, LASTV, NEGSTP, STEP2
      INTRINSIC          ABS, MIN
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      INFORM = 0

*     ------------------------------------------------------------------
*     First pass -- find steps to perturbed constraints, so that
*     PALFA1 will be slightly larger than the true step, and
*     PALFA2 will be slightly smaller than it should be.
*     In degenerate cases, this strategy gives us some freedom in the
*     second pass.  The general idea follows that described by P.M.J.
*     Harris, p.21 of Mathematical Programming 5, 1 (1973), 1--28.
*     ------------------------------------------------------------------

      NEGSTP = .FALSE.
      CALL CMALF1( FIRSTV, NEGSTP, BIGALF, BIGBND, PNORM,
     $             JADD1, JADD2, PALFA1, PALFA2,
     $             ISTATE, N, NROWA, NCTOTL,
     $             ANORM, AP, AX, BL, BU, FEATOL, P, X )

      JSAVE1 = JADD1
      JSAVE2 = JADD2

*     ------------------------------------------------------------------
*     Second pass -- recompute step-lengths without perturbation.
*     Amongst constraints that are less than the perturbed steps,
*     choose the one (of each type) that makes the largest angle
*     with the search direction.
*     ------------------------------------------------------------------
      IF (CMDBG  .AND.  ICMDBG(3) .GT. 0) WRITE (NOUT, 1000)
      ALFA1  = BIGALF
      ALFA2  = ZERO
      IF (FIRSTV) ALFA2 = BIGALF

      APMAX1 = ZERO
      APMAX2 = ZERO
      ATP1   = ZERO
      ATP2   = ZERO
      HLOW1  = .FALSE.
      HLOW2  = .FALSE.
      LASTV  = .NOT. FIRSTV

      DO 400 J = 1, NCTOTL
         JS = ISTATE(J)
         IF (JS .LE. 0) THEN
            IF (J  .LE. N)  THEN
               ATX    = X(J)
               ATP    = P(J)
               ROWNRM = ONE
            ELSE
               I      = J - N
               ATX    = AX(I)
               ATP    = AP(I)
               ROWNRM = ANORM(I) + ONE
            END IF

            IF ( ABS( ATP ) .LE. EPSPT9*ROWNRM*PNORM) THEN

*              This constraint appears to be constant along P.  It is
*              not used to compute the step.  Give the residual a value
*              that can be spotted in the debug output.

               RES = - ONE
            ELSE IF (ATP .LE. ZERO  .AND.  JS .NE. -2) THEN
*              ---------------------------------------------------------
*              a'x  is decreasing.
*              ---------------------------------------------------------
*              The lower bound is satisfied.  Test for smaller ALFA1.

               ABSATP = - ATP
               IF (BL(J) .GT. (-BIGBND)) THEN
                  RES    = ATX - BL(J)
                  IF (PALFA1*ABSATP .GE. RES  .OR.  J .EQ. JSAVE1) THEN
                     IF (APMAX1*ROWNRM*PNORM .LT. ABSATP) THEN
                        APMAX1 = ABSATP / (ROWNRM*PNORM)
                        ALFA1  = RES / ABSATP
                        JADD1  = J
                        ATP1   = ATP
                        HLOW1  = .TRUE.
                     END IF
                  END IF
               END IF

               IF (JS. EQ. -1)  THEN

*                 The upper bound is violated.  Test for either a bigger
*                 or smaller ALFA2,  depending on the value of FIRSTV.

                  RES    = ATX - BU(J)
                  IF (     (FIRSTV  .AND.  PALFA2*ABSATP .GE. RES
     $                 .OR.  LASTV  .AND.  PALFA2*ABSATP .LE. RES)
     $                 .OR.  J .EQ.  JSAVE2) THEN
                     IF (APMAX2*ROWNRM*PNORM .LT. ABSATP) THEN
                        APMAX2 = ABSATP / (ROWNRM*PNORM)
                        IF      (ABSATP .GE. ONE          ) THEN
                           ALFA2 = RES / ABSATP
                        ELSE IF (RES    .LT. BIGALF*ABSATP) THEN
                           ALFA2 = RES / ABSATP
                        ELSE
                           ALFA2 = BIGALF
                        END IF
                        JADD2  = J
                        ATP2   = ATP
                        HLOW2  = .FALSE.
                     END IF
                  END IF
               END IF
            ELSE IF (ATP .GT. ZERO  .AND.  JS .NE.  -1)  THEN
*              ---------------------------------------------------------
*              a'x  is increasing and the upper bound is not violated.
*              ---------------------------------------------------------
*              Test for smaller ALFA1.

               IF (BU(J) .LT. BIGBND) THEN
                  RES = BU(J) - ATX
                  IF (PALFA1*ATP .GE. RES  .OR.  J .EQ. JSAVE1) THEN
                     IF (APMAX1*ROWNRM*PNORM .LT. ATP) THEN
                        APMAX1 = ATP / (ROWNRM*PNORM)
                        ALFA1  = RES / ATP
                        JADD1  = J
                        ATP1   = ATP
                        HLOW1  = .FALSE.
                     END IF
                  END IF
               END IF

               IF (JS .EQ. -2)  THEN

*                 The lower bound is violated.  Test for a new ALFA2.

                  RES    = BL(J) - ATX
                  IF (     (FIRSTV  .AND.  PALFA2*ATP .GE. RES
     $                 .OR.  LASTV  .AND.  PALFA2*ATP .LE. RES)
     $                 .OR.  J .EQ.  JSAVE2) THEN
                     IF (APMAX2*ROWNRM*PNORM .LT. ATP) THEN
                        APMAX2 = ATP / (ROWNRM*PNORM)
                        IF      (ATP .GE. ONE       ) THEN
                           ALFA2 = RES / ATP
                        ELSE IF (RES .LT. BIGALF*ATP) THEN
                           ALFA2 = RES / ATP
                        ELSE
                           ALFA2 = BIGALF
                        END IF
                        JADD2  = J
                        ATP2   = ATP
                        HLOW2  = .TRUE.
                     END IF
                  END IF
               END IF
            END IF

            IF (CMDBG  .AND.  ICMDBG(3) .GT. 0)
     $      WRITE (NOUT, 1200) J, JS, FEATOL(J), RES, ATP, JADD1,
     $                         ALFA1, JADD2, ALFA2
         END IF
  400 CONTINUE

*     ==================================================================
*     Determine ALFA, the step to be taken.
*     ==================================================================
*     In the infeasible case, check whether to take the step ALFA2
*     rather than ALFA1...

      STEP2 = NUMINF .GT. 0  .AND.  JADD2 .GT. 0

*     We do so if ALFA2 is less than ALFA1 or (if FIRSTV is false)
*     lies in the range  (ALFA1, PALFA1)  and has a smaller value of
*     ATP.

      STEP2 = STEP2 .AND. (ALFA2 .LT. ALFA1   .OR.   LASTV  .AND.
     $                     ALFA2 .LE. PALFA1  .AND.  APMAX2 .GE. APMAX1)

      IF (STEP2) THEN
         ALFA   = ALFA2
         PALFA  = PALFA2
         JADD   = JADD2
         ATPHIT = ATP2
         HITLOW = HLOW2
      ELSE
         ALFA   = ALFA1
         PALFA  = PALFA1
         JADD   = JADD1
         ATPHIT = ATP1
         HITLOW = HLOW1

*        If ALFA1 is negative, the constraint to be added (JADD)
*        remains unchanged, but ALFA may be shortened to the step
*        to the nearest perturbed satisfied constraint along  - P.

         NEGSTP = ALFA .LT. ZERO
         IF (NEGSTP) THEN
            CALL CMALF1( FIRSTV, NEGSTP, BIGALF, BIGBND, PNORM,
     $                   JADD1, JADD2, PALFA1, PALFA2,
     $                   ISTATE, N, NROWA, NCTOTL,
     $                   ANORM, AP, AX, BL, BU, FEATOL, P, X )

            IF (CMDBG  .AND.  ICMDBG(1) .GT. 0)
     $         WRITE (NOUT, 9000) ALFA, PALFA1

            ALFA = - MIN( ABS( ALFA ), PALFA1 )
         END IF
      END IF

*     Test for undefined or infinite step.

      IF (JADD .EQ. 0) THEN
         ALFA   = BIGALF
         PALFA  = BIGALF
      END IF

      IF (ALFA .GE. BIGALF) INFORM = 3
      IF (CMDBG  .AND.  ICMDBG(1) .GT. 0  .AND.  INFORM .GT. 0)
     $   WRITE (NOUT, 9010) JADD, ALFA
      RETURN

 1000 FORMAT(/ ' CMALF  entered'
     $       / '    J  JS         FEATOL        RES             AP',
     $         '     JADD1        ALFA1     JADD2        ALFA2 '/)
 1200 FORMAT( I5, I4, 3G15.5, 2(I6, G17.7) )
 9000 FORMAT(/ ' //CMALF //  Negative step',
     $       / ' //CMALF //           ALFA          PALFA'
     $       / ' //CMALF //', 2G15.4 )
 9010 FORMAT(/ ' //CMALF //  Unbounded step.'
     $       / ' //CMALF //  JADD           ALFA'
     $       / ' //CMALF //  ', I4, G15.4 )

*     End of  CMALF .

      END
