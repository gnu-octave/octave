*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPMRT ( FEASQP, N, NCLIN, NCNLN,
     $                   OBJALF, GRDALF, QPCURV,
     $                   ISTATE,
     $                   CJDX, CMUL, CS,
     $                   DLAM, RHO, VIOLN,
     $                   WORK1, WORK2 )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)

      LOGICAL            FEASQP

      INTEGER            ISTATE(*)

      DOUBLE PRECISION   CJDX(*), CMUL(*), CS(*),
     $                   DLAM(*), RHO(*), VIOLN(*)
      DOUBLE PRECISION   WORK1(*), WORK2(*)

************************************************************************
*  NPMRT   computes the value and directional derivative of the
*  augmented Lagrangian merit function.  The penalty parameters RHO(j)
*  are boosted if the directional derivative of the resulting augmented
*  Lagrangian function is not sufficiently negative.  If RHO needs to
*  be increased,  the perturbation with minimum two-norm is found that
*  gives a directional derivative equal to  - p'Hp.
*
*  Systems Optimization Laboratory, Stanford University, California.
*  Original version written  27-May-1985.
*  This version of  NPMRT  dated 14-November-1985.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      COMMON    /SOL1CM/ NOUT

      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            BOOST , OVERFL
      EXTERNAL           DDIV  , DDOT  , DNRM2
      INTRINSIC          ABS   , MAX   , MIN   , SQRT
      PARAMETER        ( ZERO   = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
      PARAMETER        ( TWO    = 2.0D+0                              )

      IF (NCNLN .EQ. 0) RETURN

      RTMIN  = WMACH(6)

      OBJALF = OBJALF - DDOT  ( NCNLN, CMUL, 1, CS, 1 )
      GRDALF = GRDALF - DDOT  ( NCNLN, DLAM, 1, CS, 1 )

      CALL DCOPY ( NCNLN, CS, 1, WORK1, 1 )

      IF (.NOT. FEASQP) THEN
         NPLIN  = N + NCLIN

         DO 100 I = 1, NCNLN
            IF (ISTATE(NPLIN+I) .LT. 0  .OR.  VIOLN(I) .NE. ZERO)
     $         WORK1(I) = - CJDX(I)
  100    CONTINUE
      END IF

      GRDALF = GRDALF + DDOT  ( NCNLN, WORK1, 1, CMUL, 1 )

      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1000) QPCURV, GRDALF

      IF (FEASQP) THEN

*        Find the quantities that define  rhomin, the vector of minimum
*        two-norm such that the directional derivative is one half of
*        approximate curvature   - (dx)'H(dx).

         DO 350 I = 1, NCNLN
            IF (ABS( CS(I) ) .LE. RTMIN) THEN
               WORK2(I) = ZERO
            ELSE
               WORK2(I) = CS(I)**2
            END IF
  350    CONTINUE

         QNORM  = DNRM2 ( NCNLN, WORK2, 1 )
         TSCL   = DDIV  ( GRDALF + HALF*QPCURV, QNORM, OVERFL )
         IF (ABS( TSCL ) .LE. RHOMAX  .AND.  .NOT. OVERFL) THEN
*           ------------------------------------------------------------
*           Bounded  RHOMIN  found.  The final value of  RHO(J)  will
*           never be less than  RHOMIN(j).  If the  QP  was feasible,  a
*           trial value  RHONEW  is computed that is equal to the
*           geometric mean of the previous  RHO  and a damped value of
*           RHOMIN.  The new  RHO  is defined as  RHONEW  if it is less
*           than half the previous  RHO  and greater than  RHOMIN.
*           ------------------------------------------------------------
            SCALE  = ONE
            DO 400 I = 1, NCNLN
               RHOMIN = MAX(  (WORK2(I)/QNORM)*TSCL, ZERO )
               RHOI   = RHO(I)

               RHONEW = SQRT( RHOI*(RHODMP + RHOMIN) )
               IF (RHONEW .LT. HALF*RHOI  ) RHOI = RHONEW
               IF (RHOI   .LT.      RHOMIN) RHOI = RHOMIN
               RHO(I) = RHOI
  400       CONTINUE

            RHO1   = RHONRM
            RHONRM = DNRM2 ( NCNLN, RHO, 1 )

*           ------------------------------------------------------------
*           If  INCRUN = true,  there has been a run of iterations in
*           which the norm of  RHO  has not decreased.  Conversely,
*           INCRUN = false  implies that there has been a run of
*           iterations in which the norm of RHO has not increased.  If
*           INCRUN changes during this iteration the damping parameter
*           RHODMP is increased by a factor of two.  This ensures that
*           RHO(j) will oscillate only a finite number of times.
*           ------------------------------------------------------------
            BOOST  = .FALSE.
            IF (      INCRUN  .AND.  RHONRM .LT. RHO1) BOOST = .TRUE.
            IF (.NOT. INCRUN  .AND.  RHONRM .GT. RHO1) BOOST = .TRUE.
            IF (BOOST) THEN
               RHODMP = TWO*RHODMP
               INCRUN = .NOT. INCRUN
            END IF
         END IF

         IF (NPDBG  .AND.  INPDBG(2) .GT. 0)
     $      WRITE (NOUT, 1200) (RHO(L), L=1,NCNLN)

      ELSE

*        The  QP  was infeasible.  Do not alter the penalty parameters,
*        but compute the scale factor so that the constraint violations
*        are reduced.

         CALL DDSCL ( NCNLN, RHO, 1, WORK1, 1 )
         PTERM2 = DDOT  ( NCNLN, WORK1, 1, CS, 1 )

         SCALE  = RHOMAX
         TSCL   = DDIV  ( GRDALF, PTERM2, OVERFL )
         IF (TSCL .GT. SCALE  .AND.  TSCL .LE. RHOMAX/(ONE+RHONRM)
     $                        .AND.  .NOT. OVERFL)
     $      SCALE = TSCL

         CALL DCOPY ( NCNLN, CS, 1, WORK1, 1 )
      END IF

*     ------------------------------------------------------------------
*     Compute the new value and directional derivative of the
*     merit function.
*     ------------------------------------------------------------------
      CALL DDSCL ( NCNLN, RHO, 1, WORK1, 1 )

      PTERM  = DDOT  ( NCNLN, WORK1, 1, CS, 1 )
      OBJALF = OBJALF + HALF*SCALE*PTERM

      IF (FEASQP)
     $  PTERM2 = PTERM

      GRDALF = GRDALF -      SCALE*PTERM2

      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1100) SCALE, RHONRM, GRDALF

      RETURN

 1000 FORMAT(/ ' //NPMRT //        QPCURV        GRDALF '
     $       / ' //NPMRT //', 1P2E14.2 )
 1100 FORMAT(/ ' //NPMRT //         SCALE        RHONRM        GRDALF '
     $       / ' //NPMRT //', 1P3E14.2 )
 1200 FORMAT(/ ' //NPMRT //  Penalty parameters =       '/ (1P5E15.6))

*     End of  NPMRT .

      END
