*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  SRSUBS FORTRAN
*
*     SRCHC    SRCHQ
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE SRCHC ( DEBUG, DONE, FIRST, IMPRVD, INFORM,
     $                   ALFMAX, EPSAF, ETA,
     $                   XTRY, FTRY, GTRY, OLDF, OLDG,
     $                   TOLABS, TOLREL, TOLTNY,
     $                   ALFA, ALFBST, FBEST, GBEST )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            DEBUG , DONE  , FIRST , IMPRVD

************************************************************************
*  SRCHC   is a  step-length  algorithm for minimizing a function of one
*  variable.  It will be called  repeatedly  by a  search  routine whose
*  purpose is to  estimate a point  ALFA = ALFBST  that  minimizes  some
*  function  F(alfa)  over the closed interval (0, ALFMAX).
*
*  SRCHC  requires both the function  F(alfa)  and its gradient  G(alfa)
*  to be  evaluated  at various  points within  the interval.  New step-
*  length  estimates  are  computed  using  cubic  interpolation  with
*  safeguards.
*
*  Reverse communication is used to allow the calling program to
*  evaluate F and G.  Some of the parameters must be set or tested
*  by the calling program.  the remainder would ordinarily be local
*  variables.
*
*
*  Input parameters (relevant to the calling program)
*  --------------------------------------------------
*
*  DEBUG         specifies whether detailed output is wanted.
*
*  FIRST         must be .TRUE. on the first entry. It is subsequently
*                altered by SRCHC.
*
*  MFSRCH        is an upper limit on the number of times SRCHC is to be
*                entered consecutively with DONE = .FALSE. (following
*                an initial entry with FIRST = .TRUE..
*
*  ALFA          is the first estimate of the step length.  ALFA is
*                subsequently altered by SRCHC (see below).
*
*  ALFMAX        is the upper limit of the interval to be searched.
*
*  EPSAF         is an estimate of the absolute precision in the
*                computed value of F.
*
*  ETA           controls the accuracy of the search.  It must lie
*                in the range   0.0  le  ETA  lt  1.0.  Decreasing
*                ETA  tends to increase the accuracy of the search.
*
*  FTRY, GTRY    are the values of F, G  at the new point
*                ALFA = ALFBST + XTRY.
*
*  OLDF, OLDG    are the values of F(0) and G(0). OLDG must be negative.
*
*  TOLABS,TOLREL define a function TOL(ALFA) = TOLREL*ALFA + TOLABS such
*                that if F has already been evaluated at step ALFA,  it
*                will not be evaluated closer than TOL(ALFA).  These
*                values may be reduced by SRCHC.
*
*  TOLTNY        is the smallest value that TOLABS is allowed to be
*                reduced to.
*
*
*  Output parameters (relevant to the calling program)
*  ---------------------------------------------------
*
*  IMPRVD        is true if the previous step ALFA was the best point
*                so far.  Any related quantities should be saved by the
*                calling program (e.g., gradient arrays) before paying
*                attention to DONE.
*
*  DONE = FALSE  means the calling program should evaluate
*                      FTRY = F(ALFA),  GTRY = G(ALFA)
*                for the new trial step ALFA, and then re-enter SRCHC.
*
*  DONE = TRUE   means that no new steplength was calculated.  The value
*                of INFORM gives the result of the linesearch as follows
*
*                INFORM = 1 means the search has terminated successfully
*                           with ALFBST less than ALFMAX.
*
*                INFORM = 2 means the search has terminated successfully
*                           with ALFBST = ALFMAX.
*
*                INFORM = 3 means that the search failed to find a point
*                           of sufficient decrease in MFSRCH functions,
*                           but an improved point was found.
*
*                INFORM = 4 means ALFMAX is so small that a search
*                           should not have been attempted.
*
*                INFORM = 5 is never set by SRCHC.
*
*                INFORM = 6 means the search has failed to find a useful
*                           step.  If the function and gradient have
*                           been programmed correctly, this will usually
*                           occur if the minimum lies very close to
*                           ALFA = 0 or the gradient is not sufficiently
*                           accurate.
*
*  NFSRCH        counts the number of times SRCHC has been entered
*                consecutively with DONE = FALSE (i.e., with a new
*                function value FTRY).
*
*  ALFA          is the step at which the next function FTRY and
*                gradient GTRY must be computed.
*
*  ALFBST        should be accepted by the calling program as the
*                required step-length estimate, whenever SRCHC returns
*                INFORM = 1 or 2 (and possibly 3).
*
*  FBEST, GBEST  will be the corresponding values of F, G.
*
*
*  The following parameters retain information between entries
*  -----------------------------------------------------------
*
*  ALFUZZ        is such that, if the final ALFA lies in the interval
*                (0,ALFUZZ)  and  ABS( F(ALFA)-OLDF ).LE.EPSAF,  ALFA
*                cannot be guaranteed to be a point of sufficient
*                decrease.
*
*  BRAKTD        is false if F and G have not been evaluated at
*                the far end of the interval of uncertainty.  In this
*                case, the point B will be at ALFMAX + TOL(ALFMAX).
*
*  CRAMPD        is true if ALFMAX is very small (le TOLABS).  If the
*                search fails, this indicates that a zero step should be
*                taken.
*
*  EXTRAP        is true if XW lies outside the interval of uncertainty.
*                In this case, extra safeguards are applied to allow for
*                instability in the polynomial fit.
*
*  MOVED         is true if a better point has been found (ALFBST GT 0).
*
*  WSET          records whether a second-best point has been determined
*                It will always be true when convergence is tested.
*
*  NSAMEA        is the number of consecutive times that the left-hand
*                end of the interval of uncertainty has remained the
*                same.
*
*  NSAMEB        similarly for the right-hand end.
*
*  A, B, ALFBST  define the current interval of uncertainty.
*                The required minimum lies somewhere within the
*                closed interval  (ALFBST + A, ALFBST + B).
*
*  ALFBST        is the best point so far.  It is always at one end of
*                the interval of uncertainty.  Hence we have
*                either  A lt 0,  B = 0  or  A = 0,  B gt 0.
*
*  FBEST, GBEST  are the values of F, G at the point ALFBST.
*
*  FACTOR        controls the rate at which extrapolated estimates of
*                ALFA may expand into the interval of uncertainty.
*                FACTOR is not used if the minimum has been bracketed
*                (i.e., when the variable BRAKTD is true).
*
*  FW, GW        are the values of F, G at the point ALFBST + XW.
*                They are not defined until WSET is true.
*
*  XTRY          is the trial point within the shifted interval (A, B).
*
*  XW            is such that  ALFBST + XW  is the second-best point.
*                It is not defined until  WSET  is true.
*                In some cases,  XW  will replace a previous  XW  that
*                has a lower function but has just been excluded from
*                the interval of uncertainty.
*
*  RMU           controls what is meant by a significant decrease in F.
*                The final F(ALFBST)  should lie on or below the line
*                      L(ALFA)  =  OLDF + ALFA*RMU*OLDG.
*                RMU  should be in the open interval (0, 1/2).
*                The value  RMU = 1.0E-4  is good for most purposes.
*
*  RTMIN         is used to avoid floating-point underflow.  It should
*                be reasonably close to the square root of the smallest
*                representable positive number.
*
*
*  Systems Optimization Laboratory, Stanford University, California.
*  Original version February 1982.  Rev. May 1983.
*  Original F77 version 22-August-1985.
*  This version of SRCHC dated 29-June-1986.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/
      COMMON    /SOL1CM/ NOUT

      EXTERNAL           DNORM
      INTRINSIC          ABS   , SQRT
      LOGICAL            BRAKTD, CRAMPD, EXTRAP, MOVED , WSET
      SAVE               BRAKTD, CRAMPD, EXTRAP, MOVED , WSET

      SAVE               NFSRCH, NSAMEA, NSAMEB
      SAVE               A     , B     , ALFUZZ, FACTOR, RTMIN
      SAVE               XW    , FW    , GW    , TOLMAX

      LOGICAL            CLOSEF, CONV1 , CONV2 , CONVRG
      LOGICAL            FITOK , SETXW , SIGDEC

      PARAMETER        ( ZERO  =0.0D+0, POINT1 =0.1D+0, HALF   =0.5D+0 )
      PARAMETER        ( ONE   =1.0D+0, TWO    =2.0D+0, THREE  =3.0D+0 )
      PARAMETER        ( FIVE  =5.0D+0, TEN    =1.0D+1, ELEVEN =1.1D+1 )
      PARAMETER        ( RMU   =1.0D-4, MFSRCH =15                     )

*     ------------------------------------------------------------------
*     Local variables
*     ===============
*
*     CLOSEF     is true if the new function FTRY is within EPSAF of
*                FBEST (up or down).
*
*     CONVRG     will be set to true if at least one of the convergence
*                conditions holds at ALFBST.
*
*     SIGDEC     says whether FBEST represents a significant decrease in
*                the function, compared to the initial value OLDF.
*  ---------------------------------------------------------------------

      IMPRVD = .FALSE.
      IF (FIRST) THEN
*        ---------------------------------------------------------------
*        First entry.  Initialize various quantities, check input data
*        and prepare to evaluate the function at the initial step ALFA.
*        ---------------------------------------------------------------
         FIRST  = .FALSE.
         RTMIN  = WMACH(6)

         NFSRCH = 0
         ALFBST = ZERO
         FBEST  = OLDF
         GBEST  = OLDG
         CRAMPD = ALFMAX .LE. TOLABS
         DONE   = ALFMAX .LE. TOLTNY  .OR.  OLDG .GE. ZERO
         MOVED  = .FALSE.

         IF (.NOT. DONE) THEN
            BRAKTD = .FALSE.
            EXTRAP = .FALSE.
            WSET   = .FALSE.
            NSAMEA = 0
            NSAMEB = 0
            ALFUZZ = ALFMAX
            IF (TWO*EPSAF .LT. - OLDG*RMU*ALFMAX)
     $         ALFUZZ = - TWO*EPSAF/(RMU*OLDG)

            TOLMAX = TOLABS + TOLREL*ALFMAX
            A      = ZERO
            B      = ALFMAX + TOLMAX
            FACTOR = FIVE
            TOL    = TOLABS
            XTRY   = ALFA
            IF (DEBUG)
     $         WRITE (NOUT, 1000) ALFMAX, OLDF , OLDG  , TOLABS,
     $                            ALFUZZ, EPSAF, TOLREL, CRAMPD
         END IF
      ELSE
*        ---------------------------------------------------------------
*        Subsequent entries. The function has just been evaluated at
*        ALFA = ALFBST + XTRY,  giving FTRY and GTRY.
*        ---------------------------------------------------------------
         NFSRCH = NFSRCH + 1
         NSAMEA = NSAMEA + 1
         NSAMEB = NSAMEB + 1

         IF (.NOT. BRAKTD) THEN
            TOLMAX = TOLABS + TOLREL*ALFMAX
            B      = ALFMAX - ALFBST + TOLMAX
         END IF

*        See if the new step is better.  If ALFA is large enough that
*        FTRY can be distinguished numerically from OLDF,  the function
*        is required to be sufficiently decreased.

         IF (ALFA .LE. ALFUZZ) THEN
            SIGDEC = FTRY - OLDF                 .LE. EPSAF
         ELSE
            SIGDEC = FTRY - OLDF - ALFA*RMU*OLDG .LE. EPSAF
         END IF
         CLOSEF = ABS( FTRY - FBEST ) .LE.    EPSAF
         IMPRVD =    ( FTRY - FBEST ) .LE. (- EPSAF)
         IF (CLOSEF) IMPRVD = ABS( GTRY ) .LE. ABS( GBEST )
         IMPRVD = IMPRVD  .AND.  SIGDEC

         IF (DEBUG) WRITE (NOUT, 1100)
     $      ALFA, FTRY, GTRY, FTRY - OLDF - ALFA*RMU*OLDG

         IF (IMPRVD) THEN

*           We seem to have an improvement.  The new point becomes the
*           origin and other points are shifted accordingly.

            FW     = FBEST
            FBEST  = FTRY
            GW     = GBEST
            GBEST  = GTRY
            ALFBST = ALFA
            MOVED  = .TRUE.

            A      = A    - XTRY
            B      = B    - XTRY
            XW     = ZERO - XTRY
            WSET   = .TRUE.
            EXTRAP =       XW .LT. ZERO  .AND.  GBEST .LT. ZERO
     $               .OR.  XW .GT. ZERO  .AND.  GBEST .GT. ZERO

*           Decrease the length of the interval of uncertainty.

            IF (GTRY .LE. ZERO) THEN
               A      = ZERO
               NSAMEA = 0
            ELSE
               B      = ZERO
               NSAMEB = 0
               BRAKTD = .TRUE.
            END IF
         ELSE

*           The new function value is not better than the best point so
*           far.  The origin remains unchanged but the new point may
*           qualify as XW.  XTRY must be a new bound on the best point.

            IF (XTRY .LE. ZERO) THEN
               A      = XTRY
               NSAMEA = 0
            ELSE
               B      = XTRY
               NSAMEB = 0
               BRAKTD = .TRUE.
            END IF

*           If XW has not been set or FTRY is better than FW, update the
*           points accordingly.

            SETXW = .TRUE.
            IF (WSET)
     $         SETXW = FTRY .LE. FW + EPSAF  .OR.  .NOT. EXTRAP

            IF (SETXW) THEN
               XW     = XTRY
               FW     = FTRY
               GW     = GTRY
               WSET   = .TRUE.
               EXTRAP = .FALSE.
            END IF
         END IF

*        ---------------------------------------------------------------
*        Check the termination criteria.  WSET will always be true.
*        ---------------------------------------------------------------
         TOL    = TOLABS + TOLREL*ALFBST

         IF (ALFBST .LE. ALFUZZ) THEN
            SIGDEC = FBEST - OLDF                   .LE. EPSAF
         ELSE
            SIGDEC = FBEST - OLDF - ALFBST*RMU*OLDG .LE. EPSAF
         END IF

         CONV1  = (B - A) .LE. (TOL + TOL)
         CONV2  = MOVED  .AND.  SIGDEC
     $                   .AND.  ABS(GBEST) .LE. ETA*ABS(OLDG)
         CONVRG = CONV1  .OR.   CONV2

         IF (DEBUG) WRITE (NOUT, 1200)
     $      ALFBST + A, ALFBST + B, B - A, TOL,
     $      NSAMEA, NSAMEB, BRAKTD, CLOSEF,
     $      IMPRVD, CONV1 , CONV2 , EXTRAP,
     $      ALFBST, FBEST , GBEST , FBEST - OLDF - ALFBST*RMU*OLDG,
     $      ALFBST + XW, FW, GW

         IF (NFSRCH .GE. MFSRCH) THEN
            DONE = .TRUE.
         ELSE IF (CONVRG) THEN
            IF (MOVED) THEN
               DONE = .TRUE.
            ELSE

*              A better point has not yet been found (the step XW is no
*              better than step zero).  Check that the change in F is
*              consistent with an X-perturbation of TOL,  the minimum
*              spacing estimate.  If not, the value of TOL is reduced.
*              F is larger than EPSAF, the value of TOL is reduced.

               TOL    = TOL/TEN
               TOLABS = TOL
               IF (ABS(FW - OLDF) .LE. EPSAF  .OR.  TOL .LE. TOLTNY)
     $            DONE = .TRUE.
            END IF
         END IF

*        ---------------------------------------------------------------
*        Proceed with the computation of a trial step length.
*        The choices are...
*        1. Parabolic fit using gradients only, if the F values are
*           close.
*        2. Cubic fit for a minimum, using both function and gradients.
*        3. Damped cubic or parabolic fit if the regular fit appears to
*           be consistently over-estimating the distance to the minimum.
*        4. Bisection, geometric bisection, or a step of  TOL  if
*           choices 2 or 3 are unsatisfactory.
*        ---------------------------------------------------------------
         IF (.NOT. DONE) THEN
            XMIDPT = HALF*(A + B)
            S      = ZERO
            Q      = ZERO

            IF (CLOSEF) THEN
*              ---------------------------------------------------------
*              Fit a parabola to the two best gradient values.
*              ---------------------------------------------------------
               S      = GBEST
               Q      = GBEST - GW
               IF (DEBUG) WRITE (NOUT, 2200)
            ELSE
*              ---------------------------------------------------------
*              Fit cubic through  FBEST  and  FW.
*              ---------------------------------------------------------
               IF (DEBUG) WRITE (NOUT, 2100)
               FITOK  = .TRUE.
               R      = THREE*(FBEST - FW)/XW + GBEST + GW
               ABSR   = ABS( R )
               S      = SQRT( ABS( GBEST ) ) * SQRT( ABS( GW ) )
               IF (S .LE. RTMIN) THEN
                  Q   = ABSR
               ELSE

*                 Compute  Q =  the square root of  R*R - GBEST*GW.
*                 The method avoids unnecessary underflow and overflow.

                  IF ((GW .LT. ZERO  .AND.  GBEST .GT. ZERO) .OR.
     $                (GW .GT. ZERO  .AND.  GBEST .LT. ZERO)) THEN
                     SUMSQ  = ONE
                     IF (ABSR .LT. S) THEN
                        IF (ABSR .GE. S*RTMIN) SUMSQ = ONE + (ABSR/S)**2
                        SCALE  = S
                     ELSE
                        IF (S .GE. ABSR*RTMIN) SUMSQ = ONE + (S/ABSR)**2
                        SCALE  = ABSR
                     END IF
                     Q     = DNORM ( SCALE, SUMSQ )
                  ELSE IF (ABSR .GE. S) THEN
                     Q     = SQRT(ABSR + S)*SQRT(ABSR - S)
                  ELSE
                     FITOK  = .FALSE.
                  END IF

               END IF

               IF (FITOK) THEN

*                 Compute the minimum of the fitted cubic.

                  IF (XW .LT. ZERO) Q = - Q
                  S  = GBEST -  R - Q
                  Q  = GBEST - GW - Q - Q
               END IF
            END IF

*           ------------------------------------------------------------
*           Construct an artificial interval  (ARTIFA, ARTIFB)  in which
*           the new estimate of the step length must lie.  Set a default
*           value of XTRY that will be used if the polynomial fit fails.
*           ------------------------------------------------------------
            ARTIFA = A
            ARTIFB = B
            IF (.NOT. BRAKTD) THEN

*              The minimum has not been bracketed.  Set an artificial
*              upper bound by expanding the interval  XW  by a suitable
*              FACTOR.

               XTRY   = - FACTOR*XW
               ARTIFB =   XTRY
               IF (ALFBST + XTRY .LT. ALFMAX) FACTOR = FIVE*FACTOR

            ELSE IF (EXTRAP) THEN

*              The points are configured for an extrapolation.
*              Set a default value of  XTRY  in the interval  (A,B)
*              that will be used if the polynomial fit is rejected.  In
*              the following,  DTRY  and  DAUX  denote the lengths of
*              the intervals  (A,B)  and  (0,XW)  (or  (XW,0),  if
*              appropriate).  The value of  XTRY is the point at which
*              the exponents of  DTRY  and  DAUX  are approximately
*              bisected.

               DAUX = ABS( XW )
               DTRY = B - A
               IF (DAUX .GE. DTRY) THEN
                  XTRY = FIVE*DTRY*(POINT1 + DTRY/DAUX)/ELEVEN
               ELSE
                  XTRY = HALF * SQRT( DAUX ) * SQRT( DTRY )
               END IF
               IF (XW .GT. ZERO)   XTRY = - XTRY
               IF (DEBUG) WRITE (NOUT, 2400) XTRY, DAUX, DTRY

*              Reset the artificial bounds.  If the point computed by
*              extrapolation is rejected,  XTRY will remain at the
*              relevant artificial bound.

               IF (XTRY .LE. ZERO) ARTIFA = XTRY
               IF (XTRY .GT. ZERO) ARTIFB = XTRY
            ELSE

*              The points are configured for an interpolation.  The
*              default value XTRY bisects the interval of uncertainty.
*              The artificial interval is just (A,B).

               XTRY   = XMIDPT
               IF (DEBUG) WRITE (NOUT, 2300) XTRY
               IF (NSAMEA .GE. 3  .OR.  NSAMEB .GE. 3) THEN

*                 If the interpolation appears to be over-estimating the
*                 distance to the minimum,  damp the interpolation step.

                  FACTOR = FACTOR / FIVE
                  S      = FACTOR * S
               ELSE
                  FACTOR = ONE
               END IF
            END IF

*           ------------------------------------------------------------
*           The polynomial fits give  (S/Q)*XW  as the new step.
*           Reject this step if it lies outside  (ARTIFA, ARTIFB).
*           ------------------------------------------------------------
            IF (Q .NE. ZERO) THEN
               IF (Q .LT. ZERO) S = - S
               IF (Q .LT. ZERO) Q = - Q
               IF (S*XW .GE. Q*ARTIFA  .AND.  S*XW .LE. Q*ARTIFB) THEN

*                 Accept the polynomial fit.

                  XTRY = ZERO
                  IF (ABS( S*XW ) .GE. Q*TOL) XTRY = (S/Q)*XW
                  IF (DEBUG) WRITE (NOUT, 2500) XTRY
               END IF
            END IF
         END IF
      END IF

*     ==================================================================

      IF (.NOT. DONE) THEN
         ALFA  = ALFBST + XTRY
         IF (BRAKTD  .OR.  ALFA .LT. ALFMAX - TOLMAX) THEN

*           The function must not be evaluated too close to A or B.
*           (It has already been evaluated at both those points.)

            IF (XTRY .LE. A + TOL  .OR.  XTRY .GE. B - TOL) THEN
               XTRY = TOL
               IF (HALF*(A + B) .LE. ZERO) XTRY = - TOL
               ALFA = ALFBST + XTRY
            END IF

         ELSE

*           The step is close to or larger than ALFMAX, replace it by
*           ALFMAX to force evaluation of the function at the boundary.

            BRAKTD = .TRUE.
            XTRY   = ALFMAX - ALFBST
            ALFA   = ALFMAX

         END IF
      END IF

*     ------------------------------------------------------------------
*     Exit.
*     ------------------------------------------------------------------
      IF (DONE) THEN
         IF (MOVED) THEN
            IF (CONVRG) THEN
               INFORM = 1
               IF (ALFA .EQ. ALFMAX) INFORM = 2
            ELSE
               INFORM = 3
            END IF
         ELSE IF (OLDG .GE. ZERO  .OR.  ALFMAX .LT. TOLTNY) THEN
            INFORM = 7
         ELSE
            INFORM = 6
            IF (CRAMPD) INFORM = 4
         END IF
      END IF


      IF (DEBUG) WRITE (NOUT, 3000)
      RETURN

 1000 FORMAT(/' ALFMAX  OLDF    OLDG    TOLABS', 1P2E22.14,   1P2E16.8
     $       /' ALFUZZ  EPSAF           TOLREL', 1P2E22.14,16X,1PE16.8
     $       /' CRAMPD                        ',  L6)
 1100 FORMAT(/' ALFA    FTRY    GTRY    CTRY  ', 1P2E22.14,   1P2E16.8)
 1200 FORMAT(/' A       B       B - A   TOL   ', 1P2E22.14,   1P2E16.8
     $       /' NSAMEA  NSAMEB  BRAKTD  CLOSEF', 2I3, 2L6
     $       /' IMPRVD  CONVRG  EXTRAP        ',  L6, 3X, 2L1, L6
     $       /' ALFBST  FBEST   GBEST   CBEST ', 1P2E22.14,   1P2E16.8
     $       /' ALFAW   FW      GW            ', 1P2E22.14,    1PE16.8/)
 2100 FORMAT( ' Cubic.   ')
 2200 FORMAT( ' Parabola.')
 2300 FORMAT( ' Bisection.              XMIDPT', 1P1E22.14)
 2400 FORMAT( ' Geo. bisection. XTRY,DAUX,DTRY', 1P3E22.14)
 2500 FORMAT( ' Polynomial fit accepted.  XTRY', 1P1E22.14)
 3000 FORMAT( ' ----------------------------------------------------'/)

*     End of  SRCHC .

      END
