*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE SRCHQ ( DEBUG, DONE, FIRST, IMPRVD, INFORM,
     $                   ALFMAX, ALFSML, EPSAF, ETA,
     $                   XTRY, FTRY, OLDF, OLDG,
     $                   TOLABS, TOLREL, TOLTNY,
     $                   ALFA, ALFBST, FBEST )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            DEBUG , DONE  , FIRST , IMPRVD

************************************************************************
*  SRCHQ  is a step-length algorithm for minimizing a function of one
*  variable.  It will be called repeatedly by a search routine whose
*  purpose is to estimate a point ALFA = ALFBST that minimizes some
*  function F(ALFA) over the closed interval (0, ALFMAX).
*
*  SRCHQ  requires the function F(ALFA) (but not its gradient) to be
*  evaluated at various points within the interval.  New steplength
*  estimates are computed using quadratic interpolation with safeguards.
*
*  Reverse communication is used to allow the calling program to
*  evaluate F.  Some of the parameters must be set or tested by the
*  calling program.  The remainder would ordinarily be local variables.
*
*
*  Input parameters (relevant to the calling program)
*  --------------------------------------------------
*
*  DEBUG         specifies whether detailed output is wanted.
*
*  FIRST         must be .TRUE. on the first entry. It is subsequently
*                altered by SRCHQ.
*
*  MFSRCH        is an upper limit on the number of times SRCHQ is to be
*                entered consecutively with DONE = .FALSE. (following
*                an initial entry with FIRST = .TRUE.).
*
*  ALFA          is the first estimate of the steplength.  ALFA is
*                subsequently altered by SRCHQ (see below).
*
*  ALFMAX        is the upper limit of the interval to be searched.
*
*  ALFSML        is intended to prevent inefficiency when the optimum
*                step is very small, for cases where the calling program
*                would prefer to re-define F(ALFA).  ALFSML is allowed
*                to be zero. Early termination will occur if SRCHQ
*                determines that the optimum step lies somewhere in the
*                interval (0, ALFSML) (but not if ALFMAX .LE. ALFSML).
*
*  EPSAF         is an estimate of the absolute precision in the
*                computed value of F.
*
*  ETA           controls the accuracy of the search.  It must lie
*                in the range  0 .LE. ETA .LT. 1.  Decreasing ETA tends
*                to increase the accuracy of the search.
*
*  FTRY          the value of F at the new point ALFA = ALFBST + XTRY.
*
*  OLDF, OLDG    are the values of F(0) and G(0). OLDG must be negative.
*
*  TOLABS,TOLREL define a function  TOL(ALFA) = TOLREL*ALFA + TOLABS
*                such that if F has already been evaluated at step ALFA,
*                then it will not be evaluated at any point closer than
*                TOL(ALFA).  These values may be reduced by SRCHQ if
*                they seem to be too large.
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
*                calling program (e.g., arrays) before paying attention
*                to DONE.
*
*  DONE = FALSE  means the calling program should evaluate FTRY for the
*                new trial step ALFA, and then re-enter SRCHQ.
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
*                INFORM = 5 means that the search was terminated because
*                           of ALFSML (see above).
*
*                INFORM = 6 means the search has failed to find a useful
*                           step.  If the function has been programmed
*                           correctly, this will usually occur if the
*                           minimum lies very close to ALFA = 0.
*
*  NFSRCH        counts the number of times SRCHQ has been entered
*                consecutively with DONE = FALSE (i.e., with a new
*                function value FTRY).
*
*  ALFA          is the step at which the next function FTRY must be
*                computed.
*
*  ALFBST        should be accepted by the calling program as the
*                required steplength estimate, whenever SRCHQ returns
*                INFORM = 1, 2 or 3.
*
*  FBEST         will be the corresponding value of F.
*
*
*  The following parameters retain information between entries
*  -----------------------------------------------------------
*
*  ALFUZZ        is such that, if the final ALFA lies in the interval
*                (0,ALFUZZ) and ABS( F(ALFA)-OLDF ) .LE. EPSAF,  ALFA
*                cannot be guaranteed to be a point of sufficient
*                decrease.
*
*  BRAKTD        is false if F has not been evaluated at the far end
*                of the interval of uncertainty.  In this case, the
*                point B will be at ALFMAX + TOL(ALFMAX).
*
*  CRAMPD        is true if ALFMAX is very small (.LE. TOLABS).  If the
*                search fails, this indicates that a zero step should
*                be taken.
*
*  EXTRAP        is true if ALFBST has moved at least once and XV lies
*                outside the interval of uncertainty.  In this case,
*                extra safeguards are applied to allow for instability
*                in the polynomial fit.
*
*  MOVED         is true if a better point has been found (ALFBST GT 0).
*
*  VSET          records whether a third-best point has been defined.
*
*  WSET          records whether a second-best point has been defined.
*                It will always be true by the time the convergence
*                test is applied.
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
*  ALFBST        is the best point so far.  it is strictly within the
*                the interval of uncertainty except when it lies at the
*                left-hand end when ALFBST has not been moved.
*                Hence we have A .LE. 0 and B .GT. 0.
*
*  FBEST         is the value of F at the point ALFBST.
*
*  FA            is the value of F at the point ALFBST + A.
*
*  FACTOR        controls the rate at which extrapolated estimates of
*                ALFA  may expand into the interval of uncertainty.
*                FACTOR is not used if the minimum has been bracketed
*                (i.e., when the variable BRAKTD is true).
*
*  FV, FW        are the values of F at the points ALFBST + XV,
*                ALFBST + XW.  They are not defined until VSET or WSET
*                are true.
*
*  XTRY          is the trial point within the shifted interval (A, B).
*                The new trial function value must be computed at the
*                point ALFA = ALFBST + XTRY.
*
*  XV            is such that ALFBST + XV is the third-best point. It is
*                not defined until VSET is true.
*
*  XW            is such that ALFBST + XW is the second-best point. It
*                is not defined until WSET is true.  In some cases, XW
*                will replace a previous XW that has a lower function
*                but has just been excluded from (A,B).
*
*  RMU           controls what is meant by a significant decrease in F.
*                The final F(ALFBST)  should lie on or below the line
*                      L(ALFA)  =  OLDF + ALFA*RMU*OLDG.
*                RMU  should be in the open interval (0, 1/2).
*                The value  RMU = 1.0E-4  is good for most purposes.
*
*
*  Systems Optimization Laboratory, Stanford University, California.
*  Original version February 1982.  Rev. May 1983.
*  Original F77 version 22-August-1985.
*  This version of SRCHQ dated 30-July-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            BRAKTD, CRAMPD, EXTRAP, MOVED , VSET  , WSET
      SAVE               BRAKTD, CRAMPD, EXTRAP, MOVED , VSET  , WSET

      SAVE               NFSRCH, NSAMEA, NSAMEB
      SAVE               A     , B     , FA    , ALFUZZ, FACTOR
      SAVE               XW    , FW    , XV    , FV    , TOLMAX

      LOGICAL            CLOSEF, CONV1 , CONV2 , CONV3 , CONVRG
      LOGICAL            SETXV , SIGDEC, XINXW
      INTRINSIC          ABS   , SQRT

      PARAMETER        ( ZERO  =0.0D+0, POINT1 =0.1D+0, HALF   =0.5D+0 )
      PARAMETER        ( ONE   =1.0D+0, TWO    =2.0D+0, FIVE   =5.0D+0 )
      PARAMETER        ( TEN   =1.0D+1, ELEVEN =1.1D+1                 )
      PARAMETER        ( RMU   =1.0D-4, MFSRCH =15                     )

*     ------------------------------------------------------------------
*     Local variables
*     ===============
*
*     CLOSEF     is true if the worst function FV is within EPSAF of
*                FBEST (up or down).
*
*     CONVRG     will be set to true if at least one of the convergence
*                conditions holds at ALFBST.
*
*     SIGDEC     says whether FBEST represents a significant decrease
*             in the function, compared to the initial value OLDF.
*
*     XINXW      is true if XTRY is in (XW,0) or (0,XW).
*     ------------------------------------------------------------------

      IMPRVD = .FALSE.
      IF (FIRST) THEN
*        ---------------------------------------------------------------
*        First entry.  Initialize various quantities, check input data
*        and prepare to evaluate the function at the initial step ALFA.
*        ---------------------------------------------------------------
         FIRST  = .FALSE.
         NFSRCH = 0
         ALFBST = ZERO
         FBEST  = OLDF
         CRAMPD = ALFMAX .LE. TOLABS
         DONE   = ALFMAX .LE. TOLTNY  .OR.  OLDG .GE. ZERO
         MOVED  = .FALSE.

         IF (.NOT. DONE) THEN
            BRAKTD = .FALSE.
            CRAMPD = ALFMAX .LE. TOLABS
            EXTRAP = .FALSE.
            VSET   = .FALSE.
            WSET   = .FALSE.
            NSAMEA = 0
            NSAMEB = 0
            ALFUZZ = ALFMAX
            IF (TWO*EPSAF .LT. - OLDG*RMU*ALFMAX)
     $         ALFUZZ = - TWO*EPSAF/(RMU*OLDG)

            TOLMAX = TOLREL*ALFMAX + TOLABS
            A      = ZERO
            B      = ALFMAX + TOLMAX
            FA     = OLDF
            FACTOR = FIVE
            TOL    = TOLABS
            XTRY   = ALFA
            IF (DEBUG)
     $         WRITE (NOUT, 1000) ALFMAX, OLDF , OLDG  , TOLABS,
     $                            ALFUZZ,         EPSAF, TOLREL,
     $                            CRAMPD
         END IF
      ELSE
*        ---------------------------------------------------------------
*        Subsequent entries.  The function has just been evaluated at
*        ALFA = ALFBST + XTRY,  giving FTRY.
*        ---------------------------------------------------------------
         NFSRCH = NFSRCH + 1
         NSAMEA = NSAMEA + 1
         NSAMEB = NSAMEB + 1

         IF (.NOT. BRAKTD) THEN
            TOLMAX = TOLABS + TOLREL*ALFMAX
            B      = ALFMAX - ALFBST + TOLMAX
         END IF

*        Check if XTRY is in the interval (XW,0) or (0,XW).

         XINXW  = .FALSE.
         IF (WSET) XINXW =       ZERO .LT. XTRY  .AND.  XTRY .LE. XW
     $                     .OR.    XW .LE. XTRY  .AND.  XTRY .LT. ZERO

*        See if the new step is better.

         IF (ALFA .LE. ALFUZZ) THEN
            SIGDEC = FTRY - OLDF                 .LE. (- EPSAF)
         ELSE
            SIGDEC = FTRY - OLDF - ALFA*RMU*OLDG .LE.    EPSAF
         END IF
         IMPRVD = SIGDEC  .AND.  (FTRY .LE. FBEST - EPSAF)

         IF (DEBUG) WRITE (NOUT, 1100)
     $      ALFA, FTRY, FTRY - OLDF - ALFA*RMU*OLDG

         IF (IMPRVD) THEN

*           We seem to have an improvement.  The new point becomes the
*           origin and other points are shifted accordingly.

            IF (WSET) THEN
               XV     = XW - XTRY
               FV     = FW
               VSET   = .TRUE.
            END IF

            XW     = ZERO - XTRY
            FW     = FBEST
            WSET   = .TRUE.
            FBEST  = FTRY
            ALFBST = ALFA
            MOVED  = .TRUE.

            A      = A    - XTRY
            B      = B    - XTRY
            EXTRAP = .NOT. XINXW

*           Decrease the length of (A,B).

            IF (XTRY .GE. ZERO) THEN
               A      = XW
               FA     = FW
               NSAMEA = 0
            ELSE
               B      = XW
               NSAMEB = 0
               BRAKTD = .TRUE.
            END IF
         ELSE

*           The new function value is no better than the current best
*           point.  XTRY must an end point of the new (A,B).

            IF (XTRY .LT. ZERO) THEN
               A      = XTRY
               FA     = FTRY
               NSAMEA = 0
            ELSE
               B      = XTRY
               NSAMEB = 0
               BRAKTD = .TRUE.
            END IF

*           The origin remains unchanged but XTRY may qualify as XW.

            IF (WSET) THEN
               IF (FTRY .LE. FW + EPSAF) THEN
                  XV     = XW
                  FV     = FW
                  VSET   = .TRUE.

                  XW     = XTRY
                  FW     = FTRY
                  IF (MOVED) EXTRAP = XINXW
               ELSE IF (MOVED) THEN
                  SETXV = .TRUE.
                  IF (VSET)
     $               SETXV = FTRY .LE. FV + EPSAF  .OR.  .NOT. EXTRAP

                  IF (SETXV) THEN
                     IF (VSET  .AND.  XINXW) THEN
                        XW = XV
                        FW = FV
                     END IF
                     XV = XTRY
                     FV = FTRY
                     VSET = .TRUE.
                  END IF
               ELSE
                  XW = XTRY
                  FW = FTRY
               END IF
            ELSE
               XW     = XTRY
               FW     = FTRY
               WSET   = .TRUE.
            END IF
         END IF

*        ---------------------------------------------------------------
*        Check the termination criteria.
*        ---------------------------------------------------------------
         TOL    = TOLABS + TOLREL*ALFBST

         IF (ALFBST .LE. ALFUZZ) THEN
            SIGDEC = FBEST - OLDF                   .LE. (- EPSAF)
         ELSE
            SIGDEC = FBEST - OLDF - ALFBST*RMU*OLDG .LE.    EPSAF
         END IF
         CLOSEF = .FALSE.
         IF (VSET) CLOSEF = ABS( FBEST - FV ) .LE. EPSAF

         CONV1  =  MAX( ABS( A ), B )  .LE.  (TOL + TOL)
         CONV2  =  MOVED  .AND.  SIGDEC
     $                    .AND.  ABS( FA - FBEST )  .LE.  A*ETA*OLDG
         CONV3  = CLOSEF  .AND.  (SIGDEC  .OR.
     $                           (.NOT. MOVED)  .AND.  (B .LE. ALFUZZ))
         CONVRG = CONV1  .OR.  CONV2  .OR.  CONV3

         IF (DEBUG) THEN
            WRITE (NOUT, 1200) ALFBST + A, ALFBST + B, B - A, TOL,
     $         NSAMEA, NSAMEB, BRAKTD, CLOSEF,
     $         IMPRVD, CONV1, CONV2, CONV3, EXTRAP,
     $         ALFBST, FBEST,  FBEST - OLDF - ALFBST*RMU*OLDG,
     $         ALFBST + XW, FW
            IF (VSET)
     $         WRITE (NOUT, 1300) ALFBST + XV, FV
         END IF

         IF (NFSRCH .GE. MFSRCH  .OR.  ALFBST + B .LE. ALFSML) THEN
            DONE = .TRUE.
         ELSE IF (CONVRG) THEN
            IF (MOVED) THEN
               DONE = .TRUE.
            ELSE

*              A better point has not yet been found (the step XW is no
*              better than step zero).  Check that the change in F is
*              consistent with an X-perturbation of TOL,  the minimum
*              spacing estimate.  If not, the value of TOL is reduced.

               TOL    = TOL/TEN
               TOLABS = TOL
               IF (ABS(FW - OLDF) .LE. EPSAF  .OR.  TOL .LE. TOLTNY)
     $            DONE = .TRUE.
            END IF
         END IF

*        ---------------------------------------------------------------
*        Proceed with the computation of a trial step length.
*        The choices are...
*        1. Parabolic fit using function values only.
*        2. Damped parabolic fit if the regular fit appears to be
*           consistently over-estimating the distance to the minimum.
*        3. Bisection, geometric bisection, or a step of TOL if the
*           parabolic fit is unsatisfactory.
*        ---------------------------------------------------------------
         XMIDPT = HALF*(A + B)
         S      = ZERO
         Q      = ZERO

*        ===============================================================
*        Fit a parabola.
*        ===============================================================
*        Check if there are two or three points for the parabolic fit.

         GW = (FW - FBEST)/XW
         IF (VSET  .AND.  MOVED) THEN

*           Three points available.  Use FBEST, FW and FV.

            GV = (FV - FBEST)/XV
            S  = GV - (XV/XW)*GW
            Q  = TWO*(GV - GW)
            IF (DEBUG) WRITE (NOUT, 2200)
         ELSE

*           Only two points available.  Use FBEST, FW and OLDG.

            IF (MOVED) THEN
               S  = OLDG - TWO*GW
            ELSE
               S  = OLDG
            END IF
            Q = TWO*(OLDG - GW)
            IF (DEBUG) WRITE (NOUT, 2100)
         END IF

*        ---------------------------------------------------------------
*        Construct an artificial interval (ARTIFA, ARTIFB) in which the
*        new estimate of the steplength must lie.  Set a default value
*        of XTRY that will be used if the polynomial fit is rejected.
*        In the following, the interval (A,B) is considered the sum of
*        two intervals of lengths DTRY and DAUX, with common end point
*        the best point (zero).  DTRY is the length of the interval into
*        which the default XTRY will be placed and ENDPNT denotes its
*        non-zero end point.  The magnitude of XTRY is computed so that
*        the exponents of DTRY and DAUX are approximately bisected.
*        ---------------------------------------------------------------
         ARTIFA = A
         ARTIFB = B
         IF (.NOT. BRAKTD) THEN

*           The minimum has not been bracketed.  Set an artificial upper
*           bound by expanding the interval XW by a suitable factor.

            XTRY   = - FACTOR*XW
            ARTIFB =   XTRY
            IF (ALFBST + XTRY .LT. ALFMAX) FACTOR = FIVE*FACTOR
         ELSE IF (VSET .AND. MOVED) THEN

*           Three points exist in the interval of uncertainty.
*           Check if the points are configured for an extrapolation
*           or an interpolation.

            IF (EXTRAP) THEN

*              The points are configured for an extrapolation.

               IF (XW .LT. ZERO) ENDPNT = B
               IF (XW .GT. ZERO) ENDPNT = A
            ELSE

*              If the interpolation appears to be over-estimating the
*              distance to the minimum,  damp the interpolation step.

               IF (NSAMEA .GE. 3  .OR.   NSAMEB .GE. 3) THEN
                  FACTOR = FACTOR / FIVE
                  S      = FACTOR * S
               ELSE
                  FACTOR = ONE
               END IF

*              The points are configured for an interpolation.  The
*              artificial interval will be just (A,B).  Set ENDPNT so
*              that XTRY lies in the larger of the intervals (A,B) and
*              (0,B).

               ENDPNT = A
               IF (XMIDPT .GT. ZERO) ENDPNT = B

*              If a bound has remained the same for three iterations,
*              set ENDPNT so that  XTRY  is likely to replace the
*              offending bound.

               IF (NSAMEA .GE. 3) ENDPNT = A
               IF (NSAMEB .GE. 3) ENDPNT = B
            END IF

*           Compute the default value of  XTRY.

            DTRY = ABS( ENDPNT )
            DAUX = B - A - DTRY
            IF (DAUX .GE. DTRY) THEN
               XTRY = FIVE*DTRY*(POINT1 + DTRY/DAUX)/ELEVEN
            ELSE
               XTRY = HALF*SQRT( DAUX )*SQRT( DTRY )
            END IF
            IF (ENDPNT .LT. ZERO) XTRY = - XTRY
            IF (DEBUG) WRITE (NOUT, 2500) XTRY, DAUX, DTRY

*           If the points are configured for an extrapolation set the
*           artificial bounds so that the artificial interval lies
*           within (A,B).  If the polynomial fit is rejected,  XTRY will
*           remain at the relevant artificial bound.

            IF (EXTRAP) THEN
               IF (XTRY .LE. ZERO) THEN
                  ARTIFA = XTRY
               ELSE
                  ARTIFB = XTRY
               END IF
            END IF
         ELSE

*           The gradient at the origin is being used for the polynomial
*           fit.  Set the default XTRY to one tenth XW.

            XTRY   = XW/TEN
            IF (EXTRAP) XTRY = - XW
            IF (DEBUG) WRITE (NOUT, 2400) XTRY
         END IF

*        ---------------------------------------------------------------
*        The polynomial fits give (S/Q)*XW as the new step.  Reject this
*        step if it lies outside (ARTIFA, ARTIFB).
*        ---------------------------------------------------------------
         IF (Q .NE. ZERO) THEN
            IF (Q .LT. ZERO) S = - S
            IF (Q .LT. ZERO) Q = - Q
            IF (S*XW .GE. Q*ARTIFA   .AND.   S*XW .LE. Q*ARTIFB) THEN

*              Accept the polynomial fit.

               XTRY = ZERO
               IF (ABS( S*XW ) .GE. Q*TOL) XTRY = (S/Q)*XW
               IF (DEBUG) WRITE (NOUT, 2600) XTRY
            END IF
         END IF
      END IF

*     ==================================================================

      IF (.NOT. DONE) THEN
         ALFA  = ALFBST + XTRY
         IF (BRAKTD  .OR.  ALFA .LT. ALFMAX - TOLMAX) THEN

*           The function must not be evaluated too close to A or B.
*           (It has already been evaluated at both those points.)

            XMIDPT = HALF*(A + B)
            IF (XTRY .LE. A + TOL  .OR.  XTRY .GE. B - TOL) THEN
               XTRY = TOL
               IF (XMIDPT .LE. ZERO) XTRY = - TOL
            END IF

            IF (ABS( XTRY ) .LT. TOL) THEN
               XTRY = TOL
               IF (XMIDPT .LE. ZERO) XTRY = - TOL
            END IF
            ALFA  = ALFBST + XTRY
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
            ELSE IF (ALFBST + B .LT. ALFSML) THEN
               INFORM = 5
            ELSE
               INFORM = 3
            END IF
         ELSE IF (OLDG .GE. ZERO  .OR.  ALFMAX .LT. TOLTNY) THEN
            INFORM = 7
         ELSE IF (CRAMPD) THEN
            INFORM = 4
         ELSE IF (ALFBST + B .LT. ALFSML) THEN
            INFORM = 5
         ELSE
            INFORM = 6
         END IF
      END IF

      IF (DEBUG) WRITE (NOUT, 3000)
      RETURN

 1000 FORMAT(/' ALFMAX  OLDF    OLDG    TOLABS', 1P2E22.14,   1P2E16.8
     $       /' ALFUZZ  EPSAF           TOLREL', 1P2E22.14,16X,1PE16.8
     $       /' CRAMPD                        ',  L6)
 1100 FORMAT(/' ALFA    FTRY    CTRY          ', 1P2E22.14,   1P1E16.8)
 1200 FORMAT(/' A       B       B - A   TOL   ', 1P2E22.14,   1P2E16.8
     $       /' NSAMEA  NSAMEB  BRAKTD  CLOSEF', 2I3, 2L6
     $       /' IMPRVD  CONVRG  EXTRAP        ',  L6, 3X, 3L1, L6
     $       /' ALFBST  FBEST   CBEST         ', 1P2E22.14,   1P1E16.8
     $       /' ALFAW   FW                    ', 1P2E22.14)
 1300 FORMAT( ' ALFAV   FV                    ', 1P2E22.14 /)
 2100 FORMAT( ' Parabolic fit,    two points. ')
 2200 FORMAT( ' Parabolic fit,  three points. ')
 2400 FORMAT( ' Exponent reduced.  Trial point', 1P1E22.14)
 2500 FORMAT( ' Geo. bisection. XTRY,DAUX,DTRY', 1P3E22.14)
 2600 FORMAT( ' Polynomial fit accepted.  XTRY', 1P1E22.14)
 3000 FORMAT( ' ----------------------------------------------------'/)

*     End of  SRCHQ .

      END
