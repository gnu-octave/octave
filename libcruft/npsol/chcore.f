*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  CHSUBS FORTRAN
*
*     CHCORE   CHFD     CHKGRD   CHKJAC
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CHCORE( DEBUG, DONE, FIRST, EPSA, EPSR, FX, X,
     $                   INFORM, ITER, ITMAX,
     $                   CDEST, FDEST, SDEST, ERRBND, F1,
     $                   F2, H, HOPT, HPHI )

      IMPLICIT           DOUBLE PRECISION (A-H,O-Z)
      LOGICAL            DEBUG, DONE, FIRST

************************************************************************
*  CHCORE  implements algorithm  FD, the method described in
*  Gill, P.E., Murray, W., Saunders, M.A., and Wright, M. H.,
*  Computing Forward-Difference Intervals for Numerical Optimization,
*  Siam Journal on Scientific and Statistical Computing, vol. 4,
*  pp. 310-321, June 1983.
*
*  The procedure is based on finding an interval (HPHI) that
*  produces an acceptable estimate of the second derivative, and
*  then using that estimate to compute an interval that should
*  produce a reasonable forward-difference approximation.
*
*  One-sided difference estimates are used to ensure feasibility with
*  respect to an upper or lower bound on X.  If X is close to an upper
*  bound, the trial intervals will be negative.  The final interval is
*  always positive.
*
*  CHCORE has been designed to use a reverse communication
*  control structure, i.e., all evaluations of the function occur
*  outside this routine.  The calling routine repeatedly calls  CHCORE
*  after computing the indicated function values.
*
*  CHCORE  is similar to subroutine FDCORE described in Report
*  SOL 83-6, Documentation of FDCORE and FDCALC, by P.E. Gill,
*  W. Murray,  M.A. Saunders, and M.H. Wright, Department of
*  Operations Research,  Stanford University, Stanford, California
*  94305, June 1983.
*
*  Systems Optimization Laboratory, Stanford University.
*  Based on Fortran 66 Version 2.1 of  FDCORE  written June 1983.
*  Fortran 77 Version written 25-May-1985.
*  This version of  CHCORE  dated  11-February-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            CE1BIG, CE2BIG, TE2BIG, OVERFL
      SAVE               CDSAVE, FDSAVE, HSAVE, OLDH, RHO, SDSAVE
      SAVE               CE1BIG, CE2BIG, TE2BIG
      EXTERNAL           DDIV
      INTRINSIC          ABS   , MAX   , MIN  , SQRT

      PARAMETER         (BNDLO  =1.0D-3, BNDUP  =1.0D-1                )

      PARAMETER         (ZERO   =0.0D+0, SIXTH  =1.6D-1, FOURTH =2.5D-1)
      PARAMETER         (HALF   =5.0D-1, ONE    =1.0D+0, TWO    =2.0D+0)
      PARAMETER         (THREE  =3.0D+0, FOUR   =4.0D+0, TEN    =1.0D+1)

*     ------------------------------------------------------------------
*     Explanation of local variables...
*
*     BNDLO, BNDUP, and RHO control the logic of the routine.
*     BNDLO and BNDUP are the lower and upper bounds that define an
*     acceptable value of the bound on the relative condition error in
*     the second derivative estimate.
*
*     The scalar RHO is the factor by which the interval is multiplied
*     or divided, and also the multiple of the well-scaled interval
*     that is used as the initial trial interval.
*
*     All these values are discussed in the documentation.
*     ------------------------------------------------------------------

      ITER  = ITER + 1

*     Compute the forward-,  backward-,  central-  and second-order
*     difference estimates.

      FDEST  = DDIV  ( F1 - FX,     H, OVERFL )
      FDEST2 = DDIV  ( F2 - FX, TWO*H, OVERFL )

      OLDCD = CDEST
      CDEST = DDIV  ( FOUR*F1 - THREE*FX - F2, TWO*H, OVERFL )

      OLDSD = SDEST
      SDEST = DDIV  ( FX      - TWO*F1   + F2, H*H  , OVERFL )

*     Compute  FDCERR  and  SDCERR,  bounds on the relative condition
*     errors in the first and second derivative estimates.

      AFDMIN = MIN( ABS( FDEST ), ABS( FDEST2 ) )
      FDCERR = DDIV  ( EPSA, HALF*ABS( H )*AFDMIN, OVERFL )
      SDCERR = DDIV  ( EPSA, FOURTH*ABS( SDEST )*H*H, OVERFL )

      IF (DEBUG)
     $   WRITE (NOUT, 9000) ITER  , FX   , H,
     $                      F1    , FDEST,
     $                      F2    , FDEST2,
     $                      CDEST , SDEST,
     $                      FDCERR, SDCERR

*     ==================================================================
*     Select the correct case.
*     ==================================================================
      IF (FIRST) THEN
*        ---------------------------------------------------------------
*        First time through.
*        Check whether SDCERR lies in the acceptable range.
*        ------------------------------------------------------------
         FIRST  = .FALSE.
         DONE   = SDCERR .GE. BNDLO  .AND.  SDCERR .LE. BNDUP
         TE2BIG = SDCERR .LT. BNDLO
         CE2BIG = SDCERR .GT. BNDUP
         CE1BIG = FDCERR .GT. BNDUP

         IF (.NOT. CE1BIG) THEN
            HSAVE  = H
            FDSAVE = FDEST
            CDSAVE = CDEST
            SDSAVE = SDEST
         END IF

         RHO  = EPSR**(-SIXTH)/FOUR
         IF (TE2BIG) THEN

*           The truncation error may be too big  (same as saying
*           SDCERR is too small).  Decrease the trial interval.

            RHO    = TEN*RHO
            OLDH   = H
            H      = H / RHO
         ELSE IF (CE2BIG) THEN

*           SDCERR is too large.  Increase the trial interval.

            OLDH   = H
            H      = H*RHO
         END IF
      ELSE IF (CE2BIG) THEN
*        ---------------------------------------------------------------
*        During the last iteration,  the trial interval was
*        increased in order to decrease SDCERR.
*        ---------------------------------------------------------------
         IF (CE1BIG  .AND.  FDCERR .LE. BNDUP) THEN
            CE1BIG = .FALSE.
            HSAVE  = H
            FDSAVE = FDEST
            CDSAVE = CDEST
            SDSAVE = SDEST
         END IF

*        If SDCERR is small enough, accept H.  Otherwise,
*        increase H again.

         DONE   = SDCERR .LE. BNDUP
         IF (.NOT. DONE) THEN
            OLDH   = H
            H      = H*RHO
         END IF
      ELSE IF (TE2BIG) THEN
*        ---------------------------------------------------------------
*        During the last iteration,  the interval was decreased in order
*        to reduce the truncation error.
*        ---------------------------------------------------------------
         DONE   = SDCERR .GT. BNDUP
         IF (DONE) THEN

*           SDCERR has jumped from being too small to being too
*           large.  Accept the previous value of H.

            H     = OLDH
            SDEST = OLDSD
            CDEST = OLDCD
         ELSE

*           Test whether FDCERR is sufficiently small.

            IF (FDCERR .LE. BNDUP) THEN
               CE1BIG = .FALSE.
               HSAVE  = H
               FDSAVE = FDEST
               CDSAVE = CDEST
               SDSAVE = SDEST
            END IF

*           Check whether SDCERR is in range.

            DONE  = SDCERR .GE. BNDLO

            IF (.NOT. DONE) THEN

*              SDCERR is still too small, decrease H again.

               OLDH = H
               H    = H / RHO
            END IF
         END IF

      END IF

*     ==================================================================
*     We have either finished or have a new estimate of H.
*     ==================================================================
      IF (DONE) THEN

*        Sufficiently good second-derivative estimate found.
*        Compute the optimal interval.

         HPHI   = ABS( H )
         HOPT   = TWO * SQRT( EPSA ) / SQRT( ABS( SDEST ) )

*        ERR1 is the error bound on the forward-difference estimate
*        with the final value of H.  ERR2 is the difference of FDEST
*        and the central-difference estimate with HPHI.

         ERR1   = HOPT*ABS( SDEST )
         ERR2   = ABS( FDEST - CDEST )
         ERRBND = MAX( ERR1, ERR2 )

*        Set INFORM = 4  if the forward- and central-difference
*        estimates are not close.

         INFORM = 0
         IF (ERRBND .GT. HALF*ABS( FDEST )) INFORM = 4
      ELSE
*        ---------------------------------------------------------------
*        Check whether the maximum number of iterations has been
*        exceeded.  If not, exit.
*        ---------------------------------------------------------------
         DONE = ITER .GE. ITMAX
         IF (DONE) THEN
            IF (CE1BIG) THEN

*              FDCERR was never small.  Probably a constant function.

               INFORM = 1
               HPHI   = HOPT
               FDEST  = ZERO
               CDEST  = ZERO
               SDEST  = ZERO
               ERRBND = ZERO
            ELSE IF (CE2BIG) THEN

*              FDCERR was small,  but SDCERR was never small.
*              Probably a linear or odd function.

               INFORM = 2
               HPHI   = ABS( HSAVE )
               HOPT   = HPHI
               FDEST  = FDSAVE
               CDEST  = CDSAVE
               SDEST  = ZERO
               ERRBND = TWO*EPSA / HOPT
            ELSE

*              The only remaining case occurs when the second
*              derivative is changing too rapidly for an adequate
*              interval to be found (SDCERR remained small even
*              though H was decreased ITMAX times).

               INFORM = 3
               HPHI   = ABS( HSAVE )
               HOPT   = HPHI
               FDEST  = FDSAVE
               CDEST  = CDSAVE
               SDEST  = SDSAVE
               ERRBND = HOPT*ABS( SDEST )/TWO + TWO*EPSA/HOPT
            END IF
         END IF
      END IF

      IF (DEBUG) THEN
         WRITE (NOUT, 9001) CE1BIG, CE2BIG, TE2BIG
         IF (DONE)
     $      WRITE (NOUT, 9002) INFORM, HOPT, ERRBND
      END IF

      RETURN

 9000 FORMAT(/ ' //CHCORE//  ITN ', I3,
     $                             ' FX     H      ', 5X, 1P2D16.6
     $       / ' //CHCORE//  F1      FDEST         ', 5X, 1P2D16.6
     $       / ' //CHCORE//  F2      FDEST2        ', 5X, 1P2D16.6
     $       / ' //CHCORE//  CDEST   SDEST         ', 5X, 1P2D16.6
     $       / ' //CHCORE//  FDCERR  SDCERR        ', 5X, 1P2D16.6)
 9001 FORMAT(  ' //CHCORE//  CE1BIG  CE2BIG  TE2BIG', 5X, 3L2     )
 9002 FORMAT(  ' //CHCORE//  INFORM  HOPT    ERRBND', I5, 1P2D16.6)

*     End of  CHCORE.

      END
