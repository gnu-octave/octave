*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMPRT ( MSGLVL, NFREE, NROWA,
     $                   N, NCLIN, NCNLN, NCTOTL, BIGBND,
     $                   NAMED, NAMES, LENNAM,
     $                   NACTIV, ISTATE, KACTIV, KX,
     $                   A, BL, BU, C, CLAMDA, RLAMDA, X )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*8        NAMES(*)
      LOGICAL            NAMED
      INTEGER            ISTATE(NCTOTL), KACTIV(N), KX(N)
      DOUBLE PRECISION   A(NROWA,*), BL(NCTOTL), BU(NCTOTL), C(*),
     $                   CLAMDA(NCTOTL), RLAMDA(N), X(N)

      COMMON    /SOL1CM/ NOUT

      LOGICAL            CMDBG
      INTEGER            LCMDBG
      PARAMETER         (LCMDBG = 5)
      COMMON    /CMDEBG/ ICMDBG(LCMDBG), CMDBG

***********************************************************************
*  CMPRT   creates the expanded Lagrange multiplier vector CLAMDA.
*  If MSGLVL .EQ 1 or MSGLVL .GE. 10,  CMPRT prints  x,  A*x,
*  c(x),  their bounds, the multipliers, and the residuals (distance
*  to the nearer bound).
*  CMPRT is called by LSCORE and NPCORE just before exiting.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 77 version written  October 1984.
*  This version of  CMPRT  dated  10-June-1986.
***********************************************************************
      CHARACTER*2        LS, LSTATE(7)
      CHARACTER*5        ID(3), ID3
      CHARACTER*8        ID4
      EXTERNAL           DDOT
      INTRINSIC          ABS

      PARAMETER        ( ZERO  = 0.0D+0 )
      DATA               ID(1) / 'VARBL' /
      DATA               ID(2) / 'LNCON' /
      DATA               ID(3) / 'NLCON' /
      DATA               LSTATE(1) / '--' /, LSTATE(2) / '++' /
      DATA               LSTATE(3) / 'FR' /, LSTATE(4) / 'LL' /
      DATA               LSTATE(5) / 'UL' /, LSTATE(6) / 'EQ' /
      DATA               LSTATE(7) / 'TB' /


      NPLIN  = N     + NCLIN
      NZ     = NFREE - NACTIV

*     Expand multipliers for bounds, linear and nonlinear constraints
*     into the  CLAMDA  array.

      CALL DLOAD ( NCTOTL, ZERO, CLAMDA, 1 )
      NFIXED = N - NFREE
      DO 150 K = 1, NACTIV+NFIXED
         IF (K .LE. NACTIV) J = KACTIV(K) + N
         IF (K .GT. NACTIV) J = KX(NZ+K)
         CLAMDA(J) = RLAMDA(K)
  150 CONTINUE

      IF (MSGLVL .LT. 10  .AND.  MSGLVL .NE. 1) RETURN

      WRITE (NOUT, 1100)
      ID3 = ID(1)

      DO 500 J = 1, NCTOTL
         B1     = BL(J)
         B2     = BU(J)
         WLAM   = CLAMDA(J)
         IS     = ISTATE(J)
         LS     = LSTATE(IS + 3)
         IF (J .LE. N) THEN

*           Section 1 -- the variables  x.
*           ------------------------------
            K      = J
            V      = X(J)

         ELSE IF (J .LE. NPLIN) THEN

*           Section 2 -- the linear constraints  A*x.
*           -----------------------------------------
            IF (J .EQ. N + 1) THEN
               WRITE (NOUT, 1200)
               ID3 = ID(2)
            END IF

            K      = J - N
            V      = DDOT  ( N, A(K,1), NROWA, X, 1 )
         ELSE

*           Section 3 -- the nonlinear constraints  c(x).
*           ---------------------------------------------

            IF (J .EQ. NPLIN + 1) THEN
               WRITE (NOUT, 1300)
               ID3 = ID(3)
            END IF

            K      = J - NPLIN
            V      = C(K)
         END IF

*        Print a line for the j-th variable or constraint.
*        -------------------------------------------------
         RES    = V - B1
         RES2   = B2 - V
         IF (ABS(RES) .GT. ABS(RES2)) RES = RES2
         IP     = 1
         IF (B1 .LE. ( - BIGBND )) IP = 2
         IF (B2 .GE.     BIGBND  ) IP = IP + 2
         IF (NAMED) THEN

            ID4 = NAMES(J)
            IF (IP .EQ. 1) THEN
               WRITE (NOUT, 2100) ID4,    LS, V, B1, B2, WLAM, RES
            ELSE IF (IP .EQ. 2) THEN
               WRITE (NOUT, 2200) ID4,    LS, V,     B2, WLAM, RES
            ELSE IF (IP .EQ. 3) THEN
               WRITE (NOUT, 2300) ID4,    LS, V, B1,     WLAM, RES
            ELSE
               WRITE (NOUT, 2400) ID4,    LS, V,         WLAM, RES
           END IF

         ELSE

            IF (IP .EQ. 1) THEN
               WRITE (NOUT, 3100) ID3, K, LS, V, B1, B2, WLAM, RES
            ELSE IF (IP .EQ. 2) THEN
               WRITE (NOUT, 3200) ID3, K, LS, V,     B2, WLAM, RES
            ELSE IF (IP .EQ. 3) THEN
               WRITE (NOUT, 3300) ID3, K, LS, V, B1,     WLAM, RES
            ELSE
               WRITE (NOUT, 3400) ID3, K, LS, V,         WLAM, RES
           END IF
         END IF
  500 CONTINUE
      RETURN

 1100 FORMAT(// ' Variable        State', 5X, ' Value',
     $   6X, ' Lower bound', 4X, ' Upper bound',
     $   '  Lagr multiplier', '     Residual' /)
 1200 FORMAT(// ' Linear constr   State', 5X, ' Value',
     $   6X, ' Lower bound', 4X, ' Upper bound',
     $   '  Lagr multiplier', '     Residual' /)
 1300 FORMAT(// ' Nonlnr constr   State', 5X, ' Value',
     $   6X, ' Lower bound', 4X, ' Upper bound',
     $   '  Lagr multiplier', '     Residual' /)
 2100 FORMAT(1X, A8, 10X, A2, 3G16.7, G16.7, G16.4)
 2200 FORMAT(1X, A8, 10X, A2, G16.7, 5X, ' None', 6X, G16.7,
     $   G16.7, G16.4)
 2300 FORMAT(1X, A8, 10X, A2, 2G16.7, 5X, ' None', 6X, G16.7, G16.4)
 2400 FORMAT(1X, A8, 10X, A2,  G16.7, 5X, ' None', 11X, ' None',
     $   6X, G16.7, G16.4)
 3100 FORMAT(1X, A5, I3, 10X, A2, 3G16.7, G16.7, G16.4)
 3200 FORMAT(1X, A5, I3, 10X, A2,  G16.7,
     $   5X, ' None', 6X, G16.7, G16.7, G16.4)
 3300 FORMAT(1X, A5, I3, 10X, A2, 2G16.7, 5X, ' None', 6X,
     $   G16.7, G16.4)
 3400 FORMAT(1X, A5, I3, 10X, A2,  G16.7,
     $   5X, ' None', 11X, ' None', 6X, G16.7, G16.4)

*     End of  CMPRT

      END
