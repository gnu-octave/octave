*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPCRSH( COLD, N, NCLIN, NCNLN,
     $                   NCTOTL, NACTIV, NFREE, NZ,
     $                   ISTATE, KACTIV, BIGBND, TOLACT,
     $                   BL, BU, C )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            COLD
      INTEGER            ISTATE(NCTOTL), KACTIV(N)
      DOUBLE PRECISION   C(*), BL(NCTOTL), BU(NCTOTL)
************************************************************************
*  NPCRSH  adds indices of nonlinear constraints to the initial working
*  set.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version   14-February 1985.
*  This version of  NPCRSH  dated 14-November-1985.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      COMMON    /SOL1CM/ NOUT

      LOGICAL            NPDBG
      PARAMETER        ( LDBG = 5 )
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      INTRINSIC          ABS, MIN
      PARAMETER        ( ONE = 1.0D+0 )

      NFIXED = N      - NFREE
      LINACT = NACTIV
      NPLIN  = N      + NCLIN

*     If a cold start is being made, initialize the status of the QP
*     working set.  First,  if  BL(j) = BU(j),  set ISTATE(j)=3.

      IF (COLD) THEN
         DO  130 J = NPLIN+1, NCTOTL
            ISTATE(J) = 0
            IF (BL(J) .EQ. BU(J)) ISTATE(J) = 3
  130    CONTINUE
      END IF

*     Increment NACTIV and KACTIV.
*     Ensure that the number of bounds and general constraints in the
*     QP  working set does not exceed N.

      DO 200 J = NPLIN+1, NCTOTL
         IF (NFIXED + NACTIV .EQ. N) ISTATE(J) = 0
         IF (ISTATE(J) .GT. 0) THEN
            NACTIV = NACTIV + 1
            KACTIV(NACTIV) = J - N
         END IF
  200 CONTINUE

      IF (COLD) THEN

*        ---------------------------------------------------------------
*        If a cold start is required, an attempt is made to add as many
*        nonlinear constraints as possible to the working set.
*        ---------------------------------------------------------------
*        The following loop finds the most violated constraint.  If
*        there is room in KACTIV, it will be added to the working set
*        and the process will be repeated.


         IS     =   1
         BIGLOW = - BIGBND
         BIGUPP =   BIGBND
         TOOBIG =   TOLACT + TOLACT

*        while (is .gt. 0  .and.  nfixed + nactiv .lt. n) do
  500    IF    (IS .GT. 0  .AND.  NFIXED + NACTIV .LT. N) THEN
            IS   = 0
            CMIN = TOLACT

            DO 520 I = 1, NCNLN
               J      = NPLIN + I
               IF (ISTATE(J) .EQ. 0) THEN
                  B1     = BL(J)
                  B2     = BU(J)
                  RESL   = TOOBIG
                  RESU   = TOOBIG
                  IF (B1 .GT. BIGLOW)
     $            RESL   = ABS( C(I) - B1 ) / (ONE + ABS( B1 ))
                  IF (B2 .LT. BIGUPP)
     $            RESU   = ABS( C(I) - B2 ) / (ONE + ABS( B2 ))
                  RES    = MIN( RESL, RESU )
                  IF (RES .LT. CMIN) THEN
                     CMIN = RES
                     IMIN = I
                     IS   = 1
                     IF (RESL .GT. RESU) IS = 2
                  END IF
               END IF
  520       CONTINUE

            IF (IS .GT. 0) THEN
               NACTIV         = NACTIV + 1
               KACTIV(NACTIV) = NCLIN  + IMIN
               J              = NPLIN  + IMIN
               ISTATE(J)      = IS
            END IF
            GO TO 500
*        end while
         END IF
      END IF

*     ------------------------------------------------------------------
*     An initial working set has now been selected.
*     ------------------------------------------------------------------
      NLNACT = NACTIV - LINACT
      NZ     = NFREE  - NACTIV
      IF (NPDBG  .AND.  INPDBG(1) .GT. 0)
     $   WRITE (NOUT, 1000) NFIXED, LINACT, NLNACT

      RETURN

 1000 FORMAT(/ ' //NPCRSH//  Working set selected....'
     $       / ' //NPCRSH// NFIXED LINACT NLNACT     '
     $       / ' //NPCRSH//', 3I7 )

*     End of  NPCRSH.

      END
