*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSCRSH( COLD, VERTEX,
     $                   NCLIN, NCTOTL, NACTIV, NARTIF,
     $                   NFREE, N, NROWA,
     $                   ISTATE, KACTIV,
     $                   BIGBND, TOLACT,
     $                   A, AX, BL, BU, X, WX, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            COLD, VERTEX
      INTEGER            ISTATE(NCTOTL), KACTIV(N)
      DOUBLE PRECISION   A(NROWA,*), AX(*), BL(NCTOTL), BU(NCTOTL),
     $                   X(N), WX(N), WORK(N)

************************************************************************
*     LSCRSH  computes the quantities  ISTATE (optionally), KACTIV,
*     NACTIV, NZ and NFREE  associated with the working set at X.
*     The computation depends upon the value of the input parameter
*     COLD,  as follows...
*
*     COLD = TRUE.  An initial working set will be selected. First,
*                   nearly-satisfied or violated bounds are added.
*                   Next,  general linear constraints are added that
*                   have small residuals.
*
*     COLD = FALSE. The quantities KACTIV, NACTIV, NZ and NFREE are
*                   computed from ISTATE,  specified by the user.
*
*     Values of ISTATE(j)....
*
*        - 2         - 1         0           1          2         3
*     a'x lt bl   a'x gt bu   a'x free   a'x = bl   a'x = bu   bl = bu
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 31-October-1984.
*     This version of LSCRSH dated 27-December-1985.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      EXTERNAL           DDOT
      INTRINSIC          ABS, MIN
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      FLMAX  = WMACH(7)
      CALL DCOPY ( N, X, 1, WX, 1 )

      IF (LSDBG) THEN
         IF (ILSDBG(1) .GT. 0)
     $      WRITE (NOUT, 1000) COLD, NCLIN, NCTOTL
         IF (ILSDBG(2) .GT. 0)
     $      WRITE (NOUT, 1100) (WX(J), J = 1, N)
      END IF

      NFIXED = 0
      NACTIV = 0
      NARTIF = 0

*     If a cold start is being made, initialize  ISTATE.
*     If  BL(j) = BU(j),  set  ISTATE(j)=3  for all variables and linear
*     constraints.

      IF (COLD) THEN
         DO 100 J = 1, NCTOTL
            ISTATE(J) = 0
            IF (BL(J) .EQ. BU(J)) ISTATE(J) = 3
  100    CONTINUE
      ELSE
         DO 110 J = 1, NCTOTL
            IF (ISTATE(J) .GT. 3  .OR.  ISTATE(J) .LT. 0) ISTATE(J) = 0
  110    CONTINUE
      END IF

*     Initialize NFIXED, NFREE and KACTIV.
*     Ensure that the number of bounds and general constraints in the
*     working set does not exceed N.

      DO 200 J = 1, NCTOTL
         IF (NFIXED + NACTIV .EQ. N) ISTATE(J) = 0
         IF (ISTATE(J) .GT. 0) THEN
            IF (J .LE. N) THEN
               NFIXED = NFIXED + 1
               IF (ISTATE(J) .EQ. 1) WX(J) = BL(J)
               IF (ISTATE(J) .GE. 2) WX(J) = BU(J)
            ELSE
               NACTIV = NACTIV + 1
               KACTIV(NACTIV) = J - N
            END IF
         END IF
  200 CONTINUE

*     ------------------------------------------------------------------
*     If a cold start is required,  attempt to add as many
*     constraints as possible to the working set.
*     ------------------------------------------------------------------
      IF (COLD) THEN
         BIGLOW = - BIGBND
         BIGUPP =   BIGBND

*        See if any bounds are violated or nearly satisfied.
*        If so,  add these bounds to the working set and set the
*        variables exactly on their bounds.

         J = N
*+       WHILE (J .GE. 1  .AND.  NFIXED + NACTIV .LT. N) DO
  300    IF    (J .GE. 1  .AND.  NFIXED + NACTIV .LT. N) THEN
            IF (ISTATE(J) .EQ. 0) THEN
               B1     = BL(J)
               B2     = BU(J)
               IS     = 0
               IF (B1 .GT. BIGLOW) THEN
                  IF (WX(J) - B1 .LE. (ONE + ABS( B1 ))*TOLACT) IS = 1
               END IF
               IF (B2 .LT. BIGUPP) THEN
                  IF (B2 - WX(J) .LE. (ONE + ABS( B2 ))*TOLACT) IS = 2
               END IF
               IF (IS .GT. 0) THEN
                  ISTATE(J) = IS
                  IF (IS .EQ. 1) WX(J) = B1
                  IF (IS .EQ. 2) WX(J) = B2
                  NFIXED = NFIXED + 1
               END IF
            END IF
            J = J - 1
            GO TO 300
*+       END WHILE
         END IF

*        ---------------------------------------------------------------
*        The following loop finds the linear constraint (if any) with
*        smallest residual less than or equal to TOLACT  and adds it
*        to the working set.  This is repeated until the working set
*        is complete or all the remaining residuals are too large.
*        ---------------------------------------------------------------
*        First, compute the residuals for all the constraints not in the
*        working set.

         IF (NCLIN .GT. 0  .AND.  NACTIV+NFIXED .LT. N) THEN
            DO 410 I = 1, NCLIN
               IF (ISTATE(N+I) .LE. 0)
     $         AX(I) = DDOT  (N, A(I,1), NROWA, WX, 1 )
  410       CONTINUE

            IS     = 1
            TOOBIG = TOLACT + TOLACT

*+          WHILE (IS .GT. 0  .AND.  NFIXED + NACTIV .LT. N) DO
  500       IF    (IS .GT. 0  .AND.  NFIXED + NACTIV .LT. N) THEN
               IS     = 0
               RESMIN = TOLACT

               DO 520 I = 1, NCLIN
                  J      = N + I
                  IF (ISTATE(J) .EQ. 0) THEN
                     B1     = BL(J)
                     B2     = BU(J)
                     RESL   = TOOBIG
                     RESU   = TOOBIG
                     IF (B1 .GT. BIGLOW)
     $                  RESL  = ABS( AX(I) - B1 ) / (ONE + ABS( B1 ))
                     IF (B2 .LT. BIGUPP)
     $                  RESU  = ABS( AX(I) - B2 ) / (ONE + ABS( B2 ))
                     RESIDL   = MIN( RESL, RESU )
                     IF(RESIDL .LT. RESMIN) THEN
                        RESMIN = RESIDL
                        IMIN   = I
                        IS     = 1
                        IF (RESL .GT. RESU) IS = 2
                     END IF
                  END IF
  520          CONTINUE

               IF (IS .GT. 0) THEN
                  NACTIV = NACTIV + 1
                  KACTIV(NACTIV) = IMIN
                  J         = N + IMIN
                  ISTATE(J) = IS
               END IF
               GO TO 500
*+          END WHILE
            END IF
         END IF

*        ---------------------------------------------------------------
*        If required, add temporary bounds to make a vertex.
*        ---------------------------------------------------------------
         IF (VERTEX  .AND.  NACTIV+NFIXED .LT. N) THEN

*           Compute lengths of columns of selected linear constraints
*           (just the ones corresponding to free variables).

            DO 630 J = 1, N
               IF (ISTATE(J) .EQ. 0) THEN
                  COLSIZ = ZERO
                  DO 620 K = 1, NCLIN
                     IF (ISTATE(N+K) .GT. 0)
     $               COLSIZ = COLSIZ + ABS( A(K,J) )
  620             CONTINUE
                  WORK(J) = COLSIZ
               END IF
  630       CONTINUE

*           Find the  NARTIF  smallest such columns.
*           This is an expensive loop.  Later we can replace it by a
*           4-pass process (say), accepting the first col that is within
*           T  of  COLMIN, where  T = 0.0, 0.001, 0.01, 0.1 (say).
*           (This comment written in 1980).

*+          WHILE (NFIXED + NACTIV .LT. N) DO
  640       IF    (NFIXED + NACTIV .LT. N) THEN
               COLMIN = FLMAX
               DO 650 J = 1, N
                  IF (ISTATE(J) .EQ. 0) THEN
                     IF (NCLIN .EQ. 0) GO TO 660
                     COLSIZ = WORK(J)
                     IF (COLMIN .GT. COLSIZ) THEN
                        COLMIN = COLSIZ
                        JMIN   = J
                     END IF
                  END IF
  650          CONTINUE
               J      = JMIN
  660          ISTATE(J) = 4
               NARTIF = NARTIF + 1
               NFIXED = NFIXED + 1
               GO TO 640
*+          END WHILE
            END IF
         END IF
      END IF

      NFREE = N - NFIXED

      IF (LSDBG) THEN
         IF (ILSDBG(1) .GT. 0)
     $       WRITE (NOUT, 1300) NFIXED, NACTIV, NARTIF
         IF (ILSDBG(2) .GT. 0)
     $       WRITE (NOUT, 1200) (WX(J), J = 1, N)
      END IF

      RETURN

 1000 FORMAT(/ ' //LSCRSH// COLD NCLIN NCTOTL'
     $       / ' //LSCRSH// ', L4, I6, I7 )
 1100 FORMAT(/ ' //LSCRSH// Variables before crash... '/ (5G12.3))
 1200 FORMAT(/ ' //LSCRSH// Variables after  crash... '/ (5G12.3))
 1300 FORMAT(/ ' //LSCRSH// Working set selected ...             '
     $       / ' //LSCRSH// NFIXED NACTIV NARTIF      '
     $       / ' //LSCRSH// ', I6, 2I7 )

*     End of  LSCRSH.

      END
