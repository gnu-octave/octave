*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPFD  ( CENTRL, INFORM,
     $                   NROWJ, NROWUJ, N, NCNLN,
     $                   BIGBND, CDINT, FDINT, FDNORM, OBJF,
     $                   CONFUN, OBJFUN, NEEDC,
     $                   BL, BU, C, C1, C2, CJAC, UJAC,
     $                   GRAD, UGRAD, HFORWD, HCNTRL, X,
     $                   W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            CENTRL
      INTEGER            NEEDC(*)

      DOUBLE PRECISION   BL(N), BU(N), C(*), C1(*), C2(*),
     $                   CJAC(NROWJ,*), UJAC(NROWUJ,*)
      DOUBLE PRECISION   GRAD(N), UGRAD(N), HFORWD(N), HCNTRL(N), X(N)
      DOUBLE PRECISION   W(LENW)
      EXTERNAL           CONFUN, OBJFUN

************************************************************************
*  NPFD   evaluates any missing gradients.
*
*  Systems Optimization Laboratory, Stanford University, California.
*  Original version written 3-July-1986.
*  This version of NPFD   dated 14-July-1986.
************************************************************************

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      INTRINSIC          ABS   , MAX

      PARAMETER         (RDUMMY=-11111.0)
      PARAMETER         (ZERO  = 0.0D+0, HALF  = 0.5D+0, ONE   = 1.0D+0)
      PARAMETER         (TWO   = 2.0D+0, THREE = 3.0D+0, FOUR  = 4.0D+0)

      INFORM = 0

*     ==================================================================
*     Use the pre-assigned difference intervals to approximate the
*     derivatives.
*     ==================================================================
*     Use either the same interval for each component (LFDSET = 1),
*     or the intervals already in HFORWD or HCNTRL (LFDSET = 0 or 2).

      NSTATE =   0
      MODE   =   0

      BIGLOW = - BIGBND
      BIGUPP =   BIGBND

      FDNORM =   ZERO

      DO 340 J  = 1, N

         XJ     = X(J)
         NFOUND = 0
         IF (NCDIFF .GT. 0) THEN
            DO 310 I = 1, NCNLN
               IF (UJAC(I,J) .EQ. RDUMMY) THEN
                  NEEDC(I) = 1
                  NFOUND   = NFOUND + 1
               ELSE
                  NEEDC(I) = 0
               END IF
  310       CONTINUE
         END IF

         IF (NFOUND .GT. 0  .OR.  UGRAD(J) .EQ. RDUMMY) THEN
            STEPBL = BIGLOW
            STEPBU = BIGUPP
            IF (BL(J) .GT. BIGLOW) STEPBL = BL(J) - XJ
            IF (BU(J) .LT. BIGUPP) STEPBU = BU(J) - XJ

            IF (CENTRL) THEN
               IF (LFDSET .EQ. 1) THEN
                  DELTA = CDINT
               ELSE
                  DELTA = HCNTRL(J)
               END IF
            ELSE
               IF (LFDSET .EQ. 1) THEN
                  DELTA = FDINT
               ELSE
                  DELTA = HFORWD(J)
               END IF
            END IF

            DELTA  = DELTA*(ONE + ABS(XJ))
            FDNORM = MAX (FDNORM, DELTA)
            IF (HALF*(STEPBL + STEPBU) .LT. ZERO) DELTA =  - DELTA

            X(J) = XJ + DELTA
            IF (NFOUND .GT. 0) THEN
               CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                      NEEDC, X, C1, UJAC, NSTATE )
               IF (MODE .LT. 0) GO TO 999
            END IF

            IF (UGRAD(J) .EQ. RDUMMY) THEN
               CALL OBJFUN( MODE, N, X, OBJF1, UGRAD, NSTATE )
               IF (MODE .LT. 0) GO TO 999
            END IF

            IF (CENTRL) THEN
*              ---------------------------------------------------------
*              Central differences.
*              ---------------------------------------------------------
               X(J)  = XJ + DELTA + DELTA

               IF (NFOUND .GT. 0) THEN
                  CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                         NEEDC, X, C2, UJAC, NSTATE )
                  IF (MODE .LT. 0) GO TO 999

                  DO 320 I = 1, NCNLN
                     IF (NEEDC(I) .EQ. 1)
     $                  CJAC(I,J) = (FOUR*C1(I) - THREE*C(I) - C2(I))
     $                                  / (DELTA + DELTA)
  320             CONTINUE
               END IF

               IF (UGRAD(J) .EQ. RDUMMY) THEN
                  CALL OBJFUN( MODE, N, X, OBJF2, UGRAD, NSTATE )
                  IF (MODE .LT. 0) GO TO 999

                  GRAD(J) = (FOUR*OBJF1 - THREE*OBJF - OBJF2)
     $                                  / (DELTA + DELTA)

               END IF
            ELSE
*              ---------------------------------------------------------
*              Forward Differences.
*              ---------------------------------------------------------
               IF (NFOUND .GT. 0) THEN
                  DO 330 I = 1, NCNLN
                     IF (NEEDC(I) .EQ. 1)
     $                  CJAC(I,J) = (C1(I) -  C(I))/  DELTA
  330             CONTINUE
               END IF

               IF (UGRAD(J) .EQ. RDUMMY)
     $            GRAD(J) = (OBJF1 - OBJF) /  DELTA

            END IF
         END IF
         X(J) = XJ

  340 CONTINUE

      RETURN

  999 INFORM = MODE
      RETURN

*     End of  NPFD  .

      END
