*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CHFD  ( INFORM, MSGLVL, LVLDER,
     $                   N, NCNLN, NROWJ, NROWUJ,
     $                   BIGBND, EPSRF, FDNORM, OBJF,
     $                   OBJFUN, CONFUN, NEEDC,
     $                   BL, BU, C, C1, C2, CJAC, UJAC,
     $                   GRAD, UGRAD, HFORWD, HCNTRL,
     $                   X, Y, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            NEEDC(*)
      DOUBLE PRECISION   BL(N), BU(N)
      DOUBLE PRECISION   C(*), C1(*), C2(*),
     $                   CJAC(NROWJ,*), UJAC(NROWUJ,*)
      DOUBLE PRECISION   GRAD(N), UGRAD(N)
      DOUBLE PRECISION   HFORWD(*), HCNTRL(*)
      DOUBLE PRECISION   X(N), Y(N), W(LENW)
      EXTERNAL           OBJFUN, CONFUN

************************************************************************
*  CHFD    computes difference intervals for the missing gradients of
*  F(x) and c(x).  Intervals are computed using a procedure that usually
*  requires about two function evaluations if the function is well
*  scaled.  Central-difference gradients are obtained as a by-product
*  of the algorithm.
*
*  On entry...
*     OBJF and C contain the problem functions at the point X.
*     An element of CJAC or GRAD not equal to RDUMMY signifies a known
*     gradient value.  Such values are not estimated by differencing.
*     UJAC and UGRAD have dummy elements in the same positions as
*     CJAC and UGRAD.
*
*  On exit...
*     CJAC and GRAD contain central-difference derivative estimates.
*     Elements of UJAC and UGRAD are unaltered except for those
*     corresponding to constant derivatives, which are given the same
*     values as CJAC or GRAD.
*
*  Systems Optimization Laboratory, Department of Operations Research,
*  Stanford University, Stanford, California 94305
*  Original version written 28-July-1985.
*  This version of CHFD   dated 14-July-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            DEBUG , DONE  , FIRST , HEADNG, NEEDED
      INTRINSIC          ABS   , MAX   , MIN   , SQRT
      EXTERNAL           DNRM2
      PARAMETER         (RDUMMY =-11111.0              )
      PARAMETER         (FACTOR =0.97D+0               )
      PARAMETER         (ZERO   =0.0D+0, HALF   =0.5D+0, ONE   =1.0D+0)
      PARAMETER         (TWO    =2.0D+0, FOUR   =4.0D+0, TEN   =1.0D+1)

      INFORM = 0
      NEEDED = LVLDER .EQ. 0  .OR.  LVLDER .EQ. 2
     $                        .OR.  LVLDER .EQ. 1  .AND.  NCNLN .GT. 0
      IF (.NOT. NEEDED) RETURN

      DEBUG  = NPDBG  .AND.  INPDBG(5) .GT. 0
      IF (LFDSET .EQ. 0) THEN
         IF (MSGLVL .GT. 0) WRITE (NOUT, 1000)

         NSTATE = 0
         ITMAX  = 3
         MODE   = 0

         NCCNST = 0
         NFCNST = 0
         HEADNG = .TRUE.

         FDNORM = ZERO

*        ===============================================================
*        For each column of the Jacobian augmented by the transpose of
*        the objective gradient, rows IROW1 thru IROW2 are searched for
*        missing elements.
*        ===============================================================
         IROW1  = 1
         IROW2  = NCNLN + 1
         IF (LVLDER .EQ. 1) IROW2 = NCNLN
         IF (LVLDER .EQ. 2) IROW1 = NCNLN + 1

         BIGLOW = - BIGBND
         BIGUPP =   BIGBND

         IF (NCNLN  .GT. 0)
     $      CALL ILOAD ( NCNLN, (0), NEEDC, 1 )

         DO 600 J = 1, N
            XJ     = X(J)
            NFOUND = 0
            SUMSD  = ZERO
            SUMEPS = ZERO
            HFD    = ZERO
            HCD    = ZERO
            HMAX   = ZERO
            HMIN   = ONE / EPSPT3
            ERRMAX = ZERO
            ERRMIN = ZERO

            STEPBL = BIGLOW
            STEPBU = BIGUPP
            IF (BL(J) .GT. BIGLOW) STEPBL = BL(J) - XJ
            IF (BU(J) .LT. BIGUPP) STEPBU = BU(J) - XJ

            SIGNH  = ONE
            IF (HALF*(STEPBL + STEPBU) .LT. ZERO) SIGNH =  - ONE

            DO 500 I = IROW1, IROW2

               IF (I .LE. NCNLN) THEN
                  TEST = UJAC(I,J)
               ELSE
                  TEST = UGRAD(J)
               END IF

               IF (TEST .EQ. RDUMMY) THEN
*                 ======================================================
*                 Get the difference interval for this component.
*                 ======================================================
                  NFOUND = NFOUND + 1

                  IF (I .LE. NCNLN) THEN
                     NEEDC(I) = 1
                     FX       = C(I)
                     EPSA     = EPSRF*(ONE + ABS( C(I) ))
                  ELSE
                     FX       = OBJF
                     EPSA     = EPSRF*(ONE + ABS( FX ))
                  END IF

*                 ------------------------------------------------------
*                 Find a finite-difference interval by iteration.
*                 ------------------------------------------------------
                  ITER   = 0
                  HOPT   = TWO*(ONE + ABS( XJ ))*SQRT( EPSRF )
                  H      = SIGNH*TEN*HOPT
                  CDEST  = ZERO
                  SDEST  = ZERO
                  FIRST  = .TRUE.

*+                REPEAT
  400                X(J)  = XJ + H
                     IF (I .LE. NCNLN) THEN
                        CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                               NEEDC, X, C1, UJAC, NSTATE )
                        IF (MODE .LT. 0) GO TO 9999
                        F1 = C1(I)
                     ELSE
                        CALL OBJFUN( MODE, N, X, F1, UGRAD, NSTATE )
                        IF (MODE .LT. 0) GO TO 9999
                     END IF

                     X(J)  = XJ + H + H
                    IF (I .LE. NCNLN) THEN
                       CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                              NEEDC, X, C1, UJAC, NSTATE )
                        IF (MODE .LT. 0) GO TO 9999
                        F2 = C1(I)
                     ELSE
                        CALL OBJFUN( MODE, N, X, F2, UGRAD, NSTATE )
                        IF (MODE .LT. 0) GO TO 9999
                     END IF

                     CALL CHCORE( DEBUG, DONE, FIRST, EPSA, EPSRF,FX,XJ,
     $                            INFO, ITER, ITMAX,
     $                            CDEST, FDEST, SDEST, ERRBND, F1,
     $                            F2, H, HOPT, HPHI )

*+                UNTIL     DONE
                  IF (.NOT. DONE) GO TO 400

                  IF (I .LE. NCNLN) THEN
                     CJAC(I,J) = CDEST
                     IF (INFO .EQ. 1  .OR.  INFO .EQ. 2) THEN
                        NCCNST    =   NCCNST + 1
                        NCDIFF    =   NCDIFF - 1
                        UJAC(I,J) = - RDUMMY
                     END IF
                  ELSE
                     GRAD(J)   = CDEST
                     IF (INFO .EQ. 1  .OR.  INFO .EQ. 2) THEN
                        NFCNST    =   NFCNST + 1
                        NFDIFF    =   NFDIFF - 1
                        UGRAD(J)  = - RDUMMY
                     END IF
                  END IF

                  SUMSD  = SUMSD  + ABS( SDEST )
                  SUMEPS = SUMEPS +      EPSA
                  IF (HOPT .GT. HMAX) THEN
                     HMAX   = HOPT
                     ERRMAX = ERRBND
                  END IF
                  IF (HOPT .LT. HMIN) THEN
                     HMIN   = HOPT
                     ERRMIN = ERRBND
                  END IF

                  IF (INFO .EQ. 0) HCD  = MAX ( HCD, HPHI )
               END IF
  500       CONTINUE

            IF (NFOUND .GT. 0) THEN
               IF (HMIN .GT. HMAX) THEN
                  HMIN   = HMAX
                  ERRMIN = ERRMAX
               END IF

               IF      (FOUR*SUMEPS .LT. HMIN*HMIN*SUMSD) THEN
                  HFD    = HMIN
                  ERRMAX = ERRMIN
               ELSE IF (FOUR*SUMEPS .GT. HMAX*HMAX*SUMSD) THEN
                  HFD    = HMAX
               ELSE
                  HFD    = TWO*SQRT( SUMEPS / SUMSD )
                  ERRMAX = TWO*SQRT( SUMEPS * SUMSD )
               END IF

               IF (HCD .EQ. ZERO) HCD = TEN*HFD

               IF (MSGLVL .GT. 0) THEN
                  IF (HEADNG) WRITE (NOUT, 1100)
                  WRITE (NOUT, 1200) J, XJ, HFD, HCD, ERRMAX
                  HEADNG = .FALSE.
               END IF
            END IF

            FDNORM    = MAX (FDNORM, HFD)
            HFORWD(J) = HFD / (ONE + ABS(XJ))
            HCNTRL(J) = HCD / (ONE + ABS(XJ))
            X(J)      = XJ
  600    CONTINUE

         IF (NCCNST + NFCNST .GT. 0) THEN

*           Check that the constants have been set properly by
*           evaluating the gradients at a strange (but feasible) point.

            D      =   ONE / N

            DO 710 J = 1, N
               XJ     =   X(J)
               STEPBL = - ONE
               STEPBU =   ONE
               IF (BL(J) .GT. BIGLOW)
     $            STEPBL = MAX( STEPBL, BL(J) - XJ )
               IF (BU(J) .LT. BIGUPP  .AND.  BU(J) .GT. BL(J))
     $            STEPBU = MIN( STEPBU, BU(J) - XJ )

               IF (HALF*(STEPBL + STEPBU) .LT. ZERO) THEN
                  Y(J) = XJ + D*STEPBL
               ELSE
                  Y(J) = XJ + D*STEPBU
               END IF

               D = FACTOR*D
  710       CONTINUE

            IF (NCNLN .GT. 0) THEN
               CALL ILOAD ( NCNLN, (1), NEEDC, 1 )
               CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                      NEEDC, Y, C2, UJAC, NSTATE )
               IF (MODE .LT. 0) GO TO 9999
            END IF

            CALL OBJFUN( MODE, N, Y, OBJF2, UGRAD, NSTATE )
            IF (MODE .LT. 0) GO TO 9999

*           ------------------------------------------------------------
*           Loop over each of the components of  x.
*           ------------------------------------------------------------
            DO 800 J = 1, N
               YJ     = Y(J)
               DX     = HALF*(X(J) - YJ)
               Y(J)   = YJ + DX

               IF (NCNLN .GT. 0) THEN
                  NFOUND = 0
                  DO 720 I = 1, NCNLN
                     IF (UJAC(I,J) .EQ. - RDUMMY) THEN
                        NEEDC(I) = 1
                        NFOUND   = NFOUND + 1
                     ELSE
                        NEEDC(I) = 0
                     END IF
  720             CONTINUE

                  IF (NFOUND .GT. 0) THEN
                     CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                            NEEDC, Y, C1, UJAC, NSTATE )
                     IF (MODE .LT. 0) GO TO 9999

                     DO 730 I = 1, NCNLN
                        IF (NEEDC(I) .EQ. 1) THEN
                           CJDIFF = ( C1(I) -  C2(I) ) / DX
                           IF (CJDIFF .EQ. CJAC(I,J)) THEN
                              UJAC(I,J) = CJDIFF
                           ELSE
                              UJAC(I,J) = RDUMMY
                              NCCNST    = NCCNST - 1
                              NCDIFF    = NCDIFF + 1
                           END IF
                        END IF
  730                CONTINUE
                  END IF
               END IF

*              Now check the objective gradient component.

               IF (UGRAD(J) .EQ. - RDUMMY) THEN

                  CALL OBJFUN( MODE, N, Y, F1, UGRAD, NSTATE )
                  IF (MODE .LT. 0) GO TO 9999

                  GDIFF = (F1 - OBJF2)/DX
                  IF (GDIFF .EQ. GRAD(J)) THEN
                     UGRAD(J) = GDIFF
                  ELSE
                     UGRAD(J) = RDUMMY
                     NFDIFF   = NFDIFF + 1
                     NFCNST   = NFCNST - 1
                  END IF
               END IF

               Y(J)  = YJ
  800       CONTINUE

            IF (MSGLVL .GT. 0) THEN
               IF (LVLDER .LT. 2  .AND.  NCCNST .GT. 0)
     $            WRITE (NOUT, 1300) NCCNST
               IF (LVLDER .NE. 1  .AND.  NFCNST .GT. 0)
     $            WRITE (NOUT, 1400) NFCNST
            END IF

            IF (NCDIFF .EQ. 0  .AND.  LVLDER .LT. 2) THEN
               IF (LVLDER .EQ. 0) LVLDER = 2
               IF (LVLDER .EQ. 1) LVLDER = 3
               IF (MSGLVL .GT. 0) WRITE (NOUT, 1500) LVLDER
            END IF

            IF (NFDIFF .EQ. 0  .AND.  LVLDER .NE. 1) THEN
               IF (LVLDER .EQ. 0) LVLDER = 1
               IF (LVLDER .EQ. 2) LVLDER = 3
               IF (MSGLVL .GT. 0) WRITE (NOUT, 1600) LVLDER
            END IF
         END IF
      ELSE IF (LFDSET .EQ. 2) THEN

*        The user has supplied HFORWD and HCNTRL.
*        Check for wild values.

         DO 900 J = 1, N
            IF (HFORWD(J) .LE. ZERO) THEN
               WRITE (NOUT, 2000) J, HFORWD(J), EPSPT5
               HFORWD(J) = EPSPT5
            END IF
  900    CONTINUE
         DO 910 J = 1, N
            IF (HCNTRL(J) .LE. ZERO) THEN
               WRITE (NOUT, 2100) J, HCNTRL(J), EPSPT3
               HCNTRL(J) = EPSPT3
            END IF
  910    CONTINUE
      END IF

      RETURN

 9999 INFORM = MODE
      RETURN

 1000 FORMAT(//' Computation of the finite-difference intervals'
     $       / ' ----------------------------------------------' )
 1100 FORMAT(//'    J      X(J)   Forward DX(J)   Central DX(J) ',
     $         '     Error est.' /)
 1200 FORMAT(  I5, 1PE10.2, 1PE16.6, 1P2E16.6 )
 1300 FORMAT(/ I5,  '  constant constraint gradient elements assigned.')
 1400 FORMAT(/ I5,  '  constant  objective gradient elements assigned.')
 1500 FORMAT(//' All missing Jacobian elements are constants.  ',
     $         ' Derivative level increased to ', I4 )
 1600 FORMAT(//' All missing objective gradients are constants.  ',
     $         ' Derivative level increased to ', I4 )
 2000 FORMAT(' XXX  ', I4,'-th difference interval ',         1PE10.2,
     $       ' replaced by ', 1PE10.2 )
 2100 FORMAT(' XXX  ', I4,'-th central-difference interval ', 1PE10.2,
     $       ' replaced by ', 1PE10.2 )

*     End of  CHFD  .

      END
