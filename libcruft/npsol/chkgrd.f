*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CHKGRD( INFORM, MSGLVL, N,
     $                   BIGBND, EPSRF, OKTOL, FDCHK, OBJF, XNORM,
     $                   OBJFUN,
     $                   BL, BU, GRAD, UGRAD, DX, X, Y, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)

      DOUBLE PRECISION   BL(N), BU(N), GRAD(N), UGRAD(N), DX(N)
      DOUBLE PRECISION   X(N), Y(N), W(LENW)
      EXTERNAL           OBJFUN

************************************************************************
*  CHKGRD  checks if the gradients of the objective function have
*  been coded correctly.
*
*  On input,  the value of the objective function at the point X is
*  stored in OBJF.  The corresponding gradient is stored in UGRAD.
*  If any gradient component has not been specified,  it will have a
*  dummy value.  Missing values are not checked.
*
*  A cheap test is first undertaken by calculating the directional
*  derivative using two different methods.  If this proves satisfactory
*  and no further information is desired, CHKGRD is terminated.
*  Otherwise, the routine CHCORE is called to give optimal step-sizes
*  and a forward-difference approximation to each component
*  of the gradient for which a test is deemed necessary,
*  either by the program or the user.
*
*  Other inputs:
*
*        X         The n-dimensional point at which the
*                  gradient is to be verified.
*        EPSRF     The positive bound on the relative error
*                  associated with computing the function at
*                  the point x.
*        OKTOL     The desired relative accuracy which the
*                  components of the gradient should satisfy.
*
*  LVRFYC has the following meaning...
*
*    -1        do not perform any check.
*     0        do the cheap test only.
*     1 or 3   do both cheap and full test.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version written  19-May-1985.
*  This version of CHKGRD  dated  12-July-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      COMMON    /SOL5NP/ LVRFYC, JVERFY(4)

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            CONST , DEBUG , DONE  , FIRST , HEADNG
      LOGICAL            NEEDED, OK
      CHARACTER*4        KEY   , LBAD  , LGOOD
      CHARACTER*18       RESULT(0:4)
      INTRINSIC          ABS   , MAX   , MIN   , SQRT
      EXTERNAL           DDOT
      PARAMETER         (RDUMMY =-11111.0              )
      PARAMETER         (ZERO   =0.0D+0, HALF  = 0.5D+0, POINT9 =0.9D+0)
      PARAMETER         (ONE    =1.0D+0, TWO   = 2.0D+0, TEN    =1.0D+1)
      PARAMETER         (LBAD   ='BAD?', LGOOD = '  OK')
      DATA               RESULT
     $                 / '                 ', 'Constant?      ',
     $                   'Linear or odd?   ', 'Too nonlinear?',
     $                   'Small derivative?'                   /

      INFORM = 0
      NEEDED = LVRFYC .EQ. 0  .OR.  LVRFYC .EQ. 1  .OR.  LVRFYC .EQ. 3
      IF (.NOT. NEEDED) RETURN

      IF (MSGLVL .GT. 0) WRITE (NOUT, 1000)
      DEBUG  = NPDBG  .AND.  INPDBG(5) .GT. 0
      NSTATE = 0

      BIGLOW = - BIGBND
      BIGUPP =   BIGBND

*     ==================================================================
*     Perform the cheap test.
*     ==================================================================
      H =     (ONE + XNORM)*FDCHK

      DXJ  = ONE / N
      DO 110 J = 1, N
         DX(J) =   DXJ
         DXJ   = - DXJ*POINT9
  110 CONTINUE

*     ------------------------------------------------------------------
*     Do not perturb X(J) if the  J-th  element is missing.
*     Compute the directional derivative.
*     ------------------------------------------------------------------
      NCHECK = 0
      DO 120 J = 1, N
         IF (GRAD(J) .EQ. RDUMMY) THEN
            DX(J) = ZERO
         ELSE
            NCHECK = NCHECK + 1

            XJ     =   X(J)
            STEPBL = - ONE
            STEPBU =   ONE
            IF (BL(J) .GT. BIGLOW)
     $         STEPBL = MAX( STEPBL, BL(J) - XJ )
            IF (BU(J) .LT. BIGUPP  .AND.  BU(J) .GT. BL(J))
     $         STEPBU = MIN( STEPBU, BU(J) - XJ )

            IF (HALF*(STEPBL + STEPBU) .LT. ZERO) THEN
               DX(J) = DX(J)*STEPBL
            ELSE
               DX(J) = DX(J)*STEPBU
            END IF
         END IF
  120 CONTINUE

      IF (NCHECK .EQ. 0) THEN
         WRITE (NOUT, 3500)
         RETURN
      END IF
      GDX    = DDOT  ( N, UGRAD, 1, DX, 1 )

*     ------------------------------------------------------------------
*     Make forward-difference approximation along  p.
*     ------------------------------------------------------------------
      CALL DCOPY ( N,     X, 1, Y, 1 )
      CALL DAXPY ( N, H, DX, 1, Y, 1 )

      MODE   = 0
      CALL OBJFUN( MODE, N, Y, OBJF1, UGRAD, NSTATE )
      IF (MODE .LT. 0) GO TO 999

      GDIFF =    ( OBJF1 - OBJF) / H
      ERROR = ABS( GDIFF - GDX ) / (ONE + ABS( GDX ))

      OK    = ERROR .LE. OKTOL
      IF (OK) THEN
         IF (MSGLVL .GT. 0) WRITE (NOUT, 1100)
      ELSE
         WRITE (NOUT, 1200)
         IF (ERROR .GE. ONE) INFORM = 1
      END IF

      IF (MSGLVL .GT. 0) WRITE (NOUT, 1300) GDX, GDIFF

*     ==================================================================
*     Component-wise check.
*     ==================================================================
      IF (LVRFYC .EQ. 1  .OR.  LVRFYC .EQ. 3) THEN
         HEADNG = .TRUE.
         ITMAX  = 3
         NWRONG = 0
         NGOOD  = 0
         JMAX   = 0
         EMAX   = ZERO
         NCHECK = 0
         J1     = JVERFY(1)
         J2     = JVERFY(2)

*        ---------------------------------------------------------------
*        Loop over each of the components of  x.
*        ---------------------------------------------------------------
         DO 500 J = J1, J2

            IF (GRAD(J) .NE. RDUMMY) THEN
*              ---------------------------------------------------------
*              Check this gradient component.
*              ---------------------------------------------------------
               NCHECK = NCHECK + 1
               GJ     = GRAD(J)
               GSIZE  = ONE + ABS( GJ )
               XJ     = X(J)
*              ---------------------------------------------------------
*              Find a finite-difference interval by iteration.
*              ---------------------------------------------------------
               ITER   = 0
               EPSA   = EPSRF*(ONE + ABS( OBJF ))
               CDEST  = ZERO
               SDEST  = ZERO
               FIRST  = .TRUE.

               STEPBL = BIGLOW
               STEPBU = BIGUPP
               IF (BL(J) .GT. BIGLOW) STEPBL = BL(J) - XJ
               IF (BU(J) .LT. BIGUPP) STEPBU = BU(J) - XJ

               HOPT   = TWO*(ONE + ABS( XJ ))*SQRT( EPSRF )
               H      = TEN*HOPT
               IF (HALF*(STEPBL + STEPBU) .LT. ZERO) H =  - H

*+             REPEAT
  400             X(J)  = XJ + H
                  CALL OBJFUN( MODE, N, X, F1, UGRAD, NSTATE )
                  IF (MODE .LT. 0) GO TO 999

                  X(J)  = XJ + H + H
                  CALL OBJFUN( MODE, N, X, F2, UGRAD, NSTATE )
                  IF (MODE .LT. 0) GO TO 999

                  CALL CHCORE( DEBUG, DONE, FIRST, EPSA, EPSRF, OBJF,XJ,
     $                         INFO, ITER, ITMAX,
     $                         CDEST, FDEST, SDEST, ERRBND, F1,
     $                         F2, H, HOPT, HPHI )

*+             UNTIL     DONE
               IF (.NOT. DONE) GO TO 400

*              ---------------------------------------------------------
*              Exit for this variable.
*              ---------------------------------------------------------
               GDIFF = CDEST
               X(J)  = XJ

               ERROR  = ABS( GDIFF - GJ  ) / GSIZE
               IF (ERROR .GE. EMAX) THEN
                  EMAX  = ERROR
                  JMAX  = J
               END IF

               OK =  ERROR .LE. OKTOL
               IF (OK) THEN
                  KEY    = LGOOD
                  NGOOD  = NGOOD  + 1
               ELSE
                  KEY    = LBAD
                  NWRONG = NWRONG + 1
               END IF

*              Zero components are not printed.

               CONST = OK .AND. INFO .EQ. 1 .AND. ABS(GJ) .LT. EPSPT8
               IF (.NOT. CONST) THEN
                  IF (HEADNG) WRITE (NOUT, 3000)
                  IF (OK) THEN
                     WRITE (NOUT, 3100) J, XJ, HOPT, GJ, GDIFF,
     $                                  KEY, ITER
                  ELSE
                     WRITE (NOUT, 3110) J, XJ, HOPT, GJ, GDIFF,
     $                                  KEY, ITER, RESULT(INFO)
                  END IF
                  HEADNG = .FALSE.
               END IF
            END IF
  500    CONTINUE

*        ===============================================================
*        Done.
*        ===============================================================
         IF (NWRONG .EQ. 0) THEN
            WRITE (NOUT, 3200) NGOOD , NCHECK, J1    , J2
         ELSE
            WRITE (NOUT, 3300) NWRONG, NCHECK, J1    , J2
         END IF
         WRITE (NOUT, 3400) EMAX, JMAX
      END IF

      CALL DCOPY ( N, GRAD, 1, UGRAD, 1 )

      RETURN

  999 INFORM = MODE
      RETURN

 1000 FORMAT(/// ' Verification of the objective gradients.'
     $       /   ' ----------------------------------------' )
 1100 FORMAT(/   ' The objective gradients seem to be ok.')
 1200 FORMAT(/   ' XXX  The objective gradients seem to be incorrect.')
 1300 FORMAT(/   ' Directional derivative of the objective', 1PE18.8/
     $           ' Difference approximation               ', 1PE18.8 )
 3000 FORMAT(// 4X, 'J', 4X, 'X(J)', 5X, 'DX(J)', 11X,
     $           'G(J)', 9X, '  Difference approxn  Itns' /)
 3100 FORMAT(  I5, 1P2E10.2,      1P2E18.8, 2X, A4, I6          )
 3110 FORMAT(  I5, 1P2E10.2,      1P2E18.8, 2X, A4, I6, 2X, A18 )
 3200 FORMAT(/ I7, '  Objective gradients out of the', I6,
     $             '  set in cols', I6, '  through', I6,
     $             '  seem to be ok.')
 3300 FORMAT(/   ' XXX  There seem to be', I6,
     $           '  incorrect objective gradients out of the', I6,
     $           '  set in cols', I6, '  through', I6 )
 3400 FORMAT(/   ' The largest relative error was', 1PE12.2,
     $           '   in element', I6 /)
 3500 FORMAT(/   ' No gradient elements assigned.' )

*     End of  CHKGRD.

      END
