*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CHKJAC( INFORM, LVLDER, MSGLVL,
     $                   NCSET, N, NCNLN, NROWJ, NROWUJ,
     $                   BIGBND, EPSRF, OKTOL, FDCHK, XNORM,
     $                   CONFUN, NEEDC,
     $                   BL, BU, C, C1, CJAC, UJAC, CJDX,
     $                   DX, ERR, X, Y, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            NEEDC(*)
      DOUBLE PRECISION   BL(N), BU(N), C(*), C1(*), CJDX(*),
     $                   CJAC(NROWJ,*), UJAC(NROWUJ,*), ERR(*)
      DOUBLE PRECISION   DX(N), X(N), Y(N), W(LENW)
      EXTERNAL           CONFUN

************************************************************************
*  CHKJAC  checks if the gradients of the constraints have been coded
*  correctly.
*
*  On input,  the values of the constraints at the point X are stored
*  in C.  Their corresponding gradients are stored in UJAC.  If any
*  Jacobian component has not been specified,  it will have a dummy
*  value.  Missing values are not checked.
*
*  A cheap test is first undertaken by calculating the directional
*  derivative using two different methods.  If this proves satisfactory
*  and no further information is desired, CHKJAC is terminated.
*  Otherwise, CHCORE is called to give optimal step-sizes and a central-
*  difference approximation to each component of the Jacobian for which
*  a test is deemed necessary, either by the program or the user.
*
*  LVRFYC has the following meaning...
*
*    -1        do not perform any check.
*     0        do the cheap test only.
*     2 or 3   do both cheap and full test.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version written  19-May-1985.
*  This version of CHKJAC dated 12-July-1986.
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
      EXTERNAL           DDOT  , IDAMAX
      PARAMETER         (RDUMMY =-11111.0              )
      PARAMETER         (ZERO   =0.0D+0, HALF   =0.5D+0, POINT9 =0.9D+0)
      PARAMETER         (ONE    =1.0D+0, TWO    =2.0D+0, TEN    =1.0D+1)
      PARAMETER         (LBAD   ='BAD?', LGOOD  ='  OK')
      DATA               RESULT
     $                 / '                 ', 'Constant?      ',
     $                   'Linear or odd?   ', 'Too nonlinear?',
     $                   'Small derivative?'                   /

      INFORM = 0
      NEEDED = NCNLN  .GT. 0  .AND.
     $         LVRFYC .EQ. 0  .OR.   LVRFYC .EQ. 2  .OR.  LVRFYC .EQ. 3
      IF (.NOT. NEEDED) RETURN

      IF (MSGLVL .GT. 0) WRITE (NOUT, 1000)
      DEBUG  = NPDBG  .AND.  INPDBG(5) .GT. 0
      NSTATE = 0

      BIGLOW = - BIGBND
      BIGUPP =   BIGBND

*     ==================================================================
*     Perform the cheap test.
*     ==================================================================
      H = (ONE + XNORM)*FDCHK

      DXJ  = ONE / N
      DO 110 J = 1, N
         DX(J) =   DXJ
         DXJ   = - DXJ*POINT9
  110 CONTINUE

*     ------------------------------------------------------------------
*     Do not perturb  X(J)  if the  J-th  column contains any
*     unknown elements.  Compute the directional derivative for each
*     constraint gradient.
*     ------------------------------------------------------------------
      NCHECK = 0
      DO 140 J = 1, N
         DO 130 I = 1, NCNLN
            IF (CJAC(I,J) .EQ. RDUMMY) THEN
               DX(J) = ZERO
               GO TO 140
            END IF
  130    CONTINUE
         NCHECK = NCHECK + 1

         XJ     =   X(J)
         STEPBL = - ONE
         STEPBU =   ONE
         IF (BL(J) .GT. BIGLOW)
     $      STEPBL = MAX( STEPBL, BL(J) - XJ )
         IF (BU(J) .LT. BIGUPP  .AND.  BU(J) .GT. BL(J))
     $      STEPBU = MIN( STEPBU, BU(J) - XJ )

         IF (HALF*(STEPBL + STEPBU) .LT. ZERO) THEN
            DX(J) = DX(J)*STEPBL
         ELSE
            DX(J) = DX(J)*STEPBU
         END IF
  140 CONTINUE

      IF (NCHECK .EQ. 0) THEN
         WRITE (NOUT, 2300)
      ELSE

*        Compute  (Jacobian)*DX.

         CALL DLOAD ( NCNLN, ZERO, CJDX, 1 )
         DO 150 J = 1, N
            IF (DX(J) .NE. ZERO)
     $         CALL DAXPY ( NCNLN, DX(J), UJAC(1,J), 1, CJDX, 1 )
  150    CONTINUE

*        ---------------------------------------------------------------
*        Make forward-difference approximation along DX.
*        ---------------------------------------------------------------
         CALL DCOPY ( N,     X, 1, Y, 1 )
         CALL DAXPY ( N, H, DX, 1, Y, 1 )

         CALL ILOAD ( NCNLN, (1), NEEDC, 1 )

         MODE   = 0
         CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                NEEDC, Y, C1, UJAC, NSTATE )
         IF (MODE .LT. 0) GO TO 999

*        Set  ERR = (C1 - C)/H  - Jacobian*DX.  This should be small.

         DO 170 I = 1, NCNLN
            ERR(I) = (C1(I) - C(I)) / H  -  CJDX(I)
  170    CONTINUE
         IMAX  = IDAMAX( NCNLN, ERR, 1 )
         EMAX  = ABS( ERR(IMAX) ) / (ONE + ABS( CJDX(IMAX) ))

         IF (EMAX .LE. OKTOL) THEN
            IF (MSGLVL .GT. 0) WRITE (NOUT, 2000)
         ELSE
            WRITE (NOUT, 2100)
            IF (EMAX .GE. ONE) INFORM = 2
         END IF
         IF (MSGLVL .GT. 0) WRITE (NOUT, 2200) EMAX, IMAX
      END IF

*     ==================================================================
*     Component-wise check.
*     ==================================================================
      IF (LVRFYC .GE. 2) THEN
         IF (LVLDER .EQ. 3) THEN

*           Recompute the Jacobian to find the non-constant elements.

            DO 280 J = 1, N
               CALL DLOAD ( NCNLN, RDUMMY, UJAC(1,J), 1 )
  280       CONTINUE

            CALL ILOAD ( NCNLN, (1), NEEDC, 1 )
            NSTATE = 0
            MODE   = 2

            CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                   NEEDC, X, C1, UJAC, NSTATE )
            IF (MODE .LT. 0) GO TO 999

         END IF

         CALL ILOAD ( NCNLN, (0), NEEDC, 1 )

         ITMAX  =   3
         NCHECK =   0
         NWRONG =   0
         NGOOD  =   0
         COLMAX = - ONE
         JCOL   =   0
         IROW   =   0
         MODE   =   0
         J3     =   JVERFY(3)
         J4     =   JVERFY(4)

*        ---------------------------------------------------------------
*        Loop over each column.
*        ---------------------------------------------------------------
         DO 600 J = J3, J4

            CALL DLOAD ( NCNLN, ZERO, ERR, 1 )
            HEADNG = .TRUE.
            XJ     = X(J)

            STEPBL = BIGLOW
            STEPBU = BIGUPP
            IF (BL(J) .GT. BIGLOW) STEPBL = BL(J) - XJ
            IF (BU(J) .LT. BIGUPP) STEPBU = BU(J) - XJ

            SIGNH  = ONE
            IF (HALF*(STEPBL + STEPBU) .LT. ZERO) SIGNH =  - ONE

            DO 500 I = 1, NCNLN
               EPSACI   = EPSRF*(ONE + ABS( C(I) ))

               IF (UJAC(I,J) .NE. RDUMMY) THEN
*                 ------------------------------------------------------
*                 Check this Jacobian element.
*                 ------------------------------------------------------
                  NCHECK   = NCHECK + 1
                  NEEDC(I) = 1

                  CIJ    = CJAC(I,J)
                  CJSIZE = ONE + ABS( CIJ )
*                 ------------------------------------------------------
*                 Find a finite-difference interval by iteration.
*                 ------------------------------------------------------
                  ITER   = 0
                  HOPT   = TWO*(ONE + ABS( XJ ))*SQRT( EPSRF )
                  H      = TEN*HOPT*SIGNH
                  CDEST  = ZERO
                  SDEST  = ZERO
                  FIRST  = .TRUE.

*+                REPEAT
  400                X(J)  = XJ + H
                     CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                            NEEDC, X, C1, UJAC, NSTATE )
                     IF (MODE .LT. 0) GO TO 999
                     F1    = C1(I)

                     X(J)  = XJ + H + H
                     CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                            NEEDC, X, C1, UJAC, NSTATE )
                     IF (MODE .LT. 0) GO TO 999
                     F2    = C1(I)

                     CALL CHCORE( DEBUG,DONE,FIRST,EPSACI,EPSRF,C(I),XJ,
     $                            INFO, ITER, ITMAX,
     $                            CDEST, FDEST, SDEST, ERRBND, F1,
     $                            F2, H, HOPT, HPHI )

*+                UNTIL     DONE
                  IF (.NOT. DONE) GO TO 400

*                 ------------------------------------------------------
*                 Exit for this element.
*                 ------------------------------------------------------
                  CJDIFF   = CDEST
                  ERR(I)   = ABS( CJDIFF - CIJ  ) / CJSIZE

                  OK       = ERR(I) .LE. OKTOL
                  IF (OK) THEN
                     KEY    = LGOOD
                     NGOOD  = NGOOD  + 1
                  ELSE
                     KEY    = LBAD
                     NWRONG = NWRONG + 1
                  END IF

                  CONST = OK .AND. INFO       .EQ. 1
     $                       .AND. ABS( CIJ ) .LT. EPSPT8
                  IF (.NOT. CONST) THEN
                     IF (HEADNG) THEN
                        WRITE (NOUT, 4000)
                        IF (OK)
     $                     WRITE (NOUT, 4100)   J, XJ    , HOPT, I,
     $                                        CIJ, CJDIFF, KEY , ITER
                        IF (.NOT. OK)
     $                     WRITE (NOUT, 4110)   J, XJ    , HOPT, I,
     $                                        CIJ, CJDIFF, KEY , ITER,
     $                                        RESULT(INFO)
                        HEADNG = .FALSE.
                     ELSE
                        IF (OK)
     $                     WRITE (NOUT, 4200)              HOPT, I,
     $                                        CIJ, CJDIFF, KEY , ITER
                        IF (.NOT. OK)
     $                     WRITE (NOUT, 4210)              HOPT, I,
     $                                        CIJ, CJDIFF, KEY , ITER,
     $                                        RESULT(INFO)
                     END IF
                  END IF
                  NEEDC(I) = 0
               END IF
  500       CONTINUE

*           ------------------------------------------------------------
*           Finished with this column.
*           ------------------------------------------------------------
            IF (.NOT. HEADNG) THEN
               IMAX = IDAMAX( NCNLN, ERR, 1 )
               EMAX = ABS( ERR(IMAX) )

               IF (EMAX .GE. COLMAX) THEN
                  IROW   = IMAX
                  JCOL   = J
                  COLMAX = EMAX
               END IF
            END IF
            X(J) = XJ

  600    CONTINUE

         IF (NCHECK .EQ. 0) THEN
            WRITE (NOUT, 4600) NCSET
         ELSE
            IF (NWRONG .EQ. 0) THEN
               WRITE (NOUT, 4300) NGOOD , NCHECK, J3, J4
            ELSE
               WRITE (NOUT, 4400) NWRONG, NCHECK, J3, J4
            END IF
            WRITE (NOUT, 4500) COLMAX, IROW, JCOL
         END IF

      END IF

*     Copy  ( constants + gradients + dummy values )  back into UJAC.

      DO 700 J = 1, N
         CALL DCOPY ( NCNLN, CJAC(1,J), 1, UJAC(1,J), 1 )
  700 CONTINUE

      RETURN

  999 INFORM = MODE
      RETURN

 1000 FORMAT(/// ' Verification of the constraint gradients.'
     $       /   ' -----------------------------------------' )
 2000 FORMAT(/   ' The Jacobian seems to be ok.')
 2100 FORMAT(/   ' XXX  The Jacobian seems to be incorrect.')
 2200 FORMAT(/   ' The largest relative error was', 1PE12.2,
     $           '  in constraint', I5 /)
 2300 FORMAT(/   ' Every column contains a constant or',
     $           ' missing element.')
 4000 FORMAT(// ' Column    X(J)     DX(J)    Row   ',
     $          ' Jacobian Value      Difference Approxn  Itns' )
 4100 FORMAT(/ I7,     1P2E10.2, I5, 1P2E18.8, 2X, A4, I6         )
 4110 FORMAT(/ I7,     1P2E10.2, I5, 1P2E18.8, 2X, A4, I6, 2X, A18)
 4200 FORMAT(  7X, 10X, 1PE10.2, I5, 1P2E18.8, 2X, A4, I6         )
 4210 FORMAT(  7X, 10X, 1PE10.2, I5, 1P2E18.8, 2X, A4, I6, 2X, A18)
 4300 FORMAT(/ I7, '  Jacobian elements out of the', I6,
     $             '  set in cols', I6, '  through', I6,
     $             '  seem to be ok.')
 4400 FORMAT(/   ' XXX  There seem to be', I6,
     $           '  incorrect Jacobian elements out of the', I6,
     $           '  set in cols', I6, '  through', I6 )
 4500 FORMAT(/ ' The largest relative error was', 1PE12.2,
     $         '  in row', I5, ',  column', I5 /)
 4600 FORMAT(  ' All', I6, '   assigned Jacobian elements are',
     $         ' constant.' )

*     End of  CHKJAC.

      END
