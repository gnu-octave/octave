*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPCHKD( INFORM, MSGNP, NSTATE, LVLDER, NFUN, NGRAD,
     $                   NROWJ, NROWUJ, N, NCNLN,
     $                   CONFUN, OBJFUN, NEEDC,
     $                   BIGBND, EPSRF, CDINT, FDINT,
     $                   FDCHK, FDNORM, OBJF, XNORM,
     $                   BL, BU, C, C1, CJAC, UJAC, CJDX,
     $                   DX, GRAD, UGRAD, HFORWD, HCNTRL,
     $                   X, WRK1, WRK2, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            NEEDC(*)
      DOUBLE PRECISION   C(*), C1(*), CJAC(NROWJ,*), UJAC(NROWUJ,*),
     $                   CJDX(*)
      DOUBLE PRECISION   BL(N), BU(N), DX(N), GRAD(N), UGRAD(N), X(N)
      DOUBLE PRECISION   HFORWD(*), HCNTRL(*)
      DOUBLE PRECISION   WRK1(N+NCNLN), WRK2(N+NCNLN), W(LENW)
      EXTERNAL           CONFUN, OBJFUN

************************************************************************
*  NPCHKD  performs the following...
*  (1)  Computes the objective and constraint values OBJF and C.
*  (2)  Evaluates the user-provided gradients in UJAC and UGRAD.
*  (3)  Counts the missing gradients.
*  (4)  Loads the known gradients into GRAD and CJAC.
*  (5)  Checks that the known gradients are programmed correctly.
*  (6)  Computes the missing gradient elements.
*
*  Systems Optimization Laboratory, Stanford University, California.
*  Original version written 4-September-1985.
*  This version of NPCHKD dated  14-July-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET
      COMMON    /SOL5NP/ LVRFYC, JVERFY(4)

      LOGICAL            CENTRL, NEEDFD
      PARAMETER        ( RDUMMY =-11111.0)

      INFORM = 0
      MODE   = 2
      NFDIFF = 0
      NCDIFF = 0
      NCSET  = N*NCNLN

      IF (NCNLN .GT. 0) THEN
*        ===============================================================
*        Compute the constraints and Jacobian matrix.
*        ===============================================================
*        If some derivatives are missing, load the Jacobian with dummy
*        values.  Any elements left unaltered after the call to CONFUN
*        must be estimated.  A record of the missing Jacobian elements
*        is stored in  UJAC.

         NEEDFD = LVLDER .EQ. 0  .OR.  LVLDER .EQ. 1

         IF (NEEDFD) THEN
            DO 100 J = 1, N
               CALL DLOAD ( NCNLN, RDUMMY, UJAC(1,J), 1 )
  100       CONTINUE
         END IF

         CALL ILOAD ( NCNLN, (1), NEEDC, 1 )

         CALL CONFUN( MODE, NCNLN, N, NROWUJ,
     $                NEEDC, X, C, UJAC, NSTATE )
         IF (MODE .LT. 0) GO TO 999

         DO 110 J = 1, N
            CALL DCOPY ( NCNLN, UJAC(1,J), 1, CJAC(1,J), 1 )
  110    CONTINUE

         IF (NEEDFD) THEN

*           Count the number of missing Jacobian elements.

            DO 220 J = 1, N
               DO 210 I = 1, NCNLN
                  IF (UJAC(I,J) .EQ. RDUMMY) NCDIFF = NCDIFF + 1
  210          CONTINUE
  220       CONTINUE

            NCSET = NCSET - NCDIFF
            IF (NSTATE .EQ. 1) THEN
               IF (NCDIFF .EQ. 0) THEN
                  IF (LVLDER .EQ. 0) LVLDER = 2
                  IF (LVLDER .EQ. 1) LVLDER = 3
                  WRITE (NOUT, 1000) LVLDER
               ELSE IF (MSGNP .GT. 0) THEN
                  WRITE (NOUT, 1100) NCSET, N*NCNLN, NCDIFF
               END IF
            END IF
         END IF
      END IF

*     ==================================================================
*     Repeat the procedure above for the objective function.
*     ==================================================================
      NEEDFD = LVLDER .EQ. 0  .OR.  LVLDER .EQ. 2

      IF (NEEDFD)
     $   CALL DLOAD ( N, RDUMMY, UGRAD, 1 )

      CALL OBJFUN( MODE, N, X, OBJF, UGRAD, NSTATE )
      IF (MODE .LT. 0) GO TO 999

      CALL DCOPY ( N, UGRAD, 1, GRAD, 1 )

      IF (NEEDFD) THEN

*        Count the number of missing gradient elements.

         DO 300 J = 1, N
            IF (UGRAD(J) .EQ. RDUMMY) NFDIFF = NFDIFF + 1
  300    CONTINUE

         IF (NSTATE .EQ. 1) THEN
            IF (NFDIFF .EQ. 0) THEN
               IF (LVLDER .EQ. 0) LVLDER = 1
               IF (LVLDER .EQ. 2) LVLDER = 3
               WRITE (NOUT, 2000) LVLDER
            ELSE IF (MSGNP .GT. 0) THEN
               WRITE (NOUT, 2100) N - NFDIFF, N, NFDIFF
            END IF
         END IF
      END IF

      NFUN  = NFUN  + 1
      NGRAD = NGRAD + 1

*     ==================================================================
*     Check whatever gradient elements have been provided.
*     ==================================================================
      IF (LVRFYC .GE. 0) THEN
         IF (NCSET .GT. 0) THEN
            CALL CHKJAC( INFORM, LVLDER, MSGNP,
     $                   NCSET, N, NCNLN, NROWJ, NROWUJ,
     $                   BIGBND, EPSRF, EPSPT3, FDCHK, XNORM,
     $                   CONFUN, NEEDC,
     $                   BL, BU, C, C1, CJAC, UJAC, CJDX,
     $                   DX, WRK1, X, WRK2, W, LENW )
            IF (INFORM .LT. 0) GO TO 800
         END IF

         IF (NFDIFF .LT. N) THEN
            CALL CHKGRD( INFORM, MSGNP, N,
     $                   BIGBND, EPSRF, EPSPT3, FDCHK, OBJF, XNORM,
     $                   OBJFUN,
     $                   BL, BU, GRAD, UGRAD, DX, X, WRK1, W, LENW )
            IF (INFORM .LT. 0) GO TO 800
         END IF
      END IF

      NEEDFD = NCDIFF .GT. 0  .OR.  NFDIFF .GT. 0
      IF (NEEDFD) THEN
*        ===============================================================
*        Compute the missing gradient elements.
*        ===============================================================
         CALL CHFD  ( INFORM, MSGNP, LVLDER,
     $                N, NCNLN, NROWJ, NROWUJ,
     $                BIGBND, EPSRF, FDNORM, OBJF,
     $                OBJFUN, CONFUN, NEEDC,
     $                BL, BU, C, C1, CJDX, CJAC, UJAC,
     $                GRAD, UGRAD, HFORWD, HCNTRL, X,
     $                DX, W, LENW )

         IF (INFORM .LT. 0) GO TO 800

         IF (LFDSET .GT. 0) THEN
            CENTRL = .FALSE.
            CALL NPFD  ( CENTRL, INFORM,
     $                   NROWJ, NROWUJ, N, NCNLN,
     $                   BIGBND, CDINT, FDINT, FDNORM, OBJF,
     $                   CONFUN, OBJFUN, NEEDC,
     $                   BL, BU, C, C1, CJDX, CJAC, UJAC,
     $                   GRAD, UGRAD, HFORWD, HCNTRL, X,
     $                   W, LENW )

            IF (INFORM .LT. 0) GO TO 800
         END IF
      END IF

  800 RETURN

*     The user requested termination.

  999 INFORM = MODE
      RETURN

 1000 FORMAT(//' All Jacobian elements have been set.  ',
     $         ' Derivative level increased to ', I4 )
 1100 FORMAT(//' The user sets ', I6, '   out of', I6,
     $         '   Jacobian elements.'
     $       / ' Each iteration, ', I6,
     $         '   Jacobian elements will be estimated numerically.' )
 2000 FORMAT(//' All objective gradient elements have been set.  ',
     $         ' Derivative level increased to ', I4 )
 2100 FORMAT(//' The user sets ', I6, '   out of', I6,
     $         '   objective gradient elements.'
     $       / ' Each iteration, ', I6,
     $         '   gradient elements will be estimated numerically.' )

*     End of  NPCHKD.

      END
