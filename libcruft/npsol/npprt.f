*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPPRT ( KTCOND, CONVRG, LSUMRY, MSGNP, MSGQP,
     $                   NROWR, NROWT, N, NCLIN, NCNLN,
     $                   NCTOTL, NACTIV, LINACT, NLNACT, NZ, NFREE,
     $                   MAJITS, MINITS, ISTATE, ALFA, NFUN,
     $                   CONDHZ, CONDH, CONDT, OBJALF, OBJF,
     $                   GFNORM, GZNORM, CVNORM,
     $                   AX, C, R, T, VIOLN, X, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*4        LSUMRY
      LOGICAL            KTCOND(2), CONVRG
      INTEGER            ISTATE(NCTOTL)
      DOUBLE PRECISION   AX(*), C(*), R(NROWR,*), T(NROWT,*), VIOLN(*)
      DOUBLE PRECISION   X(N)
      DOUBLE PRECISION   WORK(N)
************************************************************************
*  NPPRT  prints various levels of output for NPCORE.
*
*           Msg        Cumulative result
*           ---        -----------------
*
*        le   0        no output.
*
*        eq   1        nothing now (but full output later).
*
*        eq   5        one terse line of output.
*
*        ge  10        same as 5 (but full output later).
*
*        ge  20        objective function,  x,  Ax  and  c.
*
*        ge  30        diagonals of  T  and  R.
*
*  Debug print is performed depending on the logical variable NPDBG.
*  NPDBG is set true when IDBG major iterations have been performed.
*  At this point,  printing is done according to a string of binary
*  digits of the form CLSVT (stored in the integer array INPDBG).
*
*  C  set 'on'  gives detailed information from the checking routines.
*  L  set 'on'  gives information from the linesearch.
*  S  set 'on'  gives information from the maximum step routine NPALF.
*  V  set 'on'  gives various vectors in  NPCORE  and its auxiliaries.
*  T  set 'on'  gives a trace of which routine was called and an
*               indication of the progress of the run.
*
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 66 version written November-1982.
*  This version of  NPPRT  dated  14-November-1985.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      LOGICAL            PHEAD
      EXTERNAL           DNRM2

      IF (MSGNP .GE. 20) WRITE (NOUT, 1000) MAJITS

      IF (MSGNP  .GE. 5) THEN
*        ---------------------------------------------------------------
*        Print heading and terse line.
*        ---------------------------------------------------------------
         PHEAD = MSGQP .GT. 0  .OR.  MAJITS .EQ. 0

         IF (NCNLN .EQ. 0) THEN
            IF (PHEAD) WRITE (NOUT, 1100)
            WRITE (NOUT, 1300) MAJITS, MINITS, ALFA, NFUN, OBJALF,
     $                         N-NFREE, LINACT, NZ,
     $                         GFNORM, GZNORM, CONDH, CONDHZ, CONDT,
     $                         CONVRG, KTCOND(1), KTCOND(2), LSUMRY

         ELSE
            IF (PHEAD) WRITE (NOUT, 1110)
            WRITE (NOUT, 1310) MAJITS, MINITS, ALFA, NFUN, OBJALF,
     $                         N-NFREE, LINACT, NLNACT, NZ,
     $                         GFNORM, GZNORM, CONDH, CONDHZ, CONDT,
     $                         CVNORM, SCALE*RHONRM,
     $                         CONVRG, KTCOND(1), KTCOND(2), LSUMRY
         END IF

         IF (MSGNP .GE. 20) THEN
            IF (NCNLN .EQ. 0) THEN
               WRITE (NOUT, 1400) OBJF
            ELSE
               CVIOLS = DNRM2 ( NCNLN, VIOLN, 1 )
               WRITE (NOUT, 1410) OBJF, CVIOLS
            END IF

*           ------------------------------------------------------------
*           Print the constraint values.
*           ------------------------------------------------------------
            WRITE (NOUT, 2000)
            WRITE (NOUT, 2100) (X(J), ISTATE(J), J=1,N)
            IF (NCLIN .GT. 0)
     $         WRITE (NOUT, 2200) (AX(K), ISTATE(N+K),       K=1,NCLIN )
            IF (NCNLN .GT. 0)
     $         WRITE (NOUT, 2300) (C(K) , ISTATE(N+NCLIN+K), K=1,NCNLN )

            IF (MSGNP .GE. 30) THEN
*              ---------------------------------------------------------
*              Print the diagonals of  T  and  R.
*              ---------------------------------------------------------
               INCT   = NROWT - 1
               IF (NACTIV .GT. 0) THEN
                  CALL DCOPY( NACTIV, T(NACTIV,NZ+1), INCT, WORK, 1 )
                  WRITE (NOUT, 3000) (WORK(J), J=1,NACTIV)
               END IF
               WRITE (NOUT, 3100) (R(J,J), J=1,N)
            END IF
         END IF
      END IF

      IF (MSGNP .GE. 20) WRITE (NOUT, 5000)

      LSUMRY(1:2) = '  '
      LSUMRY(4:4) = ' '

      RETURN

 1000 FORMAT(/// ' Major iteration', I5
     $       /   ' ====================' )
 1100 FORMAT(//  '  Itn', ' ItQP', '     Step',
     $           '  Nfun', '     Objective', ' Bnd', ' Lin', '  Nz',
     $           '  Norm Gf', '  Norm Gz', '  Cond H', ' Cond Hz',
     $           '  Cond T', ' Conv' )
 1110 FORMAT(//  '  Itn', ' ItQP', '     Step',
     $           '  Nfun', '         Merit', ' Bnd', ' Lin',
     $           ' Nln', '  Nz',
     $           '  Norm Gf', '  Norm Gz', '  Cond H', ' Cond Hz',
     $           '  Cond T' , '   Norm C', '  Penalty', ' Conv' )
 1300 FORMAT(2I5, 1PE9.1, I6, 1PE14.6, 3I4, 1P2E9.1, 1P3E8.0,
     $                        1X, L1, 1X, 2L1, A4 )
 1310 FORMAT(2I5, 1PE9.1, I6, 1PE14.6, 4I4, 1P2E9.1, 1P3E8.0,
     $            1P2E9.1,    1X, L1, 1X, 2L1, A4 )
 1400 FORMAT(/ ' Nonlinear objective value = ', 1PE15.6 )
 1410 FORMAT(/ ' Nonlinear objective value = ', 1PE15.6, '   Norm of',
     $         ' the nonlinear constraint violations = ', 1PE15.6 )
 2000 FORMAT(/ ' Values of the constraints and their predicted status'
     $       / ' ----------------------------------------------------')
 2100 FORMAT(/ ' Variables                  '/ (1X, 5(1PE15.6, I4)))
 2200 FORMAT(/ ' General linear constraints '/ (1X, 5(1PE15.6, I4)))
 2300 FORMAT(/ ' Nonlinear constraints      '/ (1X, 5(1PE15.6, I4)))
 3000 FORMAT(/ ' Diagonals of  T  =         '/       (1P5E15.6))
 3100 FORMAT(/ ' Diagonals of  R  =         '/       (1P5E15.6))
 5000 FORMAT(  ' ==================================================',
     $         '======================================='///)

*     End of  NPPRT .

      END
