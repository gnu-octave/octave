*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSPRT ( PRBTYP, PRNT1, ISDEL, ITER, JADD, JDEL,
     $                   MSGLVL, NACTIV, NFREE, N, NCLIN,
     $                   NRANK, NROWR, NROWT, NZ, NZ1, ISTATE,
     $                   ALFA, CONDRZ, CONDT, GFNORM, GZNORM, GZ1NRM,
     $                   NUMINF, SUMINF, CTX, SSQ,
     $                   AX, R, T, X, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*2        PRBTYP
      LOGICAL            PRNT1
      INTEGER            ISTATE(*)
      DOUBLE PRECISION   AX(*), R(NROWR,*), T(NROWT,*), X(N)
      DOUBLE PRECISION   WORK(N)

************************************************************************
*  LSPRT  prints various levels of output for  LSCORE.
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
*        ge  20        constraint status,  x  and  Ax.
*
*        ge  30        diagonals of  T  and  R.
*
*
*  Debug printing is performed depending on the logical variable  LSDBG.
*  LSDBG  is set true when  IDBG  major iterations have been performed.
*  At this point,  printing is done according to a string of binary
*  digits of the form  SVT  (stored in the integer array  ILSDBG).
*
*  S  set 'on'  gives information from the maximum step routine  CMALF.
*  V  set 'on'  gives various vectors in  LSCORE  and its auxiliaries.
*  T  set 'on'  gives a trace of which routine was called and an
*               indication of the progress of the run.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version written 31-October-1984.
*  This version of LSPRT dated 14-January-1985.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      CHARACTER*2        LADD, LDEL
      CHARACTER*2        LSTATE(0:5)
      DATA               LSTATE(0), LSTATE(1), LSTATE(2)
     $                  /'  '     , 'L '     , 'U '     /
      DATA               LSTATE(3), LSTATE(4), LSTATE(5)
     $                  /'E '     , 'T '     , 'Z '     /

      IF (MSGLVL .GE. 15) WRITE (NOUT, 1000) PRBTYP, ITER

      IF (MSGLVL .GE. 5) THEN
         IF      (JDEL .GT. 0) THEN
            KDEL =   ISDEL
         ELSE IF (JDEL .LT. 0) THEN
            JDEL = - JDEL
            KDEL =   5
         ELSE
            KDEL =   0
         END IF

         IF (JADD .GT. 0) THEN
            KADD = ISTATE(JADD)
         ELSE
            KADD = 0
         END IF

         LDEL   = LSTATE(KDEL)
         LADD   = LSTATE(KADD)

         IF (NUMINF .GT. 0) THEN
            OBJ    = SUMINF
         ELSE
            OBJ    = SSQ + CTX
         END IF

*        ---------------------------------------------------------------
*        Print the terse line.
*        ---------------------------------------------------------------
         IF (NRANK .EQ. 0) THEN
            IF (PRNT1  .OR.  MSGLVL .GE. 15) WRITE (NOUT, 1100)
            WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD,
     $                         ALFA, NUMINF, OBJ, N-NFREE, NACTIV,
     $                         NZ, NZ1, GFNORM, GZ1NRM, CONDT
         ELSE
            IF (PRNT1  .OR.  MSGLVL .GE. 15) WRITE (NOUT, 1110)
            WRITE (NOUT, 1200) ITER, JDEL, LDEL, JADD, LADD,
     $                         ALFA, NUMINF, OBJ, N-NFREE, NACTIV,
     $                         NZ, NZ1, GFNORM, GZ1NRM, CONDT, CONDRZ
         END IF

         IF (MSGLVL .GE. 20) THEN
            WRITE (NOUT, 2000) PRBTYP
            WRITE (NOUT, 2100) (X(J) , ISTATE(J)  ,  J=1,N)
            IF (NCLIN .GT. 0)
     $      WRITE (NOUT, 2200) (AX(K), ISTATE(N+K), K=1,NCLIN )

            IF (MSGLVL .GE. 30) THEN
*              ---------------------------------------------------------
*              Print the diagonals of  T  and  R.
*              ---------------------------------------------------------
               IF (NACTIV .GT. 0) THEN
                  CALL DCOPY ( NACTIV, T(NACTIV,NZ+1), NROWT-1, WORK,1 )
                  WRITE (NOUT, 3000) PRBTYP, (WORK(J), J=1,NACTIV)
               END IF
               IF (NRANK  .GT. 0)
     $            WRITE (NOUT, 3100) PRBTYP, (R(J,J) , J=1,NRANK )
            END IF
            WRITE (NOUT, 5000)
         END IF
      END IF

      PRNT1 = .FALSE.

      RETURN

 1000 FORMAT(/// ' ', A2, ' iteration', I5
     $         / ' =================' )
 1100 FORMAT(// '  Itn Jdel  Jadd      Step',
     $          ' Ninf  Sinf/Objective', '  Bnd', '  Lin', '    Nz',
     $          '   Nz1   Norm Gf  Norm Gz1   Cond T' )
 1110 FORMAT(// '  Itn Jdel  Jadd      Step',
     $          ' Ninf  Sinf/Objective', '  Bnd', '  Lin', '    Nz',
     $          '   Nz1   Norm Gf  Norm Gz1   Cond T Cond Rz1' )
 1200 FORMAT(I5, I5, A1, I5, A1, 1PE9.1, I5, 1X, 1PE15.6, 2I5,
     $       2I6, 1P2E10.2, 1P2E9.1 )
 2000 FORMAT(/ ' Values and status of the ', A2, ' constraints'
     $       / ' ---------------------------------------' )
 2100 FORMAT(/ ' Variables...'                 /   (1X, 5(1PE15.6, I5)))
 2200 FORMAT(/ ' General linear constraints...'/   (1X, 5(1PE15.6, I5)))
 3000 FORMAT(/ ' Diagonals of ' , A2,' working set factor T'/(1P5E15.6))
 3100 FORMAT(/ ' Diagonals of ' , A2, ' triangle R         '/(1P5E15.6))
 5000 FORMAT(/// ' ---------------------------------------------------',
     $           '--------------------------------------------' )

*     End of  LSPRT .

      END
