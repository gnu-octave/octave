*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMCHK ( NERROR, MSGLVL, COLD, USERKX,
     $                   LIWORK, LWORK, LITOTL, LWTOTL,
     $                   N, NCLIN, NCNLN,
     $                   ISTATE, KX, NAMED, NAMES, LENNAM,
     $                   BL, BU, X )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*8        NAMES(*)
      LOGICAL            COLD, NAMED, USERKX
      INTEGER            ISTATE(N+NCLIN+NCNLN), KX(N)
      DOUBLE PRECISION   BL(N+NCLIN+NCNLN), BU(N+NCLIN+NCNLN), X(N)

      COMMON    /SOL1CM/ NOUT

************************************************************************
*  CMCHK   checks the data input to various optimizers.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 66 version written 10-May-1980.
*  Fortran 77 version written  5-October-1984.
*  This version of CMCHK dated  23-January-1987.
************************************************************************
      LOGICAL            OK
      INTRINSIC          ABS
      PARAMETER        ( ZERO   =  0.0D+0 , ONE    =  1.0D+0 )

      CHARACTER*5        ID(3)
      DATA                ID(1)   ,  ID(2)   ,  ID(3)
     $                 / 'VARBL'  , 'LNCON'  , 'NLCON'   /

      NERROR = 0

*     ------------------------------------------------------------------
*     Check that there is enough workspace to solve the problem.
*     ------------------------------------------------------------------
      OK     = LITOTL .LE. LIWORK  .AND.  LWTOTL .LE. LWORK
      IF (.NOT. OK)  THEN
         WRITE (NOUT, 1100) LIWORK, LWORK, LITOTL, LWTOTL
         NERROR = NERROR + 1
         WRITE (NOUT, 1110)
      ELSE IF (MSGLVL .GT. 0)  THEN
         WRITE (NOUT, 1100) LIWORK, LWORK, LITOTL, LWTOTL
      END IF

      IF (USERKX) THEN
*        ---------------------------------------------------------------
*        Check for a valid KX.
*        ---------------------------------------------------------------
         IFAIL = 1
         CALL CMPERM( KX, 1, N, IFAIL )
         IF (IFAIL .NE. 0) THEN
            WRITE (NOUT, 1300)
            NERROR = NERROR + 1
         END IF
      END IF

*     ------------------------------------------------------------------
*     Check the bounds on all variables and constraints.
*     ------------------------------------------------------------------
      DO 200 J = 1, N+NCLIN+NCNLN
         B1     = BL(J)
         B2     = BU(J)
         OK     = B1 .LE. B2
         IF (.NOT. OK)  THEN
            NERROR = NERROR + 1
            IF (J .GT. N+NCLIN)  THEN
               K  = J - N - NCLIN
               L  = 3
            ELSE IF (J .GT. N)  THEN
               K  = J - N
               L  = 2
            ELSE
               K = J
               L = 1
            END IF
            IF (.NOT. NAMED) WRITE (NOUT, 1200) ID(L), K, B1, B2
            IF (      NAMED) WRITE (NOUT, 1210) NAMES(J), B1, B2
         END IF
  200 CONTINUE

*     ------------------------------------------------------------------
*     If warm start, check  ISTATE.
*     ------------------------------------------------------------------
      IF (.NOT. COLD) THEN
         DO 420 J = 1, N+NCLIN+NCNLN
            IS     = ISTATE(J)
            OK     = IS .GE. (- 2)   .AND.   IS .LE. 4
            IF (.NOT. OK)  THEN
               NERROR = NERROR + 1
               WRITE (NOUT, 1500) J, IS
            END IF
  420    CONTINUE
      END IF

      RETURN

 1100 FORMAT(/ ' Workspace provided is     IW(', I6,
     $         '),  W(', I6, ').' /
     $         ' To solve problem we need  IW(', I6,
     $         '),  W(', I6, ').')
 1110 FORMAT(/ ' XXX  Not enough workspace to solve problem.')
 1200 FORMAT(/ ' XXX  The bounds on  ', A5, I3,
     $         '  are inconsistent.   BL =', G16.7, '   BU =', G16.7)
 1210 FORMAT(/ ' XXX  The bounds on  ', A8,
     $         '  are inconsistent.   BL =', G16.7, '   BU =', G16.7)
 1300 FORMAT(/ ' XXX  KX has not been supplied as a valid',
     $         '  permutation.' )
 1500 FORMAT(/ ' XXX  Component', I5, '  of  ISTATE  is out of',
     $         ' range...', I10)

*     End of  CMCHK .

      END
