*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSMULS( PRBTYP,
     $                   MSGLVL, N, NACTIV, NFREE,
     $                   NROWA, NROWT, NUMINF, NZ, NZ1,
     $                   ISTATE, KACTIV, KX, DINKY,
     $                   JSMLST, KSMLST, JINF, JTINY,
     $                   JBIGST, KBIGST, TRULAM,
     $                   A, ANORMS, GQ, RLAMDA, T, WTINF )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*2        PRBTYP
      INTEGER            ISTATE(*), KACTIV(N), KX(N)
      DOUBLE PRECISION   A(NROWA,*), ANORMS(*),
     $                   GQ(N), RLAMDA(N), T(NROWT,*), WTINF(*)

************************************************************************
*     LSMULS  first computes the Lagrange multiplier estimates for the
*     given working set.  It then determines the values and indices of
*     certain significant multipliers.  In this process, the multipliers
*     for inequalities at their upper bounds are adjusted so that a
*     negative multiplier for an inequality constraint indicates non-
*     optimality.  All adjusted multipliers are scaled by the 2-norm
*     of the associated constraint row.  In the following, the term
*     minimum refers to the ordering of numbers on the real line,  and
*     not to their magnitude.
*
*     JSMLST  is the index of the minimum of the set of adjusted
*             multipliers with values less than  - DINKY.  A negative
*             JSMLST defines the index in Q'g of the artificial
*             constraint to be deleted.
*     KSMLST  marks the position of general constraint JSMLST in KACTIV.
*
*     JBIGST  is the index of the largest of the set of adjusted
*             multipliers with values greater than (1 + DINKY).
*     KBIGST  marks its position in KACTIV.
*
*     On exit,  elements 1 thru NACTIV of RLAMDA contain the unadjusted
*     multipliers for the general constraints.  Elements NACTIV onwards
*     of RLAMDA contain the unadjusted multipliers for the bounds.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 31-October-1984.
*     This version of LSMULS dated  30-June-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      INTRINSIC          ABS, MIN
      PARAMETER        ( ZERO   =0.0D+0,ONE    =1.0D+0 )

      NFIXED =   N - NFREE

      JSMLST =   0
      KSMLST =   0
      SMLLST = - DINKY

      TINYLM =   DINKY
      JTINY  =   0

      JBIGST =   0
      KBIGST =   0
      BIGGST =   ONE + DINKY

      IF (NZ1 .LT. NZ) THEN
*        ---------------------------------------------------------------
*        Compute JSMLST for the artificial constraints.
*        ---------------------------------------------------------------
         DO 100 J = NZ1+1, NZ
            RLAM = - ABS( GQ(J) )
            IF (RLAM .LT. SMLLST) THEN
               SMLLST =   RLAM
               JSMLST = - J
            ELSE IF (RLAM .LT. TINYLM) THEN
               TINYLM =   RLAM
               JTINY  =   J
            END IF
  100    CONTINUE

         IF (MSGLVL .GE. 20)
     $      WRITE (NOUT, 1000) (GQ(K), K=NZ1+1,NZ)

      END IF

*     ------------------------------------------------------------------
*     Compute JSMLST for regular constraints and temporary bounds.
*     ------------------------------------------------------------------
*     First, compute the Lagrange multipliers for the general
*     constraints in the working set, by solving  T'*lamda = Y'g.

      IF (N .GT. NZ)
     $   CALL DCOPY ( N-NZ, GQ(NZ+1), 1, RLAMDA, 1 )
      IF (NACTIV .GT. 0)
     $   CALL CMTSOL( 2, NROWT, NACTIV, T(1,NZ+1), RLAMDA )

*     -----------------------------------------------------------------
*     Now set elements NACTIV, NACTIV+1,... of  RLAMDA  equal to
*     the multipliers for the bound constraints.
*     -----------------------------------------------------------------
      DO 190 L = 1, NFIXED
         J     = KX(NFREE+L)
         BLAM  = RLAMDA(NACTIV+L)
         DO 170 K = 1, NACTIV
            I    = KACTIV(K)
            BLAM = BLAM - A(I,J)*RLAMDA(K)
  170    CONTINUE
         RLAMDA(NACTIV+L) = BLAM
  190 CONTINUE

*     -----------------------------------------------------------------
*     Find JSMLST and KSMLST.
*     -----------------------------------------------------------------
      DO 330 K = 1, N - NZ
         IF (K .GT. NACTIV) THEN
            J = KX(NZ+K)
         ELSE
            J = KACTIV(K) + N
         END IF

         IS   = ISTATE(J)

         I    = J - N
         IF (J .LE. N) ANORMJ = ONE
         IF (J .GT. N) ANORMJ = ANORMS(I)

         RLAM = RLAMDA(K)

*        Change the sign of the estimate if the constraint is in
*        the working set at its upper bound.

         IF (IS .EQ. 2) RLAM =      - RLAM
         IF (IS .EQ. 3) RLAM =   ABS( RLAM )
         IF (IS .EQ. 4) RLAM = - ABS( RLAM )

         IF (IS .NE. 3) THEN
            SCDLAM = RLAM * ANORMJ
            IF      (SCDLAM .LT. SMLLST) THEN
               SMLLST = SCDLAM
               JSMLST = J
               KSMLST = K
            ELSE IF (SCDLAM .LT. TINYLM) THEN
               TINYLM = SCDLAM
               JTINY  = J
            END IF
         END IF

         IF (NUMINF .GT. 0  .AND.  J .GT. JINF) THEN
            SCDLAM = RLAM/WTINF(J)
            IF (SCDLAM .GT. BIGGST) THEN
               BIGGST = SCDLAM
               TRULAM = RLAMDA(K)
               JBIGST = J
               KBIGST = K
            END IF
         END IF
  330 CONTINUE

*     -----------------------------------------------------------------
*     If required, print the multipliers.
*     -----------------------------------------------------------------
      IF (MSGLVL .GE. 20) THEN
         IF (NFIXED .GT. 0)
     $      WRITE (NOUT, 1100) PRBTYP, (KX(NFREE+K),
     $                         RLAMDA(NACTIV+K), K=1,NFIXED)
         IF (NACTIV .GT. 0)
     $      WRITE (NOUT, 1200) PRBTYP, (KACTIV(K),
     $                         RLAMDA(K), K=1,NACTIV)
      END IF

      IF (LSDBG  .AND.  ILSDBG(1) .GT. 0) THEN
         WRITE (NOUT, 9000) JSMLST, SMLLST, KSMLST
         WRITE (NOUT, 9100) JBIGST, BIGGST, KBIGST
         WRITE (NOUT, 9200) JTINY , TINYLM
      END IF

      RETURN

 1000 FORMAT(/ ' Multipliers for the artificial constraints        '
     $       / 4(5X, 1PE11.2))
 1100 FORMAT(/ ' Multipliers for the ', A2, ' bound  constraints   '
     $       / 4(I5, 1PE11.2))
 1200 FORMAT(/ ' Multipliers for the ', A2, ' linear constraints   '
     $       / 4(I5, 1PE11.2))
 9000 FORMAT(/ ' //LSMULS//  JSMLST     SMLLST     KSMLST (Scaled) '
     $       / ' //LSMULS//  ', I6, 1PE11.2, 5X, I6 )
 9100 FORMAT(  ' //LSMULS//  JBIGST     BIGGST     KBIGST (Scaled) '
     $       / ' //LSMULS//  ', I6, 1PE11.2, 5X, I6 )
 9200 FORMAT(  ' //LSMULS//   JTINY     TINYLM                     '
     $       / ' //LSMULS//  ', I6, 1PE11.2)

*     End of  LSMULS.

      END
