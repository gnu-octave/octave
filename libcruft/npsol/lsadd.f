*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  LSSUBS FORTRAN
*
*     LSADD    LSADDS   LSBNDS   LSCHOL   LSCORE   LSCRSH   LSDEL
*     LSDFLT   LSFEAS   LSFILE   LSGETP   LSGSET   LSKEY    LSLOC
*     LSMOVE   LSMULS   LSOPTN   LSPRT    LSSETX   LSSOL
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSADD ( UNITQ,
     $                   INFORM, IFIX, IADD, JADD,
     $                   NACTIV, NZ, NFREE, NRANK, NRES, NGQ,
     $                   N, NROWA, NQ, NROWR, NROWT,
     $                   KX, CONDMX,
     $                   A, R, T, RES, GQ, ZY, WRK1, WRK2 )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            KX(N)
      DOUBLE PRECISION   A(NROWA,*), R(NROWR,*), T(NROWT,*),
     $                   RES(N,*), GQ(N,*), ZY(NQ,*)
      DOUBLE PRECISION   WRK1(N), WRK2(N)
************************************************************************
*  LSADD   updates the factorization,  A(free) * (Z Y) = (0 T),  when a
*  constraint is added to the working set.  If  NRANK .gt. 0, the
*  factorization  ( R ) = PWQ  is also updated,  where  W  is the
*                 ( 0 )
*  least squares matrix,  R  is upper-triangular,  and  P  is an
*  orthogonal matrix.  The matrices  W  and  P  are not stored.
*
*  There are three separate cases to consider (although each case
*  shares code with another)...
*
*  (1) A free variable becomes fixed on one of its bounds when there
*      are already some general constraints in the working set.
*
*  (2) A free variable becomes fixed on one of its bounds when there
*      are only bound constraints in the working set.
*
*  (3) A general constraint (corresponding to row  IADD  of  A) is
*      added to the working set.
*
*  In cases (1) and (2), we assume that  KX(IFIX) = JADD.
*  In all cases,  JADD  is the index of the constraint being added.
*
*  If there are no general constraints in the working set,  the
*  matrix  Q = (Z Y)  is the identity and will not be touched.
*
*  If  NRES .GT. 0,  the row transformations are applied to the rows of
*  the  (N by NRES)  matrix  RES.
*  If  NGQ .GT. 0,  the column transformations are applied to the
*  columns of the  (NGQ by N)  matrix  GQ'.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original version written 31-October--1984.
*  This version of LSADD dated 29-December-1985.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      LOGICAL            BOUND , OVERFL
      EXTERNAL           DDOT  , DDIV  , DNRM2
      INTRINSIC          MAX   , MIN
      PARAMETER         (ZERO = 0.0D+0, ONE = 1.0D+0)

*     If the condition estimator of the updated factors is greater than
*     CONDBD,  a warning message is printed.

      CONDBD = ONE / EPSPT9

      OVERFL = .FALSE.
      BOUND  = JADD .LE. N
      IF (BOUND) THEN
*        ===============================================================
*        A simple bound has entered the working set.  IADD  is not used.
*        ===============================================================
         IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $      WRITE (NOUT, 1010) NACTIV, NZ, NFREE, IFIX, JADD, UNITQ
         NANEW = NACTIV

         IF (UNITQ) THEN

*           Q  is not stored, but KX defines an ordering of the columns
*           of the identity matrix that implicitly define  Q.
*           Reorder KX so that variable IFIX is moved to position
*           NFREE+1 and variables IFIX+1,...,NFREE+1 are moved one
*           position to the left.

            CALL DLOAD ( NFREE, (ZERO), WRK1, 1 )
            WRK1(IFIX) = ONE

            DO 100 I = IFIX, NFREE-1
               KX(I) = KX(I+1)
  100       CONTINUE
         ELSE
*           ------------------------------------------------------------
*           Q  is stored explicitly.
*           ------------------------------------------------------------
*           Set  WRK1 = the  (IFIX)-th  row of  Q.
*           Move the  (NFREE)-th  row of  Q  to position  IFIX.

            CALL DCOPY ( NFREE, ZY(IFIX,1), NQ, WRK1, 1 )
            IF (IFIX .LT. NFREE) THEN
               CALL DCOPY ( NFREE, ZY(NFREE,1), NQ, ZY(IFIX,1), NQ )
               KX(IFIX) = KX(NFREE)
            END IF
         END IF
         KX(NFREE) = JADD
      ELSE
*        ===============================================================
*        A general constraint has entered the working set.
*        IFIX  is not used.
*        ===============================================================
         IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $      WRITE (NOUT, 1020) NACTIV, NZ, NFREE, IADD, JADD, UNITQ

         NANEW  = NACTIV + 1

*        Transform the incoming row of  A  by  Q'.

         CALL DCOPY ( N, A(IADD,1), NROWA, WRK1, 1 )
         CALL CMQMUL( 8, N, NZ, NFREE, NQ, UNITQ, KX, WRK1, ZY, WRK2)

*        Check that the incoming row is not dependent upon those
*        already in the working set.

         DTNEW  = DNRM2 ( NZ, WRK1, 1 )
         IF (NACTIV .EQ. 0) THEN

*           This is the only general constraint in the working set.

            COND   = DDIV  ( ASIZE, DTNEW, OVERFL )
            TDTMAX = DTNEW
            TDTMIN = DTNEW
         ELSE

*           There are already some general constraints in the working
*           set. Update the estimate of the condition number.

            TDTMAX = MAX( DTNEW, DTMAX )
            TDTMIN = MIN( DTNEW, DTMIN )
            COND   = DDIV  ( TDTMAX, TDTMIN, OVERFL )
         END IF

         IF (COND .GT. CONDMX  .OR.  OVERFL) GO TO 900

         IF (UNITQ) THEN

*           This is the first general constraint to be added.
*           Set  Q = I.

            DO 200 J = 1, NFREE
               CALL DLOAD ( NFREE, (ZERO), ZY(1,J), 1 )
               ZY(J,J) = ONE
  200       CONTINUE
            UNITQ  = .FALSE.
         END IF
      END IF

      NZERO  = NZ - 1
      IF (BOUND) NZERO = NFREE - 1

*     ------------------------------------------------------------------
*     Use a sequence of 2*2 column transformations to reduce the
*     first NZERO elements of WRK1 to zero.  This affects ZY, except
*     when UNITQ is true.  The transformations may also be applied
*     to R, T and GQ'.
*     ------------------------------------------------------------------
      LROWR  = N
      NELM   = 1
      IROWT  = NACTIV

      DO 300 K = 1, NZERO

*        Compute the transformation that reduces WRK1(K) to zero,
*        then apply it to the relevant columns of  Z  and  GQ'.

         CALL DROT3G( WRK1(K+1), WRK1(K), CS, SN )
         IF (.NOT. UNITQ)
     $      CALL DROT3 ( NFREE, ZY(1,K+1), 1, ZY(1,K), 1, CS, SN )
         IF (NGQ .GT. 0)
     $      CALL DROT3 ( NGQ  , GQ(K+1,1), N, GQ(K,1), N, CS, SN )

         IF (K .GE. NZ  .AND.  NACTIV .GT. 0) THEN

*           Apply the rotation to  T.

            T(IROWT,K) = ZERO
            CALL DROT3 ( NELM, T(IROWT,K+1), 1, T(IROWT,K), 1, CS, SN )
            NELM  = NELM  + 1
            IROWT = IROWT - 1
         END IF

         IF (NRANK .GT. 0) THEN

*           Apply the same transformation to the columns of R.
*           This generates a subdiagonal element in R that must be
*           eliminated by a row rotation.

            IF (K .LT. NRANK) R(K+1,K) = ZERO
            LCOL   = MIN( K+1, NRANK )

            CALL DROT3 ( LCOL, R(1,K+1), 1, R(1,K), 1, CS, SN )
            IF (K .LT. NRANK) THEN
               CALL DROT3G( R(K,K), R(K+1,K), CS, SN )
               LROWR  = LROWR - 1
               CALL DROT3 ( LROWR,   R(K,K+1)  , NROWR,
     $                               R(K+1,K+1), NROWR, CS, SN )

               IF (NRES .GT. 0)
     $            CALL DROT3 ( NRES, RES(K,1)  , N    ,
     $                               RES(K+1,1), N    , CS, SN )
            END IF
         END IF
  300 CONTINUE

      IF (BOUND) THEN

*        The last row and column of ZY has been transformed to plus
*        or minus the unit vector E(NFREE).  We can reconstitute the
*        columns of GQ and R corresponding to the new fixed variable.

         IF (WRK1(NFREE) .LT. ZERO) THEN
            NFMIN = MIN( NRANK, NFREE )
            IF (NFMIN .GT. 0) CALL DSCAL ( NFMIN, -ONE, R(1,NFREE) , 1 )
            IF (NGQ   .GT. 0) CALL DSCAL ( NGQ  , -ONE, GQ(NFREE,1), N )
         END IF

*        ---------------------------------------------------------------
*        The diagonals of T have been altered.  Recompute the
*        largest and smallest values.
*        ---------------------------------------------------------------
         IF (NACTIV .GT. 0) THEN
            CALL DCOND( NACTIV, T(NACTIV,NZ), NROWT-1, TDTMAX, TDTMIN )
            COND   = DDIV  ( TDTMAX, TDTMIN, OVERFL )
         END IF
      ELSE
*        ---------------------------------------------------------------
*        General constraint.  Install the new row of T.
*        ---------------------------------------------------------------
         CALL DCOPY ( NANEW, WRK1(NZ), 1, T(NANEW,NZ), NROWT )
      END IF

*     ==================================================================
*     Prepare to exit.  Check the magnitude of the condition estimator.
*     ==================================================================
  900 IF (NANEW .GT. 0) THEN
         IF (COND .LT. CONDMX  .AND.  .NOT. OVERFL) THEN

*           The factorization has been successfully updated.

            INFORM = 0
            DTMAX  = TDTMAX
            DTMIN  = TDTMIN
            IF (COND .GE. CONDBD) WRITE (NOUT, 2000) JADD
         ELSE

*           The proposed working set appears to be linearly dependent.

            INFORM = 1
            IF (LSDBG  .AND.  ILSDBG(1) .GT. 0) THEN
               WRITE( NOUT, 3000 )
               IF (BOUND) THEN
                  WRITE (NOUT, 3010) ASIZE, DTMAX, DTMIN
               ELSE
                  IF (NACTIV .GT. 0) THEN
                     WRITE (NOUT, 3020) ASIZE, DTMAX, DTMIN, DTNEW
                  ELSE
                     WRITE (NOUT, 3030) ASIZE, DTNEW
                  END IF
               END IF
            END IF
         END IF
      END IF

      RETURN

 1010 FORMAT(/ ' //LSADD //  Simple bound added.'
     $       / ' //LSADD //  NACTIV    NZ NFREE  IFIX  JADD UNITQ'
     $       / ' //LSADD //  ', 5I6, L6 )
 1020 FORMAT(/ ' //LSADD //  General constraint added.           '
     $       / ' //LSADD //  NACTIV    NZ NFREE  IADD  JADD UNITQ'
     $       / ' //LSADD //  ', 5I6, L6 )
 2000 FORMAT(/ ' XXX  Serious ill-conditioning in the working set',
     $         ' after adding constraint ',  I5
     $       / ' XXX  Overflow may occur in subsequent iterations.'//)
 3000 FORMAT(/ ' //LSADD //  Dependent constraint rejected.' )
 3010 FORMAT(/ ' //LSADD //     ASIZE     DTMAX     DTMIN        '
     $       / ' //LSADD //', 1P3E10.2 )
 3020 FORMAT(/ ' //LSADD //     ASIZE     DTMAX     DTMIN     DTNEW'
     $       / ' //LSADD //', 1P4E10.2 )
 3030 FORMAT(/ ' //LSADD //     ASIZE     DTNEW'
     $       / ' //LSADD //', 1P2E10.2 )

*     End of  LSADD .

      END
