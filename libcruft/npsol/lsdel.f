*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSDEL ( UNITQ,
     $                   N, NACTIV, NFREE, NRES, NGQ, NZ, NZ1,
     $                   NROWA, NQ, NROWR, NROWT, NRANK,
     $                   JDEL, KDEL, KACTIV, KX,
     $                   A, RES, R, T, GQ, ZY, WORK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            KACTIV(N), KX(N)
      DOUBLE PRECISION   A(NROWA,*), RES(N,*), R(NROWR,*), T(NROWT,*),
     $                   GQ(N,*), ZY(NQ,*)
      DOUBLE PRECISION   WORK(N)

************************************************************************
*     LSDEL   updates the least-squares factor R and the factorization
*     A(free) (Z Y) = (0 T) when a regular, temporary or artificial
*     constraint is deleted from the working set.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 31-October-1984.
*     This version of LSDEL dated 10-June-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      EXTERNAL           IDAMAX
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      IF (JDEL .GT. 0) THEN

*        Regular constraint or temporary bound deleted.

         IF (JDEL .LE. N) THEN

*           Case 1.  A simple bound has been deleted.
*           =======  Columns NFREE+1 and IR of R must be swapped.

            IR     = NZ    + KDEL
            IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $         WRITE (NOUT, 1100) NACTIV, NZ, NFREE, IR, JDEL, UNITQ

            IBEGIN = 1
            NFREE  = NFREE + 1
            IF (NFREE .LT. IR) THEN
               KX(IR)    = KX(NFREE)
               KX(NFREE) = JDEL
               IF (NRANK .GT. 0)
     $            CALL CMRSWP( N, NRES, NRANK, NROWR, NFREE, IR,
     $                         R, RES, WORK )
               CALL DSWAP ( NGQ, GQ(NFREE,1), N, GQ(IR,1), N )
            END IF

            IF (.NOT. UNITQ) THEN

*              Copy the incoming column of  A(free)  into the end of T.

               DO 130 KA = 1, NACTIV
                  I = KACTIV(KA)
                  T(KA,NFREE) = A(I,JDEL)
  130          CONTINUE

*              Expand Q by adding a unit row and column.

               IF (NFREE .GT. 1) THEN
                  CALL DLOAD ( NFREE-1, ZERO, ZY(NFREE,1), NQ )
                  CALL DLOAD ( NFREE-1, ZERO, ZY(1,NFREE), 1  )
               END IF
               ZY(NFREE,NFREE) = ONE
            END IF
         ELSE

*           Case 2.  A general constraint has been deleted.
*           =======

            IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $         WRITE (NOUT, 1200) NACTIV, NZ, NFREE, KDEL, JDEL, UNITQ

            IBEGIN = KDEL
            NACTIV = NACTIV - 1

*           Delete a row of T and move the ones below it up.

            DO 220 I = KDEL, NACTIV
               KACTIV(I) = KACTIV(I+1)
               LD        = NFREE - I
               CALL DCOPY ( I+1, T(I+1,LD), NROWT, T(I,LD), NROWT )
  220       CONTINUE
         END IF

*        ---------------------------------------------------------------
*        Eliminate the super-diagonal elements of  T,
*        using a backward sweep of 2*2 transformations.
*        ---------------------------------------------------------------
         K     = NFREE  - IBEGIN
         L     = NACTIV - IBEGIN
         LROWR = N      - K

         DO 420 I = IBEGIN, NACTIV
            CALL DROT3G( T(I,K+1), T(I,K), CS, SN )

            IF (L .GT. 0)
     $      CALL DROT3 ( L    , T(I+1,K+1), 1, T(I+1,K ), 1, CS, SN )
            CALL DROT3 ( NFREE, ZY(1,K+1) , 1, ZY(1,K  ), 1, CS, SN )
            CALL DROT3 ( NGQ  , GQ(K+1,1) , N, GQ(K,1)  , N, CS, SN )

*           Apply the column transformations to  R.  The non-zero
*           sub-diagonal that is generated must be eliminated by a row
*           rotation.

            IF (K .LT. NRANK) R(K+1,K) = ZERO
            LCOL   = MIN( K+1, NRANK )
            IF (LCOL .GT. 0)
     $         CALL DROT3 ( LCOL, R(1,K+1), 1, R(1,K), 1, CS, SN )

            IF (K .LT. NRANK) THEN
               CALL DROT3G( R(K,K), R(K+1,K), CS, SN )

               CALL DROT3 ( LROWR, R(K,K+1)    , NROWR,
     $                             R(K+1,K+1)  , NROWR, CS, SN )
               CALL DROT3 ( NRES , RES(K,1)    , N    ,
     $                             RES(K+1,1)  , N    , CS, SN )
            END IF
            K     = K     - 1
            L     = L     - 1
            LROWR = LROWR + 1
  420    CONTINUE

         NZ  = NZ  + 1

*        ---------------------------------------------------------------
*        Estimate the condition number of  T.
*        ---------------------------------------------------------------
         IF (NACTIV .EQ. 0) THEN
            DTMAX = ONE
            DTMIN = ONE
         ELSE
            CALL DCOND ( NACTIV, T(NACTIV,NZ+1), NROWT-1, DTMAX, DTMIN )
         END IF

      END IF

      NZ1 = NZ1 + 1

      IF (NZ .GT. NZ1) THEN
         IF (JDEL .GT. 0) THEN
            JART =   NZ1 - 1 + IDAMAX( NZ-NZ1+1, GQ(NZ1,1), 1 )
         ELSE
            JART = - JDEL
         END IF

         IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $      WRITE( NOUT, 1000 ) NZ, NZ1, JART

         IF (JART .GT. NZ1) THEN

*           Swap columns NZ1 and JART of R.

            IF (UNITQ) THEN
               K        = KX(NZ1)
               KX(NZ1)  = KX(JART)
               KX(JART) = K
            ELSE
               CALL DSWAP ( NFREE, ZY(1,NZ1), 1, ZY(1,JART), 1 )
            END IF

            CALL DSWAP ( NGQ, GQ(NZ1,1), N, GQ(JART,1), N )
            IF (NRANK .GT. 0)
     $         CALL CMRSWP( N, NRES, NRANK, NROWR, NZ1, JART,
     $                      R, RES, WORK )
         END IF
      END IF

      RETURN

 1000 FORMAT(/ ' //LSDEL //  Artificial constraint deleted.      '
     $       / ' //LSDEL //      NZ   NZ1   JART                 '
     $       / ' //LSDEL //  ', 3I6 )
 1100 FORMAT(/ ' //LSDEL //  Simple bound deleted.               '
     $       / ' //LSDEL //  NACTIV    NZ NFREE    IR  JDEL UNITQ'
     $       / ' //LSDEL //  ', 5I6, L6 )
 1200 FORMAT(/ ' //LSDEL //  General constraint deleted.         '
     $       / ' //LSDEL //  NACTIV    NZ NFREE  KDEL  JDEL UNITQ'
     $       / ' //LSDEL //  ', 5I6, L6 )

*     End of  LSDEL .

      END
