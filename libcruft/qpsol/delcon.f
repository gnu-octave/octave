      SUBROUTINE DELCON( MODFYG, ORTHOG, UNITQ,
     *                   JDEL, KDEL, NACTIV, NCOLZ, NFREE,
     *                   N, NQ, NROWA, NROWRT, NCOLRT,
     *                   KACTIV, KFREE,
     *                   A, QTG, RT, ZY )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      LOGICAL            MODFYG, ORTHOG, UNITQ
      INTEGER            JDEL, KDEL, NACTIV, NCOLZ, NFREE, N, NQ,
     *                   NROWA, NROWRT, NCOLRT
      INTEGER            KACTIV(N), KFREE(N)
      DOUBLE PRECISION   ASIZE, DTMAX, DTMIN
      DOUBLE PRECISION   A(NROWA,N), RT(NROWRT,NCOLRT), QTG(N),
     *                   ZY(NQ,NQ)
C
      INTEGER            NOUT, MSG, ISTART
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN
C
C  *********************************************************************
C  DELCON  UPDATES THE FACTORIZATION OF THE MATRIX OF
C  CONSTRAINTS IN THE WORKING SET,  A(FREE) * (Z Y) = (0 T).
C
C  IF THERE ARE NO GENERAL CONSTRAINTS IN THE WORKING SET AND THE
C  MATRIX  Q = (Z Y)  IS THE IDENTITY,  Q  WILL NOT BE
C  TOUCHED.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  VERSION OF DECEMBER 1981.  REV. OCT. 1982.
C  *********************************************************************
C
      INTEGER            I, IBEGIN, IFREED, INCT, ISTORE, K, KA,
     *                   KB, L, LDIAG, LENQ, LENRT, NACTPI, NACTP1,
     *                   NACTV1, NCOLZ1, NFIXD1, NFREEI, NFREE1
      DOUBLE PRECISION   CS, ONE, SN, STORE
      DOUBLE PRECISION   DMAX1
      DATA               ONE/1.0D+0/
C
      LENQ   = NQ*(NQ - 1) + 1
      IF (JDEL .GT. N) GO TO 200
C
C  ------------------------------------------------------------------
C  A SIMPLE BOUND IS BEING DELETED FROM THE WORKING SET.
C  ------------------------------------------------------------------
      IFREED = KDEL - NACTIV
      IF (MSG .GE. 80)
     *WRITE (NOUT, 1010) NACTIV, NCOLZ, NFREE,IFREED,JDEL, UNITQ
      NACTV1 = NACTIV
      NFREE1 = NFREE + 1
      IBEGIN = 1
      KFREE(NFREE1) = JDEL
C
C  ADD THE GRADIENT CORRESPONDING TO THE NEWLY-FREED VARIABLE TO THE
C  END OF  Q(FREE)(T)G(FREE).  THIS IS DONE BY INTERCHANGING THE
C  APPROPRIATE ELEMENTS OF  QTG  AND  KACTIV.
C
      IF (.NOT. MODFYG)  GO TO 120
      IF (IFREED .EQ. 1) GO TO 120
      NFREEI = NFREE  + IFREED
      NACTP1 = NACTIV + 1
      NACTPI = NACTIV + IFREED
      STORE          = QTG(NFREE1)
      QTG(NFREE1)    = QTG(NFREEI)
      QTG(NFREEI)    = STORE
      ISTORE         = KACTIV(NACTP1)
      KACTIV(NACTP1) = KACTIV(NACTPI)
      KACTIV(NACTPI) = ISTORE
C
C  COPY THE INCOMING COLUMN OF  A  INTO THE END OF  T.
C
  120 IF (UNITQ        ) GO TO 400
      IF (NACTIV .EQ. 0) GO TO 150
C
      DO 130 KA = 1, NACTIV
         I = KACTIV(KA)
         RT(KA,NFREE1) = A(I,JDEL)
  130 CONTINUE
C
C  EXPAND  Q  BY ADDING A UNIT ROW AND COLUMN.
C
  150 CALL ZEROVC( NFREE, ZY(NFREE1,1), LENQ, NQ )
      CALL ZEROVC( NFREE, ZY(1,NFREE1), NQ, 1 )
      ZY(NFREE1,NFREE1) = ONE
      GO TO 400
C
C  ------------------------------------------------------------------
C  A GENERAL CONSTRAINT IS BEING DELETED FROM THE WORKING SET.
C  ------------------------------------------------------------------
  200 IF (MSG .GE. 80)
     *WRITE (NOUT, 1020) NACTIV, NCOLZ, NFREE, KDEL, JDEL, UNITQ
      NACTV1 = NACTIV - 1
      NFREE1 = NFREE
      IBEGIN = KDEL
      IF (KDEL .GT. NACTV1) GO TO 400
C
C  DELETE A ROW OF  T  AND MOVE THE ONES BELOW IT UP.
C
      DO 220 I = KDEL, NACTV1
         KACTIV(I) = KACTIV(I+1)
         LENRT     = NROWRT*I + 1
         LDIAG     = NFREE - I
         CALL COPYVC( I+1, RT(I+1,LDIAG), LENRT, NROWRT,
     *                       RT(I,LDIAG), LENRT, NROWRT )
  220 CONTINUE
C
C  ------------------------------------------------------------------
C  ELIMINATE THE SUPER-DIAGONAL ELEMENTS OF  T,
C  USING A BACKWARD SWEEP OF 2*2 TRANFORMATIONS.
C  ------------------------------------------------------------------
  400 IF (IBEGIN .GT. NACTV1) GO TO 800
      K = NFREE1 - IBEGIN
      L = NACTV1 - IBEGIN
C
      DO 420 I = IBEGIN, NACTV1
         CALL ELMGEN( ORTHOG, RT(I,K+1), RT(I,K), CS, SN )
         IF (L .GT. 0)
     *   CALL ELM   ( ORTHOG, L, RT(I+1,K+1), L, 1,
     *                           RT(I+1,K  ), L, 1, CS, SN )
         IF (NACTV1 .GT. 0)
     *   CALL ELM   ( ORTHOG, NFREE1, ZY(1,K+1), NQ, 1,
     *                                ZY(1,K  ), NQ, 1, CS, SN )
         IF (MODFYG)
     *   CALL ELM   ( ORTHOG, 1, QTG(K+1), 1, 1, QTG(K), 1, 1, CS, SN )
         K = K - 1
         L = L - 1
  420 CONTINUE
C
C  ------------------------------------------------------------------
C  COMPRESS THE ELEMENTS OF  KACTIV  CORRESPONDING TO FIXED VARIABLES.
C  ------------------------------------------------------------------
  800 NFIXD1 = N - NFREE1
      KB     = NACTV1 + 1
      IF (NFIXD1 .EQ. 0) GO TO 900
      DO 810 K = 1, NFIXD1
         KACTIV(KB) = KACTIV(KB+1)
         KB         = KB + 1
  810 CONTINUE
C
C  ------------------------------------------------------------------
C  ESTIMATE THE CONDITION NUMBER OF  T.
C  ------------------------------------------------------------------
  900 NCOLZ1 = NCOLZ + 1
      LENRT  = NROWRT*(NACTV1 - 1) + 1
      INCT   = NROWRT - 1
      IF (NACTV1 .GT. 0)
     *   CALL CONDVC( NACTV1, RT(NACTV1,NCOLZ1+1), LENRT, INCT,
     *                DTMAX, DTMIN )
C
      RETURN
C
 1010 FORMAT(/ 34H //DELCON//  SIMPLE BOUND DELETED.
     *       / 49H //DELCON//  NACTIV NCOLZ NFREE IFREED JDEL UNITQ
     *       / 13H //DELCON//  , 3I6, I7, I5, L6 )
 1020 FORMAT(/ 40H //DELCON//  GENERAL CONSTRAINT DELETED.
     *       / 49H //DELCON//  NACTIV NCOLZ NFREE  KDEL  JDEL UNITQ
     *       / 13H //DELCON//  , 5I6, L6 )
C
C  END OF DELCON
      END
