      SUBROUTINE ALLOC ( NALG, N, NCLIN, NCNLN, NCTOTL, NROWA, NROWJ,
     *                   LITOTL, LWTOTL )
C
C     IMPLICIT           REAL*8(A-H,O-Z)
      INTEGER            NALG, N, NCLIN, NCNLN, NCTOTL, NROWA, NROWJ,
     *                   LITOTL, LWTOTL
C
      LOGICAL            SCALED
      INTEGER            NOUT, MSG, ISTART, LENNAM, NROWRT,
     *                   NCOLRT, NQ, NCQP, NROWQP
      DOUBLE PRECISION   PARM
      COMMON    /SOL1CM/ NOUT, MSG, ISTART
      COMMON    /SOL3CM/ LENNAM, NROWRT, NCOLRT, NQ
      COMMON    /SOL4CM/ PARM(10)
C
      INTEGER            LOCLP
      COMMON    /SOL1LP/ LOCLP(15)
C
      INTEGER            LOCNP
      COMMON    /SOL1NP/ LOCNP(30)
      COMMON    /SOL2NP/ NCQP, NROWQP
C
      INTEGER            LOCLC
      COMMON    /SOL1LC/ LOCLC(15)
C
C  *********************************************************************
C  ALLOC  ALLOCATES THE ADDRESSES OF THE WORK ARRAYS FOR LPCORE, QPCORE
C  LCCORE  AND  NPCORE.
C
C  SYSTEMS OPTIMIZATION LABORATORY, STANFORD UNIVERSITY.
C  ORIGINAL VERSION              JANUARY  1983.
C           VERSION 0.0  LCCORE  MAY      1983.
C           VERSION 2.0  NPCORE  APRIL    1984.
C  *********************************************************************
C
      INTEGER            MAX0
C
      GO TO (100, 100, 300, 400), NALG
C
C  ---------------------------------------------------------------------
C  ALLOCATE THE ADDRESSES FOR  LPCORE  AND  QPCORE.
C  ---------------------------------------------------------------------
  100 LKACTV    = LITOTL + 1
      LKFREE    = LKACTV + N
      LITOTL    = LKFREE + N - 1
C
      LANORM    = LWTOTL + 1
      LAP       = LANORM + NCLIN
      LPX       = LAP    + NCLIN
      LQTG      = LPX    + N
      LRLAM     = LQTG   + N
      LRT       = LRLAM  + N
      LZY       = LRT    + NROWRT*NCOLRT
      LWRK      = LZY    + NQ*NQ
      LWTOTL    = LWRK   + N - 1
C
      LOCLP( 2) = LKACTV
      LOCLP( 3) = LKFREE
      LOCLP( 4) = LANORM
      LOCLP( 5) = LAP
      LOCLP( 6) = LPX
      LOCLP( 7) = LQTG
      LOCLP( 8) = LRLAM
      LOCLP( 9) = LRT
      LOCLP(10) = LZY
      LOCLP(11) = LWRK
C
      GO TO 900
C
C  ---------------------------------------------------------------------
C  ALLOCATE THE ADDRESSES FOR NPCORE.
C  ---------------------------------------------------------------------
  300 LKACTV    = LITOTL + 1
      LKFREE    = LKACTV + N
      LIQPST    = LKFREE + N
      LITOTL    = LIQPST + NCTOTL - 1
C
C  VARIABLES USED NOT ONLY BY  NPCORE,  BUT ALSO  LPCORE AND  QPCORE.
C
      LANORM    = LWTOTL + 1
      LQTG      = LANORM + NROWQP
      LRLAM     = LQTG   + N
      LRT       = LRLAM  + N
      LZY       = LRT    + NROWRT*NCOLRT
C
      LOCLP( 2) = LKACTV
      LOCLP( 3) = LKFREE
      LOCLP( 4) = LANORM
      LOCLP( 7) = LQTG
      LOCLP( 8) = LRLAM
      LOCLP( 9) = LRT
      LOCLP(10) = LZY
C
C  ASSIGN THE ADDRESSES FOR THE WORKSPACE ARRAYS USED BY  NPIQP.
C
      LQPADX    = LZY    + NQ*NQ
      LQPDX     = LQPADX + NROWQP
      LQPWRK    = LQPDX  + N
C
      LOCLP( 5) = LQPADX
      LOCLP( 6) = LQPDX
      LOCLP(11) = LQPWRK
C
C  ASSIGN THE ADDRESSES FOR ARRAYS USED IN  NPCORE.
C
      IF (NCNLN .EQ. 0) LENAQP = 0
      IF (NCNLN .GT. 0) LENAQP = NROWQP*N
C
      LAQP      = LQPWRK + N
      LADX      = LAQP   + LENAQP
      LBL       = LADX   + NROWQP
      LBU       = LBL    + NCTOTL
      LDX       = LBU    + NCTOTL
      LG1       = LDX    + N
      LG2       = LG1    + N
      LQPTOL    = LG2    + N
      LX1       = LQPTOL + NCTOTL
      LNPWRK    = LX1    + N
C
      LOCNP( 1) = LIQPST
      LOCNP( 2) = LAQP
      LOCNP( 3) = LADX
      LOCNP( 4) = LBL
      LOCNP( 5) = LBU
      LOCNP( 6) = LDX
      LOCNP( 7) = LG1
      LOCNP( 8) = LG2
      LOCNP( 9) = LQPTOL
      LOCNP(10) = LX1
      LOCNP(11) = LNPWRK
C
      LCS1      = LNPWRK + NCTOTL
      LCS2      = LCS1   + NCNLN
      LCSL1     = LCS2   + NCNLN
      LCSLAM    = LCSL1  + NCNLN
      LCJDX     = LCSLAM + NCNLN
      LDLAM     = LCJDX  + NCNLN
      LDSLK     = LDLAM  + NCNLN
      LRHO      = LDSLK  + NCNLN
      LSIGMA    = LRHO   + NCNLN
      LSLK1     = LSIGMA + NCNLN
      LSLK      = LSLK1  + NCNLN
C
      LOCNP(12) = LCS1
      LOCNP(13) = LCS2
      LOCNP(14) = LCSL1
      LOCNP(15) = LCSLAM
      LOCNP(16) = LCJDX
      LOCNP(17) = LDLAM
      LOCNP(18) = LDSLK
      LOCNP(19) = LRHO
      LOCNP(20) = LSIGMA
      LOCNP(21) = LSLK1
      LOCNP(22) = LSLK
C
      LWTOTL    = LSLK   + NCNLN  - 1
C
      GO TO 900
C
C  ---------------------------------------------------------------------
C  ALLOCATE THE ADDRESSES FOR  LCCORE.
C  ---------------------------------------------------------------------
  400 LKACTV    = LITOTL + 1
      LKFREE    = LKACTV + N
      LITOTL    = LKFREE + N - 1
C
      LZTG2     = LWTOTL + 1
C
      LOCLC( 1) = LZTG2
C
C  ARRAYS USED NOT ONLY BY  LCCORE,  BUT ALSO  LPCORE.
C
      LANORM    = LZTG2  + N
      LAP       = LANORM + NCLIN
      LPX       = LAP    + NCLIN
      LQTG      = LPX    + N
      LRLAM     = LQTG   + N
      LRT       = LRLAM  + N
      LZY       = LRT    + NROWRT*NCOLRT
      LWRK      = LZY    + NQ*NQ
C
      LOCLP( 2) = LKACTV
      LOCLP( 3) = LKFREE
      LOCLP( 4) = LANORM
      LOCLP( 5) = LAP
      LOCLP( 6) = LPX
      LOCLP( 7) = LQTG
      LOCLP( 8) = LRLAM
      LOCLP( 9) = LRT
      LOCLP(10) = LZY
      LOCLP(11) = LWRK
C
      LSHARE    = LWRK   + N
C
C  ASSIGN THE ADDRESSES OF THE WORKSPACE USED BY  LCSRCH.
C  THIS WORKSPACE IS SHARED BY  LCAPPG.
C
      LX2       = LSHARE
      LGRAD2    = LX2    + N
      LMAX1     = LGRAD2 + N      - 1
C
C  ASSIGN THE ADDRESSES OF THE WORKSPACE USED BY  LCAPPG.
C  THIS WORKSPACE IS SHARED BY  LCSRCH.
C
      LXFWD     = LSHARE
      LXBWD     = LXFWD   + N
      LMAX2     = LXBWD   + N     - 1
C
      LWTOTL    = MAX0( LMAX1, LMAX2 )
C
      LOCLC( 2) = LX2
      LOCLC( 3) = LGRAD2
C
      LOCLC( 4) = LXFWD
      LOCLC( 5) = LXBWD
C
  900 RETURN
C
C  END OF ALLOC
      END
