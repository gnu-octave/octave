*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPLOC ( N, NCLIN, NCNLN, NCTOTL, LITOTL, LWTOTL)

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)

************************************************************************
*     NPLOC   allocates the addresses of the work arrays for NPCORE and
*     LSCORE.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version   14-February-1985.
*     This version of  NPLOC  dated 12-July-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT, NCOLT, NQ

      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      PARAMETER         (LENNP = 35)
      COMMON    /SOL1NP/ LOCNP(LENNP)

      LOGICAL            NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG

      MINIW     = LITOTL + 1
      MINW      = LWTOTL + 1

*     Assign array lengths that depend upon the problem dimensions.

      IF (NCLIN + NCNLN .EQ. 0) THEN
         LENT      = 0
         LENZY     = 0
      ELSE
         LENT  = NROWT*NCOLT
         LENZY = NQ   *NQ
      END IF

      IF (NCNLN .EQ. 0) THEN
         LENAQP = 0
      ELSE
         LENAQP = (NCLIN + NCNLN)*N
      END IF

      LKACTV    = MINIW
      LKX       = LKACTV + N
      LNEEDC    = LKX    + N
      LIPERM    = LNEEDC + NCNLN
      MINIW     = LIPERM + NCTOTL

      LHFRWD    = MINW
      LHCTRL    = LHFRWD + N
      LANORM    = LHCTRL + N
      LQPGQ     = LANORM + NCLIN + NCNLN
      LGQ       = LQPGQ  + N
      LRLAM     = LGQ    + N
      LT        = LRLAM  + N
      LZY       = LT     + LENT
      MINW      = LZY    + LENZY

      LOCLS( 1) = LKACTV
      LOCLS( 2) = LANORM
      LOCLS( 8) = LQPGQ
      LOCLS( 9) = LGQ
      LOCLS(10) = LRLAM
      LOCLS(11) = LT
      LOCLS(12) = LZY

*     Assign the addresses for the workspace arrays used by  NPIQP.

      LQPADX    = MINW
      LQPDX     = LQPADX + NCLIN + NCNLN
      LRPQ      = LQPDX  + N
      LRPQ0     = LRPQ   + N
      LQPHZ     = LRPQ0  + N
      LWTINF    = LQPHZ  + N
      LWRK1     = LWTINF + NCTOTL
      LQPTOL    = LWRK1  + NCTOTL
      MINW      = LQPTOL + NCTOTL

      LOCLS( 3) = LQPADX
      LOCLS( 4) = LQPDX
      LOCLS( 5) = LRPQ
      LOCLS( 6) = LRPQ0
      LOCLS( 7) = LQPHZ
      LOCLS(13) = LWTINF
      LOCLS(14) = LWRK1
      LOCLS(15) = LQPTOL

*     Assign the addresses for arrays used in NPCORE.

      LAQP      = MINW
      LADX      = LAQP   + LENAQP
      LBL       = LADX   + NCLIN  + NCNLN
      LBU       = LBL    + NCTOTL
      LDX       = LBU    + NCTOTL
      LGQ1      = LDX    + N
      LFEATL    = LGQ1   + N
      LX1       = LFEATL + NCTOTL
      LWRK2     = LX1    + N
      MINW      = LWRK2  + NCTOTL

      LOCNP( 1) = LKX
      LOCNP( 2) = LIPERM
      LOCNP( 3) = LAQP
      LOCNP( 4) = LADX
      LOCNP( 5) = LBL
      LOCNP( 6) = LBU
      LOCNP( 7) = LDX
      LOCNP( 8) = LGQ1
      LOCNP(10) = LFEATL
      LOCNP(11) = LX1
      LOCNP(12) = LWRK2

      LCS1      = MINW
      LCS2      = LCS1   + NCNLN
      LC1MUL    = LCS2   + NCNLN
      LCMUL     = LC1MUL + NCNLN
      LCJDX     = LCMUL  + NCNLN
      LDLAM     = LCJDX  + NCNLN
      LDSLK     = LDLAM  + NCNLN
      LRHO      = LDSLK  + NCNLN
      LWRK3     = LRHO   + NCNLN
      LSLK1     = LWRK3  + NCNLN
      LSLK      = LSLK1  + NCNLN
      MINW      = LSLK   + NCNLN

      LOCNP(13) = LCS1
      LOCNP(14) = LCS2
      LOCNP(15) = LC1MUL
      LOCNP(16) = LCMUL
      LOCNP(17) = LCJDX
      LOCNP(18) = LDLAM
      LOCNP(19) = LDSLK
      LOCNP(20) = LRHO
      LOCNP(21) = LWRK3
      LOCNP(22) = LSLK1
      LOCNP(23) = LSLK
      LOCNP(24) = LNEEDC

      LCJAC     = MINW
      LGRAD     = LCJAC  + NCNLN*N
      MINW      = LGRAD  + N

      LOCNP(25) = LHFRWD
      LOCNP(26) = LHCTRL
      LOCNP(27) = LCJAC
      LOCNP(28) = LGRAD

      LITOTL    = MINIW - 1
      LWTOTL    = MINW  - 1

      RETURN

*     End of  NPLOC .

      END
