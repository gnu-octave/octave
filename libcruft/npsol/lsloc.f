*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSLOC ( LPROB, N, NCLIN, LITOTL, LWTOTL )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)

************************************************************************
*     LSLOC   allocates the addresses of the work arrays for  LSCORE.
*
*     Note that the arrays  ( GQ, CQ )  and  ( RES, RES0, HZ )  lie in
*     contiguous areas of workspace.
*     RES, RES0 and HZ are not needed for LP.
*     CQ is defined when the objective has an explicit linear term.
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written  29-October-1984.
*     This version of LSLOC dated 16-February-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT, NCOLT, NQ

      PARAMETER        ( LENLS = 20 )
      COMMON    /SOL1LS/ LOCLS(LENLS)

      LOGICAL            LSDBG
      PARAMETER        ( LDBG = 5 )
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      MINIW     = LITOTL + 1
      MINW      = LWTOTL + 1


*     Assign array lengths that depend upon the problem dimensions.

      IF (NCLIN .EQ. 0) THEN
         LENT      = 0
         LENZY     = 0
      ELSE
         LENT  = NROWT*NCOLT
         LENZY = NQ   *NQ
      END IF

      LENCQ  = 0
      IF (LPROB .EQ. 2*(LPROB/2)) LENCQ  = N
      LENRES = 0
      IF (LPROB .GT. 2          ) LENRES = N

      LKACTV    = MINIW
      MINIW     = LKACTV + N

      LANORM    = MINW
      LAP       = LANORM + NCLIN
      LPX       = LAP    + NCLIN
      LGQ       = LPX    + N
      LCQ       = LGQ    + N
      LRES      = LCQ    + LENCQ
      LRES0     = LRES   + LENRES
      LHZ       = LRES0  + LENRES
      LRLAM     = LHZ    + LENRES
      LT        = LRLAM  + N
      LZY       = LT     + LENT
      LWTINF    = LZY    + LENZY
      LWRK      = LWTINF + N  + NCLIN
      LFEATL    = LWRK   + N  + NCLIN
      MINW      = LFEATL + N  + NCLIN

      LOCLS( 1) = LKACTV
      LOCLS( 2) = LANORM
      LOCLS( 3) = LAP
      LOCLS( 4) = LPX
      LOCLS( 5) = LRES
      LOCLS( 6) = LRES0
      LOCLS( 7) = LHZ
      LOCLS( 8) = LGQ
      LOCLS( 9) = LCQ
      LOCLS(10) = LRLAM
      LOCLS(11) = LT
      LOCLS(12) = LZY
      LOCLS(13) = LWTINF
      LOCLS(14) = LWRK
      LOCLS(15) = LFEATL

      LITOTL    = MINIW - 1
      LWTOTL    = MINW  - 1

      RETURN

*     End of  LSLOC .

      END
