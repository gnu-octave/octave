*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSDFLT( M, N, NCLIN, TITLE )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)

      CHARACTER*(*)      TITLE

************************************************************************
*  LSDFLT  loads the default values of parameters not set by the user.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 77 version written 17-September-1985.
*  This version of LSDFLT dated   9-September-1986.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      LOGICAL            CMDBG, LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG
      COMMON    /CMDEBG/ ICMDBG(LDBG), CMDBG

      LOGICAL            NEWOPT
      COMMON    /SOL3LS/ NEWOPT
      SAVE      /SOL3LS/

*-----------------------------------------------------------------------
      PARAMETER         (MXPARM = 30)
      INTEGER            IPRMLS(MXPARM), IPSVLS
      DOUBLE PRECISION   RPRMLS(MXPARM), RPSVLS

      COMMON    /LSPAR1/ IPSVLS(MXPARM),
     $                   IDBGLS, ITMAX1, ITMAX2, LCRASH, LDBGLS, LPROB ,
     $                   MSGLS , NN    , NNCLIN, NPROB , IPADLS(20)

      COMMON    /LSPAR2/ RPSVLS(MXPARM),
     $                   BIGBND, BIGDX , BNDLOW, BNDUPP, TOLACT, TOLFEA,
     $                   TOLRNK, RPADLS(23)

      EQUIVALENCE       (IPRMLS(1), IDBGLS), (RPRMLS(1), BIGBND)

      SAVE      /LSPAR1/, /LSPAR2/
*-----------------------------------------------------------------------
      EQUIVALENCE   (MSGLS , MSGLVL), (IDBGLS, IDBG), (LDBGLS, MSGDBG)

      LOGICAL            CDEFND
      CHARACTER*4        ICRSH(0:2)
      CHARACTER*3        LSTYPE(1:10)
      CHARACTER*16       KEY
      INTRINSIC          LEN    ,  MAX   , MOD
      PARAMETER        ( ZERO   =  0.0D+0, TEN    = 10.0D+0)
      PARAMETER        ( RDUMMY = -11111., IDUMMY = -11111 )
      PARAMETER        ( GIGANT = 1.0D+10*.99999           )
      PARAMETER        ( WRKTOL = 1.0D-2                   )
      DATA               ICRSH(0), ICRSH(1), ICRSH(2)
     $                 /'COLD'   ,'WARM'   ,'HOT '   /
      DATA               LSTYPE(1), LSTYPE(2)
     $                 /' FP'     ,' LP'     /
      DATA               LSTYPE(3), LSTYPE(4), LSTYPE(5), LSTYPE(6)
     $                 /'QP1'     ,'QP2'     ,'QP3'     ,'QP4'     /
      DATA               LSTYPE(7), LSTYPE(8), LSTYPE(9), LSTYPE(10)
     $                 /'LS1'     ,'LS2'     ,'LS3'     ,'LS4'     /

      EPSMCH = WMACH( 3)

*     Make a dummy call to LSKEY to ensure that the defaults are set.

      CALL LSKEY ( NOUT, '*', KEY )
      NEWOPT = .TRUE.

*     Save the optional parameters set by the user.  The values in
*     RPRMLS and IPRMLS may be changed to their default values.

      CALL ICOPY ( MXPARM, IPRMLS, 1, IPSVLS, 1 )
      CALL DCOPY ( MXPARM, RPRMLS, 1, RPSVLS, 1 )

      IF (       LPROB  .LT. 0      )  LPROB   = 7
                                       CDEFND  = LPROB .EQ. 2*(LPROB/2)
      IF (       LCRASH .LT. 0
     $    .OR.   LCRASH .GT. 2      )  LCRASH  = 0
      IF (       ITMAX1 .LT. 0      )  ITMAX1  = MAX(50, 5*(N+NCLIN))
      IF (       ITMAX2 .LT. 0      )  ITMAX2  = MAX(50, 5*(N+NCLIN))
      IF (       MSGLVL .EQ. IDUMMY )  MSGLVL  = 10
      IF (       IDBG   .LT. 0
     $    .OR.   IDBG   .GT. ITMAX1 + ITMAX2
     $                              )  IDBG    = 0
      IF (       MSGDBG .LT. 0      )  MSGDBG  = 0
      IF (       MSGDBG .EQ. 0      )  IDBG    = ITMAX1 + ITMAX2 + 1
      IF (       TOLACT .LT. ZERO   )  TOLACT  = WRKTOL
      IF (       TOLFEA .EQ. RDUMMY
     $    .OR.  (TOLFEA .GE. ZERO
     $    .AND.  TOLFEA .LT. EPSMCH))  TOLFEA  = EPSPT5
      IF (       TOLRNK .LE. ZERO
     $    .AND.  CDEFND             )  TOLRNK  = EPSPT5
      IF (       TOLRNK .LE. ZERO   )  TOLRNK  = TEN*EPSMCH
      IF (       BIGBND .LE. ZERO   )  BIGBND  = GIGANT
      IF (       BIGDX  .LE. ZERO   )  BIGDX   = MAX(GIGANT, BIGBND)

      LSDBG = IDBG .EQ. 0
      CMDBG = LSDBG
      K     = 1
      MSG   = MSGDBG
      DO 200 I = 1, LDBG
         ILSDBG(I) = MOD( MSG/K, 10 )
         ICMDBG(I) = ILSDBG(I)
         K = K*10
  200 CONTINUE

      IF (MSGLVL .GT. 0) THEN

*        Print the title.

         LENT = LEN( TITLE )
         IF (LENT .GT. 0) THEN
            NSPACE = (81 - LENT)/2 + 1
            WRITE (NOUT, '(///// (80A1) )')
     $         (' ', J=1, NSPACE), (TITLE(J:J), J=1,LENT)
            WRITE (NOUT, '(80A1 //)')
     $         (' ', J=1, NSPACE), ('='       , J=1,LENT)
         END IF

         WRITE (NOUT, 2000)
         WRITE (NOUT, 2100) LSTYPE(LPROB),
     $                      NCLIN , TOLFEA, ICRSH(LCRASH),
     $                      N     , BIGBND, TOLACT,
     $                      M     , BIGDX , TOLRNK
         WRITE (NOUT, 2200) EPSMCH, ITMAX1, MSGLVL,
     $                              ITMAX2
      END IF

      RETURN

 2000 FORMAT(
     $//' Parameters'
     $/ ' ----------' )
 2100 FORMAT(
     $/ ' Problem type...........', 7X, A3
     $/ ' Linear constraints.....', I10,     6X,
     $  ' Feasibility tolerance..', 1PE10.2, 6X,
     $  1X, A4, ' start.............'
     $/ ' Variables..............', I10,     6X,
     $  ' Infinite bound size....', 1PE10.2, 6X,
     $  ' Crash tolerance........', 1PE10.2
     $/ ' Objective matrix rows..', I10,     6X,
     $  ' Infinite step size.....', 1PE10.2, 6X,
     $  ' Rank tolerance.........', 1PE10.2 )
 2200 FORMAT(
     $/ ' EPS (machine precision)', 1PE10.2, 6X,
     $  ' Feasibility phase itns.', I10, 6X,
     $  ' Print level............', I10
     $/ 40X,
     $  ' Optimality  phase itns.', I10 )

*     End of  LSDFLT.

      END
