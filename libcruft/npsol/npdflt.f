*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPDFLT( N, NCLIN, NCNLN, LENIW, LENW, TITLE )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)

      CHARACTER*(*)      TITLE

************************************************************************
*  NPDFLT  loads the default values of parameters not set in the options
*  file.
*
*  Systems Optimization Laboratory, Stanford University.
*  Original Fortran 77 version written 10-September-1985.
*  This version of NPDFLT dated  14-July-1986.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9

      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET
      COMMON    /SOL5NP/ LVRFYC, JVERFY(4)

      LOGICAL            CMDBG, LSDBG, NPDBG
      PARAMETER        ( LDBG = 5 )
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG
      COMMON    /CMDEBG/ ICMDBG(LDBG), CMDBG

      LOGICAL            NEWOPT
      COMMON    /SOL7NP/ NEWOPT
      SAVE      /SOL7NP/

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
*-----------------------------------------------------------------------
      INTEGER            IPRMNP(MXPARM), IPSVNP
      DOUBLE PRECISION   RPRMNP(MXPARM), RPSVNP

      COMMON    /NPPAR1/ IPSVNP(MXPARM),
     $                   IDBGNP, ITMXNP, JVRFY1, JVRFY2, JVRFY3, JVRFY4,
     $                   LDBGNP, LFORMH, LVLDER, LVERFY, MSGNP , NLNF  ,
     $                   NLNJ  , NLNX  , NNCNLN, IPADNP(15)

      COMMON    /NPPAR2/ RPSVNP(MXPARM),
     $                   CDINT , CTOL  , EPSRF , ETA   , FDINT , FTOL  ,
     $                   RPADNP(24)

      EQUIVALENCE       (IPRMNP(1), IDBGNP), (RPRMNP(1), CDINT)

      SAVE      /NPPAR1/, /NPPAR2/
*-----------------------------------------------------------------------
      EQUIVALENCE  (IDBGNP, IDBG  ), (ITMXNP, NMAJOR), (ITMAX2, NMINOR)
      EQUIVALENCE  (LDBGLS, MNRDBG), (LDBGNP, MJRDBG), (MSGLS , MSGQP )

      INTRINSIC          ABS    , LEN    , MOD
      PARAMETER        ( ZERO   =  0.0D+0, ONE    =  1.0D+0 )
      PARAMETER        ( POINT3 =  3.3D-1, POINT8 =  0.8D+0 )
      PARAMETER        ( POINT9 =  0.9D+0                   )
      PARAMETER        ( RDUMMY = -11111., IDUMMY = -11111  )
      PARAMETER        ( GIGANT =  1.0D+10*.99999           )
      PARAMETER        ( WRKTOL =  1.0D-2                   )

      CHARACTER*4        ICRSH(0:2)
      CHARACTER*16       KEY
      DATA                ICRSH(0),  ICRSH(1),  ICRSH(2)
     $                 / 'COLD'   , 'WARM'   , 'HOT '    /

      EPSMCH = WMACH( 3)
      NOUT   = WMACH(11)
      NCQP   = NCLIN + NCNLN
      NPLIN  = N     + NCLIN
      NCTOTL = NPLIN + NCNLN

*     Make a dummy call NPKEY to ensure that the defaults are set.

      CALL NPKEY ( NOUT, '*', KEY )
      NEWOPT = .TRUE.

*     Save the optional parameters set by the user.  The values in
*     IPRMLS, RPRMLS, IPRMNP and RPRMNP may be changed to their
*     default values.

      CALL ICOPY ( MXPARM, IPRMLS, 1, IPSVLS, 1 )
      CALL DCOPY ( MXPARM, RPRMLS, 1, RPSVLS, 1 )
      CALL ICOPY ( MXPARM, IPRMNP, 1, IPSVNP, 1 )
      CALL DCOPY ( MXPARM, RPRMNP, 1, RPSVNP, 1 )

      IF (          LCRASH .LT. 0
     $    .OR.      LCRASH .GT. 2     )   LCRASH  =  0
      IF (          LVLDER .LT. 0
     $    .OR.      LVLDER .GT. 3     )   LVLDER  =  3
      IF (          LFORMH .LT. 0
     $    .OR.      LFORMH .GT. 1     )   LFORMH  =  0
      IF (          NMAJOR .LT. 0     )   NMAJOR  = MAX(50, 3*NPLIN+
     $                                                     10*NCNLN )
      IF (          NMINOR .LT. 1     )   NMINOR  = MAX(50, 3*NCTOTL)
      IF (          MJRDBG .LT. 0     )   MJRDBG  =  0
      IF (          MNRDBG .LT. 0     )   MNRDBG  =  0
      IF (          IDBG   .LT. 0
     $    .OR.      IDBG   .GT. NMAJOR)   IDBG    =  0
      IF (          MJRDBG .EQ. 0
     $    .AND.     MNRDBG .EQ. 0     )   IDBG    = NMAJOR + 1
      IF (          MSGNP  .EQ. IDUMMY)   MSGNP   = 10
      IF (          MSGQP  .EQ. IDUMMY)   MSGQP   =  0
                                          NLNF    =  N
                                          NLNJ    =  N
                                          NLNX    =  N
      IF (          JVRFY2 .LT. 0
     $    .OR.      JVRFY2 .GT. N     )   JVRFY2  =  N
      IF (          JVRFY1 .LT. 0
     $    .OR.      JVRFY1 .GT. JVRFY2)   JVRFY1  =  1
      IF (          JVRFY4 .LT. 0
     $    .OR.      JVRFY4 .GT. N     )   JVRFY4  =  N
      IF (          JVRFY3 .LT. 0
     $    .OR.      JVRFY3 .GT. JVRFY4)   JVRFY3  =  1
      IF (          LVERFY .EQ. IDUMMY
     $    .OR.      LVERFY .GT. 13    )   LVERFY  =  0
      IF (          TOLACT .LT. ZERO
     $    .OR.      TOLACT .GE. ONE   )   TOLACT  =  WRKTOL
      IF (          TOLFEA .LT. EPSMCH
     $    .OR.      TOLFEA .GE. ONE   )   TOLFEA  =  EPSPT5
      IF (          EPSRF  .LT. EPSMCH
     $    .OR.      EPSRF  .GE. ONE   )   EPSRF   =  EPSPT9
                                          LFDSET  =  0
      IF (          FDINT  .LT. ZERO  )   LFDSET  =  2
      IF (          FDINT  .EQ. RDUMMY)   LFDSET  =  0
      IF (          FDINT  .GE. EPSMCH
     $    .AND.     FDINT  .LT. ONE   )   LFDSET  =  1
      IF (          LFDSET .EQ. 1
     $    .AND.    (CDINT  .LT. EPSMCH
     $    .OR.      CDINT  .GE. ONE  ))   CDINT   = EPSRF**POINT3
      IF (          BIGBND .LE. ZERO  )   BIGBND  = GIGANT
      IF (          BIGDX  .LE. ZERO  )   BIGDX   = MAX( GIGANT,BIGBND )
      IF (          ETA    .LT. ZERO
     $    .OR.      ETA    .GE. ONE   )   ETA     = POINT9
      IF (          FTOL   .LT. EPSRF
     $    .OR.      FTOL   .GE. ONE   )   FTOL    = EPSRF**POINT8

                                          DCTOL   = EPSPT5
      IF (          LVLDER .LT. 2     )   DCTOL   = EPSPT3
      IF (          CTOL   .LT. EPSMCH
     $    .OR.      CTOL   .GE. ONE   )   CTOL    = DCTOL

      ITMAX1    = MAX( 50, 3*(N + NCLIN + NCNLN) )
      JVERFY(1) = JVRFY1
      JVERFY(2) = JVRFY2
      JVERFY(3) = JVRFY3
      JVERFY(4) = JVRFY4

      NPDBG = IDBG .EQ. 0
      CMDBG = NPDBG

      K     = 1
      MSG1  = MJRDBG
      MSG2  = MNRDBG
      DO 200 I = 1, LDBG
         INPDBG(I) = MOD( MSG1/K, 10 )
         ICMDBG(I) = INPDBG(I)
         ILSDBG(I) = MOD( MSG2/K, 10 )
         K = K*10
  200 CONTINUE

      IF (MSGNP .GT. 0) THEN

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
         WRITE (NOUT, 2100) NCLIN , TOLFEA, ICRSH(LCRASH) ,
     $                      N     , BIGBND, TOLACT,
     $                              BIGDX
         WRITE (NOUT, 2200) NCNLN , FTOL  , EPSRF ,
     $                      NLNJ  , CTOL  ,
     $                      NLNF  , ETA   ,
     $                      EPSMCH,
     $                      LVLDER, LVERFY
         WRITE (NOUT, 2300) NMAJOR, MSGNP,
     $                      NMINOR, MSGQP

         IF (LVLDER .LT. 3) THEN
            IF      (LFDSET .EQ. 0) THEN
               WRITE (NOUT, 2400)
            ELSE IF (LFDSET .EQ. 1) THEN
               WRITE (NOUT, 2401) FDINT, CDINT
            ELSE IF (LFDSET .EQ. 2) THEN
               WRITE (NOUT, 2402)
            END IF
         END IF

      END IF

      RETURN

 2000 FORMAT(
     $//' Parameters'
     $/ ' ----------' )
 2100 FORMAT(
     $/ ' Linear constraints.....', I10,     6X,
     $  ' Linear feasibility.....', 1PE10.2, 6X,
     $  1X, A4, ' start.............'
     $/ ' Variables..............', I10,     6X,
     $  ' Infinite bound size....', 1PE10.2, 6X,
     $  ' Crash tolerance........', 1PE10.2
     $/   24X,                      16X,
     $  ' Infinite step size.....', 1PE10.2  )
 2200 FORMAT(
     $/ ' Nonlinear constraints..', I10,     6X,
     $  ' Optimality tolerance...', 1PE10.2, 6X,
     $  ' Function precision.....', 1PE10.2
     $/ ' Nonlinear Jacobian vars', I10,     6X,
     $  ' Nonlinear feasibility..', 1PE10.2
     $/ ' Nonlinear objectiv vars', I10,     6X,
     $  ' Linesearch tolerance...', 1PE10.2
     $/ ' EPS (machine precision)', 1PE10.2, 6X,
     $  ' Derivative level.......', I10,     6X,
     $  ' Verify level...........', I10)
 2300 FORMAT(
     $/ ' Major iterations limit.', I10, 6X,
     $  ' Major print level......', I10
     $/ ' Minor iterations limit.', I10, 6X,
     $  ' Minor print level......', I10 )
 2400 FORMAT(/ ' Difference intervals to be computed.' )
 2401 FORMAT(/ ' Difference interval....', 1PE10.2, 6X,
     $         ' Central diffce interval', 1PE10.2 )
 2402 FORMAT(/ ' User-supplied difference intervals.' )

*     End of  NPDFLT.

      END
