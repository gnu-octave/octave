*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPKEY ( NOUT, BUFFER, KEY )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*(*)      BUFFER

************************************************************************
*     NPKEY   decodes the option contained in  BUFFER  in order to set
*     a parameter value in the relevant element of the parameter arrays.
*
*
*     Input:
*
*     NOUT   A unit number for printing error messages.
*            NOUT  must be a valid unit.
*
*     Output:
*
*     KEY    The first keyword contained in BUFFER.
*
*
*     NPKEY  calls OPNUMB and the subprograms
*                 LOOKUP, SCANNR, TOKENS, UPCASE
*     (now called OPLOOK, OPSCAN, OPTOKN, OPUPPR)
*     supplied by Informatics General, Inc., Palo Alto, California.
*
*     Systems Optimization Laboratory, Stanford University.
*     This version of NPKEY  dated 12-July-1986.
************************************************************************
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

      EXTERNAL           OPNUMB
      LOGICAL            FIRST , MORE  , NUMBER, OPNUMB, SORTED
      SAVE               FIRST

      PARAMETER         (     MAXKEY = 38,  MAXTIE = 19,   MAXTOK = 10)
      CHARACTER*16       KEYS(MAXKEY), TIES(MAXTIE), TOKEN(MAXTOK)
      CHARACTER*16       KEY, KEY2, KEY3, VALUE

      PARAMETER         (IDUMMY = -11111,  RDUMMY = -11111.0,
     $                   SORTED = .TRUE.,  ZERO   =  0.0     )

      DATA                FIRST
     $                  /.TRUE./
      DATA   KEYS
     $ / 'BEGIN           ',
     $   'CENTRAL         ', 'COLD            ', 'CONSTRAINTS     ',
     $   'CRASH           ', 'DEBUG           ', 'DEFAULTS        ',
     $   'DERIVATIVE      ', 'DIFFERENCE      ',
     $   'END             ', 'FEASIBILITY     ', 'FUNCTION        ',
     $   'HESSIAN         ', 'HOT             ', 'INFINITE        ',
     $   'IPRMLS          ', 'ITERATIONS      ', 'ITERS:ITERATIONS',
     $   'ITNS :ITERATIONS', 'LINEAR          ', 'LINESEARCH      ',
     $   'LIST            ', 'LOWER           ',
     $   'MAJOR           ', 'MINOR           ',
     $   'NOLIST          ',
     $   'NONLINEAR       ', 'OPTIMALITY      ', 'PRINT           ',
     $   'PROBLEM         ', 'ROW             ', 'RPRMLS          ',
     $   'START           ', 'STOP            ', 'UPPER           ',
     $   'VARIABLES       ', 'VERIFY          ', 'WARM            '/

      DATA   TIES
     $ / 'BOUND           ', 'CONSTRAINTS     ', 'DEBUG           ',
     $   'FEASIBILITY     ', 'GRADIENTS       ',
     $   'ITERATIONS      ', 'ITERS:ITERATIONS',
     $   'ITNS :ITERATIONS', 'JACOBIAN        ', 'LEVEL           ',
     $   'NO              ',
     $   'NO.      :NUMBER',
     $   'NUMBER          ', 'OBJECTIVE       ', 'PRINT           ',
     $   'STEP            ', 'TOLERANCE       ',
     $   'VARIABLES       ', 'YES             '/
*-----------------------------------------------------------------------

      IF (FIRST) THEN
         FIRST  = .FALSE.
         DO 10 I = 1, MXPARM
            RPRMLS(I) = RDUMMY
            IPRMLS(I) = IDUMMY
            RPRMNP(I) = RDUMMY
            IPRMNP(I) = IDUMMY
   10    CONTINUE
      END IF

*     Eliminate comments and empty lines.
*     A '*' appearing anywhere in BUFFER terminates the string.

      I      = INDEX( BUFFER, '*' )
      IF (I .EQ. 0) THEN
         LENBUF = LEN( BUFFER )
      ELSE
         LENBUF = I - 1
      END IF
      IF (LENBUF .LE. 0) THEN
         KEY = '*'
         GO TO 900
      END IF

*     ------------------------------------------------------------------
*     Extract up to MAXTOK tokens from the record.
*     NTOKEN returns how many were actually found.
*     KEY, KEY2, KEY3 are the first tokens if any, otherwise blank.
*     ------------------------------------------------------------------
      NTOKEN = MAXTOK
      CALL OPTOKN( BUFFER(1:LENBUF), NTOKEN, TOKEN )
      KEY    = TOKEN(1)
      KEY2   = TOKEN(2)
      KEY3   = TOKEN(3)

*     Certain keywords require no action.

      IF (KEY .EQ. ' '     .OR.  KEY .EQ. 'BEGIN' ) GO TO 900
      IF (KEY .EQ. 'LIST'  .OR.  KEY .EQ. 'NOLIST') GO TO 900
      IF (KEY .EQ. 'END'                          ) GO TO 900

*     Most keywords will have an associated integer or real value,
*     so look for it no matter what the keyword.

      I      = 1
      NUMBER = .FALSE.

   50 IF (I .LT. NTOKEN  .AND.  .NOT. NUMBER) THEN
         I      = I + 1
         VALUE  = TOKEN(I)
         NUMBER = OPNUMB( VALUE )
         GO TO 50
      END IF

      IF (NUMBER) THEN
         READ (VALUE, '(BN, E16.0)') RVALUE
      ELSE
         RVALUE = ZERO
      END IF

*     Convert the keywords to their most fundamental form
*     (upper case, no abbreviations).
*     SORTED says whether the dictionaries are in alphabetic order.
*     LOCi   says where the keywords are in the dictionaries.
*     LOCi = 0 signals that the keyword wasn't there.

      CALL OPLOOK( MAXKEY, KEYS, SORTED, KEY , LOC1 )
      CALL OPLOOK( MAXTIE, TIES, SORTED, KEY2, LOC2 )

*     ------------------------------------------------------------------
*     Decide what to do about each keyword.
*     The second keyword (if any) might be needed to break ties.
*     Some seemingly redundant testing of MORE is used
*     to avoid compiler limits on the number of consecutive ELSE IFs.
*     ------------------------------------------------------------------
      MORE   = .TRUE.
      IF (MORE) THEN
         MORE   = .FALSE.
         IF      (KEY .EQ. 'CENTRAL     ') THEN
            CDINT  = RVALUE
         ELSE IF (KEY .EQ. 'COLD        ') THEN
            LCRASH = 0
         ELSE IF (KEY .EQ. 'CONSTRAINTS ') THEN
            NNCLIN = RVALUE
         ELSE IF (KEY .EQ. 'CRASH       ') THEN
            TOLACT = RVALUE
         ELSE IF (KEY .EQ. 'DEBUG       ') THEN
            IDBG   = RVALUE
         ELSE IF (KEY .EQ. 'DEFAULTS    ') THEN
            DO 20 I = 1, MXPARM
               IPRMLS(I) = IDUMMY
               RPRMLS(I) = RDUMMY
               IPRMNP(I) = IDUMMY
               RPRMNP(I) = RDUMMY
   20       CONTINUE
         ELSE IF (KEY .EQ. 'DERIVATIVE  ') THEN
            LVLDER = RVALUE
         ELSE IF (KEY .EQ. 'DIFFERENCE  ') THEN
            FDINT  = RVALUE
         ELSE IF (KEY .EQ. 'FEASIBILITY ') THEN
            TOLFEA = RVALUE
            CTOL   = RVALUE
         ELSE IF (KEY .EQ. 'FUNCTION    ') THEN
            EPSRF  = RVALUE
         ELSE
            MORE   = .TRUE.
         END IF
      END IF

      IF (MORE) THEN
         MORE   = .FALSE.
         IF      (KEY .EQ. 'HESSIAN     ') THEN
            LFORMH = 1
            IF   (KEY2.EQ. 'NO          ') LFORMH = 0
         ELSE IF (KEY .EQ. 'HOT         ') THEN
            LCRASH = 2
         ELSE IF (KEY .EQ. 'INFINITE    ') THEN
              IF (KEY2.EQ. 'BOUND       ') BIGBND = RVALUE * 0.99999
              IF (KEY2.EQ. 'STEP        ') BIGDX  = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'IPRMLS      ') THEN
*           Allow things like  IPRMLS 21 = 100  to set IPRMLS(21) = 100
            IVALUE = RVALUE
            IF (IVALUE .GE. 1  .AND. IVALUE .LE. MXPARM) THEN
               READ (KEY3, '(BN, I16)') IPRMLS(IVALUE)
            ELSE
               WRITE(NOUT, 2400) IVALUE
            END IF
         ELSE IF (KEY .EQ. 'ITERATIONS  ') THEN
            NMAJOR = RVALUE
         ELSE IF (KEY .EQ. 'LINEAR      ') THEN
            IF (KEY2  .EQ. 'CONSTRAINTS ') NNCLIN = RVALUE
            IF (KEY2  .EQ. 'FEASIBILITY ') TOLFEA = RVALUE
            IF (LOC2 .EQ.  0             ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'LINESEARCH  ') THEN
            ETA    = RVALUE
         ELSE IF (KEY .EQ. 'LOWER       ') THEN
            BNDLOW = RVALUE
         ELSE
            MORE   = .TRUE.
         END IF
      END IF

      IF (MORE) THEN
         MORE   = .FALSE.
         IF      (KEY .EQ. 'MAJOR       ') THEN
              IF (KEY2.EQ. 'DEBUG       ') MJRDBG = RVALUE
              IF (KEY2.EQ. 'ITERATIONS  ') NMAJOR = RVALUE
              IF (KEY2.EQ. 'PRINT       ') MSGNP  = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'MINOR       ') THEN
              IF (KEY2.EQ. 'DEBUG       ') MNRDBG = RVALUE
              IF (KEY2.EQ. 'ITERATIONS  ') NMINOR = RVALUE
              IF (KEY2.EQ. 'PRINT       ') MSGQP  = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'NONLINEAR   ') THEN
              IF (KEY2.EQ. 'CONSTRAINTS ') NNCNLN = RVALUE
              IF (KEY2.EQ. 'FEASIBILITY ') CTOL   = RVALUE
              IF (KEY2.EQ. 'JACOBIAN    ') NLNJ   = RVALUE
              IF (KEY2.EQ. 'OBJECTIVE   ') NLNF   = RVALUE
              IF (KEY2.EQ. 'VARIABLES   ') NLNX   = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'OPTIMALITY  ') THEN
            FTOL   = RVALUE
         ELSE
            MORE   = .TRUE.
         END IF
      END IF

      IF (MORE) THEN
         MORE   = .FALSE.
         IF      (KEY .EQ. 'PRINT       ') THEN
            MSGNP  = RVALUE
         ELSE IF (KEY .EQ. 'PROBLEM     ') THEN
              IF (KEY2.EQ. 'NUMBER      ') NPROB  = RVALUE
         ELSE IF (KEY .EQ. 'ROW         ') THEN
              IF (KEY2.EQ. 'TOLERANCE   ') CTOL   = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'RPRMLS      ') THEN
*           Allow things like  RPRMLS 21 = 2  to set RPRMLS(21) = 2.0
            IVALUE = RVALUE
            IF (IVALUE .GE. 1  .AND. IVALUE .LE. MXPARM) THEN
               READ (KEY3, '(BN, E16.0)') RPRMLS(IVALUE)
            ELSE
               WRITE(NOUT, 2400) IVALUE
            END IF
         ELSE IF (KEY .EQ. 'START       ') THEN
              IF (KEY2.EQ. 'CONSTRAINTS ') JVRFY3 = RVALUE
              IF (KEY2.EQ. 'OBJECTIVE   ') JVRFY1 = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'STOP        ') THEN
              IF (KEY2.EQ. 'CONSTRAINTS ') JVRFY4 = RVALUE
              IF (KEY2.EQ. 'OBJECTIVE   ') JVRFY2 = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE IF (KEY .EQ. 'UPPER       ') THEN
            BNDUPP = RVALUE
         ELSE IF (KEY .EQ. 'VARIABLES   ') THEN
            NN     = RVALUE
         ELSE IF (KEY .EQ. 'VERIFY      ') THEN
              IF (KEY2.EQ. 'OBJECTIVE   ') LVERFY =  1
              IF (KEY2.EQ. 'CONSTRAINTS ') LVERFY =  2
              IF (KEY2.EQ. 'NO          ') LVERFY = -1
              IF (KEY2.EQ. 'YES         ') LVERFY =  3
              IF (KEY2.EQ. 'GRADIENTS   ') LVERFY =  3
              IF (KEY2.EQ. 'LEVEL       ') LVERFY =  RVALUE
              IF (LOC2.EQ.  0            ) LVERFY =  3
         ELSE IF (KEY .EQ. 'WARM        ') THEN
            LCRASH = 1
         ELSE
            WRITE(NOUT, 2300) KEY
         END IF
      END IF

  900 RETURN

 2300 FORMAT(' XXX  Keyword not recognized:         ', A)
 2320 FORMAT(' XXX  Second keyword not recognized:  ', A)
 2330 FORMAT(' XXX  Third  keyword not recognized:  ', A)
 2400 FORMAT(' XXX  The PARM subscript is out of range:', I10)

*     End of NPKEY

      END
