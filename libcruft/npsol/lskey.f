*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSKEY ( NOUT, BUFFER, KEY )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*(*)      BUFFER

************************************************************************
*     LSKEY   decodes the option contained in  BUFFER  in order to set
*     a parameter value in the relevant element of  IPRMLS  or  RPRMLS.
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
*     LSKEY  calls OPNUMB and the subprograms
*                 LOOKUP, SCANNR, TOKENS, UPCASE
*     (now called OPLOOK, OPSCAN, OPTOKN, OPUPPR)
*     supplied by Informatics General, Inc., Palo Alto, California.
*
*     Systems Optimization Laboratory, Stanford University.
*     This version dated Jan 22, 1986.
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

      EXTERNAL           OPNUMB
      LOGICAL            FIRST , MORE  , NUMBER, OPNUMB, SORTED
      SAVE               FIRST

      PARAMETER         (     MAXKEY = 27,  MAXTIE = 10,   MAXTOK = 10,
     $                        MAXTYP = 16)
      CHARACTER*16       KEYS(MAXKEY), TIES(MAXTIE), TOKEN(MAXTOK),
     $                   TYPE(MAXTYP)
      CHARACTER*16       KEY, KEY2, KEY3, VALUE

      PARAMETER         (IDUMMY = -11111,  RDUMMY = -11111.0,
     $                   SORTED = .TRUE.,  ZERO   =  0.0     )

      DATA                FIRST
     $                  /.TRUE./
      DATA   KEYS
     $ / 'BEGIN           ',
     $   'COLD            ', 'CONSTRAINTS     ', 'CRASH           ',
     $   'DEBUG           ', 'DEFAULTS        ', 'END             ',
     $   'FEASIBILITY     ', 'HOT             ', 'INFINITE        ',
     $   'IPRMLS          ', 'ITERATIONS      ', 'ITERS:ITERATIONS',
     $   'ITNS :ITERATIONS', 'LINEAR          ', 'LIST            ',
     $   'LOWER           ', 'NOLIST          ', 'OPTIMALITY      ',
     $   'PRINT           ', 'PROBLEM         ', 'RANK            ',
     $   'RPRMLS          ', 'START           ', 'UPPER           ',
     $   'VARIABLES       ', 'WARM            '/

      DATA   TIES
     $ / 'BOUND           ', 'CONSTRAINTS     ',
     $   'NO              ', 'NO.      :NUMBER', 'NUMBER          ',
     $   'PHASE           ', 'STEP            ',
     $   'TOLERANCE       ', 'TYPE            ', 'YES             '/

      DATA   TYPE
     $ / 'FP              ',
     $   'LEAST       :LS1', 'LINEAR       :LP', 'LP              ',
     $   'LS          :LS1', 'LS1             ', 'LS2             ',
     $   'LS3             ', 'LS4             ', 'LSQ         :LS1',
     $   'QP          :QP2', 'QP1             ', 'QP2             ',
     $   'QP3             ', 'QP4             ', 'QUADRATIC   :QP2'/
*-----------------------------------------------------------------------

      IF (FIRST) THEN
         FIRST  = .FALSE.
         DO 10 I = 1, MXPARM
            IPRMLS(I) = IDUMMY
            RPRMLS(I) = RDUMMY
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
         IF (KEY .EQ. 'COLD        ') THEN
            LCRASH = 0
         ELSE IF (KEY .EQ. 'CONSTRAINTS ') THEN
            NNCLIN = RVALUE
         ELSE IF (KEY .EQ. 'CRASH       ') THEN
            TOLACT = RVALUE
         ELSE IF (KEY .EQ. 'DEBUG       ') THEN
            LDBGLS = RVALUE
         ELSE IF (KEY .EQ. 'DEFAULTS    ') THEN
            DO 20 I = 1, MXPARM
               IPRMLS(I) = IDUMMY
               RPRMLS(I) = RDUMMY
   20       CONTINUE
         ELSE IF (KEY .EQ. 'FEASIBILITY ') THEN
              IF (KEY2.EQ. 'PHASE       ') ITMAX1 = RVALUE
              IF (KEY2.EQ. 'TOLERANCE   ') TOLFEA = RVALUE
              IF (LOC2.EQ.  0            ) WRITE(NOUT, 2320) KEY2
         ELSE
            MORE   = .TRUE.
         END IF
      END IF

      IF (MORE) THEN
         MORE   = .FALSE.
         IF (KEY .EQ. 'HOT         ') THEN
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
            ITMAX2 = RVALUE
         ELSE IF (KEY .EQ. 'LINEAR      ') THEN
            NNCLIN = RVALUE
         ELSE IF (KEY .EQ. 'LOWER       ') THEN
            BNDLOW = RVALUE
         ELSE
            MORE   = .TRUE.
         END IF
      END IF

      IF (MORE) THEN
         MORE   = .FALSE.
         IF      (KEY .EQ. 'OPTIMALITY  ') THEN
            ITMAX2 = RVALUE
         ELSE IF (KEY .EQ. 'PROBLEM     ') THEN
            IF      (KEY2 .EQ. 'NUMBER') THEN
               NPROB  = RVALUE
            ELSE IF (KEY2 .EQ. 'TYPE  ') THEN

*              Recognize     Problem type = LP     etc.

               CALL OPLOOK( MAXTYP, TYPE, SORTED, KEY3, LOC3 )
               IF (KEY3 .EQ. 'FP' ) LPROB = 1
               IF (KEY3 .EQ. 'LP' ) LPROB = 2
               IF (KEY3 .EQ. 'QP1') LPROB = 3
               IF (KEY3 .EQ. 'QP2') LPROB = 4
               IF (KEY3 .EQ. 'QP3') LPROB = 5
               IF (KEY3 .EQ. 'QP4') LPROB = 6
               IF (KEY3 .EQ. 'LS1') LPROB = 7
               IF (KEY3 .EQ. 'LS2') LPROB = 8
               IF (KEY3 .EQ. 'LS3') LPROB = 9
               IF (KEY3 .EQ. 'LS4') LPROB = 10
               IF (LOC3 .EQ.  0  ) WRITE(NOUT, 2330) KEY3
            ELSE
               WRITE(NOUT, 2320) KEY2
            END IF
         ELSE
            MORE   = .TRUE.
         END IF
      END IF

      IF (MORE) THEN
         MORE   = .FALSE.
         IF      (KEY .EQ. 'PRINT       ') THEN
            MSGLS  = RVALUE
         ELSE IF (KEY .EQ. 'RANK        ') THEN
            TOLRNK = RVALUE
         ELSE IF (KEY .EQ. 'RPRMLS      ') THEN
*           Allow things like  RPRMLS 21 = 2  to set RPRMLS(21) = 2.0
            IVALUE = RVALUE
            IF (IVALUE .GE. 1  .AND. IVALUE .LE. MXPARM) THEN
               READ (KEY3, '(BN, E16.0)') RPRMLS(IVALUE)
            ELSE
               WRITE(NOUT, 2400) IVALUE
            END IF
         ELSE IF (KEY .EQ. 'START       ') THEN
            IDBGLS = RVALUE
         ELSE IF (KEY .EQ. 'UPPER       ') THEN
            BNDUPP = RVALUE
         ELSE IF (KEY .EQ. 'VARIABLES   ') THEN
            NN     = RVALUE
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

*     End of LSKEY

      END
