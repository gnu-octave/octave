*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSOPTN( STRING )
      CHARACTER*(*)      STRING

************************************************************************
*     LSOPTN  loads the option supplied in  STRING  into the relevant
*     element of  IPRMLS  or  RPRMLS.
************************************************************************

      LOGICAL             NEWOPT
      COMMON     /SOL3LS/ NEWOPT
      SAVE       /SOL3LS/

      DOUBLE PRECISION    WMACH(15)
      COMMON     /SOLMCH/ WMACH
      SAVE       /SOLMCH/

      EXTERNAL            MCHPAR
      CHARACTER*16        KEY
      CHARACTER*72        BUFFER
      LOGICAL             FIRST , PRNT
      SAVE                FIRST , NOUT  , PRNT
      DATA                FIRST /.TRUE./

*     If first time in, set  NOUT.
*     NEWOPT  is true first time into  LSFILE  or  LSOPTN
*     and just after a call to  LSSOL.
*     PRNT    is set to true whenever  NEWOPT  is true.

      IF (FIRST) THEN
         FIRST  = .FALSE.
         NEWOPT = .TRUE.
         CALL MCHPAR()
         NOUT   =  WMACH(11)
      END IF
      BUFFER = STRING

*     Call  LSKEY   to decode the option and set the parameter value.
*     If NEWOPT is true, reset PRNT and test specially for NOLIST.

      IF (NEWOPT) THEN
         NEWOPT = .FALSE.
         PRNT   = .TRUE.
         CALL LSKEY ( NOUT, BUFFER, KEY )

         IF (KEY .EQ. 'NOLIST') THEN
            PRNT   = .FALSE.
         ELSE
            WRITE (NOUT, '(// A / A /)')
     $         ' Calls to LSOPTN',
     $         ' ---------------'
            WRITE (NOUT, '( 6X, A )') BUFFER
         END IF
      ELSE
         IF (PRNT)
     $      WRITE (NOUT, '( 6X, A )') BUFFER
         CALL LSKEY ( NOUT, BUFFER, KEY )

         IF (KEY .EQ.   'LIST') PRNT = .TRUE.
         IF (KEY .EQ. 'NOLIST') PRNT = .FALSE.
      END IF

      RETURN

*     End of  LSOPTN.

      END
