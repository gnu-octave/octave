*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPOPTN( STRING )
      CHARACTER*(*)      STRING

************************************************************************
*     NPOPTN  loads the option supplied in STRING into the relevant
*     element of IPRMLS, RPRMLS, IPRMNP or RPRMNP.
************************************************************************

      LOGICAL             NEWOPT
      COMMON     /SOL7NP/ NEWOPT
      SAVE       /SOL7NP/

      DOUBLE PRECISION    WMACH(15)
      COMMON     /SOLMCH/ WMACH
      SAVE       /SOLMCH/

      EXTERNAL            MCHPAR
      CHARACTER*16        KEY
      CHARACTER*72        BUFFER
      LOGICAL             FIRST , PRNT
      SAVE                FIRST , NOUT  , PRNT
      DATA                FIRST /.TRUE./

*     If first time in, set NOUT.
*     NEWOPT is true first time into NPFILE or NPOPTN
*     and just after a call to an optimization routine.
*     PRNT is set to true whenever NEWOPT is true.

      IF (FIRST) THEN
         FIRST  = .FALSE.
         NEWOPT = .TRUE.
         CALL MCHPAR()
         NOUT   =  WMACH(11)
      END IF
      BUFFER = STRING

*     Call NPKEY to decode the option and set the parameter value.
*     If NEWOPT is true, reset PRNT and test specially for NOLIST.

      IF (NEWOPT) THEN
         NEWOPT = .FALSE.
         PRNT   = .TRUE.
         CALL NPKEY ( NOUT, BUFFER, KEY )

         IF (KEY .EQ. 'NOLIST') THEN
            PRNT   = .FALSE.
         ELSE
            WRITE (NOUT, '(// A / A /)')
     $         ' Calls to NPOPTN',
     $         ' ---------------'
            WRITE (NOUT, '( 6X, A )') BUFFER
         END IF
      ELSE
         IF (PRNT)
     $      WRITE (NOUT, '( 6X, A )') BUFFER
         CALL NPKEY ( NOUT, BUFFER, KEY )

         IF (KEY .EQ.   'LIST') PRNT = .TRUE.
         IF (KEY .EQ. 'NOLIST') PRNT = .FALSE.
      END IF

      RETURN

*     End of  NPOPTN.

      END
