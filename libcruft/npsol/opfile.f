*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  OPSUBS FORTRAN
*
*     OPFILE   OPLOOK   OPNUMB   OPSCAN   OPTOKN   OPUPPR
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE OPFILE( IOPTNS, NOUT, INFORM, OPKEY )
      INTEGER            IOPTNS, NOUT, INFORM
      EXTERNAL           OPKEY

************************************************************************
*     OPFILE  reads the options file from unit  IOPTNS  and loads the
*     options into the relevant elements of the integer and real
*     parameter arrays.
*
*     Systems Optimization Laboratory, Stanford University.
*     This version dated December 18, 1985.
************************************************************************
      LOGICAL             PRNT
      CHARACTER*16        KEY   , TOKEN(1)
      CHARACTER*72        BUFFER, OLDBUF

      PRNT   = .TRUE.

*     Return if the unit number is out of range.

      IF (IOPTNS .LT. 0  .OR.  IOPTNS .GT. 99) THEN
         INFORM = 1
         RETURN
      END IF

*     ------------------------------------------------------------------
*     Look for  BEGIN, ENDRUN  or  SKIP.
*     ------------------------------------------------------------------
      NREAD  = 0
   50    READ (IOPTNS, '(A)', END = 930) BUFFER
         NREAD = NREAD + 1
         NKEY  = 1
         CALL OPTOKN( BUFFER, NKEY, TOKEN )
         KEY   = TOKEN(1)
         IF (KEY .EQ. 'ENDRUN') GO TO 940
         IF (KEY .NE. 'BEGIN' ) THEN
            IF (NREAD .EQ. 1  .AND.  KEY .NE. 'SKIP') THEN
               WRITE (NOUT, 2000) IOPTNS, BUFFER
            END IF
            GO TO 50
         END IF

*     ------------------------------------------------------------------
*     BEGIN found.
*     This is taken to be the first line of an OPTIONS file.
*     Read the second line to see if it is NOLIST.
*     ------------------------------------------------------------------
      OLDBUF = BUFFER
      READ (IOPTNS, '(A)', END = 920) BUFFER

      CALL OPKEY ( NOUT, BUFFER, KEY )

      IF (KEY .EQ. 'NOLIST') THEN
         PRNT   = .FALSE.
      END IF

      IF (PRNT) THEN
         WRITE (NOUT, '(// A / A /)')
     $      ' OPTIONS file',
     $      ' ------------'
         WRITE (NOUT, '(6X, A )') OLDBUF, BUFFER
      END IF

*     ------------------------------------------------------------------
*     Read the rest of the file.
*     ------------------------------------------------------------------
*+    while (key .ne. 'end') loop
  100 IF    (KEY .NE. 'END') THEN
         READ (IOPTNS, '(A)', END = 920) BUFFER
         IF (PRNT)
     $      WRITE (NOUT, '( 6X, A )') BUFFER

         CALL OPKEY ( NOUT, BUFFER, KEY )

         IF (KEY .EQ.   'LIST') PRNT = .TRUE.
         IF (KEY .EQ. 'NOLIST') PRNT = .FALSE.
         GO TO 100
      END IF
*+    end while

      INFORM =  0
      RETURN

  920 WRITE (NOUT, 2200) IOPTNS
      INFORM = 2
      RETURN

  930 WRITE (NOUT, 2300) IOPTNS
      INFORM = 3
      RETURN

  940 WRITE (NOUT, '(// 6X, A)') BUFFER
      INFORM = 4
      RETURN

 2000 FORMAT(
     $ //' XXX  Error while looking for an OPTIONS file on unit', I7
     $ / ' XXX  The file should start with BEGIN, SKIP or ENDRUN'
     $ / ' XXX  but the first record found was the following:'
     $ //' ---->', A
     $ //' XXX  Continuing to look for OPTIONS file...')
 2200 FORMAT(//' XXX  End-of-file encountered while processing',
     $         ' an OPTIONS file on unit', I6)
 2300 FORMAT(//' XXX  End-of-file encountered while looking for',
     $         ' an OPTIONS file on unit', I6)

*     End of  OPFILE.

      END
