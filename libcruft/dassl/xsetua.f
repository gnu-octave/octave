C*DECK XSETUA
      SUBROUTINE XSETUA (IUNITA, N)
C***BEGIN PROLOGUE  XSETUA
C***PURPOSE  Set logical unit numbers (up to 5) to which error
C            messages are to be sent.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3B
C***TYPE      ALL (XSETUA-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  JONES, R. E., (SNLA)
C             Modified by
C           FRITSCH, F. N., (LLNL)
C***DESCRIPTION
C
C     Abstract
C        XSETUA may be called to declare a list of up to five
C        logical units, each of which is to receive a copy of
C        each error message processed by this package.
C        The purpose of XSETUA is to allow simultaneous printing
C        of each error message on, say, a main output file,
C        an interactive terminal, and other files such as graphics
C        communication files.
C
C     Description of Parameters
C      --Input--
C        IUNIT - an array of up to five unit numbers.
C                Normally these numbers should all be different
C                (but duplicates are not prohibited.)
C        N     - the number of unit numbers provided in IUNIT
C                must have 1 .LE. N .LE. 5.
C
C     CAUTION:  The use of COMMON in this version is not safe for
C               multiprocessing.
C
C***REFERENCES  JONES R.E., KAHANER D.K., 'XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE', SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  XERMSG
C***COMMON BLOCKS    XERUNI
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900510  Change call to XERRWV to XERMSG.  (RWC)
C   901011  Rewritten to not use J4SAVE.  (FNF)
C***END PROLOGUE  XSETUA
      DIMENSION IUNITA(5)
      INTEGER  NUNIT, IUNIT(5)
      COMMON /XERUNI/ NUNIT, IUNIT
      CHARACTER *8 XERN1
C***FIRST EXECUTABLE STATEMENT  XSETUA
C
      IF (N.LT.1 .OR. N.GT.5) THEN
         WRITE (XERN1, '(I8)') N
         CALL XERMSG ('SLATEC', 'XSETUA',
     *      'INVALID NUMBER OF UNITS, N = ' // XERN1, 1, 2)
         RETURN
      ENDIF
C
      DO 10 I=1,N
         IUNIT(I) = IUNITA(I)
   10 CONTINUE
      NUNIT = N
      RETURN
      END
