C*DECK XGETUA
      SUBROUTINE XGETUA (IUNITA, N)
C***BEGIN PROLOGUE  XGETUA
C***PURPOSE  Return unit number(s) to which error messages are being
C            sent.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XGETUA-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  JONES, R. E., (SNLA)
C             Modified by
C           FRITSCH, F. N., (LLNL)
C***DESCRIPTION
C
C     Abstract
C        XGETUA may be called to determine the unit number or numbers
C        to which error messages are being sent.
C        These unit numbers may have been set by a call to XSETUN,
C        or a call to XSETUA, or may be a default value.
C
C     Description of Parameters
C      --Output--
C        IUNIT - an array of one to five unit numbers, depending
C                on the value of N.  A value of zero refers to the
C                default unit, as defined by the I1MACH machine
C                constant routine.  Only IUNIT(1),...,IUNIT(N) are
C                defined by XGETUA.  The values of IUNIT(N+1),...,
C                IUNIT(5) are not defined (for N .LT. 5) or altered
C                in any way by XGETUA.
C        N     - the number of units to which copies of the
C                error messages are being sent.  N will be in the
C                range from 1 to 5.
C
C     CAUTION:  The use of COMMON in this version is not safe for
C               multiprocessing.
C
C***REFERENCES  JONES R.E., KAHANER D.K., 'XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE', SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***COMMON BLOCKS    XERUNI
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   901011  Rewritten to not use J4SAVE.  (FNF)
C   901012  Corrected initialization problem.  (FNF)
C***END PROLOGUE  XGETUA
      DIMENSION IUNITA(5)
      INTEGER  NUNIT, IUNIT(5)
      COMMON /XERUNI/ NUNIT, IUNIT
C***FIRST EXECUTABLE STATEMENT  XGETUA
C       Initialize so XERMSG will use standard error unit number if
C       block has not been set up by a CALL XSETUA.
C       CAUTION:  This assumes uninitialized COMMON tests .LE.0 .
      IF (NUNIT.LE.0) THEN
         NUNIT = 1
         IUNIT(1) = 0
      ENDIF
      N = NUNIT
      DO 30 I=1,N
         IUNITA(I) = IUNIT(I)
   30 CONTINUE
      RETURN
      END
