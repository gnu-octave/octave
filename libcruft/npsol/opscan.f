C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE OPSCAN (STRING, FIRST, LAST, MARK)
C
C
C Description and usage:
C
C       Looks for non-blank fields ("tokens") in a string, where the
C    fields are of arbitrary length, separated by blanks, tabs, commas,
C    colons, or equal signs.  The position of the end of the 1st token
C    is also returned, so this routine may be conveniently used within
C    a loop to process an entire line of text.
C
C       The procedure examines a substring, STRING (FIRST : LAST), which
C    may of course be the entire string (in which case just call OPSCAN
C    with FIRST <= 1 and LAST >= LEN (STRING) ).  The indices returned
C    are relative to STRING itself, not the substring.
C
C
C Parameters:
C
C    Name    Dimension  Type  I/O/S  Description
C    STRING              C    I      Text string containing data to be
C                                    scanned.
C    FIRST               I    I/O    Index of beginning of substring.
C                                    If <= 1, the search begins with 1.
C                                    Output is index of beginning of
C                                    first non-blank field, or 0 if no
C                                    token was found.
C    LAST                I    I/O    Index of end of substring.
C                                    If >= LEN (STRING), the search
C                                    begins with LEN (STRING).  Output
C                                    is index of end of last non-blank
C                                    field, or 0 if no token was found.
C    MARK                I      O    Points to end of first non-blank
C                                    field in the specified substring.
C                                    Set to 0 if no token was found.
C
C
C Environment:  Digital VAX-11/780 VMS FORTRAN (FORTRAN 77).
C               ANSI Fortran 77, except for the tab character HT.
C
C Notes:
C
C    (1)  IMPLICIT NONE is non-standard.  Constant HT (Tab) is defined
C         in a non-standard way:  the CHAR function is not permitted
C         in a PARAMETER declaration (OK on VAX, though).  For Absoft
C         FORTRAN 77 on 68000 machines, use HT = 9.  In other cases, it
C         may be best to declare HT as a variable and assign
C         HT = CHAR(9) on ASCII machines, or CHAR(5) for EBCDIC.
C
C    (2)  The pseudo-recursive structure was chosen for fun.  It is
C         equivalent to three DO loops with embedded GO TOs in sequence.
C
C    (3)  The variety of separators recognized limits the usefulness of
C         this routine somewhat.  The intent is to facilitate handling
C         such tokens as keywords or numerical values.  In other
C         applications, it may be necessary for ALL printing characters
C         to be significant.  A simple modification to statement
C         function SOLID will do the trick.
C
C
C Author:  Robert Kennelly, Informatics General Corporation.
C
C
C Development history:
C
C    29 Dec. 1984    RAK    Initial design and coding, (very) loosely
C                           based on SCAN_STRING by Ralph Carmichael.
C    25 Feb. 1984    RAK    Added ':' and '=' to list of separators.
C    16 Apr. 1985    RAK    Defined SOLID in terms of variable DUMMY
C                           (previous re-use of STRING was ambiguous).
C
C-----------------------------------------------------------------------


C     Variable declarations.
C     ----------------------

*     IMPLICIT NONE

C     Parameters.

      CHARACTER
     $   BLANK, EQUAL, COLON, COMMA, HT
      PARAMETER
     $   (BLANK = ' ', EQUAL = '=', COLON = ':', COMMA = ',')

C     Variables.

      LOGICAL
     $   SOLID
      INTEGER
     $   BEGIN, END, FIRST, LAST, LENGTH, MARK
      CHARACTER
     $   DUMMY, STRING * (*)

C     Statement functions.

      SOLID (DUMMY) = (DUMMY .NE. BLANK) .AND.
     $                (DUMMY .NE. COLON) .AND.
     $                (DUMMY .NE. COMMA) .AND.
     $                (DUMMY .NE. EQUAL) .AND.
     $                (DUMMY .NE. HT)


C     Executable statements.
C     ----------------------

****  HT     = CHAR(9) for ASCII machines, CHAR(5) for EBCDIC.
      HT     = CHAR(9)
      MARK   = 0
      LENGTH = LEN (STRING)
      BEGIN  = MAX (FIRST, 1)
      END    = MIN (LENGTH, LAST)

C     Find the first significant character ...

      DO 30 FIRST = BEGIN, END, +1
         IF (SOLID (STRING (FIRST : FIRST))) THEN

C           ... then the end of the first token ...

            DO 20 MARK = FIRST, END - 1, +1
               IF (.NOT.SOLID (STRING (MARK + 1 : MARK + 1))) THEN

C                 ... and finally the last significant character.

                  DO 10 LAST = END, MARK, -1
                     IF (SOLID (STRING (LAST : LAST))) THEN
                        RETURN
                     END IF
   10             CONTINUE

C                 Everything past the first token was a separator.

                  LAST = LAST + 1
                  RETURN
               END IF
   20       CONTINUE

C           There was nothing past the first token.

            LAST = MARK
            RETURN
         END IF
   30 CONTINUE

C     Whoops - the entire substring STRING (BEGIN : END) was composed of
C     separators !

      FIRST = 0
      MARK = 0
      LAST = 0
      RETURN

C     End of OPSCAN
      END
