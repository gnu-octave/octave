*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSFILE( IOPTNS, INFORM )
      INTEGER            IOPTNS, INFORM

************************************************************************
*     LSFILE  reads the options file from unit  IOPTNS  and loads the
*     options into the relevant elements of  IPRMLS  and  RPRMLS.
*
*     If  IOPTNS .lt. 0  or  IOPTNS .gt. 99  then no file is read,
*     otherwise the file associated with unit  IOPTNS  is read.
*
*     Output:
*
*         INFORM = 0  if a complete  OPTIONS  file was found
*                     (starting with  BEGIN  and ending with  END);
*                  1  if  IOPTNS .lt. 0  or  IOPTNS .gt. 99;
*                  2  if  BEGIN  was found, but end-of-file
*                     occurred before  END  was found;
*                  3  if end-of-file occurred before  BEGIN  or
*                     ENDRUN  were found;
*                  4  if  ENDRUN  was found before  BEGIN.
************************************************************************
      LOGICAL             NEWOPT
      COMMON     /SOL3LS/ NEWOPT
      SAVE       /SOL3LS/

      DOUBLE PRECISION    WMACH(15)
      COMMON     /SOLMCH/ WMACH
      SAVE       /SOLMCH/

      EXTERNAL            MCHPAR, LSKEY
      LOGICAL             FIRST
      SAVE                FIRST , NOUT
      DATA                FIRST /.TRUE./

*     If first time in, set NOUT.
*     NEWOPT is true first time into LSFILE or LSOPTN
*     and just after a call to LSSOL.

      IF (FIRST) THEN
         FIRST  = .FALSE.
         NEWOPT = .TRUE.
         CALL MCHPAR()
         NOUT = WMACH(11)
      END IF

      CALL OPFILE( IOPTNS, NOUT, INFORM, LSKEY )

      RETURN

*     End of  LSFILE.

      END
