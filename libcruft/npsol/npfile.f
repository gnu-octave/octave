*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPFILE( IOPTNS, INFORM )
      INTEGER            IOPTNS, INFORM

************************************************************************
*     NPFILE  reads the options file from unit  IOPTNS  and loads the
*     options into the relevant elements of  IPRMNP  and  RPRMNP.
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
      COMMON     /SOL7NP/ NEWOPT
      SAVE       /SOL7NP/

      DOUBLE PRECISION    WMACH(15)
      COMMON     /SOLMCH/ WMACH
      SAVE       /SOLMCH/

      EXTERNAL            MCHPAR, NPKEY
      LOGICAL             FIRST
      SAVE                FIRST , NOUT
      DATA                FIRST /.TRUE./

*     If first time in, set  NOUT.
*     NEWOPT is true first time into NPFILE or NPOPTN
*     and just after a call to NPSOL.

      IF (FIRST) THEN
         FIRST  = .FALSE.
         NEWOPT = .TRUE.
         CALL MCHPAR()
         NOUT = WMACH(11)
      END IF

      CALL OPFILE( IOPTNS, NOUT, INFORM, NPKEY )

      RETURN

*     End of  NPFILE.

      END
