*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMPERM( KX, M1, M2, IFAIL )

      INTEGER            IFAIL, M1, M2
      INTEGER            KX(M2)

      COMMON    /SOL1CM/ NOUT

************************************************************************
*     CMPERM checks that elements M1 to M2 of KX contain a valid
*     permutation of the integers M1 to M2. The contents of KX are
*     unchanged on exit.
*
*     SOL version of NAG Library routine M01ZBF.
*     Written by N.N.Maclaren, University of Cambridge.
*     This version of CMPERM dated 18-June-1986.
************************************************************************

      LOGICAL            CMDBG
      INTEGER            LCMDBG
      PARAMETER         (LCMDBG = 5)
      COMMON    /CMDEBG/ ICMDBG(LCMDBG), CMDBG

      INTEGER            I, IERR, J, K
      INTRINSIC          ABS

*     Check the parameters.

      IF (M2 .LT. 1  .OR.  M1 .LT. 1  .OR.  M1 .GT. M2) THEN
         IERR = 1
         IF (CMDBG  .AND.  ICMDBG(3) .GT. 0)
     $      WRITE (NOUT, FMT=1100) M1, M2
      ELSE
         IERR = 0

*        Check that KX is within range.

         DO 20 I = M1, M2
            J = KX(I)
            IF ((J .LT. M1) .OR. (J .GT. M2)) GO TO 100
            IF (I .NE. J) KX(I) = -J
   20    CONTINUE

*        Check that no value is repeated.

         DO 60 I = M1, M2
            K = - KX(I)
            IF (K .GE. 0) THEN
               J     = I
   40          KX(J) = K
               J     = K
               K     = - KX(J)
               IF (K .GT. 0) GO TO 40
               IF (J .NE. I) GO TO 120
            END IF
   60    CONTINUE
      END IF

*     Return

   80 IF (IERR .NE. 0) THEN
         IFAIL = IERR
      ELSE
         IFAIL = 0
      END IF
      RETURN
  100 IERR = 2
      WRITE (NOUT, FMT=1200) I, J
      GO TO 140
  120 IERR = 3
      WRITE (NOUT, FMT=1300) J

*     Restore KX.

  140 DO 160 I = M1, M2
         KX(I) = ABS(KX(I))
  160 CONTINUE
      GO TO 80

 1100 FORMAT(/ ' //CMPERM//  Illegal parameter values,'
     $       / ' //CMPERM//    M1    M1'
     $       / ' //CMPERM//', 2I6 )
 1200 FORMAT(/ ' XXX  KX(',I6,') contains an out-of-range value =', I16)
 1300 FORMAT(/ ' XXX  KX contains a duplicate value =',             I16)

*     End of CMPERM.

      END
