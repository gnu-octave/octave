*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE ICOPY ( N, IX, INCIX, IY, INCIY )

      INTEGER            N, INCIX, INCIY
      INTEGER            IX(*), IY(*)

C
C  Copy the first N elements of IX into IY.
C

      INTEGER            J, JX, JY

      IF (N .GE. 1) THEN
         IF (INCIX .EQ. 1  .AND.  INCIY .EQ. 1) THEN

            DO 10 J = 1, N
               IY(J) = IX(J)
   10       CONTINUE

         ELSE

            JX = 1
            JY = 1
            DO 20 J = 1, N
               IY(JY) = IX(JX)
               JX = JX + INCIX
               JY = JY + INCIY
   20       CONTINUE

         END IF
      END IF

      RETURN

*     End of  ICOPY

      END
