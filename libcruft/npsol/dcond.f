*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  BLAS FORTRAN
*
*                         Others
*                         ------
*     DCOND*   DDDIV*   DDIV     DDSCL    DGRFG    DLOAD    DNORM
*     DROT3*   DROT3G*  DSSQ     ICOPY*   ILOAD    IDRANK+
*
*    *Not in the Nag Blas.
*    +Differs from the Nag Blas.
*
*                         QR Routines
*                         -- --------
*     DGEQR    DGEQRP   DGEAP    DGEAPQ
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DCOND ( N, X, INCX, AXMAX, AXMIN )

      INTEGER            N, INCX
      DOUBLE PRECISION   AXMAX, AXMIN
      DOUBLE PRECISION   X( (N-1)*INCX+1 )
C
C     DCOND   finds the elements in  x  that are largest and smallest
C     in magnitude.
C
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
      INTEGER            I, IX
      INTRINSIC          ABS, MAX, MIN

      IF (N .EQ. 0) THEN
         AXMAX = ZERO
         AXMIN = ZERO
      ELSE
         AXMAX = ABS( X(1) )
         AXMIN = AXMAX
         IX    = 1
         DO 100 I = 2, N
            IX    = IX + INCX
            AXMAX = MAX( AXMAX, ABS( X(IX) ) )
            AXMIN = MIN( AXMIN, ABS( X(IX) ) )
  100    CONTINUE
      END IF

      RETURN

*     End of  DCOND

      END
