      REAL FUNCTION genf(dfn,dfd)
C**********************************************************************
C
C     REAL FUNCTION GENF( DFN, DFD )
C                GENerate random deviate from the F distribution
C
C
C                              Function
C
C
C     Generates a random deviate from the F (variance ratio)
C     distribution with DFN degrees of freedom in the numerator
C     and DFD degrees of freedom in the denominator.
C
C
C                              Arguments
C
C
C     DFN --> Numerator degrees of freedom
C             (Must be positive)
C                              REAL DFN
C      DFD --> Denominator degrees of freedom
C             (Must be positive)
C                              REAL DFD
C
C
C                              Method
C
C
C     Directly generates ratio of chisquare variates
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL dfd,dfn
C     ..
C     .. Local Scalars ..
      REAL xden,xnum
C     ..
C     .. External Functions ..
      REAL genchi
      EXTERNAL genchi
C     ..
C     .. Executable Statements ..
      IF (.NOT. (dfn.LE.0.0.OR.dfd.LE.0.0)) GO TO 10
      WRITE (*,*) 'Degrees of freedom nonpositive in GENF - abort!'
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd
      CALL XSTOPX ('Degrees of freedom nonpositive in GENF - abort!')

   10 xnum = genchi(dfn)/dfn
C      GENF = ( GENCHI( DFN ) / DFN ) / ( GENCHI( DFD ) / DFD )
      xden = genchi(dfd)/dfd
      IF (.NOT. (xden.LE. (1.2E-38*xnum))) GO TO 20
      WRITE (*,*) ' GENF - generated numbers would cause overflow'
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden
      WRITE (*,*) ' GENF returning 1.0E38'
      genf = 1.0E38
      GO TO 30

   20 genf = xnum/xden
   30 RETURN

      END
