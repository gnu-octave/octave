      REAL FUNCTION gennf(dfn,dfd,xnonc)

C**********************************************************************
C
C     REAL FUNCTION GENNF( DFN, DFD, XNONC )
C           GENerate random deviate from the Noncentral F distribution
C
C
C                              Function
C
C
C     Generates a random deviate from the  noncentral F (variance ratio)
C     distribution with DFN degrees of freedom in the numerator, and DFD
C     degrees of freedom in the denominator, and noncentrality parameter
C     XNONC.
C
C
C                              Arguments
C
C
C     DFN --> Numerator degrees of freedom
C             (Must be >= 1.0)
C                              REAL DFN
C      DFD --> Denominator degrees of freedom
C             (Must be positive)
C                              REAL DFD
C
C     XNONC --> Noncentrality parameter
C               (Must be nonnegative)
C                              REAL XNONC
C
C
C                              Method
C
C
C     Directly generates ratio of noncentral numerator chisquare variate
C     to central denominator chisquare variate.
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL dfd,dfn,xnonc
C     ..
C     .. Local Scalars ..
      REAL xden,xnum
      LOGICAL qcond
C     ..
C     .. External Functions ..
      REAL genchi,gennch
      EXTERNAL genchi,gennch
C     ..
C     .. Executable Statements ..
      qcond = dfn .LE. 1.0 .OR. dfd .LE. 0.0 .OR. xnonc .LT. 0.0
      IF (.NOT. (qcond)) GO TO 10
      WRITE (*,*) 'In GENNF - Either (1) Numerator DF <= 1.0 or'
      WRITE (*,*) '(2) Denominator DF < 0.0 or '
      WRITE (*,*) '(3) Noncentrality parameter < 0.0'
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd,'XNONC value: ',
     +  xnonc
      CALL XSTOPX
     + ('Degrees of freedom or noncent param our of range in GENNF')

   10 xnum = gennch(dfn,xnonc)/dfn
C      GENNF = ( GENNCH( DFN, XNONC ) / DFN ) / ( GENCHI( DFD ) / DFD )
      xden = genchi(dfd)/dfd
      IF (.NOT. (xden.LE. (1.2E-38*xnum))) GO TO 20
      WRITE (*,*) ' GENNF - generated numbers would cause overflow'
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden
      WRITE (*,*) ' GENNF returning 1.0E38'
      gennf = 1.0E38
      GO TO 30

   20 gennf = xnum/xden
   30 RETURN

      END
