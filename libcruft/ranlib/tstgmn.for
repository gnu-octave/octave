      REAL FUNCTION covar(x,y,n)
C     .. Scalar Arguments ..
      INTEGER n
C     ..
C     .. Array Arguments ..
      REAL x(n),y(n)
C     ..
C     .. Local Scalars ..
      REAL avx,avy,varx,vary,xmax,xmin
      INTEGER i
C     ..
C     .. External Subroutines ..
      EXTERNAL stat
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC real
C     ..
C     .. Executable Statements ..
      CALL stat(x,n,avx,varx,xmin,xmax)
      CALL stat(y,n,avy,vary,xmin,xmax)
      covar = 0.0
      DO 10,i = 1,n
          covar = covar + (x(i)-avx)* (y(i)-avy)
   10 CONTINUE
      covar = covar/real(n-1)
      RETURN

      END
      SUBROUTINE prcomp(p,mean,xcovar,answer)

      INTEGER p,maxp
      PARAMETER (maxp=10)
      REAL mean(p),xcovar(p,p),rcovar(maxp,maxp)
      REAL answer(1000,maxp)
      REAL rmean(maxp),rvar(maxp)
      INTEGER maxobs
      PARAMETER (maxobs=1000)

      DO 10,i = 1,p
          CALL stat(answer(1,i),maxobs,rmean(i),rvar(i),dum1,dum2)
          WRITE (*,*) ' Variable Number',i
          WRITE (*,*) ' Mean ',mean(i),' Generated ',rmean(i)
          WRITE (*,*) ' Variance ',xcovar(i,i),' Generated',rvar(i)
   10 CONTINUE
      WRITE (*,*) '                   Covariances'
      DO 30,i = 1,p
          DO 20,j = 1,i - 1
              WRITE (*,*) ' I = ',i,' J = ',j
              rcovar(i,j) = covar(answer(1,i),answer(1,j),maxobs)
              WRITE (*,*) ' Covariance ',xcovar(i,j),' Generated ',
     +          rcovar(i,j)
   20     CONTINUE
   30 CONTINUE
      RETURN

      END
      SUBROUTINE setcov(p,var,corr,covar)
C     Set covariance matrix from variance and common correlation
C     .. Scalar Arguments ..
      REAL corr
      INTEGER p
C     ..
C     .. Array Arguments ..
      REAL covar(p,p),var(p)
C     ..
C     .. Local Scalars ..
      INTEGER i,j
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC sqrt
C     ..
C     .. Executable Statements ..
      DO 40,i = 1,p
          DO 30,j = 1,p
              IF (.NOT. (i.EQ.j)) GO TO 10
              covar(i,j) = var(i)
              GO TO 20

   10         covar(i,j) = corr*sqrt(var(i)*var(j))
   20         CONTINUE
   30     CONTINUE
   40 CONTINUE
      RETURN

      END
      SUBROUTINE stat(x,n,av,var,xmin,xmax)
C     .. Scalar Arguments ..
      REAL av,var,xmax,xmin
      INTEGER n
C     ..
C     .. Array Arguments ..
      REAL x(n)
C     ..
C     .. Local Scalars ..
      REAL sum
      INTEGER i
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC real
C     ..
C     .. Executable Statements ..
      xmin = x(1)
      xmax = x(1)
      sum = 0.0
      DO 10,i = 1,n
          sum = sum + x(i)
          IF (x(i).LT.xmin) xmin = x(i)
          IF (x(i).GT.xmax) xmax = x(i)
   10 CONTINUE
      av = sum/real(n)
      sum = 0.0
      DO 20,i = 1,n
          sum = sum + (x(i)-av)**2
   20 CONTINUE
      var = sum/real(n-1)
      RETURN

      END
      PROGRAM tstgmn
C     Test Generation of Multivariate Normal Data
C     .. Parameters ..
      INTEGER maxp
      PARAMETER (maxp=10)
      INTEGER maxobs
      PARAMETER (maxobs=1000)
      INTEGER p2
      PARAMETER (p2=maxp*maxp)
C     ..
C     .. Local Scalars ..
      REAL corr
      INTEGER i,iobs,is1,is2,j,p
      CHARACTER phrase*100
C     ..
C     .. Local Arrays ..
      REAL answer(1000,maxp),ccovar(p2),covar(p2),mean(maxp),param(500),
     +     temp(maxp),var(maxp),work(maxp)
C     ..
C     .. External Subroutines ..
      EXTERNAL genmn,phrtsd,prcomp,setall,setcov,setgmn
C     ..
C     .. Executable Statements ..
      WRITE (*,9000)

 9000 FORMAT (
     +     ' Tests Multivariate Normal Generator for Up to 10 Variables'
     +       /
     +  ' User inputs means, variances, one correlation that is applied'
     +       /'     to all pairs of variables'/
     +       ' 1000 multivariate normal deviates are generated'/
     +     ' Means, variances and covariances are calculated for these.'
     +       )

   10 WRITE (*,*) 'Enter number of variables for normal generator'
      READ (*,*) p
      WRITE (*,*) 'Enter mean vector of length ',p
      READ (*,*) (mean(i),i=1,p)
      WRITE (*,*) 'Enter variance vector of length ',p
      READ (*,*) (var(i),i=1,p)
      WRITE (*,*) 'Enter correlation of all variables'
      READ (*,*) corr
      CALL setcov(p,var,corr,covar)
      WRITE (*,*) ' Enter phrase to initialize rn generator'
      READ (*,'(a)') phrase
      CALL phrtsd(phrase,is1,is2)
      CALL setall(is1,is2)
      DO 20,i = 1,p2
          ccovar(i) = covar(i)
   20 CONTINUE
C
C     Generate Variables
C
      CALL setgmn(mean,ccovar,p,param)
      DO 40,iobs = 1,maxobs
          CALL genmn(param,work,temp)
          DO 30,j = 1,p
              answer(iobs,j) = work(j)
   30     CONTINUE
   40 CONTINUE
      CALL prcomp(p,mean,covar,answer)
C
C     Print Comparison of Generated and Reconstructed Values
C
      GO TO 10

      END
