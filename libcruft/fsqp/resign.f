c
      subroutine resign(n,neqn,psf,grdpsf,penp,g,gradg,signeq,job1,job2)
      integer i,j,job1,job2,n,neqn
      double precision psf,grdpsf(1),penp(1),g(1),gradg(n,1),
     *                 signeq(1)
c     double precision psf,grdpsf(n),penp(neqn),g(neqn),gradg(n,neqn),
c    *                 signeq(neqn)
c
c     job1=10: g*signeq, job1=11: gradg*signeq, job1=12: job1=10&11
c     job1=20: do not change sign
c     job2=10: psf,      job2=11: grdpsf,       job2=12: job2=10&11
c     job2=20: do not compute psf or grdpsf
c
      if(job2.eq.10.or.job2.eq.12) psf=0.d0
      do 100 i=1,neqn
        if(job1.eq.10.or.job1.eq.12) g(i)=signeq(i)*g(i)
        if(job2.eq.10.or.job2.eq.12) psf=psf+g(i)*penp(i)
        if(job1.eq.10.or.job1.eq.20) goto 100
          do 50 j=1,n
            gradg(j,i)=gradg(j,i)*signeq(i)
  50      continue
 100  continue
      if(job2.eq.10.or.job2.eq.20) goto 9000
      call nullvc(n,grdpsf)
      do 120 i=1,n
        do 110 j=1,neqn
 110      grdpsf(i)=grdpsf(i)+gradg(i,j)*penp(j)
 120  continue
c
 9000 return
      end
