      subroutine grobfd(nparam,j,x,gradf,obj)
c
c     FSQP Version 3.3  : computation of gradients of objective
c                         functions by forward finite differences
c
c     implicit real*8(a-h,o-z)
      integer nparam,j
      double  precision x(nparam),gradf(nparam)
      external obj
c
      integer io,iprint,ipspan,ipyes,info,ipd,idum,idum2
      double  precision epsmac,rteps,udelta,fj
      common  /fsqpp2/io,iprint,ipspan,ipyes,info,ipd,idum,idum2,
     *        /fsqpp3/epsmac,rteps,udelta,fj
c
c     estimates the gradient of the objective function 
c     by forward finite differences
c
      integer i
      double  precision xi,delta,dmax1
c
      do 10 i=1,nparam
        xi=x(i)
        delta=dmax1(udelta,rteps*dmax1(1.d0,dabs(xi)))
        if (xi.lt.0.d0) delta=-delta
        if (ipd.eq.1.or.j.ne.1.or.iprint.lt.3.or.ipyes.gt.0) goto 9
          if(i.eq.1) write(io,1001) delta
          if(i.ne.1) write(io,1002) delta
  9     x(i)=xi+delta
        call obj(nparam,j,x,gradf(i))
        gradf(i)=(gradf(i)-fj)/delta
        x(i)=xi
 10   continue
      return
 1001 format(1x,t17,8hdelta(i),t45,e22.14)
 1002 format(1x,t45,e22.14)
      end
