c
      subroutine grcnfd(nparam,j,x,gradg,constr)
c
c     FSQP Version 3.3  : computation of gradients of constraint
c                         functions by forward finite differences
c
c     implicit real*8(a-h,o-z)
      integer nparam,j
      double  precision x(nparam),gradg(nparam)
      external constr
c
      integer io,iprint,ipspan,ipyes,info,ipd,idum,idum2
      double  precision epsmac,rteps,udelta,gj
      common  /fsqpp2/io,iprint,ipspan,ipyes,info,ipd,idum,idum2,
     *        /fsqpp3/epsmac,rteps,udelta,gj
c
c     estimate the gradient of the ith constraint 
c     by forward finite differences
c
      integer i
      double  precision xi,delta,dmax1
c
      do 10 i=1,nparam
        xi=x(i)
        delta=dmax1(udelta,rteps*dmax1(1.d0,dabs(xi)))
        if (xi.lt.0.d0) delta=-delta
        if (j.ne.1.or.iprint.lt.3) goto 9
        if (ipspan.ge.10.and.ipyes.gt.0) goto 9
          if(i.eq.1) write(io,1001) delta
          if(i.ne.1) write(io,1002) delta
        ipd=1
  9     x(i)=xi+delta
        call constr(nparam,j,x,gradg(i))
        gradg(i)=(gradg(i)-gj)/delta
        x(i)=xi
 10   continue
      return
 1001 format(1x,t17,8hdelta(i),t45,e22.14)
 1002 format(1x,t45,e22.14)
      end
