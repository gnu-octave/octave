c
      double precision function slope(nob,nobL,neqn,nparam,feasb,
     *                         f,gradf,grdpsf,x,y,fM,theta,job)
c     implicit real*8(a-h,o-z)
      integer nob,nobL,neqn,nparam,job,i
      double  precision fM,theta,slope1,dmax1,dmin1,rhs,rhog,
     *        grdftx,grdfty,diff,scaprd,grpstx,grpsty
      double  precision f(nob),gradf(nparam,nob),grdpsf(nparam),
     *        x(nparam),y(nparam)
c     double  precision f(1),gradf(nparam,1),grdpsf(nparam),
c    *        x(nparam),y(nparam)
      logical feasb
c
      double  precision bigbnd,dummy
      common  /fsqpq1/bigbnd,dummy
c
c     job=0 : compute the generalized gradient of the minimax
c     job=1 : compute rhog in mode = 1
c
      slope=-bigbnd
      if(neqn.eq.0.or..not.feasb) then
        grpstx=0.d0
        grpsty=0.d0
      else
        grpstx=scaprd(nparam,grdpsf,x)
        grpsty=scaprd(nparam,grdpsf,y)
      endif
      do 100 i=1,nob
        slope1=f(i)+scaprd(nparam,gradf(1,i),x)
        slope=dmax1(slope,slope1)
        if(nobL.ne.nob) slope=dmax1(slope,-slope1)
 100  continue
      slope=slope-fM-grpstx
      if (job.eq.0) goto 9000
      rhs=theta*slope+fM
      rhog=1.d0
      do 200 i=1,nob
        grdftx=scaprd(nparam,gradf(1,i),x)-grpstx
        grdfty=scaprd(nparam,gradf(1,i),y)-grpsty
        diff=grdfty-grdftx
        if (diff.le.0.d0) goto 200
        rhog=dmin1(rhog,(rhs-f(i)-grdftx)/diff)
        if(nobL.ne.nob) rhog=dmin1(rhog,-(rhs+f(i)+grdftx)/diff)
 200  continue
      slope=rhog
 9000 return
      end
