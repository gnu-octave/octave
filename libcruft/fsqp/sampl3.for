c
c     problem description
c
      integer iwsize,nwsize,nparam,nf,nineq,neq
      parameter (iwsize=33, nwsize=284)
      parameter (nparam=4, nf=1)
      parameter (nineq=1, neq=1)
      integer iw(iwsize)
      double  precision x(nparam),bl(nparam),bu(nparam),f(nf+1),
     *        g(nineq+neq+1),w(nwsize)
      external obj,cntr,gradob,gradcn
c
      integer mode,iprint,miter,neqn,nineqn,inform
      double precision bigbnd,eps,epsneq,udelta
c
      mode=100
      iprint=1
      miter=500
      bigbnd=1.d+10
      eps=1.d-07
      epsneq=7.d-6
      udelta=0.d0
c
      neqn=1
      nineqn=1
      bl(1)=1.d0
      bl(2)=1.d0
      bl(3)=1.d0
      bl(4)=1.d0
      bu(1)=5.d0
      bu(2)=5.d0
      bu(3)=5.d0
      bu(4)=5.d0
c
c     give the initial value of x
c
      x(1)=1.d0
      x(2)=5.d0
      x(3)=5.d0
      x(4)=1.d0
c
      call FSQPD(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,
     *           miter,inform,bigbnd,eps,epsneq,udelta,bl,bu,x,f,g,
     *           iw,iwsize,w,nwsize,obj,cntr,gradob,gradcn)
      end
c
      subroutine obj(nparam,j,x,fj)
      integer nparam,j
      double precision x(nparam),fj
c
      fj=x(1)*x(4)*(x(1)+x(2)+x(3))+x(3)
      return
      end
c     
      subroutine gradob(nparam,j,x,gradfj,dummy)
      integer nparam,j
      double precision dummy,x(nparam),gradfj(nparam)
      external dummy
c  
      gradfj(1)=x(4)*(x(1)+x(2)+x(3))+x(1)*x(4)
      gradfj(2)=x(1)*x(4)
      gradfj(3)=x(1)*x(4)+1.d0
      gradfj(4)=x(1)*(x(1)+x(2)+x(3))
      return
      end
c     
      subroutine cntr(nparam,j,x,gj)
      integer nparam,j
      double precision x(nparam),gj
c
      goto (10,20),j
 10   gj=25.d0-x(1)*x(2)*x(3)*x(4)
      return
 20   gj=x(1)**2+x(2)**2+x(3)**2+x(4)**2-40.d0
      return
      end
c
      subroutine gradcn(nparam,j,x,gradgj,dummy)
      integer nparam,j
      double precision dummy,x(nparam),gradgj(nparam)
      external dummy
c
      goto (10,20),j
 10   gradgj(1)=-x(2)*x(3)*x(4)
      gradgj(2)=-x(1)*x(3)*x(4)
      gradgj(3)=-x(1)*x(2)*x(4)
      gradgj(4)=-x(1)*x(2)*x(3)
      return
 20   gradgj(1)=2.d0*x(1)
      gradgj(2)=2.d0*x(2)
      gradgj(3)=2.d0*x(3)
      gradgj(4)=2.d0*x(4)
      return
      end
