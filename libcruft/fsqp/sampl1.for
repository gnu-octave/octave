c
c     problem description
c
      program sampl1
c
      integer iwsize,nwsize,nparam,nf,nineq,neq
      parameter (iwsize=29, nwsize=219)
      parameter (nparam=3, nf=1)
      parameter (nineq=1, neq=1)
      integer iw(iwsize)
      double  precision x(nparam),bl(nparam),bu(nparam),f(nf+1),
     *        g(nineq+neq+1),w(nwsize)
      external obj32,cntr32,grob32,grcn32
c
      integer mode,iprint,miter,neqn,nineqn,inform
      double precision bigbnd,eps,epsneq,udelta
c
      mode=100
      iprint=1
      miter=500
      bigbnd=1.d+10
      eps=1.d-08
      epsneq=0.d0
      udelta=0.d0
c
c     nparam=3
c     nf=1
      neqn=0
      nineqn=1
c     nineq=1
c     neq=1
c
      bl(1)=0.d0
      bl(2)=0.d0
      bl(3)=0.d0
      bu(1)=bigbnd
      bu(2)=bigbnd
      bu(3)=bigbnd
c
c     give the initial value of x
c
      x(1)=0.1d0
      x(2)=0.7d0
      x(3)=0.2d0
c
      call FSQPD(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,
     *           miter,inform,bigbnd,eps,epsneq,udelta,bl,bu,x,f,g,
     *           iw,iwsize,w,nwsize,obj32,cntr32,grob32,grcn32)
      end
c
      subroutine obj32(nparam,j,x,fj)
      integer nparam,j
      double precision x(nparam),fj
c
      fj=(x(1)+3.d0*x(2)+x(3))**2+4.d0*(x(1)-x(2))**2
      return
      end
c     
      subroutine grob32(nparam,j,x,gradfj,dummy)
      integer nparam,j
      double precision dummy,x(nparam),gradfj(nparam),
     *       fa,fb
      external dummy
c  
      fa=2.d0*(x(1)+3.d0*x(2)+x(3))
      fb=8.d0*(x(1)-x(2))
      gradfj(1)=fa+fb
      gradfj(2)=fa*3.d0-fb
      gradfj(3)=fa
      return
      end
c     
      subroutine cntr32(nparam,j,x,gj)
      integer nparam,j
      double precision x(nparam),gj
c
      go to (10,20),j
 10   gj=x(1)**3-6.0d0*x(2)-4.0d0*x(3)+3.d0
      return
 20   gj=1.0d0-x(1)-x(2)-x(3)
      return
      end
c
      subroutine grcn32(nparam,j,x,gradgj,dummy)
      integer nparam,j
      double precision dummy,x(nparam),gradgj(nparam)
      external dummy
c
      go to (10,20),j
 10   gradgj(1)=3.d0*x(1)**2
      gradgj(2)=-6.d0
      gradgj(3)=-4.d0
      return
 20   gradgj(1)=-1.d0
      gradgj(2)=-1.d0
      gradgj(3)=-1.d0
      return
      end
