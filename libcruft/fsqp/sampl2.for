c
c     problem description
c
      program sampl2
c
      integer   nwsize,iwsize,nparam,nf,nineq,neq
      parameter (iwsize=1029, nwsize=7693)
      parameter (nparam=6, nf=163, nineq=7, neq=0)
      integer   nineqn,neqn,mode,iprint,miter,inform,
     *          iw(iwsize)
      double precision bigbnd,eps,epsneq,delta
      double precision x(nparam),bl(nparam),bu(nparam),w(nwsize),
     *                 f(nf+1),g(nineq+neq+1)
      external objmad,cnmad,grobfd,grcnfd
c
      mode=111
      iprint=1
      miter=500
      bigbnd=1.d+10
      eps=1.d-08
      epsneq=0.d0
      delta=0.d0
c
c     nparam=6
c     nf=163
      nineqn=0
      neqn=0
c     nineq=7
c     neq=0
c
      bl(1)=-bigbnd
      bl(2)=-bigbnd
      bl(3)=-bigbnd
      bl(4)=-bigbnd
      bl(5)=-bigbnd
      bl(6)=-bigbnd
      bu(1)=bigbnd
      bu(2)=bigbnd
      bu(3)=bigbnd
      bu(4)=bigbnd
      bu(5)=bigbnd
      bu(6)=bigbnd
c
c     give the initial value of x
c
      x(1) =  0.5d0
      x(2) =  1.d0
      x(3) =  1.5d0
      x(4) =  2.d0
      x(5) =  2.5d0
      x(6) =  3.d0
c
      call FSQPD(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,
     *           miter,inform,bigbnd,eps,epsneq,delta,bl,bu,x,f,
     *           g,iw,iwsize,w,nwsize,objmad,cnmad,grobfd,grcnfd)
      end
c
      subroutine objmad(nparam,j,x,fj)
      integer nparam,j,i
      double precision x(nparam),theta,pi,fj
c
      pi=3.14159265358979d0
      theta=pi*(8.5d0+dble(j)*0.5d0)/180.d0
      fj=0.d0
      do 10 i=1,6
 10     fj=fj+dcos(2.d0*pi*x(i)*dsin(theta))
      fj=2.d0*(fj+dcos(2.d0*pi*3.5d0*dsin(theta)))/15.d0+1.d0/15.d0
      return
      end
c     
      subroutine cnmad(nparam,j,x,gj)
      integer nparam,j
      double precision x(nparam),ss,gj
c
      ss=0.425d0
      goto(10,20,30,40,50,60,70),j
 10   gj=ss-x(1)
      return
 20   gj=ss+x(1)-x(2)
      return
 30   gj=ss+x(2)-x(3)
      return
 40   gj=ss+x(3)-x(4)
      return
 50   gj=ss+x(4)-x(5)
      return
 60   gj=ss+x(5)-x(6)
      return
 70   gj=ss+x(6)-3.5d0
      return
      end
